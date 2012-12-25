{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, PatternGuards #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend

#ifdef DEBUG
  , wrap
  , unwrap
#endif
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Map (Map)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(..), SeekMode(..))
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified System.IO as IO

import Control.Monad.Error (throwError)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Serialize (Serialize, put, get, encode, decode)
import Data.Conduit (Conduit, ($=), ($$), await, yield)
import Data.Conduit.Binary (sourceFile)
import Data.Default (def)
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Class (Backend(..), Query(..),
                               withTable, fetchTable, updateRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize)
import Data.Bro.Types (TableName, Table(..), Row(..), Projection(..))

data DiskBackend = DiskBackend { diskRoot   :: FilePath
                               , diskTables :: !(Map TableName Table)
                               }

instance Serialize DiskBackend where
    put (DiskBackend { .. }) = put diskRoot >> put diskTables

    get = DiskBackend <$> get <*> get

instance Backend DiskBackend where
    lookupTable name = M.lookup name <$> gets diskTables
    insertTable name schema = do
        res <- lookupTable name
        when (isJust res) $ throwError TableAlreadyExists
        modifyBackend $ \b@(DiskBackend { .. }) ->
            b { diskTables = M.insert name table diskTables }
        diskRoot <- gets diskRoot
        liftIO $! IO.withBinaryFile
            (diskRoot </> S.unpack name) WriteMode IO.hFlush
      where
        table :: Table
        table = def { tabName = name
                    , tabSchema = schema
                    , tabRowSize = rowSize schema
                    }

    modifyTable name f = do
        table <- fetchTable name
        modifyBackend $ \b@(DiskBackend { .. }) ->
            b { diskTables = M.insert name (f table) diskTables }

    modifyBackend f = do
        modify f
        -- FIXME(Sergei): unfortunately, we can't use 'get' from the
        -- 'Control.Monad.State' here, because of the name clash with
        -- 'get' from 'Data.Binary'.
        diskRoot   <- gets diskRoot
        diskTables <- gets diskTables
        liftIO $! S.writeFile  (diskRoot </> "_index") (encode diskTables)

    deleteTable name = withTable name $ \Table { tabName } -> do
        diskRoot <- gets diskRoot
        liftIO $! removeFile (diskRoot </> S.unpack tabName)

instance Query DiskBackend where
    selectAll name = do
        Table { tabName, tabRowSize } <- lift $ fetchTable name
        diskRoot <- lift $ gets diskRoot
        sourceFile (diskRoot </> S.unpack tabName) $=
            slice tabRowSize $= CL.map decoduit $= CL.catMaybes
      where
        decoduit :: S.ByteString -> Maybe Row
        decoduit bytes = case decode (unwrap bytes) of
            Right (Row { rowIsDeleted = True }) -> Nothing
            Right row -> Just row
            Left e    -> error e -- FIXME(Sergei): return a proper failure?

        slice :: Monad m => Int -> Conduit S.ByteString m S.ByteString
        slice n = go S.empty where
          go acc
              | S.length acc >= n =
                  let (a, b) = S.splitAt n acc in
                  yield a >> go b
              | otherwise = maybe (return ()) go =<< await

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        Table { tabCounter, tabRowSize } <- fetchTable name
        diskRoot <- gets diskRoot
        let bytes = wrap tabRowSize . encode $! row { rowId = Just tabCounter }
        liftIO $! S.appendFile (diskRoot </> S.unpack name) bytes
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return $! tabCounter
    insertInto _name _row = error "Inserting existing Row"

    update name exprs c = do
        table@(Table { tabName }) <- fetchTable name
        diskRoot <- gets diskRoot
        h <- liftIO $ IO.openBinaryFile (diskRoot </> S.unpack tabName) ReadWriteMode
        count <- select name (Projection []) c $=
                 CL.map (updateRow table exprs) $$
                 CL.foldM (\acc row -> rewriteRow h table row >> return (acc + 1)) 0
        liftIO $ IO.hClose h
        return count

    delete name c = do
        table@(Table { tabName, tabSize }) <- fetchTable name
        diskRoot <- gets diskRoot
        h <- liftIO $ IO.openBinaryFile (diskRoot </> S.unpack tabName) ReadWriteMode
        count <- select name (Projection []) c $=
                 CL.map (\r -> r { rowIsDeleted = True }) $$
                 CL.foldM (\acc row -> rewriteRow h table row >> return (acc + 1)) 0
        modifyTable name $ \_table ->
            -- FIXME(Sergei): we assume the state doesn't change during
            -- 'delete' operation, since no concurency is involved.
            table { tabSize = tabSize - count }
        return count

rewriteRow :: MonadIO m => IO.Handle -> Table -> Row -> m ()
rewriteRow h (Table { tabRowSize }) row
        | Row { rowId = Just rowId } <- row =
            let bytes  = wrap tabRowSize (encode row)
                -- Note(Sergei): rows are indexed from '1'.
                offset = fromIntegral $ tabRowSize * (rowId - 1)
            in liftIO $ do
                IO.hSeek h AbsoluteSeek offset
                S.hPut h bytes
        | otherwise = error "can't update row without row id"

-- | @wrap n s@ padds row chunk with a prefix of @\0\0...\xff@,
-- where @\xff@ starts data segment. A given chunk is expected to
-- be shorter than @n - 1@ to fit the @\xff@ tag.
wrap :: Int -> S.ByteString -> S.ByteString
wrap n s =
    let len = fromIntegral $ S.length s
        pad = fromIntegral $ n - 1 - len
    in case compare len (n - 1) of
        EQ -> S.cons '\xff' s
        LT -> S.append (S.replicate pad '\0') $ S.cons '\xff' s
        GT -> error "row chunk size overflow"

-- | @unwrap s@ removes padding, added by @wrap@.
unwrap :: S.ByteString -> S.ByteString
unwrap = S.tail . S.dropWhile (/= '\xff')

makeDiskBackend :: FilePath -> IO DiskBackend
makeDiskBackend root = do
    exists <- doesFileExist index
    tables <- if exists
              then do
                  bytes <- S.readFile index
                  case decode bytes of
                      Left err -> fail err
                      Right i  -> return i
              else return M.empty
    return $! DiskBackend root tables
  where
    index = root </> "_index"
