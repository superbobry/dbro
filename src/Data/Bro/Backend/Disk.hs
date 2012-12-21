{-# LANGUAGE RecordWildCards, NamedFieldPuns, PatternGuards #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Serialize (Serialize, put, get, encodeLazy, decodeLazy)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(..), SeekMode(..))
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified System.IO as IO

import Control.Monad.Error (throwError, strMsg)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (liftIO)
import Data.Default (def)

import Data.Bro.Backend.Class (Backend(..), Query(..), withTable, fetchTable, transformRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize)
import Data.Bro.Monad (Bro)
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
            (diskRoot </> S.unpack name) WriteMode (\_ -> return ())
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
        liftIO $! L.writeFile  (diskRoot </> "_index") (encodeLazy diskTables)

    deleteTable name = withTable name $ \Table { tabName } -> do
        diskRoot <- gets diskRoot
        liftIO $! removeFile (diskRoot </> S.unpack tabName)

instance Query DiskBackend where
    selectAll name =
        withTable name $ \(Table { .. }) -> do
            diskRoot <- gets diskRoot
            bytes <- liftIO $! L.readFile (diskRoot </> S.unpack tabName)
            let rowSize1 = fromIntegral tabRowSize
            when (L.length bytes `mod` rowSize1 /= 0) $
                throwError (strMsg "rows blob is not a multiple of row size")
            return $! go [] bytes rowSize1 tabSize
      where
        go :: [Row] -> L.ByteString -> Int64 -> Int -> [Row]
        go rows _bytes _rowSize1 0 = reverse rows
        go rows bytes rowSize1 i =
            case decodeLazy . unwrap $ L.take rowSize1 bytes of
                Right (Row { rowIsDeleted = True }) ->
                    -- Note(Sergei): skip deleted rows.
                    go rows (L.drop rowSize1 bytes) rowSize1 (i - 1)
                Right row ->
                    row `seq` go (row:rows) (L.drop rowSize1 bytes) rowSize1 (i - 1)
                Left e -> error e

        unwrap :: L.ByteString -> L.ByteString
        unwrap = L.tail . L.dropWhile (/= 0xff)

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        Table { tabCounter, tabRowSize } <- fetchTable name
        diskRoot <- gets diskRoot
        let bytes = wrap tabRowSize . encodeLazy $! row { rowId = Just tabCounter }
        liftIO $! L.appendFile (diskRoot </> S.unpack name) bytes
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return $! tabCounter
    insertInto _name _row = error "Inserting existing Row"

    update name exprs c = do
        table <- fetchTable name
        rows  <- map (transformRow table exprs) <$> select name (Projection []) c
        rewriteRows table rows

    delete name c = do
        table <- fetchTable name
        rows  <- map (\r -> r { rowIsDeleted = True }) <$>
                 select name (Projection []) c
        affected <- rewriteRows table rows
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabSize = tabSize - affected }
        return $! affected

rewriteRows :: Table -> [Row] -> Bro BackendError DiskBackend Int
rewriteRows table@(Table { tabName }) rows = do
    diskRoot <- gets diskRoot
    liftIO $! IO.withBinaryFile
        (diskRoot </> S.unpack tabName)
        ReadWriteMode
        (\h -> mapM_ (go h table) rows >> return (length rows))
  where
    go :: IO.Handle -> Table -> Row -> IO ()
    go h (Table { tabRowSize }) row
        | Row { rowId = Just rowId0 } <- row =
            let bytes  = wrap tabRowSize (encodeLazy row)
                -- Note(Sergei): rows are indexed from '1'.
                offset = fromIntegral $ tabRowSize * (rowId0 - 1)
            in do
                IO.hSeek h AbsoluteSeek offset
                L.hPut h bytes
        | otherwise = error "can't update row without row id"

-- | @wrap n s@ padds row chunk with a prefix of @\0\0...\xff@,
-- where @\xff@ starts data segment. A given chunk is expected to
-- be shorter than @n - 1@ to fit the @\xff@ tag.
wrap :: Int -> L.ByteString -> L.ByteString
wrap n s =
    let len = fromIntegral $ L.length s
        pad = fromIntegral $ n - 1 - len
    in case compare len (n - 1) of
        EQ -> L.cons 0xff s
        LT -> L.append (L.replicate pad 0) $ L.cons 0xff s
        GT -> error "row chunk size overflow"

makeDiskBackend :: FilePath -> IO DiskBackend
makeDiskBackend root = do
    exists <- doesFileExist index
    tables <- if exists
              then do
                  bytes <- L.readFile index
                  case decodeLazy bytes of
                      Left err -> fail err
                      Right i  -> return i
              else return M.empty
    return $! DiskBackend root tables
  where
    index = root </> "_index"
