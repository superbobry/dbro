{-# LANGUAGE RecordWildCards, NamedFieldPuns, PatternGuards, OverloadedStrings #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_)
import Data.Int (Int32)
import Data.List (findIndex)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
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
import Data.Conduit (Conduit, ($=), ($$), await, yield, addCleanup)
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.Lazy (lazyConsume)
import Data.Default (def)
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Class (Backend(..), Query(..),
                               withTable, fetchTable, updateRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize, rangeToVal, wrap, unwrap)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, IndexName, Table(..), Row(..), ColumnType(..),
                       Projection(..), Range, RowId, toIntegral)
import Data.Bro.Condition (evalRange)
import Data.BTree (BTree, BVal, btreeOpen, btreeClose, btreeFindRange, btreeAdd)

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
    selectAll name cond = do
        Table { tabName, tabRowSize, tabIndex } <- lift $ fetchTable name
        --liftIO $ print tabIndex
        let indexes = evalRange tabIndex cond
        --liftIO $ print $ indexes
        rIds <- lift $ liftIO $ getRecFromIndex indexes
        --liftIO $ print rIds
        diskRoot <- lift $ gets diskRoot
        if null rIds
            then sourceFile (diskRoot </> S.unpack tabName) $=
                slice (fromIntegral tabRowSize) $= CL.map decoduit $= CL.catMaybes
            else do
                h <- liftIO $ IO.openBinaryFile (diskRoot </> S.unpack tabName) ReadMode
                addCleanup (\_done -> liftIO $ IO.hClose h) $ CL.sourceList rIds $=
                    CL.mapM (readChunk h tabRowSize)
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
              | otherwise = await >>= maybe (return ()) (go . S.append acc)

        readChunk :: Backend b => IO.Handle -> Int32 -> RowId -> Bro BackendError b Row
        readChunk h tSize rId = do
            let offset = tSize * (rId - 1)
            liftIO $ IO.hSeek h AbsoluteSeek (fromIntegral offset)
            bytes <- liftIO $ S.hGet h (fromIntegral tSize)
            case decode (unwrap bytes) of
                Right row   -> return row
                Left e      -> error e

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        Table { tabCounter, tabRowSize } <- fetchTable name
        diskRoot <- gets diskRoot
        let bytes = wrap (fromIntegral tabRowSize) . encode $! row { rowId = Just tabCounter }
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
        return $ fromIntegral count

    createIndex tName indName colNames = do
        table@(Table { tabIndex, tabSchema }) <- fetchTable tName
        forM_ colNames $ \col -> do
            case lookup col tabSchema of
                Nothing -> throwError ColumnDoesNotExist
                Just t | t /= IntegerColumn ->
                    throwError $ ColumnTypeUnsupported t
                Just _t -> return ()

            when (isJust $ lookup col tabIndex) $
                throwError IndexAlreadyExists

            let fileName = S.intercalate "_" [tName, indName, col]
            let colId = findIndex (\(n,_) -> n == col) tabSchema
            insertIntoBtree (S.unpack fileName) tName $ fromJust colId
            modifyTable tName $ \_table ->
                table { tabIndex = (col, fileName):tabIndex }

insertIntoBtree :: (Backend b, Query b) => FilePath -> TableName ->
                                            Int -> Bro BackendError b ()
insertIntoBtree file tName colId = do
    tree <- liftIO $ btreeOpen file
    rows <- lazyConsume $ selectAll tName Nothing
    liftIO $ go tree rows
    liftIO $ btreeClose tree
    return ()
      where
        go tree (row:rows) = do
            let val = toIntegral $ (rowData row) !! colId
            btreeAdd tree val $ fromJust (rowId row)
            go tree rows
        go _tree [] = return ()

rewriteRow :: MonadIO m => IO.Handle -> Table -> Row -> m ()
rewriteRow h (Table { tabRowSize }) row
        | Row { rowId = Just rowId } <- row =
            let bytes  = wrap (fromIntegral tabRowSize) (encode row)
                -- Note(Sergei): rows are indexed from '1'.
                offset = fromIntegral $ tabRowSize * (rowId - 1)
            in liftIO $ do
                IO.hSeek h AbsoluteSeek offset
                S.hPut h bytes
        | otherwise = error "can't update row without row id"

-- TODO: return as source
getRecFromIndex :: [(IndexName, Range)] -> IO [RowId]
getRecFromIndex ((n, r):indexes) = do
    tree <- btreeOpen $ S.unpack n
    lst <- getSeq tree r
    btreeClose tree
    lst' <- getRecFromIndex indexes
    return $ lst ++ lst'
    where
      getSeq :: BTree -> Range -> IO [BVal]
      getSeq tree ((rl, rr):ranges) = do
          --print $ rangeToVal rl
          --print $ rangeToVal rr
          res <- btreeFindRange tree (rangeToVal rl) (rangeToVal rr)
          --print res
          res' <- getSeq tree ranges
          return $ res ++ res'
      getSeq _tree [] = return []
getRecFromIndex [] = return []

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
