{-# LANGUAGE RecordWildCards, NamedFieldPuns, PatternGuards #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when, forM_)
import Data.List (findIndex)
import Data.Map (Map, (!))
import Data.Maybe (fromJust, isJust)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(..), SeekMode(..), BufferMode(..))
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified System.IO as IO

import Control.Monad.Error (throwError)
import Control.Monad.State (get, gets, modify)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Data.Serialize (encode, decode)
import Data.Conduit (Source, Conduit, ($=), ($$), addCleanup)
import Data.Default (def)
import qualified Data.Conduit.List as CL

import Data.Bro.Backend.Class (Backend(..), Query(..),
                               withTable, fetchTable, updateRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize, rangeToVal, wrap, unwrap)
import Data.Bro.Monad (Bro)
import Data.Bro.Types (TableName, IndexName, Table(..), Row(..), ColumnType(..),
                       Projection(..), Range, RowId, toIntegral, TableSchema,
                       TableIndex)
import Data.Bro.Condition (evalRange)
import Data.BTree   (BTree, BVal, BKey, btreeOpen, btreeClose, btreeFindRange,
                    btreeAdd, btreeEraseAll)

data DiskBackend = DiskBackend { diskRoot    :: FilePath
                               , diskTables  :: !(Map TableName Table)
                               , diskHandles :: !(Map TableName IO.Handle)
                               }

instance Backend DiskBackend where
    lookupTable name = M.lookup name <$> gets diskTables

    insertTable name schema = do
        res <- lookupTable name
        when (isJust res) $ throwError TableAlreadyExists
        modifyBackend $ \b@(DiskBackend { .. }) ->
            b { diskTables = M.insert name table diskTables }
        tabPath <- whereIs name
        liftIO $! IO.withBinaryFile tabPath WriteMode IO.hFlush
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
        DiskBackend { diskRoot, diskTables } <- get
        liftIO $! S.writeFile (diskRoot </> "_index") (encode diskTables)

    deleteTable name = withTable name $ \Table { tabName } -> do
        tabPath <- whereIs tabName
        liftIO $! removeFile tabPath

instance Query DiskBackend where
    selectAll (Table { .. }) cond = do
        let indexes = evalRange tabIndex cond
        rowIds  <- liftIO $ getRecFromIndex indexes
        tabPath <- lift $ whereIs tabName
        h <- liftIO $ IO.openBinaryFile tabPath ReadWriteMode
        lift $ modify (\b@(DiskBackend { diskHandles }) ->
                        b { diskHandles = M.insert tabName h diskHandles })
        liftIO $ IO.hSetBuffering h NoBuffering
        let sourceRows = sourceChunks h
                         (fromIntegral tabRowSize)
                         (if null rowIds
                          then replicate (fromIntegral $ tabCounter - 1) 0
                          else rowIds)
        addCleanup (\_done -> do
                         modify (\b@(DiskBackend { diskHandles }) ->
                                  b { diskHandles = M.delete tabName diskHandles })
                         liftIO $ IO.hClose h) $!
            sourceRows $= decoduit $= CL.catMaybes

    insertInto (Table { .. }) row@(Row { rowId = Nothing, .. }) = do
        tabPath <- whereIs tabName
        let newRow = row { rowId = Just tabCounter }
        let bytes  = wrap (fromIntegral tabRowSize) . encode $! newRow
        liftIO $! S.appendFile tabPath bytes
        withBtree tabSchema tabIndex newRow btreeAdd
        modifyTable tabName $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return tabCounter
    insertInto _name _row = error "Inserting existing Row"

    update table@(Table { tabName }) exprs c =
        select table (Projection []) c $=
            CL.map (updateRow table exprs) $$
            CL.foldM (\acc row -> do
                           h <- gets $ (! tabName) . diskHandles
                           rewriteRow h table row >> return (acc + 1)) 0

    delete table@(Table { .. }) c = do
        count <- select table (Projection []) c $=
                 CL.map (\r -> r { rowIsDeleted = True }) $$
                 CL.foldM (\acc row -> do
                                withBtree tabSchema tabIndex row $ \t k _v ->
                                    btreeEraseAll t k
                                h <- gets $ (! tabName) . diskHandles
                                rewriteRow h table row >> return (acc + 1)) 0
        modifyTable tabName $ \table -> table { tabSize = tabSize - count }
        return $ fromIntegral count

    createIndex table@(Table { .. }) indName colNames = forM_ colNames $ \col -> do
        case lookup col tabSchema of
            Nothing -> throwError ColumnDoesNotExist
            Just t | t /= IntegerColumn ->
                throwError $ ColumnTypeUnsupported t
            Just _t -> return ()

        when (isJust $ lookup col tabIndex) $
            throwError IndexAlreadyExists

        let fileName = S.intercalate "_" [tabName, indName, col]
        let colId = findIndex (\(n,_) -> n == col) tabSchema
        tree <- liftIO $ btreeOpen $ S.unpack fileName
        selectAll table Nothing $$ CL.mapM_ $ \r -> do
            let key = toIntegral $ (rowData r) !! (fromJust colId)
            liftIO $ btreeAdd tree key $ fromJust (rowId r)
        liftIO $ btreeClose tree

        modifyTable tabName $ \_table ->
            table { tabIndex = (col, fileName):tabIndex }

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

withBtree :: Backend b => TableSchema -> TableIndex -> Row ->
            (BTree -> BKey -> BVal -> IO ()) -> Bro BackendError b ()
withBtree schema index row f = forM_ index $ \(colName, path) -> do
            tree <- liftIO $ btreeOpen $ S.unpack path
            let colId = findIndex (\(n,_) -> n == colName) schema
            let key = toIntegral $ (rowData row) !! (fromJust colId)
            liftIO $ f tree key $ fromJust (rowId row)
            liftIO $ btreeClose tree

whereIs :: TableName -> Bro BackendError DiskBackend FilePath
whereIs name = (</> S.unpack name) <$> gets diskRoot
{-# INLINE whereIs #-}

-- | A source, which yields chunks of size @n@ from a given @handle@.
sourceChunks :: MonadIO m
             => IO.Handle -> Int -> [RowId] -> Source m S.ByteString
sourceChunks !h n rowIds = CL.sourceList rowIds $= CL.mapM f where
  f rowId =
      let offset = fromIntegral $ n * (fromIntegral rowId - 1) in
      liftIO $! do
          when (rowId /= 0) $! IO.hSeek h AbsoluteSeek offset
          S.hGet h n

-- | A conduit, which decodes @Row@ values from bytes of fixed length.
decoduit :: Monad m => Conduit S.ByteString m (Maybe Row)
decoduit = CL.map f where
  f !bytes = case decode (unwrap bytes) of
      Right (Row { rowIsDeleted = True }) -> Nothing
      Right row -> Just row
      Left e    -> error e -- FIXME(Sergei): return a proper failure?

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
    return $! DiskBackend root tables M.empty
  where
    index = root </> "_index"
