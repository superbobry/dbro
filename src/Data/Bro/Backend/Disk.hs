{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Binary (Binary, encodeFile, decodeFile, put, get,
                    encode, decode)
import Data.Map (Map)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(..), withBinaryFile)
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Control.Monad.Error (throwError)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (liftIO)
import Data.Default (def)

import Data.Bro.Backend.Class (Backend(..), Query(..), withTable, fetchTable)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize)
import Data.Bro.Types (TableName, Table(..), Row(..))

data DiskBackend = DiskBackend { diskRoot   :: FilePath
                               , diskTables :: !(Map TableName Table)
                               }

instance Binary DiskBackend where
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
        liftIO $! withBinaryFile
            (diskRoot </> S.unpack name) WriteMode (\_ -> return ())
      where
        table :: Table
        table = def { tabName = name
                    , tabSchema = (schema, rowSize schema)
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
        liftIO $! encodeFile (diskRoot </> "_index") diskTables

    deleteTable name = withTable name $ \Table { tabName } -> do
        diskRoot <- gets diskRoot
        liftIO $! removeFile (diskRoot </> S.unpack tabName)

instance Query DiskBackend where
    selectAll name = withTable name $ \table@(Table { tabName, tabSize }) -> do
        diskRoot <- gets diskRoot
        liftIO $! do
            bytes <- L.readFile $ diskRoot </> S.unpack tabName
            return $! go [] bytes table tabSize
      where
        go :: [Row] -> L.ByteString -> Table -> Int -> [Row]
        go xs _bytes _table 0 = reverse xs
        go xs bytes table@(Table { tabSchema = (_, rowSize0) }) i =
            let rowSize1 = fromIntegral rowSize0
                x = decode $ L.take rowSize1 bytes
            in x `seq` go (x:xs) (L.drop rowSize1 bytes) table (i - 1)

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        Table { tabCounter } <- fetchTable name
        diskRoot <- gets diskRoot
        liftIO $! do
            let bytes = encode $! row { rowId = Just tabCounter }
            L.appendFile (diskRoot </> S.unpack name) bytes
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return $! tabCounter
    insertInto _name _row = error "Inserting existing Row"

makeDiskBackend :: FilePath -> IO DiskBackend
makeDiskBackend root = do
    exists <- doesFileExist index
    if exists
        then DiskBackend root <$> decodeFile index
        else return $! DiskBackend root M.empty
  where
    index = root </> "_index"
