{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Binary (Binary, encodeFile, decodeFile, put, get,
                    encode, decode)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import System.Directory (doesFileExist, removeFile)
import System.IO (IOMode(..), SeekMode(..), withBinaryFile, openFile, hSeek, hClose)
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Control.Monad.Error (throwError, strMsg)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (liftIO)
import Data.Default (def)

import Data.Bro.Backend.Class (Backend(..), Query(..), withTable, fetchTable, transformRow)
import Data.Bro.Backend.Error (BackendError(..))
import Data.Bro.Backend.Util (rowSize)
import Data.Bro.Types (TableName, Table(..), Row(..), Projection(..))

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
    selectAll name =
        withTable name $ \(Table { tabSchema = (_, rowSize0), .. }) -> do
            diskRoot <- gets diskRoot
            bytes <- liftIO $! L.readFile (diskRoot </> S.unpack tabName)
            let rowSize1 = fromIntegral rowSize0
            when (L.length bytes `mod` rowSize1 /= 0) $
                throwError (strMsg "rows blob is not a multiple of row size")
            return $! go [] bytes rowSize1 tabSize
      where
        go :: [Row] -> L.ByteString -> Int64 -> Int -> [Row]
        go xs _bytes _rowSize1 0 = reverse xs
        go xs bytes rowSize1 i =
            case decode . unwrap $ L.take rowSize1 bytes of
                x@(Row { rowIsDeleted = True }) ->
                    -- Note(Sergei): skip deleted rows.
                    go xs (L.drop rowSize1 bytes) rowSize1 (i - 1)
                x ->
                    x `seq` go (x:xs) (L.drop rowSize1 bytes) rowSize1 (i - 1)

        unwrap :: L.ByteString -> L.ByteString
        unwrap = L.tail . L.dropWhile (/= 0xff)

    insertInto name row@(Row { rowId = Nothing, .. }) = do
        Table { tabCounter, tabSchema = (_, rowSize0) } <- fetchTable name
        diskRoot <- gets diskRoot
        let bytes = wrap rowSize0 . encode $! row { rowId = Just tabCounter }
        liftIO $! L.appendFile (diskRoot </> S.unpack name) bytes
        modifyTable name $ \table@(Table { tabSize }) ->
            table { tabCounter = tabCounter + 1, tabSize = tabSize + 1 }
        return $! tabCounter
    insertInto _name _row = error "Inserting existing Row"

    update name exprs c = do
        table@(Table { tabSchema = (_, rowSize0) }) <- fetchTable name
        rows <- select name (Projection []) c
        let newRows = map (transformRow table exprs) rows
        diskRoot <- gets diskRoot
        hTbl <- liftIO $! openFile (diskRoot </> S.unpack name) ReadWriteMode
        rewriteRows hTbl rowSize0 newRows
        liftIO $! hClose hTbl
        return $ length newRows

    delete name cond = do
        Table { tabSchema = (_, rowSize0) } <- fetchTable name
        rows <- select name (Projection []) cond
        let newRows = map (\r -> r { rowIsDeleted = True } ) rows
        diskRoot <- gets diskRoot
        hTbl <- liftIO $! openFile (diskRoot </> S.unpack name) ReadWriteMode
        rewriteRows hTbl rowSize0 newRows
        liftIO $! hClose hTbl
        return $ length newRows

rewriteRows handle rowSz (row:rows) = do
	let bytes = wrap rowSz (encode row)
	liftIO $! putStrLn (decode bytes)
	let offset = rowSz * ((fromJust $ rowId row) - 1) --seems that we have indexes from 1?
	writeBytes handle bytes (fromIntegral offset)
	rewriteRows handle rowSz rows
rewriteRows _ _ [] = return ()

writeBytes handle bytes off = do
	liftIO $! hSeek handle AbsoluteSeek off
	liftIO $! L.hPut handle bytes

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
    if exists
        then DiskBackend root <$> decodeFile index
        else return $! DiskBackend root M.empty
  where
    index = root </> "_index"
