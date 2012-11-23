{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, decodeFile, put, get)
import Data.Map (Map)
import System.Directory (removeFile)
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as S

import Control.Monad.State (gets)
import Control.Monad.Trans (liftIO)

import Data.Bro.Backend.Class (Backend(..), withTable)
import Data.Bro.Types (TableName, TableSchema, Table(..))

data DiskBackend = DiskBackend { dbRoot  :: FilePath
                               , dbIndex :: Map TableName TableSchema
                               }

instance Binary DiskBackend where
    put (DiskBackend { .. }) = do
        put dbRoot
        put dbIndex

    get = DiskBackend <$> get <*> get

instance Backend DiskBackend where
    lookupTable = undefined

    insertTable = undefined

    modifyTable = undefined

    deleteTable name = withTable name $ \Table { tabName } -> do
        root <- gets dbRoot
        liftIO $! removeFile (root </> S.unpack tabName)

makeDiskBackend :: FilePath -> IO DiskBackend
makeDiskBackend root =
    DiskBackend root <$> decodeFile (root </> "_index")