{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bro.Backend.Disk
  ( DiskBackend
  , makeDiskBackend
  ) where

import Data.Binary (Binary, decodeFile)
import Data.Map (Map)

import Data.Bro.Backend.Class (Backend(..))
import Data.Bro.Types (TableName, TableSchema)

newtype DiskBackend = DiskBackend (Map TableName TableSchema) deriving Binary

instance Backend DiskBackend where
    lookupTable = undefined

    insertTable = undefined

    modifyTable = undefined

    deleteTable = undefined


makeDiskBackend :: FilePath -> IO DiskBackend
makeDiskBackend = decodeFile