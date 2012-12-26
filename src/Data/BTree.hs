{-# LANGUAGE ForeignFunctionInterface, TypeSynonymInstances #-}

module Data.BTree
  ( BTree
  , BKey
  , BVal
  , btreeOpen
  , btreeClose
  , btreeAdd
  , btreeEraseAll
  , btreeErase
  , btreeFind
  , btreeFindRange
  ) where

--import Foreign.C.Types (CInt)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import Data.Int (Int32)

type BTreeInst = ()
type BTree = Ptr BTreeInst
type BKey = Int32
type BVal = Int32

bNull :: BVal
bNull = 0

szInt :: Int
szInt = sizeOf (undefined::Int32)

foreign import ccall "btree.h btree_create"
    c_btreeCreate :: CString -> IO BTree

foreign import ccall "btree.h btree_add"
    c_btreeAdd :: BTree -> BKey -> BVal -> IO ()

foreign import ccall "btree.h btree_erase_all"
    c_btreeEraseAll :: BTree -> BKey -> IO ()

--foreign import ccall "btree.h btree_erase"
--    c_btreeErase :: BTree -> BKey -> BVal -> IO ()

foreign import ccall "btree.h btree_find"
    c_btreeFind :: BTree -> BKey -> IO BVal

foreign import ccall "btree.h btree_find_range"
    c_btreeFindRange :: BTree -> BKey -> BKey -> IO (Ptr BVal)

foreign import ccall "btree.h btree_close"
    c_btreeClose :: BTree -> IO ()

foreign import ccall "btree.h btree_free"
    c_btreeFree :: (Ptr BVal) -> IO ()

btreeOpen :: String -> IO BTree
btreeOpen path = withCString path c_btreeCreate

btreeClose :: BTree -> IO ()
btreeClose tree = c_btreeClose tree

btreeAdd :: BTree -> BKey -> BVal -> IO ()
btreeAdd tree key val = c_btreeAdd tree key val

btreeEraseAll :: BTree -> BKey -> IO ()
btreeEraseAll tree key = c_btreeEraseAll tree key

btreeErase :: BTree -> BKey -> BVal -> IO ()
btreeErase _tree _key _val = undefined --c_btreeErase tree key val (not implemented yet)

btreeFind :: BTree -> BKey -> IO (Maybe BVal)
btreeFind tree key = c_btreeFind tree key >>=
                        \f -> return $ if f /= bNull then Just f else Nothing

btreeFindRange :: BTree -> BKey -> BKey -> IO [BVal]
btreeFindRange tree beg end = do
    ptr <- c_btreeFindRange tree beg end
    list <- resultList ptr
    c_btreeFree ptr
    return list
  where
    resultList ptr = do
        val <- peek ptr
        if val /= bNull
            then resultList (plusPtr ptr szInt) >>= \l ->
                return $ val : l
            else return []
