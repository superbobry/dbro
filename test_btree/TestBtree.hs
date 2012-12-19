{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

--module BTree    (BTree,
--                btreeCreate,
--                btreeAdd,
--                btreeEraseAll,
--                btreeErase,
--                btreeFind,
--                btreeFindRange) where

import Foreign.C.Types (CInt)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)

type BTreeInst = ()
type BTree = Ptr BTreeInst
type BKey = Int
type BVal = Int

bNull :: BVal
bNull = 0

--FIXME: set proper sizeof Int
szInt :: Int
szInt = 4

foreign import ccall "btree.h btree_create"
    c_btreeCreate :: CString -> IO BTree

foreign import ccall "btree.h btree_add"
    c_btreeAdd :: BTree -> BKey -> BVal -> IO ()

foreign import ccall "btree.h btree_erase_all"
    c_btreeEraseAll :: BTree -> BKey -> IO ()

foreign import ccall "btree.h btree_erase"
    c_btreeErase :: BTree -> BKey -> BVal -> IO ()

foreign import ccall "btree.h btree_find"
    c_btreeFind :: BTree -> BKey -> IO BVal

foreign import ccall "btree.h btree_find_range"
    c_btreeFindRange :: BTree -> BKey -> BKey -> IO (Ptr BVal)

btreeCreate :: String -> IO BTree
btreeCreate path = withCString path c_btreeCreate

btreeAdd :: BTree -> BKey -> BVal -> IO ()
btreeAdd tree key val = c_btreeAdd tree key val

btreeEraseAll :: BTree -> BKey -> IO ()
btreeEraseAll tree key = c_btreeEraseAll tree key

btreeErase :: BTree -> BKey -> BVal -> IO ()
btreeErase tree key val = c_btreeErase tree key val

btreeFind :: BTree -> BKey -> IO BVal
btreeFind tree key = c_btreeFind tree key

btreeFindRange :: BTree -> BKey -> BKey -> IO [BVal]
btreeFindRange tree beg end = c_btreeFindRange tree beg end >>=
                              \p -> resultList p
                              where
                                resultList ptr = do
                                    val <- peek ptr
                                    if val /= bNull
                                        then resultList (plusPtr ptr szInt) >>= \l ->
                                            return $ val : l
                                        else return []
main = do
    btree <- withCString "path" c_btreeCreate
    btreeAdd btree 1 2
    btreeEraseAll btree 9
    btreeErase btree 2 3
    btreeFind btree 44
    ret <- btreeFindRange btree 1 2
    print $ show ret
    return ()
