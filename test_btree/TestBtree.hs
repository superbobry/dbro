{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- #include "btree.h"

-- {# context lib="btree" #}

-- module BTreeTest where

-- import C2HS
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "btree.h create_btree"
	c_createBtree :: IO (Ptr CInt)

main :: IO ()
main = do
	c_createBtree
	return ()
