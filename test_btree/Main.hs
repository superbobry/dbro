module Main where

import BTree

main = do
    btree <- btreeCreate "BTREE.bin"
    btreeAdd btree 10 24
    btreeAdd btree 11 35
    btreeAdd btree 2 4
    btreeAdd btree 99 42
    ret <- btreeFind btree 10
    print $ show ret
    btreeEraseAll btree 11
    ret <- btreeFindRange btree 10 99 
    print $ show ret
    btreeClose btree
    return ()
