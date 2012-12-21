module Main where

import BTree

main = do
    btree <- btreeCreate "BTREE.bin"
    btreeAdd btree 10 24
    --btreeEraseAll btree 9
    --btreeErase btree 2 3
    ret <- btreeFind btree 10
    --ret <- btreeFindRange btree 1 2
    print $ show ret
    btreeClose btree
    return ()
