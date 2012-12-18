all: TestBtree

TestBtree: libtree.a
	ghc --make TestBtree.hs -lstdc++ -L. -lbtree

libtree.a: btree.h btree.cpp
	g++ -c btree.cpp
	ar rvs libbtree.a btree.o

