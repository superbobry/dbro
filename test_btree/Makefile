all: TestBtree

TestBtree: libtree.a
	ghc --make Btree.hs Main.hs -lstdc++ -L. -lbtree

libtree.a: btree.h btree.cpp
	g++ -c btree.cpp
	g++ -c btree_impl.cpp
	ar rvs libbtree.a btree.o btree_impl.o

