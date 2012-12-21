#include <string>
#include "btree_impl.h"

extern "C"
{
	BTTableClass* btree_create(const char* path);
	void 		btree_add(BTTableClass* tree, int key, int val);
	void 		btree_erase_all(BTTableClass* tree, int key);
	void 		btree_erase(BTTableClass* tree, int key_tip, int val);
	int 		btree_find(BTTableClass* tree, int key);
	int* 		btree_find_range(BTTableClass* tree, int begin, int end);
	void		btree_close(BTTableClass* tree);
}
