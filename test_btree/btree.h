#include <string>
#include <stdint.h>
#include "btree_impl.h"

extern "C"
{
	BTTableClass* btree_create(const char* path);
	void 		btree_add(BTTableClass* tree, int32_t key, int32_t val);
	void 		btree_erase_all(BTTableClass* tree, int32_t key);
	void 		btree_erase(BTTableClass* tree, int32_t key_tip, int32_t val);
	int32_t 	btree_find(BTTableClass* tree, int32_t key);
	int32_t* 	btree_find_range(BTTableClass* tree, int32_t begin, int32_t end);
	void		btree_close(BTTableClass* tree);
    void        btree_free(int32_t* ptr);
}
