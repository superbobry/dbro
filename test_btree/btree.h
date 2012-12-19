#include <string>

class btree
{
public:
	typedef int key_type;
	typedef int value_type;

	btree(const char* path);
	void add(key_type key, value_type val);
	void erase_all(key_type key);
	void erase(key_type key_tip, value_type val);
	int	 find(key_type key);
	int* find_range(key_type begin, key_type end);
	void print_path();


private:
	std::string m_Path;

};

extern "C"
{
	btree* 		btree_create(const char* path);
	void 		btree_add(btree* tree, btree::key_type key, btree::value_type val);
	void 		btree_erase_all(btree* tree, btree::key_type key);
	void 		btree_erase(btree* tree, btree::key_type key_tip, btree::value_type val);
	btree::value_type 	btree_find(btree* tree, btree::key_type key);
	btree::value_type* 	btree_find_range(btree* tree, btree::key_type begin, btree::key_type end);
}
