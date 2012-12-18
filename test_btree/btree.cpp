#include "btree.h"
#include <iostream>

btree* create_btree()
{
	std::cout << "Hello from cpp\n";
	return new btree("");
}

btree::btree(const char* path):m_Path(path) {}
void btree::add(key_type key, value_type val) {std::cout << "add: " << key << " " << val << std::endl;}
void btree::erase_all(key_type key) {std::cout << "erase_all: " << key << std::endl;}
void btree::erase(key_type key_tip, value_type val) {std::cout << "erase: " << key_tip << " " << val << std::endl;}
int	 btree::find(key_type key) {std::cout << "find: " << key << std::endl; return 0;}
int* btree::find_range(key_type begin, key_type end) {std::cout << "find_range: " << begin << " " << end << std::endl; return 0;}
void btree::print_path() {std::cout << "path: " << m_Path << std::endl;}
