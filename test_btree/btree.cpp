#include "btree.h"
#include <iostream>



btree::btree(const char* path):m_Path(path) {}
void btree::add(key_type key, value_type val) 
{
	std::cout << "add: " << key << " " << val << " ";
	std::cout << m_Path << std::endl;
}

void btree::erase_all(key_type key) 
{
	std::cout << "erase_all: " << key << std::endl;
}

void btree::erase(key_type key_tip, value_type val) 
{
	std::cout << "erase: " << key_tip << " " << val << std::endl;
}

int	 btree::find(key_type key) 
{
	std::cout << "find: " << key << std::endl; 
	return 42;
}

int* btree::find_range(key_type begin, key_type end) 
{
    int* ret = new int[3];
    ret[0] = 42;
    ret[1] = 43;
    ret[2] = 0;
	std::cout << "find_range: " << begin << " " << end << std::endl; 
    return ret;
}

void btree::print_path() 
{
	std::cout << "path: " << m_Path << std::endl;
}

///////////////////////////////////
btree* btree_create(const char* path)
{
	std::cout << "Hello from cpp\n";
	return new btree(path);
}

void btree_add(btree* tree, btree::key_type key, btree::value_type val) 	
{
	tree->add(key, val);
}

void btree_erase_all(btree* tree, btree::key_type key) 						
{
	tree->erase_all(key);
}

void btree_erase(btree* tree, btree::key_type key_tip, btree::value_type val)	
{
	tree->erase(key_tip, val);
}

btree::value_type btree_find(btree* tree, btree::key_type key)						
{
	return tree->find(key);
}

btree::value_type* btree_find_range(btree* tree, btree::key_type begin, btree::key_type end)	
{
	return tree->find_range(begin, end);
}
