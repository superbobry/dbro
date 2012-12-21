#include "btree.h"
#include <cstring>
#include <iostream>
#include <stdexcept>

///////////////////////////////////
BTTableClass* btree_create(const char* path)
{
	return new BTTableClass(path);
}

void btree_close(BTTableClass* tree)
{
	delete tree;
}

void btree_add(BTTableClass* tree, int key, int val) 	
{
	ItemType item; 
	memset(item.KeyField, 0, sizeof(ItemType::KeyField));
	memcpy(item.KeyField, reinterpret_cast<char*>(&key), sizeof(int)); 
	memset(item.DataField, 0, sizeof(ItemType::DataField));
	memcpy(item.DataField, reinterpret_cast<char*>(&val), sizeof(int)); 
	try
	{
		tree->Insert(item);
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error inserting in btree: " << e.what() << std::endl;
	}
}

void btree_erase_all(BTTableClass* tree, int key) 						
{
	//tree->erase_all(key);
}

void btree_erase(BTTableClass* tree, int key_tip, int val)	
{
	//tree->erase(key_tip, val);
}

int btree_find(BTTableClass* tree, int key)						
{
	KeyFieldType treekey; 
	memset(treekey, 0, sizeof(KeyFieldType));
	memcpy(treekey, reinterpret_cast<char*>(&key), sizeof(int)); 

	ItemType data;
	bool found = false;
	try
	{
		found = tree->Retrieve(treekey, data);
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error inserting in btree: " << e.what() << std::endl;
		return 0;
	}

	if (found)
	{
		int ret = *(reinterpret_cast<int*>(data.DataField));
		return ret;
	}
	else
	{
		return 0;
	}
}

int* btree_find_range(BTTableClass* tree, int begin, int end)	
{
	return new int(0);
	//return tree->find_range(begin, end);
}
