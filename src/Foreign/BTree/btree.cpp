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

void btree_add(BTTableClass* tree, int32_t key, int32_t val) 
{
	ItemType item(key, val);
	try
	{
		tree->Insert(item);
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error inserting in btree: " << e.what() << std::endl;
	}
}

void btree_erase_all(BTTableClass* tree, int32_t key)
{
	try
	{
		tree->DeleteItem(KeyType(key));
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error deleting from btree: " << e.what() << std::endl;
	}
}

void btree_erase(BTTableClass* tree, int32_t key_tip, int32_t val)
{
	//tree->erase(key_tip, val);
}

int32_t btree_find(BTTableClass* tree, int32_t key)	
{
	ItemType data;
	bool found = false;
	try
	{
		found = tree->Retrieve(KeyType(key), data);
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error inserting in btree: " << e.what() << std::endl;
		return 0;
	}

	if (found)
	{
		return data.value;
	}
	else
	{
		return 0;
	}
}

int32_t* btree_find_range(BTTableClass* tree, int32_t begin, int32_t end)	
{
	std::vector<ItemType> items;
	try
	{
		tree->RetriveRange(KeyType(begin), KeyType(end), items);
	}
	catch (std::runtime_error& e)
	{
		std::cerr << "Error finding range in btree: " << e.what() << std::endl;
	}
	int32_t* data = new int32_t[items.size() + 1];
	for (size_t i = 0; i < items.size(); ++i)
	{
		data[i] = items[i].value;
	}
	data[items.size()] = 0;
	return data;
}

void btree_free(int32_t* ptr)
{
	delete[] ptr;
}
