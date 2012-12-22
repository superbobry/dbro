#include "btree_impl.h"
#include <iostream>
#include <cstring>
#include <cassert>
#include <cstdlib>

int main()
{
    std::cout << sizeof(ItemType) << " " << sizeof(NodeType) << std::endl;
	BTTableClass tree("tree.bin");
     
	for (int i = 0; i < 1000000; ++i)
	{
		ItemType item;
		item.key = i;
		item.value = i;
				
		std::cout << "inserting " << i << " " << i << std::endl;
		tree.Insert(item);
    }
    for (int i = 0; i < 1000000; ++i)
	{
		KeyType treekey = i;

		ItemType data;
		tree.Retrieve(treekey, data);
	
		std::cout << "found " << i << " " << data.value << std::endl;
	}
    for (int i = 0; i < 500000; ++i)
    {
        std::cout << "deleting " << 2 * i << std::endl;
        tree.DeleteItem(2 * i);
    }
	//range test
	//tree.Dump();
	KeyType lkey = 1000;
	KeyType rkey = 3000; 

	std::vector<ItemType> found;
	tree.RetriveRange(lkey, rkey, found);
	std::cout << "found in range " << lkey << " -> " << rkey << std::endl;
	for (size_t i = 0; i < found.size(); ++i)
	{
		std::cout << found[i].key << " " << found[i].value << std::endl;
	}
	return 0;
}
