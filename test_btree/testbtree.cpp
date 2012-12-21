#include "btree_impl.h"

int main()
{
	BTTableClass tree("tree.bin");
	for (int i = 0; i < 10000; ++i)
	{
		int key = i;
			
		ItemType item;
		int val = rand() % 1000;
		memset(item.KeyField, 0, sizeof(ItemType::KeyField));
		memcpy(item.KeyField, reinterpret_cast<char*>(&key), sizeof(int)); 
		memset(item.DataField, 0, sizeof(ItemType::DataField));
		memcpy(item.DataField, reinterpret_cast<char*>(&val), sizeof(int)); 
		
		std::cout << "Inserting " << key << " " << val << std::endl;
		tree.Insert(item);

		KeyFieldType treekey; 
		memset(treekey, 0, sizeof(KeyFieldType));
		memcpy(treekey, reinterpret_cast<char*>(&key), sizeof(int)); 

		ItemType data;
		assert(tree.Retrieve(treekey, data));
	
		int ret = *(reinterpret_cast<int*>(data.DataField));
		
		std::cout << "found " << key << " " << ret << std::endl;

	}
	return 0;
}
