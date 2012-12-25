/* Filename:  btree.h

   Programmer:  Br. David Carlson

   Date:  November 2, 1997

   Modified:  October 25, 1999

   This is the header file to go with btree.cpp, an implementation of
   a B-tree-based table class.
*/
#include <fstream>
#include <vector>
#include <stdint.h>
#include <cstring>

// comment off the following line if you want to omit debugging output:
// #define DEBUG

const int   MAX_KEYS    = 314;            // max number of keys in a node
const int   MAX_KEYS_P1 = MAX_KEYS + 1;
const int   MIN_KEYS    = 150;            // min number of keys in a node
const long  NIL_PTR     = -1L;            // the L indicates a long int

typedef int32_t KeyType;
typedef int32_t ValueType;

struct ItemType
{
    ItemType(KeyType _key = 0, ValueType _value = 0): key(_key), value(_value) {}
    KeyType     key;
    ValueType   value;
};

struct NodeType
{
    NodeType()  {memset(Deleted, 0, MAX_KEYS);}
    int32_t  Count;                     // Number of keys stored in the current node
    ItemType Key[MAX_KEYS];              // Warning: indexing starts at 0, not 1
    int32_t  Branch[MAX_KEYS_P1];    // Fake pointers to child nodes
    bool     Deleted[MAX_KEYS];          // List of deleted nodes
    char     _unused_byte[3];           // For alignment: structure size should be 4096 bytes
};

class BTTableClass
{
public:
    BTTableClass(const char * FileName);
    ~BTTableClass(void);
    bool Empty(void);
    bool Insert(const ItemType& Item);
    void DeleteItem(const KeyType& Key);
    bool Retrieve(KeyType SearchKey, ItemType& Item);
    void RetriveRange(KeyType keyBegin, KeyType keyEnd, std::vector<ItemType>& items);
    void Dump(void);            // for debugging only - could be removed
    //void Check(void);         // for debugging only
private:
    //void CheckSubtree(long Current, KeyType & Last); // for debugging
    bool SearchNode(const KeyType& Target, NodeType& node, int& location) const;
    void AddItem(const ItemType & NewItem, long NewRight,
         NodeType & Node, int Location);
    void Split(const ItemType & CurrentItem, long CurrentRight,
         long CurrentRoot, int Location, ItemType & NewItem,
         long & NewRight);
    void PushDown(const ItemType & CurrentItem, long CurrentRoot,
                    bool & MoveUp, ItemType & NewItem, long & NewRight);
    void findRangeRecursive(long nodeId, KeyType keyBegin, KeyType keyEnd,
                            std::vector<ItemType>& items);

    long m_Root;                // fake pointer to the root node
    long m_NumNodes;            // number of nodes in the B-tree
    int  m_NodeSize;            // number of bytes per node
    NodeType m_CurrentNode;     // storage for current node being worked on

    std::fstream m_DataFile;    // the file stream for the table data
    long m_NumItems;            // number of records in the table
};

