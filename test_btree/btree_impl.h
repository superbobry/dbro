/* Filename:  btree.h

   Programmer:  Br. David Carlson

   Date:  November 2, 1997

   Modified:  October 25, 1999

   This is the header file to go with btree.cpp, an implementation of
   a B-tree-based table class.
*/
#include <fstream>

// comment off the following line if you want to omit debugging output:
// #define DEBUG

const int MaxKeys = 11;   // max number of keys in a node
const int MaxKeysPlusOne = MaxKeys + 1;
const int MinKeys = 5;    // min number of keys in a node
const long NilPtr = -1L;   // the L indicates a long int
const int KeyFieldMax = 4; 
const int KFMaxPlus1 = KeyFieldMax + 1; 
const int DataFieldMax = 4; 
const int DFMaxPlus1 = DataFieldMax + 1; 

typedef char KeyFieldType[KFMaxPlus1]; 
typedef char DataFieldType[DFMaxPlus1]; 
typedef struct 
{ 
	KeyFieldType KeyField; 
	DataFieldType DataField; 
} ItemType;

typedef struct
{
   int Count;               // Number of keys stored in the current node
   ItemType Key[MaxKeys];   // Warning: indexing starts at 0, not 1
   long Branch[MaxKeysPlusOne];   // Fake pointers to child nodes
} NodeType;

class BTTableClass
{
public:
    BTTableClass(const char * FileName);
    ~BTTableClass(void);
    bool Empty(void);
    bool Insert(const ItemType & Item);
    bool Retrieve(KeyFieldType SearchKey, ItemType & Item);
    void Dump(void);        // for debugging only - could be removed
    void Check(void);       // for debugging only
private:
    void CheckSubtree(long Current, KeyFieldType & Last); // for debugging
    bool SearchNode(const KeyFieldType Target, int & location) const;
    void AddItem(const ItemType & NewItem, long NewRight,
         NodeType & Node, int Location);
    void Split(const ItemType & CurrentItem, long CurrentRight,
         long CurrentRoot, int Location, ItemType & NewItem,
         long & NewRight);
    void PushDown(const ItemType & CurrentItem, long CurrentRoot,
         bool & MoveUp, ItemType & NewItem, long & NewRight);
    long Root;       // fake pointer to the root node
    long NumNodes;   // number of nodes in the B-tree
    int NodeSize;    // number of bytes per node
    NodeType CurrentNode;   // storage for current node being worked on

    std::fstream DataFile; // the file stream for the table data 
    long NumItems; // number of records in the table 
    char OpenMode;
};

