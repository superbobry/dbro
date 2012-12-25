/* Filename:  btree.cpp

   Programmer:  Br. David Carlson

   Reference:  Data Structures & Program Design, 2nd ed., by Robert L.
   Kruse.

   Date:  November 4, 1997

   Modified:  October 25, 1999 and June 27, 2000

   Modified:  March 20, 2001 to fix NumItems counter.

   This file contains the implementation of the functions of the
   BTTableClass as set up in btree.h.
*/

#include "btree_impl.h"
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <algorithm>
#include <sys/stat.h>

using namespace std;

/* Given:  msg   A message.
   Task:   To print msg and exit the program.
   Return: Nothing to the program, of course, but returns 1 to the
           operating system.
*/
namespace
{
    //class ItemExists{};
    void Error(const char * msg)
    {
        throw std::runtime_error(msg);
    }

    struct Compare
    {
        bool operator()(const ItemType& k1, const ItemType& k2) {return k1.key < k2.key;}
    };
}


/* Given:   Nothing (other than the implicit BTTableClass object)
   Task:    To print out all info associated with the current table.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Nothing.
*/
void BTTableClass::Dump(void)
{
   int k;
   long p;

   cout << endl << "Root is node (record) number " << m_Root << endl;

   for (p = 0; p <= m_NumNodes; p++)
   {
      if (p % 4 == 3)
      {
         cout << " Press ENTER";
         cin.get();
      }

      m_DataFile.seekg(p * m_NodeSize, ios::beg);
      m_DataFile.read(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

      if (p == 0)
      {
         cout << "Node 0 is not part of tree, contains this data:" << endl;
         cout << "   NumItems = " << m_CurrentNode.Branch[0] << endl;
         cout << "   NumNodes = " << m_CurrentNode.Branch[1] << endl;
         cout << "   Root = " << m_CurrentNode.Branch[2] << endl;
      }
      else
      {
         cout << "Dump of node number " << p << endl;
         cout << "   Count: " << m_CurrentNode.Count << endl;

         cout << "   Keys: ";
         for (k = 0; k < m_CurrentNode.Count; k++)
         {
             //int rKey = *(reinterpret_cast<int*>(m_CurrentNode.Key[k].KeyField));
             cout << m_CurrentNode.Key[k].key << " ";
         }

         cout << endl << "   Branches: ";
         for (k = 0; k <= m_CurrentNode.Count; k++)
            cout << m_CurrentNode.Branch[k] << " ";
         cout << endl << endl;
      }
   }
}


/* Given:   Nothing (other than the implicit BTTableClass object)
   Task:    To do an inorder traversal of the B-Tree looking for out of
            order items.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Nothing.
*/
/*
void BTTableClass::Check(void)
{
   KeyType Last;

   //Last[0] = '*';
   //Last[1] = '\0';
   CheckSubtree(m_Root, Last);
}
*/

/* Given:   The implicit BTTableClass object plus:
            Current   A pseudopointer to the root node of the subtree.
            Last      The Last key field value that was checked.
   Task:    To do an inorder traversal of the subtree rooted at the
            current node.  Each key field value is checked against Last
            to see if it is out of order relative to Last.  If so,
            debugging info is printed, including a complete dump of
            the B-tree.
            Note that this is for debugging purposes.  This function
            could be removed once debugging is complete.
   Return:  Nothing.
*/
/*
void BTTableClass::CheckSubtree(long Current, KeyFieldType & Last)
{
   NodeType Node;
   int k;

   if (Current == NilPtr)
      return;

   m_DataFile.seekg(Current * m_NodeSize, ios::beg);
   m_DataFile.read(reinterpret_cast <char *> (&Node), m_NodeSize);
   for (k = 0; k < Node.Count; k++)
   {
      CheckSubtree(Node.Branch[k], Last);
      if ((Last[0] != '*') && (strcmp(Last, Node.Key[k].KeyField) >= 0))
      {
         cout << "Check has found a problem in node " << Current <<
            " index " << k << " key " << Node.Key[k].KeyField << endl;
         Dump();
         exit(1);
      }
      strcpy(Last, Node.Key[k].KeyField);
   }
   CheckSubtree(Node.Branch[Node.Count], Last);
}
*/

/* Given:   Mode      A char(r or w) to indicate read or write mode.
            FileName  A char string holding the external filename.
   Task:    This is the constructor for a BTTableClass object.  If mode
            r is specified, it opens the table stored in the given file
            for reading.  If w is specified, it opens a new, empty table
            for writing (to the given file).  A new empty table contains
            a "dummy" node (node zero) that will be used to hold info
            about the whole table.
   Return:  Nothing directly, but the implicit object is created.
*/
BTTableClass::BTTableClass(const char * FileName)
{
	m_NodeSize = sizeof(NodeType);

	struct stat buf;
    if (stat(FileName, &buf) == -1)
	{
		std::ofstream file(FileName);
	}

	m_DataFile.open(FileName, ios::in | ios::out | ios::binary);
	m_DataFile.read(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

    if (m_DataFile.fail())
    {   
		// assume the Btree is empty if you cannot read from the file
		m_DataFile.clear();
        m_NumItems = 0;
        m_NumNodes = 0;
        m_Root = NIL_PTR;
		m_CurrentNode.Branch[0] = m_NumItems;
		m_CurrentNode.Branch[1] = m_NumNodes;
		m_CurrentNode.Branch[2] = m_Root;
			
		m_DataFile.seekp(0, ios::beg);
       	m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);
	}
	else
	{
       	m_NumItems = m_CurrentNode.Branch[0];
       	m_NumNodes = m_CurrentNode.Branch[1];
       	m_Root = m_CurrentNode.Branch[2];
	}
}


/* Given:   Nothing (other than the implicit object).
   Task:    This is the destructor for a BTTableClass object.  Its job
            is to destroy the BTTableClass object, while making sure that
            all of the table data is stored in the associated file.
   Return:  Nothing directly, but the file is updated.
*/
BTTableClass::~BTTableClass(void)
{
   #ifdef DEBUG
      cout << endl << "BTTableClass destructor called" << endl;
   #endif

   //  Be sure to write out the updated node zero:
   m_CurrentNode.Branch[0] = m_NumItems;
   m_CurrentNode.Branch[1] = m_NumNodes;
   m_CurrentNode.Branch[2] = m_Root;
   m_DataFile.seekp(0, ios::beg);
   m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

   #ifdef DEBUG
	  cout << "W";
   #endif

   #ifdef DEBUG
      Dump();
   #endif
}


/* Given:   Nothing (other than the implicit object).
   Task:    To decide if the implicit table object is empty.
   Return:  In the function name, true if the table object is empty,
            false otherwise.
*/
bool BTTableClass::Empty(void)
{   // we could read node zero, but this is faster:
   return (m_Root == NIL_PTR);
}


/* Given:   The implicit BTTableClass object as well as:
            Target        The value to look for in the CurrentNode field.
   Task:    To look for Target as a key in CurrentNode.
   Return:  In the function name, return true if found, false otherwise.
            Location      The index of where Target was found.  If not
                          found, index and index + 1 are the indices between
                          which Target would fit.  (If Target fits to the
                          left of the first key, returns index of -1.)
*/



bool BTTableClass::SearchNode(const KeyType& Target, NodeType& node, int& Location) const
{
    bool Found = false;

    if (Target < node.Key[0].key)
    {
        Location = -1;
    }
    else
    {
        ItemType dummy(Target, 0);
        ItemType* found = std::lower_bound(node.Key, node.Key + node.Count, dummy, Compare());
        Location = found - node.Key;
        /*Location = node.Count - 1;
        while (Target < node.Key[Location].key && Location > 0)
        {
            --Location;
        }*/

        if (Target == node.Key[Location].key)
        {
            Found = true;
        }
        else
        {
            --Location;
        }
    }
    return Found;
}


/* Given:   The implicit BTTableClass object as well as:
            NewItem       Item to add to Node.
            NewRight      Pseudopointer to right subtree below NewItem.
            Node          The node to be added to.
            Location      The index at which to add newItem.
   Task:    To add Item to Node at index Location, and add NewRight
            as the branch just to the right of NewItem.  The addition is
            made by moving the needed keys and branches right by 1 in order
            to clear out index Location for NewItem.
   Return:  Node          Updated node.
*/
void BTTableClass::AddItem(const ItemType & NewItem, long NewRight,
   NodeType & Node, int Location)
{
   int j;

   for (j = Node.Count; j > Location; j--)
   {
      Node.Key[j] = Node.Key[j - 1];
      Node.Branch[j + 1] = Node.Branch[j];
   }

   Node.Key[Location] = NewItem;
   Node.Branch[Location + 1] = NewRight;
   Node.Count++;
}


/* Given: The implicit BTTableClass object as well as:
          CurrentItem    Item whose attempted placement into a node is
                         causing the node to be split.
          CurrentRight   Pseudopointer to the child just to the right of
                         CurrentItem.
          CurrentRoot    Pseudopointer to the node to be split.
          Location       Index of where CurrentItem should go in this node.
  Task:   To split the node that CurrentRoot points to into 2 nodes,
          pointed to by CurrentRoot and NewRight.  CurrentItem is properly
          placed in 1 of these 2 nodes (unless it is the median that gets
          moved up to the parent).  Finds Newitem, the median item that is
          to be moved up to the parent node.
  Return: NewItem        The item to be moved up into the parent node.
          NewRight       The pseudopointer to the child to the right of
                         NewItem (i.e. a pointer to the new RightNode).
*/
void BTTableClass::Split(const ItemType & CurrentItem, long CurrentRight,
   long CurrentRoot, int Location, ItemType & NewItem, long & NewRight)
{
   int j, Median;
   NodeType RightNode;

   #ifdef DEBUG
      cout << "S";
   #endif

   if (Location < MIN_KEYS)
      Median = MIN_KEYS;
   else
      Median = MIN_KEYS + 1;

   m_DataFile.seekg(CurrentRoot * m_NodeSize, ios::beg);
   m_DataFile.read(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

   #ifdef DEBUG
      cout << "R";
   #endif

   for (j = Median; j < MAX_KEYS; j++)
   {  // move half of the items to the RightNode
      RightNode.Key[j - Median] = m_CurrentNode.Key[j];
      RightNode.Branch[j - Median + 1] = m_CurrentNode.Branch[j + 1];
   }

   RightNode.Count = MAX_KEYS - Median;
   m_CurrentNode.Count = Median;   // is then incremented by AddItem

   // put CurrentItem in place
   if (Location < MIN_KEYS)
      AddItem(CurrentItem, CurrentRight, m_CurrentNode, Location + 1);
   else
      AddItem(CurrentItem, CurrentRight, RightNode,
         Location - Median + 1);

   NewItem = m_CurrentNode.Key[m_CurrentNode.Count - 1];
   RightNode.Branch[0] = m_CurrentNode.Branch[m_CurrentNode.Count];
   m_CurrentNode.Count--;

   m_DataFile.seekp(CurrentRoot * m_NodeSize, ios::beg);
   m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

   #ifdef DEBUG
      cout << "W";
   #endif

   m_NumNodes++;
   NewRight = m_NumNodes;
   m_DataFile.seekp(NewRight * m_NodeSize, ios::beg);
   m_DataFile.write(reinterpret_cast <char *> (&RightNode), m_NodeSize);

   #ifdef DEBUG
      cout << "W";
   #endif
}

/* Given:  The implicit BTTableClass object as well as:
           CurrentItem   The item to be inserted into the B-tree table.
           CurrentRoot   Pseudopointer to root of current subtree.
   Task:   To find where to put CurrentItem in a node of the subtree with
           the given root.  CurrentItem is ordinarily inserted, though
           a duplicate item is refused.  It is also possible that
           CurrentItem might be the item moved up to be inserted into
           the parent node if a split is done.
   Return: MoveUp        True if NewItem (and associated NewRight pointer)
                         must be placed in the parent node due to
                         splitting, false otherwise.
           NewItem       Item to be placed into parent node if a split was
                         done.
           NewRight      Pseudopointer to child to the right of NewItem.
*/
void BTTableClass::PushDown(const ItemType & CurrentItem, long CurrentRoot,
   bool & MoveUp, ItemType & NewItem, long & NewRight)
{
   int Location;

    #ifdef DEBUG
    cout << "P";
    #endif

    if (CurrentRoot == NIL_PTR)   // stopping case
    {   // cannot insert into empty tree
        MoveUp = true;
        NewItem = CurrentItem;
        NewRight = NIL_PTR;
    }
    else   // recursive case
    {
        m_DataFile.seekg(CurrentRoot * m_NodeSize, ios::beg);
        m_DataFile.read(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

        #ifdef DEBUG
        cout << "R";
        #endif

        if (SearchNode(CurrentItem.key, m_CurrentNode, Location))
        {
            if (!m_CurrentNode.Deleted[Location])
            {
                std::cerr << "Trying to insert duplicate key. Not supported yet :(" << std::endl;
            }
            else
            {
                m_CurrentNode.Deleted[Location] = false;
                m_CurrentNode.Key[Location] = CurrentItem;
                m_DataFile.seekp(CurrentRoot * m_NodeSize, ios::beg);
                m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);
            }
            MoveUp = false;
            return;
        }

        PushDown(CurrentItem, m_CurrentNode.Branch[Location + 1], MoveUp, NewItem, NewRight);

        if (MoveUp)
        {
            m_DataFile.seekg(CurrentRoot * m_NodeSize, ios::beg);
            m_DataFile.read(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

            #ifdef DEBUG
            cout << "R";
            #endif

            if (m_CurrentNode.Count < MAX_KEYS)
            {
                MoveUp = false;
                AddItem(NewItem, NewRight, m_CurrentNode, Location + 1);
                m_DataFile.seekp(CurrentRoot * m_NodeSize, ios::beg);
                m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

                #ifdef DEBUG
                cout << "W";
                #endif
            }
            else
            {
                MoveUp = true;
                Split(NewItem, NewRight, CurrentRoot, Location, NewItem, NewRight);
            }
        }
    }
}


/* Given:   The implicit BTTableClass object as well as:
            Item       Item to add to the table.
   Task:    To add Item to the table.
   Return:  In the function name, returns true to indicate success.
            (The implicit object is modified, of course.)
*/
bool BTTableClass::Insert(const ItemType & Item)
{
   bool MoveUp;
   long NewRight;
   ItemType NewItem;

   #ifdef DEBUG
      cout << "I";
   #endif

   PushDown(Item, m_Root, MoveUp, NewItem, NewRight);

   if (MoveUp)      // create a new root node
   {
       m_CurrentNode.Count = 1;
       m_CurrentNode.Key[0] = NewItem;
       m_CurrentNode.Branch[0] = m_Root;
       m_CurrentNode.Branch[1] = NewRight;
       m_NumNodes++;
       m_Root = m_NumNodes;
       m_DataFile.seekp(m_NumNodes * m_NodeSize, ios::beg);
       m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);

       #ifdef DEBUG
          cout << "W";
       #endif
   }

   ++m_NumItems;    // fixed 12/21/2001
   return true;     // no reason not to assume success
}


/* Given:   The implicit BTTableClass object as well as:
            SearchKey   Key value to look for in the table.
   Task:    To look for SearchKey in the table.
   Return:  In the function name, true if SearchKey was found,
            false otherwise.
            Item        The item were SearchKey was found.
*/
bool BTTableClass::Retrieve(KeyType SearchKey, ItemType & Item)
{
    long CurrentRoot = m_Root;
    bool Found = false;

    while (CurrentRoot != NIL_PTR)
    {
        m_DataFile.seekg(CurrentRoot * m_NodeSize, ios::beg);
        m_DataFile.read(reinterpret_cast<char*>(&m_CurrentNode), m_NodeSize);

        #ifdef DEBUG
        cout << "R";
        #endif

        int Location;
        if (SearchNode(SearchKey, m_CurrentNode, Location))
        {
            if (!m_CurrentNode.Deleted[Location])
            {
                Found = true;
                Item = m_CurrentNode.Key[Location];
            }
            break;
        }
        else
        {
           CurrentRoot = m_CurrentNode.Branch[Location + 1];
        }
    }

    return Found;
}

void BTTableClass::findRangeRecursive(long nodeId, KeyType keyBegin, KeyType keyEnd,
                                        std::vector<ItemType>& items)
{
    NodeType node;
    m_DataFile.seekg(nodeId * m_NodeSize, ios::beg);
    m_DataFile.read(reinterpret_cast<char*>(&node), m_NodeSize);

    int leftKeyPos = 0;
    bool leftFound = SearchNode(keyBegin, node, leftKeyPos);

    int rightKeyPos = 0;
    SearchNode(keyEnd, node, rightKeyPos);

    //std::cout << leftKeyPos << " " << rightKeyPos << " : ";

    for (int i = leftKeyPos; i <= rightKeyPos; ++i)
    {
        long branch = node.Branch[i + 1];
        if ((leftFound || branch == NIL_PTR) &&
            i >= 0 && i < node.Count && !node.Deleted[i])
        {
            items.push_back(node.Key[i]);
            //std::cout << "(" << i << "," << node.Key[i].key << ") ";
        }
        if (branch != NIL_PTR) findRangeRecursive(branch, keyBegin, keyEnd, items);
    }
    //std::cout << std::endl;
}

void BTTableClass::RetriveRange(KeyType keyBegin, KeyType keyEnd, std::vector<ItemType>& items)
{
    if (this->Empty()) return;
    this->findRangeRecursive(m_Root, keyBegin, keyEnd, items);
}

void BTTableClass::DeleteItem(const KeyType& Key)
{
    long CurrentRoot = m_Root;

    while (CurrentRoot != NIL_PTR)
    {
        m_DataFile.seekg(CurrentRoot * m_NodeSize, ios::beg);
        m_DataFile.read(reinterpret_cast<char*>(&m_CurrentNode), m_NodeSize);

        #ifdef DEBUG
        cout << "R";
        #endif

        int Location;
        if (SearchNode(Key, m_CurrentNode, Location))
        {
            m_CurrentNode.Deleted[Location] = true;
            m_DataFile.seekp(CurrentRoot * m_NodeSize, ios::beg);
            m_DataFile.write(reinterpret_cast <char *> (&m_CurrentNode), m_NodeSize);
            return;
        }
        else
        {
            CurrentRoot = m_CurrentNode.Branch[Location + 1];
        }
    }
}
