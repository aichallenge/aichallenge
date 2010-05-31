////////////////////////////////////////////////////////////////////////////
//
// list.h
//
// Remi Coulom
//
// april 1996
//
// list template
// efficient for small types (int, pointers, ...)
//
////////////////////////////////////////////////////////////////////////////
#ifndef LIST_H
#define LIST_H

#include "debug.h"      // Debug macros (ASSERT, INLINE)

#ifdef LISTINLINE
#define BIGINLINE INLINE
#else
#define BIGINLINE
#endif

template<class T> class CListIterator;
template<class T> class CConstListIterator;
template<class T> class CListSort;

////////////////////////////////////////////////////////////////////////////
// CListCell class definition
////////////////////////////////////////////////////////////////////////////
template<class T>
class CListCell // cell
{
 private:
  T tValue;
  CListCell<T> *pcellNext;

 public:
  T& Value() {return tValue;};
  const T& Value() const {return tValue;};

  CListCell<T> *Next() {return pcellNext;};
  const CListCell<T> *Next() const {return pcellNext;};

  void Link(CListCell<T> *pcell) {pcellNext = pcell;};
};

////////////////////////////////////////////////////////////////////////////
// CCellBloc class definition
////////////////////////////////////////////////////////////////////////////
template<class T>
class CCellBloc // bloc
{
 private:
  CListCell<T> *ptcell;         // Array of cells
  CCellBloc<T> *pblocNext;      // Next bloc

 public:
  CCellBloc(int Size, CCellBloc<T> *pbloc);
  ~CCellBloc();

  const CListCell<T> *FirstCell() const {return ptcell;};
  CListCell<T> *FirstCell() {return ptcell;};

  const CCellBloc<T> *Next() const {return pblocNext;};
  CCellBloc<T> *Next() {return pblocNext;};
};

////////////////////////////////////////////////////////////////////////////
// CList class definition
////////////////////////////////////////////////////////////////////////////
template<class T>
class CList //lt
{
 friend class CListIterator<T>;
 friend class CConstListIterator<T>;
 friend class CListSort<T>;

 private: //////////////////////////////////////////////////////////////////
  int BlocSize;
  CListCell<T> *pcellFirst;
  CListCell<T> *pcellFree;
  CCellBloc<T> *pblocFirst;

  void FreeCell(CListCell<T> *pcell);
  CListCell<T> *AllocateCell();

 public: ///////////////////////////////////////////////////////////////////
  CList();
  ~CList();
  CList(const CList<T> &lt);
  CList<T> &operator=(const CList<T> &lt);

  void Reset();
  void Append(const CList<T> &lt);
  void Add();
  void Remove();
  void SetBlocSize(int Size);
  T& Head();

  const T& Head() const;
  int IsEmpty() const;
  int GetSize() const;
};

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
template<class T>
CCellBloc<T>::CCellBloc(int Size, CCellBloc<T> *pbloc)
{
 ASSERT(Size > 0);
 pblocNext = pbloc;
 ptcell = new CListCell<T>[Size];

 {
  for (int i = Size - 1; --i >= 0;)
   ptcell[i].Link(&ptcell[i + 1]);
 }

 ptcell[Size - 1].Link(0);
}

////////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////////
template<class T>
inline
CCellBloc<T>::~CCellBloc()
{
 delete[] ptcell;
}

////////////////////////////////////////////////////////////////////////////
// Constructor for CList<T>
////////////////////////////////////////////////////////////////////////////
template<class T>
BIGINLINE
CList<T>::CList()
{
 pcellFirst = 0;
 pcellFree = 0;
 BlocSize = 1;
 pblocFirst = 0;
}

////////////////////////////////////////////////////////////////////////////
// Copy constructor for CList<T>
////////////////////////////////////////////////////////////////////////////
template<class T>
CList<T>::CList(const CList<T> &lt)
{
 BlocSize = lt.BlocSize;

 pcellFirst = 0;
 pcellFree = 0;
 pblocFirst = 0;

 Append(lt);
}

////////////////////////////////////////////////////////////////////////////
// Assignment operator
////////////////////////////////////////////////////////////////////////////
template<class T>
CList<T> &CList<T>::operator=(const CList<T> &lt)
{
 if (&lt != this)
 {
  Reset();
  BlocSize = lt.BlocSize;
  Append(lt);
 } 
 return *this;
}

////////////////////////////////////////////////////////////////////////////
// Destructor : deletes all the allocated memory
////////////////////////////////////////////////////////////////////////////
template<class T>
CList<T>::~CList()
{
 Reset();
}

////////////////////////////////////////////////////////////////////////////
// Function called in destructor and assignment operator to free memory
////////////////////////////////////////////////////////////////////////////
template<class T>
void CList<T>::Reset()
{
 CCellBloc<T> *pblocCurrent = pblocFirst;

 while (pblocCurrent)
 {
  CCellBloc<T> *pblocNext = pblocCurrent->Next();
  delete pblocCurrent;
  pblocCurrent = pblocNext;
 }

 pcellFirst = 0;
 pcellFree = 0;
 pblocFirst = 0;
}

////////////////////////////////////////////////////////////////////////////
// Function to append a list at the end of a list
////////////////////////////////////////////////////////////////////////////
template<class T>
void CList<T>::Append(const CList<T> &lt)
{
 if (lt.pcellFirst)
 {
  CListCell<T> *pcellCopy = AllocateCell();

  //
  // Allocation of a copy of the appended list
  //
  {
   const CListCell<T> *pcellFrom = lt.pcellFirst;
   CListCell<T> *pcellTo = pcellCopy;

   do
   {
    pcellTo->Value() = pcellFrom->Value();
    pcellFrom = pcellFrom->Next();
    if (pcellFrom)
    {
     pcellTo->Link(AllocateCell());
     pcellTo = pcellTo->Next();
    }
    else
     pcellTo->Link(0);
   }
   while(pcellFrom);
  }

  //
  // Search of the end of this list and addition of the copy
  //
  {
   CListCell<T> *pcell = pcellFirst;

   if (pcell)
   {
    while (pcell->Next())
     pcell = pcell->Next();
    pcell->Link(pcellCopy);
   }
   else
    pcellFirst = pcellCopy;
  }
 }
}

////////////////////////////////////////////////////////////////////////////
// Adds a new cell at the beginning of the list
////////////////////////////////////////////////////////////////////////////
template<class T>
BIGINLINE
void CList<T>::Add()
{
 CListCell<T> *pcellNew = AllocateCell();
 ASSERT(!!pcellNew);
 pcellNew->Link(pcellFirst);
 pcellFirst = pcellNew;
}

////////////////////////////////////////////////////////////////////////////
// Deletes the head of the list
////////////////////////////////////////////////////////////////////////////
template<class T>
BIGINLINE
void CList<T>::Remove()
{
 ASSERT(!IsEmpty());
 CListCell<T> *pcell = pcellFirst;
 pcellFirst = pcellFirst->Next();
 FreeCell(pcell);
}

////////////////////////////////////////////////////////////////////////////
// Function to set the bloc size
////////////////////////////////////////////////////////////////////////////
template<class T>
void CList<T>::SetBlocSize(int Size)
{
 ASSERT(Size > 0);
 BlocSize = Size;
}

////////////////////////////////////////////////////////////////////////////
// Access to data at the head of the list
////////////////////////////////////////////////////////////////////////////
template<class T> // const version
INLINE
const T& CList<T>::Head() const
{
 ASSERT(!IsEmpty());
 return pcellFirst->Value();
}
template<class T> // non-const version
INLINE
T& CList<T>::Head()
{
 ASSERT(!IsEmpty());
 return pcellFirst->Value();
}

////////////////////////////////////////////////////////////////////////////
// Tests whether the list is empty
////////////////////////////////////////////////////////////////////////////
template<class T>
inline
int CList<T>::IsEmpty() const
{
 return !pcellFirst;
}

////////////////////////////////////////////////////////////////////////////
// Frees one cell
////////////////////////////////////////////////////////////////////////////
template<class T>
inline
void CList<T>::FreeCell(CListCell<T> *pcell)
{
 pcell->Link(pcellFree);
 pcellFree = pcell;
}

////////////////////////////////////////////////////////////////////////////
// Count of list elements
////////////////////////////////////////////////////////////////////////////
template<class T>
int CList<T>::GetSize() const
{
 CListCell<T> *pcell = pcellFirst;
 int s = 0;

 while (pcell)
 {
  s++;
  pcell = pcell->Next();
 }

 return s;
}

////////////////////////////////////////////////////////////////////////////
// Allocates a new cell
////////////////////////////////////////////////////////////////////////////
template<class T>
BIGINLINE
CListCell<T> *CList<T>::AllocateCell()
{
 if (!pcellFree)
 {
  pblocFirst = new CCellBloc<T>(BlocSize, pblocFirst);
  pcellFree = pblocFirst->FirstCell();
 }

 CListCell<T> *pcell = pcellFree;
 pcellFree = pcellFree->Next();
 return pcell;
}

#endif
