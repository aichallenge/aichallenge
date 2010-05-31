/////////////////////////////////////////////////////////////////////////////
//
// CIndirectCompare.h
//
// Rémi Coulom
//
// December, 2005
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CIndirectCompare_Declared
#define CIndirectCompare_Declared

template<class T>
class CIndirectCompare
{
 private:
  const T *pelo;

 public:
  CIndirectCompare(const T *p) : pelo(p) {}

  bool operator()(int i, int j) const
  {
   return pelo[i] > pelo[j];
  }
};

#endif
