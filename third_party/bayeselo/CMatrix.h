/////////////////////////////////////////////////////////////////////////////
//
// CMatrix.h
//
// Rémi Coulom
//
// February, 2005
//
/////////////////////////////////////////////////////////////////////////////
#ifndef Math_CMatrix_Declared
#define Math_CMatrix_Declared

#include "CVector.h"

class CMatrix: public CVector
{
 private: ///////////////////////////////////////////////////////////////////
  int Rows;
  int Columns;

 public: ////////////////////////////////////////////////////////////////////
  CMatrix(int RowsInit = 0, int ColumnsInit = 0);

  void SetSize(int RowsInit, int ColumnsInit);
  int GetRows() const {return Rows;}
  int GetColumns() const {return Columns;}

  double GetElement(int i, int j) const {return (*this)[i * Columns + j];}
  void SetElement(int i, int j, double x) {(*this)[i * Columns + j] = x;}
  void AddToElement(int i, int j, double x) {(*this)[i * Columns + j] += x;}

  void SetProductByTranspose(const CMatrix &mA, const CMatrix &mB);
};

#endif
