////////////////////////////////////////////////////////////////////////////
//
// CLUDecomposition.cpp
//
// Remi Coulom
//
// September, 2000
//
////////////////////////////////////////////////////////////////////////////
#include "CLUDecomposition.h"

#include <math.h>

////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CLUDecomposition::CLUDecomposition(int nInit) :
 n(nInit)
{
 pdImplicitScaling = new double[n];
 pdy = new double[n];
}

////////////////////////////////////////////////////////////////////////////
// Decompose
////////////////////////////////////////////////////////////////////////////
void CLUDecomposition::Decompose(double *pdMatrix, int *pIndex) const
{
 //
 // Loop over rows to get the implicit scaling information
 //
 for (int i = n; --i >= 0;)
 {
  pIndex[i] = i;
  double Max = 0.0;
  for (int j = n; --j >= 0;)
  {
   double a = fabs(pdMatrix[i * n + j]);
   if (a > Max)
    Max = a;
  }
  pdImplicitScaling[i] = 1.0 / Max;
 }

 //
 // Main loop over columns
 //
 for (int j = 0; j < n; j++)
 {
  for (int i = 0; i < j; i++)
  {
   double Sum = pdMatrix[i * n + j];
   for (int k = 0; k < i; k++)
    Sum -= pdMatrix[i * n + k] * pdMatrix[k * n + j];
   pdMatrix[i * n + j] = Sum;
  }

  int iMax = j;
  double Max = 0.0;
  for (int i = j; i < n; i++)
  {
   double Sum = pdMatrix[i * n + j];
   for (int k = 0; k < j; k++)
    Sum -= pdMatrix[i * n + k] * pdMatrix[k * n + j];
   pdMatrix[i * n + j] = Sum;
   double a = pdImplicitScaling[i] * fabs(Sum);
   if (a > Max)
   {
    Max = a;
    iMax = i;
   }
  }

  if (iMax != j)
  {
   for (int k = n; --k >= 0;)
   {
    double tmp = pdMatrix[iMax * n + k];
    pdMatrix[iMax * n + k] = pdMatrix[j * n + k];
    pdMatrix[j * n + k] = tmp;
   }
   pdImplicitScaling[iMax] = pdImplicitScaling[j];
   {
    int tmp = pIndex[j];
    pIndex[j] = pIndex[iMax];
    pIndex[iMax] = tmp;
   }
  }

  {
   double x = 1.0 / pdMatrix[j * n + j];
   for (int i = j + 1; i < n; i++)
    pdMatrix[i * n + j] *= x;
  }
 }
}

////////////////////////////////////////////////////////////////////////////
// Backsubstitution
////////////////////////////////////////////////////////////////////////////
void CLUDecomposition::Solve(const double *pdMatrix,
                             const int *pIndex,
                             const double *pb,
                             double *px) const
{
 for (int i = 0; i < n; i++)
 {
  double Sum = pb[pIndex[i]];
  for (int j = 0; j < i; j++)
   Sum -= pdMatrix[i * n + j] * px[j];
  px[i] = Sum;
 }
 for (int i = n; --i >= 0;)
 {
  double Sum = px[i];
  for (int j = i + 1; j < n; j++)
   Sum -= pdMatrix[i * n + j] * px[j];
  px[i] = Sum / pdMatrix[i * n + i];
 }
}

////////////////////////////////////////////////////////////////////////////
// SolveTranspose
////////////////////////////////////////////////////////////////////////////
void CLUDecomposition::SolveTranspose(const double *pdMatrix,
                                      const int *pIndex,
                                      const double *pb,
                                      double *px) const
{
 for (int i = 0; i < n; i++)
 {
  double Sum = pb[i];
  for (int j = 0; j < i; j++)
   Sum -= pdMatrix[j * n + i] * pdy[j];
  pdy[i] = Sum / pdMatrix[i * n + i];
 }
 for (int i = n; --i >= 0;)
 {
  double Sum = pdy[i];
  for (int j = i + 1; j < n; j++)
   Sum -= pdMatrix[j * n + i] * pdy[j];
  pdy[i] = Sum;
  px[pIndex[i]] = pdy[i];
 }
}

////////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////////
CLUDecomposition::~CLUDecomposition()
{
 delete[] pdImplicitScaling;
 delete[] pdy;
}
