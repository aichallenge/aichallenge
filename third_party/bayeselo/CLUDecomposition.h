////////////////////////////////////////////////////////////////////////////
//
// CLUDecomposition.h
//
// Remi Coulom
//
// September, 2000
//
////////////////////////////////////////////////////////////////////////////
#ifndef Math_CLUDecomposition_Declared
#define Math_CLUDecomposition_Declared

class CLUDecomposition // lud
{
 private: /////////////////////////////////////////////////////////////////
  int n;
  double *pdImplicitScaling;
  double *pdy;

 public: //////////////////////////////////////////////////////////////////
  CLUDecomposition(int nInit);

  void Decompose(double *pdMatrix, int *pIndex) const;
  void Solve(const double *pdMatrix,
             const int *pIndex,
             const double *pb,
             double *px) const;
  void SolveTranspose(const double *pdMatrix,
                      const int *pIndex,
                      const double *pb,
                      double *px) const;

  ~CLUDecomposition();
};

#endif
