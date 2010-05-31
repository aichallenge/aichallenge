////////////////////////////////////////////////////////////////////////////
//
// CVector.h
//
// Remi Coulom
//
// September, 2000
//
////////////////////////////////////////////////////////////////////////////
#ifndef Math_CVector_Declared
#define Math_CVector_Declared

class CVector // v
{
 private: /////////////////////////////////////////////////////////////////
  double *pd;
  int Size;

 public: //////////////////////////////////////////////////////////////////
  CVector(int SizeInit = 0);
  CVector(const CVector &v);
  CVector &operator=(const CVector &v);   

  int GetSize() const {return Size;}
  void SetSize(int NewSize);

  operator double * () {return pd;}
  operator const double * () const {return pd;}

  double operator[](int i) const {return pd[i];}
  double &operator[](int i) {return pd[i];}

  void Zero();
  void Scale(double x);
  void Add(const double *pdp);
  void AddScaled(const double *pdp, double x);
  void Set(const double *px);

  ~CVector();
};

#endif
