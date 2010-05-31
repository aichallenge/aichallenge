/////////////////////////////////////////////////////////////////////////////
//
// Rémi Coulom
//
// January, 2005
//
/////////////////////////////////////////////////////////////////////////////
#ifndef CDiscretization_Declared
#define CDiscretization_Declared

class CDiscretization
{
 protected: /////////////////////////////////////////////////////////////////
  const int Size;
  const double Min;
  const double Max;

 public: ////////////////////////////////////////////////////////////////////
  CDiscretization(int SizeInit, double MinInit, double MaxInit):
   Size(SizeInit),
   Min(MinInit),
   Max(MaxInit)
  {}

  int GetDiscretizationSize() const {return Size;}

  int IndexFromValue(double x) const
  {
   return int(0.5 + (Size - 1) * (x - Min) / (Max - Min));
  }

  double ValueFromIndex(int i) const
  {
   return Min + ((i + 0.5) * (Max - Min)) / Size;
  }
};

#endif
