////////////////////////////////////////////////////////////////////////////
//
// random.h
//
// CRandom class declaration
// (Algorithm from "The Art Of Computer Programming")
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef RANDOM_H
#define RANDOM_H

#include <iosfwd>

template<class TYPE>
class CRandom // rnd
{
 private: //////////////////////////////////////////////////////////////////
  int Index1;
  int Index2;
  TYPE tulArray[55];

  int fNextGaussian;
  double NextGaussianValue;

 public: ///////////////////////////////////////////////////////////////////
  CRandom(TYPE n = 0) : fNextGaussian(0) {Seed(n);}

  //
  // Seed
  //
  void Seed(TYPE ulSeed);

  //
  // Random number generation
  //
  TYPE NewValue()
  {
   if (--Index1 < 0)
    Index1 = 54;

   if (--Index2 < 0)
    Index2 = 54;

   return tulArray[Index2] += tulArray[Index1];
  }
  double NextDouble();
  double NextGaussian();
  double NextExponential();

  //
  // Binary I/O
  //
  void BinaryWrite(std::ostream &out) const;
  void BinaryRead(std::istream &in);
};

#include "random.cpp"

#endif
