////////////////////////////////////////////////////////////////////////////
//
// random.cpp
//
// CRandom class
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#include <cmath>

////////////////////////////////////////////////////////////////////////////
// New seed
////////////////////////////////////////////////////////////////////////////
template<class TYPE>
void CRandom<TYPE>::Seed(TYPE ulSeed)
{
 Index1 = 23;
 Index2 = 54;
 fNextGaussian = 0;

 //
 // The array is initialized with random values
 //
 {
  for (int i = 55; --i >= 0;)
   tulArray[i] = ulSeed = (ulSeed * 7) + 123456789L;
 }

 //
 // These are scrambled a little
 //
 {
  for (int i = 200; --i >= 0;)
   NewValue();
 }
}

////////////////////////////////////////////////////////////////////////////
// Next double (??? very bad)
////////////////////////////////////////////////////////////////////////////
template<class TYPE>
double CRandom<TYPE>::NextDouble()
{
 return double(NewValue() & 0x7fffffff) / double(1U << 31);
}

////////////////////////////////////////////////////////////////////////////
// Next N(0,1)
////////////////////////////////////////////////////////////////////////////
template<class TYPE>
double CRandom<TYPE>::NextGaussian()
{
 if (fNextGaussian)
 {
  fNextGaussian = 0;
  return NextGaussianValue;
 }
 else
 {
  fNextGaussian = 1;
  double x, y, n2;
  do
  {
   x = 2 * NextDouble() - 1;
   y = 2 * NextDouble() - 1;
   n2 = x * x + y * y;
  }
  while (n2 >= 1 || n2 == 0);
  double m = std::sqrt(-2 * std::log(n2) / n2);
  NextGaussianValue = y * m;
  return x * m;
 }
}

////////////////////////////////////////////////////////////////////////////
// Exponential law
////////////////////////////////////////////////////////////////////////////
template<class TYPE>
double CRandom<TYPE>::NextExponential()
{
 double x;
 do
  x = NextDouble();
 while (x == 0);
 return -std::log(x);
}

////////////////////////////////////////////////////////////////////////////
// Binary I/O
////////////////////////////////////////////////////////////////////////////
#include <iostream>

template<class TYPE>
void CRandom<TYPE>::BinaryWrite(std::ostream &out) const
{
 out.write((const char *)&Index1, sizeof(Index1));
 out.write((const char *)&Index2, sizeof(Index2));
 out.write((const char *)tulArray, sizeof(tulArray));
 out.write((const char *)&fNextGaussian, sizeof(fNextGaussian));
 out.write((const char *)&NextGaussianValue, sizeof(NextGaussianValue));
}

template<class TYPE>
void CRandom<TYPE>::BinaryRead(std::istream &in)
{
 in.read((char *)&Index1, sizeof(Index1));
 in.read((char *)&Index2, sizeof(Index2));
 in.read((char *)tulArray, sizeof(tulArray));
 in.read((char *)&fNextGaussian, sizeof(fNextGaussian));
 in.read((char *)&NextGaussianValue, sizeof(NextGaussianValue));
}
