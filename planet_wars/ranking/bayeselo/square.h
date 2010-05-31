////////////////////////////////////////////////////////////////////////////
//
// square.h
//
// CSquare class definition
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef SQUARE_H
#define SQUARE_H

class CSquare // sq
{
 private: //////////////////////////////////////////////////////////////////
  int sq;

 public: ///////////////////////////////////////////////////////////////////
  enum
  {
   a1, b1, c1, d1, e1, f1, g1, h1,
   a2, b2, c2, d2, e2, f2, g2, h2,
   a3, b3, c3, d3, e3, f3, g3, h3,
   a4, b4, c4, d4, e4, f4, g4, h4,
   a5, b5, c5, d5, e5, f5, g5, h5,
   a6, b6, c6, d6, e6, f6, g6, h6,
   a7, b7, c7, d7, e7, f7, g7, h7,
   a8, b8, c8, d8, e8, f8, g8, h8
  };

  enum
  {
   Left  = -1,
   Right = +1,
   Up    = +8,
   Down  = -8
  };

  CSquare() {}
  CSquare(int i) {sq = i;}
  CSquare(const char *psz) {sq = psz[0] - 'a' + 8 * (psz[1] - '1');}
                
  operator int() const {return sq;}
  const CSquare &operator +=(int i) {sq += i; return *this;}
  const CSquare &operator -=(int i) {sq -= i; return *this;}
  const CSquare &operator ++() {sq++; return *this;}

  int GetRank() const {return sq / 8;}
  int GetFile() const {return sq % 8;}
  int IsWhite() const {return ((sq / 8) + (sq % 8)) & 1;}
  void MakeString(char *psz)
  {
   psz[0] = 'a' + GetFile();
   psz[1] = '1' + GetRank();
   psz[2] = 0;
  }
};

#endif
