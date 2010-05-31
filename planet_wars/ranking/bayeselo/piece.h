////////////////////////////////////////////////////////////////////////////
//
// piece.h
//
// CPiece class definition
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef PIECE_H
#define PIECE_H

class CPiece // piece
{
 private: //////////////////////////////////////////////////////////////////
  int piece;
  static char tcPiece[];

 public: ///////////////////////////////////////////////////////////////////
  enum
  {
   King   = 0x00,
   Queen  = 0x01,
   Rook   = 0x02,
   Bishop = 0x04,
   Knight = 0x06,
   Pawn   = 0x08,
   Black  = 0x10,
   Undef  = 0x20,
   Empty  = 0xff
  };

  CPiece(int pieceValue) {piece = pieceValue;};
  CPiece(const char *pcPiece);
  
  operator int() const {return piece;};
  int GetNature() const {return piece & 0x0f;};
  int GetColor() const {return (piece & 0x30) >> 4;};
  char GetChar() const;
};

#endif
