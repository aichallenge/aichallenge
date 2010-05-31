////////////////////////////////////////////////////////////////////////////
//
// const.h
//
// CConst class definition
//
// Remi Coulom
//
// July 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef CONST_H
#define CONST_H

class CConst
{
 public: ///////////////////////////////////////////////////////////////////

 //
 // Flags indicating direction and distance from the edge for each square
 //
 enum
 {
  Left   = 0x01,
  Up     = 0x02,
  Right  = 0x04,
  Down   = 0x08,
  Left2  = 0x10,
  Up2    = 0x20,
  Right2 = 0x40,
  Down2  = 0x80
 };

 static const int tcEdge[64];
 struct st {int cDirection; int Direction;};
 static const struct st TKnightMove[8];
 static const struct st TKingMove[8];
 static const int tDirection[13];
};


#endif
