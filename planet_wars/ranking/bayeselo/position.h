////////////////////////////////////////////////////////////////////////////
//
// position.h
//
// CPosition class definition
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef POSITION_H
#define POSITION_H

class CMove;
class CHashCodeComputer;

class CPosition // pos
{
 private: //////////////////////////////////////////////////////////////////
  static CHashCodeComputer hcc;

  unsigned char tpiece[64];
  unsigned char player;
  char fBlackOO;
  char fWhiteOO;
  char fBlackOOO;
  char fWhiteOOO;
  unsigned char sqEnPassant;
  int nhmReversible;
  int nm;

  int IsAttackedDir(int sqFrom, int piece1, int piece2, int DirFlag) const;

 public: ///////////////////////////////////////////////////////////////////
  //
  // Default constructor to prevent uninitialized data
  //
  CPosition();
  
  //
  // Test for equality
  //
  int operator==(const CPosition &pos) const;
  int operator!=(const CPosition &pos) const {return !operator==(pos);}

  //
  // Gets
  //
  int GetPiece(int sq) const {return tpiece[sq];}
  int GetPlayer() const {return player;}
  int GetOpponent() const {return player ^ 1;}
  int GetBlackOO() const {return fBlackOO;}
  int GetWhiteOO() const {return fWhiteOO;}
  int GetBlackOOO() const {return fBlackOOO;}
  int GetWhiteOOO() const {return fWhiteOOO;}
  int GetEnPassant() const {return sqEnPassant;}
  int GetReversible() const {return nhmReversible;}
  int GetMoves() const {return nm;}

  int IsAttacked(int sq, int player) const;
  int Check() const;
  int IsLegal() const;
  void Symmetrical(CPosition &pos) const;

  //
  // Sets
  //
  void SetPiece(int sq, int piece) {tpiece[sq] = piece;}
  void SetPlayer(int playerNew) {player = playerNew;}
  void SetBlackOO(int f) {fBlackOO = f;}
  void SetWhiteOO(int f) {fWhiteOO = f;}
  void SetBlackOOO(int f) {fBlackOOO = f;}
  void SetWhiteOOO(int f) {fWhiteOOO = f;}
  void SetEnPassant(int sq) {sqEnPassant = sq;}
  void SetReversible(int nhm) {nhmReversible = nhm;}
  void SetMoves(int nmNew) {nm = nmNew;}

  void Reset();
  void Clear();
  void PlayMove(CMove move);

  //
  // Access to the hashcode computer
  //
  static const CHashCodeComputer &GetHashCodeComputer() {return hcc;}
};

#endif
