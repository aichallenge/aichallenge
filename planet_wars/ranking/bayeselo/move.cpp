////////////////////////////////////////////////////////////////////////////
//
// move.cpp
//
// CMove class
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#include <cstring>
#include <iostream>

#include "move.h"
#include "square.h"   // CSquare
#include "player.h"   // CPlayer
#include "piece.h"    // CPiece
#include "position.h" // CPosition

using namespace std;

CMove CMove::moveNull(0, 0, 0, 0);

////////////////////////////////////////////////////////////////////////////
// Function to write a move string
// psz must point to a string with at least MaxMoveString characters
////////////////////////////////////////////////////////////////////////////
#define ADD(c) do {*psz = (c); psz++;} while(0)
void CMove::MakeString(char *psz, int Notation) const
{
 //
 // Null move
 //
 if (!sqFrom && !sqTo)
 {
  ADD('n');
  ADD('u');
  ADD('l');
  ADD('l');
  ADD(0);
  return;
 }
 
 static const char PieceLetter[] = "?KQRBNP";
 static int Conversion[] = {1, 2, 3, 3, 4, 4, 5, 5, 6};

 if (Notation == Basic || Notation == ChessBase)
 {
  ADD('a' + (sqFrom & 7));
  ADD('1' + (sqFrom / 8));
  ADD('a' + (sqTo & 7));
  ADD('1' + (sqTo / 8));
  
  if (ucType & Promotion)
  {
   if (Notation == Basic)
    ADD('=');
   ADD(PieceLetter[Conversion[ucType & 0x0f]]);
  }
  
  if (ucType == ep && Notation == Basic)
  {
   ADD('e');
   ADD('p');
  }
  
  if ((ucType == OO || ucType == OOO) && Notation == Basic)
   ADD('c');
 }
 else
 {
  //
  // Short castle
  //
  if (ucType == OO)
  {
   ADD('O');
   ADD('-');
   ADD('O');
  }

  //
  // Long castle
  //
  else if (ucType == OOO)
  {
   ADD('O');
   ADD('-');
   ADD('O');
   ADD('-');
   ADD('O');
  }

  //
  // Other moves
  //
  else
  {
   //
   // Origin square
   //
   if ((ucSAN & 0x07) == Pawn &&
       (ucSAN & Capture) &&
       Notation == StandardAlgebraic)
    ADD('a' + (sqFrom & 7));
   else if ((ucSAN & 0x07) != Pawn || Notation == LongAlgebraic)
   {
    if ((ucSAN & 0x07) != Pawn)
     ADD(PieceLetter[ucSAN & 0x07]);
    if ((ucSAN & File) || Notation == LongAlgebraic)
     ADD('a' + (sqFrom & 7));
    if ((ucSAN & Rank) || Notation == LongAlgebraic)
     ADD('1' + (sqFrom / 8));
   }

   //
   // Capture indication
   //
   if (ucSAN & Capture)
    ADD('x');
   else if (Notation == LongAlgebraic)
    ADD('-');
 
   //
   // Destination square
   //
   ADD('a' + (sqTo & 7));
   ADD('1' + (sqTo / 8));

   //
   // Promotions
   //
   if (ucType & Promotion)
   {
    ADD('=');
    ADD(PieceLetter[Conversion[ucType & 0x0f]]);
   }
  }
 
  //
  // Check and mate
  //
  if (FullSANComputed())
  {
   if (ucSAN & Mate)
    ADD('#');
   else if (ucSAN & Check)
    ADD('+');
  }  
 }
 
 //
 // End of move string
 //
 ADD(0);
}
#undef ADD

////////////////////////////////////////////////////////////////////////////
// Function to parse a move string
// Return value : 0 when sucessfully parsed, 1 otherwise
////////////////////////////////////////////////////////////////////////////
int CMove::ParseString(const char *psz)
{
 ucType = 0;
 ucSAN = 0;

 //
 // null move
 //
 if (psz[0] == 'n' && psz[1] == 'u' && psz[2] == 'l' && psz[3] == 'l')
  *this = moveNull;

 //
 // sqFrom and sqTo;
 //
 sqFrom = CSquare(psz);
 sqTo = CSquare(psz + 2);
 if (sqFrom > CSquare::h8 || sqTo > CSquare::h8)
  return 1;  
  
 //
 // Castling
 //
 if (psz[4] == 'c')
 {
  if (psz[2] == 'g')
   ucType = CMove::OO;
  else
   ucType = CMove::OOO;
 }

 //
 // Promotion
 //
 if (psz[4] == '=')
 {
  switch(psz[5])
  {
   case 'Q': ucType = Promotion | CPiece::Queen; break;
   case 'R': ucType = Promotion | CPiece::Rook; break;
   case 'B': ucType = Promotion | CPiece::Bishop; break;
   case 'N': ucType = Promotion | CPiece::Knight; break;
  }
 }
 
 //
 // capture "en passant"
 //
 if (psz[4] == 'e')
  ucType = ep;

 return 0;
}

////////////////////////////////////////////////////////////////////////////
// output operator
////////////////////////////////////////////////////////////////////////////
ostream &operator<<(ostream &ostr, const CMove &move)
{
 char szMove[CMove::MaxMoveString];
 if (move.FullSANComputed())
  move.MakeString(szMove, CMove::StandardAlgebraic);
 else
  move.MakeString(szMove, CMove::Basic);
 return ostr << szMove;
}

////////////////////////////////////////////////////////////////////////////
// Function to get the gnuchess notation of a move
////////////////////////////////////////////////////////////////////////////
void CMove::GNUChessNotation(char *pszString) const
{
#define ADD(c) do {*pszString = c; pszString++;} while(0)
 ADD('a' + (GetFrom() & 7));
 ADD('1' + (GetFrom() / 8));
 ADD('a' + (GetTo() & 7));
 ADD('1' + (GetTo() / 8));

 static const char PieceLetter[] = "?kqrbnp";
 static int Conversion[] = {1, 2, 3, 3, 4, 4, 5, 5, 6};

 if (GetType() & Promotion)
  ADD(PieceLetter[Conversion[GetType() & 0x0f]]);

 ADD(0);  
#undef ADD
}
