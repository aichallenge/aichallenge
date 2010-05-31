////////////////////////////////////////////////////////////////////////////
//
// pgnlex.h
//
// CPGNLex class definition
// (PGN lexical analysis)
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef PGNLEX_H
#define PGNLEX_H

#include <iostream>

class CPGNLex //pgnlex
{
 private: //////////////////////////////////////////////////////////////////
  std::istream *pistr;

  int CurrChar;
  int Token;
  int Value;
  enum {MaxPGNLine = 1024}; 
  char szString[MaxPGNLine];
  enum {ContextSize = 40};
  char szContextBuffer[ContextSize + 1];
  int ContextIndex;

  int NextChar();

 public: ///////////////////////////////////////////////////////////////////
  //
  // Tokens
  // Note : these tokens do not conform strictly with standard.txt
  //
  // DIGIT = [0-9]
  // LETTER = [a-zA-Z]
  // SYMBOLCONTINUATION = LETTER U DIGIT U {_} U {+} U {#} U {=} U {:} U {-}
  //
  enum
  {
   TOK_BOF,             //  Beginning Of File
   TOK_EOF,             //  End Of File
   TOK_String,          //s '"'(ALLCHARACTERS U '\\' U '\"' - '\' - '"')*'"'
   TOK_Integer,         //n DIGIT+
   TOK_Period,          //  '.'
   TOK_GameTermination, //n '0-1' U '1/2-1/2' U '1-0' U '*'
   TOK_TagOpen,         //  '['
   TOK_TagClose,        //  ']'
   TOK_RAVOpen,         //  '('
   TOK_RAVClose,        //  ')'
   TOK_ReservedOpen,    //  '<'
   TOK_ReservedClose,   //  '>'
   TOK_NAG,             //n '$'DIGIT* U '?' U '!' U '!?' U '?!' U '!!' U '??'
   TOK_Symbol,          //s LETTER(SYMBOLCONTINUATION)*
   TOK_Comment,         //s '{'(ALLCHARACTERS - '}')*'}'
   TOKs
  };

  //
  // Constructor
  //
  CPGNLex();
  CPGNLex(std::istream &istr);
  
  //
  // Function to reset the input stream
  //
  
  // Sets the current token to TOK_BOF
  void Reset();
  // Changes the input stream
  void Reset(std::istream &istr);  

  //
  // Functions to read tokens
  //

  // Function to move to the next token
  int ReadNextToken();
  
  // Access to the current token type
  int GetToken() const {return Token;}
  
  // Access to the current token value
  // This value is set when TOK_Integer, TOK_GameTermination, TOK_NAG read
  int TokenValue() const;
  
  // Access to the current token string
  // This value is set when a TOK_String or TOK_Symbol is read
  const char *TokenString() const;
  
  // returns 0 if the searched token is found, 1 if eof is reached
  int WaitForToken(int Searched);

  //
  // Get context (useful to display error messages)
  //
  void PrintContext(std::ostream &out) const;
};

#endif
