////////////////////////////////////////////////////////////////////////////
//
// pgnlex.cpp
//
// CPGNLex
//
// Remi Coulom
//
// August 1996
//
////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <cstdio> // for EOF

#include "debug.h"
#include "pgnlex.h"
#include "str.h"    // CSTR Class definition (for game terminations)

using namespace std;

//#define TRACETOKEN
#ifndef TRACETOKEN
#undef TRACE
#define TRACE(x)
#endif
static const char * const tszToken[] =
{
 "TOK_BOF",
 "TOK_EOF",
 "TOK_String",
 "TOK_Integer",
 "TOK_Period",
 "TOK_GameTermination",
 "TOK_TagOpen",
 "TOK_TagClose",
 "TOK_RAVOpen",
 "TOK_RAVClose",
 "TOK_ReservedOpen",
 "TOK_ReservedClose",
 "TOK_NAG",
 "TOK_Symbol",
 "TOK_Comment",
};

////////////////////////////////////////////////////////////////////////////
// Default constructor
////////////////////////////////////////////////////////////////////////////
CPGNLex::CPGNLex()
{
 Reset();
}
  
////////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////////
CPGNLex::CPGNLex(istream &istr)
{
 Reset(istr);
}

////////////////////////////////////////////////////////////////////////////
// Sets the current token to TOK_BOF
////////////////////////////////////////////////////////////////////////////
void CPGNLex::Reset()
{
 Token = TOK_BOF;
 Value = 0;
 szString[0] = 0;
 CurrChar = '\n';
 ContextIndex = 0;
 for (int i = ContextSize + 1; --i >= 0;)
  szContextBuffer[i] = 0;
}

////////////////////////////////////////////////////////////////////////////
// Changes the input stream
////////////////////////////////////////////////////////////////////////////
void CPGNLex::Reset(istream &istr)
{
 pistr = &istr;

 Reset();
}

////////////////////////////////////////////////////////////////////////////
// Reading of the next character from the input stream
////////////////////////////////////////////////////////////////////////////
int CPGNLex::NextChar()
{
 if (pistr)
  CurrChar = pistr->get();
 else
  CurrChar = EOF;

 szContextBuffer[ContextIndex] = char(CurrChar);
 ContextIndex = (ContextIndex + 1) % ContextSize;
 szContextBuffer[ContextIndex] = 0;

 return CurrChar;
}

////////////////////////////////////////////////////////////////////////////
// Parse next token
////////////////////////////////////////////////////////////////////////////
int CPGNLex::ReadNextToken()
{
 lblNextToken:
  switch (CurrChar)
  {
   case EOF:
    Token = TOK_EOF;
   break; 

   //
   // Separators and escape character
   //
   case '\n':
    if (NextChar() == '%')
    {
     while (NextChar() != '\n')
      if (CurrChar == EOF)
      {
       Token = TOK_EOF;
       break;
      } 
     NextChar();
    }
   goto lblNextToken;

   case ' ': case '\t':
    NextChar();
   goto lblNextToken;

   //
   // Comments to EOL are parsed as separators
   //
   case ';':
    while (NextChar() != '\n')
     if (CurrChar == EOF)
     {
      Token = TOK_EOF;
      break;
     } 
    NextChar();
   goto lblNextToken;

   //
   // Comments between braces are parsed as tokens
   //
   case '{':
   {
    int i = 0;
    
    lblNextCharComment:
    switch (NextChar())
    {
     case EOF:
     {
      Token = TOK_EOF;
      goto lblReturn;
     } 
     case '}':
      szString[i] = 0;
      NextChar();
     break;
     default:
      if (CurrChar == '\n')
       szString[i] = ' ';
      else
       szString[i] = CurrChar; 
      if (++i >= MaxPGNLine)
       i = MaxPGNLine - 1;
     goto lblNextCharComment;
    }
    Token = TOK_Comment;
   } 
   break; 

   //
   // Strings
   //
   case '"':
   {
    int i = 0;

    lblNextCharString:
    switch (NextChar())
    {
     case EOF:
     {
      Token = TOK_EOF;
      goto lblReturn;
     } 
     case '"':
      szString[i] = 0;
      NextChar();
     break;
     case '\\':
      NextChar();
     default:
      szString[i] = CurrChar;
      if (++i >= MaxPGNLine)
       i = MaxPGNLine - 1;
     goto lblNextCharString;
    }
    Token = TOK_String;
   }
   break; 

   //
   // Tokens starting with digits
   //
   case '0': case '1': case '2': case '3': case '4':
   case '5': case '6': case '7': case '8': case '9':
   {
    int PrevChar = CurrChar;
    NextChar();

    //
    // Game termination
    //
    if (PrevChar == '1' && CurrChar == '-')
    {
     NextChar(); // 0
     NextChar();
     Value = CSTR::WhiteWins;
     Token = TOK_GameTermination;
     break;
    }
    if (PrevChar == '0' && CurrChar == '-')
    {
     NextChar(); // 1
     NextChar();
     Value = CSTR::BlackWins;
     Token = TOK_GameTermination;
     break;
    }
    if (PrevChar == '1' && CurrChar == '/')
    {
     NextChar(); // 2
     NextChar(); // -
     NextChar(); // 1
     NextChar(); // /
     NextChar(); // 2
     NextChar();
     Value = CSTR::Draw;
     Token = TOK_GameTermination;
     break;
    }

    //
    // Integers
    //
    Value = PrevChar - '0';
    while ('0' <= CurrChar && CurrChar <= '9')
    {
     Value = Value * 10 + CurrChar - '0';
     NextChar();
    }
    Token = TOK_Integer;
   }
   break;

   //
   // NAG
   //
   case '$':
    Value = -1;
    NextChar();
    Token = TOK_NAG;
   break; 

   case '?':
    Value = 2;
    switch(NextChar())
    {
     case '?':
      Value = 4;
      NextChar();
     break;

     case '!':
      Value = 6;
      NextChar();
     break;
    }
    Token = TOK_NAG;
   break;
    
   case '!':
    Value = 1;
    switch(NextChar())
    {
     case '?':
      Value = 5;
      NextChar();
     break;
     
     case '!':
      Value = 3;
      NextChar();
     break;
    }
    Token = TOK_NAG;
   break; 

   //
   // Single character tokens
   //
   case '*':
    NextChar();
    Value = CSTR::Unknown;
    Token = TOK_GameTermination;
   break; 

   case '.':
    NextChar();
    Token = TOK_Period;
   break;

   case '[':
    NextChar();
    Token = TOK_TagOpen;
   break;

   case ']':
    NextChar();
    Token = TOK_TagClose;
   break;

   case '(':
    NextChar();
    Token = TOK_RAVOpen;
   break;

   case ')':
    NextChar();
    Token = TOK_RAVClose;
   break; 

   case '<':
    NextChar();
    Token = TOK_ReservedOpen;
   break; 

   case '>':
    NextChar();
    Token = TOK_ReservedClose;
   break; 

   //
   // Symbols
   //
   default:
    if (('a' <= CurrChar && CurrChar <= 'z') ||
        ('A' <= CurrChar && CurrChar <= 'Z'))
    {
     int i = 0;

     do
     {
      szString[i] = CurrChar;
      if (++i >= MaxPGNLine)
       i = MaxPGNLine - 1;
      NextChar();
     }
     while (('a' <= CurrChar && CurrChar <= 'z') ||
            ('A' <= CurrChar && CurrChar <= 'Z') ||
            ('0' <= CurrChar && CurrChar <= '9') ||
            CurrChar == '_' ||
            CurrChar == '=' ||
            CurrChar == ':' ||
            CurrChar == '-' ||
            CurrChar == '+' ||
            CurrChar == '#');

     szString[i] = 0;
     Token = TOK_Symbol;
     break;
    }
    else // Unexpected characters are skipped
    {
     NextChar();
     goto lblNextToken;
    }
  }

lblReturn:

 TRACE(tszToken[Token] << ' ' << Value << ' ' << szString);
 
 return Token;
}

////////////////////////////////////////////////////////////////////////////
// Access to the previously read token value
////////////////////////////////////////////////////////////////////////////
int CPGNLex::TokenValue() const
{
 return Value;
}

////////////////////////////////////////////////////////////////////////////
// Access to the previously read token string
////////////////////////////////////////////////////////////////////////////
const char *CPGNLex::TokenString() const
{
 return szString;
}

////////////////////////////////////////////////////////////////////////////
// WaitForToken
////////////////////////////////////////////////////////////////////////////
int CPGNLex::WaitForToken(int Searched)
{
 while (Token != Searched)
  if (ReadNextToken() == TOK_EOF)
   return 1;
 
 return 0;
}

////////////////////////////////////////////////////////////////////////////
// PrintContext
////////////////////////////////////////////////////////////////////////////
void CPGNLex::PrintContext(std::ostream &out) const
{
 out << szContextBuffer + ContextIndex + 1 << szContextBuffer << '\n';
 out << "Token : " << tszToken[Token] << '\n';
 out << "Value : " << Value << '\n';
 out << "String : " << szString << '\n';
}
