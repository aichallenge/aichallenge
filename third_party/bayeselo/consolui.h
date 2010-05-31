////////////////////////////////////////////////////////////////////////////
//
// consolui.h
//
// CConsoleUI class declaration
//
// This class is an abstract class, used to define console user interfaces.
// It is typically used by adding a pointer to a class, and overriding the
// ProcessCommand member function to process commands on this class.
// See classes that inherit from CConsoleUI to find examples demonstrating
// how to do so.
//
// Remi Coulom
//
// june 1996
//
////////////////////////////////////////////////////////////////////////////
#ifndef CONSOLUI_H
#define CONSOLUI_H

#include <iosfwd>

#include "list.h" // CList<>

class CConsoleUI //cui
{
 private: //////////////////////////////////////////////////////////////////
  //
  // private data
  //
  static const int BufferSize;
  static const char * const tszCommands[];

  CList<CConsoleUI *> lpcuiChildren;
  CConsoleUI *pcuiParent;
  int fPrompt;
  const int openmode;

  //
  // private functions
  //
  void Parse(char *pszCommand,
             const char *&pszCommand2,
             const char *&pszParameters,
             const char *&pszInput,
             const char *&pszOutput) const;

  void OpenChild(int Parameter, std::istream &in, std::ostream &out);

  //
  // Copy constructor and assignment operator do not work
  //
  // CConsoleUI(const CConsoleUI &cui): openmode(0)
  //  {const CConsoleUI *pcui = &cui; pcui = pcui;}
  // CConsoleUI &operator=(const CConsoleUI &cui): openmode(0)
  //  {const CConsoleUI *pcui = &cui; pcui = pcui; return *this;}

 protected: ////////////////////////////////////////////////////////////////
  //
  // Prompt functions
  //
  virtual void PrintLocalPrompt(std::ostream &out) = 0;
  void PrintPrompt(std::ostream &out);

  //
  // Useful fonctions to help processing commands
  // ??? (should not be a member function)
  //
  const char *SetFlag(int &f, const char *psz);
  int ArrayLookup(const char * const psz,
                  const char * const * const ppsz);

  //
  // This function is called repeatedly by EnterParseAndProcess
  //
  virtual int ProcessCommand(const char *pszCommand,
                             const char *pszParameters,
                             std::istream &in,
                             std::ostream &out);

  //
  // Opening modes
  //
  enum // openmode
  {
   OpenModal,
   OpenModeless,
   OpenPermanent
  };

  //
  // Function to print an error message
  //
  virtual void PrintErrorMessage(std::ostream &out, const char *pszCommand);
  
  //
  // Function used to add a CConsoleCUI to the list of modeless children
  // These user interfaces are deleted when they are closed (they must have
  // been allocated dynamically by new ...)
  //
  void AddChild(CConsoleUI *pcui, std::istream &in, std::ostream &out);

  //
  // Utility function to get[set] variables
  //
  template<class T>
  static void GetSet(T &x, const char *psz, std::ostream &out);

 public: ///////////////////////////////////////////////////////////////////
  //
  // Constructor
  //
  CConsoleUI(CConsoleUI *pcui = 0, int openmodeInit = OpenModal);

  //
  // ProcessCommand return values
  //
  enum
  {
   PC_Continue, // The command loop should go on
   PC_Close,    // The command loop should stop and return PC_Close
   PC_Parent    // The command loop should stop and return PC_Parent
  };

  //
  // Functions to set/get the prompt flag
  //
  void SetFPrompt(int i) {fPrompt = !!i;};
  int GetFPrompt() const {return fPrompt;};

  //
  // set parent
  //
  void SetParent(CConsoleUI *pcui);
 
  //
  // Function to process a single command
  //
  int EnterParseAndProcess(std::istream &in,
                           std::ostream &out,
                           std::ostream *pofsLog = 0);

  //
  // Main command loop of calls to EnterParseAndProcess
  //
  int MainLoop(std::istream &in, std::ostream &out);

  //
  // This class has virtual functions, so we provide a virtual destructor
  //
  virtual ~CConsoleUI();
};

#endif
