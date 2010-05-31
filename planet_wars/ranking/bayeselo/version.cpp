////////////////////////////////////////////////////////////////////////////
//
// version.cpp
//
// CVersion class
//
// Remi Coulom
//
// May, 1997
//
////////////////////////////////////////////////////////////////////////////
#include "version.h"

#include "version_number.h"

const char CVersion::szVersion[] = VERSION;
const char CVersion::szCopyright[] = 
"version "VERSION", Copyright (C) 1997-2007 Remi Coulom.\n"
"compiled "__DATE__" "__TIME__".\n"
"This program comes with ABSOLUTELY NO WARRANTY.\n"
"This is free software, and you are welcome to redistribute it\n"
"under the terms and conditions of the GNU General Public License.\n"
"See http://www.gnu.org/copyleft/gpl.html for details.\n";

const char CVersion::szBinary[] = 
"version "VERSION".\n"
"Copyright 1997-2005 by Remi Coulom. All Rights Reserved.\n"
"Permission is granted to freely use and distribute this program,\n"
"provided that no charge is made for it and it is distributed unmodified\n"
"This program comes with ABSOLUTELY NO WARRANTY.\n";
