// Part of dvi2bitmap.
// Copyright 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef BYTE_HEADER_READ
#define BYTE_HEADER_READ 1

// I believe this to be the only machine-dependent bit, since in
// principle a byte could be different from an unsigned char on some
// odd platforms.  We don't even have to worry about bytesex.
typedef unsigned char Byte;

#endif // #ifdef BYTE_HEADER_READ
