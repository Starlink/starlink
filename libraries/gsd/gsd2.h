/* Top of gsd2.h */
#if !defined GSD2_H
#define GSD2_H
/*+
 * Name:
 *    gsd2.h

 * Purpose:
 *    Internal include file for GSD library.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    C function.

 * Invocation:
 *    #include "gsd2.h"

 * Description:
 *    This include file contains platform-dependent settings necessary to
 *    convert byte groups from old VAX/GSD files to the native format of the
 *    local machine, and to convert between different data types on the local
 *    machine.
 *

 * Implementation Status:
 *    The sun4 and vax cases are untested.
 *    The bad values for linux are untested.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    timj: Tim Jenness (JAC, Hilo)

 * History:
 *    12 Dec 1994 (hme):
 *       Original version.
 *    15 Dec 1999 (timj):
 *       Add Linux
 *    10 Mar 2004 (timj):
 *       Switch to configure based checking

 * Copyright:
 *    Copyright (C) 1994-1999,2004 Particle Physics and Astronomy Research 
 *    Council. All Rights Reserved.

 *-
 */

/* System specfic defines */
#include <config.h>

/* General gsd definitions */
#include "gsd1.h"


/* For the various platforms define the static global host_order, which is used
 * by routines to decide to do the right thing on the actual machine.
 * host_order says something about the byte order and FP format of the machine.
 * The elements in order are concerened with
 *  0: byte
 *  1: logical
 *  2: word
 *  3: integer
 *  4: real
 *  5: double
 *  6: char
 * Logical and word are currently not used in GSD, expect bugs for those.
 */

#define  LITTLEEND 0            /* Little endian integers */
#define  BIGEND    1            /* Big endian integers */
#define  VAXF      0            /* VAX  floating point format */
#define  IEEE      1            /* IEEE floating point format */
#define  IEEEBS    2            /* IEEE floating point format byte swapped */


/* See if we have IEEE floats */
#if STDC_HEADERS
# include <float.h>
# if FLT_RADIX == 2
#  if FLT_MANT_DIG == 24
#    define HAVE_IEEE_FLOATS
#  endif
# endif
#endif

/* Choose the endianness of our ints and floats*/
#ifdef WORDS_BIGENDIAN
# define GSD_INTEND BIGEND
# ifdef HAVE_IEEE_FLOATS
#   define GSD_FLOAT  IEEE
# else
    /* have no idea */
#   define GSD_FLOAT IEEE
# endif
#else
# define GSD_INTEND LITTLEEND
# ifdef HAVE_IEEE_FLOATS
#   define GSD_FLOAT  IEEEBS
# else
#   define GSD_FLOAT  VAXF
# endif
#endif



/* Since I have not worked out how configure will tell me the form
   of floating point numbers (real and double) assume IEEE */

static const int host_order[GSD_NTYPES] =
 { GSD_INTEND, GSD_INTEND, GSD_INTEND, GSD_INTEND, GSD_FLOAT, GSD_FLOAT, 0 };


/* Set the VAX/GSD bad value patterns. Note these are different from
 * VAX/PRIMDAT patterns and different from IEEE/PRIMDAT patterns. Since this
 * version of the GSD library is concerned only with reading and only with VAX
 * GSD files, we know this is what to expect in GSD files.
 */

static const unsigned char gsd__bbad[1] = { 0x81 };
static const unsigned char gsd__wbad[2] = { 0x01, 0x80 };
static const unsigned char gsd__ibad[4] = { 0x01, 0x00, 0x00, 0x80 };
static const unsigned char gsd__rbad[4] = { 0xFF, 0xFF, 0xF7, 0xFF };
static const unsigned char gsd__dbad[8] = { 0xFF, 0xFF, 0xF7, 0xFF,
                                            0xFF, 0xFF, 0xFF, 0xFF };


/* Set the PRIMDAT bad value pattern for the local machine. The VAX/GSD bad
 * patterns are replaced with these (even on a VAX).
 *
 * This simply switches on endiannes. In principal we should be using
 * primdat directly.
 */

#ifdef WORDS_BIGENDIAN
static const union { unsigned char c[1]; char b; }
   val1__badb = { 0x80 };
static const union { unsigned char c[2]; short w; }
   val1__badw = { 0x80, 0x00 };
static const union { unsigned char c[4]; int i; }
   val1__badi = { 0x80, 0x00, 0x00, 0x00 };
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0x7F, 0xFF, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }; 
#else
static const union { unsigned char c[1]; char b; }
   val1__badb = { 0x80 };
static const union { unsigned char c[2]; short w; }
   val1__badw = { 0x00, 0x80 };
static const union { unsigned char c[4]; int i; }
   val1__badi = { 0x00, 0x00, 0x00, 0x80 };
# if GSD_FLOAT == VAXF
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0xFF, 0xFF, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };
# else
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0xFF, 0x7F, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xEF, 0xFF };
# endif
#endif

/* Now actually set up the shorthand macros */

#define val__badb val1__badb.c
#define VAL__BADB val1__badb.b
#define val__badw val1__badw.c
#define VAL__BADW val1__badw.w
#define val__badi val1__badi.c
#define VAL__BADI val1__badi.i
#define val__badr val1__badr.c
#define VAL__BADR val1__badr.r
#define val__badd val1__badd.c
#define VAL__BADD val1__badd.d

#endif
/* Bottom of gsd2.h */
