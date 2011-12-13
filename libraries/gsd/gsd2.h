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

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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

/* Bad values */
#include "prm_par.h"

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
#   error "Big endian system without IEEE floats. Do not know what to do"
# endif
#else
# define GSD_INTEND LITTLEEND
# ifdef HAVE_IEEE_FLOATS
#   define GSD_FLOAT  IEEEBS
# else
#   warn "Little endian system without IEEE floats. Assuming VAX floats"
#   define GSD_FLOAT VAXF
# endif
#endif

/* GSD requires that we have 1 byte char, 2 byte short, 4 byte float
   and int and 8 byte double. All else will break the conversions.
   Probably should use types that are guaranteed to be the correct
   size (int32_t?). For now just abort if this is not the case */

# if SIZEOF_CHAR != 1
#  error "GSD library assumes 1 byte char"
# endif

# if SIZEOF_SHORT_INT != 2
#  error "GSD library assumes 2 byte short int"
# endif

# if SIZEOF_INT != 4
#  error "GSD library assumes 4 byte int"
# endif

# if SIZEOF_FLOAT != 4
#  error "GSD library assumes 4 byte float"
# endif

# if SIZEOF_DOUBLE != 8
#  error "GSD library assumes 8 byte double"
# endif


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
 * Store the PRIMDAT values in a union so that we can convert them
 * easily to a byte array (for byte to byte comparison).
 */

/* Create the unions with primdat bad values */
static const union { char b; unsigned char c[1];}
  val1__badb = { VAL__BADB };
static const union { short w; unsigned char c[2]; }
  val1__badw = { VAL__BADW };
static const union { int i; unsigned char c[4]; }
  val1__badi = { VAL__BADI };
static const union { float r; unsigned char c[4]; }
  val1__badr = { VAL__BADR };
static const union { double d; unsigned char c[8]; }
  val1__badd = { VAL__BADD };

/* Now actually set up the shorthand macros to the byte arrays*/

#define val__badb val1__badb.c
#define val__badw val1__badw.c
#define val__badi val1__badi.c
#define val__badr val1__badr.c
#define val__badd val1__badd.c

#endif
/* Bottom of gsd2.h */
