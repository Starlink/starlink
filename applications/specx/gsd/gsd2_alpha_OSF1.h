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
 *    The generic file gsd2_unkwnown.h is intended for editing the platform-
 *    dependent files. It is prepared for alpha_OSF1, sun4_Solaris, sun4, and
 *    VAX.

 * Implementation Status:
 *    The sun4 and vax cases are untested.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    12 Dec 1994 (hme):
 *       Original version.
 *-
 */

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

static const int host_order[GSD_NTYPES] =
/* sun4_Solaris.
 * sun4.
   { BIGEND, BIGEND, BIGEND, BIGEND, IEEE, IEEE, 0 }; */
/* alpha_OSF1. */
   { LITTLEEND, LITTLEEND, LITTLEEND, LITTLEEND, IEEEBS, IEEEBS, 0 };
/* vax.
   { LITTLEEND, LITTLEEND, LITTLEEND, LITTLEEND, VAXF, VAXF, 0 }; */


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
 */

/* sun4_Solaris.
 * sun4.
static const union { unsigned char c[1]; char b; }
   val1__badb = { 0x80 };
static const union { unsigned char c[2]; short w; }
   val1__badw = { 0x80, 0x00 };
static const union { unsigned char c[4]; int i; }
   val1__badi = { 0x80, 0x00, 0x00, 0x00 };
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0x7F, 0xFF, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }; */

/* alpha_OSF1. */
static const union { unsigned char c[1]; char b; }
   val1__badb = { 0x80 };
static const union { unsigned char c[2]; short w; }
   val1__badw = { 0x00, 0x80 };
static const union { unsigned char c[4]; int i; }
   val1__badi = { 0x00, 0x00, 0x00, 0x80 };
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0xFF, 0x7F, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xEF, 0xFF };

/* vax.
static const union { unsigned char c[1]; char b; }
   val1__badb = { 0x80 };
static const union { unsigned char c[2]; short w; }
   val1__badw = { 0x00, 0x80 };
static const union { unsigned char c[4]; int i; }
   val1__badi = { 0x00, 0x00, 0x00, 0x80 };
static const union { unsigned char c[4]; float r; }
   val1__badr = { 0xFF, 0xFF, 0xFF, 0xFF };
static const union { unsigned char c[8]; double d; }
   val1__badd = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }; */

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
