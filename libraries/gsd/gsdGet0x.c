/*
*+
* Name:
*    gsdGet0<t>

* Purpose:
*    Get a scalar value from a GSD file.

* Language:
*    ANSI C

* Type of Module:
*    C function.

* Invocation:
*    int gsdGet0{blwirdc}( const GSDFileDesc *file_dsc,
*       const  GSDItemDesc *item_dsc, const char *data_ptr,
*       int itemno, <type> *value );

* Prototype:
*    available via #include "gsd.h"
 *    int gsdGet0{blwirdc}( const GSDFileDesc *file_dsc,
 *       const GSDItemDesc *item_dsc, char *data_ptr,
 *       int itemno, <type> *value );

* Description:
*    This routine returns the value of a scalar GSD item. The item must be
*    specified by the file desciptor, item descriptor array, data array and
*    item number.
*
*    <t> <type>     Fortran       GSD
*     b   char      byte          byte
*     l   char      logical*1     logical
*     w   short     integer*2     word
*     i   int       integer*4     integer
*     r   float     real*4        real
*     d   double    real*8        double
*     c   char[17]  character*16  char
*
*    This routine will convert between numeric types (all but GSD type char).
*    That is to say, the calling routine can request, say, an integer value by
*    calling gsdGet0i, even if the item in the GSD file has a different
*    numeric type, say real. C casting rules are applied, which may differ
*    from Fortran truncation rules. No test for conversion errors is
*    performed.

* Arguments:
*    const GSDFileDesc *file_dsc (Given)
*       The GSD file descriptor.
*    const GSDItemDesc *item_dsc (Given)
*       The array of GSD item descriptors related to the GSD file.
*    const char *data_ptr (Given)
*       The buffer with all the data from the GSD file.
*    int itemno (Given)
*       The number of the item in the GSD file.
*    <type> *value (Returned)
*       The data value. For gsdGet0c value should be declared with length 17
*       at least. The returned string is null-terminated in value[16].

* Returned value:
*    int gsdGet0<t>();
*       Status.
*        -[1:] Failure to read the item value.
*        -[2:] Numbered item cannot be found.
*        -[3:] Item is not scalar.
*        -[0:] Otherwise.

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
*    jhf: Jon Fairclough (UKTH)
*    rp: Rachael Padman (MRAO)
*    hme: Horst Meyerdierks (UoE, Starlink)
*    timj: Tim Jenness (JAC, Hawaii)


* History:
*    08 Sep 1986 (jhf):
*       Original.
*    01 Feb 1988 (jhf):
*       Change indexing system.
*    17 Jul 1994 (rp):
*       Adaption to Remo's C code.
*    02 Dec 1994 (hme):
*       Translation to C. Interface revised.
*    07 Dec 1994 (hme):
*       Let gsd1_getval do the type conversion.
*       Do not abort when the item is an array. But always specify to
*       gsd1_getval, that element 1 is the first and last.
*    04 Jul 2008 (timj):
*       use proper GSD structs rather than void. use const.

* Copyright:
*    Copyright (C) 2008 Science and Technology Facilities Council.
*    Copyright (C) 1986-1999 Particle Physics and Astronomy Research Council.
*    All Rights Reserved.
*-
*/

#include <stdio.h>
#include <string.h>
#include "gsd1.h"
#include "gsd.h"

/*:
 */

int gsdGet0b( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 1, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0l( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 2, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0w( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, short *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 3, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0i( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, int *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 4, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0r( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, float *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 5, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0d( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, double *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 6, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;

/* Return.
 */
   abort:
   return status;
}

/*:
 */

int gsdGet0c( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, char *value )
{
   int    status;
   char   name[16];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Get the value into our data buffer.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 7, name,
      &itemno, 1, 1, (char *) value );
   if ( status < 0 ) { status = 1; goto abort; }
   else                status = 0;
   value[16] = '\0';

/* Return.
 */
   abort:
   return status;
}
