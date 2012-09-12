/*
*+
* Name:
*    gsdGet1<t>

* Purpose:
*    Get an array from a GSD file.

* Language:
*    ANSI C

* Type of Module:
*    C function.

* Invocation:
*    int gsdGet1{blwird}( const GSDFileDesc *file_dsc,
*       const GSDItemDesc *item_dsc, const char *data_ptr,
*       int itemno, int ndims, const int dimvals[], const int start[],
*       const int end[],
*       <type> *values, int *actvals );

* Prototype:
*    available via #include "gsd.h"
 *    int gsdGet1{blwird}( const GSDFileDesc *file_dsc,
 *       const GSDItemDesc *item_dsc, const char *data_ptr,
 *       int itemno, int ndims, const int dimvals[], const int start[],
 *       const int end[],
 *       <type> *values, int *actvals );

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
*     c   char[16]  character*16  char
*
*    This routine does not convert between types. If the type of the GSD item
*    does not match the type of the routine, then it returns with an error.
*
*    It is possible to get only part of the array. Although the part can be
*    specified in terms of an N-dimensional array, this routine does not take
*    a proper N-D section of the array. The caller can specify the start
*    pixel in N dimensions and the end pixel in N dimensions. These two pixels
*    will be converted to memory locations and all memory between the two is
*    returned. This emulates the old GSD library. It is useful really only for
*    parts of 1-D arrays, parts of rows, or single pixels.

* Arguments:
*    const GSDFileDesc *file_dsc (Given)
*       The GSD file descriptor.
*    const GSDItemDesc *item_dsc (Given)
*       The array of GSD item descriptors related to the GSD file.
*    const char *data_ptr (Given)
*       The buffer with all the data from the GSD file.
*    int itemno (Given)
*       The number of the item in the GSD file.
*    int ndims (Given)
*       The dimensionality the calling routine uses to specify the start and
*       end elements.
*    const int dimvals[] (Given)
*       The array of ndims dimensions (array sizes along each axis).
*    const int start[] (Given)
*       The array indices for the first element.
*    const int end[]
*       The array indices for the last element.
*    <type> *value (Returned)
*       The data values. The calling routine must make sure that sufficient
*       memory is provided. Thus it must find out the data type and array size
*       before calling this routine.
*       If the data type is character, then the routine returns a byte
*       buffer with all strings concatenated. There are no string
*       terminators in the buffer and there is none at the end. Each
*       string is 16 byte long and immediately followed by the next string.
*    int *actvals (Returned)
*       The number of array values returned. This saves the caller to work out
*       how many array elements correspond to start and end given the dimvals.

* Returned value:
*    int gsdGet1<t>();
*       Status.
*        -[1:] Failure to read the item values.
*        -[2:] Numbered item cannot be found.
*        -[4:] Given start and end are inconsistent.
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

static int ndindex( int ndims, const int dimvals[], const int start[] );

/*:
 */

int gsdGet1b( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
   int itemno, int ndims, const int dimvals[], const int start[], const int end[],
   char *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 1, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1l( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr, int itemno, int ndims, const int dimvals[],
              const int start[], const int end[],
              char *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 2, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1w( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr,
              int itemno, int ndims, const int dimvals[], const int start[],
              const int end[],
              short *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 3, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1i( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr, int itemno, int ndims, const int dimvals[],
              const int start[], const int end[],
              int *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 4, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1r( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr, int itemno, int ndims, const int dimvals[],
              const int start[], const int end[],
              float *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];
   int i;

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 5, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   printf("Status = %d first=%d last=%d, '%s'\n", status,first,last, name );
   if ( status ) {
     *actvals = 0;
   } else {
     *actvals = last - first + 1;
   }
   for (i=0; i<*actvals;i++) {
     printf("values[%d] = %g\n", i, values[i] );
   }
   return status;
}

/*:
 */

int gsdGet1d( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr, int itemno, int ndims, const int dimvals[],
              const int start[], const int end[],
              double *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 6, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1c( const GSDFileDesc *file_dsc, const GSDItemDesc *item_dsc,
              const char *data_ptr, int itemno, int ndims, const int dimvals[],
              const int start[], const int end[],
              char *values, int *actvals )
{
   int    status;
   int    first, last;
   char   name[GSD_NAMELEN+1];

/*.
 */

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out 1-D range.
 */
   first = ndindex( ndims, dimvals, start );
   last  = ndindex( ndims, dimvals, end   );
   if ( first < 0 || last < 0 || last < first ) { status = 4; goto abort; }

/* Get the values.
 */
   status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 7, name,
      &itemno, first, last, (char *) values );
   if ( status < 0 ) status = 1;
   else              status = 0;

/* Return.
 */
   abort:
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

/* Translation of the old gsd_sys_location.
 */

static int ndindex( int no_dims, const int bounds[], const int subscripts[] )
{
   int cell_count, i, j;
   int index;

   index = subscripts[0];
   for ( i = no_dims; i >= 2; i-- )
   {  cell_count = subscripts[i-1] - 1;
      if ( cell_count > 0 )
      {  for ( j = 1; j <= i-1; j++ )
            cell_count *= bounds[j-1];
         index += cell_count;
      }
   }
   return index;
}
