/*+
 * Name:
 *    gsdGet1<t>

 * Purpose:
 *    Get an array from a GSD file.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd.h")
 *    int gsdGet1{blwird}( void *file_dsc, void *item_dsc, char *data_ptr,
 *       int itemno, int ndims, int *dimvals, int *start, int *end,
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
 *    void *file_dsc (Given)
 *       The GSD file descriptor.
 *    void *item_dsc (Given)
 *       The array of GSD item descriptors related to the GSD file.
 *    char *data_ptr (Given)
 *       The buffer with all the data from the GSD file.
 *    int itemno (Given)
 *       The number of the item in the GSD file.
 *    int ndims (Given)
 *       The dimensionality the calling routine uses to specify the start and
 *       end elements.
 *    int *dimvals (Given)
 *       The array of ndims dimensions (array sizes along each axis).
 *    int *start (Given)
 *       The array indices for the first element.
 *    int *end
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
 *        1: Failure to read the item values.
 *        2: Numbered item cannot be found.
 *        4: Given start and end are inconsistent.
 *        0: Otherwise.

 * Authors:
 *    jhf: Jon Fairclough (UKTH)
 *    rp: Rachael Padman (MRAO)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Sep 1986 (jhf):
 *       Original.
 *    01 Feb 1988 (jhf):
 *       Change indexing system.
 *    17 Jul 1994 (rp):
 *       Adaption to Remo's C code.
 *    02 Dec 1994 (hme):
 *       Translation to C. Interface revised.
 *-
 */

#include <stdio.h>
#include <string.h>
#include "gsd1.h"
#include "gsd.h"

static int ndindex( int ndims, int *dimvals, int *start );

/*:
 */

int gsdGet1b( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[16];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

int gsdGet1l( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[15];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

int gsdGet1w( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   short *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[15];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

int gsdGet1i( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   int *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[15];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

int gsdGet1r( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   float *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[15];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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
   if ( status ) *actvals = 0; else *actvals = last - first + 1;
   return status;
}

/*:
 */

int gsdGet1d( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   double *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[15];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

int gsdGet1c( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int ndims, int *dimvals, int *start, int *end,
   char *values, int *actvals )
{
   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int    status;
   int    first, last;
   int    size;
   char   name[16];

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

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

static int ndindex( int no_dims, int *bounds, int *subscripts )
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
