/*+
 * Name:
 *    gsdInqSize

 * Purpose:
 *    Inquire array size.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd.h")
 *    int gsdInqSize( void *file_dsc, void *item_dsc, char *data_ptr,
 *       int itemno, int maxdims,
 *       char **dimnames, char **dimunits, int *dimvals,
 *       int *actdims, int *size );

 * Description:
 *    This routine returns information about the specified array. Returned are
 *    the names and units of each dimension, the size along each dimension, and
 *    the overall size.

 * Arguments:
 *    void *file_dsc (Given)
 *       The GSD file descriptor.
 *    void *item_dsc (Given)
 *       The array of GSD item descriptors related to the GSD file.
 *    char *data_ptr (Given)
 *       The buffer with all the data from the GSD file.
 *    int itemno (Given)
 *       The number of the item in the GSD file.
 *    int maxdims (Given)
 *       The number of dimensions required and accommodated by the calling
 *       routine.
 *    char **dimnames (Returned)
 *       The names for each dimension. The calling routine must provide maxdims
 *       pointers to strings. It must also provide the space for the strings,
 *       16 bytes. See Notes for how to declare and pass dimnames.
 *    char **dimunits (Returned)
 *       The units for each dimension. The calling routine must provide maxdims
 *       pointers to strings. It must also provide the space for the strings,
 *       11 bytes. See Notes for how to declare and pass dimunits.
 *    int *dimvals (Returned)
 *       The values for each dimension. The calling routine must provide an
 *       array of maxdims integers. This would probably be declared as
 *       int dimvals[MAXDIMS];
 *    int *actdims (Returned)
 *       The actual number of dimensions. If actdims is less than maxdims, then
 *       only actims elements are returned in dimnames, dimunits, dimvals.
 *       Further elements declared by the caller are unchanged by this routine.
 *    int *size (Returned)
 *       The total number of elements in the array.

 * Returned value:
 *    int gsdInqSize();
 *       Status.
 *        1: Failed to get a dimension value and name.
 *        2: Numbered item does not exist.
 *        3: Array has more dimensions than accommodated by calling routine.
 *        0: Otherwise.

 * Note:
 *    The calling routine will probably allocate storage for dimension names by
 *    declaring a two-dimensional array. That is not suitable for passing to
 *    this routine though. The pointers to each string must be copied into an
 *    array of pointers. For example:
 *
 *       char  actual_space[MAXDIMS][16];
 *       char *pointr_array[MAXDIMS];
 *       for ( i = 0; i < MAXDIMS; i++ ) pointr_array[i] = actual_space[i];
 *       status = gsdInqSize( ..., pointr_array, ... );
 *
 *    The reason why this call works but passing actual_space does not work, is
 *    that gsdInqSize uses the given value as a char **. So in this routine
 *    given[1] goes forward in memory by the size of a char * or the number of
 *    bytes needed to store a pointer. actual_space would need a step in memory
 *    by 16 bytes, i.e. the distance from one string to the next. The main
 *    routine knows about this, because it declared actual_space _and_
 *    pointr_array.

 * Authors:
 *    jhf: Jon Fairclough (UKTH)
 *    rp: Rachael Padman (MRAO)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Sep 1986 (jhf):
 *       Original.
 *    21 Jan 1988 (jhf):
 *       Improve code modularisation.
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

/*:
 */

int gsdInqSize( void *file_dsc_arg, void *item_dsc_arg, char *data_ptr,
   int itemno, int maxdims,
   char **dimnames, char **dimunits, int *dimvals, int *actdims, int *size )
{
   const int gsd_byte[GSD_NTYPES] =  /* Size for each type. */
   {  GSD_SZBYTE, GSD_SZLOGICAL, GSD_SZWORD, GSD_SZINTEGER,
      GSD_SZREAL, GSD_SZDOUBLE,  GSD_SZCHAR
   };

   struct file_descriptor *file_dsc;
   struct item_descriptor *item_dsc;

   int  status;
   int  i, dimitemno;

/*.
 */

/* Cast given descriptor pointers.
 */
   file_dsc = (struct file_descriptor *) file_dsc_arg;
   item_dsc = (struct item_descriptor *) item_dsc_arg;

/* Check item number is valid.
 */
   if ( itemno < 1 || itemno > file_dsc->no_items ) { status = 2; goto abort; }

/* Work out the scalar return values.
 */
   *actdims = (item_dsc+itemno-1)->no_dims;
   *size    = (item_dsc+itemno-1)->length;
   *size   /= gsd_byte[(item_dsc+itemno-1)->data_type-1];

/* Check actual dimensions <= accommodated dimensions.
 */
   if ( *actdims > maxdims ) { status = 3; goto abort; }

/* Get information for each actual dimension.
 */
   for ( i = 0; i < *actdims; i++ )
   {
/*    The item with the given number contains for each dimension another item
 *    number where we can look up the description of the diemension.
 */
      dimitemno = (item_dsc+itemno-1)->dimnumbers[i];

/*    Look up the dimension value. As a side effect we get the name as well.
 *    mode = 2 to find by number and get data.
 *    data_type = 4 for integer.
 */
      status = gsd1_getval( file_dsc, item_dsc, data_ptr, 2, 4, dimnames[i],
         &dimitemno, 0, 0, (char *) (dimvals+i) );
      if ( status < 0 ) { status = 1; goto abort; }
      else                status = 0;

/*    Copy the dimension unit.
 */
      (void) memcpy( dimunits[i], (item_dsc+dimitemno-1)->unit, 10 );
      dimunits[i][10] = '\0';
   }

/* Return.
 */
   abort:
   return status;
}
