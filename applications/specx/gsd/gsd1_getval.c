/*+
 * Name:
 *    gsd1_getval

 * Purpose:
 *    Get values from GSD file (memory).

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    int gsd1_getval( struct file_descriptor *file_dsc,
 *       struct item_descriptor *item_dsc, char *data_ptr,
 *       int mode, short data_type, char *name, int *itemno,
 *       int first, int last, char *gsdval );

 * Description:
 *    Function to get values from the GSD data in memory. The routine 
 *    finds a GSDITEM and returns all the associated data as a byte-string.
 *    This routine is not meant to deal with multiple-scan GSD files.
 *   
 *    There are several different modes of operation:
 *   
 *       (GSD item numbers start at 1).
 *
 *       mode 0: search for item by name and return the item number and the
 *               data.
 *       mode 1: search for item by name and return the item number only.
 *       mode 2: search for item by number and return name and data.
 *       mode 3: search for item by number and return name only.
 *   
 *    Unless pfirst or plast are 0 (in which case the whole array is
 *    returned), the first array item returned will be pfirst, the
 *    last plast. Array item indices START with 1.
 *
 *    This routine assumes that all items from the file have been read into
 *    memory. If this is not the case, it will just read from unrelated memory.
 *
 *    This routine will convert between numeric data types, if the requested
 *    type differs from the type used in the file.

 * Arguments:
 *    struct file_descriptor *file_dsc (Given)
 *       The GSD file descriptor.
 *    struct item_descriptor *item_dsc (Given)
 *       The array of GSD item descriptors.
 *    char *data_ptr (Given)
 *       The pointer to the data previously read from the GSD file into memory.
 *       This argument is unused in mode 1.
 *    int mode (Given)
 *       The mode in which the routine is to be used. 0, 1, 2, or 3.
 *    short data_type (Given)
 *       The data type requested by the caller. 1 ... 7 for B, L, W, I, R, D,
 *       C, resp. This is the same value as used in the item descriptor's
 *       data_type member. This argument is unused in modes 1 and 3.
 *    char *name (Given or Returend)
 *       In modes 0 and 1 given, in mode 2 and 3 returned. The name of the
 *       item. This should be an array of 16 characters (char name[16]) and a
 *       null-terminated string.
 *    int *itemno (Given or Returned)
 *       In modes 0 and 1 returned, in mode 2 and 3 given. The number of the
 *       item. GSD items are counted starting with 1.
 *    int first (Given)
 *       The first array element to be returned. Use zero to get whole array.
 *       Array elements count from 1 (Fortran convention)!
 *    int last (Given)
 *       The last array element to be returned. Use zero to get whole array.
 *       Array elements count from 1 (Fortran convention)!
 *    char *gsdval (Returned)
 *       Unused in mode 1 and 3. In modes 0 and 2 the data requested are copied
 *       to this buffer. It must be big enough. Otherwise this routine will
 *       overwrite unrelated memory. Note that this is a byte buffer and
 *       not a null-terminated string, even if the data type is character.

 * Returned value:
 *    int gsd1_getval();
 *       Status.
 *       -1: Item not found.
 *       -2: Specified part of array exceeds bounds.
 *       -3: Impossible conversion requested.
 *        N: Otherwise, the item number.

 * Authors:
 *    rpt: Remo Tilanus (JACH)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    04 Feb 1994 or 02 Apr 1994 (rpt):
 *       Original version.
 *    02 Dec 1994 (hme):
 *       Split off gsd.c. This is the only routine that uses the static int
 *       array gsd_byte[]. So that can now be removed from gsd.h.
 *       Add a given argument for declared size of the buffer, and do not copy
 *       more data than fit into the buffer.
 *       ANSI C. Review interface completely. Move static array inside the
 *       routine.
 *       Add mode 3.
 *    07 Dec 1994 (hme):
 *       Check that last item is within array.
 *       Convert numeric types.
 *-
 */

#include <ctype.h>
#include <string.h>
#include "gsd1.h"

/*:
 */

int gsd1_getval( struct file_descriptor *file_dsc,
   struct item_descriptor *item_dsc, char *data_ptr,
   int mode, short data_type, char *name, int *itemno,
   int first, int last, char *gsdval )
{ 
   const int gsd_byte[GSD_NTYPES] =  /* Size for each type. */
   {  GSD_SZBYTE, GSD_SZLOGICAL, GSD_SZWORD, GSD_SZINTEGER,
      GSD_SZREAL, GSD_SZDOUBLE,  GSD_SZCHAR
   };

   struct item_descriptor *item_dsc2, *dim_ptr;

   char  *byte_ptr;
   char   upper_name[16];
   int    i, j, status;
   int    dlength;
   size_t nbytes;
   double buffer[4];

/*.
 */


/* Name to number or number to name.
 * =================================
 */

/* If name is given, number must be returned, and vice versa.
 * This implies locating the item in question, of course.
 */
   if ( mode == 2 || mode == 3 )                           /* Number given */
   {  if ( *itemno < 1 || *itemno > file_dsc->no_items ) return -1;
      item_dsc2 = item_dsc + *itemno - 1;
      (void) memcpy( name, item_dsc2->name, 15 ); name[15] = '\0';
   }
   else                                                      /* Name given */
   {  for ( i = 0; name[i]; i++ ) upper_name[i] = toupper( name[i] );
      for (      ; i <  15; i++ ) upper_name[i] = ' ';
      upper_name[15] = '\0';
      for ( j = 0; j < file_dsc->no_items; j++ )
         if ( !memcmp( (item_dsc+j)->name, upper_name, 15 ) ) break;
      if ( j >= file_dsc->no_items ) return -1;
      *itemno = j + 1;
      item_dsc2 = item_dsc + *itemno - 1;
   }

/* If the caller does not want any data (modes 1 and 3) just return now.
 */
   if ( mode == 1 || mode == 3 ) return *itemno;


/* Copy data.
 * ==========
 */

/* Cell size in bytes (according to data type in file).
 */
   dlength = gsd_byte[item_dsc2->data_type-1];

/* Update first and last.
 */
   if ( first == 0 ) first = 1;
   if (  last == 0 )  last = item_dsc2->length / dlength;

/* Check first, last and size.
 */
   if ( first < 1 || last < 1 || last < first ||
        last > item_dsc2->length / dlength )
      return -2;

/* Pointer to start of array (not to first requested element).
 */
   byte_ptr  = data_ptr + item_dsc2->location - file_dsc->str_data;
   byte_ptr += ( first - 1 ) * dlength;

/* gsd2_copya wants to know the two types, the size in number of elements and
 * the pointers to input and output. Size and pointers refer to the first
 * element requested and number of elements requested, not the whole array.
 */
   status = gsd2_copya( (enum type_tag) item_dsc2->data_type,
      (enum type_tag) data_type, (last-first+1), byte_ptr, gsdval );
   if ( status < 0 ) return -3;

/* Return.
 */
   return *itemno;
}
