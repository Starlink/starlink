/*+
 * Name:
 *    gsd1_rddata

 * Purpose:
 *    Read GSD data.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    int gsd1_rddata( FILE *fptr, struct item_descriptor *item_ptr,
 *       char *data_ptr, int startitem, int noitems )

 * Description:
 *     Routine to read data associated with header items from GSD file. It 
 *     stores the data into the memory pointed at by data_ptr.
 *     Bytes are swapped from VMS to UNIX conventions depending upon
 *     the type of the item(s).
 *    
 *     WARNING: THIS ROUTINE ACCESSES THE FILE SEQUENTIALLY, AND, ALTHOUGH
 *     IT ALLOWS FOR IT TO BE READ IN PIECES, THE START_ITEM NUMBER SHOULD
 *     BE
 *     [USE A] THE LAST ITEM PREVIOUSLY READ + 1 (WITH THE FIRST READ 
 *             STARTING WITH ITEM 1)
 *     OR
 *     [USE B] THE REMAINING DATA OF THE CURRENT ITEM.
 *    
 *     Use A: read the data associated with the number of items
 *     --------------------------------------------------------
 *    
 *     The routine reads data associated with item 'start_item' through
 *     'start_item+noitems-1'. 'Start_Item' has to be a valid positive item 
 *     number.
 *    
 *     In order to prevent problems the routines returns the next item
 *     number to be read. Hence, it should be used as
 *    
 *             start_item = read_gsd_data_(...,start_item,...) 
 *                 ...
 *             start_item = read_gsd_data_(...,start_item,...) 
 *    
 *    
 *     Use B: read some data associated one particular item
 *     ----------------------------------------------------
 *    
 *     The routines reads 'noitems' bytes from the current item: '-start_item'.
 *     'Start_Item' must be a negative number(!) equal to -1 * item nr.
 *     The routine returns the number of bytes read and the user must keep 
 *     track of the total byte count, as well as making sure the number of 
 *     bytes specified does not cut a datavlue in two! 
 *     A Fortran example which assumes each scan to be 2048 double precision
 *     points and no knowledge about the total number of scans in the file 
 *     (unlikely, but alas).
 *    
 *            btotal= item.dsc(start_item).length
 *            bread = 0                
 *            bnext = 2048 * 8         
 *            do while (bread .lt. btotal)
 *              if ( (bread+bnext) .gt. btotal) then
 *                 bnext = btotal - bread
 *              endif
 *              bnew = read_gsd_data(...,-start_item, bnext)
 *              if (bnew .eq. -1) then
 *                write(*,*) 'FATAL: ....'
 *                call exit(-1)
 *              else
 *                bread = bread + bnew
 *                  -- do your stuff with scan data --
 *              endif
 *            enddo

 * Arguments:
 *    FILE *fptr (Given)
 *       C file descriptor.
 *    struct item_descriptor *item_ptr (Given)
 *       Array of item descriptors.
 *    char *data_ptr (Returned)
 *       The data. The buffer must be provided by the calling routine and large
 *       enough. Otherwise this routine will overwrite unrelated memory. In Use
 *       B the number of bytes read is simply noitems. In Use A the number of
 *       bytes read is the sum of the sizes in bytes of the items to be read.
 *       The calling routines must work out itself how much that might be and
 *       must provide sufficient buffer.
 *    int startitem (Given)
 *       First item number.
 *    int noitems (Given)
 *       Number of items to read

 * Returned value:
 *    int gsd1_rddata();
 *       The returned value is -1 if the fread failed, i.e. if not enough bytes
 *       were left in the file. Otherwise the returned value is positive. In
 *       Use A it is the next item to be read. In Use B it is the number of
 *       bytes read.

 * Authors:
 *    rpt: Remo Tilanus (JACH)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    04 Feb 1994 or 02 Apr 1994 (rpt):
 *       Original version.
 *    02 Dec 1994 (hme):
 *       Split off gsd.c.
 *       ANSI C. Change the two given integers to be passed by value.
 *    08 Dec 1994 (hme):
 *       Use gsd2_nativa routine.
 *-
 */

#include <stdio.h>
#include "gsd1.h"

/*:
 */

int gsd1_rddata( FILE *fptr, struct item_descriptor *item_ptr,
   char *data_ptr, int startitem, int noitems )
{
   struct item_descriptor *item_ptr2;
   enum type_tag d_type;

   int   i;
   int   start_item, no_items, start_byte, bytes, last_item;
   int   d_length;
   char *ch_ptr;

/*.
 */

   if ( startitem == 0 ) startitem = 1;          /* Just to make sure */

   if ( startitem  > 0 )                                     /* Use A */
   {  start_item = startitem;
      no_items = noitems;
      last_item = start_item + no_items - 1;
      item_ptr2 = item_ptr + start_item - 1;
      start_byte = item_ptr2->location;
      item_ptr2 = item_ptr2 + no_items - 1;
      bytes = item_ptr2->location + item_ptr2->length - start_byte;
   }
   else                                                      /* Use B */
   {  start_item = -1 * startitem;
      no_items = 1;
      item_ptr2 = item_ptr + start_item - 1;
      start_byte = item_ptr2->location;
      bytes = noitems;
   }

/* Read in the whole bytes block                                    
 */ 
   if ( fread( data_ptr, sizeof(char), bytes, fptr ) != bytes ) return -1;

/* Swap byte order from VAX to Local Host order for all data according to 
 * datatype of item.
 */
   item_ptr2 = item_ptr + start_item - 1;;
   for ( i = 0; i < no_items; i++, item_ptr2++ )
   {  ch_ptr = ( data_ptr + item_ptr2->location - start_byte );
      d_type = (enum type_tag) item_ptr2->data_type;
      if ( startitem > 0 ) d_length = item_ptr2->length;    /* Use A */
      else                 d_length = bytes;                /* Use B */
      (void) gsd2_nativa( ch_ptr, d_type, d_length );
   }

   if ( startitem > 0 ) return ( last_item + 1 );           /* Use A */
   else                 return bytes;                       /* Use B */
}
