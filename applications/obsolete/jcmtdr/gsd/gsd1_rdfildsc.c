/*+
 * Name:
 *    gsd1_rdfildsc

 * Purpose:
 *    Read GSD file descriptor.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    int gsd1_rdfildsc( FILE *fptr, struct file_descriptor *file_dsc );

 * Description:
 *    Routine to read file descriptor from GSD file and store result
 *    in file descriptor structure.

 * Arguments:
 *    FILE *fptr (Given)
 *       C file descriptor.
 *    struct file_descriptor *file_dsc (Returned)
 *       GSD header file descriptor.

 * Returned value:
 *    int gsd_rdfildsc();
 *       -1 if the fread failed, i.e. if there were not enough bytes left in
 *       the file. Zero otherwise. A return value of zero does not guarantee
 *       that anything useful was read from the file, say whether it is a GSD
 *       file at all.

 * Implementation Status:
 *    This routine looks at how many bytes this C implementation needs for the
 *    structure that is a GSD file descriptor. It then reads that many bytes in
 *    one chunk with fread. Unless fread is very clever, problems may arise
 *    when the struct contains padding bytes. There will also be a problem if
 *    in the file there are padding bytes that this C implementation would not
 *    want in the struct.
 *
 *    This could be fixed by reading each struct member with its own fread.

 * Authors:
 *    rpt: Remo Tilanus (JACH)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    04 Feb 1994 or 02 Apr 1994 (rpt):
 *       Original version.
 *    02 Dec 1994 (hme):
 *       Split off gsd.c. ANSI C. Do not null-terminate string within
 *       structure.
 *    08 Dec 1994 (hme):
 *       Use gsd2_nativx routines.
 *-
 */

#include <stdio.h>
#include "gsd1.h"

/*:
 */

int gsd1_rdfildsc( FILE *fptr, struct file_descriptor *file_dsc )
{
   int size_of_fdsc;

/*.
 */

   size_of_fdsc = (int) sizeof( struct file_descriptor );

   if ( fread( file_dsc, size_of_fdsc, 1, fptr) != 1 ) return -1;

   (void) gsd2_nativr( (unsigned char *) &file_dsc->version      );
   (void) gsd2_nativi( (unsigned char *) &file_dsc->max_no_items );
   (void) gsd2_nativi( (unsigned char *) &file_dsc->no_items     );
   (void) gsd2_nativi( (unsigned char *) &file_dsc->str_data     );
   (void) gsd2_nativi( (unsigned char *) &file_dsc->end_data     );
   (void) gsd2_nativi( (unsigned char *) &file_dsc->size         );

   return 0;
}
