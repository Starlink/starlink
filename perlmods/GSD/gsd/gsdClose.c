/*+
 * Name:
 *    gsdClose

 * Purpose:
 *    Close a GSD file.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd.h")
 *    int gsdClose( FILE *fptr, void *file_dsc, void *item_dsc,
 *       char *data_ptr );

 * Description:
 *    This routine closes a GSD file opened previously with gsdOpenRead. It
 *    also releases the memory that gsdOpenRead allocated in connection to that
 *    file. For this purpose this routine must be given the standard C file
 *    pointer, the pointer to the GSD file descriptor, the pointer to the GSD
 *    item descriptors, and the pointer to the data buffer.

 * Arguments:
 *    FILE *fptr (Given)
 *       The file descriptor for the GSD file to be closed.
 *    void *file_dsc (Given)
 *       The GSD file descriptor related to the file opened on fptr.
 *    void *item_dsc (Given)
 *       The array of GSD item descriptors related to the file opened on fptr.
 *    char *data_ptr (Given)
 *       The buffer with all the data from the GSD file opened on fptr.

 * Returned Value:
 *    int gsdClose();
 *       Status from fclose.

 * Authors:
 *    jhf: Jon Fairclough (UKTH)
 *    rp: Rachael Padman (MRAO)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Sep 1986 (jhf):
 *       Original.
 *    29 Oct 1987 (jhf):
 *       Set FILE__FREE properly!
 *    17 Jul 1994 (rp):
 *       Adaption to Remo's C code.
 *    30 Nov 1994 (hme):
 *       Translation to C. Renamed from GSD_CLOSE. Interface revised.
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include "gsd1.h"
#include "gsd.h"

/*:
 */

int gsdClose( FILE *fptr, void *file_dsc, void *item_dsc, char *data_ptr )
{
   int status;

   status = 0;
   if ( fptr     ) status = fclose( fptr );
   if ( file_dsc ) (void) free( file_dsc );
   if ( item_dsc ) (void) free( item_dsc );
   if ( data_ptr ) (void) free( data_ptr );
   return status;
}
