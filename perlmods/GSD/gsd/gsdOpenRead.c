/*+
 * Name:
 *    gsdOpenRead

 * Purpose:
 *    Open a GSD file for reading and map it.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd.h")
 *    int gsdOpenRead( char *file, float *version, char *label, int *no_items,
 *       FILE **fptr, void **file_dsc, void **item_dsc, char **data_ptr );

 * Description:
 *    This routine opens the named GSD file and reads its contents into memory.
 *    It returns a standard C file descriptor, a GSD file descriptor, a pointer
 *    to the array of GSD item descriptors, and a pointer to the collective
 *    data.
 *
 *    This routine allocates memory to accommodate the GSD file descriptor, the
 *    GSD item descriptors, and the data from the GSD file. It also leaves the
 *    GSD file open. Any call to this routine must be matched with a call to
 *    gsdClose with the information returned by this routine. gsdClose will
 *    close the file and release the memory allocated by this routine.

 * Arguments:
 *    char *file (Given)
 *       The name of the GSD file to be opened.
 *    float *version (Returned)
 *       The GSD file version number.
 *    char *label (Returned)
 *       The GSD file label. This is a null-terminated string. It should be
 *       declared by the calling routine with length 41.
 *    int *no_items (Returned)
 *       The number of items in the GSD file.
 *    FILE **fptr (Returned)
 *       The file descriptor for the GSD file opened.
 *    void **file_dsc (Returned)
 *       The GSD file descriptor. This routine allocates the memory necessary
 *       and fills it with the relevant information from the GSD file. A call
 *       to gsdClose will release this memory (given the pointer).
 *    void **item_dsc (Returned)
 *       The array of GSD item descriptors. This routine allocates the memory
 *       necessary and fills it with the relevant information from the GSD
 *       file. A call to gsdClose will release this memory (given the pointer).
 *       The number of array elements is returned in no_items.
 *    char **data_ptr (Returned)
 *       The buffer with all the data from the GSD file. This routine allocates
 *       the memory necessary and reads the data into it. A call to gsdClose
 *       will release this memory (given the pointer). The size of this buffer
 *       does not matter, but it can be calculated in bytes as
 *          file_dsc->end_data - file_dsc->str_data + 1
 *       if you know what a struct file_descriptor looks like.

 * Returned Value:
 *    int gsdOpenRead();
 *       Status. Status is set to
 *        1: Failure to open named file,
 *        2: Failure to read file_dsc from file,
 *        3: Failure to allocate memory for item_dsc,
 *        4: Failure to read item_dsc from file,
 *        6: Failure to read data_ptr from file,
 *        7: Failure to allocate memory for data_ptr,
 *        0: Otherwise.

 * Authors:
 *    jhf: Jon Fairclough (UKTH)
 *    rp: Rachael Padman (MRAO)
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    rpt: Remo Tilanus (JAC, Hilo)
 *    sec: Steve Cockayne (JAC, Hilo)

 * History:
 *    08 Sep 1986 (jhf):
 *       Original.
 *    18 May 1987 (jhf):
 *       Extra OPEN keywords.
 *    03 Jun 1987 (jhf):
 *       Make global section name the same as the filename.
 *    06 Jul 1987 (jhf):
 *       Check status RMS returns.
 *    14 Jan 1988 (jhf):
 *       Improve modularity for OPEN routines.
 *    17 Jul 1994 (rp):
 *       Adaption to Remo's C code.
 *    02 Dec 1994 (hme):
 *       Translation to C. Renamed from GSD_OPEN_READ. Interface revised.
 *    01 Aug 1995 (rpt):
 *       Support for DATADIR variable and upper/lower case file suffix
 *    26 Jul 1996 (sec):
 *       Support for the actual given filename. 
 *-
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gsd1.h"
#include "gsd.h"

/*:
 */

int gsdOpenRead( char *file, float *version, char *label, int *nitem,
   FILE **fptr, void **file_dsc_arg, void **item_dsc_arg, char **data_ptr )
{
   struct file_descriptor **file_dsc;
   struct item_descriptor **item_dsc;
   size_t nbytes;

   int status;
   int ier, inr, no_items, start_item, start_byte, bytes;

   char dfile[128], *datadir;
/*.
 */

/* Reset return values.
 * The returned generic pointers are by default NULL.
 */
   status = 0;
   *fptr = *file_dsc_arg = *item_dsc_arg = *data_ptr = NULL;

/* Get environment variable DATADIR
 */
   datadir = getenv( "DATADIR" );

/* Open the GSD file for read access. Check first the actual filename, 
 * then upper and lower case suffix and DATADIR
 */
   sprintf( dfile, "%s", file );
   *fptr = fopen( dfile, "r" );
   if ( !*fptr ) { 
     sprintf( dfile, "%s.dat", file );
     *fptr = fopen( dfile, "r" );
     if ( !*fptr ) { 
       sprintf( dfile, "%s.DAT", file );
       *fptr = fopen( dfile, "r" );
       if ( !*fptr ) { 
         if ( datadir != NULL ) {
  	   sprintf( dfile, "%s/%s.dat", datadir, file );
  	   *fptr = fopen( dfile, "r" );
  	   if ( !*fptr ) { 
  	     sprintf( dfile, "%s/%s.DAT", datadir, file );
  	     *fptr = fopen( dfile, "r" );
  	   }
         }
       }
     }
   }

   if ( !*fptr ) { status = 1; goto abort; }

/* Read the GSD file into memory.
 */
   *file_dsc_arg = malloc( sizeof( struct file_descriptor ) );
   if ( !*file_dsc_arg ) { status = 2; goto abort; }
   file_dsc = (struct file_descriptor **) file_dsc_arg;
   ier = gsd1_rdfildsc( *fptr, *file_dsc );
   if ( ier ) { status = 2; goto abort; }

/* Get the memory for the items and the data.
 */
   nbytes = sizeof( struct item_descriptor ); nbytes *= (**file_dsc).no_items;
   *item_dsc_arg = malloc( nbytes );
   if ( !*item_dsc_arg ) { status = 3; goto abort; }
   item_dsc = (struct item_descriptor **) item_dsc_arg;
   nbytes = (size_t) ( (**file_dsc).end_data - (**file_dsc).str_data + 1 );
   *data_ptr = (char *) malloc( nbytes );
   if ( !*data_ptr ) { status = 7; goto abort; }

/* Read the GSD header into memory.
 */
   ier = gsd1_rdhead( *fptr, *file_dsc, *item_dsc );
   if ( ier ) { status = 4; goto abort; }

/* Read the GSD data into memory.
 */
   start_item = 1;
   no_items = (**file_dsc).no_items;
   inr = start_item;
   start_byte = (*item_dsc+inr-1)->location;
   inr += no_items - 1;
   nbytes = (*item_dsc+inr-1)->location + (*item_dsc+inr-1)->length
          - start_byte;
   start_item = gsd1_rddata( *fptr, *item_dsc, *data_ptr,
      start_item, no_items );
   if ( start_item <= 0 ) { status = 6; goto abort; }

/* Set returned values.
 */
   *version = (**file_dsc).version;
   *nitem   = no_items;
   (void) memcpy( label, (**file_dsc).comment, 40 ); label[40] = '\0';

/* Return.
 */
   abort:
   if ( status && *fptr         ) (void) fclose( *fptr );
   if ( status && *file_dsc_arg ) (void) free( *file_dsc_arg );
   if ( status && *item_dsc_arg ) (void) free( *item_dsc_arg );
   if ( status && *data_ptr     ) (void) free( *data_ptr );
   return status;
}
