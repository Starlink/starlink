/*
*+
* Name:
*    gsdClose

* Purpose:
*    Close a GSD file.

* Language:
*    ANSI C

* Type of Module:
*    C function.

* Invocation:
*    int gsdClose( FILE *fptr, GSDFileDesc *file_dsc, GSDItemDesc *item_dsc,
*       char *data_ptr );

* Prototype:
*    available via #include "gsd.h"
 *    int gsdClose( FILE *fptr, GSDFileDesc *file_dsc, GSDItemDesc *item_dsc,
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
*    GSDFileDesc *file_dsc (Given)
*       The GSD file descriptor related to the file opened on fptr.
*    GSDItemDesc *item_dsc (Given)
*       The array of GSD item descriptors related to the file opened on fptr.
*    char *data_ptr (Given)
*       The buffer with all the data from the GSD file opened on fptr.

* Returned Value:
*    int gsdClose();
*       Status from fclose.

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
*    ror: Russell Redman (HIA)

* History:
*    08 Sep 1986 (jhf):
*       Original.
*    29 Oct 1987 (jhf):
*       Set FILE__FREE properly!
*    17 Jul 1994 (rp):
*       Adaption to Remo's C code.
*    30 Nov 1994 (hme):
*       Translation to C. Renamed from GSD_CLOSE. Interface revised.
*    04 Jul 2008 (timj):
*       use proper GSD structs rather than void
*    06 Oct 2008 (ror):
*       Support reading from stdin.


* Copyright:
*    Copyright (C) 2008 Science and Technology Facilities Council.
*    Copyright (C) 1986-1999 Particle Physics and Astronomy Research Council.
*    All Rights Reserved.

*-
*/

#include <stdio.h>
#include <stdlib.h>
#include "gsd1.h"
#include "gsd.h"

/*:
 */

int gsdClose( FILE *fptr, GSDFileDesc *file_dsc, GSDItemDesc *item_dsc,
              char *data_ptr )
{
   int status;

   status = 0;
   if ( fptr     ) {
     if (fptr == stdin) {
       if (ferror(stdin)) clearerr(stdin);
     } else {
       status = fclose( fptr );
     }
   }
   if ( file_dsc ) (void) free( file_dsc );
   if ( item_dsc ) (void) free( item_dsc );
   if ( data_ptr ) (void) free( data_ptr );
   return status;
}
