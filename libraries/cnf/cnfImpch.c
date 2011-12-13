#include <string.h>
#include "f77.h"                 /* CNF macros and prototypes               */
#if HAVE_CONFIG_H
#  include <config.h>
#endif

void cnfImpch( const char *source_f, int nchars, char *dest_c )

/*
*+
*  Name:
*     cnfImpch

*  Purpose:
*     Import a FORTRAN string into a C array of char.

*  Language:
*     ANSI C

*  Invocation:
*     cnfImpch( source_f, nchars, dest_c )

*  Description:
*     Import a FORTRAN string into a C array of char, copying `nchars'
*     characters.
*     No characters, are special so this may be used to import an HDS locator
*     which could contain any character.

*  Arguments:
*     const char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int nchars (Given)
*        The number of characters to be copied from source_f to
*        dest_c
*     char *dest_c (Returned via pointer)
*        A pointer to the C array of char.

*  Notes:
*     No check is made that there is sufficient space allocated to
*     the C array to hold the FORTRAN string.
*     It is the responsibility of the programmer to check this.

*  Copyright:
*     Copyright (C) 1996 Council for the Central Laboratory of the Research
*     Councils

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

*  Authors:
*     AJC: A.J. Chipperfield (Starlink, RAL)
*     PWD: Peter Draper (Starlink, University of Durham)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-JUN-1996 (AJC):
*        Original version.
*     16-Aug-1996 (AJC):
*        Correct include file
*     24-SEP-1998 (AJC):
*        Specify const char * for input strings
*     19-JUL-2004 (PWD):
*        Changed to use memmove when bcopy isn't available. Also to
*        just do the straight copy when neither is available (very unlikely).
*        Note that bcopy is deprecated in POSIX.
*     19-SEP-2005 (TIMJ):
*        Prefer memmove over bcopy.
*        Remove double copy (bcopy was always called at least once).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

#if !HAVE_MEMMOVE && !HAVE_BCOPY
   int i;			 /* Loop counter			    */
#endif

/* Copy the characters of the input C array to the output FORTRAN string.  */

#if HAVE_MEMMOVE
   memmove( (void *)dest_c, (const void *)source_f, (size_t)nchars );
#elif HAVE_BCOPY
   bcopy( (const void *)source_f, (void *)dest_c, (size_t)nchars );
#else
/* Do this by hand, note no overlaps allowed */
   for ( i = 0; i < nchars; i++ ) {
       dest_c[i] = source_f[i];
   }
#endif


}
