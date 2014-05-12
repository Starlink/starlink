/*
*  Name:
*     c2f77.c

*  Purpose:
*     Implement the interface between the C and FORTRAN 77 languages.

*  Description:
*     This file implements language-specific functions which support
*     the FORTRAN 77 interface to the AST library.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Notes:
*     - Some of the functions in this module are potentially platform
*     dependent and may need to be re-implemented when porting the AST
*     library to new platforms.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: D.S. Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     15-NOV-1996 (RFWS):
*        Original version (based on work by DSB and on code from the
*        Starlink CNF library).
*/

/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to protected interfaces. */
#define astCLASS

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "c2f77.h"               /* Interface to this module */

/* Function implementations. */
/* ========================= */
void astStringExport_( const char *source_c, char *dest_f, int dest_len ) {
/*
*+
*  Name:
*     astStringExport

*  Purpose:
*     Export a C string to a FORTRAN string.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "c2f77.h"
*     void astStringExport( const char *source_c, char *dest_f, int dest_len )

*  Description:
*     This function creates a FORTRAN string from a C string, storing
*     it in the supplied memory. If the C string is shorter than the
*     space allocated for the FORTRAN string, then it is padded with
*     blanks. If the C string is longer than the space allocated for
*     the FORTRAN string, then the string is truncated.

*  Parameters:
*     source_c
*        A pointer to the input C string.
*     dest_f
*        A pointer to the output FORTRAN string.
*     dest_len
*        The length of the output FORTRAN string.

*  Notes:
*     - This function is potentially platform-specific. For example,
*     if FORTRAN strings were passed by descriptor, then the
*     descriptor address would be passed as "dest_f" and this must
*     then be used to locate the actual FORTRAN character data.
*     - This function is described as protected but is in fact
*     available through the public interface so that it may be used in
*     constructing the FORTRAN 77 public interface.
*     - This is the UNIX version of this function.
*-
*/

/* Local Variables:*/
   int i;                        /* Loop counter for characters */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global error status. */
   if ( !astOK ) return;

/* Copy the characters of the input C string to the output FORTRAN
   string, taking care not to go beyond the end of the FORTRAN
   string.*/
   for ( i = 0; source_c[ i ] && ( i < dest_len ); i++ ) {
      dest_f[ i ] = source_c[ i ];
   }

/* Fill the rest of the output FORTRAN string with blanks. */
   for ( ; i < dest_len; i++ ) dest_f[ i ] = ' ';
}
