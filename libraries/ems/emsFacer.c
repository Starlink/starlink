/*+
 *  Name:
 *     emsFacer

 *  Purpose:
 *     Assign a facility error message to a token.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsFacer( token, status )

 *  Description:
 *     This function queires the Error Message Service database
 *     to associate an error message with a supplied status value.

 *  Arguments:
 *     token = const char * (Given)
 *        The message token to be associated with the error text.
 *     status = int (Given)
 *        The status value.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     Copyright (C) 1994 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
 *     All Rights Reserved.

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
 *     AJC: A.J.Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     14-SEP-1994 (AJC):
 *        Original version, based on ems_syser_c
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_facer_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_FACER
 *      6-MAR-2001 (AJC):
 *        Use FSTAT not LSTAT in sprintf
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     15-SEP-2008 (TIMJ):
 *        3 arg emsSetc is deprecated.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include <string.h>                    /* String handling library functions */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitons: */
void emsFacer( const char *token, int fstat ){

/* Type Definitions: */
   int meslen;              /* Status message length */

   char mesval[EMS__SZMSG];            /* Status message string */

   TRACE("emsFacer");

/*  Initialise MESVAL.
 */
   strcpy(mesval, "");

/*  Load message string associated with the given facility status value
 */
   ems1Fcerr( mesval, &fstat );

/*  Check the success of the call.
 */
   meslen = strlen( mesval );

   if ( meslen == 0 ) {

/*     No message string could be found for this status value, so
 *     return the integer status value.
 */
      (void) sprintf( mesval, "Failed get facility error for value %d", fstat );
      meslen = strlen( mesval );
   }

/*  A message has been returned.
 */
   emsSetc( token, mesval );

   return;
}
