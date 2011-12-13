/*+
 *  Name:
 *     emsSetnc

 *  Purpose:
 *     Assign a CHARACTER value to a message token (concise).

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsSetnc( token, cvalue, maxchar )

 *  Description:
 *     This function sets the specified message token to the given string
 *     value, leading spaces are retained but trailing spaces removed.
 *     This is similar to emsSetc but should be used if the given string
 *     is not null-terminated, as a maximum length for the token is specified.
 *     In this respect it is a replacement for the old ems_setc_c interface
 *     to the Fortran EMS. References to ems_setc_c will be translated to
 *     references to this function by a definition in ems.h
 *
 *     A null or blank string will be rendered as a token of one space and if
 *     cvalue is not null-terminated earlier, it will be truncated at maxchar
 *     or EMS__SZTOK characters, whichever is less.
 *
 *     There is no Fortran interface to emsSetnc.

 *  Arguments:
 *     token = const char * (Given)
 *        The message token name.
 *     cvalue = const char * (Given)
 *        The CHARACTER value to be assigned to the message token.
 *     maxchar = int (Given)
 *        The maximum desired length of cvalue.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     10-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_setc_c
 *     20-SEP--2001 (AJC):
 *        Rewritten to avoid calling Fortran
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
void emsSetnc( const char *token, const char *cvalue, int maxchar ){

   int vallen;
   int i;
   char valbuf[ EMS__SZTOK + 1 ];

   TRACE("emsSetnc");
   DEBUG("emsSetnc", "emsSetnc: '%s'", token);

/*  Find the used length of the string */
   vallen = maxchar > EMS__SZTOK ? EMS__SZTOK : maxchar;

   strncpy( valbuf, cvalue, vallen );
   valbuf[vallen] = '\0';

   for ( i=strlen(valbuf); i>0 ; i-- ) {
      if ( valbuf[ i-1 ] != ' ' ) break;
   }
   valbuf[i] = '\0';

/*  Ensure minimum one space */
   if ( !i ) {
      strcpy( valbuf, " " );
   }

/*  Set the token value. */
   ems1Stok( token, valbuf );

   return;

}
