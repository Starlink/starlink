/*+
 *  Name:
 *     emsMark

 *  Purpose:
 *     Start a new error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsMark( )

 *  Description:
 *     This function provides a C interface for the Error Message
 *     Service routine EMS_MARK (written in Fortran).

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
 *     RTP: R.T.Platon (STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     9-AUG-1990 (PCTR):
 *        C function code.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_mark_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_MARK
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitons: */
void emsMark( void ){

   TRACE ( "emsMark" );

/*  Get a new error message context. */
   ems1Emark();

/*  Get a new message token context. */
   ems1Mpush();

   return;
}
