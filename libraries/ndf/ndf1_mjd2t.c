#include <stdio.h>

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "f77.h"		 /* Fortran <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */
#include "ast.h"		 /* AST functions and constants 	    */

   F77_SUBROUTINE(ndf1_mjd2t)( DOUBLE(MJD),
                               INTEGER_ARRAY(YMDHM),
                               REAL(SEC),
			       INTEGER(STATUS) )
   {
/*
*+
*  Name:
*     NDF1_MJD2T

*  Purpose:
*     Convert an Modified Julian Date to separate date and time fields.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_MJD2T( MJD, YMDHM, SEC, STATUS )

*  Description:
*     The routine converts the supplied MJD into separate year, month,
*     day, hour, minute and second values.

*  Arguments:
*     MJD = DOUBLE PRECISION (Given)
*        The Modified Julian Date to convert.
*     YMDHM( 5 ) = INTEGER (Returned)
*        The full year, month, day, hour and minute values (in that
*        order), stored as integers.
*     SEC = REAL (Returned)
*        The seconds value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The year value is given in full (i.e. 1993, not simply 93).
*     -  The month value starts at 1 for January.
*     -  The day value is the day of the month, starting at 1.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2009 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given:							    */
      GENPTR_DOUBLE(MJD)

/* Arguments Returned:							    */
      GENPTR_INTEGER_ARRAY(YMDHM)
      GENPTR_REAL(SEC)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* Local Variables:							    */
      AstTimeFrame *tf;
      const char *iso;
/*.									    */

/* Check inherited status.						    */
      if ( *STATUS != SAI__OK ) return;

/* Format the MJD as an ISO date string with 3 decimal places in the
   seconds field. Use an AST TimeFrame to do this.   */
      tf = astTimeFrame( "Format=iso.3,System=MJD" );
      iso = astFormat( tf, 1, *MJD );

/* If succesful, parse the string to extract the fields. */
      if( astOK ) {
         sscanf( iso, "%d-%d-%d %d:%d:%f", YMDHM, YMDHM + 1, YMDHM + 2,
                 YMDHM + 3, YMDHM + 4, SEC );
      }

/* Annul the TimeFrame. */
      tf = astAnnul( tf );

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "NDF1_MJD2T", STATUS );

/* Exit the routine.							    */
      return;
   }
