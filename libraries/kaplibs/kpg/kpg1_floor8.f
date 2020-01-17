      INTEGER*8 FUNCTION KPG1_FLOOR8( VALUE )
*+
*  Name:
*     KPG1_FLOOR8

*  Purpose:
*     Returns the largest integer*8 smaller than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_FLOOR8( VALUE )

*  Description:
*     This routine returns the largest integer smaller than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = REAL (Given)
*        The value.

*  Function Value:
*     KPG1_FLOOR8 = INTEGER*8
*        The largest integer smaller than or equal to the supplied value.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-2020 (DSB):
*        Original version, based on KPG1_FLOOR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      REAL VALUE

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'    ! Definitions of conversion routines

*  Do it.

      IF( VALUE .GT. NUM_KTOR( VAL__MAXK ) ) THEN
         KPG1_FLOOR8 = VAL__MAXK

      ELSE IF( VALUE .LT. NUM_KTOR( VAL__MINK ) ) THEN
         KPG1_FLOOR8 = VAL__MINK

      ELSE
         KPG1_FLOOR8 = NUM_RTOK( VALUE )
         IF( NUM_KTOR( KPG1_FLOOR8 ) .GT. VALUE ) THEN
            KPG1_FLOOR8 = KPG1_FLOOR8 - 1
         END IF
      END IF

      END

