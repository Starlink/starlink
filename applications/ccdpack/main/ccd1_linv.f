      SUBROUTINE CCD1_LINV( TR, TRINV, STATUS )
*+
*  Name:
*     CCD1_LINV

*  Purpose:
*     Finds the inverse of a linear transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LINV( TR, TRINV, STATUS )

*  Description:
*     This routine determines the inverse transformation of
*     a 6 parameter linear fit of the type.
*
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*
*     The inverse is simply.
*
*        X = TRINV( 1 ) + TRINV( 2 ) * XDASH + TRINV( 3 ) * YDASH
*        X = TRINV( 4 ) + TRINV( 5 ) * XDASH + TRINV( 6 ) * YDASH
*
*     Where
*
*        TRINV( 1 ) = ( TR( 4 ) * TR( 3 ) - TR( 1 ) * TR( 6 ) ) / DET
*        TRINV( 2 ) = TR( 6 ) / DET
*        TRINV( 3 ) = -TR( 3 ) / DET
*        TRINV( 4 ) = ( TR( 1 ) * TR( 5 ) - TR( 4 ) * TR( 2 ) ) / DET
*        TRINV( 5 ) = -TR( 5 ) / DET
*        TRINV( 6 ) = TR( 2 ) / DET
*     and
*         DET = TR( 2 ) * TR( 6 ) - TR( 3 ) * TR( 5 )

*  Arguments:
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The transformation whose inverse is required.
*     TRINV( 6 ) = DOUBLE PRECISION (Returned)
*        The inverse transformation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the determinant of the input transformation then
*     an error will be reported.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-JUL-1992 (PDRAPER):
*        Original version based on EDRS version by R.F. Warren-Smith
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION TR( 6 )

*  Arguments Returned:
      DOUBLE PRECISION TRINV( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DET

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Form determinant and determine if transformation is singular.
      DET = TR( 2 ) * TR( 6 ) - TR( 3 ) * TR( 5 )
      IF ( DET .EQ. 0.0D0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_LINV',
     :   '  CCD1_LINV: Transformation is singular and cannot be '//
     :   'inverted', STATUS )
      ELSE

*  If not, calculate the inverse.
         TRINV( 1 ) = ( TR( 4 ) * TR( 3 ) - TR( 1 ) * TR( 6 ) ) / DET
         TRINV( 2 ) = TR( 6 ) / DET
         TRINV( 3 ) = -TR( 3 ) / DET
         TRINV( 4 ) = ( TR( 1 ) * TR( 5 ) - TR( 4 ) * TR( 2 ) ) / DET
         TRINV( 5 ) = -TR( 5 ) / DET
         TRINV( 6 ) = TR( 2) / DET
      END IF
      END
* $Id$
