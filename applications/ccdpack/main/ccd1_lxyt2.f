      SUBROUTINE CCD1_LXYT2( XIN, YIN, NIN, TR, XOUT, YOUT, STATUS )
*+
*  Name:
*     CCD1_LXYT2

*  Purpose:
*     Transforms the X and Y positions using a linear transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LXYT2( XIN, YIN, NIN, TR, XOUT, YOUT, STATUS )

*  Description:
*     The routine transforms the X and Y positions using a linear
*     transformation whose coefficients are specified by the TR array.
*     The transformed positions are returned in the XOUT and YOUT
*     arrays.

*  Arguments:
*     XIN( NIN ) = DOUBLE PRECISION (Given)
*        Input X positions.
*     YIN( NIN ) = DOUBLE PRECISION (Given)
*        Input Y positions.
*     NIN = INTEGER (Given)
*        The number of input values.
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The transformation coefficients. The transformation is:
*
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*     XOUT( NIN ) = DOUBLE PRECISION (Given)
*        Output X positions.
*     YOUT( NIN ) = DOUBLE PRECISION (Given)
*        Output Y positions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The linear transform used by this routines is:
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y

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
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     27-JUL-1992 (PDRAPER):
*        Changed to use separated X and Y positions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NIN
      DOUBLE PRECISION XIN( NIN )
      DOUBLE PRECISION YIN( NIN )
      DOUBLE PRECISION TR( 6 )

*  Arguments Returned:
      DOUBLE PRECISION XOUT( NIN )
      DOUBLE PRECISION YOUT( NIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the positions and copy the extra values into ARROUT.
      DO 1 I = 1, NIN
         XOUT( I ) = TR( 1 ) + TR( 2 ) * XIN( I ) + TR( 3 ) * YIN( I )
         YOUT( I ) = TR( 4 ) + TR( 5 ) * XIN( I ) + TR( 6 ) * YIN( I )
 1    CONTINUE

      END
* $Id$
