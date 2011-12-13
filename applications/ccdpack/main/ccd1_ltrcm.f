      SUBROUTINE CCD1_LTRCM( TA, TB, TOUT, STATUS )
*+
*  Name:
*     CCD1_LTRCM

*  Purpose:
*     Combine two linear coordinate transformations into one

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL CCD1_LTRCM( TA, TB, TOUT, STATUS )

*  Description:
*     Forms a set of linear transformation coefficients TOUT which are
*     equivalent to applying the transformation represented by
*     the coefficients TA and TB consecutively.

*  Arguments:
*     TA( 6 ) = DOUBLE PRECISION (Given)
*        The first set of transformation coefficients
*     TB( 6 ) = DOUBLE PRECISION  (Given)
*        The second set of transformation coefficients
*     TOUT( 6 ) = REAL (Returned)
*        The output transformation coefficients.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DUVAD::RFWS: R.F.Warren-Smith (Durham Polarimetry Group)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     (DUVAD::RFWS):
*        Original version.
*     26-JUN-1991 (DUVAD::TMG):
*        change prologue to STARLINK format
*     27-JUL-1992 (PDRAPER):
*        Changed to use STATUS and DOUBLE PRECISION.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION TA( 6 )
      DOUBLE PRECISION TB( 6 )

*  Arguments Returned:
      DOUBLE PRECISION TOUT( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Combine the transformations.
      TOUT( 1 ) = TB( 1 ) + TB( 2 ) * TA( 1 ) + TB( 3 ) * TA( 4 )
      TOUT( 2 ) = TA( 2 ) * TB( 2 ) + TB( 3 ) * TA( 5 )
      TOUT( 3 ) = TB( 2 ) * TA( 3 ) + TB( 3 ) * TA( 6 )
      TOUT( 4 ) = TB( 4 ) + TB( 5 ) * TA( 1 ) + TB( 6 ) * TA( 4 )
      TOUT( 5 ) = TB( 5 ) * TA( 2 ) + TB( 6 ) * TA( 5 )
      TOUT( 6 ) = TB( 5 ) * TA( 3 ) + TB( 6 ) * TA( 6 )

      END
* $Id$
