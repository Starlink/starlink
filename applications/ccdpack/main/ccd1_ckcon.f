      SUBROUTINE CCD1_CKCON( FOR, INV, NVAL, STATUS )
*+
*  Name:
*     CCD1_CKCON

*  Purpose:
*     Checks fitted transformation values for conditioning.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CKCON( FOR, INV, NVAL, STATUS )

*  Description:
*     This routine checks the values returned by independently
*     fitting a forward and a backward mapping for near equality.
*     The values are checked to lie within a few times the machine
*     precision. If they lie outside of this bounds a warning about
*     ill-conditioning is issued through the CCDPACK logging/message
*     system. No BAD status results from this routine.

*  Arguments:
*     FOR( NVAL ) = DOUBLE PRECISION (Given)
*        The parameter values which have been fitted in the forward
*        mapping.
*     INV( NVAL ) = DOUBLE PRECISION (Given)
*        The parameter values which have been fitted in the inverse
*        mapping.
*     NVAL = INTEGER (Given)
*        The number of values in the input arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     6-OCT-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NVAL
      DOUBLE PRECISION FOR( NVAL )
      DOUBLE PRECISION INV( NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION RANGE     ! Range of mismatch
      INTEGER I                  ! Loop variable
      INTEGER MISS               ! Number of substantially different
                                 ! values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the range of mismatch at 3 time the machine precision for double
*  precision.
      RANGE = 3.0D0 * VAL__EPSD

*  Loop over all values counting the number of mismatches.
      MISS = 0
      DO 1 I = 1, NVAL
         IF ( FOR( I ) .LT. INV( I ) - RANGE .OR.
     :        FOR( I ) .GT. INV( I ) + RANGE ) THEN
            MISS = MISS + 1
         END IF
 1    CONTINUE

*  Issue warning if necessary.
      IF ( MISS .GT. 0 ) THEN
         IF ( MISS .EQ. 1 ) THEN
            CALL CCD1_MSG( ' ',
     : '  Warning - the fit to the forward and inverse'//
     : ' transformations have given different parameter values.'//
     : ' Equations may be ill-conditioned.', STATUS )
         ELSE
            CALL MSG_SETI( 'MISS', MISS )
            CALL CCD1_MSG( ' ',
     : '  Warning - the fit to the forward and inverse'//
     : ' transformations have given ^MISS different parameter values.'//
     : ' Equations may be ill-conditioned.', STATUS )
         END IF
      END IF
      END
* $Id$
