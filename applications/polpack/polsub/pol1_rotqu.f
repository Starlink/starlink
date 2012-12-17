      SUBROUTINE POL1_ROTQU( EL, ANGLE, VAR, QIN, UIN, QOUT, UOUT,
     :                       STATUS )
*+
*  Name:
*     POL1_ROTQU

*  Purpose:
*     Get the rotation needed to align two reference directions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_ROTQU( EL, ANGLE, VAR, QIN, UIN, QOUT, UOUT, STATUS )

*  Description:
*     The routine creates new Q and U values by rotating the reference
*     direction by a given angle.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the arrays.
*     ANGLE = REAL (Given)
*        The clockwise angle in degrees by which to rotate the reference
*        direction within the GRID Frame.
*     VAR = _LOGICAL (Given)
*        If .TRUE., then the supplied arrays hold variance values.
*     QIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of Q values.
*     UIN( EL ) = DOUBLE PRECISION (Given)
*        The supplied array of U values.
*     QOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of Q values.
*     UOUT( EL ) = DOUBLE PRECISION (Returned)
*        The Returned array of U values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER EL
      REAL ANGLE
      LOGICAL VAR
      DOUBLE PRECISION QIN( EL )
      DOUBLE PRECISION UIN( EL )

*  Arguments Retiurned:
      DOUBLE PRECISION QOUT( EL )
      DOUBLE PRECISION UOUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COS2A
      DOUBLE PRECISION SIN2A
      DOUBLE PRECISION QVAL
      DOUBLE PRECISION UVAL
      INTEGER I
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the trig terms in advance.
      COS2A = COS( 2*ANGLE*AST__DD2R )
      SIN2A = SIN( 2*ANGLE*AST__DD2R )

*  Loop round all points.
      DO I = 1, EL
         QVAL = QIN( I )
         UVAL = UIN( I )

*  If the values are good, calculate the rotated vector.
         IF( QVAL .NE. VAL__BADD .AND. UVAL .NE. VAL__BADD ) THEN
            QOUT( I ) = QVAL*COS2A - UVAL*SIN2A
            UOUT( I ) = UVAL*COS2A + QVAL*SIN2A
         ELSE
            QOUT( I ) = VAL__BADD
            UOUT( I ) = VAL__BADD
         END IF

      END DO


      END
