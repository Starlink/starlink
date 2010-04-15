      SUBROUTINE POL1_STKSM( EL, DIN, DOUT, VOUT, CNT, STATUS )
*+
*  Name:
*     POL1_STKSM

*  Purpose:
*     Increment the running sum images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STKSM( EL, DIN, DOUT, VOUT, CNT, STATUS )

*  Description:
*     This routine increments the running sum images at every pixel
*     which is good in DIN:
*
*        DOUT = DOUT + DIN
*        VOUT = VOUT + DIN*DIN
*        CNT = CNT + 1.0

*  Arguments:
*     EL = INTEGER (Given)
*        Size of each array.
*     DIN( EL ) = REAL (Given)
*        The input DATA array.
*     DOUT( EL ) = REAL (Given and Returned)
*        The sum of the input DATA values.
*     VOUT( EL ) = REAL (Given and Returned)
*        The sum of the squared input DATA values.
*     CNT( EL ) = REAL (Given and Returned)
*        The number of input pixels included.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
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

*  Arguments Given:
      INTEGER EL
      REAL DIN( EL )

*  Arguments Given and Returned:
      REAL DOUT( EL )
      REAL VOUT( EL )
      REAL CNT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index of current input NDF
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each pixel, adding the good values into the arrays.
      DO I = 1, EL
         IF( DIN( I ) .NE. VAL__BADR ) THEN
            DOUT( I ) = DOUT( I ) + DIN( I )
            VOUT( I ) = VOUT( I ) + DIN( I )**2
            CNT( I ) = CNT( I ) + 1.0
         END IF
      END DO

      END
