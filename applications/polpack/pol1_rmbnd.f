      SUBROUTINE POL1_RMBND( NIN, BLO, BHI, MAG, ANG, X, Y, NOUT, 
     :                       STATUS )
*+
*  Name:
*     POL1_RMBND

*  Purpose:
*     Set positions bad which are outside supplied bounds.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_RMBND( NIN, BLO, BHI, MAG, ANG, X, Y, NOUT, STATUS )

*  Description:
*     This routine sets values bad in the supplied arrays if the X or
*     Y value is outside the supplied bounds. All bad values are then 
*     shuffled to the end of the array, and the number of valid positions
*     in the returned arrays is returned.

*  Arguments:
*     NIN = INTEGER (Given)
*        The number of positions in each array.
*     BLO( 2 ) = REAL (Given)
*        The lower bounds on the two axes.
*     BHI( 2 ) = REAL (Given)
*        The upper bounds on the two axes.
*     MAG( NIN ) = REAL (Given and Returned)
*        The array of magnitude values.
*     ANG( NIN ) = REAL (Given and Returned)
*        The array of angle values.
*     X( NIN ) = DOUBLE PRECISION (Given and Returned)
*        The array of X values.
*     Y( NIN ) = DOUBLE PRECISION (Given and Returned)
*        The array of Y values.
*     NOUT = INTEGER (Returned)
*        The number of good positions in the returned array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-AUG-1998 (DSB):
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
      INTEGER NIN
      REAL BLO( 2 )
      REAL BHI( 2 )

*  Arguments Given and Returned:
      REAL MAG( NIN )
      REAL ANG( NIN )
      DOUBLE PRECISION X( NIN )
      DOUBLE PRECISION Y( NIN )

*  Arguments Returned:
      INTEGER NOUT

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION XLO       ! Lower X bound
      DOUBLE PRECISION XHI       ! Upper X bound
      DOUBLE PRECISION YLO       ! Lower Y bound
      DOUBLE PRECISION YHI       ! Upper Y bound
      INTEGER I                  ! Index count
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store _DOUBLE versions of the bounds.
      XHI = DBLE( BHI( 1 ) )
      XLO = DBLE( BLO( 1 ) )
      YHI = DBLE( BHI( 2 ) )
      YLO = DBLE( BLO( 2 ) )

*  First set any bad or out-of-bounds points bad (in all arrays).
      DO I = 1, NIN
         IF( MAG( I ) .EQ. VAL__BADR .OR. ANG( I ) .EQ. VAL__BADR .OR.
     :       X( I ) .EQ. VAL__BADD .OR. Y( I ) .EQ. VAL__BADD .OR.
     :       X( I ) .LT. XLO .OR. X( I ) .GT. XHI .OR.
     :       Y( I ) .LT. YLO .OR. Y( I ) .GT. YHI ) THEN
            MAG( I ) = VAL__BADR
            ANG( I ) = VAL__BADR
            X( I ) = VAL__BADD
            Y( I ) = VAL__BADD
         END IF
      END DO

*  Now shuffle the good values to the start of each array.
      NOUT = 0
      DO I = 1, NIN
         IF( MAG( I ) .NE. VAL__BADR ) THEN
            NOUT = NOUT + 1
            IF( I .NE. NOUT ) THEN
               MAG( NOUT ) = MAG( I )            
               ANG( NOUT ) = ANG( I )            
               X( NOUT ) = X( I )            
               Y( NOUT ) = Y( I )            
            END IF
         END IF
      END DO

*  Fill the remainder of each array with bad values.
      DO I = NOUT + 1, NIN
         MAG( I ) = VAL__BADR
         ANG( I ) = VAL__BADR
         X( I ) = VAL__BADD
         Y( I ) = VAL__BADD
      END DO

      END
