      SUBROUTINE POL1_SIMCL( VAR, EL, STOKES, VSTOK, T, EPS, PHI,
     :                       DOUT, VOUT, STATUS )
*+
*  Name:
*     POL1_SNGSI

*  Purpose:
*     Generate simulated intensity data from a set of Stokes vectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SIMCL( VAR, EL, STOKES, VSTOK, T, EPS, PHI, DOUT, VOUT,
*                      STATUS )

*  Description:
*     This routine returns the simulated intensity data from a set of
*     Stokes vectors.

*  Arguments:
*     VAR = LOGICAL (Given)
*        Should variances be created for the simulated intensity values?
*     EL = INTEGER (Given)
*        No. of pixels in each image.
*     STOKES( EL, 3 ) = REAL (Given)
*        The Stokes parameters.
*     VSTOK( EL, 3 ) = REAL (Given)
*        The variances for STOKES. Only accessed if VAR is TRUE.
*     T = REAL (Given)
*        The analyser transmission factor for the supplied array.
*     EPS = REAL (Given)
*        The analyser efficiency factor for the supplied array.
*     PHI = REAL (Given)
*        The analyser angle for the supplied array. In radians.
*     DOUT( EL ) = REAL (Returned)
*        The simulated intensity values.
*     VOUT( EL ) = REAL (Returned)
*        The variances on the simulated intensity values. Only accessed
*        if VAR is TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1999 (DSB):
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
      LOGICAL VAR
      INTEGER EL
      REAL STOKES( EL, 3 )
      REAL VSTOK( EL, 3 )
      REAL T
      REAL PHI
      REAL EPS

*  Arguments Returned:
      REAL DOUT( EL )
      REAL VOUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Pixel index
      REAL K1, K2, K3            ! Constants
      REAL K1S, K2S, K3S         ! Constants
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store some constants.
      K1 = 0.5*T
      K2 = EPS*COS( 2*PHI )
      K3 = EPS*SIN( 2*PHI )
      K1S = K1**2
      K2S = K2**2
      K3S = K3**2

*  Do each pixel.
      DO I = 1, EL

*  Check all Stokes parameters are good.
         IF( STOKES( I, 1 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 2 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 3 ) .NE. VAL__BADR ) THEN

*  Calculate the expected intensity value on the basis of the supplied
*  Stokes vector.
            DOUT( I ) = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 )
     :                                      + K3*STOKES( I, 3 ) )

*  Store a bad value if any of the input values were bad.
         ELSE
            DOUT( I ) = VAL__BADR
         END IF

      END DO

*  IF variances are required...
      IF( VAR ) THEN

*  Do each pixel.
         DO I = 1, EL

*  Check all Stokes variances are good.
            IF( VSTOK( I, 1 ) .NE. VAL__BADR .AND.
     :          VSTOK( I, 2 ) .NE. VAL__BADR .AND.
     :          VSTOK( I, 3 ) .NE. VAL__BADR ) THEN

*  Get the variance on the expected value.
               VOUT( I ) = K1S*( VSTOK( I, 1 ) + K2S*VSTOK( I, 2 )
     :                                         + K3S*VSTOK( I, 3 ) )

*  Store a bad residual if any of the input values were bad.
            ELSE
               VOUT( I ) = VAL__BADR
            END IF

         END DO

      END IF

      END
