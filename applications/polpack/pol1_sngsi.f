      SUBROUTINE POL1_SNGSI( T, PHI, EPS, EL, STOKES, DIN, DOUT, 
     :                       STATUS )
*+
*  Name:
*     POL1_SNGSI

*  Purpose:
*     Return residuals between real and simulated intensity values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGSI( T, PHI, EPS, EL, STOKES, DIN, DOUT, STATUS )

*  Description:
*     This routine returns the squared residuals between the input
*     intensity values (DIN) and the intensity values implied by the
*     input Stokes vectors (STOKES).

*  Arguments:
*     T = REAL (Given)
*        The analyser transmission factor for the supplied array.
*     PHI = REAL (Given)
*        The analyser angle for the supplied array. In radians.
*     EPS = REAL (Given)
*        The analyser efficiency factor for the supplied array.
*     EL = INTEGER (Given)
*        No. of pixels in each plane.
*     STOKES( EL, 3 ) = REAL (Given)
*        The current (smoothed) estimate of the Stokes parameters.
*     DIN( EL ) = REAL (Given)
*        The intensity values read from the input NDF.
*     DOUT( EL ) = REAL (Returned)
*        The squared intensity residuals between DIN and STOKES.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1999 (DSB):
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
      REAL T
      REAL PHI
      REAL EPS
      INTEGER EL
      REAL STOKES( EL, 3 )
      REAL DIN( EL )

*  Arguments Returned:
      REAL DOUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element index
      REAL EXPECT                ! Expected input data value
      REAL K1, K2, K3            ! Constants
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store some constants.
      K1 = 0.5*T
      K2 = EPS*COS( 2*PHI )
      K3 = EPS*SIN( 2*PHI )

*  Do each pixel.
      DO I = 1, EL

*  Check all Stokes parameters and NDF inetensity values are good.
         IF( STOKES( I, 1 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 2 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 3 ) .NE. VAL__BADR .AND.
     :       DIN( I ) .NE. VAL__BADR ) THEN

*  Calculate the expected intensity value on the basis of the supplied
*  Stokes vector.
            EXPECT = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 ) 
     :                                 + K3*STOKES( I, 3 ) )

*  Store the squared difference between the above expected intensity and
*  the intensity in the NDF,
            DOUT( I ) = ( EXPECT - DIN( I ) )**2            

*  Store a bad residual if any of the input values were bad.
         ELSE
            DOUT( I ) = VAL__BADR
         END IF

      END DO

      END
