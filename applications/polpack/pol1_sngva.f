      SUBROUTINE POL1_SNGVA( EL, DIN, T, PHI, EPS, DIM3, STOKES, SUM1, 
     :                       SUM2, TVAR, STATUS )

*+
*  Name:
*     POL1_SNGVA

*  Purpose:
*     Increment running sum images used to estimate input variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGVA( EL, DIN, T, PHI, EPS, DIM3, STOKES, SUM1, SUM2, 
*                      TVAR, STATUS )

*  Description:
*     For each pixel, this routine finds the residual between the
*     supplied input intensity value (DIN), and the corresponding 
*     intensity value implied by the Stokes vectors (STOKES). The
*     corresponding pixel in SUM1 is incremented by 1.0 (if the
*     input intensity value and Stokes vectors is good), and the 
*     corresponding pixel in SUM2 is incremented by the square of 
*     the residual.
*
*     Also returns the mean squared residual (i.e. the mean variance) in
*     the supplied image alone (excluding unusally large residuals).

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in an image.
*     DIN( EL ) = REAL (Given)
*        The input intensity values read from the current input NDF.
*     T = REAL (Given)
*        The analyser transmission factor for the current NDF.
*     PHI = REAL (Given)
*        The analyser angle for the current NDF. In radians.
*     EPS = REAL (Given)
*        The analyser efficiency factor for the current NDF.
*     DIM3 = INTEGER (Given)
*        No. of planes in STOKES.
*     STOKES( EL, DIM3 ) = REAL (Given)
*        The current (smoothed) estimate of the Stokes parameters.
*        multiple of the standard deviation.
*     SUM1( EL ) = REAL (Given and Returned)
*        The number of good residuals stored in SUM2 at each pixel.
*     SUM2( EL ) = REAL (Given and Returned)
*        The sum of the squared residuals at each pixel.
*     TVAR = REAL (Given and Returned)
*        The mean squared residual in the supplied image. Returned as
*        VAL__BADR if the supplied image has no good data. Should be
*        supplied equal to VAL__BADR if no estimate is available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-APR-1999 (DSB):
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
      REAL T
      REAL PHI
      REAL EPS
      INTEGER DIM3
      REAL STOKES( EL, DIM3 )

*  Arguments Given and Returned:
      REAL SUM1( EL )
      REAL SUM2( EL )

*  Arguments Returned:
      REAL TVAR 

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I    
      INTEGER TSUM1
      REAL EXPECT
      REAL K1
      REAL K2
      REAL K3
      REAL SQRES
      REAL TLIM
      REAL TSUM2   
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inialise.
      TSUM1 = 0
      TSUM2 = 0.0

*  Set up the highest squared residual value which will be included in
*  the estimate of the mean squared residual for the current image.
*  Very large square residuals are not included since they are probably
*  caused by stars etc. The limit corresponds to 4 times the previous
*  standard deviation estimate. No limit is imposed if there is no
*  previous estimate of the standard deviation.
      IF( TVAR .NE. VAL__BADR ) THEN
         TLIM = 16.0*TVAR
      ELSE
         TLIM = VAL__MAXR
      END IF

*  Store some constants.
      K1 = 0.5*T
      K2 = EPS*COS( 2*PHI )
      K3 = EPS*SIN( 2*PHI )

*  Do each pixel.
      DO I = 1, EL

*  Check all values are good.
         IF( STOKES( I, 1 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 2 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 3 ) .NE. VAL__BADR .AND.
     :       DIN( I ) .NE. VAL__BADR ) THEN

*  Calculate the expected intensity value on the basis of the supplied
*  Stokes vector.
            EXPECT = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 ) 
     :                                   + K3*STOKES( I, 3 ) )

*  Form the squared residual.
            SQRES = ( EXPECT - DIN( I ) )**2

*  Increment the running sum images.
            SUM1( I ) = SUM1( I ) + 1.0      
            SUM2( I ) = SUM2( I ) + SQRES

*  Increment the running sum values for this single image, but only if
*  this residual is not very large.
            IF( SQRES .LT. TLIM ) THEN
               TSUM1 = TSUM1 + 1
               TSUM2 = TSUM2 + SQRES
            END IF

         END IF

      END DO

*  Return the mean variance in this image.
      IF( TSUM1 .GT. 0 ) THEN
         TVAR = TSUM2/REAL( TSUM1 )
      ELSE
         TVAR = VAL__BADR
      END IF

      END
