      SUBROUTINE POL1_SNGSI( T, PHI, EPS, EL, DIM1, DIM2, STOKES, 
     :                       VSTOK, DIN, DOUT, WORK, WGT, DMAX, DMIN, 
     :                       STATUS )
*+
*  Name:
*     POL1_SNGSI

*  Purpose:
*     Return squared residuals between real and simulated intensity values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGSI( T, PHI, EPS, EL, DIM1, DIM2, STOKES, VSTOK, 
*                      DIN, DOUT, WORK, WGT, DMAX, DMIN, STATUS )

*  Description:
*     This routine returns the squared residuals between the input
*     intensity values (DIN) and the intensity values implied by the
*     input Stokes vectors (STOKES). The residuals are high-pass filtered
*     before being returned in order to reduce the effects of zero point
*     errors (i.e. errors in sky subtraction). This filtering is performed
*     by smoothing the residuals with a 9x9 pixel mean box filter to
*     obtain an estimate fo the local mean residual. This local mean
*     residual is then subtracted from the original residuals. The
*     residuals are weighted by the recprocal of the Stokes variances, when
*     forming the local mean residual estimates.

*  Arguments:
*     T = REAL (Given)
*        The analyser transmission factor for the supplied array.
*     PHI = REAL (Given)
*        The analyser angle for the supplied array. In radians.
*     EPS = REAL (Given)
*        The analyser efficiency factor for the supplied array.
*     EL = INTEGER (Given)
*        Total number of pixels in each plane.
*     DIM1 = INTEGER (Given)
*        No. of pixels in each row.
*     DIM2 = INTEGER (Given)
*        No. of rows in each plane.
*     STOKES( EL, 3 ) = REAL (Given)
*        The current (smoothed) estimate of the Stokes parameters.
*     VSTOK( EL, 3 ) = REAL (Given)
*        The variances for STOKES.
*     DIN( EL ) = REAL (Given)
*        The intensity values read from the input NDF.
*     DOUT( EL ) = REAL (Returned)
*        The filtered, squared intensity residuals between DIN and STOKES.
*     WORK( EL ) = REAL (Returned)
*        A work array. Returned holding the expected intensity values
*        implied by teh STokes vectors.
*     WGT( EL ) = REAL (Returned)
*        The weights associated with the DOUT array. 
*     DMAX = REAL (Returned)
*        The highest expected data value. 
*     DMIN = REAL (Returned)
*        The lowest expected data value.
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
      INTEGER DIM1
      INTEGER DIM2
      REAL STOKES( EL, 3 )
      REAL VSTOK( EL, 3 )
      REAL DIN( EL )

*  Arguments Returned:
      REAL DOUT( EL )
      REAL WORK( EL )
      REAL WGT( EL )
      REAL DMAX
      REAL DMIN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HW                 ! Half width of smoothing box in pixels
      PARAMETER( HW = 4 ) 

*  Local Variables:
      INTEGER I                  ! Pixel index
      INTEGER IPW1               ! Pointer to work space
      INTEGER IPW2               ! Pointer to work space
      REAL EVAR                  ! Variance for the residual
      REAL EXPECT                ! Expected input data value
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

*  Initialise the returned data limits
      DMAX = VAL__MINR
      DMIN = VAL__MAXR

*  Do each pixel.
      DO I = 1, EL

*  Check all Stokes parameters and NDF inetensity values are good.
         IF( STOKES( I, 1 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 2 ) .NE. VAL__BADR .AND.
     :       STOKES( I, 3 ) .NE. VAL__BADR .AND.
     :       VSTOK( I, 1 ) .NE. VAL__BADR .AND.
     :       VSTOK( I, 2 ) .NE. VAL__BADR .AND.
     :       VSTOK( I, 3 ) .NE. VAL__BADR .AND.
     :       DIN( I ) .NE. VAL__BADR ) THEN

*  Calculate the expected intensity value on the basis of the supplied
*  Stokes vector.
            EXPECT = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 ) 
     :                                   + K3*STOKES( I, 3 ) )
             
*  Get the variance on the expected value.
            EVAR = K1S*( VSTOK( I, 1 ) + K2S*VSTOK( I, 2 )
     :                                       + K3S*VSTOK( I, 3 ) )

*  Store a weight for the residual.
            IF( EVAR .GT. 1.0E-15 ) THEN
               WGT( I ) = 1.0/EVAR
            ELSE
               CALL MSG_SETR( 'V', EVAR )
               CALL MSG_OUT( ' ', 'WARNING: Zero or negative '//
     :                       'variance encountered: ^V', STATUS )
               WGT( I ) = 0.0
            END IF

*  Store the difference between the above expected intensity and
*  the intensity in the NDF,
            DOUT( I ) = ( EXPECT - DIN( I ) )
            work( i ) = expect

*  Update the data limits.
            DMAX = MAX( DMAX, EXPECT )
            DMIN = MIN( DMIN, EXPECT )

*  Store a bad residual if any of the input values were bad.
         ELSE
            DOUT( I ) = VAL__BADR
            WGT( I ) = 0.0
         END IF

      END DO

*  Allocate work space for the smoothing routine.
      CALL PSX_CALLOC( DIM1, '_REAL', IPW1, STATUS )
      CALL PSX_CALLOC( DIM1, '_REAL', IPW2, STATUS )

*  Smooth the residuals array, putting the results in the work array.
*  This is a 9x9 mean box-filter.
      CALL POL1_BLKWR( DIM1, DIM2, DOUT, WGT, HW, HW, 0.0, 
     :                 WORK, %VAL( IPW1 ), %VAL( IPW2 ), STATUS )

*  Subtract these smoothed residuals from the original residuals, and
*  square them. Also return the expected data value, and square the
*  residuals weight.
      DO I = 1, EL
         IF( DOUT( I ) .NE. VAL__BADR .AND.
     :       WORK( I ) .NE. VAL__BADR ) THEN
            DOUT( I ) = ( DOUT( I ) - WORK( I ) )**2

            WORK( I ) = K1*( STOKES( I, 1 ) + K2*STOKES( I, 2 ) 
     :                                      + K3*STOKES( I, 3 ) )


            WGT( I ) = 1.0
         ELSE
            DOUT( I ) = VAL__BADR
            WGT( I ) = 0.0
            WORK( I ) = VAL__BADR
         END IF
      END DO

*  Free the work space used by the smoothing routine.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

      END
