      SUBROUTINE MASPEC( FILL, MASK, NPOINT, X, Y, W, DQ, INDS )
*+
*  Name:
*     SUBROUTINE MASPEC

*  Purpose:
*     Add-map current calibrated IUEHI order onto supplied grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MASPEC( FILL, MASK, NPOINT, X, Y, W, DQ, INDS )

*  Arguments:

*  Description:
*     The current calibrated order is mapped onto the supplied x-grid
*     using triangle function folding matched to the x-grid and
*     spectrum resolutions. The result is added into Y with a
*     weight appropriate to the original Net value; the weight is
*     added into W.
*     Undefined grid points have DQ=1, and these can receive contributions
*     from the input spectrum.
*     Defined grid points have DQ=0, and these can be added to.
*     Bad grid points are marked by DQ=3, and cannot be used further.

*  Method:
*     The folding function width is matched to the largest sampling
*     rate found in the order and the mapping grid.
*     The points in the order contribute to the mapping grid with
*     a weight that is inversely proportional to the calibration
*     factor that has been applied.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMWAV'
      INCLUDE 'CMFLX'
      INCLUDE 'CMCAL'

*  Arguments Given:
      LOGICAL FILL           ! whether a grid point can be filled if

      INTEGER MASK           ! mask for defining which DQ bits to use
      INTEGER NPOINT         ! number of points

      REAL*8 X(NPOINT)       ! x-values

*  Arguments Given and Returned:
      REAL*8 Y(NPOINT)       ! y-values
      REAL*8 W(NPOINT)       ! averaging weights

      INTEGER DQ(NPOINT)     ! data quality

*  Arguments Returned:
      INTEGER INDS(2)        ! indices of grid that have been modified

*  External References:
      INTEGER DQ_AND         ! 32 bit integer AND operation

*  Local Variables:
      REAL*8 DJ              ! x-grid index offset
      REAL*8 DJDX            ! x-grid index derivative
      REAL*8 HWZI            ! HWZI of triangle function
      REAL*8 WI              ! weight of order flux point
      REAL*8 WJ              ! weight of folding function
      REAL*8 XSTEP           ! x-grid step length

      REAL XLIM( 2 )         ! grid limits

      LOGICAL GOOD           ! whether input point is GOOD

      INTEGER I              ! loop index
      INTEGER I1             ! array index in GRF_XIND call
      INTEGER I2             ! array index in GRF_XIND call
      INTEGER J              ! loop index
      INTEGER J1             ! loop start
      INTEGER J2             ! loop end
*.

*   Set grid limits.
      XLIM( 1 ) = REAL( X( 1 ) )
      XLIM( 2 ) = REAL( X( NPOINT ) )
      INDS( 1 ) = NPOINT + 1
      INDS( 2 ) = 0

*   Find grid characteristics.
      XSTEP = ( X( NPOINT ) - X( 1 ) ) / DBLE( NPOINT - 1 )
      DJDX = 1.0 / XSTEP
      DJ = 1.0 - X( 1 ) / XSTEP

*   Allow for original data on coarser grid.
      HWZI = XSTEP
      DO I = 2, NWAV
         HWZI = MAX( HWZI, ( WAVAIR( I ) - WAVAIR( I - 1 ) ) )
      END DO

*   Find start-end indices for input spectrum.
      DO I = 1, NWAV
         FWAVAIR( I ) = REAL( WAVAIR( I ) )
      END DO
      CALL GRF_XIND( 1, NWAV, FWAVAIR, QFLX, XLIM, I1, I2 )

      IF ( I1 .GT. I2 ) THEN
         GO TO 100
      END IF

*   Let each point in order contribute to grid
      DO I = I1, I2
         GOOD = ( DQ_AND( QFLX( I ), MASK ) .EQ. 0 )
         J1 = DJDX * ( WAVAIR( I ) - HWZI ) + DJ
         J2 = DJDX * ( WAVAIR( I ) + HWZI ) + DJ + 1.0d0
         IF ( J1 .GT. NPOINT ) THEN
            GO TO 100
         END IF

         IF ( J2 .GE. 1 ) THEN
            J1 = MAX( J1, 1 )
            J2 = MIN( J2, NPOINT )
            WI = SCAL( I )

            DO J = J1, J2
               WJ = WI * MAX(
     :                    1.0d0 - ABS( ( WAVAIR( I ) - X( J ) ) / HWZI),
     :                    0.0d0 )
               IF ( WJ .GT. 0.0 ) THEN
                  INDS( 1 ) = MIN( INDS( 1 ), J )
                  INDS( 2 ) = MAX( INDS( 2 ), J )
                  IF ( DQ( J ) .NE. 3 ) THEN
                     IF ( GOOD ) THEN
                        Y( J ) = Y( J ) + WJ * SFLX( I )
                        W( J ) = W( J ) + WJ
                        DQ( J ) = 0

                     ELSE IF ( .NOT. FILL ) THEN
                        DQ( J ) = 3
                     END IF
                  END IF
               END IF
            END DO
         END IF
      END DO
 100  CONTINUE

      END
