      SUBROUTINE POL1_SNGSM( ILEVEL, NPIX, NROW, NP, VAR, DATA, WORK, 
     :                       WGT, STATUS )
*+
*  Name:
*     POL1_SNGSM

*  Purpose:
*     Smoothes all planes of a 3D cube.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGSM( ILEVEL, NPIX, NROW, NP, VAR, DATA, WORK, WGT, 
*                      STATUS )

*  Description:
*     This routine smoothes each plane of the supplied cube. A quadratic
*     surface is fitted to the data in a 7x7 box centred on each pixel in
*     turn. The central pixel value is replaced by the value of the 
*     fitted surface at the central pixel. 

*  Arguments:
*     ILEVEL = INTEGER (Given)
*        The level of information to display on the screen. Values above 1
*        result in a message being displayed indicating which images
*        (I,Q, or U) are being smoothed.
*     NPIX = INTEGER (Given)
*        The number of pixels per row in each plane.
*     NROW = INTEGER (Given)
*        The number of rows in each plane.
*     NP = INTEGER (Given)
*        The number of planes.
*     VAR( NPIX, NROW, NP ) = REAL (Given)
*        The variances for the array being smoothed.
*     DATA( NPIX, NROW, NP ) = REAL (Given and Returned)
*        The array to be smoothed.
*     WORK( NPIX, NROW ) = REAL (Given and Returned)
*        A work array.
*     WGT( NPIX, NROW ) = REAL (Given and Returned)
*        A work array for weights
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
      INTEGER ILEVEL
      INTEGER NPIX
      INTEGER NROW
      INTEGER NP
      REAL VAR( NPIX, NROW, NP )

*  Arguments Given and Returned:
      REAL DATA( NPIX, NROW, NP )
      REAL WORK( NPIX, NROW )
      REAL WGT( NPIX, NROW )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER HW                 ! Half-size of the fitting box 
      PARAMETER ( HW = 3 )

*  Local Variables:
      DOUBLE PRECISION C( 6 )    ! Fit co-efficients
      DOUBLE PRECISION DVAL      ! Data value
      DOUBLE PRECISION MAT(6,6)  ! Matrix representing normal equations
      DOUBLE PRECISION SD        ! Sum over the box
      DOUBLE PRECISION SDX       ! Sum over the box
      DOUBLE PRECISION SDX2      ! Sum over the box
      DOUBLE PRECISION SDXY      ! Sum over the box
      DOUBLE PRECISION SDY       ! Sum over the box
      DOUBLE PRECISION SDY2      ! Sum over the box
      DOUBLE PRECISION SW        ! Sum over the box
      DOUBLE PRECISION SX        ! Sum over the box
      DOUBLE PRECISION SX2       ! Sum over the box
      DOUBLE PRECISION SX2Y      ! Sum over the box
      DOUBLE PRECISION SX2Y2     ! Sum over the box
      DOUBLE PRECISION SX3       ! Sum over the box
      DOUBLE PRECISION SX3Y      ! Sum over the box
      DOUBLE PRECISION SX4       ! Sum over the box
      DOUBLE PRECISION SXY       ! Sum over the box
      DOUBLE PRECISION SXY2      ! Sum over the box
      DOUBLE PRECISION SXY3      ! Sum over the box
      DOUBLE PRECISION SY        ! Sum over the box
      DOUBLE PRECISION SY2       ! Sum over the box
      DOUBLE PRECISION SY3       ! Sum over the box
      DOUBLE PRECISION SY4       ! Sum over the box
      DOUBLE PRECISION W         ! Pixel weight
      INTEGER I                  ! Pixel index
      INTEGER II                 ! Pixel offset
      INTEGER IHI                ! Upper X bound of fitting box
      INTEGER ILO                ! Lower X bound of fitting box
      INTEGER IX                 ! Pixel index
      INTEGER IY                 ! Row index
      INTEGER IZ                 ! Plane index
      INTEGER J                  ! Row index
      INTEGER JJ                 ! Pixel offset
      INTEGER JHI                ! Upper Y bound of fitting box
      INTEGER JLO                ! Lower Y bound of fitting box
      INTEGER K                  ! Loop count

      DOUBLE PRECISION X4( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION X3Y( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION X2Y2( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION XY3( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION Y4                   ! Co-ordinate data

      DOUBLE PRECISION X3( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION X2Y( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION XY2( -HW:HW, -HW:HW ) ! Co-ordinate data
      DOUBLE PRECISION Y3                   ! Co-ordinate data

      DOUBLE PRECISION X2                   ! Co-ordinate data
      DOUBLE PRECISION XY                   ! Co-ordinate data
      DOUBLE PRECISION Y2                   ! Co-ordinate data

      DOUBLE PRECISION X                   ! Co-ordinate data
      DOUBLE PRECISION Y                   ! Co-ordinate data

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the arrays holding the high (3 and 4) powers of X and Y at the 
*  centre of each pixel in a fitting box. 
      DO JJ = -HW, HW      
         DO II = -HW, HW      
            X4( II, JJ ) = II**4
            X3Y( II, JJ ) = ( II**3 )*( JJ )
            X2Y2( II, JJ ) = ( II**2 )*( JJ**2 )
            XY3( II, JJ ) = ( II )*( JJ**3 )
            X3( II, JJ ) = II**3 
            X2Y( II, JJ ) = ( II**2 )*( JJ )
            XY2( II, JJ ) = ( II )*( JJ**2 )
         END DO
      END DO

*  Loop round each plane.
      DO IZ = 1, NP

*  Tell the user what is happening since this routine can take a long
*  time to run.
         IF( ILEVEL .GT. 1 ) THEN

            IF( IZ .EQ. 1 ) THEN
               CALL MSG_SETC( 'S', 'I' ) 
            ELSE IF( IZ .EQ. 2 ) THEN
               CALL MSG_SETC( 'S', 'Q' ) 
            ELSE IF( IZ .EQ. 3 ) THEN
               CALL MSG_SETC( 'S', 'U' ) 
            END IF            

            CALL MSG_OUT( 'POL1_SNGSM_MSG1', '   Reducing noise in '//
     :                    'the ^S image.', STATUS )

         END IF

*  Create the weights image for this plane.
         DO IY = 1, NROW     
            DO IX = 1, NPIX
               IF( DATA( IX, IY, IZ ) .NE. VAL__BADR .AND.
     :             VAR( IX, IY, IZ ) .NE. VAL__BADR .AND.
     :             VAR( IX, IY, IZ ) .GT. 0.0 ) THEN
                  WGT( IX, IY ) = 1.0/( VAR( IX, IY, IZ )**2 )
               ELSE
                  WGT( IX, IY ) = 0.0
               END IF
            END DO
         END DO

*  Smooth every pixel in this plane.
         DO IY = 1, NROW     
            DO IX = 1, NPIX
                
*  Smooth the current pixel.
*  --------------------------

*  If the pixel being fitted has zero weight, return a bad value.
               IF( WGT( IX, IY ) .EQ. 0.0 ) THEN
                   WORK( IX, IY ) = VAL__BADR

*  Otherwise find the fitted value...
               ELSE

*  Find the pixel index bounds of the box.
                  ILO = MAX( 1, IX - HW )
                  IHI = MIN( NPIX, IX + HW )
                  JLO = MAX( 1, IY - HW )
                  JHI = MIN( NROW, IY + HW )

*  Initilise the required sums.
                  SX4 = 0.0
                  SX3Y = 0.0
                  SX2Y2 = 0.0
                  SXY3 = 0.0
                  SY4 = 0.0
                  SX3 = 0.0
                  SX2Y = 0.0
                  SXY2 = 0.0
                  SY3 = 0.0
                  SX2 = 0.0
                  SXY = 0.0
                  SY2 = 0.0
                  SX = 0.0
                  SY = 0.0
                  SDX2 = 0.0
                  SDY2 = 0.0
                  SDXY = 0.0
                  SDX = 0.0
                  SDY = 0.0
                  SD = 0.0
                  SW = 0.0

*  Scan the rows in the box.
                  DO J = JLO, JHI
                     JJ = J - IY 
                     Y = DBLE( JJ )
                     Y2 = DBLE( JJ**2 )
                     Y3 = DBLE( JJ**3 )
                     Y4 = DBLE( JJ**4 )

*  Scan the pixels in this row.
                     DO I = ILO, IHI
                        II = I - IX 
                        X = DBLE( II )
                        XY = DBLE( JJ * II )
                        X2 = DBLE( II * II )

*  Only proceed if the weight for this pixel is greater than zero.
                        IF(  WGT( I, J ) .GT. 0.0 ) THEN

*  Save the weight for this pixel.
                           W = DBLE( WGT( I, J ) )

*  Reduce the weight for the central pixel to reduce the correlation
*  between the noise in the fitted value and the noise in the central value.
                           IF( II .EQ. 0 .AND. JJ .EQ. 0 ) W = 0.5*W

*  Save the weighted data value.
                           DVAL = W*DBLE( DATA( I, J, IZ ) )

*  Increment the sums.
                           SX4 = SX4 + X4( II, JJ )*W
                           SX3Y = SX3Y + X3Y( II, JJ )*W
                           SX2Y2 = SX2Y2 + X2Y2( II, JJ )*W
                           SXY3 = SXY3 + XY3( II, JJ )*W
                           SY4 = SY4 + Y4*W
                           SX3 = SX3 + X3( II, JJ )*W
                           SX2Y = SX2Y + X2Y( II, JJ )*W
                           SXY2 = SXY2 + XY2( II, JJ )*W
                           SY3 = SY3 + Y3*W
                           SX2 = SX2 + X2*W
                           SXY = SXY + XY*W
                           SY2 = SY2 + Y2*W
                           SX = SX + X*W
                           SY = SY + Y*W
                           SDX2 = SDX2 + DVAL*X2
                           SDY2 = SDY2 + DVAL*Y2
                           SDXY = SDXY + DVAL*XY
                           SDX = SDX + DVAL*X
                           SDY = SDY + DVAL*Y
                           SD = SD + DVAL
                           SW = SW + W
                  
                        END IF
                  
                     END DO
   
                  END DO

*  If no good data was found in the box, return a bad value.
                  IF( SW .LE. 0.0 ) THEN
                     WORK( IX, IY ) = VAL__BADR

*  Otherwise, solve the normal equations for the least squares solution.
                  ELSE

*  Normalised all sums by dividing by the sum of the weights.
                     SX4 = SX4/SW
                     SX3Y = SX3Y/SW
                     SX2Y2 = SX2Y2/SW
                     SXY3 = SXY3/SW
                     SY4 = SY4/SW
                     SX3 = SX3/SW
                     SX2Y = SX2Y/SW
                     SXY2 = SXY2/SW
                     SY3 = SY3/SW
                     SX2 = SX2/SW
                     SXY = SXY/SW
                     SY2 = SY2/SW
                     SX = SX/SW
                     SY = SY/SW
                     SDX2 = SDX2/SW
                     SDY2 = SDY2/SW
                     SDXY = SDXY/SW
                     SDX = SDX/SW  
                     SDY = SDY/SW  
                     SD = SD/SW   
                     SW = 1.0
   
*  Set up the matrix representing the normal equations. The matrix is 
*  symetric and so onlt the elements on or above the diagonal need
*  to be set (the other elements will not be referenced).
                     MAT(1,1) = SX4
                     MAT(2,1) = SX2Y2
                     MAT(3,1) = SX3Y
                     MAT(4,1) = SX3
                     MAT(5,1) = SX2Y
                     MAT(6,1) = SX2
                     
                     MAT(2,2) = SY4
                     MAT(3,2) = SXY3
                     MAT(4,2) = SXY2
                     MAT(5,2) = SY3
                     MAT(6,2) = SY2
                     
                     MAT(3,3) = SX2Y2
                     MAT(4,3) = SX2Y
                     MAT(5,3) = SXY2
                     MAT(6,3) = SXY
                     
                     MAT(4,4) = SX2
                     MAT(5,4) = SXY
                     MAT(6,4) = SX
                     
                     MAT(5,5) = SY2
                     MAT(6,5) = SY
                     
                     MAT(6,6) = 1.0
   
*  Set up the offset vector.
                     C( 1 ) = SDX2
                     C( 2 ) = SDY2
                     C( 3 ) = SDXY
                     C( 4 ) = SDX
                     C( 5 ) = SDY
                     C( 6 ) = SD
   
*  Find the constant term in the solution by inverting the matrix equation set 
*  up above. We only need the last (constant) term ( C(6) ) and so we do
*  not need to do a full matrix inversion. Also, the matrix is symetric
*  and so we only need to look at the elements above the diagonal.
                     DO K = 1, 5
                        DO I = K + 1, 6
                           DO J = I, 6
                              MAT( J, I ) = MAT( I, K )*MAT( J, K ) -
     :                                      MAT( J, I )*MAT( K, K )
                           END DO
                           C( I ) = C( K )*MAT( I, K ) - 
     :                              C( I )*MAT( K, K )
                        END DO
                     END DO

*  If the matrix was singular, return VAL__BADR. Otherwise, return the
*  central value.
                     IF( MAT( 6, 6 ) .EQ. 0.0 ) THEN
                        WORK( IX, IY ) = VAL__BADR

                     ELSE
                        WORK( IX, IY ) = REAL( C( 6 )/ MAT( 6, 6 ) )
                     END IF
               
                  END IF
               
               END IF

*  -------------------------------------
*  Now move on to smooth the next pixel.

            END DO
         END DO

*  Copy the smoothed data for this plane from the work array back to the 
*  input array. 
         DO IY = 1, NROW
            DO IX = 1, NPIX

               DATA( IX, IY, IZ ) = WORK( IX, IY )

            END DO
         END DO

      END DO

      IF( ILEVEL .GT. 1 ) CALL MSG_BLANK( STATUS )

      END
