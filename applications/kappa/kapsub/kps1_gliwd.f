      SUBROUTINE KPS1_GLIWD( LBND1, LBND2, UBND1, UBND2, VAR, NPOS,
     :                       INDIM, PIXPOS, DIN, VIN, DOUT, VOUT, NREP,
     :                       STATUS )
*+
*  Name:
*     KPS1_GLIWD

*  Purpose:
*     Store bad pixels positions in a supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GLIWD( LBND1, LBND2, UBND1, UBND2, VAR, NPOS, INDIM,
*                      PIXPOS, DIN, VIN, DOUT, VOUT, NREP, STATUS )

*  Description:
*     This routine replaces specified pixels in the dupplied data and
*     variance arrays by the median of the neighbouring pixels.

*  Arguments:
*     LBND1 = INTEGER (Given)
*        Lower pixel index on axis 1.
*     LBND2 = INTEGER (Given)
*        Lower pixel index on axis 2.
*     UBND1 = INTEGER (Given)
*        Upper pixel index on axis 1.
*     UBND2 = INTEGER (Given)
*        Upper pixel index on axis 2.
*     VAR = LOGICAL (Given)
*        Should variance values be used?
*     NPOS = INTEGER (Given)
*        The number of positions to replace.
*     INDIM = INTEGER (Given)
*        The size of the first axis of the positions array.
*     PIXPOS( INDIM, 2 ) = DOUBLE PRECISION (Given)
*        Stores the NPOS pixel positions which are to be replaced.
*     DIN( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given)
*        The input data array.
*     VIN( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given)
*        The input variance array. Only accessed if VAR is .TRUE.
*     DOUT( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given and Returned)
*        The output data array. This should be supplied holding a copy of
*        DIN.
*     VOUT( LBND1:UBND1, LBND2:UBND2 ) = DOUBLE PRECISION (Given and Returned)
*        The output variance array. Only accessed if VAR is .TRUE. in
*        which case it should be supplied holding a copy of VIN.
*     NREP = INTEGER (Returned)
*        The number of pixels succesfully replaced.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2000 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE parameters
      INCLUDE 'PRM_PAR'        ! VAL__ constants
      INCLUDE 'AST_PAR'        ! AST__ constants

*  Arguments Given:
      INTEGER LBND1
      INTEGER LBND2
      INTEGER UBND1
      INTEGER UBND2
      LOGICAL VAR
      INTEGER NPOS
      INTEGER INDIM
      DOUBLE PRECISION PIXPOS( INDIM, 2 )
      DOUBLE PRECISION DIN( LBND1:UBND1, LBND2:UBND2 )
      DOUBLE PRECISION VIN( LBND1:UBND1, LBND2:UBND2 )

*  Arguments Given and Returned:
      DOUBLE PRECISION DOUT( LBND1:UBND1, LBND2:UBND2 )
      DOUBLE PRECISION VOUT( LBND1:UBND1, LBND2:UBND2 )

*  Arguments Returned:
      INTEGER NREP

*  Global Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_CEIL               ! Round up

*  Local Constants:
      INTEGER NPIX                    ! No of adjacent pixels
      PARAMETER( NPIX = 8 )

      INTEGER NMAT                    ! Workspace dimension
      PARAMETER( NMAT = NPIX*( NPIX + 1 )/2 )

*  Local Variables:
      DOUBLE PRECISION PP( NPIX )     ! Workspace for order statistics calcs
      DOUBLE PRECISION COVEC( NMAT, NPIX )! Ordered statistics variace-covariance
      INTEGER IPIX                    ! Index of pixel to be repalced
      INTEGER I0                      ! X pixel index of central pixel
      INTEGER J0                      ! Y pixel index of central pixel
      INTEGER IHI                     ! Upper limit on X of 3x3 box
      INTEGER ILO                     ! Lower limit on X of 3x3 box
      INTEGER JHI                     ! Upper limit on Y of 3x3 box
      INTEGER JLO                     ! Lower limit on Y of 3x3 box
      INTEGER K                       ! Neighbouring pixel index
      INTEGER KTOP                    ! No. of neighbouring pixels
      INTEGER I                       ! X pixel index
      INTEGER J                       ! Y pixel index
      INTEGER NGOOD                   ! No. of good neighbouring pixels
      INTEGER POINT( NPIX )           ! Original indices of sorted values
      DOUBLE PRECISION NEWVAR              ! New variance
      DOUBLE PRECISION SUM                 ! Sum of weights
      DOUBLE PRECISION SVAR                ! Variance of the unordered sample
      DOUBLE PRECISION WRK1( NPIX )        ! Data values to use
      DOUBLE PRECISION WRK2( NPIX )        ! Variance/Weight values to use
*.

*  Initialise
      NREP = 0

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Derive the variance-covariance matrix for the order statistics of a
*  normal population with up to 8 members. This also sets up the scale factor
*  for converting mean variances to median variances.
      CALL KPG1_ORVAR( NPIX, NMAT, PP, COVEC, STATUS )

*  Loop round all the pixels to be replaced.
      DO IPIX = 1, NPOS

*  Check this position is defined.
         IF( PIXPOS( IPIX, 1 ) .NE. AST__BAD .AND.
     :       PIXPOS( IPIX, 2 ) .NE. AST__BAD ) THEN

*  Get the indices of the pixel to be replaced. Round up so that a
*  supplied value of 1.0 causes pixel index 1 to be used.
            I0 = KPG1_CEIL( REAL( PIXPOS( IPIX, 1 ) ) )
            J0 = KPG1_CEIL( REAL( PIXPOS( IPIX, 2 ) ) )

*  Check this pixel is inside the image.
            IF( I0 .GE. LBND1 .AND. I0 .LE. UBND1 .AND.
     :          J0 .GE. LBND2 .AND. J0 .LE. UBND2 ) THEN

*  Find the bounds of a 3x3 square of pixels centred on the pixel being
*  replaced
               JLO = MAX( LBND2, J0 - 1 )
               JHI = MIN( UBND2, J0 + 1 )
               ILO = MAX( LBND1, I0 - 1 )
               IHI = MIN( UBND1, I0 + 1 )

*  Loop round this square, storing the values of all pixels, except the
*  central pixel.
               K = 1
               DO J = JLO, JHI
                  DO I = ILO, IHI
                     IF( I .NE. I0 .OR. J .NE. J0 ) THEN
                        WRK1( K ) = DIN( I, J )
                        K = K + 1
                     END IF
                  END DO
               END DO

*  Save the number of values stored in the array.
               KTOP = K - 1

*  Do the same for the variance, if variance is available.
               IF( VAR ) THEN
                  K = 1
                  DO J = JLO, JHI
                     DO I = ILO, IHI
                        IF( I .NE. I0 .OR. J .NE. J0 ) THEN
                           WRK2( K ) = VIN( I, J )
                           K = K + 1
                        END IF
                     END DO
                  END DO

* If no variance is available, use a fixed variance of 1.0 for all pixels.
               ELSE
                  K = 1
                  DO K = 1, 8
                     WRK2( K ) = 1.0
                  END DO
               END IF

*  Remove any pixels which are not good in either data or variance array.
*  This is done by shuffling the good data to the start of the array.
*  Convert variances to weights at the same time, and form the sum of the
*  weights.
               NGOOD = 0
               SUM = 0.0
               DO K = 1, KTOP
                  IF( WRK1( K ) .NE. VAL__BADD .AND.
     :                WRK2( K ) .NE. VAL__BADD .AND.
     :                WRK2( K ) .NE. 0.0 ) THEN
                     NGOOD = NGOOD + 1
                     WRK1( NGOOD ) = WRK1( K )
                     WRK2( NGOOD ) = 1.0/WRK2( K )
                     POINT( NGOOD ) = K
                     SUM = SUM + WRK2( NGOOD )
                  END IF
               END DO

*  Store bad output values if there are less than 3 good input values in
*  the box.
               IF( NGOOD .LT. 3 ) THEN
                  DOUT( I0, J0 ) = VAL__BADD
                  IF( VAR ) VOUT( I0, J0 ) = VAL__BADD

*  Otherwise, find the weighted median of the data values, and the
*  corresponding variance.
               ELSE

*  Form initial population variance.
                  SVAR = 1.0 / SUM

*  Sort these values into increasing order.
                  CALL KPG1_IS3D( WRK1, WRK2, POINT, NGOOD, STATUS )

*  Find the weighted median.
                  CALL KPG1_WTM3D( WRK1, WRK2, SVAR, NGOOD,
     :                      COVEC( 1, NGOOD ), DOUT( I0, J0 ), NEWVAR,
     :                      STATUS )

                  IF( VAR ) VOUT( I0, J0 ) = NEWVAR

*  Increment the number of modified pixels.
                  NREP = NREP + 1

               END IF

            END IF

         END IF

      END DO

      END
