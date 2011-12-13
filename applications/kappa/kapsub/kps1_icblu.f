      SUBROUTINE KPS1_ICBLU( ICF, FFT, WORK1, WORK2, DATA, STATUS )
*+
*  Name:
*     KPS1_ICBLU

*  Purpose:
*     Blurs the data with a Gaussian ICF.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL KPS1_ICBLU( ICF, FFT, WORK1, WORK2, DATA, STATUS )

*  Description:
*     An image holding a Gaussian is formed in a work array.  Its FFT
*     is multiplied by the FFT of the input data (or by the input data
*     itself if the input data is an FFT).  Unless the output is
*     required in the form of an FFT, the inverse FFT is taken and
*     returned.

*  Arguments:
*     ICF = REAL (Given)
*        The FWHM of the Gaussian ICF, in pixels.
*     FFT = LOGICAL (Given)
*        If .TRUE., then the DATA array is taken to contain an FFT.  If
*        .FALSE., then it is taken to contain an actual image.
*     WORK1( C1_NPX, C1_NLN ) = REAL (Given)
*        Work space.
*     WORK2( C1_NPX, C1_NLN ) = REAL (Given)
*        Work space.
*     DATA( C1_NPX, C1_NLN ) = REAL (Given and Returned)
*        On entry, the data to be blurred.  On exit, the blurred data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Name changed from ICBLUR to KPS1_ICBLU
*     22-FEB-1995 (DSB):
*        Comments re-formatted.  Use of NAG removed.
*     1995 April 7 (MJC):
*        Minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS.
*        C1_NPX = INTEGER (Read)
*           The X dimension of all internal images (including margin).
*        C1_NLN = INTEGER (Read)
*           The Y dimension of all internal images (including margin).

*  Arguments Given:
      REAL     ICF
      LOGICAL  FFT
      REAL     WORK1( C1_NPX, C1_NLN )
      REAL     WORK2( C1_NPX, C1_NLN )

*  Arguments Given and Returned:
      REAL     DATA( C1_NPX, C1_NLN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL AA                    ! Constant defining Gaussian
      PARAMETER ( AA = 2.77259 )

*  Local Variables:
      REAL     A                 ! Constant defining the Gaussian
      REAL     EXPVAL            ! Current ICF value
      REAL     FACTOR            ! Normalization factor
      INTEGER  LIN               ! Line counter in output frame
      INTEGER  PIX               ! Pixel counter in output frame
      REAL     SUM               ! Sum of ICF values
      INTEGER  X                 ! Gaussian centred coordinate
      INTEGER  XHI               ! Highest value of x which will fit in
                                 ! the output image
      INTEGER  XLO               ! Lowest value of x which will fit in
                                 ! the output image
      INTEGER  Y                 ! Gaussian centred coordinate
      REAL     Y2                ! Y squared
      INTEGER  YHI               ! Highest value of y which will fit in
                                 ! the output image
      INTEGER  YLO               ! Lowest value of y which will fit in
                                 ! the output image
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  If a negative or zero width was requested for the ICF, return
*  immediately without changing the data.
      IF( ICF .LE. 0.0) RETURN

*  Calculate the constant to use when generating the Gaussian which
*  will give the required FWHM.
      A = - AA / ( ICF * ICF )

*  (X,Y) values are centre on the centre of the Gaussian.  Calculate
*  the limits of (X,Y) which will fit into an internal image.
      XLO = - C1_NPX/2
      XHI = ( C1_NPX - 1 )/2

      YLO = - C1_NLN/2
      YHI = ( C1_NLN - 1 )/2

*  Generate the Gaussian with its centre at pixel (1,1), in wrap-around
*  format.  Find the total data sum in the ICF at the same time.
      SUM = 0.0

      DO Y = YLO, YHI
         LIN = Y + 1
         IF ( LIN .LE. 0 ) LIN = LIN + C1_NLN
         Y2 = REAL( Y * Y )

         DO X = XLO, XHI
            PIX = X + 1
            IF ( PIX .LE. 0 ) PIX = PIX + C1_NPX

            EXPVAL = EXP( A * ( REAL( X * X ) + Y2 ) )
            WORK1( PIX, LIN ) = EXPVAL
            SUM = SUM + EXPVAL

         END DO

      END DO

*  Normalize the ICF to have a total data sum equal to the square root
*  of the number of pixels in the image.  This ensures that the
*  zero-frequency pixel will have value 1.0 when the FFT is taken.  (See
*  NAG manual Chapter C06).
      FACTOR = SQRT( REAL( C1_NPX * C1_NLN ) ) / SUM

      DO LIN = 1, C1_NLN
         DO PIX = 1, C1_NPX
            WORK1( PIX, LIN ) = FACTOR * WORK1( PIX, LIN )
         END DO
      END DO

*  Take the forward FFT of the ICF image, storing the result back in
*  the WORK1 image.
      CALL KPG1_FFTFR( C1_NPX, C1_NLN, WORK1, WORK2, WORK1, STATUS )

*  Unless the input data is already in FFT form, take its FFT.
      IF ( .NOT. FFT ) THEN
         CALL KPG1_FFTFR( C1_NPX, C1_NLN, DATA, WORK2, DATA, STATUS )
      END IF

*  Multiply the FFT of the ICF with the data FFT.
      CALL KPG1_HMLTR( C1_NPX, C1_NLN, DATA, WORK1, DATA, STATUS )

*  Unless the output data is required as an FFT, take the inverse FFT of
*  the product.
      IF ( .NOT. FFT ) THEN
         CALL KPG1_FFTBR( C1_NPX, C1_NLN, DATA, WORK2, DATA, STATUS )
      END IF

      END
