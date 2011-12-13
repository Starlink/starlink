      SUBROUTINE KPS1_GAUPS( FWHM, WORK, OUT, STATUS )
*+
*  Name:
*     KPS1_GAUPS

*  Purpose:
*     Gets the FFT of a Gaussian PSF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GAUPS( FWHM, WORK, OUT, STATUS )

*  Description:
*     An image is created holding a Gaussian centred on pixel (1,1) in
*     wrap-around format.  It is normalised so that when its FFT is
*     used to smooth an image, the smoothed image flux is conserved.
*     The FFT is then formed in Hermitian format.  If a negative or
*     zero FWHM is requested then the PSF used consists of a delta
*     function at pixel (1,1) (i.e. the PSF would introduce no
*     smoothing if used).

*  Arguments:
*     FWHM = REAL (Given)
*        The Full Width at Half Maximum of the Gaussian PSF (in pixels).
*     WORK( C1_NPX, C1_NLN ) = REAL (Returned)
*        A work array.
*     OUT( C1_NPX, C1_NLN ) = REAL (Returned)
*        The output FFT in Hermitian form.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-1990 (DSB):
*        Original version.
*     27-FEB-1991 (DSB):
*        Name changed from GAUPSF to KPS1_GAUPS
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
      REAL  FWHM
      REAL  WORK( C1_NPX, C1_NLN )

*  Arguments Returned:
      REAL  OUT( C1_NPX, C1_NLN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL AA                    ! Constant defining Gaussian
      PARAMETER ( AA = 2.77259 )

*  Local Variables:
      REAL     A                 ! Constant defining the Gaussian
      REAL     EXPVAL            ! Current PSF value
      REAL     FACTOR            ! Normalization factor
      INTEGER  LIN               ! Line counter in output frame
      INTEGER  PIX               ! Pixel counter in output frame
      REAL     SUM               ! Sum of all PSF values
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
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the constant to use when generating the Gaussian which
*  will give the required FWHM.
      IF ( FWHM .GT. 0.0 ) THEN
         A = - AA / ( FWHM * FWHM )

*  (X,Y) values are centre on the centre of the Gaussian.  Calculate the
*  limits of (X,Y) which will fit into an internal image.
         XLO = - C1_NPX/2
         XHI = ( C1_NPX - 1 ) / 2

         YLO = - C1_NLN/2
         YHI = ( C1_NLN - 1 ) / 2

*  Generate the Gaussian with its centre at pixel (1,1), in wrap-around
*  format.  Find the total data sum in the PSF at the same time.
         SUM = 0.0

         DO Y = YLO, YHI
            LIN = Y + 1
            IF ( LIN .LE. 0 ) LIN = LIN + C1_NLN
            Y2 = REAL( Y * Y )

            DO X = XLO, XHI
               PIX = X + 1
               IF ( PIX .LE. 0 ) PIX = PIX + C1_NPX

               EXPVAL = EXP( A * ( REAL( X * X ) + Y2 ) )

               OUT( PIX, LIN ) = EXPVAL
               SUM = SUM + EXPVAL

            END DO

         END DO

*  Normalize the PSF to have a total data sum equal to the square root
*  of the number of pixels in the image.  This ensures that the
*  zero-frequency pixel will have value 1.0 when the FFT is taken.
         FACTOR = SQRT( REAL( C1_NPX * C1_NLN ) ) / SUM

         DO LIN = 1, C1_NLN
            DO PIX = 1, C1_NPX
               OUT( PIX, LIN ) = FACTOR * OUT( PIX, LIN )
            END DO
         END DO

*  If a zero or negative FWHM was given, give a mesage and return a PSF
*  which produces no smoothing.
      ELSE

         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_OUT( 'REPORT', '  Requested PSF has zero width',
     :                  STATUS )
         CALL MSG_OUT( 'REPORT', ' ', STATUS )

         DO LIN = 1, C1_NLN
            DO PIX = 1, C1_NPX
               OUT( PIX, LIN ) = 0.0
            END DO
         END DO

         OUT( 1, 1 ) = SQRT( REAL( C1_NPX * C1_NLN ) )

      END IF

*  Take the forward FFT of the PSF image, storing the result back in
*  the OUT image.
      CALL KPG1_FFTFR( C1_NPX, C1_NLN, OUT, WORK, OUT, STATUS )

      END
