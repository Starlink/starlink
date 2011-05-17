      SUBROUTINE KPS1_LUCFP( DIM1, DIM2, PSF, NPIX, NLIN, XCEN,
     :                       YCEN, OUT, WORK, STATUS )
*+
*  Name:
*     KPS1_LUCFP

*  Purpose:
*     Gets the FFT of the given PSF for Richardson-Lucy reconstruction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCFP( DIM1, DIM2, PSF, NPIX, NLIN, XCEN, YCEN,
*                      OUT, WORK, STATUS )

*  Description:
*     The input image is shifted so that the given PSF centre is
*     located at pixel (1,1), and negative co-ordinate values are
*     wrapped around to the other side of the image.  If the PSF is
*     smaller than the output image size, then it is padded with zeros.
*     The PSF is normalized so that the FFT convolution routine will
*     not change the total data sum of the image being convolved with
*     the PSF.  The FFT is taken and is returned in the form of a
*     Hermitian FFT.  N.B., it is assumed that there are no
*     invalid pixels in the PSF.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        Number of pixels per line in the PSF image.
*     DIM2 = INTEGER (Given)
*        Number of lines in the PSF image.
*     PSF( DIM1, DIM2 ) = REAL (Given)
*        The input PSF image.
*     NPIX = INTEGER (Given)
*        Number of pixels per line in the work array.
*     NLIN = INTEGER (Given)
*        Number of lines in the work array.
*     XCEN = INTEGER (Given)
*        X pixel index of the centre of the PSF.
*     YCEN = INTEGER (Given)
*        Y pixel index of the centre of the PSF.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The Hermitian FFT of the PSF.
*     WORK( NPIX, NLIN ) = REAL (Returned)
*        A work array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1995 (DSB):
*        Original version.
*     1995 April 6 (MJC):
*        Minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER  DIM1
      INTEGER  DIM2
      REAL     PSF( DIM1, DIM2 )
      INTEGER  NPIX
      INTEGER  NLIN
      INTEGER  XCEN
      INTEGER  YCEN

*  Arguments Returned:
      REAL     OUT( NPIX, NLIN )
      REAL     WORK( NPIX, NLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL     FACTOR            ! Normalisation factor
      INTEGER  LIN               ! Line counter
      INTEGER  OLIN              ! Line counter in output frame
      INTEGER  OLINHI            ! Highest value of OLIN (before wrap
                                 ! around) which can be stored in the
                                 ! output image
      INTEGER  OLINLO            ! Lowest value of OLIN (before wrap
                                 ! around) which can be stored in the
                                 ! output image
      INTEGER  OPIX              ! Pixel counter in output frame.
      INTEGER  OPIXHI            ! Highest value of OPIX (before wrap
                                 ! around) which can be stored in the
                                 ! output image
      INTEGER  OPIXLO            ! Lowest value of OPIX (before wrap
                                 ! around) which can be stored in the
                                 ! output image
      INTEGER  PIX               ! Pixel counter
      REAL     PSFVAL            ! Current PSF value
      REAL     SUM               ! Sum of input PSF values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the whole output image to zero.
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = 0.0
         END DO
      END DO

*  Copy the PSF to the output image, incorporating a shift of origin
*  such that the centre of the PSF goes to pixel (1,1).  Pixels which
*  have negative co-ordinates as a result of this shift are `wrapped
*  around' to the opposite size of the output image.  Find the total
*  data sum in the PSF at the same time.
      SUM = 0.0

*  Set the limits of the output image co-ordinates (before wrap-around
*  correction is applied to remove negative co-ordinates).
      OPIXLO = ( 1 - NPIX ) / 2
      OPIXHI = NPIX / 2

      OLINLO = ( 1 - NLIN ) / 2
      OLINHI = NLIN / 2

*  Loop through all the lines of the input PSF.
      DO LIN = 1, DIM2

*  Calculate the line number in the output image at which this input
*  line is stored.  The resulting value may be negative.
         OLIN = LIN - YCEN +1

*  Limit the area of the input PSF which is used, to be not greater than
*  the area of the output image.
         IF ( OLIN .GE. OLINLO .AND. OLIN .LE. OLINHI ) THEN

*  If the line number is negative wrap it round to the other side of
*  the output image.
            IF ( OLIN .LE. 0 ) OLIN = OLIN + NLIN

*  Do the same for the pixel number.
            DO PIX = 1, DIM1

               OPIX = PIX - XCEN +1
               IF ( OPIX .GE. OPIXLO .AND. OPIX .LE. OPIXHI ) THEN

                  IF ( OPIX .LE. 0 ) OPIX = OPIX + NPIX

*  Copy the input PSF value to the correct place in the output image,
*  and increment the total data sum.
                  PSFVAL = PSF( PIX, LIN )
                  OUT( OPIX, OLIN ) = PSFVAL
                  SUM = SUM + PSFVAL

               END IF

            END DO

         END IF

      END DO

*  If the PSF had zero data sum, abort.
      IF ( SUM .EQ. 0.0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LUCFP_ERR1', 'PSF has zero data sum.',
     :                  STATUS )
         GO TO 999
      END IF

*  Normalize the PSF to have a total data sum equal to the square root
*  of the number of pixels in the image.  This ensures that the
*  zero-frequency pixel will have value 1.0 when the FFT is taken.
      FACTOR = SQRT( REAL( NPIX * NLIN ) ) / SUM

      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = FACTOR * OUT( PIX, LIN )
         END DO
      END DO

*  Take the forward FFT of the PSF image, storing the result back in
*  the OUT image.
      CALL KPG1_FFTFR( NPIX, NLIN, OUT, WORK, OUT, STATUS )

 999  CONTINUE

      END
