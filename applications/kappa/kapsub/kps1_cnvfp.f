      SUBROUTINE KPS1_CNVFP( NORM, VAR, NXP, NYP, PSF, NPIX, NLIN, XCEN,
     :                       YCEN, OUT, STATUS )
*+
*  Name:
*     KPS1_CNVFP

*  Purpose:
*     Prepares the given PSF for use in CONVOLVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNVFP( NORM, VAR, NXP, NYP, PSF, NPIX, NLIN, XCEN, YCEN,
*                      OUT, STATUS )

*  Description:
*     A constant background value is subtracted from the supplied PSF.
*     This value is the minimum of the pixel values in the four corners
*     of the supplied PSF.  The PSF is then shifted so that the given
*     PSF centre is located at pixel (1,1), and negative co-ordinate
*     values are wrapped around to the other side of the image.  If the
*     PSF is smaller than the output image size, then it is padded with
*     zeros.  If the PSF is to be used to smooth a VARIANCE array, then
*     the returned PSF values are squared.

*  Arguments:
*     NORM = LOGICAL (Given)
*        Normalise the PSF to a data sum of unity before using?
*     VAR = LOGICAL (Given)
*        .TRUE. if the VARIANCE component is being smoothed.
*     NXP = INTEGER (Given)
*        Number of pixels per line in the supplied PSF image.
*     NYP = INTEGER (Given)
*        Number of lines in the supplied PSF image.
*     PSF( NXP, NYP ) = DOUBLE PRECISION (Given)
*        The PSF image.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the internal images,
*        including a margin to reduce wrap-around effects.
*     NLIN = INTEGER (Given)
*        The number of lines in the internal images, including a margin
*        to reduce wrap-around effects.
*     XCEN = INTEGER (Given)
*        X pixel index of the centre of the PSF.
*     YCEN = INTEGER (Given)
*        Y pixel index of the centre of the PSF.
*     OUT( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        The prepared PSF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     10-JAN-1995 (DSB):
*        Original version, based on KPS1_MEMFP.
*     1995 March 22 (MJC):
*        Made some stylistic changes, removed long lines and used
*        modern-style variable declarations.
*     1995 April 4 (DSB):
*        Modified to normalise the total data sum in the PSF to 1.
*     26-MAY-2010 (DSB):
*        Added argument NORM.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL  NORM
      LOGICAL  VAR
      INTEGER  NXP
      INTEGER  NYP
      DOUBLE PRECISION PSF( NXP, NYP )
      INTEGER  NPIX
      INTEGER  NLIN
      INTEGER  XCEN
      INTEGER  YCEN

*  Arguments Returned:
      DOUBLE PRECISION OUT( NPIX, NLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BACK      ! Background value within the supplied
                                 ! psf
      DOUBLE PRECISION FACTOR    ! Normalisation factor
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
      DOUBLE PRECISION SUM       ! Total data sum in PSF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the whole output image to zero.
      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = 0.0D0
         END DO
      END DO

*  Form a `background value'.  This is the minimum absolute value in
*  the four corners of the supplied PSF.
      BACK = MIN( ABS( PSF( 1, 1 ) ), ABS( PSF( NXP, 1 ) ),
     :            ABS( PSF( 1, NYP ) ), ABS( PSF( NXP, NYP ) ) )

*  Copy the PSF to the output image, incorporating a shift of origin
*  such that the centre of the PSF goes to pixel (1,1).  Pixels which
*  have negative co-ordinates as a result of this shift are `wrapped
*  around' to the opposite size of the output image.  Also find the
*  total data sum in the PSF.
      SUM = 0.0D0

*  Set the bounds of the output image (before wrap-around correction is
*  applied to remove negative co-ordinates).
      OPIXLO = ( 1 - NPIX ) / 2
      OPIXHI = NPIX / 2

      OLINLO = ( 1 - NLIN ) / 2
      OLINHI = NLIN / 2

*  Loop through all the lines of the input PSF.
      DO LIN = 1, NYP

*  Calculate the line number in the output image at which this input
*  line is stored.  The resulting value may be negative.
         OLIN = LIN - YCEN +1

*  Limit the area of the input PSF which is used, to be not greater
*  than the area of the output image.
         IF ( OLIN .GE. OLINLO .AND. OLIN .LE. OLINHI ) THEN

*  If the line number is negative wrap it round to the other side of
*  the output image.
            IF ( OLIN .LE. 0 ) OLIN = OLIN + NLIN

*  Do the same for the pixel number.
            DO PIX = 1, NXP

               OPIX = PIX - XCEN +1
               IF ( OPIX .GE. OPIXLO .AND. OPIX .LE. OPIXHI ) THEN

                  IF ( OPIX .LE. 0 ) OPIX = OPIX + NPIX

*  Subtract the background value from the input PSF value and store it
*  in the correct place in the output image.
                  OUT( OPIX, OLIN ) = PSF( PIX, LIN ) - BACK
                  SUM = SUM + OUT( OPIX, OLIN )

               END IF

            END DO

         END IF

      END DO

*  If the PSF had zero data sum, abort.
      IF ( NORM .AND. SUM .EQ. 0.0D0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CNVFP_ERR1', 'PSF has zero data sum.',
     :                  STATUS )
         GO TO 999
      END IF

*  Normalise the PSF to have a total data sum equal to the square root
*  of the number of pixels in the image.  This ensures that the
*  zero-frequency pixel will have value 1.0 when the FFT is taken.
      FACTOR = SQRT( DBLE( NPIX * NLIN ) )
      IF( NORM ) FACTOR = FACTOR / SUM

      DO LIN = 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = FACTOR * OUT( PIX, LIN )
         END DO
      END DO

*  If the PSF is to be used to smooth a variance array, square the PSF
*  values.
      IF ( VAR ) THEN

         DO LIN = 1, NLIN
            DO PIX = 1, NPIX
               OUT( PIX, LIN ) = OUT( PIX, LIN )**2
            END DO
         END DO

      END IF

 999  CONTINUE

      END
