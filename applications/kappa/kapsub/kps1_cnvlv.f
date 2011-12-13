      SUBROUTINE KPS1_CNVLV( VAR, NX, NY, DATIN, VARIN, NXP, NYP, PSFIN,
     :                       XC, YC, NPIX, NLIN, WLIM, NORM, DATOUT,
     :                       VAROUT, BAD, ISTAT, W1, W2, W3, W4,
     :                       STATUS )
*+
*  Name:
*     KPS1_CNVLV

*  Purpose:
*     Convolves two supplied DATA and VARIANCE arrays, for CONVOLVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNVLV( VAR, NX, NY, DATIN, VARIN, NXP, NYP, PSFIN, XC,
*                      YC, NPIX, NLIN, WLIM, NORM, DATOUT, VAROUT, BAD,
*                      ISTAT, W1, W2, W3, W4, STATUS )

*  Description:
*     This routine convolves the supplied data array (and optionally
*     the variance array) with the supplied PSF array, by multiplying
*     the Fourier transforms of the two arrays.  The arrays are padded
*     with a margin before the FFTs are taken to reduce edge effects,
*     so that the padded arrays have the dimensions specified by NPIX
*     and NLIN.  The margins around the PSF array are considered to
*     hold the value zero, and the margins around the data (or
*     variance) array is filled by replicating the closest edge pixels.
*
*     Bad pixels in the data and variance arrays are handled in a
*     manner that depends on the supplied value for NORM. If NORM is
*     .FALSE., bad pixels are simply replaced by the mean value in the
*     array, and included in the convolution as normal. If NORM is
*     .TRUE., each pixel in the data (or variance) array has an
*     associated weight which is stored in a work array called the 'mask'
*     array (W3).  This weight is 1 if both the data AND the variance
*     values are good (the requirement for a good variance value is
*     removed if VAR is .FALSE.), and zero otherwise.  If a pixel has
*     zero weight then the data AND variance values are set to zero
*     before the FFT is taken.  The mask array is smoothed using the
*     same PSF as the data array, thus giving the total weight
*     associated with good input pixels at each output pixel.  The
*     smoothed data array is then normalised by dividing it by the
*     smoothed mask array.
*
*     The variance array is processed in a similar way to the data
*     array, except that the square of the supplied PSF is used, and
*     the smoothed variance array is normalised using the square of the
*     smoothed mask array (if NORM is .TRUE.).
*
*     The supplied PSF array must not contain any bad values.

*  Arguments:
*     VAR = LOGICAL (Given)
*        If .TRUE. then the variance array is to be smoothed as well as
*        the data array.  Otherwise, the VARIN and VAROUT arguments are
*        ignored.
*     NX = INTEGER (Given)
*        The number of columns in the supplied data and variance arrays.
*     NY = INTEGER (Given)
*        The number of rows in the supplied data and variance arrays.
*     DATIN( NX, NY ) = DOUBLE PRECISION (Given)
*        The input data array.
*     VARIN( NX, NY ) = DOUBLE PRECISION (Given)
*        The input variance array (ignored if VAR=.FALSE.).
*     NXP = INTEGER (Given)
*        The number of columns in the supplied PSF array.
*     NYP = INTEGER (Given)
*        The number of rows in the supplied PSF array.
*     PSFIN( NX, NY ) = DOUBLE PRECISION (Given)
*        The PSF array.  An error is reported if this array contains
*        any bad values.  A constant background is removed from this
*        array before being used.  This background level is the
*        smallest absolute value of the four corner pixels.
*     XC = INTEGER (Given)
*        The column index (within PSFIN) of the central pixel of the
*        PSF.
*     YC = INTEGER (Given)
*        The row index (within PSFIN) of the central pixel of the PSF.
*     NPIX = INTEGER (Given)
*        The number of columns in the work arrays.  This must be larger
*        than NX by twice the size of the required horizontal margin.
*     NLIN = INTEGER (Given)
*        The number of rows in the work arrays.  This must be larger
*        than NY by twice the size of the required vertical margin.
*     WLIM = REAL (Given)
*        The minimum value in the smoothed mask array for which valid
*        output pixels are created.  If any smoothed data value has an
*        associated smoothed mask value which is smaller than WLIM,
*        then the corresponding output pixel (i.e. DATOUT) is set bad.
*        If the supplied value of WLIM is negative, then the pattern of
*        bad pixels is propagated from DATIN.
*     NORM = LOGICAL (Given)
*        If .TRUE., the returned array is normalised to correct for the
*        presence of bad pixels in the input array. Otherwise, bad pixels
*        are replaced with the mean value of the data array, and included
*        in the convolution as normal.
*     DATOUT( NX, NY ) = DOUBLE PRECISION (Returned)
*        The output data array.
*     VAROUT( NX, NY ) = DOUBLE PRECISION (Returned)
*        The output variance array (ignored if VAR=.FALSE.).
*     BAD = LOGICAL (Returned)
*        Returned .TRUE. if any bad values are written to the output
*        arrays.  It is .FALSE. otherwise.
*     ISTAT = INTEGER (Returned)
*        0 - The arrays were processed successfully.
*        1 - All input pixels were bad.
*        2 - All output pixels were bad.
*     W1( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        Work array.
*     W2( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        Work array.
*     W3( NPIX, NLIN ) = DOUBLE PRECISION (Returned)
*        Work array. Only accessed if NORM is .TRUE.
*     W4( * ) = DOUBLE PRECISION (Returned)
*        Work array.  Must be at least ( 3 * MAX( NPIX, NLIN ) + 15 )
*        elements long.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
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
*     16-JAN-1995 (DSB):
*        Original version.
*     1995 March 22 (MJC):
*        Made some stylistic changes, removed long lines and used
*        modern-style variable declarations.
*     22-APR-2009 (DSB):
*        Added ISTAT argument.
*     26-MAY-2010 (DSB):
*        - Allow for negative PSF values.
*        - Add NORM argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      LOGICAL VAR
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION DATIN( NX, NY )
      DOUBLE PRECISION VARIN( NX, NY )
      INTEGER NXP
      INTEGER NYP
      DOUBLE PRECISION PSFIN( NXP, NYP )
      INTEGER XC
      INTEGER YC
      INTEGER NPIX
      INTEGER NLIN
      REAL WLIM
      LOGICAL NORM

*  Arguments Returned:
      DOUBLE PRECISION DATOUT( NX, NY )
      DOUBLE PRECISION VAROUT( NX, NY )
      LOGICAL BAD
      INTEGER ISTAT
      DOUBLE PRECISION W1( NPIX, NLIN )
      DOUBLE PRECISION W2( NPIX, NLIN )
      DOUBLE PRECISION W3( NPIX, NLIN )
      DOUBLE PRECISION W4( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION FAC       ! Correction factor for normalising VARIANCEs
      DOUBLE PRECISION INVAL     ! Input data or variance value
      DOUBLE PRECISION MEAN      ! mean array value
      DOUBLE PRECISION SUM       ! Sum of all good values
      DOUBLE PRECISION W         ! Weight for current output pixel
      INTEGER LIN                ! Row counter for external arrays
      INTEGER NGOOD              ! Number of good values in array
      INTEGER PIX                ! Column counter for external arrays
      INTEGER SHIFTX             ! X shift from external to internal arrays
      INTEGER SHIFTY             ! Y shift from external to internal arrays
      INTEGER WLIN               ! Row counter for internal (work) arrays
      INTEGER WPIX               ! Column counter for internal (work) arrays
      LOGICAL ALLBAD             ! Are all input pixels bad ?
      LOGICAL USE                ! Can the current input pixel be used?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      ISTAT = 0

*  Prepare the PSF for smoothing the DATA array, and store its FFT in
*  array W1.
      CALL KPS1_CNVFP( NORM, .FALSE., NXP, NYP, PSFIN, NPIX, NLIN, XC,
     :                 YC, W1, STATUS )
      CALL KPG1_FFTFD( NPIX, NLIN, W1, W4, W1, STATUS )

*  Find the pixel shifts which will centre the input and output NDF
*  arrays within the internal work arrays.
      SHIFTX = ( NPIX - NX ) / 2
      SHIFTY = ( NLIN - NY ) / 2

*  If we are not using a mask to normalised the returned values, we will
*  replace bad data values with the mean data value. So find the mean now
*  if required.
      MEAN = 0.0D0
      IF( .NOT. NORM ) THEN

         SUM = 0.0D0
         NGOOD = 0

         DO LIN = 1, NY
            DO PIX = 1, NX
               INVAL = DATIN( PIX, LIN )
               IF( INVAL .NE. VAL__BADD ) THEN
                  SUM = SUM + INVAL
                  NGOOD = NGOOD + 1
               END IF
            END DO
         END DO

         IF( NGOOD .GT. 0 ) MEAN = SUM / NGOOD

      END IF

*  Set a flag indicating that no good input DATA pixels have yet been
*  found.
      ALLBAD = .TRUE.

*  Copy the input DATA array to W2, so that it is centred within W2.
*  Only use pixels which have good DATA and (if available) VARIANCE
*  values (other DATA values are set to zero or the mean). If required,
*  also create a mask image in W3 which is 1 for every pixel with a good
*  DATA and (if available) VARIANCE value and zero everywhere else.
      DO LIN = 1, NY
         WLIN = LIN + SHIFTY

         DO PIX = 1, NX
            WPIX = PIX + SHIFTX

            INVAL = DATIN( PIX, LIN )

            USE = INVAL .NE. VAL__BADD
            IF ( VAR ) USE = USE .AND.
     :                       ( VARIN( PIX, LIN ) .NE. VAL__BADD )

            IF( NORM ) THEN
               IF ( USE ) THEN
                  W2( WPIX, WLIN ) = INVAL
                  W3( WPIX, WLIN ) = 1.0D0
                  ALLBAD = .FALSE.
               ELSE
                  W2( WPIX, WLIN ) = 0.0D0
                  W3( WPIX, WLIN ) = 0.0D0
               END IF

            ELSE
               IF ( USE ) THEN
                  W2( WPIX, WLIN ) = INVAL
                  ALLBAD = .FALSE.
               ELSE
                  W2( WPIX, WLIN ) = MEAN
               END IF
            END IF

         END DO

      END DO

*  Fill the output arrays with bad values and return with ISTAT set to
*  1 if all input pixels are bad.
      IF ( ALLBAD .AND. STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_FILLD( VAL__BADD, NX*NY, DATOUT, STATUS )
         IF( VAR ) CALL KPG1_FILLD( VAL__BADD, NX*NY, VAROUT, STATUS )
         ISTAT = 1
         GO TO 999
      END IF

*  Replicate the edge pixel values into the margins for the DATA array
*  and the mask array.
      CALL KPS1_CNVRP( 1 + SHIFTX, NX + SHIFTX, 1 + SHIFTY, NY + SHIFTY,
     :                 NPIX, NLIN, W2, STATUS )
      IF( NORM ) CALL KPS1_CNVRP( 1 + SHIFTX, NX + SHIFTX, 1 + SHIFTY,
     :                            NY + SHIFTY, NPIX, NLIN, W3, STATUS )

*  Take the forward FFT of the DATA array, storing the result back in
*  the same array.
      CALL KPG1_FFTFD( NPIX, NLIN, W2, W4, W2, STATUS )

*  Multiply the FFT of the DATA array by the FFT of the PSF.  Store the
*  result back in W2.
      CALL KPG1_HMLTD( NPIX, NLIN, W2, W1, W2, STATUS )

*  Take the inverse FFT of the product to get the smoothed DATA array.
*  Store the results back in W2.
      CALL KPG1_FFTBD( NPIX, NLIN, W2, W4, W2, STATUS )

*  Now smooth the mask array in the same way, storing the results back
*  in W3.
      IF( NORM ) THEN
         CALL KPG1_FFTFD( NPIX, NLIN, W3, W4, W3, STATUS )
         CALL KPG1_HMLTD( NPIX, NLIN, W3, W1, W3, STATUS )
         CALL KPG1_FFTBD( NPIX, NLIN, W3, W4, W3, STATUS )
      END IF

*  Set a flag indicating that no good output DATA pixels have yet been
*  found.
      ALLBAD = .TRUE.

*  Set a flag indicating that no bad output DATA pixels have yet been
*  found.
      BAD = .FALSE.

*  Store the normalisation factor used if NORM = .FALSE.
      W = 1.0D0

*  Loop round each line of the output DATA array.  Form the index of
*  the corresponding line in the smoothed input DATA array (W2).
      DO LIN = 1, NY
         WLIN = LIN + SHIFTY

*  Loop round each pixel in this line.  Form the index of the
*  corresponding pixel in the smoothed input DATA array.
         DO PIX = 1, NX
            WPIX = PIX + SHIFTX

*  Store the smoothed mask value.
            IF( NORM ) W = W3( WPIX, WLIN )

*  The output pixel will be bad if the weight for this pixel is too
*  low, (if wlim is positive), or if the input DATA value was bad (if
*  wlim is negative).
            IF ( ( WLIM .GE. 0 .AND. W .LE. WLIM ) .OR.
     :           ( WLIM .LT. 0 .AND.
     :             DATIN( PIX, LIN ) .EQ. VAL__BADD ) ) THEN
               DATOUT( PIX, LIN ) = VAL__BADD
               BAD = .TRUE.

*  Otherwise, normalise the smoothed input value and store it in the
*  output.
            ELSE

               IF ( W .NE. 0.0D0 ) THEN
                  DATOUT( PIX, LIN ) = W2( WPIX, WLIN ) / W
                  ALLBAD = .FALSE.

               ELSE
                  DATOUT( PIX, LIN ) = VAL__BADD
                  BAD = .TRUE.

               END IF

            END IF

         END DO

      END DO

*  Fill any output variance array with bad values and return with ISTAT set
*  to 2 if all the output pixel values are bad.
      IF ( NORM .AND. ALLBAD ) THEN
         IF( VAR ) CALL KPG1_FILLD( VAL__BADD, NX*NY, VAROUT, STATUS )
         ISTAT = 2
         GO TO 999
      END IF

*  We now smooth the VARIANCE array, if required.
      IF ( VAR ) THEN

*  The variance array is smoothed with the square of the supplied PSF.
*  Prepare the squared PSF and store its FFT in W1.
         CALL KPS1_CNVFP( NORM, .TRUE., NXP, NYP, PSFIN, NPIX, NLIN, XC,
     :                    YC, W1, STATUS )
         CALL KPG1_FFTFD( NPIX, NLIN, W1, W4, W1, STATUS )

*  Find the mean variance value now if required.
         MEAN = 0.0D0
         IF( .NOT. NORM ) THEN

            SUM = 0.0D0
            NGOOD = 0

            DO LIN = 1, NY
               DO PIX = 1, NX
                  INVAL = VARIN( PIX, LIN )
                  IF( INVAL .NE. VAL__BADD ) THEN
                     SUM = SUM + INVAL
                     NGOOD = NGOOD + 1
                  END IF
               END DO
            END DO

            IF( NGOOD .GT. 0 ) MEAN = SUM / NGOOD

         END IF

*  Copy the input VARIANCE array to W2, so that it is centred within
*  W2.  If the VARIANCE or corresponding DATA value is bad, replace the
*  variance value with zero or the mean value.
         DO LIN = 1, NY
            WLIN = LIN + SHIFTY

            DO PIX = 1, NX
               WPIX = PIX + SHIFTX

               INVAL = VARIN( PIX, LIN )
               USE = ( INVAL .NE. VAL__BADD ) .AND.
     :               ( DATIN( PIX, LIN ) .NE. VAL__BADD )

               IF (  USE ) THEN
                  W2( WPIX, WLIN ) = INVAL
               ELSE IF( NORM ) THEN
                  W2( WPIX, WLIN ) = 0.0D0
               ELSE
                  W2( WPIX, WLIN ) = MEAN
               END IF

            END DO

         END DO

*  Replicate the edge pixel values into the margins.
         CALL KPS1_CNVRP( 1 + SHIFTX, NX + SHIFTX, 1 + SHIFTY,
     :                    NY + SHIFTY, NPIX, NLIN, W2, STATUS )

*  Smooth the VARIANCE array, storing the results back in W2.
         CALL KPG1_FFTFD( NPIX, NLIN, W2, W4, W2, STATUS )
         CALL KPG1_HMLTD( NPIX, NLIN, W2, W1, W2, STATUS )
         CALL KPG1_FFTBD( NPIX, NLIN, W2, W4, W2, STATUS )

*  Store the square root of the number of pixels in the work arrays.
*  This is needed because the process of smoothing using the FFTPACK
*  Hermitian routines, gives a result which is too small (compared to
*  the mathematical definition of convolution) by a constant factor
*  equal to the square root of the number of pixels in the array.  When
*  creating the DATA array this factor cancelled out when the smoothed
*  DATA array was divided by the smoothed mask array.  However, when
*  finding output VARIANCE Values the smoothed variance value is
*  normalised by THE SQUARE OF the smoothed mask array, and so the
*  factors no longer cancel.
         FAC = 1.0D0 / SQRT( DBLE( NPIX * NLIN ) )

*  Store the normalisation factor used if NORM = .FALSE.
         W = 1.0D0

*  Loop round each line of the output VARIANCE array.  Form the index
*  of the corresponding line in the smoothed input VARIANCE array (W2).
         DO LIN = 1, NY
            WLIN = LIN + SHIFTY

*  Loop round each pixel in this line.  Form the index of the
*  corresponding pixel in the smoothed input VARIANCE array.
            DO PIX = 1, NX
               WPIX = PIX + SHIFTX

*  Store the smoothed mask value.
               IF( NORM ) W = W3( WPIX, WLIN )

*  The output VARIANCE value will be bad iff the output DATA value was
*  bad.  Otherwise, normalise the smoothed variance value using the
*  square of the smoothed mask value.
               IF ( DATOUT( PIX, LIN ) .EQ. VAL__BADD ) THEN
                  VAROUT( PIX, LIN ) = VAL__BADD
               ELSE
                  VAROUT( PIX, LIN ) = FAC * W2( WPIX, WLIN ) / ( W**2 )
               END IF

            END DO

         END DO

      END IF

 999  CONTINUE

      END
