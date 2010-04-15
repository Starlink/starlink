      SUBROUTINE JCMT_BESSEL_REGRID_2 (ZIN, WEIGHT_IN, FBAD, X, Y, NPIX,
     :   PIXSPACE, NI, NJ, ICEN, JCEN, XCEN, YCEN, TOT_WEIGHT_IN,
     :   CONV_SUM, CONV_WEIGHT, STATUS)
*+
*  Name:
*     JCMT_BESSEL_REGRID_2

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE JCMT_BESSEL_REGRID_2 (ZIN, WEIGHT_IN, FBAD, X, Y, NPIX,
*    :   PIXSPACE, NI, NJ, ICEN, JCEN, XCEN, YCEN, TOT_WEIGHT_IN,
*    :   CONV_SUM, CONV_WEIGHT, STATUS)

*  Description:

*  Arguments:
*     ZIN (NPIX) = REAL (Given)
*        The input data values.
*     WEIGHT_IN = REAL (Given)
*        The weight of this dataset.
*     FBAD = REAL (Given)
*        Value signalling bad pixel.
*     X (NPIX) = DOUBLE PRECISION (Given)
*        The x coordinates of the input pixels.
*     Y (NPIX) = DOUBLE PRECISION (Given)
*        The y coordinates of the input pixels.
*     NPIX = INTEGER (Given)
*        the number of input pixels.
*     PIXSPACE = DOUBLE PRECISION (Given)
*        the pixel spacing of the output map.
*     NI = INTEGER (Given)
*        The number of output pixels in the x direction.
*     NJ = INTEGER (Given)
*        The number of output pixels in the y direction.
*     ICEN = INTEGER (Given)
*        the x index of the centre of the output array.
*     JCEN = INTEGER (Given)
*        the y index of the centre of the output array.
*     XCEN = DOUBLE PRECISION (Given)
*        the x coordinate of the centre of the output array.
*     YCEN = DOUBLE PRECISION (Given)
*        the y coordinate of the centre of the output array.
*     TOT_WEIGHT_IN (NI, NJ) = REAL (Given)
*        the `total weight' of each output pixel.
*     CONV_SUM (NI,NJ) = REAL (Given and returned)
*        the convolution sum for each output pixel.
*     CONV_WEIGHT (NI,NJ) = REAL (Given and returned)
*        the convolution weight for each output pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*     8-OCT-1991 (REVAD::JFL): Original version.
*    11-NOV-1991 (REVAD::JFL): Fixed to work out x offsets with cos(dec)
*                              effect taken into account.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'

*  Arguments Given:
      INTEGER NPIX
      REAL ZIN (NPIX)
      REAL WEIGHT_IN
      REAL FBAD
      DOUBLE PRECISION X(NPIX)
      DOUBLE PRECISION Y(NPIX)
      DOUBLE PRECISION PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      DOUBLE PRECISION XCEN
      DOUBLE PRECISION YCEN
      REAL TOT_WEIGHT_IN (NI,NJ)

*  Arguments Returned:
      REAL CONV_SUM (NI, NJ)
      REAL CONV_WEIGHT (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:
      DOUBLE PRECISION PDA_DBESJ1                ! NAG Bessel function

*  Local Constants:
      INTEGER WTRESOL                            ! number of subpixel
      PARAMETER (WTRESOL = 200)                  ! interpolations of the
                                                 ! weighting function
      INTEGER WTPIXMAX                           ! maximum number of pixels
      PARAMETER (WTPIXMAX = 7)                   ! that the weighting function
                                                 ! extends to

*  Local Variables:
      INTEGER I                                  ! loop counter
      INTEGER ISTART
      INTEGER IOUT, JOUT                         ! loop counters
      INTEGER PIX                                ! current input pixel number
      INTEGER INEAR                              ! x index of nearest output
                                                 !   pixel
      INTEGER JNEAR                              ! y index of nearest output
                                                 !   pixel
      INTEGER IFAIL                              ! PDA routine IFAIL
      DOUBLE PRECISION XINC                      ! x-axis pixel increment
      DOUBLE PRECISION YINC                      ! y-axis pixel increment
      DOUBLE PRECISION WTFN (WTPIXMAX * WTRESOL) ! weighting function
      DOUBLE PRECISION WT                        ! value of weighting function
                                                 ! at output pixel
      DOUBLE PRECISION XPIX, YPIX                ! position of output pixel
      DOUBLE PRECISION XIN, YIN                  ! position of input pixel
      DOUBLE PRECISION RPIX                      ! distance of current output
                                                 ! pixel from current input pixel
      INTEGER ICPIX                              ! position in convolution
                                                 ! function array corresponding
                                                 ! to RPIX
      DOUBLE PRECISION XX                        ! argument of PDA_DBESJ1

*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

*  Weighting function used is the function (I/I0 = 2*J1(x)/x), with
*  a cosine apodization to zero over the last third of its extent.
*  It is tabulated up to WTPIXMAX pixels out at a resolution of 1/WTRESOL
*  of a pixel. The FWHM of the function is such that it would have
*  been fully sampled by a rectangular mesh with sample points separated
*  by the input pixel spacing. The width, WTPIXMAX and WTRESOL are all
*  measured in units of input pixels.

*  ..basic function

      IFAIL = 0
      WTFN(1) = 1.0D0
      DO I = 2, WTPIXMAX*WTRESOL
         XX = DBLE(I-1) * DPI / DBLE(WTRESOL)
         WTFN (I) = 2.0D0 * PDA_DBESJ1(XX, IFAIL) / XX
      END DO

*  ..apodization

      ISTART = NINT (WTPIXMAX*WTRESOL*0.66)
      DO I = ISTART, WTPIXMAX*WTRESOL
         WTFN (I) = WTFN (I) *
     :   COS (DBLE(I-ISTART)*DPI/(2.0D0*DBLE(WTPIXMAX*WTRESOL-ISTART)))
      END DO

*  now do the convolution, looping over the input pixels

      DO PIX = 1, NPIX

         IF (ZIN(PIX) .NE. FBAD) THEN

*  find the co-ords (INEAR, JNEAR) of the output pixel nearest to the current
*  input pixel. X offset is corrected for cos(dec) effect.

            XIN = (X(PIX) - XCEN) * COS (Y(PIX))
            INEAR = NINT (XIN/XINC) + ICEN
            JNEAR = NINT ((Y(PIX)-YCEN)/YINC) + JCEN

*  loop over x's and y's in output array that are covered by the convolution
*  function centred at (INEAR, JNEAR)

            DO JOUT = MAX(1,JNEAR-WTPIXMAX), MIN(NJ,JNEAR+WTPIXMAX)
               DO IOUT = MAX(1,INEAR-WTPIXMAX), MIN(NI,INEAR+WTPIXMAX)

*  work out x,y offset of current output pixel

                  YPIX = DBLE (JOUT-JCEN) * YINC
                  XPIX = DBLE (IOUT-ICEN) * XINC

*  x, y offset of current input pixel, x-offset corrected for cos(dec) effect

                  YIN = Y(PIX) - YCEN
                  XIN = (X(PIX) - XCEN) * COS (Y(PIX))

*  distance between them, in units of average input pixel
*  spacing which is the unit of the interpolation function

                  RPIX = SQRT ((YPIX-YIN)**2 + (XPIX-XIN)**2)
                  RPIX = RPIX / PIXSPACE

*  work out the appropriate value of the interpolation function

                  ICPIX = NINT (RPIX * WTRESOL) + 1
                  IF ((ICPIX.GE.1).AND.(ICPIX.LE.WTPIXMAX*WTRESOL)) THEN
                     WT = WTFN (ICPIX)
                  ELSE
                     WT = 0.0D0
                  END IF

*  add into the convolution result and weight arrays unless TOT_WEIGHT_IN
*  of output is zero, signifying output pixel is beyond limits of
*  mapped area. The coaddition is normalised by the `total weight'
*  associated with this input pixel.

                  IF (TOT_WEIGHT_IN (IOUT,JOUT) .NE. 0.0) THEN
                     CONV_WEIGHT (IOUT,JOUT) = CONV_WEIGHT(IOUT,JOUT) +
     :                  WT * WEIGHT_IN / TOT_WEIGHT_IN (INEAR,JNEAR)
                     CONV_SUM (IOUT,JOUT) = CONV_SUM (IOUT,JOUT) +
     :                  WT * ZIN(PIX) * WEIGHT_IN /
     :                  TOT_WEIGHT_IN (INEAR,JNEAR)
                  END IF

               END DO
            END DO

         END IF

      END DO

      END
