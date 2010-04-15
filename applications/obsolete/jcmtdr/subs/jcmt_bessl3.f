      SUBROUTINE JCMT_BESSEL_REGRID_3 (FBAD, PIXSPACE, NI, NJ,
     :   ICEN, JCEN, XCEN, YCEN, TOT_WEIGHT_IN, CONV_SUM,
     :   CONV_WEIGHT, STATUS)
*+
*  Name:
*     JCMT_BESSEL_REGRID_3

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_BESSEL_REGRID_3 (FBAD, PIXSPACE, NI, NJ,
*    :   ICEN, JCEN, XCEN, YCEN, TOT_WEIGHT_IN, CONV_SUM,
*    :   CONV_WEIGHT, STATUS)

*  Description:

*  Arguments:
*     FBAD = REAL (Given)
*        Value signalling bad pixel.
*     PIXSPACE = DOUBLE PRECISION (Given)
*        the pixel spacing of the output pixels.
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

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'

*  Arguments Given:
      REAL FBAD
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
      INTEGER ICONV, JCONV                       ! loop counters
      INTEGER IFAIL                              ! PDA routine IFAIL
      DOUBLE PRECISION XINC                      ! x-axis pixel increment
      DOUBLE PRECISION YINC                      ! y-axis pixel increment
      DOUBLE PRECISION WTFN (WTPIXMAX * WTRESOL) ! weighting function
      DOUBLE PRECISION WT                        ! value of weighting function
                                                 ! at output pixel
      DOUBLE PRECISION XPIX, YPIX                ! position of output pixel
      DOUBLE PRECISION XCONV, YCONV              ! position of convolution
                                                 !   function pixel
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

*  now do the convolution

      DO JOUT = 1, NJ
         DO IOUT = 1, NI

            IF (TOT_WEIGHT_IN (IOUT,JOUT) .NE. 0.0) THEN

*  OK, good map point, loop through other map points in convolution
*  function range ...

               YPIX = (JOUT-JCEN) * YINC + YCEN
               XPIX = (IOUT-ICEN) * XINC + XCEN

               DO JCONV = MAX(1,JOUT-WTPIXMAX), MIN(NJ,JOUT+WTPIXMAX)
                  DO ICONV = MAX(1,IOUT-WTPIXMAX), MIN(NI,IOUT+WTPIXMAX)

*  and if they have zero weight, add them into the convolution function as
*  if they were measured to be zero

                     IF (TOT_WEIGHT_IN(ICONV,JCONV) .EQ. 0.0) THEN
                        YCONV = (JCONV-JCEN) * YINC + YCEN
                        XCONV = (ICONV-ICEN) * XINC + XCEN
                        RPIX = SQRT ((YPIX-YCONV)**2 + (XPIX-XCONV)**2)
                        RPIX = RPIX / PIXSPACE
                        ICPIX = NINT (RPIX * WTRESOL) + 1
                        IF ((ICPIX.GE.1).AND.
     :                      (ICPIX.LE.WTPIXMAX*WTRESOL)) THEN
                           WT = WTFN (ICPIX)
                        ELSE
                           WT = 0.0D0
                        END IF
                        CONV_WEIGHT(IOUT,JOUT) = CONV_WEIGHT(IOUT,JOUT)
     :                     + WT
                     END IF

                  END DO
               END DO

            END IF

         END DO
      END DO

*  finally go through output pixels normalising them by CONV_WEIGHT
*  to give the final result. Those whose `total input weight' is zero
*  are set to FBAD

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            IF (TOT_WEIGHT_IN (IOUT,JOUT) .EQ. 0.0) THEN
               CONV_SUM (IOUT,JOUT) = FBAD
            ELSE
               IF (CONV_WEIGHT (IOUT,JOUT) .NE. 0.0) THEN
                  CONV_SUM (IOUT,JOUT) = CONV_SUM (IOUT,JOUT) /
     :               CONV_WEIGHT (IOUT,JOUT)
               ELSE
                  CONV_SUM (IOUT,JOUT) = FBAD
               END IF
            END IF
         END DO
      END DO

      END
