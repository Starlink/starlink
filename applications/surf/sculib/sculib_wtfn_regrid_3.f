      SUBROUTINE SCULIB_WTFN_REGRID_3 (USEGUARD, RES, PIXSPACE, NI,NJ,
     :     ICEN, JCEN, TOT_WEIGHT_IN, CONV_DATA_SUM,
     :     CONV_VARIANCE_SUM, CONV_QUALITY_SUM, CONV_WEIGHT,
     :     WEIGHTSIZE, SCLSZ, WTFN, STATUS)
*+
*  Name:
*     SCULIB_WTFN_REGRID_3

*  Purpose:
*     Sets up the 'guard ring' of bolometers outside the data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_WTFN_REGRID_3 (METHOD,RES,PIXSPACE, NI, NJ, ICEN, JCEN,
*    :  TOT_WEIGHT_IN, CONV_DATA_SUM, CONV_VARIANCE_SUM,
*    :  CONV_QUALITY_SUM, CONV_WEIGHT, WEIGHTSIZE, SCLSZ, WTFN, STATUS)

*  Description:
*     Takes the regridded image and adds in data of value 'zero'
*     for any points that are within range of the weighting function
*     (ie if TOTAL_WEIGHT is greater than 0) but that do not already
*     contain any data (ie TOTAL_WEIGHT is set to something becuase
*     regrid_1 determined that the pixel was close enough to be
*     influenced by the convolution function, BUT no data was actually
*     found to lie in that pixel). This has the effect of enforcing
*     zero flux density at the edges of the map or where no data
*     was present (eg surrounding a bad pixel).
*     This is only really necessary for the bessel function regrid since
*     that function is extremely sensitive to edge effects.
*     Once the 'guard ring' has been processed, the output image is
*     normalised by dividing by the convolution weight (ie the actual
*     weight used for each pixel). Any point that has zero convolution
*     weight is set to bad.


*  Arguments:
*     USEGUARD                       = LOGICAL (Given)
*       Logical to determine whether the gueard ring should be used.
*     RES                            = INTEGER (Given)
*        number of resolution elements per scale length
*     PIXSPACE                       = REAL (Given)
*        the pixel spacing of the output pixels (radians).
*     NI                             = INTEGER (Given)
*        The number of output pixels in the x direction.
*     NJ                             = INTEGER (Given)
*        The number of output pixels in the y direction.
*     ICEN                           = INTEGER (Given)
*        the x index of the centre of the output array.
*     JCEN                           = INTEGER (Given)
*        the y index of the centre of the output array.
*     TOT_WEIGHT_IN (NI, NJ)         = REAL (Given)
*        the `total weight' of each output pixel.
*     CONV_DATA_SUM (NI,NJ)          = REAL (Given and returned)
*        the convolution sum for each output pixel.
*     CONV_VARIANCE_SUM (NI,NJ)      = REAL (Given and returned)
*        the convolved variance sum for each output pixel.
*     CONV_QUALITY_SUM (NI,NJ)       = BYTE (Given)
*        the quality on the convolution sum for each output pixel.
*     CONV_WEIGHT (NI,NJ)            = REAL (Given and returned)
*        the convolution weight for each output pixel.
*     WEIGHTSIZE                       = INTEGER (Given)
*        radius of weight function in scale units (SCUIP__FILTRAD for BESSEL, 1 for LINEAR)
*     SCLSZ                          = REAL (Given)
*        1 scale length in the same units as pixspace
*     WTFN (RES * RES * WEIGHTSIZE * WEIGHTSIZE) = REAL (Given)
*        convolution weighting function
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     REVAD::JFL: John Lightfoot
*     Tim Jenness (JAC)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.9  2005/03/23 03:48:22  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     scale+weightsize throughout
*
*     Revision 1.8  1999/08/06 02:24:53  timj
*     Tweak headers for use with PROLAT.
*
*     15-AUG-1995: original version, modified from JCMT_BESSEL_REGRID_3

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      LOGICAL USEGUARD
      INTEGER RES
      REAL    PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      REAL    SCLSZ
      REAL    TOT_WEIGHT_IN (NI, NJ)
      INTEGER WEIGHTSIZE
      REAL    WTFN(RES * RES * WEIGHTSIZE * WEIGHTSIZE + 1)

*  Arguments Returned:
      REAL    CONV_DATA_SUM (NI, NJ)
      REAL    CONV_VARIANCE_SUM (NI, NJ)
      BYTE    CONV_QUALITY_SUM (NI, NJ)
      REAL    CONV_WEIGHT (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  Local Constants:

*  Local Variables:
      INTEGER ICONV                              ! I index in convolution
      INTEGER ICPIX                              ! index in convolution function
                                                 ! corresponding to RPIX
      INTEGER IOUT                               ! I pixel index in output array
      INTEGER JCONV                              ! J index in convolution
      INTEGER JOUT                               ! J pixel index in output array
      INTEGER PIX_RANGE                          ! radius over which convolution
                                                 ! function is none zero, in
                                                 ! pixels
      REAL    RES_ELEMENT                        ! critical sample spacing
                                                 ! at WAVELENGTH (radians)
      REAL    RPIX                               ! distance of pixel from centre
                                                 ! of convolution function
      REAL    RTEMP                              ! scratch real
      REAL    XCONV                              ! x coord of convolution
                                                 ! function pixel
      REAL    XINC                               ! x-axis pixel increment
      REAL    XPIX                               ! x coord of output pixel
      REAL    YCONV                              ! y coord of convolution
                                                 ! function pixel
      REAL    YINC                               ! y-axis pixel increment
      REAL    YPIX                               ! y coord of output pixel
      REAL    WT                                 ! value of weighting function
                                                 ! at output pixel
      INTEGER FILTER1_SQ
      INTEGER FILTER_RAD_SQ
      REAL    SCALE
      REAL    SCALESQ
      REAL    RES_SCAL
      REAL    RAD_OV_SCAL
      REAL    SMALL
      REAL    SMALLRT
*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*     If we are processing the guard ring do this.
*     else go straight to the divide-by-weights part.

      IF (USEGUARD) THEN

* Set up small to prevent comparing REAL to 0.0

         SMALL = VAL__SMLR
         SMALLRT = SQRT(SMALL)

*  set x and y axis pixel increments, x increases to left hence -ve.

         XINC = -PIXSPACE
         YINC = PIXSPACE

* Some time saving squares

         FILTER1_SQ = RES * RES
         FILTER_RAD_SQ = WEIGHTSIZE * WEIGHTSIZE

*  ..extent of convolution function in units of output pixels

         SCALE = 1.0 / SCLSZ

         SCALESQ = SCALE * SCALE

         RTEMP = REAL(WEIGHTSIZE) * SCLSZ / PIXSPACE
         PIX_RANGE = INT (RTEMP) + 1

         RAD_OV_SCAL = REAL(FILTER_RAD_SQ) / SCALESQ
         RES_SCAL = REAL(FILTER1_SQ) * SCALESQ

*  now do the convolution

         DO JOUT = 1, NJ
            DO IOUT = 1, NI

               IF (TOT_WEIGHT_IN (IOUT,JOUT) .GT. SMALL) THEN

*  OK, good map point, loop through other map points in convolution
*  function range ...

                  YPIX = REAL (JOUT-JCEN) * YINC
                  XPIX = REAL (IOUT-ICEN) * XINC

                  DO JCONV = MAX(1,JOUT-PIX_RANGE),
     :                 MIN(NJ,JOUT+PIX_RANGE)
                     DO ICONV = MAX(1,IOUT-PIX_RANGE),
     :                    MIN(NI,IOUT+PIX_RANGE)

*  and if they have zero weight, add them into the convolution function
*  weight as if they were measured to be zero

                        IF (TOT_WEIGHT_IN(ICONV,JCONV) .LT. SMALL) THEN
                           YCONV = REAL (JCONV-JCEN) * YINC
                           XCONV = REAL (ICONV-ICEN) * XINC
                           RPIX = (YPIX-YCONV)**2 + (XPIX-XCONV)**2

                           WT = 0.0
* Same method as used in REGRID_2
                           IF (RPIX .LT. RAD_OV_SCAL) THEN
                              ICPIX = NINT(RES_SCAL * RPIX)
                              WT = WTFN(ICPIX+1)

                              CONV_WEIGHT(IOUT,JOUT) =
     :                             CONV_WEIGHT(IOUT,JOUT) + WT

                           ENDIF

                        END IF

                     END DO
                  END DO

               END IF

            END DO
         END DO

      END IF

*  finally go through output pixels normalising them by CONV_WEIGHT
*  to give the final result. Those whose `total input weight' is zero
*  have their quality set bad

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            IF (TOT_WEIGHT_IN (IOUT,JOUT) .LT. SMALL) THEN
               CONV_DATA_SUM (IOUT,JOUT) = VAL__BADR
               CONV_VARIANCE_SUM (IOUT,JOUT) = VAL__BADR
               CONV_QUALITY_SUM (IOUT,JOUT) = 1
            ELSE
               IF (ABS(CONV_WEIGHT(IOUT,JOUT)) .GT. SMALLRT) THEN
                  CONV_DATA_SUM (IOUT,JOUT) =
     :              CONV_DATA_SUM (IOUT,JOUT) /
     :               CONV_WEIGHT (IOUT,JOUT)
                  CONV_VARIANCE_SUM (IOUT,JOUT) =
     :              CONV_VARIANCE_SUM (IOUT,JOUT) /
     :              CONV_WEIGHT (IOUT,JOUT)**2
                  CONV_QUALITY_SUM (IOUT,JOUT) = 0
               ELSE
                  CONV_DATA_SUM (IOUT,JOUT) = VAL__BADR
                  CONV_VARIANCE_SUM (IOUT,JOUT) = VAL__BADR
                  CONV_QUALITY_SUM (IOUT,JOUT) = 1
               END IF
            END IF
         END DO
      END DO

      END
