      SUBROUTINE SCULIB_WTFN_REGRID_3 (DIAMETER, RES, PIXSPACE, NI,NJ,
     :     ICEN, JCEN, TOT_WEIGHT_IN, WAVELENGTH, CONV_DATA_SUM, 
     :     CONV_VARIANCE_SUM, CONV_QUALITY_SUM, CONV_WEIGHT, 
     :     WEIGHTSIZE, WTFN, STATUS)
*+
*  Name:
*     SCULIB_BESSEL_REGRID_3

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_BESSEL_REGRID_3 (DIAMETER, RES,PIXSPACE, NI, NJ, ICEN, JCEN, 
*    :  TOT_WEIGHT_IN, WAVELENGTH, CONV_DATA_SUM, CONV_VARIANCE_SUM, 
*    :  CONV_QUALITY_SUM, CONV_WEIGHT, WEIGHTSIZE, WTFN, STATUS)

*  Description:

*  Arguments:
*     DIAMETER                       = REAL (given)
*        size of dish
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
*     WAVELENGTH                     = REAL (Given)
*        the wavelength at which the maps was made (microns).
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
*     WTFN (RES * RES * WEIGHTSIZE * WEIGHTSIZE) = REAL (Given)
*        convolution weighting function
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*     15-AUG-1995: original version, modified from JCMT_BESSEL_REGRID_3

*  Bugs:

*-
      
*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      REAL DIAMETER
      REAL RES
      REAL PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      REAL TOT_WEIGHT_IN (NI, NJ)
      REAL WAVELENGTH
      INTEGER WEIGHTSIZE
      REAL WTFN(RES * RES * WEIGHTSIZE * WEIGHTSIZE)

*  Arguments Returned:
      REAL CONV_DATA_SUM (NI, NJ)
      REAL CONV_VARIANCE_SUM (NI, NJ)
      BYTE CONV_QUALITY_SUM (NI, NJ)
      REAL CONV_WEIGHT (NI, NJ)

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
      REAL FILTER1_SQ
      REAL FILTER_RAD_SQ
      REAL SCALE
      REAL SCALESQ
      REAL RES_SCAL
      REAL RAD_OV_SCAL
      REAL SMALL
      REAL SMALLRT
*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

* Set up small to prevent comparing REAL to 0.0

      SMALL = VAL__SMLR * 10.0
      SMALLRT = SQRT(SMALL)


*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

* Some time saving squares

      FILTER1_SQ = RES * RES
      FILTER_RAD_SQ = WEIGHTSIZE * WEIGHTSIZE

*  ..extent of convolution function in units of output pixels
    
      RES_ELEMENT = WAVELENGTH * 1.0E-6 / (2.0 * DIAMETER)
      SCALE = 1.0 / RES_ELEMENT

      SCALESQ = SCALE * SCALE
      
      RTEMP = REAL(WEIGHTSIZE) * RES_ELEMENT / PIXSPACE
      PIX_RANGE = INT (RTEMP) + 1

      RAD_OV_SCAL = FILTER_RAD_SQ / SCALESQ
      RES_SCAL = FILTER1_SQ * SCALESQ


*  now do the convolution

      DO JOUT = 1, NJ
         DO IOUT = 1, NI

            IF (TOT_WEIGHT_IN (IOUT,JOUT) .GT. SMALL) THEN

*  OK, good map point, loop through other map points in convolution
*  function range ...

               YPIX = REAL (JOUT-JCEN) * YINC     
               XPIX = REAL (IOUT-ICEN) * XINC

               DO JCONV = MAX(1,JOUT-PIX_RANGE), MIN(NJ,JOUT+PIX_RANGE)
                  DO ICONV = MAX(1,IOUT-PIX_RANGE), 
     :              MIN(NI,IOUT+PIX_RANGE)

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
                        ENDIF

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
*  have their quality set bad

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            IF (TOT_WEIGHT_IN (IOUT,JOUT) .LT. SMALL) THEN
               CONV_DATA_SUM (IOUT,JOUT) = 0.0
               CONV_VARIANCE_SUM (IOUT,JOUT) = 0.0
               CONV_QUALITY_SUM (IOUT,JOUT) = 1
            ELSE
               IF (CONV_WEIGHT(IOUT,JOUT) .GT. SMALLRT) THEN
                  CONV_DATA_SUM (IOUT,JOUT) = 
     :              CONV_DATA_SUM (IOUT,JOUT) /
     :               CONV_WEIGHT (IOUT,JOUT)
                  CONV_VARIANCE_SUM (IOUT,JOUT) =
     :              CONV_VARIANCE_SUM (IOUT,JOUT) /
     :              CONV_WEIGHT (IOUT,JOUT)**2
                  CONV_QUALITY_SUM (IOUT,JOUT) = 0
               ELSE
                  CONV_DATA_SUM (IOUT,JOUT) = 0.0
                  CONV_VARIANCE_SUM (IOUT,JOUT) = 0.0
                  CONV_QUALITY_SUM (IOUT,JOUT) = 1
               END IF
            END IF
         END DO
      END DO

      END
