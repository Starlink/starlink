      SUBROUTINE SCULIB_BESSEL_REGRID_2 (IN_DATA, IN_VARIANCE,
     :  WEIGHT, X, Y, NPIX, PIXSPACE, NI, NJ, ICEN, JCEN, 
     :  TOTAL_WEIGHT, WAVELENGTH, CONV_DATA_SUM, CONV_VARIANCE_SUM, 
     :  CONV_WEIGHT, WEIGHTSIZE, WTFN, STATUS)
*+
*  Name:
*     SCULIB_BESSEL_REGRID_2

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE SCULIB_BESSEL_REGRID_2 (IN_DATA, IN_VARIANCE,
*    :  WEIGHT, X, Y, NPIX, PIXSPACE, NI, NJ, ICEN, JCEN, 
*    :  TOTAL_WEIGHT, WAVELENGTH, CONV_DATA_SUM, CONV_VARIANCE_SUM,
*    :  CONV_WEIGHT, STATUS)

*  Description:

*  Arguments:
*     IN_DATA (NPIX)                   = REAL (Given)
*        The input data values.
*     IN_VARIANCE (NPIX)               = REAL (Given)
*        Variance on IN_DATA.
*     WEIGHT                           = REAL (Given)
*        The weight of this dataset.
*     X (NPIX)                         = DOUBLE PRECISION (Given)
*        The x coordinates of the input pixels.
*     Y (NPIX)                         = DOUBLE PRECISION (Given)
*        The y coordinates of the input pixels.
*     NPIX                             = INTEGER (Given)
*        the number of input pixels.
*     PIXSPACE                         = REAL (Given)
*        the pixel spacing of the output map.
*     NI                               = INTEGER (Given)
*        The number of output pixels in the x direction.
*     NJ                               = INTEGER (Given)
*        The number of output pixels in the y direction.
*     ICEN                             = INTEGER (Given)
*        the x index of the centre of the output array.
*     JCEN                             = INTEGER (Given)
*        the y index of the centre of the output array.
*     TOTAL_WEIGHT (NI,NJ)             = REAL (Given)
*        the `total weight' of each output pixel.
*     WAVELENGTH                       = REAL (Given)
*        the wavelength at which the maps were made (microns).
*     CONV_DATA_SUM (NI,NJ)            = REAL (Given and returned)
*        the convolution sum for each output pixel.
*     CONV_VARIANCE_SUM (NI,NJ)        = REAL (Given and returned)
*        the variance convolution sum for each output pixel.
*     CONV_WEIGHT (NI,NJ)              = REAL (Given and returned)
*        the convolution weight for each output pixel.
*     WEIGHTSIZE                       = INTEGER (Given)
*        radius of weight function in scale units (SCUIP__FILTRAD for BESSEL, 1 for LINEAR)
*     WTFN (SCUIP__RES1 * SCUIP__RES1 +   = REAL (Given)
*     :     SCUIP__RES2 * (SCUIP__FILTRAD * SCUIP__FILTRAD))
*        convolution weighting function
*     STATUS                           = INTEGER (Given and Returned)
*        The global status.
*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)

*  History:
*    21-AUG-1995: original version, adapted from JCMT_BESSEL_REGRID_2.

*  Bugs:

*-
      
*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'REDS_SYS'

*  Arguments Given:
      INTEGER NPIX
      REAL IN_DATA (NPIX)
      REAL IN_VARIANCE (NPIX)
      REAL WEIGHT
      REAL WWEIGHT
      DOUBLE PRECISION X(NPIX)
      DOUBLE PRECISION Y(NPIX)
      REAL PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      REAL TOTAL_WEIGHT (NI,NJ)
      REAL WAVELENGTH
      INTEGER WEIGHTSIZE
      REAL WTFN(SCUIP__RES1 * SCUIP__RES1 +
     :     SCUIP__RES2 * (SCUIP__FILTRAD * SCUIP__FILTRAD))

*  Arguments Returned:
      REAL CONV_DATA_SUM (NI, NJ)
      REAL CONV_VARIANCE_SUM (NI, NJ)
      REAL CONV_WEIGHT (NI, NJ)

*  Status:
      INTEGER STATUS  

*  Local Constants:

*  Local Variables:
      INTEGER ICPIX                              ! index in convolution function
                                                 ! corresponding to RPIX
      INTEGER INEAR                              ! I index of output pixel 
                                                 ! nearest to input
      INTEGER IOUT                               ! I pixel index in output array
      INTEGER JNEAR                              ! J index of output pixel
                                                 ! nearest to input
      INTEGER JOUT                               ! J pixel index in output array
      INTEGER PIX                                ! input pixel number
      INTEGER PIX_RANGE                          ! radius over which convolution
                                                 ! function is non-zero, in
                                                 ! pixels
      REAL    RES_ELEMENT                        ! critical sample spacing at
                                                 ! WAVELENGTH (radians)
      REAL    RPIX                               ! distance between input and
                                                 ! output pixel
      REAL    RTEMP                              ! scratch real
      REAL    WT                                 ! value of convolution 
                                                 ! function at output pixel
      REAL    XINC                               ! x-axis pixel increment
      REAL    XPIX                               ! x offset of output pixel
      REAL    YINC                               ! y-axis pixel increment
      REAL    YPIX                               ! y offset of output pixel
      
      REAL FILTER1_SQ
      INTEGER  FILTER_RAD_SQ
      REAL SCALE
      REAL SCALESQ
*   local data
*.

      IF (STATUS .NE. SAI__OK) RETURN


*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

* Some time saving squares

      FILTER1_SQ = SCUIP__RES1 * SCUIP__RES1
      FILTER_RAD_SQ = WEIGHTSIZE * WEIGHTSIZE

*  ..extent of convolution function in units of output pixels

      RES_ELEMENT = WAVELENGTH * 1.0E-6 / (2.0 * DIAMETER)
      SCALE = 1.0 / RES_ELEMENT

      SCALESQ = SCALE * SCALE
      
      RTEMP = REAL(WEIGHTSIZE) * RES_ELEMENT / PIXSPACE
      PIX_RANGE = INT (RTEMP) + 1

*  now do the convolution, looping over the input pixels

      DO PIX = 1, NPIX

         IF (IN_DATA(PIX) .NE. VAL__BADR) THEN
         
*  find the coords (INEAR, JNEAR) of the output pixel nearest to the current
*  input pixel. 

            INEAR = NINT (REAL(X(PIX))/XINC) + ICEN
            JNEAR = NINT (REAL(Y(PIX))/YINC) + JCEN

*  loop over x's and y's in output array that are covered by the convolution 
*  function centred at (INEAR, JNEAR)
         
            DO JOUT = MAX(1,JNEAR-PIX_RANGE), MIN(NJ,JNEAR+PIX_RANGE)
               DO IOUT = MAX(1,INEAR-PIX_RANGE), MIN(NI,INEAR+PIX_RANGE)

*  work out x,y offset of current output pixel

                  YPIX = REAL (JOUT-JCEN) * YINC
                  XPIX = REAL (IOUT-ICEN) * XINC

*  distance between output and input pixels, in SCALE LENGTHS**2 units
 
                  RPIX = (YPIX-REAL(Y(PIX)))**2 + 
     :              (XPIX-REAL(X(PIX)))**2

                  RPIX = RPIX * SCALESQ

* Now work out which part of the weight function t ouse

                  WT = 0.0
                  IF (RPIX .LT. FILTER_RAD_SQ) THEN
                     IF (RPIX .GT. 1.0) THEN
                        ICPIX = NINT(FILTER1_SQ + (RPIX-1.0)
     :                       * SCUIP__RES2)
                     ELSE
                        ICPIX = NINT(FILTER1_SQ * RPIX)
                     ENDIF
                     WT = WTFN(ICPIX+1)

                  END IF

*  add into the convolution result and weight arrays unless TOTAL_WEIGHT
*  of output is zero, signifying output pixel is beyond limits of
*  mapped area. The coaddition is normalised by the `total weight' 
*  associated with this input pixel.

                  IF (TOTAL_WEIGHT (IOUT,JOUT) .NE. 0.0) THEN

                     WWEIGHT = WT * WEIGHT / TOTAL_WEIGHT (INEAR,JNEAR)

                     CONV_WEIGHT (IOUT,JOUT) = CONV_WEIGHT(IOUT,JOUT) + 
     :                 WWEIGHT
                     CONV_DATA_SUM (IOUT,JOUT) = 
     :                 CONV_DATA_SUM (IOUT,JOUT) + 
     :                 WWEIGHT * IN_DATA(PIX)
                     CONV_VARIANCE_SUM (IOUT,JOUT) =
     :                 CONV_VARIANCE_SUM (IOUT,JOUT) +
     :                 (WWEIGHT)**2 * IN_VARIANCE (PIX)
                  END IF

               END DO
            END DO

         END IF

      END DO

      END
