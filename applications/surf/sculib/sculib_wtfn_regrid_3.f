      SUBROUTINE SCULIB_BESSEL_REGRID_3 (PIXSPACE, NI, NJ, ICEN, 
     :  JCEN, TOT_WEIGHT_IN, WAVELENGTH, CONV_DATA_SUM, 
     :  CONV_VARIANCE_SUM, CONV_QUALITY_SUM, CONV_WEIGHT, STATUS)
*+
*  Name:
*     SCULIB_BESSEL_REGRID_3

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_BESSEL_REGRID_3 (PIXSPACE, NI, NJ, ICEN, JCEN, 
*    :  TOT_WEIGHT_IN, WAVELENGTH, CONV_DATA_SUM, CONV_VARIANCE_SUM, 
*    :  CONV_QUALITY_SUM, CONV_WEIGHT, STATUS)

*  Description:

*  Arguments:
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

*  Arguments Given:
      REAL PIXSPACE
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      REAL TOT_WEIGHT_IN (NI, NJ)
      REAL WAVELENGTH

*  Arguments Returned:
      REAL CONV_DATA_SUM (NI, NJ)
      REAL CONV_VARIANCE_SUM (NI, NJ)
      BYTE CONV_QUALITY_SUM (NI, NJ)
      REAL CONV_WEIGHT (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:
      REAL SCULIB_BESSJ1                         ! J1(x) Bessel function
*  Local Constants:
      REAL    DIAMETER                           ! diameter of JCMT mirror
      PARAMETER (DIAMETER = 15.0)
      INTEGER MAX_RES_ELEMENT                    ! radius of convolution
      PARAMETER (MAX_RES_ELEMENT = 7)            ! function in resolution 
                                                 ! elements
      REAL    PI                                 !
      PARAMETER (PI = 3.14159265359)
      INTEGER WTRESOL                            ! number of sub resolution 
      PARAMETER (WTRESOL = 200)                  ! element interpolations of the
                                                 ! weighting function

*  Local Variables:
      INTEGER I                                  ! loop counter
      INTEGER ICONV                              ! I index in convolution
      INTEGER ICPIX                              ! index in convolution function
                                                 ! corresponding to RPIX
      INTEGER IOUT                               ! I pixel index in output array
      INTEGER ISTART                             !
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
      REAL    WTFN (MAX_RES_ELEMENT * WTRESOL)   ! weighting function
*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  set x and y axis pixel increments, x increases to left hence -ve.

      XINC = -PIXSPACE
      YINC = PIXSPACE

*  Weighting function used is the function (I/I0 = 2*J1(x)/x), with
*  a cosine apodization to zero over the last third of its extent.
*  It is tabulated up to MAX_RES_ELEMENT resolution elements out at a 
*  resolution of 1/WTRESOL of a resolution element. The angular size
*  of a resolution element is equal to WAVELENGTH / 2 * DIAMETER. 

*  ..basic function

      WTFN(1) = 1.0
      DO I = 2, MAX_RES_ELEMENT * WTRESOL
         RTEMP = REAL(I-1) * PI / REAL(WTRESOL)
         WTFN (I) = 2.0 * SCULIB_BESSJ1(RTEMP, STATUS) / RTEMP
      END DO

*  ..apodization

      ISTART = NINT (REAL(MAX_RES_ELEMENT * WTRESOL) * 0.66)
      DO I = ISTART, MAX_RES_ELEMENT * WTRESOL
         WTFN (I) = WTFN (I) *
     :     COS (REAL(I-ISTART)*PI/
     :     (2.0*REAL(MAX_RES_ELEMENT*WTRESOL-ISTART)))
      END DO

*  ..extent of convolution function in units of output pixels
    
      RES_ELEMENT = WAVELENGTH * 1.0E-6 / (2.0 * DIAMETER)
      RTEMP = REAL (MAX_RES_ELEMENT) * RES_ELEMENT / PIXSPACE
      PIX_RANGE = INT (RTEMP) + 1

*  now do the convolution

      DO JOUT = 1, NJ
         DO IOUT = 1, NI

            IF (TOT_WEIGHT_IN (IOUT,JOUT) .NE. 0.0) THEN

*  OK, good map point, loop through other map points in convolution
*  function range ...

               YPIX = REAL (JOUT-JCEN) * YINC     
               XPIX = REAL (IOUT-ICEN) * XINC

               DO JCONV = MAX(1,JOUT-PIX_RANGE), MIN(NJ,JOUT+PIX_RANGE)
                  DO ICONV = MAX(1,IOUT-PIX_RANGE), 
     :              MIN(NI,IOUT+PIX_RANGE)

*  and if they have zero weight, add them into the convolution function 
*  weight as if they were measured to be zero

                     IF (TOT_WEIGHT_IN(ICONV,JCONV) .EQ. 0.0) THEN
                        YCONV = REAL (JCONV-JCEN) * YINC     
                        XCONV = REAL (ICONV-ICEN) * XINC 
                        RPIX = SQRT ((YPIX-YCONV)**2 + (XPIX-XCONV)**2)
                        RPIX = RPIX / RES_ELEMENT         

                        ICPIX = NINT (RPIX * WTRESOL) + 1
                        IF ((ICPIX.GE.1).AND.
     :                      (ICPIX.LE.MAX_RES_ELEMENT*WTRESOL)) THEN
                           WT = WTFN (ICPIX)
                        ELSE
                           WT = 0.0
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
*  have their quality set bad

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            IF (TOT_WEIGHT_IN (IOUT,JOUT) .EQ. 0.0) THEN
               CONV_DATA_SUM (IOUT,JOUT) = 0.0
               CONV_VARIANCE_SUM (IOUT,JOUT) = 0.0
               CONV_QUALITY_SUM (IOUT,JOUT) = 1
            ELSE
               IF (CONV_WEIGHT(IOUT,JOUT)**2 .NE. 0.0) THEN
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
