*+  SCULIB_DIV_CALIBRATOR - divides the calibrator signal into the chop signal
      SUBROUTINE SCULIB_DIV_CALIBRATOR (NELM, DATA, VARIANCE, 
     :  CALIBRATOR, QUALITY)
*    Description :
*     This routine divides the calibrator signal into the chop signal and
*     variance over an array of measurements. No division will occur if the 
*     measurement quality is bad or the square of the calibrator signal is zero.
*    Invocation :
*     CALL SCULIB_DIV_CALIBRATOR (NELM, DATA, VARIANCE, CALIBRATOR, 
*    :  QUALITY)
*    Parameters :
*     NELM                = INTEGER (Given)
*              Number of elements in each array
*     DATA (NELM)         = REAL (Given and returned)
*              Demodulated data array
*     VARIANCE (NELM)     = REAL (Given and returned)
*              Variance on DATA
*     CALIBRATOR (NELM)   = REAL (Given)
*              Calibration array
*     QUALITY (NELM)      = BYTE (Given)
*              Quality on DATA
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     6-JUN-1995: Original
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER  NELM
      REAL     CALIBRATOR (NELM)
      BYTE  QUALITY (NELM)
*    Import-Export :
      REAL     DATA (NELM)
      REAL     VARIANCE (NELM)
*    Export :
*    Status :
*    External references :
      BYTE SCULIB_BITON
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I
*    Internal References :
*    Local data :
*-

      DO I = 1, NELM

         IF (CALIBRATOR(I)**2 .GT. 1.0E-8) THEN
            DATA (I) = DATA (I) / CALIBRATOR (I)
            VARIANCE (I) = VARIANCE (I) / CALIBRATOR(I)**2
         ELSE
            QUALITY(I) = SCULIB_BITON(QUALITY(I), 0) ! Set INVALID bit
         END IF

      END DO

      END
