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
*     QUALITY (NELM)      = INTEGER (Given)
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
      INTEGER  QUALITY (NELM)
*    Import-Export :
      REAL     DATA (NELM)
      REAL     VARIANCE (NELM)
*    Export :
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER  GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)
*    Local variables :
      INTEGER I
*    Internal References :
*    Local data :
*-

      DO I = 1, NELM

         IF (QUALITY (I) .EQ. GOOD) THEN
            IF (CALIBRATOR(I)**2 .NE. 0.0) THEN
               DATA (I) = DATA (I) / CALIBRATOR (I)
               VARIANCE (I) = VARIANCE (I) / CALIBRATOR(I)**2
            ELSE
               QUALITY (I) = BAD
            END IF
         END IF

      END DO

      END
