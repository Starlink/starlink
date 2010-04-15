      SUBROUTINE SCULIB_DIV_CALIBRATOR_2 (N_BOLS, N_POS, DATA, VARIANCE,
     :  CALIBRATOR, QUALITY,STATUS)
*+
*  Name:
*     SCULIB_DIV_CALIBRATOR_2

*  Purpose:
*     divides the mean of the calibrator signal into
*     the chop signal

*  Description:
*     This routine divides the calibrator signal into the chop signal and
*     variance over an array of measurements. No division will occur if the
*     measurement quality is bad or the square of the calibrator signal is zero.

*  Invocation:
*     CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS, N_POS, DATA, VARIANCE,
*    :  CALIBRATOR, QUALITY)

*  Arguments:
*     N_BOLS              = INTEGER (Given)
*              Number of bolometers measured
*     N_POS               = INTEGER (Given)
*              Number of positions measured
*     DATA (N_BOLS,N_POS) = REAL (Given and returned)
*              Demodulated data array
*     VARIANCE (N_BOLS,N_POS)
*                         = REAL (Given and returned)
*              Variance on DATA
*     CALIBRATOR (N_BOLS,N_POS)
*                         = REAL (Given)
*              Calibration array
*     QUALITY (N_BOLS,N_POS)
*                         = BYTE (Given and returned)
*              Quality on DATA
*     STATUS = INTEGER (Given & Returned)
*              inherited status


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER  N_BOLS
      INTEGER  N_POS
      REAL     CALIBRATOR (N_BOLS, N_POS)

*  Arguments Given & Returned:
      REAL     DATA (N_BOLS, N_POS)
      REAL     VARIANCE (N_BOLS, N_POS)
      BYTE     QUALITY (N_BOLS, N_POS)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITON

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL
      REAL    CAL_AVERAGE (144)
      INTEGER N_SUM
      INTEGER POS
      REAL    SUM

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  cycle through the bolometers

      DO BOL = 1, N_BOLS

*  average the calibrator signal over the positions measured

         SUM = 0.0
         N_SUM = 0
         CAL_AVERAGE (BOL) = 0.0

         DO POS = 1, N_POS
            IF (QUALITY(BOL,POS) .EQ. 0) THEN
               SUM = SUM + CALIBRATOR (BOL,POS)
               N_SUM = N_SUM + 1
            END IF
         END DO

         IF (N_SUM .GT. 0) THEN
            CAL_AVERAGE (BOL) = SUM / REAL (N_SUM)
         END IF
      END DO

*  divide the calibrator average into the data

      DO BOL = 1, N_BOLS
         IF (CAL_AVERAGE(BOL)**2 .GT. 1.0E-8) THEN
            DO POS = 1, N_POS
               IF (QUALITY(BOL,POS) .EQ. 0) THEN
                  DATA (BOL,POS) = DATA (BOL,POS) / CAL_AVERAGE (BOL)
                  VARIANCE (BOL,POS) = VARIANCE (BOL,POS) /
     :              CAL_AVERAGE(BOL)**2
               END IF
            END DO
         ELSE

*  set data invalid bit

            DO POS = 1, N_POS
               QUALITY(BOL,POS) = SCULIB_BITON(QUALITY(BOL,POS), 0)
            END DO
         END IF
      END DO

      END
