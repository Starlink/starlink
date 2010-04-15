      SUBROUTINE SCULIB_DIV_CALIBRATOR (NELM, DATA, VARIANCE,
     :  CALIBRATOR, QUALITY,STATUS)
*+
*  Name:
*     SCULIB_DIV_CALIBRATOR

*  Purpose:
*     divides the calibrator signal into the chop signal

*  Description:
*     This routine divides the calibrator signal into the chop signal and
*     variance over an array of measurements. No division will occur if the
*     measurement quality is bad or the square of the calibrator signal is zero.

*  Invocation:
*     CALL SCULIB_DIV_CALIBRATOR (NELM, DATA, VARIANCE, CALIBRATOR,
*    :  QUALITY)

*  Arguments:
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
*     6-JUN-1995: Original
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER  NELM
      REAL     CALIBRATOR (NELM)
      BYTE  QUALITY (NELM)

*  Arguments Given & Returned:
      REAL     DATA (NELM)
      REAL     VARIANCE (NELM)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      BYTE SCULIB_BITON

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NELM

         IF (CALIBRATOR(I)**2 .GT. 1.0E-8) THEN
            DATA (I) = DATA (I) / CALIBRATOR (I)
            VARIANCE (I) = VARIANCE (I) / CALIBRATOR(I)**2
         ELSE
            QUALITY(I) = SCULIB_BITON(QUALITY(I), 0) ! Set INVALID bit
         END IF

      END DO

      END
