      SUBROUTINE SCULIB_INTEGRATE_PHOTOM_JIGGLE (BOL, N_BOLS, J_COUNT,
     :  DATA, VARIANCE, QUALITY, RESULT_D, RESULT_V, RESULT_Q, STATUS)
*+
*  Name:
*     SCULIB_INTEGRATE_PHOTOM_JIGGLE

*  Purpose:
*     integrate the jiggle map made by a
*     bolometer during a PHOTOM observation

*  Description:
*     This routine just sums the data for the specified bolometer over the
*     jiggle pattern.
*
*       After checking status on entry the routine checks that the bolometer
*     to be integrated is among those that were measured. All being well it
*     then loops through the jiggle pattern summing the valid data and variance
*     measured at each position for that bolometer. If no valid data were
*     obtained then the result quality will be set bad.

*  Invocation:
*     CALL SCULIB_INTEGRATE_PHOTOM_JIGGLE (BOL, N_BOLS, J_COUNT,
*    :  DATA, VARIANCE, QUALITY, RESULT_D, RESULT_V, RESULT_Q, STATUS)

*  Arguments:
*     BOL                          = INTEGER (Given)
*           the index of the bolometer whose data is to be integrated
*     N_BOLS                       = INTEGER (Given)
*           the number of bolometers being measured
*     J_COUNT                      = INTEGER (Given)
*           the number of jiggles in the pattern
*     DATA (N_BOLS, J_COUNT)       = REAL (Given)
*           the measured data
*     VARIANCE (N_BOLS, J_COUNT)   = REAL (Given)
*           the variance
*     QUALITY (N_BOLS, J_COUNT)    = INTEGER (Given)
*           the quality
*     RESULT_D                     = REAL (Returned)
*           the sum of the input data
*     RESULT_V                     = REAL (Returned)
*           the sum of the input variances
*     RESULT_Q                     = INTEGER (Returned)
*           the quality on the sum
*     STATUS                       = INTEGER (Given and returned)
*           global status

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
*     9-AUG-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER BOL
      INTEGER N_BOLS
      INTEGER J_COUNT
      REAL DATA (N_BOLS, J_COUNT)
      REAL VARIANCE (N_BOLS, J_COUNT)
      INTEGER QUALITY (N_BOLS, J_COUNT)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL RESULT_D
      REAL RESULT_V
      INTEGER RESULT_Q

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER J                         ! DO loop

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF ((BOL .LT. 1) .OR. (BOL .GT. N_BOLS)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('BOL', BOL)
         CALL MSG_SETI ('N_BOLS', N_BOLS)
         CALL ERR_REP (' ', 'SCULIB_INTEGRATE_PHOTOM_JIGGLE: '//
     :     'bolometer ^BOL out of range 1 - ^N_BOLS', STATUS)
      ELSE

         RESULT_D = 0.0
         RESULT_V = 0.0
         RESULT_Q = 1

*  the routine just sums the valid data for the specified bolometer over the
*  jiggle pattern measured

         DO J = 1, J_COUNT

            IF (QUALITY(BOL,J) .EQ. 0) THEN
               RESULT_D = RESULT_D + DATA (BOL,J)
               RESULT_V = RESULT_V + VARIANCE (BOL,J)
               RESULT_Q = 0
            END IF

         END DO

      END IF

      END
