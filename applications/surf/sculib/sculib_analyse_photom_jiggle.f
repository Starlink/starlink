      SUBROUTINE SCULIB_ANALYSE_PHOTOM_JIGGLE (PHOTOM_ANALYSIS,
     :     BOL, N_BOLS, J_COUNT, JIGGLE_X, JIGGLE_Y, JDATA, VARIANCE,
     :     QUALITY,RESULT_D, RESULT_V, RESULT_Q, A0, A1, X0, Y0, BADBIT,
     :     STATUS)
*+
*  Name:
*     SCULIB_ANALYSE_PHOTOM_JIGGLE

*  Purpose:
*     analyse the jiggle map made by a
*     bolometer during a PHOTOM observation

*  Description:
*     This routine analyses the jiggle map made by a bolometer during a
*     PHOTOM observation.
*        If status is good on entry the routine checks that the data array
*     holds data for the specified bolometer. If it does not an error
*     message will be output and the bad status returned.
*        All being well the routine will analyse the data by the method
*      specified in PHOTOM_ANALYSIS:-
*
*     AVERAGE -    the `result' will be the mean of the jiggle points.
*                  The variance is the sum of the variances of the averaged
*                  data divided by the number of them squared. The result
*                  will be flat-fielded and reported to the user. If no
*                  valid data were available for the calculation of the
*                  mean then a warning message will be output but good
*                  status returned.
*
*     PARABOLA -   the routine will call SCULIB_FIT_2D_PARABOLA to fit a
*                  2-d parabola to the data and the `result' will be the
*                  peak of the fitted curve. If the fit is good the results
*                  will be flat-fielded and reported to the user. Otherwise
*                  the result quality is set bad but good status will be
*                  returned.

*  Invocation:
*     CALL SCULIB_ANALYSE_PHOTOM_JIGGLE (PHOTOM_ANALYSIS, BOL, N_BOLS,
*    :  J_COUNT, JDATA, VARIANCE, QUALITY, RESULT_D, RESULT_V, RESULT_Q,
*    :  BADBIT, STATUS)

*  Arguments:
*     PHOTOM_ANALYSIS              = CHARACTER*(*) (Given)
*           the method of analysis to be applied to the data
*     BOL                          = INTEGER (Given)
*           the index of the bolometer whose data is to be analysed
*     N_BOLS                       = INTEGER (Given)
*           the number of bolometers being measured
*     J_COUNT                      = INTEGER (Given)
*           the number of jiggles in the pattern
*     JIGGLE_X                     = REAL (Given)
*           x jiggle offsets
*     JIGGLE_Y                     = REAL (Given)
*           y jiggle offsets
*     JDATA (N_BOLS, J_COUNT)       = REAL (Given)
*           the measured data
*     VARIANCE (N_BOLS, J_COUNT)   = REAL (Given)
*           the variance
*     QUALITY (N_BOLS, J_COUNT)    = BYTE (Given)
*           the quality
*     RESULT_D                     = REAL (Returned)
*           the sum of the input data
*     RESULT_V                     = REAL (Returned)
*           the sum of the input variances
*     RESULT_Q                     = BYTE (Returned)
*           the quality on the sum
*     A0                           = REAL (Returned)
*           fitted parabola parameter
*     A1                           = REAL (Returned)
*           fitted parabola parameter
*     X0                           = REAL (Returned)
*           x offset of peak of fitted parabola
*     Y0                           = REAL (Returned)
*           y offset of peak of fitted parabola
*     BADBIT                       = BYTE (Given)
*           bad bit mask
*     STATUS                       = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     T.Jenness (t.jenness@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1993-1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     9-AUG-1993: Original version.
*     8-JAN-1996: Modified to fit parabola.
*    17-JUL-1996: Modified to multiply by flat-field rather than divide (JFL).
*    18-JUL-1996: Modified to use PHOTOM_ANALYSIS (JFL).
*    endhistory

*  Method:

*  Deficiencies:

*  Bugs:


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      CHARACTER*(*) PHOTOM_ANALYSIS
      INTEGER       BOL
      INTEGER       N_BOLS
      INTEGER       J_COUNT
      REAL          JIGGLE_X (J_COUNT)
      REAL          JIGGLE_Y (J_COUNT)
      REAL          JDATA (N_BOLS, J_COUNT)
      REAL          VARIANCE (N_BOLS, J_COUNT)
      BYTE          QUALITY (N_BOLS, J_COUNT)
      BYTE          BADBIT

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL RESULT_D
      REAL RESULT_V
      BYTE RESULT_Q
      REAL A0
      REAL A1
      REAL X0
      REAL Y0

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      REAL         ALLOWED_BAD_FRAC     ! Fraction of bad data allowed
      PARAMETER (ALLOWED_BAD_FRAC = 0.3)! in a AVERAGE jiggle
      INTEGER      MAX_JIGGLE           ! max number of jiggle positions
      PARAMETER (MAX_JIGGLE = 512)

*  Local variables:
      CHARACTER*80 ANALYSIS             ! an uppercase copy of
                                        ! PHOTOM_ANALYSIS
      REAL         GOODFRAC             ! Fraction of good data
      INTEGER      J                    ! jiggle number in DO loop
      INTEGER      N_GOOD               ! Number of good jiggle points
      REAL         T_DATA (MAX_JIGGLE)  ! data for a particular bolometer
      BYTE         T_QUALITY (MAX_JIGGLE)
                                        ! quality for a particular bolometer
      REAL         T_VARIANCE (MAX_JIGGLE)
                                        ! variance for a particular bolometer

*  Internal References:

*  Local data:

*  External functions:
      INCLUDE 'NDF_FUNC'

*.

      IF (STATUS .NE. SAI__OK) RETURN

      ANALYSIS = PHOTOM_ANALYSIS
      CALL CHR_UCASE (ANALYSIS)

      IF ((BOL .LT. 1) .OR. (BOL .GT. N_BOLS)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('BOL', BOL)
         CALL MSG_SETI ('N_BOLS', N_BOLS)
         CALL ERR_REP (' ', 'SCULIB_ANALYSE_PHOTOM_JIGGLE: '//
     :     'bolometer ^BOL out of range 1 - ^N_BOLS', STATUS)
      ELSE

*  perform the requested analysis

         IF (ANALYSIS .EQ. 'AVERAGE') THEN

*  Calculate the mean of the data keeping track of bad data points

            RESULT_D = 0.0
            RESULT_V = 0.0
            N_GOOD    = 0

            DO J = 1, J_COUNT
               IF (JDATA(BOL,J) .NE. VAL__BADR .AND.
     :              NDF_QMASK(QUALITY(BOL,J), BADBIT)) THEN
                  N_GOOD = N_GOOD + 1
                  RESULT_D = RESULT_D + JDATA (BOL,J)
                  RESULT_V = RESULT_V + VARIANCE (BOL,J)
               END IF

            END DO

* The entire jiggle is bad if the fraction of good data points in the
* jiggle is smaller than 1-ALLOWED_BAD_FRAC

            GOODFRAC = REAL(N_GOOD) / REAL(J_COUNT)

            IF (GOODFRAC .GE. 1.0-ALLOWED_BAD_FRAC) THEN

               RESULT_D = RESULT_D / REAL (N_GOOD)
               RESULT_V = RESULT_V / REAL (N_GOOD * N_GOOD)
               RESULT_Q = 0

            ELSE

               RESULT_D = VAL__BADR
               RESULT_V = 0.0
               RESULT_Q = 1

            END IF

         ELSE IF (ANALYSIS .EQ. 'PARABOLA') THEN

*  the `analysis' consists of fitting a parabola to the valid data for the
*  specified bolometer over the jiggle pattern

*  copy the jiggle result into 1-d arrays

            DO J = 1, J_COUNT
               T_DATA (J) = JDATA (BOL,J)
               T_VARIANCE (J) = VARIANCE (BOL,J)
               T_QUALITY (J) = QUALITY (BOL,J)
            END DO

            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_FIT_2D_PARABOLA (J_COUNT, T_DATA, T_VARIANCE,
     :              T_QUALITY, JIGGLE_X, JIGGLE_Y, A0, A1, X0, Y0,
     :              RESULT_D, RESULT_V, BADBIT, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN
                  RESULT_Q = 0
               ELSE
                  RESULT_Q = 1
                  CALL ERR_FLUSH (STATUS)
               END IF
            END IF
         END IF
      END IF

      END
