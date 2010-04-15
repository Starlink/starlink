      SUBROUTINE SCULIB_COMPRESS_DEMOD (N_BOLS, N_JIGS, N_INTS,
     :  IN_DATA, ADC_INDEX, CHAN_INDEX, INT_INDEX, ADC, CHAN,
     :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_COMPRESS_DEMOD

*  Purpose:
*     get demodulated data for a bolometer and coadd
*     jiggles in each integration

*  Description:
*     This routine selects data for a specified bolometer from a demodulated
*     data array, and coadds the results for each jiggle in each integration
*     to give an average for that integration. Data with bad quality are
*     ignored. The variance on the average will also be derived; set equal
*     to the variance on the input data if only one jiggle contributes to the
*     average, otherwise calculated from the spread of the input points about
*     the mean.

*  Invocation:
*     CALL SCULIB_COMPRESS_DEMOD (N_BOLS, N_JIGS, N_INTS,
*    :  IN_DATA, ADC_INDEX, CHAN_INDEX, INT_INDEX, ADC, CHAN,
*    :  OUT_DATA, OUT_VARIANCE, OUT_QUALITY, STATUS)

*  Arguments:
*     N_BOLS                 = INTEGER (Given)
*           number of bolometers measured
*     N_JIGS                 = INTEGER (Given)
*           number of jiggles in pattern
*     N_INTS                 = INTEGER (Given)
*           number of integrations taken
*     IN_DATA (4, N_BOLS, N_JIGS * N_INTS)
*                            = REAL (Given)
*           the demodulated data; 1=data, 2=variance, 3=calibrator, 4=quality
*     ADC_INDEX (N_BOLS)     = INTEGER (Given)
*           the ADC numbers of the measured bolometers
*     CHAN_INDEX (N_BOLS)    = INTEGER (Given)
*           the channel numbers of the measured bolometers
*     INT_INDEX (N_INTS)     = INTEGER (Given)
*           the k index in IN_DATA of the first jiggle of each integration
*     ADC                    = INTEGER (Given)
*           the ADC number of the requested bolometer
*     CHAN                   = INTEGER (Given)
*           the channel number of the requested bolometer
*     OUT_DATA (N_INTS)      = REAL (Returned)
*           the values for each integration averaged over the jiggles
*     OUT_VARIANCE (N_INTS)  = REAL (Returned)
*           the variance on OUT_DATA
*     OUT_QUALITY (N_INTS)   = INTEGER (Returned)
*           the quality on OUT_DATA
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Notes:
*     Only works for data where SWITCH_PER_EXP=1, EXP_PER_INT=1 and
*     N_MEASUREMENTS=1.

*  Authors:
*     J.Lightfoot (JFL/ROE)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     13-DEC-1994: Orginal version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                   ! for VAL__BADI

*  Arguments Given:
      INTEGER N_BOLS
      INTEGER N_JIGS
      INTEGER N_INTS
      REAL    IN_DATA (4, N_BOLS, N_JIGS * N_INTS)
      INTEGER ADC_INDEX (N_BOLS)
      INTEGER CHAN_INDEX (N_BOLS)
      INTEGER INT_INDEX (N_INTS)
      INTEGER ADC
      INTEGER CHAN

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    OUT_DATA (N_INTS)
      REAL    OUT_VARIANCE (N_INTS)
      INTEGER OUT_QUALITY (N_INTS)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL                        ! index of bolometer data in array
      INTEGER I                          ! DO loop index
      INTEGER INT                        ! integration number
      INTEGER JIG                        ! jiggle index
      INTEGER JIG_INDEX                  ! index of JIG in integration INT

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  find the index of the desired bolometer

      BOL = VAL__BADI

      IF (N_BOLS .GT. 0) THEN
         DO I = 1, N_BOLS
            IF ((CHAN .EQ. CHAN_INDEX(I)) .AND.
     :          (ADC .EQ. ADC_INDEX(I)))  THEN
               BOL = I
            END IF
         END DO
      END IF

*  check

      IF (STATUS .EQ. SAI__OK) THEN
         IF (BOL .EQ. VAL__BADI) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_COMPRESS_DEMOD: no data for '//
     :        'requested bolometer', STATUS)
         END IF
      END IF

*  do the calculation

      IF (STATUS .EQ. SAI__OK) THEN
         DO INT = 1, N_INTS
            OUT_DATA (INT) = 0.0
            OUT_VARIANCE (INT) = 0.0
            OUT_QUALITY (INT) = 0

*  add up sum and sum of squares of jiggle measurements

            DO JIG = 1, N_JIGS
               JIG_INDEX = INT_INDEX(INT) + JIG - 1

               IF (NINT(IN_DATA(4,BOL,JIG_INDEX)) .EQ. 0) THEN
                  OUT_QUALITY (INT) = OUT_QUALITY (INT) + 1

                  IF (OUT_QUALITY(INT) .EQ. 1) THEN
                     OUT_VARIANCE (INT) = IN_DATA (2,BOL,JIG_INDEX)
                  ELSE IF (OUT_QUALITY(INT) .EQ. 2) THEN
                     OUT_VARIANCE (INT) = OUT_DATA (INT)**2 +
     :                 IN_DATA (1,BOL,JIG_INDEX)**2
                  ELSE
                     OUT_VARIANCE (INT) = OUT_VARIANCE (INT) +
     :                 IN_DATA (1,BOL,JIG_INDEX) **2
                  END IF

                  OUT_DATA (INT) = OUT_DATA (INT) +
     :              IN_DATA (1,BOL,JIG_INDEX)
               END IF
            END DO

*  OK, now calculate average, etc.

            IF (OUT_QUALITY(INT) .EQ. 0) THEN
               OUT_QUALITY (INT) = 1
            ELSE IF (OUT_QUALITY(INT) .EQ. 1) THEN
               OUT_QUALITY (INT) = 0
            ELSE
               OUT_DATA (INT) = OUT_DATA (INT) / REAL(OUT_QUALITY (INT))
               OUT_VARIANCE (INT) = (OUT_VARIANCE(INT) -
     :           REAL (OUT_QUALITY(INT)) * OUT_DATA(INT)**2) /
     :           REAL (OUT_QUALITY(INT) * (OUT_QUALITY(INT) - 1))
               OUT_QUALITY (INT) = 0
            END IF

         END DO

      END IF

      END
