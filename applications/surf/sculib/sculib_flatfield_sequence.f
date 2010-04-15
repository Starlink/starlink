      SUBROUTINE SCULIB_FLATFIELD_SEQUENCE (BOLOMETERS, NUM_CHAN,
     :  NUM_ADC, NUM_SUB, BOL_TYPE, BOL_CALB, BOL_DU3, BOL_DU4,
     :  BOL_QUAL, BOL_ENABLED, BOLS_MEASURED, N_BOLS, BOL_SELECT_CHAN,
     :  BOL_SELECT_ADC, N_SUBS, SUB_INSTRUMENT, FLATREF_CHAN,
     :  FLATREF_ADC, N_MEASUREMENTS, FLAT_CHAN, FLAT_ADC, FLAT_INDEX,
     :  STATUS)
*+
*  Name:
*     SCULIB_FLATFIELD_SEQUENCE

*  Purpose:
*     get bolometer measurement sequence for FLATFIELD

*  Description:
*     This routine works out the bolometer measurement sequence for a FLATFIELD
*     observation. Only one of SCUBA's sub-instruments can be flat-fielded at
*     a time. If the sub-instrument is one of the arrays then the measurement
*     sequence will be ref-bol-ref-bol-......-bol-ref, where the `bol's are the
*     bolometers to be measured and `ref' the reference bolometer on the array.
*     If the sub-instrument is one of the photometry pixels then only that
*     bolometer will be measured.
*
*        Whatever bolometer is the target of the measurement, data will be
*     taken from all A/D channels.
*
*        If status is good on entry SCULIB_BOLSELECT is called to decode
*     the bolometers to be measured and the sub-instruments involved. If
*     the bolometers belong to more than one sub-instrument an error will
*     be reported and bad status returned.
*
*        If the sub-instrument is one of the arrays then the name of the
*     reference bolometer will be read from parameters LONGREF_BOL or
*     SHORTREF_BOL as appropriate. The number of measurements and their
*     sequence is set as described above in N_MEASUREMENTS, FLAT_CHAN and
*     FLAT_ADC. If the sub-instrument is not one of the arrays then the
*     reference bolometer will not be used and the bolometers(s) will be
*     measured in sequence.
*
*        For each measurement of a target bolometer data will be taken from
*     all data channels; BOLS_MEASURED is set to `ALL' and SCULIB_BOLSELECT
*     called to decode this to channel and ADC numbers. A check is made
*     that the reference bolometer, if used, is among those being measured.
*     If not, an error message will be output and bad status returned.
*
*        Lastly, the FLAT_INDEX array is set so that it points to the
*     position in the datablock of data from the target bolometer at each
*     measurement.

*  Invocation:
*     CALL SCULIB_FLATFIELD_SEQUENCE (BOLOMETERS, NUM_CHAN,
*    :  NUM_ADC, NUM_SUB, BOL_TYPE, BOL_CALB, BOL_DU3, BOL_DU4,
*    :  BOL_QUAL, BOL_ENABLED, BOLS_MEASURED, N_BOLS, BOL_SELECT_CHAN,
*    :  BOL_SELECT_ADC, N_SUBS, SUB_INSTRUMENT, FLATREF_CHAN,
*    :  FLATREF_ADC, N_MEASUREMENTS, FLAT_CHAN, FLAT_ADC, FLAT_INDEX,
*    :  STATUS)

*  Arguments:
*     BOLOMETERS                  = CHARACTER*(*)
*           the bolometers to be measured in this FLATFIELD
*     NUM_CHAN                    = INTEGER (Given)
*           the number of channels per A/D
*     NUM_ADC                     = INTEGER (Given)
*           the number of A/Ds
*     NUM_SUB                     = INTEGER (Given)
*           the number of sub-instruments in SCUBA
*     BOL_TYPE (NUM_CHAN,NUM_ADC) = CHARACTER*(*) (Given)
*           the type of each bolometer
*     BOL_CALB (NUM_CHAN,NUM_ADC) = REAL (Given)
*           the flat-field factor for each bolometer
*     BOL_DU3 (NUM_CHAN,NUM_ADC)  = REAL (Given)
*           the dU3 coord of each bolometer
*     BOL_DU4 (NUM_CHAN,NUM_ADC)  = REAL (Given)
*           the dU4 coord of each bolometer
*     BOL_QUAL (NUM_CHAN,NUM_ADC) = INTEGER (Given)
*           the quality of each bolometer
*     BOL_ENABLED (NUM_CHAN,NUM_ADC) = LOGICAL (Returned)
*           .TRUE. if a bolometer is to be measured
*     BOLS_MEASURED               = CHARACTER*(*) (Returned)
*           the bolometers to be measured for each FLATFIELD measurement
*     N_BOLS                      = INTEGER (Returned)
*           the number of bolometers to be measured at each measurement
*     BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC) = INTEGER (Returned)
*           the channel numbers of the bolometers to be measured at each
*           measurement
*     BOL_SELECT_ADC (NUM_CHAN * NUM_ADC) = INTEGER (Returned)
*           the A/D numbers of the bolometers to be measured at each measurement
*     N_SUBS                      = INTEGER (Returned)
*           the number of sub-instruments being used
*     SUB_INSTRUMENT (NUM_SUB)    = CHARACTER*(*) (Returned)
*           name of sub-instrument being used
*     FLATREF_CHAN                = INTEGER (Returned)
*           channel number of reference bolometer
*     FLATREF_ADC                 = INTEGER (Returned)
*           A/D number of reference bolometer
*     N_MEASUREMENTS              = INTEGER (Returned)
*           the number of measurements in this FLATFIELD observation
*     FLAT_CHAN (2 * NUM_CHAN * NUM_ADC) = INTEGER (Returned)
*           the channel number of the target bolometer in each measurement
*     FLAT_ADC (2 * NUM_CHAN * NUM_ADC) = INTEGER (Returned)
*           the A/D number of the target bolometer in each measurement
*     FLAT_INDEX (2 * NUM_CHAN * NUM_ADC) = INTEGER (Returned)
*           the index in the datablock of the target bolometer in each
*           measurement
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (JFL/ROE).

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     25-NOV-1994: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                        ! for VAL__BADI

*  Arguments Given:
      CHARACTER*(*) BOLOMETERS
      INTEGER       NUM_CHAN
      INTEGER       NUM_ADC
      INTEGER       NUM_SUB
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      REAL          BOL_CALB (NUM_CHAN, NUM_ADC)
      REAL          BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL          BOL_DU4 (NUM_CHAN, NUM_ADC)
      INTEGER       BOL_QUAL (NUM_CHAN, NUM_ADC)

*  Arguments Given & Returned:

*  Arguments Returned:
      LOGICAL       BOL_ENABLED (NUM_CHAN, NUM_ADC)
      CHARACTER*(*) BOLS_MEASURED
      INTEGER       N_BOLS
      INTEGER       BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC)
      INTEGER       BOL_SELECT_ADC (NUM_CHAN * NUM_ADC)
      INTEGER       N_SUBS
      CHARACTER*(*) SUB_INSTRUMENT (NUM_SUB)
      INTEGER       FLATREF_CHAN
      INTEGER       FLATREF_ADC
      INTEGER       N_MEASUREMENTS
      INTEGER       FLAT_CHAN (2 * NUM_CHAN * NUM_ADC)
      INTEGER       FLAT_ADC (2 * NUM_CHAN * NUM_ADC)
      INTEGER       FLAT_INDEX (2 * NUM_CHAN * NUM_ADC)

*  Status:
      INTEGER       STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      CHARACTER*3   BOL_NAME                   ! name of bolometer
      INTEGER       FLATREF_INDEX              ! index of reference bolometer
                                               ! in datablock
      INTEGER       I                          ! DO loop index
      INTEGER       ITEMP                      ! scratch integer
      INTEGER       J                          ! DO loop index
      REAL          RTEMP                      ! scratch real
      CHARACTER*15  STEMP                      ! scratch string

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      FLATREF_CHAN = VAL__BADI

      CALL SCULIB_BOLSELECT (BOLOMETERS, BOL_TYPE, BOL_CALB, BOL_DU3,
     :  BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC, 0.0, 0.0,
     :  BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, NUM_SUB,
     :  SUB_INSTRUMENT, N_SUBS, RTEMP, RTEMP, STATUS)

      IF (N_SUBS .NE. 1) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_FLATFIELD_SEQUENCE: only one '//
     :     'sub-instrument can be measured at a time', STATUS)
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((SUB_INSTRUMENT(1) .EQ. 'LONG') .OR.
     :       (SUB_INSTRUMENT(1) .EQ. 'SHORT')) THEN

*  get the channel, ADC of the appropriate reference bolometer

            IF (SUB_INSTRUMENT(1) .EQ. 'LONG') THEN
               CALL PAR_GET0C ('LONGREF_BOL', BOL_NAME, STATUS)
            ELSE
               CALL PAR_GET0C ('SHORTREF_BOL', BOL_NAME, STATUS)
            END IF
            CALL SCULIB_BOLDECODE (BOL_NAME, FLATREF_ADC, FLATREF_CHAN,
     :        STATUS)

*  set up the sequence in which the selected bolometers are to be measured

            N_MEASUREMENTS = 2 * N_BOLS + 1

            DO I = 1, N_BOLS
               FLAT_CHAN (2*I-1) = FLATREF_CHAN
               FLAT_CHAN (2*I) = BOL_SELECT_CHAN (I)
               FLAT_ADC (2*I-1) = FLATREF_ADC
               FLAT_ADC (2*I) = BOL_SELECT_ADC (I)
            END DO

            FLAT_CHAN (2*N_BOLS+1) = FLATREF_CHAN
            FLAT_ADC (2*N_BOLS+1) = FLATREF_ADC

*  set which bolometers are to actually be measured

            call msg_out (' ', 'SCULIB: bodge in '//
     :        'SCULIB_FLATFIELD_SEQUENCE', STATUS)
*            BOLS_MEASURED = 'ALL'
            BOLS_MEASURED = 'LONG'

         ELSE

*  non array bolometers, presumably photometers

            N_MEASUREMENTS = N_BOLS

            DO I = 1, N_BOLS
               FLAT_CHAN (I) = BOL_SELECT_CHAN (I)
               FLAT_ADC (I) = BOL_SELECT_ADC (I)
            END DO

            BOLS_MEASURED = 'ALL'
         END IF
      END IF

*  decode the BOLS_MEASURED string

      STEMP = SUB_INSTRUMENT (1)
      CALL SCULIB_BOLSELECT (BOLS_MEASURED, BOL_TYPE, BOL_CALB,
     :  BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC,
     :  0.0, 0.0, BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, NUM_SUB,
     :  SUB_INSTRUMENT, ITEMP, RTEMP, RTEMP, STATUS)
      SUB_INSTRUMENT (1) = STEMP

      IF (STATUS .EQ. SAI__OK) THEN

*  check that the reference bolometer is among those being measured

         IF (FLATREF_CHAN .NE. VAL__BADI) THEN
            FLATREF_INDEX = VAL__BADI

            DO I = 1, N_BOLS
               IF ((BOL_SELECT_CHAN(I) .EQ. FLATREF_CHAN) .AND.
     :             (BOL_SELECT_ADC(I) .EQ. FLATREF_ADC))   THEN
                  FLATREF_INDEX = I
               END IF
            END DO

            IF (FLATREF_INDEX .EQ. VAL__BADI) THEN
               CALL MSG_SETC ('BOL', BOL_NAME)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_FLATFIELD_SEQUENCE: the '//
     :           'reference bolometer ^BOL is not among those selected',
     :           STATUS)
            END IF
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN

*  calculate FLAT_INDEX, the indices in the data array of the bolometers
*  measured

         DO I = 1, N_MEASUREMENTS
            DO J = 1, N_BOLS
               IF ((FLAT_CHAN(I) .EQ. BOL_SELECT_CHAN(J)) .AND.
     :             (FLAT_ADC(I) .EQ. BOL_SELECT_ADC(J)))  THEN
                  FLAT_INDEX (I) = J
               END IF
            END DO
         END DO

      END IF

      END
