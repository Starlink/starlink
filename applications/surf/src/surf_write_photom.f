      SUBROUTINE REDS_WRITE_PHOTOM (FD, PARABOLA, MAX_BEAM,
     :  N_BOLS, BOL_CHAN, BOL_ADC, PHOT_BB, MAX_INT, N_INTEGRATIONS, 
     :  PEAK_D, PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT,
     :  MEAS_1_D, MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q,
     :  MEAS_2_D, MEAS_2_V, MEAS_2_Q, STATUS)
*+
*  Name:
*     REDS_WRITE_PHOTOM 

*  Description:
*     Routine to output ASCII results of PHOTOM reduction

*    Description :
*     This routine writes out the results for 1 sub-instrument of a PHOTOM
*     observation.
*     For each bolometer that measured the source:-
*      Bolometer             : <bolometer name>
*      Weight                : <weight to be given to its results>
*
*      Integration   Peak     Peak_sig      Peak_x      Peak_y      Quality
*      <integration> <peak>   <Error>   <x of peak> <y of peak>   <quality>
*                - for all the integrations taken in the observation -
*
*      Measurement results   :
*        Fit to coadded jiggle:
*                    <peak>   <variance>   <x of peak> <y of peak>   <quality>
*        Coadded fit results :
*                    <peak>   <variance>                             <quality>
*

*    Invocation :
*      CALL REDS_WRITE_PHOTOM (FD, MAX_BEAM, 
*     :  N_BOLS, BOL_CHAN, BOL_ADC, PHOT_BB, MAX_INT, N_INTEGRATIONS, 
*     :  PEAK_D, PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT,
*     :  MEAS_1_D, MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q,
*     :  MEAS_2_D, MEAS_2_V, MEAS_2_Q, STATUS)

*    Parameters:
*     FD                     = INTEGER (Given)
*           ASCII file descriptor
*     MAX_BEAM               = INTEGER (Given)
*           the maximum number of bolometers that can observe the source
*           in a single observation
*     PARABOLA               = LOGICAL (Given)
*           True if we fitted the coaaded jiggle with a parabola
*     N_BOLS                 = INTEGER (Given)
*           the number of bolometers used in the sub-instrument
*     BOL_CHAN (N_BOLS)      = INTEGER (Given)
*           the channel numbers of the bolometers observing the object
*     BOL_ADC (N_BOLS)       = INTEGER (Given)
*           the A/D numbers of the bolometers observing the object
*     PHOT_BB (MAX_BEAM)     = INTEGER (Given)
*           the indices of the bolometers used to observe the source in 
*           each beam in the BOL_CHAN and BOL_ADC arrays
*     MAX_INT                = INTEGER (Given)
*           the maximum number of integrations in an observation
*     N_INTEGRATIONS         = INTEGER (Given)
*           the number of integrations in the observation
*     PEAK_D (MAX_INT, MAX_BEAM)
*                            = REAL (Given)
*           the fitted peak value for each integration with each 
*           bolometer
*     PEAK_V (MAX_INT, MAX_BEAM)
*                            = REAL (Given)
*           the variance on PEAK
*     PEAK_X (MAX_INT, MAX_BEAM)
*                            = REAL (Given)
*           the x offset of the fitted peak for each integration with
*           each bolometer
*     PEAK_Y (MAX_INT, MAX_BEAM)
*                            = REAL (Given)
*           the y offset of the fitted peak
*     PEAK_Q (MAX_INT, MAX_BEAM)
*                            = BYTE (Given)
*           the quality of each fitted peak (0 is good)
*     BEAM_WEIGHT (MAX_BEAM) = REAL (Given)
*           the weights assigned to the measurements with each bolometer
*     MEAS_1_D (MAX_BEAM)    = REAL (given)
*           the fitted peak to the coadded integrations
*     MEAS_1_V (MAX_BEAM)    = REAL (Given)
*           the variance on MEAS_1_D
*     MEAS_1_X (MAX_BEAM)    = REAL (Given)
*           the x offset of the peak fitted to the coadd
*     MEAS_1_Y (MAX_BEAM)    = REAL (Given)
*           the y offset
*     MEAS_1_Q (MAX_BEAM)    = BYTE (Given)
*           the quality on MEAS_1_D
*     MEAS_2_D (MAX_BEAM)    = REAL (Given)
*           the coadd of the peaks fitted to the individual integrations
*           for each bolometer
*     MEAS_2_V (MAX_BEAM)    = REAL (Given)
*           the variance on MEAS_2_D
*     MEAS_2_Q (MAX_BEAM)    = BYTE (Given)
*           the quality on MEAS_2_D
*     STATUS                 = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     13-MAR-1996: original version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER       MAX_BEAM
      INTEGER       N_BOLS
      INTEGER       BOL_CHAN (N_BOLS)
      INTEGER       BOL_ADC (N_BOLS)
      INTEGER       PHOT_BB (MAX_BEAM)
      INTEGER       MAX_INT
      INTEGER       N_INTEGRATIONS
      LOGICAL       PARABOLA
      REAL          PEAK_D (MAX_INT, MAX_BEAM)
      REAL          PEAK_V (MAX_INT, MAX_BEAM)
      REAL          PEAK_X (MAX_INT, MAX_BEAM)
      REAL          PEAK_Y (MAX_INT, MAX_BEAM)
      BYTE          PEAK_Q (MAX_INT, MAX_BEAM)
      REAL          BEAM_WEIGHT (MAX_BEAM)
      REAL          MEAS_1_D (MAX_BEAM)
      REAL          MEAS_1_V (MAX_BEAM)
      REAL          MEAS_1_X (MAX_BEAM)
      REAL          MEAS_1_Y (MAX_BEAM)
      BYTE          MEAS_1_Q (MAX_BEAM)
      REAL          MEAS_2_D (MAX_BEAM)
      REAL          MEAS_2_V (MAX_BEAM)
      BYTE          MEAS_2_Q (MAX_BEAM)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER RECLEN                     ! length of record written to file
      PARAMETER (RECLEN = 80)            !
*    Local variables :
      INTEGER            BEAM            ! beam index
      REAL               ERROR           ! SQRT variance
      INTEGER            FD              ! FIO file identifier
      INTEGER            I               ! DO loop index
      INTEGER            ITEMP           ! scratch integer
      CHARACTER*(RECLEN) LINE            ! line to be written to file
      CHARACTER*15       STEMP           ! scratch string
      REAL               STON            ! Signal to noise
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  now output the results

      DO BEAM = 1, MAX_BEAM
         IF (PHOT_BB(BEAM) .NE. 0) THEN
            CALL FIO_WRITE (FD, ' ', STATUS)

            CALL SCULIB_BOLNAME (BOL_ADC(PHOT_BB(BEAM)), 
     :        BOL_CHAN(PHOT_BB(BEAM)), STEMP, STATUS)
            LINE = 'Bolometer: '//STEMP
            CALL FIO_WRITE (FD, LINE, STATUS)

            LINE = 'Weight:    '
            ITEMP = 11
            CALL CHR_PUTR (BEAM_WEIGHT(BEAM), LINE, ITEMP)
            CALL FIO_WRITE (FD, LINE, STATUS)

            CALL FIO_WRITE (FD, ' ', STATUS)
 
            LINE = 'Integration  Peak    Error       S/N      '//
     ;        'Peak_x     Peak_y'
            CALL FIO_WRITE (FD, LINE, STATUS)

            DO I = 1, N_INTEGRATIONS
               IF (PEAK_D(I,BEAM).EQ.VAL__BADR .OR. 
     :              PEAK_Q(I,BEAM) .GT. 0) THEN
                  WRITE(LINE,15) I
               ELSE
                  ERROR = SQRT(PEAK_V(I,BEAM))

*     Protect against division by zero and 0.0/0.0
                  IF (ERROR .GT. 1.0E-10) THEN
                     STON = PEAK_D(I,BEAM)/ERROR
                  ELSE
                     STON = 0.0
                  END IF

                  WRITE (LINE, 10) I, PEAK_D(I,BEAM), ERROR,
     :                 STON, PEAK_X(I,BEAM),
     :                 PEAK_Y(I,BEAM)
               END IF
               CALL FIO_WRITE (FD, LINE, STATUS)
            END DO

	    CALL FIO_WRITE (FD, ' ', STATUS)

            LINE = 'Measurement results:'
            CALL FIO_WRITE (FD, LINE, STATUS)

*       Parabola
            IF (PARABOLA) THEN

               LINE = ' Parabolic fit to coadded jiggle:'
               CALL FIO_WRITE (FD, LINE, STATUS)

               ERROR = SQRT(MEAS_1_V(BEAM))
               IF (ERROR .GT. 1.0E-10) THEN
                  STON = MEAS_1_D(BEAM)/ERROR
               ELSE
                  STON = 0.0
               END IF

               WRITE (LINE,20) MEAS_1_D(BEAM), ERROR, STON,
     :              MEAS_1_X(BEAM), MEAS_1_Y(BEAM)
               CALL FIO_WRITE (FD, LINE, STATUS)

            END IF

            LINE = '  Coadded result of individual integrations:'
            CALL FIO_WRITE (FD, LINE, STATUS)

            ERROR = SQRT(MEAS_2_V(BEAM))
            IF (ERROR .GT. 1.0E-10) THEN
               STON = MEAS_2_D(BEAM)/ERROR
            ELSE
               STON = 0.0
            END IF

            WRITE (LINE,30) MEAS_2_D(BEAM), ERROR, STON
            CALL FIO_WRITE (FD, LINE, STATUS)
         END IF
      END DO

  10  FORMAT (I4, '  ', 2E11.3, ' ', F8.3, 2E11.3)
  15  FORMAT (I4, ' Bad integration')
  20  FORMAT ('      ', 2E11.3, ' ', F8.3, 2E11.3)
  30  FORMAT ('      ', 2E11.3, ' ', F8.3)

      END
