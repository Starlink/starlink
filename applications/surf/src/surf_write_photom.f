*+  REDS_WRITE_PHOTOM - routine to output ASCII results of PHOTOM reduction
      SUBROUTINE REDS_WRITE_PHOTOM (ODF, OBS_DATE, OBS_TIME, ANALYSIS,
     :  RUN_NUMBER, OBJECT, SUB_INSTRUMENT, FILTER, CENTRE_COORDS,
     :  LAT, LONG, LAT2, LONG2, MJD1, MJD2, OFFSET_COORDS, MAP_X,
     :  MAP_Y, SAMPLE_COORDS, SAMPLE_PA, SKY_SUBTRACTION, MAX_BEAM, 
     :  N_BOLS, BOL_CHAN, BOL_ADC, PHOT_BB, MAX_INT, N_INTEGRATIONS, 
     :  PEAK_D, PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT,
     :  MEAS_1_D, MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q,
     :  MEAS_2_D, MEAS_2_V, MEAS_2_Q, STATUS)
*    Description :
*     This routine writes out the results for 1 sub-instrument of a PHOTOM
*     observation.
*        If status is good on entry the routine will call FIO_ASSOC to open
*     the ASCII file to hold the results; the filename will be read from
*     parameter 'FILE'. FIO_WRITE will then be called to write out the results
*     in the following format:-
*
*      Output from REDS reduction of a PHOTOM observation
*      Reduction date        : <date>
*      Observation definition: <ODF name>
*      Date of observation   : <date>
*      Time of observation   : <time>
*      Run number            : <number>
*      Object                : <object>
*      Sub-instrument        : <name>
*      Filter                : <name>
*      Centre coords         : <coord system of centre>
*      Latitude              : <latitude>
*      Longitude             : <longitude>
*
*     If the centre coordinate system is PLANET then:-
*      2nd latitude          : <latitude on MJD2>
*      2nd longitude         : <longitude on MJD2>
*      date of 1st position  : <modified Julian day when source at lat,long>
*      date of 2nd position  : <modified Julian day when source at lat2,long2>
*     end if
*
*      Offset coords         : <coord system of source offset>
*      x offset              : <x offset of source>
*      y offset              : <y offset of source>
*      Sampe coords          : <coord system of jiggle offsets>
*      Sample position angle : <angle that x axis of jiggle offsets is rotated
*                              anticlockwise from the x axis of the sample 
*                              coord system>
*      Sky error removal     : <TRUE if the REDS SKY_ERROR application has been
*                              run on the data>
*      Analysis mode         : AVERAGE or PARABOLA
*    
*     Then, for each bolometer that measured the source:-
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
*     Lastly, FIO_CLOSE is called to close the file.
*    Invocation :
*     CALL REDS_WRITE_PHOTOM (ODF, OBS_DATE, OBS_TIME, RUN_NUMBER,
*    :  OBJECT, SUB_INSTRUMENT, FILTER, CENTRE_COORDS, LAT, LONG,
*    :  LAT2, LONG2, MJD1, MJD2, OFFSET_COORDS, MAP_X, MAP_Y,
*    :  SAMPLE_COORDS, SAMPLE_PA, SKY_SUBTRACTION, MAX_BEAM, N_BOLS,
*    :  BOL_CHAN, BOL_ADC, PHOT_BB, MAX_INT, N_INTEGRATIONS, PEAK_D,
*    :  PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT, MEAS_1_D,
*    :  MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q, MEAS_2_D, MEAS_2_V,
*    :  MEAS_2_Q, STATUS)
*    Parameters :
*     ODF                    = CHARACTER*(*) (Given)
*           the name of the observation definition file
*     OBS_DATE               = CHARACTER*(*) (Given)
*           the date of the observation
*     OBS_TIME               = CHARACTER*(*) (Given)
*           the UT of the observation
*     RUN_NUMBER             = INTEGER (Given)
*           the observation number
*     OBJECT                 = CHARACTER*(*) (Given)
*           the name of the object observed
*     SUB_INSTRUMENT         = CHARACTER*(*) (Given)
*           the name of the sub-instrument used
*     FILTER                 = CHARACTER*(*) (Given)
*           the name of the filter used
*     CENTRE_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the telescope centre coords
*     LAT                    = CHARACTER*(*) (Given)
*           the latitude of the telescope centre
*     LONG                   = CHARACTER*(*) (Given)
*           the longitude of the telescope centre
*     LAT2                   = CHARACTER*(*) (Given)
*           the second source latitude if CENTRE_COORDS is PLANET
*     LONG2                  = CHARACTER*(*) (Given)
*           the second source longitude if CENTRE_COORDS is PLANET
*     MJD1                   = DOUBLE PRECISION (Given)
*           the modified Julian date of LAT,LONG if CENTRE_COORDS is PLANET
*     MJD2                   = DOUBLE PRECISION (Given)
*           the modified Julian date of LAT2,LONG2 if CENTRE_COORDS is PLANET
*     OFFSET_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the map centre offsets
*     MAP_X                  = REAL (Given)
*           the x offset of the map centre
*     MAP_Y                  = REAL (Given)
*           the y offset of the map centre
*     SAMPLE_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the jiggle offsets
*     SAMPLE_PA              = REAL (Given)
*           the position angle by which the x-axis of the jiggle offsets is
*           rotated (anti-clockwise) from the x-axis of the SAMPLE_COORDS
*           system
*     SKY_SUBTRACTION        = LOGICAL (Given)
*           .TRUE. if the sky error has been removed
*     MAX_BEAM               = INTEGER (Given)
*           the maximum number of bolometers that can observe the source
*           in a single observation
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
      CHARACTER*(*) ODF
      CHARACTER*(*) OBS_DATE
      CHARACTER*(*) OBS_TIME
      INTEGER       RUN_NUMBER
      CHARACTER*(*) OBJECT
      CHARACTER*(*) SUB_INSTRUMENT
      CHARACTER*(*) FILTER
      CHARACTER*(*) ANALYSIS
      CHARACTER*(*) CENTRE_COORDS
      CHARACTER*(*) LAT
      CHARACTER*(*) LONG
      CHARACTER*(*) LAT2
      CHARACTER*(*) LONG2
      DOUBLE PRECISION MJD1
      DOUBLE PRECISION MJD2
      CHARACTER*(*) OFFSET_COORDS
      REAL          MAP_X
      REAL          MAP_Y
      CHARACTER*(*) SAMPLE_COORDS
      REAL          SAMPLE_PA
      LOGICAL       SKY_SUBTRACTION
      INTEGER       MAX_BEAM
      INTEGER       N_BOLS
      INTEGER       BOL_CHAN (N_BOLS)
      INTEGER       BOL_ADC (N_BOLS)
      INTEGER       PHOT_BB (MAX_BEAM)
      INTEGER       MAX_INT
      INTEGER       N_INTEGRATIONS
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
      INTEGER            NTICKS          ! number of ticks since some date
      CHARACTER*15       STEMP           ! scratch string
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  open the file

      CALL FIO_ASSOC ('FILE', 'WRITE', 'LIST', 0, FD, STATUS)

*  write header information

      LINE = 'Output from REDS reduction of a PHOTOM observation'
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Reduction date        : '
      CALL PSX_TIME (NTICKS, STATUS)
      CALL PSX_CTIME (NTICKS, LINE(25:), STATUS)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Observation definition: '//ODF
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Date of observation   : '//OBS_DATE
      CALL FIO_WRITE (FD, LINE, STATUS)
 
      LINE = 'Time of observation   : '//OBS_TIME
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Run number            : '
      CALL CHR_ITOC (RUN_NUMBER, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Object                : '//OBJECT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sub-instrument        : '//SUB_INSTRUMENT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Filter                : '//FILTER
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Centre coords         : '//CENTRE_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)
     
      LINE = 'Latitude              : '//LAT
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Longitude             : '//LONG
      CALL FIO_WRITE (FD, LINE, STATUS)

      CALL CHR_UCASE (CENTRE_COORDS)
      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         LINE = '2nd latitude          : '//LAT2
         CALL FIO_WRITE (FD, LINE, STATUS)

	 LINE = '2nd longitude         : '//LONG2
         CALL FIO_WRITE (FD, LINE, STATUS)

         LINE = 'date of 1st position  : '
         CALL CHR_DTOC (MJD1, LINE(25:), ITEMP)
         CALL FIO_WRITE (FD, LINE, STATUS)

         LINE = 'date of 2nd position  : '
         CALL CHR_DTOC (MJD1, LINE(25:), ITEMP)
         CALL FIO_WRITE (FD, LINE, STATUS)
      END IF

      LINE = 'Offset coords         : '//OFFSET_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'x offset              : '
      CALL CHR_RTOC (MAP_X, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'y offset              : '
      CALL CHR_RTOC (MAP_Y, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sample coords         : '//SAMPLE_COORDS
      CALL FIO_WRITE (FD, LINE, STATUS)

      LINE = 'Sample position angle : '
      CALL CHR_RTOC (SAMPLE_PA, LINE(25:), ITEMP)

      LINE = 'Sky error removal     : '
      CALL CHR_LTOC (SKY_SUBTRACTION, LINE(25:), ITEMP)
      CALL FIO_WRITE (FD, LINE, STATUS)
      CALL FIO_WRITE (FD, ' ', STATUS)

      LINE = 'Photometric analysis  : '//ANALYSIS
      CALL FIO_WRITE (FD, LINE, STATUS)

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
                  WRITE (LINE, 10) I, PEAK_D(I,BEAM), ERROR,
     :                 PEAK_D(I,BEAM)/ERROR, PEAK_X(I,BEAM),
     :                 PEAK_Y(I,BEAM)
               END IF
               CALL FIO_WRITE (FD, LINE, STATUS)
            END DO

	    CALL FIO_WRITE (FD, ' ', STATUS)

            LINE = 'Measurement results:'
            CALL FIO_WRITE (FD, LINE, STATUS)

            LINE = ' Parabolic fit to coadded jiggle:'
            CALL FIO_WRITE (FD, LINE, STATUS)

            ERROR = SQRT(MEAS_1_V(BEAM))

            WRITE (LINE,20) MEAS_1_D(BEAM), ERROR, MEAS_1_D(BEAM)/ERROR,
     :        MEAS_1_X(BEAM), MEAS_1_Y(BEAM)
            CALL FIO_WRITE (FD, LINE, STATUS)

            LINE = '  Coadded result of individual integrations:'
            CALL FIO_WRITE (FD, LINE, STATUS)

            ERROR = SQRT(MEAS_2_V(BEAM))
            WRITE (LINE,30) MEAS_2_D(BEAM), ERROR, MEAS_2_D(BEAM)/ERROR
            CALL FIO_WRITE (FD, LINE, STATUS)
         END IF
      END DO

  10  FORMAT (I4, '  ', 2E11.3, ' ', F8.3, 2E11.3)
  15  FORMAT (I4, ' Bad integration')
  20  FORMAT ('      ', 2E11.3, ' ', F8.3, 2E11.3)
  30  FORMAT ('      ', 2E11.3, ' ', F8.3)

*  close the file

      CALL FIO_CLOSE (FD, STATUS)

      END
