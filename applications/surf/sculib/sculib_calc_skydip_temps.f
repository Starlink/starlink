      SUBROUTINE SCULIB_CALC_SKYDIP_TEMPS( N_TEMPS, SUB_REQUIRED,N_FITS,
     :     FITS, PARAM, N_INTEGRATIONS, N_MEASUREMENTS, N_POS, N_BOLS,
     :     MAX_SUB, NUM_CHAN, NUM_ADC, BOL_CHAN, BOL_TYPE, BOL_ADC,
     :     DEM_PNTR,IN_DATA, OUT_DATA, OUT_VAR, OUT_QUAL, OUT_DEM_PNTR, 
     :     OUT_DATA_AV, OUT_VAR_AV, OUT_QUAL_AV, SCRATCH,
     :     STATUS)
*+
*  Name:
*     SCULIB_CALC_SKYDIP_TEMPS
 
*  Purpose:
*     Calculate all the skydip temps from raw data
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SCULIB_CALC_SKYDIP_TEMPS( N_TEMPS, N_FITS, FITS, PARAM,
*    :     N_INTEGRATIONS, N_MEASUREMENTS, N_POS, N_BOLS, DEM_PNTR, 
*    :     IN_DATA, OUT_DATA, OUT_VAR, OUT_QUAL, OUT_DEM_PNTR, 
*    :     OUT_DATA_AV, OUT_VAR_AV, OUT_QUAL_AV, SCRATCH,
*    :     STATUS)
 
*  Description:
*     This routine accepts the SKYDIP (3 dimensional) raw data and
*     works out the temperature associated with each integration.
*     The 2 dimensional array (bolometers, integrations) is then returned.

*  Arguments:
*     N_TEMPS = INTEGER (Given)
*        Number of SKYDIP temperatures.
*     SUB_REQUIRED = INTEGER (Given)
*        The number of the sub-instrument that is requested.
*        If this number is less than 1 or greater than N_BOLS then
*        all subinstrumentes are returned. This should be 0 when called
*        from REDUCE_SWITCH and the required number if called from
*        SKYDIP. Exists purely to prevent multiple requests for T_COLD
*        if we are only interested in one wavelength
*     N_FITS = INTEGER (Given)
*        Number of FITS items
*     FITS = CHARACTER*80 (Given)
*        The FITS items
*     PARAM = CHARACTER*(PAR__SZNAM) (Given)
*        Name of parameter from which T_COLD can be read
*     N_INTEGRATIONS = INTEGER (Given)
*        Number of integrations
*     N_MEASUREMENTS = INTEGER (Given)
*        Number of measurements
*     N_POS = INTEGER (Given)
*        Number of data points
*     N_BOLS = INTEGER (Given)
*        Number of bolometers (sub instruments)      
*     NUM_CHAN                   = INTEGER (Given)
*           number of A/D channels per A/D card
*     NUM_ADC                    = INTEGER (Given)
*           number of A/D cards
*     BOL_TYPE (NUM_CHAN, NUM_ADC)
*                                = CHARACTER*(*) (Given)
*           the types of the bolometers
*     BOL_CHAN (N_BOLS)          = INTEGER (Given)
*           channel numbers of selected bolometers
*     BOL_ADC (N_BOLS)           = INTEGER (Given)
*           ADC numbers of selected bolometers
*     DEM_PNTR(1, 1, N_INTEGRATIONS, N_MEASUREMENTS) = INTEGER (Given)
*        Pointer to positions in the input data
*     IN_DATA(N_TEMPS, N_BOLS, N_POS) = REAL (Given)
*        Input data [NTEMPS, NBOLS, NPOS]
*     OUT_DATA(N_BOLS, N_POS) = REAL (Returned)
*        Processed sky brightness temperatures.
*     OUT_VAR(N_BOLS, N_POS) = REAL (Returned)
*        Variance on each point in OUT_DATA
*     OUT_QUAL(N_BOLS, N_POS) = BYTE (Returned)
*        Quality of OUT_DATA
*     OUT_DEM_PNTR(1, 1, N_INTEGRATIONS, N_MEASUREMENTS) = INTEGER (Returned)
*        Modified DEM_PNTR to reflect changes in data array
*     OUT_DATA_AV(N_MEASUREMENTS, N_BOLS) = REAL (Returned)
*        Average sky temp for each measurement
*     OUT_VAR_AV(N_MEASUREMENTS, N_BOLS) = REAL (Returned)
*        Variance of the Average sky temp for each measurement
*     OUT_QUAL_AV(N_MEASUREMENTS, N_BOLS) = BYTE (Returned)
*        Quality of the Average sky temp for each measurement
*     SCRATCH(N_TEMPS, N_BOLS, N_INTEGRATIONS) = REAL (Given)
*        Work space to store the data for each measurement (since I 
*        cant simply pass a pointer in as the data is non-contiguous)
*     STATUS = INTEGER (Given)
*        Global Status

*  Authors:
*     TIMJ: Tim Jenness (JACH)
 
*  History:
*     1997 March 21 (TIMJ)
*        Original version

*  Bugs:
*     {note_any_bugs_here}
 
*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'PRM_PAR'          ! VAL__BAD

*  External references
      INTEGER  CHR_LEN           ! Length of string
      EXTERNAL CHR_LEN

*  Arguments Given:
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER N_POS
      INTEGER N_BOLS
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      INTEGER BOL_CHAN (N_BOLS)
      INTEGER BOL_ADC (N_BOLS)
      INTEGER MAX_SUB
      INTEGER N_TEMPS
      INTEGER N_FITS
      CHARACTER * (*) FITS
      CHARACTER * (*) PARAM
      INTEGER DEM_PNTR(1,1,N_INTEGRATIONS, N_MEASUREMENTS)
      REAL    IN_DATA(N_TEMPS, N_BOLS, N_POS)
      INTEGER SUB_REQUIRED
      REAL    SCRATCH(N_TEMPS, N_BOLS, N_POS)

*     Arguments Returned:      
      REAL    OUT_DATA(N_BOLS, N_POS)
      REAL    OUT_VAR (N_BOLS, N_POS)
      BYTE    OUT_QUAL(N_BOLS, N_POS)
      INTEGER OUT_DEM_PNTR(1,1,N_INTEGRATIONS, N_MEASUREMENTS)
      REAL    OUT_DATA_AV(MAX_SUB, N_MEASUREMENTS)
      REAL    OUT_VAR_AV (MAX_SUB, N_MEASUREMENTS)
      BYTE    OUT_QUAL_AV(MAX_SUB, N_MEASUREMENTS)


*     Status:
      INTEGER STATUS

*  Local constants:

      INTEGER   SCUBA__MAX_SUB   ! I need SURF_PAR
      PARAMETER (SCUBA__MAX_SUB = 5)

*  Local Variables:
      INTEGER BOL                ! Loop counter
      INTEGER COUNT              ! Loop counter
      INTEGER EXP_END            ! End of exposure (integration)
      INTEGER EXP_START          ! Star of exposure (integration)
      INTEGER FINISH             ! End of loop for T_COLD
      INTEGER I                  ! Loop variable
      INTEGER INTEGRATION        ! Loop variable
      INTEGER ITEMP              ! Temporary integer
      REAL    J_SKY_AV(SCUBA__MAX_SUB)   ! Average sky temperature
      REAL    J_SKY_AV_VARIANCE(SCUBA__MAX_SUB) ! Variance on average Sky temp
      BYTE    J_SKY_AV_QUALITY(SCUBA__MAX_SUB) ! Quality on average sky temp
      INTEGER LOAD               ! Loop variable
      INTEGER MEASUREMENT        ! Loop variable
      INTEGER N_SAMP_IN          ! Number of samples passed to SKYDIP_TEMP
      INTEGER N_SUB              ! Number of sub instruments. Equivalent to
                                 ! N_BOLS
      INTEGER POS                ! Postion in output array
      INTEGER START              ! Start of loop for T_COLD
      CHARACTER*(80) STEMP       ! Temporary string
      CHARACTER*15 SUB_INSTRUMENT (SCUBA__MAX_SUB)
                                 ! sub-instruments used
      REAL    SUB_WAVE (SCUBA__MAX_SUB) ! wavelengths of observation
      REAL    T_ASK              ! Temperature read from parameter
      REAL    T_COLD(SCUBA__MAX_SUB) ! Cold temperature
      REAL    T_HOT              ! Temperature of the hot load
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     First need to find out the wavelengths and sub-instrument name

      N_SUB = N_BOLS

*     Get SUB_instrument list
      STEMP = 'SUB_'
      DO I = 1, N_SUB
         ITEMP = 4
         CALL CHR_PUTI (I, STEMP, ITEMP)

         CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :        STEMP, SUB_INSTRUMENT(I), STATUS)
         CALL CHR_UCASE (SUB_INSTRUMENT(I))

*     Add the '_DC' suffix

         ITEMP = CHR_LEN(SUB_INSTRUMENT(I))
         CALL CHR_APPND('_DC', SUB_INSTRUMENT(I), ITEMP)

      END DO


*     BOL_CHAN and BOL_ADC are set in the demodulated data file so we
*     dont need to do anything here

      
*     Get WAVElength list
      STEMP = 'WAVE_'
      DO I = 1, N_SUB
         ITEMP = 5
         CALL CHR_PUTI (I, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :        STEMP, SUB_WAVE(I), STATUS)
      END DO

* Read the hot load temperature

      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, 'T_HOT',
     :  T_HOT, STATUS)

      
* Read the COLD temperatures from FITS information

      STEMP = 'T_COLD_'
      DO I = 1, N_SUB
         ITEMP = 7
         CALL CHR_PUTI(I, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :        T_COLD(I), STATUS)
      END DO


* Get all the temperatures. Note that we have to change the prompt
*     as we are asking multiple times

*     This bit decides on how many sub instruments we are going to ask
*     questions about. If we are running from reduce_switch we have to
*     process all the subinstruments. Running from SKYDIP we only need
*     to deal with one subinstrument.

      IF (SUB_REQUIRED .EQ. 0 .OR. SUB_REQUIRED .GT. N_SUB) THEN
         START = 1
         FINISH = N_SUB
      ELSE
         START = SUB_REQUIRED
         FINISH = SUB_REQUIRED
      END IF

      DO I = START, FINISH

*     Set the default value
         CALL PAR_DEF0R(PARAM, T_COLD(I), STATUS)


*     Set the prompt
         STEMP = 'Temperature of cold load for '//SUB_INSTRUMENT(I)
         CALL PAR_PROMT(PARAM, STEMP, STATUS)

*     Get the parameter value and unset it for next time
         CALL PAR_GET0R (PARAM, T_ASK, STATUS )
         CALL PAR_CANCL (PARAM, STATUS)

*     Check to see if the value has changed
*     and if it has changed then rewrite the FITS entry

         IF (T_ASK.NE. T_COLD(I)) THEN
            CALL MSG_SETR ('T_COLD', T_COLD(I))
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT(I))
            CALL MSG_OUTIF(MSG__NORM,' ', 
     :           'SKYDIP: Redefining T_COLD from ^T_COLD for sub ^SUB', 
     :           STATUS)
            T_COLD(I) = T_ASK

            STEMP = 'T_COLD_'
            ITEMP = 7
            CALL CHR_PUTI(I, STEMP, ITEMP)
            CALL SCULIB_REWRITE_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :           T_COLD(I), STATUS)

         ENDIF
      END DO

*     Fill the output array with bad values before filling it with good!
*     Do this so that we have a set of data stored in case this was
*     an aborted observation and the output data would then be garbage

      DO BOL = 1, N_BOLS
         DO I = 1, N_POS
            OUT_DATA(BOL,I) = VAL__BADR
            OUT_VAR(BOL,I) = VAL__BADR
            OUT_QUAL(BOL,I)  = 1
         END DO
      END DO 

*     Now we can read in the data and average it all together

      IF (STATUS .EQ. SAI__OK) THEN

      DO MEASUREMENT = 1, N_MEASUREMENTS


*  find where the exposure starts and finishes in the data array
 
         CALL SCULIB_FIND_SWITCH (DEM_PNTR,
     :        1, 1, N_INTEGRATIONS, N_MEASUREMENTS,
     :        N_POS, 1, 1, 1, MEASUREMENT,
     :        EXP_START, EXP_END, STATUS)

*     Check for aborted/incomplete DEM_PNTR

         IF ((EXP_START .EQ. VAL__BADI) .OR.
     :        (EXP_START .EQ. 0) .OR.
     :        (EXP_END .EQ. 0)) THEN

            CALL MSG_SETI ('M', MEASUREMENT)
            CALL MSG_OUT (' ', 'SCULIB_CALC_SKYDIP_TEMPS: no data '//
     :           'in measurement ^M', STATUS)

*     Since there is no data set the quality to bad for all bolometers
            
            DO BOL = 1, N_BOLS
               
               OUT_DATA_AV(BOL, MEASUREMENT) = VAL__BADR
               OUT_VAR_AV(BOL, MEASUREMENT) = VAL__BADR
               OUT_QUAL_AV(BOL, MEASUREMENT) = 1

            END DO
            

         ELSE



*  cycle through the measurements in the exposure

            N_SAMP_IN = EXP_END - EXP_START + 1

            DO LOAD = 1, N_TEMPS
               DO BOL = 1, N_BOLS
                  DO INTEGRATION = EXP_START, EXP_END
                     COUNT = INTEGRATION - EXP_START + 1
                     SCRATCH(LOAD, BOL, COUNT) = IN_DATA(LOAD, BOL, 
     :                    INTEGRATION)
                  END DO
               END DO
            END DO

            POS = EXP_START

            CALL SCULIB_SKYDIP_TEMPERATURES(T_COLD, T_HOT, 
     :           N_SUB, SUB_INSTRUMENT, SUB_WAVE, NUM_CHAN,
     :           NUM_ADC, BOL_TYPE, N_BOLS, 
     :           BOL_CHAN, BOL_ADC, N_SAMP_IN, SCRATCH,
     :           N_INTEGRATIONS, OUT_DATA(1,POS), OUT_VAR(1,POS),
     :           OUT_QUAL(1,POS), J_SKY_AV,
     :           J_SKY_AV_VARIANCE, J_SKY_AV_QUALITY, STATUS)

*     Extract the average data values

            DO BOL = 1, N_BOLS

               OUT_DATA_AV(BOL, MEASUREMENT) = J_SKY_AV(BOL)
               OUT_VAR_AV(BOL, MEASUREMENT) = J_SKY_AV_VARIANCE(BOL)
               OUT_QUAL_AV(BOL, MEASUREMENT) = J_SKY_AV_QUALITY(BOL)

            END DO

         END IF

      END DO

      END IF

*     Fix DEM_PNTR for the output file
*     I know where all the data has gone so I just need to set the 
*     reference

      COUNT = 1

      DO MEASUREMENT = 1, N_MEASUREMENTS

         DO INTEGRATION = 1, N_INTEGRATIONS

            OUT_DEM_PNTR(1,1,INTEGRATION, MEASUREMENT) = COUNT
            COUNT = COUNT + 1

         END DO
         
      END DO




      END

