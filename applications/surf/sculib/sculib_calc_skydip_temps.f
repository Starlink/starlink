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

*  Notes:
*     Uses the T_HOT and TARRAY parameters.


*  Copyright:
*     Copyright (C) 1995-2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 March 21 (TIMJ)
*        Original version
*     $Log$
*     Revision 1.9  2004/07/14 21:54:04  timj
*     Now include SURF_PAR from ../src
*
*     Revision 1.8  2000/10/28 03:06:27  timj
*     Catch division by zero error in SCULIB_JNU
*
*     Revision 1.7  2000/06/16 01:26:30  timj
*     - Correct for new format SCULIB_GET_MJD
*     - Correct < to .LT. typo
*
*     Revision 1.6  2000/05/11 20:00:06  timj
*     Add support for defaulting temperature values intelligently.
*     Uses TARRAY parameter
*

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'PRM_PAR'          ! VAL__BAD
      INCLUDE 'SURF_PAR'         ! SURF constants

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

*  Local Variables:
      LOGICAL ADJUST_TCOLD       ! Have we overriden T_COLD
      LOGICAL ARRAY              ! Use array read for T_COLD
      INTEGER BOL                ! Loop counter
      INTEGER COUNT              ! Loop counter
      REAL    EPOCH              ! Julian epoch of observation
      INTEGER EXP_END            ! End of exposure (integration)
      INTEGER EXP_START          ! Star of exposure (integration)
      INTEGER FINISH             ! End of loop for T_COLD
      CHARACTER * 10 FILT_NAME   ! Filter name in use
      INTEGER I                  ! Loop variable
      CHARACTER * 20 INSTRUMENT  ! Instrument name
      INTEGER INTEGRATION        ! Loop variable
      INTEGER ITEMP              ! Temporary integer
      REAL    J_SKY_AV(SCUBA__MAX_SUB)   ! Average sky temperature
      REAL    J_SKY_AV_VARIANCE(SCUBA__MAX_SUB) ! Variance on average Sky temp
      BYTE    J_SKY_AV_QUALITY(SCUBA__MAX_SUB) ! Quality on average sky temp
      INTEGER LOAD               ! Loop variable
      INTEGER MEASUREMENT        ! Loop variable
      DOUBLE PRECISION MJD       ! Modified Julian Date of observation
      INTEGER N_SAMP_IN          ! Number of samples passed to SKYDIP_TEMP
      INTEGER N_SUB              ! Number of sub instruments. Equivalent to
                                 ! N_BOLS
      INTEGER POS                ! Postion in output array
      REAL    RTEMP              ! Scratch real
      INTEGER START              ! Start of loop for T_COLD
      CHARACTER*(80) STEMP       ! Temporary string
      CHARACTER*15 SUB_INSTRUMENT (SCUBA__MAX_SUB)
                                 ! sub-instruments used
      CHARACTER*10 SUB_FILT (SCUBA__MAX_SUB) ! filters of observation
      REAL    SUB_WAVE (SCUBA__MAX_SUB) ! wavelengths of observation
      REAL    T_ASK              ! Temperature read from parameter
      REAL    T_COLD(SCUBA__MAX_SUB) ! Cold temperature
      REAL    T_HOT (SCUBA__MAX_SUB) ! Temperature of the hot load for each sub
      REAL    T_HOT_DELTA(SCUBA__MAX_SUB) ! Adjustment to T_HOT for each sub
      REAL    T_HOT_FITS         ! Temperature of the hot load from FITS header
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     First need to find out the wavelengths and sub-instrument name

      N_SUB = N_BOLS

*     Initialise T_HOT and T_COLD so that we can track errors
*     The zero will deliberately cause error lower down when converted
*     to J temp if not trapped
      DO I = 1, N_SUB
         T_HOT(N_SUB) = 0.0
         T_COLD(N_SUB) = 0.0
      END DO

*     Get the instrument name
      CALL SCULIB_GET_FITS_C( N_FITS, N_FITS, FITS,
     :     'INSTRUME', INSTRUMENT, STATUS)
      CALL CHR_UCASE( INSTRUMENT )

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

*     Get FILTER name
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'FILTER', FILT_NAME, STATUS)

*     Convert to upper case
      CALL CHR_UCASE( FILT_NAME )


*     Construct a filter name list. Include the w and n specifiers
*     as required. Does not work for 450o:850s or 850s:phot
      STEMP = 'FILT_'
      DO I = 1, N_SUB

         ITEMP = 5
         CALL CHR_PUTI(I, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :        STEMP, SUB_FILT(I), STATUS)

*     Compare with the filter name itself and add a 'w' or 'n' as
*     required.
         ITEMP = CHR_LEN(SUB_FILT(I))
         IF (FILT_NAME .EQ. '450N:850N') THEN
            CALL CHR_APPND('N', SUB_FILT(I), ITEMP)
         ELSE IF (FILT_NAME .EQ. '450W:850W') THEN
            CALL CHR_APPND('W', SUB_FILT(I), ITEMP)
         END IF

      END DO

*     Read the hot load temperature from the FITS data and then
*     get the required value from the parameter.
*     Before 19980204 (MJD = 50848) we have to use T_AMB instead.
*     Get the MJD (don't bother about the startup time correction)
      CALL SCULIB_GET_MJD(N_FITS, FITS, -1.0D0, MJD, EPOCH, RTEMP,
     :     STATUS)

      IF (MJD .LT. 50848.0D0 .AND. INSTRUMENT .EQ. 'SCUBA') THEN
         CALL MSG_OUTIF(MSG__NORM, ' ', ' Skydip taken before '//
     :        '19980204. Using T_AMB rather than T_HOT as default.',
     :        STATUS)
         CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, 'T_AMB',
     :        T_HOT_FITS, STATUS)
      ELSE
         CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, 'T_HOT',
     :        T_HOT_FITS, STATUS)
      END IF

      CALL PAR_DEF0R('T_HOT', T_HOT_FITS, STATUS)
      CALL PAR_GET0R('T_HOT', T_HOT_FITS, STATUS)

*     Update the FITS entry
      CALL SCULIB_REWRITE_FITS_R (N_FITS, N_FITS, FITS, 'T_HOT',
     :     T_HOT_FITS, STATUS)


*     Read the COLD temperatures from FITS information
*     We now have to be clever since it is not always true that the
*     cold load temperatures are correct. Prior to 13 March 2000
*     the 450/850 T_COLD values stored in the header are known to
*     be incorrect. After that date, the colod temperatures are correct
*     for 450W:850W but not for 450N:850N. We have no idea about the
*     validity of the cold load temperatures for other wavelengths.

*     Must also determine the hot load temperature offset to be applied.
*     This is sub-instrument dependent in the same way as T_COLD but
*     is a correction on the T_HOT that is stored in the header.

      ADJUST_TCOLD = .FALSE.
      STEMP = 'T_COLD_'
      DO I = 1, N_SUB
         ITEMP = 7
         CALL CHR_PUTI(I, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :        T_COLD(I), STATUS)

*     Now apply a modification to T_COLD if required
*     Adjust T_COLD regardless of date. On 13 March 2000
*     we changed the wide band cold temperatures.
*     but since this does not mean that the other filters will be correct.
*     we change everything for now.

         IF (INSTRUMENT .EQ. 'SCUBA') THEN
            IF (SUB_FILT(I) .EQ. '450W') THEN

               T_COLD(I) = 95.0
               ADJUST_TCOLD = .TRUE. ! Indicate we have overriden default

            ELSE IF (SUB_FILT(I) .EQ. '450N') THEN

               T_COLD(I) = 102.0
               ADJUST_TCOLD = .TRUE. ! Indicate we have overriden default

            ELSE IF (SUB_FILT(I) .EQ. '850W') THEN

               T_COLD(I) = 90.0
               ADJUST_TCOLD = .TRUE. ! Indicate we have overriden default

            ELSE IF (SUB_FILT(I) .EQ. '850N') THEN

               T_COLD(I) = 92.0
               ADJUST_TCOLD = .TRUE. ! Indicate we have overriden default

            END IF

         END IF

      END DO

*     Get all the temperatures. Note that we have to change the prompt
*     as we are asking multiple times (unless TARRAY = TRUE)

*     This bit decides on how many sub instruments we are going to ask
*     questions about. If we are running from reduce_switch we have to
*     process all the subinstruments. Running from SKYDIP we only need
*     to deal with one subinstrument.

*     In order to automate the setting of T_COLD from the command-line
*     or from ORAC-DR we need to be able to specify all the cold temperatures
*     in one go rather than looping round and anulling a parameter.
*     This means that REDUCE_SWITCH needs to use an ARRAY parameter
*     and SKYDIP needs to use a SCALAR. There is a backwards compatibility
*     issue but I feel that hardly anyone runs REDUCE_SWITCH on

      IF (SUB_REQUIRED .EQ. 0 .OR. SUB_REQUIRED .GT. N_SUB) THEN
         START = 1
         FINISH = N_SUB

*     Ask whether we want to supply numbers as an array or a set
*     of scalars. Default to false for backwards compatibility.
         CALL PAR_DEF0L('TARRAY', .FALSE., STATUS)
         CALL PAR_GET0L('TARRAY', ARRAY, STATUS)

      ELSE
         START = SUB_REQUIRED
         FINISH = SUB_REQUIRED
         ARRAY = .FALSE.
      END IF

*     Now need to calculate the override of T_HOT for each sub-instrument
*     Do it here since we are printing output from each sub-instrument
*     and we only want to do that if the sub-inst is selected.
*     This varies with sub-instrument so we have to adjust after the
*     value has been supplied (not modifying the default, just storing
*     the offset). This is because T_HOT is not an array value.
*     Separate this from the T_COLD code since it may have a different
*     time scale to the T_COLD adjustment (this will always be required
*     (modulo BOLOCAM) whereas the T_COLD adjustment will be removable
*     after a certain date. (and the adjustment is only wavelength
*     dependent, not filter dependent)
*     Generate an array of T_HOT
*     This works because START=1 for SKYDIP and REDUCE_SWITCH
*     (If it didnt we would have a problem since the early members
*     would contain undefined values)
*     Do it here since we need START/FINISH. We also want these messages
*     to appear before the warning messages re T_COLD (and before T_COLD
*     is requested) so that T_HOT is dealt with (from the users perspective)
*     before moving on to T_COLD

      DO I = START, FINISH

         CALL MSG_SETC('SUB', SUB_INSTRUMENT(I))

         IF (SUB_FILT(I)(1:3) .EQ. '450') THEN

            T_HOT_DELTA(I) = -3.0
            CALL MSG_SETR('T', T_HOT_DELTA(I))
            CALL MSG_OUTIF(MSG__NORM, ' ', ' Hot load temperature for'//
     :           ' sub ^SUB will be changed by ^T K', STATUS)

         ELSE IF (SUB_FILT(I)(1:3) .EQ. '850') THEN

            T_HOT_DELTA(I) = -1.0
            CALL MSG_SETR('T', T_HOT_DELTA(I))
            CALL MSG_OUTIF(MSG__NORM, ' ', ' Hot load temperature for'//
     :           ' sub ^SUB will be changed by ^T K', STATUS)

         ELSE

            T_HOT_DELTA(I) = 0.0

         END IF

*     Calculate T_HOT for this sub
         T_HOT(I) = T_HOT_FITS + T_HOT_DELTA(I)

      END DO

*     Print informative message to indicate that we have overriden
*     header values
*     Print this after T_HOT has been corrected so as not to confuse
*     the user with warning messages coming out in the wrong order.
      IF (ADJUST_TCOLD) THEN

         CALL MSG_OUTIF(MSG__NORM, ' ', ' Reading default T_COLD '//
     :        'from a lookup table rather than '//
     :        'from the FITS header', STATUS)

      END IF


*     Now, depending on the ARRAY logical we loop over sub-instruments
*     or we read all the values in one go
      IF (ARRAY) THEN

*     Print out message indicating the required order of the temperatures
         CALL MSG_OUTIF(MSG__NORM,' ','Order of sub-instruments with '//
     :        'current cold temps:', STATUS)
         DO I = START, FINISH
            CALL MSG_SETC('SUB', SUB_INSTRUMENT(I))
            CALL MSG_SETR('T_COLD', T_COLD(I))
            CALL MSG_OUTIF(MSG__NORM, ' ', '  ^SUB  - ^T_COLD K',
     :           STATUS)
         END DO

*     Calculate how many we want to read
         I = FINISH - START + 1

*     Now set the default values
         CALL PAR_DEF1R( PARAM, I, T_COLD, STATUS)

*     Now read the required number of values from the parameter
*     Currently assume that START = 1 since we would have to shift
*     the offset of the array otherwsie using %LOC

         IF (START .NE. 1 .AND. STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI('I', START)
            CALL ERR_REP(' ','SCULIB_CALC_SKYDIP_TEMPS: Start '//
     :           'index must be 1 but is ^I. Serious error.',
     :           STATUS)
         END IF

         CALL PAR_EXACR(PARAM, I, T_COLD, STATUS)

      ELSE

         DO I = START, FINISH

*     Set the default value
            CALL PAR_DEF0R(PARAM, T_COLD(I), STATUS)

*     Set the prompt
            STEMP = 'Temperature of cold load for '//SUB_INSTRUMENT(I)
            CALL PAR_PROMT(PARAM, STEMP, STATUS)

*     Get the parameter value and unset it for next time
            CALL PAR_GET0R (PARAM, T_ASK, STATUS )
            CALL PAR_CANCL (PARAM, STATUS)

         END DO

      END IF

*     Now rewrite the FITS header since we want the selected T_COLD values
*     to be stored in the header on exit

      DO I = START, FINISH
         STEMP = 'T_COLD_'
         ITEMP = 7
         CALL CHR_PUTI(I, STEMP, ITEMP)
         CALL SCULIB_REWRITE_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :        T_COLD(I), STATUS)
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

