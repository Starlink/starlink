      SUBROUTINE SCULIB_PROCESS_BOLS(EXTINCTION, N_BEAMS, N_BOL, N_POS, 
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     N_MAP, N_FITS, FITS, DEM_PNTR, LST_STRT,
     :     IN_ROTATION, SAMPLE_MODE, SAMPLE_COORDS, OUT_COORDS,
     :     JIGGLE_REPEAT, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :     JIGGLE_P_SWITCH, RA_CEN, DEC_CEN,
     :     RA1, RA2, DEC1, DEC2, MJD_STANDARD, IN_UT1,
     :     N_POINT, POINT_LST, POINT_DAZ, POINT_DEL, 
     :     NUM_CHAN, NUM_ADC, BOL_ADC, BOL_CHAN, BOL_DU3, BOL_DU4, 
     :     SCAN_REVERSAL, FIRST_LST, SECOND_LST, FIRST_TAU, SECOND_TAU,
     :     BOL_DEC, BOL_RA, NDATA, NVARIANCE, ELEVATION, PAR_ANGLE,
     :     STATUS)
*+
*  Name:
*     SCULIB_PROCESS_BOLS

*  Purpose:
*     Calculate apparent RA/Dec of bolometers and some extra processing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE SCULIB_PROCESS_BOLS(EXTINCTION, N_BEAMS, N_BOL, N_POS, 
*    :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
*    :     N_MAP, N_FITS, FITS,  DEM_PNTR, LST_STRT, 
*    :     IN_ROTATION, SAMPLE_MODE, SAMPLE_COORDS, OUT_COORDS,
*    :     JIGGLE_REPEAT, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
*    :     JIGGLE_P_SWITCH, RA_CEN, DEC_CEN,
*    :     RA1, RA2, DEC1, DEC2, MJD_STANDARD, IN_UT1,
*    :     N_POINT, POINT_LST, POINT_DAZ, POINT_DEL, 
*    :     NUM_CHAN, NUM_ADC, BOL_ADC, BOL_CHAN, BOL_DU3, BOL_DU4, 
*    :     SCAN_REVERSAL, FIRST_LST, SECOND_LST, FIRST_TAU, SECOND_TAU,
*    :     BOL_DEC, BOL_RA, NDATA, NVARIANCE, ELEVATION, PAR_ANGLE,
*    :     STATUS)

*  Description:
*     This routine calculates the apparent RA and Dec of each bolometer
*     for all measurements, integrations and exposures. Both JIGGLE and
*     SCAN data are supported.
*     In addition, if this is called from EXTINCTION (ie EXTINCTION=.TRUE.)
*     the data is corrected for extinction using the calculated elevation
*     of the bolometers. If EXTINCTION is .FALSE. all the bolometers positions
*     are converted to apparent RA-Dec at the reference modified Julian 
*     date (given by MJD_STANDARD).

*  Arguments:
*     EXTINCTION = _LOGICAL (Given)
*        Determines whether the data are corrected for extinction (.TRUE.)
*        or whether the bolometer positions are converted to a standaed MJD.
*     N_BEAMS = _INTEGER (Given)
*        Number of beams in the input data. Only used if EXTINCTION=.TRUE.
*     N_BOL = _INTEGER (Given)
*        Number of bolometers in the input data
*     N_POS = _INTEGER (Given)
*        Number of `samples' taken
*     N_SWITCHES = _INTEGER (Given)
*        Number of switches
*     N_EXPOSURES = _INTEGER (Given)
*        Number of exposures
*     N_INTEGRATIONS = _INTEGER (Given)
*        Number of integrations
*     N_MEASUREMENTS = _INTEGER (Given)
*        Number of measurements
*     N_MAP = _INTEGER (Given)
*        Map number of input data (only used if EXTINCTION=.FALSE.)
*     N_FITS = _INTEGER (Given)
*        Number of FITS items
*     FITS() = _CHAR*80 (Given)
*        FITS array
*     DEM_PNTR() = _INTEGER (Given)
*        DEM_PNTR array - position in file of each exposure
*     LST_STRT() = _DOUBLE (Given)
*        LST of each exposure
*     IN_ROTATION = _DOUBLE (Given)
*        Angle between apparent N and N of input coord system (radians)
*     SAMPLE_MODE = _CHAR (Given)
*        Sample mode of input file
*     SAMPLE_COORDS = _CHAR (Given)
*        Coordinate system of sample offsets
*     OUT_COORDS = _CHAR (Given)
*        Output coordinate system
*     JIGGLE_REPEAT = _INTEGER (Given)
*        Number of times jiggle pattern is repeated in a switch
*     JIGGLE_COUNT = _INTEGER (Given)
*        Number of jiggle in pattern
*     JIGGLE_X(JIGGLE_COUNT) = _REAL (Given)
*        X jiggle offsets (arcsec)
*     JIGGLE_Y(JIGGLE_COUNT) = _REAL (Given)
*        Y jiggle offsets (arcsec)
*     JIGGLE_P_SWITCH = _INTEGER
*        Number of jiggles per switch
*     RA_CEN = _DOUBLE (Given)
*        apparent RA of output map centre (radians) Only used for JIGGLE data.
*     DEC_CEN = _DOUBLE (Given)
*        apparent Dec of output map centre (radians) Only used for JIGGLE data.
*     CENTRE_DU3 = _REAL (Given)
*        dU3 Nasmyth coordinate of point on focal plane that defines 
*        telescope axis.
*     CENTRE_DU4 = _REAL (Given)
*        dU4 Nasmyth coordinate of point on focal plane that defines 
*        telescope axis.
*     RA1 = _REAL (Given)
*        RA at start of scan for each exposure (SCAN only)
*     RA2 = _REAL (Given)
*        RA at end of scan for each exposure (SCAN only)
*     DEC1 = _REAL (Given)
*        DEC at start of scan for each exposure (SCAN only)
*     DEC2 = _REAL (Given)
*        DEC at end of scan for each exposure (SCAN only)
*     MJD_STANDARD = _DOUBLE (Given)
*        Standard MJD to which each input map is referenced (EXTINCTION=FALSE)
*     IN_UT1 = _DOUBLE (Given)
*        MJD of input data.
*     N_POINT = _INTEGER (Given)
*        Number of pointing corrections (should be zero if EXTINCTION)
*     POINT_DEL = _REAL (Given)
*        Elevation pointing corrections (radians) [only if EXTINCTION=FALSE]
*     POINT_DAZ = _REAL (Given)
*        Azimuth pointing corrections (radians) [only if EXTINCTION=FALSE]
*     POINT_LST = _DOUBLE (Given)
*        LST of pointing corrections (radians) [only if EXTINCTION=FALSE]
*     NUM_CHAN = _INTEGER (Given)
*        Number of channels in DAQ
*     NUM_ADC = _INTEGER (Given)
*        Number of AtoD cards.
*     BOL_ADC = _INTEGER (Given)
*        A/D numbers of bolometers measured in input file
*     BOL_CHAN = _INTEGER (Given)
*        channel numbers of bolometers measured in input file
*     BOL_DU3 = _REAL (Given)
*        dU3 Nasmyth coordinates of bolometers
*     BOL_DU4 = _REAL (Given)
*        dU4 Nasmyth coordinates of bolometers
*     SCAN_REVERSAL = LOGICAL (Given)
*        Multiply alternate exposures by -1 if SCANning
*     FIRST_LST = _DOUBLE (Given)
*        LST of first tau value (EXTINCTION only)
*     SECOND_LST = _DOUBLE (Given)
*        LST of second tau value (EXTINCTION only)
*     FIRST_TAU = _REAL (Given)
*        First tau value (EXTINCTION only)
*     SECOND_TAU = _REAL (Given)
*        Second tau value (EXTINCTION only)
*     BOL_DEC(N_BOL, N_POS) = _DOUBLE (Returned)
*         Apparent DEC of bolometers for each measurement for MJD_STANDARD
*     BOL_RA(N_BOL, N_POS) = _DOUBLE (Returned)
*         Apparent RA of bolometers for each measurement for MJD_STANDARD
*     NDATA(N_BOL, N_POS, N_BEAMS) = _REAL (Given & Returned)
*         Extinction corrected data (EXTINCTION only)
*     NVARIANCE(N_BOL, N_POS, N_BEAMS) = _REAL (Given & Returned)
*         Extinction corrected variance (EXTINCTION only)
*     ELEVATION(N_POS) = _DOUBLE (Returned)
*        Elevation of each N_POS offset
*     PAR_ANGLE(N_POS) = _DOUBLE (Returned)
*        PAR_ANGLE of each N_POS offset
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Prior Requirements:

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)

*  History:
*     1997 March 20 (TIMJ)
*        Extract from main tasks

*  Bugs:
*     {note_any_bugs_here}
 
*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER          BOL_ADC (NUM_CHAN * NUM_ADC)
      INTEGER          BOL_CHAN (NUM_CHAN * NUM_ADC)
      REAL             BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL             BOL_DU4 (NUM_CHAN, NUM_ADC)
      REAL             DEC1(N_SWITCHES, N_EXPOSURES,
     :                      N_INTEGRATIONS, N_MEASUREMENTS)
      REAL             DEC2(N_SWITCHES, N_EXPOSURES, 
     :                      N_INTEGRATIONS, N_MEASUREMENTS)
      DOUBLE PRECISION DEC_CEN
      INTEGER          DEM_PNTR(N_EXPOSURES, N_INTEGRATIONS, 
     :                          N_MEASUREMENTS)
      LOGICAL          EXTINCTION
      CHARACTER*(*)    FITS(N_FITS)
      DOUBLE PRECISION FIRST_LST
      REAL             FIRST_TAU
      DOUBLE PRECISION IN_ROTATION
      DOUBLE PRECISION IN_UT1
      INTEGER          JIGGLE_COUNT
      INTEGER          JIGGLE_P_SWITCH
      INTEGER          JIGGLE_REPEAT
      REAL             JIGGLE_X(JIGGLE_COUNT)
      REAL             JIGGLE_Y(JIGGLE_COUNT)
      DOUBLE PRECISION LST_STRT(N_SWITCHES, N_EXPOSURES,
     :                          N_INTEGRATIONS, N_MEASUREMENTS)
      DOUBLE PRECISION MJD_STANDARD
      INTEGER          NUM_CHAN
      INTEGER          NUM_ADC
      INTEGER          N_BEAMS
      INTEGER          N_BOL
      INTEGER          N_EXPOSURES
      INTEGER          N_FITS
      INTEGER          N_INTEGRATIONS
      INTEGER          N_MAP
      INTEGER          N_MEASUREMENTS
      INTEGER          N_POINT
      INTEGER          N_POS
      INTEGER          N_SWITCHES
      CHARACTER *(*)   OUT_COORDS
      REAL             POINT_DAZ(N_POINT)
      REAL             POINT_DEL(N_POINT)
      DOUBLE PRECISION POINT_LST(N_POINT)
      DOUBLE PRECISION RA_CEN
      REAL             RA1(N_SWITCHES, N_EXPOSURES,
     :                     N_INTEGRATIONS, N_MEASUREMENTS)
      REAL             RA2(N_SWITCHES, N_EXPOSURES,
     :                     N_INTEGRATIONS, N_MEASUREMENTS)
      CHARACTER *(*)   SAMPLE_COORDS
      CHARACTER *(*)   SAMPLE_MODE
      LOGICAL          SCAN_REVERSAL
      DOUBLE PRECISION SECOND_LST
      REAL             SECOND_TAU

*     Arguments Returned:      
      DOUBLE PRECISION BOL_DEC(N_BOL, N_POS)
      DOUBLE PRECISION BOL_RA(N_BOL, N_POS)
      DOUBLE PRECISION ELEVATION (N_POS)
      DOUBLE PRECISION PAR_ANGLE (N_POS)

*     Given & Returned
      REAL             NDATA(N_BOL, N_POS, N_BEAMS)
      REAL             NVARIANCE(N_BOL, N_POS, N_BEAMS)

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.14159265359D0)

*  Local Variables:
      LOGICAL          AZNASCAN         ! Am I using RASTER with AZ or NA?
      DOUBLE PRECISION ARRAY_DEC_CENTRE ! apparent DEC of array centre (rads)
      DOUBLE PRECISION ARRAY_RA_CENTRE  ! apparent RA of array centre (rads)
      INTEGER          BEAM             ! Loop counter for N_BEAMS
      REAL             CENTRE_DU3       ! dU3 Nasmyth coordinate of point on 
                                        ! focal plane that defines tel axis
      REAL             CENTRE_DU4       ! dU3 Nasmyth coordinate of point on 
                                        ! focal plane that defines tel axis
      INTEGER          DATA_OFFSET      ! Offset in BOL_RA and BOL_DEC
      REAL             DEC_START        ! declination at start of SCAN 
      REAL             DEC_END          ! declination at end of SCAN
      DOUBLE PRECISION ELTEMP           ! Elevation from CALC_BOL_COORDS
      INTEGER          EXPOSURE         ! Loop counter for N_EXPOSURES
      INTEGER          EXP_END          ! Position of end of exposure
      DOUBLE PRECISION EXP_LST          ! LST of exposure
      REAL             EXP_TIME         ! Exposure time per measurement
      INTEGER          EXP_START        ! Position of start of exposure
      LOGICAL          FLIP             ! Multiply data by -1
      INTEGER          I                ! Loop counter
      INTEGER          INTEGRATION      ! Loop counter for N_INTEGRATIONS
      INTEGER          ITEMP            ! Temp int
      INTEGER          JIGGLE           ! 
      DOUBLE PRECISION LAT_OBS          ! Latitude of observatory (radians)
      DOUBLE PRECISION LST              ! LST of switch
      INTEGER          MEASUREMENT      ! Loop counter for N_MEASUREMENTS
      CHARACTER *(2)   OFFSET_COORDS    ! Coordinate system of offsets
      REAL             OFFSET_X         ! X offset of measurement in OFF_COORDS
      REAL             OFFSET_Y         ! Y offset of measurement in OFF_COORDS
      CHARACTER *(2)   OUTCRDS          ! Coordinate system of output map
      DOUBLE PRECISION PATEMP           ! Par Angle from CALC_BOL_COORDS
      REAL             RA_END           ! RA at end of SCAN
      REAL             RA_START         ! RA at start of SCAN
      REAL             RTEMP            ! Temp real
      REAL             TAUZ             ! Tau at zenith for given Bol elevation
*.

      IF (STATUS .NE. SAI__OK) RETURN


*     Get some values from the FITS array

      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'EXP_TIME', EXP_TIME, STATUS)

      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'CNTR_DU3', CENTRE_DU3, STATUS)
      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'CNTR_DU4', CENTRE_DU4, STATUS)

*     Determine the offset coordinate system

      OFFSET_COORDS = 'RD'

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
         IF (SAMPLE_COORDS .EQ. 'NA' .OR.
     :        SAMPLE_COORDS .EQ. 'AZ') THEN
            OFFSET_COORDS = SAMPLE_COORDS
         END IF
      END IF

*     Determine the output offset coordinates
*     Note that we use RA output coords for NA/AZ output coords if
*     we are using SCAN mode. This is because NA/AZ offsets are calculated
*     later for SCAN/MAP from the tangent plane offsets.

      OUTCRDS = 'RA'
      AZNASCAN = .FALSE.

      IF ((OUT_COORDS .EQ. 'NA')  .OR.
     :     (OUT_COORDS .EQ. 'AZ')) THEN

         IF (SAMPLE_MODE .NE. 'RASTER') THEN
            OUTCRDS = OUT_COORDS
         ELSE
            AZNASCAN = .TRUE.
         END IF

      END IF

*     Set up the default value for flipping
*     This assumes the first scan is the reference
      FLIP = .FALSE.

*     now go through the various exposures of the observation calculating the
*     observed positions

      IF (STATUS .NE. SAI__OK) RETURN

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS
            DO EXPOSURE = 1, N_EXPOSURES

*     find where the exposure starts and finishes in the data array

               CALL SCULIB_FIND_SWITCH (
     :              DEM_PNTR, 1, N_EXPOSURES,
     :              N_INTEGRATIONS, N_MEASUREMENTS,N_POS,
     :              1, EXPOSURE, INTEGRATION, MEASUREMENT,
     :              EXP_START, EXP_END, STATUS)

               IF ((EXP_START .EQ. VAL__BADI) .OR.
     :              (EXP_START .EQ. 0))        THEN
                  CALL MSG_SETI ('E', EXPOSURE)
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUT (' ', 'SCULIB_PROCESS_BOLS: no data '//
     :                 'for exp ^E in int ^I, meas ^M', STATUS)
               ELSE

*     OK, there is some data, first calculate mean LST for the switch
*     sequence making up the exposure

                  EXP_LST = 0.0D0
                  DO I = 1, N_SWITCHES

                     EXP_LST = EXP_LST + LST_STRT(I, EXPOSURE,
     :                    INTEGRATION, MEASUREMENT)

                  END DO
                  EXP_LST = EXP_LST / DBLE (N_SWITCHES)

*     get the scan parameters for a raster map
                  
                  IF (SAMPLE_MODE .EQ. 'RASTER') THEN
                     RA_START = RA1(N_SWITCHES, EXPOSURE, INTEGRATION,
     :                    MEASUREMENT)
                     RA_END   = RA2(N_SWITCHES, EXPOSURE, INTEGRATION,
     :                    MEASUREMENT)
                     DEC_START= DEC1(N_SWITCHES, EXPOSURE, INTEGRATION,
     :                    MEASUREMENT)
                     DEC_END  = DEC2(N_SWITCHES, EXPOSURE, INTEGRATION,
     :                    MEASUREMENT)

*     convert to radians

                     RA_START = RA_START * REAL (PI) / 12.0
                     RA_END = RA_END * REAL (PI) / 12.0
                     DEC_START = DEC_START * REAL (PI) / 180.0
                     DEC_END = DEC_END * REAL (PI) / 180.0
                  END IF

*     cycle through the measurements in the exposure

                  DO I = EXP_START, EXP_END

*     calculate the LST at which the measurement was made (hardly worth the
*     bother because it's averaged over the switches anyway)

                     LST = EXP_LST + DBLE(I - EXP_START) *
     :                    DBLE(EXP_TIME) * 1.0027379D0 * 
     :                    2.0D0 * PI / (3600.0D0 * 24.0D0)

*     work out the offset at which the measurement was made in arcsec
                     
                     IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
                        ARRAY_RA_CENTRE = RA_CEN
                        ARRAY_DEC_CENTRE = DEC_CEN
                        
                        IF (JIGGLE_REPEAT .EQ. 1) THEN
                           JIGGLE = (EXPOSURE-1) *
     :                          JIGGLE_P_SWITCH +
     :                          I - EXP_START + 1
                        ELSE
                           JIGGLE = MOD (I - EXP_START,
     :                          JIGGLE_COUNT) + 1
                        END IF
                        OFFSET_X = JIGGLE_X (JIGGLE)
                        OFFSET_Y = JIGGLE_Y (JIGGLE)

                     ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

*     This has problems if EXP_END = EXP_START
*     Only happens if using SCUOVER
                        IF (EXP_END .EQ. EXP_START .AND.
     :                       I .EQ. EXP_START) THEN

                           ARRAY_RA_CENTRE = DBLE(RA_START)
                           ARRAY_DEC_CENTRE= DBLE(DEC_START)
                        ELSE
                           ARRAY_RA_CENTRE = DBLE (RA_START) +
     :                          DBLE (RA_END - RA_START) *
     :                          DBLE (I - EXP_START) /
     :                          DBLE (EXP_END - EXP_START)
                           ARRAY_DEC_CENTRE = DBLE (DEC_START) +
     :                          DBLE (DEC_END - DEC_START) *
     :                          DBLE (I - EXP_START) /
     :                          DBLE (EXP_END - EXP_START)
                        END IF

                        OFFSET_X = 0.0
                        OFFSET_Y = 0.0
                     END IF

*     EXTINCTION only stores one exposures worth of bol positions

                     IF (EXTINCTION) THEN
                        DATA_OFFSET = 1
                     ELSE
                        DATA_OFFSET = I
                     END IF

*     now call a routine to work out the apparent RA,Dec of the measured
*     bolometers at this position

                     ELTEMP = VAL__BADD
                     PATEMP = VAL__BADD

                     CALL SCULIB_CALC_BOL_COORDS (OUTCRDS, 
     :                    ARRAY_RA_CENTRE, ARRAY_DEC_CENTRE, LST, 
     :                    LAT_OBS, OFFSET_COORDS, OFFSET_X, 
     :                    OFFSET_Y, IN_ROTATION, N_POINT,
     :                    N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :                    NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN,
     :                    BOL_ADC, BOL_DU3, BOL_DU4,
     :                    CENTRE_DU3, CENTRE_DU4,
     :                    BOL_RA(1, DATA_OFFSET), 
     :                    BOL_DEC(1, DATA_OFFSET),
     :                    ELTEMP, PATEMP, STATUS)


                     IF (AZNASCAN) THEN
                        ELEVATION (DATA_OFFSET) = ELTEMP
                        PAR_ANGLE (DATA_OFFSET) = PATEMP
                     END IF


                     IF (EXTINCTION) THEN
*     work out the zenith sky opacity at this LST

                        IF (LST .LE. FIRST_LST) THEN
                           TAUZ = FIRST_TAU
                        ELSE IF (LST .GE. SECOND_LST) THEN
                           TAUZ = SECOND_TAU
                        ELSE
                           TAUZ = FIRST_TAU + (SECOND_TAU-FIRST_TAU) *
     :                          (LST - FIRST_LST) /
     :                          (SECOND_LST - FIRST_LST)
                        END IF
                        
*     correct the bolometer data for the sky opacity

                        DO BEAM = 1, N_BEAMS

                           CALL SCULIB_CORRECT_EXTINCTION (
     :                          NUM_ADC * NUM_CHAN, N_BOL,
     :                          NDATA(1, I, BEAM), 
     :                          NVARIANCE(1, I, BEAM),
     :                          BOL_RA, BOL_DEC, LST, LAT_OBS, TAUZ,
     :                          STATUS)
                        END DO
                     ELSE

*     convert the coordinates to apparent RA,Dec on MJD_STANDARD
                        IF (OUTCRDS .EQ. 'RA') THEN
                           IF (N_MAP .NE. 1) THEN
                              CALL SCULIB_STANDARD_APPARENT (
     :                             N_BOL, BOL_RA(1,DATA_OFFSET), 
     :                             BOL_DEC(1, DATA_OFFSET),
     :                             IN_UT1, MJD_STANDARD, STATUS)
                           END IF
                        END IF
                     END IF
                  END DO

*     If we are using SCAN_REVERSAL then multiply every other
*     exposure by -1

                  IF (SCAN_REVERSAL) THEN
                     IF (FLIP) THEN
                        IF (STATUS .EQ. SAI__OK) THEN
                           ITEMP = N_BOL * (EXP_END - EXP_START + 1)
                           RTEMP = -1.0

                           CALL SCULIB_MULCAR(ITEMP, 
     :                          NDATA(1, EXP_START, 1), RTEMP, 
     :                          NDATA(1, EXP_START, 1))
                        END IF
                        FLIP = .FALSE.
                     ELSE
                        FLIP = .TRUE.
                     END IF
                  END IF

               END IF

            END DO
         END DO
      END DO


      END
