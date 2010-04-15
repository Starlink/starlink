      SUBROUTINE SURFLIB_PROCESS_BOLS(TSKNAME, N_BEAMS,
     :     N_BOL, N_POS, N_POS_BEAMS, N_SWITCHES, N_EXPOSURES,
     :     N_INTEGRATIONS,
     :     N_MEASUREMENTS,START_EXP, END_EXP, START_INT, END_INT,
     :     START_MEAS, END_MEAS,N_MAP, N_FITS, FITS, DEM_PNTR, LST_STRT,
     :     IN_ROTATION, SAMPLE_MODE, SAMPLE_COORDS, OUT_COORDS,
     :     JIGGLE_REPEAT, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :     JIGGLE_P_SWITCH, FOCAL_STATION, RA_CEN, DEC_CEN,
     :     RA1, RA2, DEC1, DEC2, MJD_STANDARD, IN_UT1,
     :     MJD1, LONG1, LAT1, MJD2, LONG2, LAT2,
     :     LOCAL_COORDS, MAP_X, MAP_Y,
     :     N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :     NUM_CHAN, NUM_ADC, BOL_ADC, BOL_CHAN, BOL_DU3, BOL_DU4,
     :     SCAN_REVERSAL, FIRST_LST, SECOND_LST, FIRST_TAU, SECOND_TAU,
     :     BOL_DEC, BOL_RA, NDATA, NVARIANCE, USE_LST, LST_DATA,
     :     WAVEPLATE_ANG, BOL_IP_DATA,
     :     STATUS)
*+
*  Name:
*     SURFLIB_PROCESS_BOLS

*  Purpose:
*     Calculate apparent RA/Dec of bolometers and some extra processing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_PROCESS_BOLS(TSKNAME, N_BEAMS,
*    :     N_BOL, N_POS, N_POS_BEAMS, N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
*    :     N_MEASUREMENTS,START_EXP, END_EXP, START_INT, END_INT,
*    :     START_MEAS, END_MEAS,N_MAP, N_FITS, FITS, DEM_PNTR, LST_STRT,
*    :     IN_ROTATION, SAMPLE_MODE, SAMPLE_COORDS, OUT_COORDS,
*    :     JIGGLE_REPEAT, JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
*    :     JIGGLE_P_SWITCH, FOCAL_STATION, RA_CEN, DEC_CEN,
*    :     RA1, RA2, DEC1, DEC2, MJD_STANDARD, IN_UT1,
*    :     MJD1, LONG1, LAT1, MJD2, LONG2, LAT2,
*    :     LOCAL_COORDS, MAP_X, MAP_Y,
*    :     N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
*    :     NUM_CHAN, NUM_ADC, BOL_ADC, BOL_CHAN, BOL_DU3, BOL_DU4,
*    :     SCAN_REVERSAL, FIRST_LST, SECOND_LST, FIRST_TAU, SECOND_TAU,
*    :     BOL_DEC, BOL_RA, NDATA, NVARIANCE, USE_LST, LST_DATA,
*    :     WAVEPLATE_ANG, BOL_IP_DATA,
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
*
*     There is some complication for moving sources (use PL output coords):
*
*      For JIGGLE:
*       -[1] Calculate map RA/Dec at new MJD
*       -[2] Convert to tangent plane offsets for this map centre
*
*      For SCAN:
*       -[1] Calculate array centre for reference MJD
*       -[2] Calculate tangent offsets of array centre relative to map centre
*       -[3] Calculate map centre at the interpolated MJD
*       -[4] Work out array centre given new map centre
*
*      This is all because the file stores RA start and end relative
*      to the fixed centre at the start of the observation.

*  Arguments:
*     TSKNAME    = _CHAR (Given)
*        Taskname that is used to determine whether certain parts
*        of the code are executed. Special values are:
*        SCUOVER - when using SCUOVER this prevents the EXP_START
*                  loop from running properly. Only a single set
*                  of bolometer positions are returned.
*        EXTINCTION - corrects the input data for extinction
*        REMIP   - Corrects the input data for instrumental polarisation
*        All other values for this parameter assume that the bolometer
*        positions are to be calculated and returned (converted to
*        a standard MJD)
*     N_BEAMS = _INTEGER (Given)
*        Number of beams in the input data. Only used if EXTINCTION=.TRUE.
*     N_BOL = _INTEGER (Given)
*        Number of bolometers in the input data
*     N_POS = _INTEGER (Given)
*        Number of `samples' taken
*     N_POS_BEAMS = INTEGER (Given & Returned)
*        Number of beam positions to be returned per point. Can only be
*        reduced. If equals 3 then order is M,L,R
*     N_SWITCHES = _INTEGER (Given)
*        Number of switches
*     N_EXPOSURES = _INTEGER (Given)
*        Number of exposures
*     N_INTEGRATIONS = _INTEGER (Given)
*        Number of integrations
*     N_MEASUREMENTS = _INTEGER (Given)
*        Number of measurements
*     START_EXP  = INTEGER (Given)
*        First exposure to process
*     END_EXP    = INTEGER (Given)
*        Last exposure to process
*     START_INT  = INTEGER (Given)
*        First integration to process
*     END_INT    = INTEGER (Given)
*        End integration to process
*     START_MEAS = INTEGER (Given)
*        First measurement to process
*     END_MEAS   = INTEGER (Given)
*        End measurement to process
*     N_MAP = _INTEGER (Given)
*        Map number of input data (not used for EXTINCTION or REMIP)
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
*     FOCAL_STATION = _CHAR (Given)
*        Focal station for the instrument. RNASMYTH, LNASMYTH or CASS
*     RA_CEN = _DOUBLE (Given)
*        apparent RA of output map centre (radians). Used mainly for JIGGLE
*        but also for scan/map data pre Dec 1997
*     DEC_CEN = _DOUBLE (Given)
*        apparent Dec of output map centre (radians) Used mainly for JIGGLE
*        but also for scan/map data pre Dec 1997
*     RA1 = _REAL (Given)
*        RA (in RD, RJ or AZ) at start of scan for each exposure (SCAN only)
*     RA2 = _REAL (Given)
*        RA (in RD, RJ or AZ) at end of scan for each exposure (SCAN only)
*     DEC1 = _REAL (Given)
*        DEC (in RJ, RD or AZ) at start of scan for each exposure (SCAN only)
*     DEC2 = _REAL (Given)
*        DEC (in RD, RJ, AZ) at end of scan for each exposure (SCAN only)
*     MJD_STANDARD = _DOUBLE (Given)
*        Standard MJD to which each input map is referenced (EXTINCTION=FALSE)
*     IN_UT1 = _DOUBLE (Given)
*        MJD of input data.
*     MJD1 = DOUBLE (Given)
*        MJD of first planet position
*     LONG1 = DOUBLE (Given)
*        Longitude (apparent RA) at MJD1
*     LAT1 = DOUBLE (Given)
*        Latitude (apparent Dec) at MJD1
*     MJD2 = DOUBLE (Given)
*        MJD of second planet position
*     LONG2 = DOUBLE (Given)
*        Longitude (apparent RA) at MJD2
*     LAT2 = DOUBLE (Given)
*        Latitude (apparent Dec) at MJD2
*     LOCAL_COORDS = CHARACTER (Given)
*        Coordinate system of offsets
*     MAP_X = DOUBLE (Given)
*        X offset in LOCAL_COORDS (radians)
*     MAP_Y = DOUBLE (Given)
*        Y offset in LOCAL_COORDS (radians)
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
*     BOL_DEC(N_BOL, N_POS, N_POS_BEAMS) = _DOUBLE (Returned)
*         Apparent DEC of bolometers for each measurement for MJD_STANDARD
*         Depending on N_POS_BEAMS can contain M, L and R beam positions.
*     BOL_RA(N_BOL, N_POS, N_POS_BEAMS) = _DOUBLE (Returned)
*         Apparent RA of bolometers for each measurement for MJD_STANDARD
*         Depending on N_POS_BEAMS can contain M, L and R beam positions.
*     NDATA(N_BOL, N_POS, N_BEAMS) = _REAL (Given & Returned)
*         corrected data (EXTINCTION and REMIP only)
*     NVARIANCE(N_BOL, N_POS, N_BEAMS) = _REAL (Given & Returned)
*         Extinction corrected variance (EXTINCTION only)
*     USE_LST = LOGICAL (Given)
*         Do we return the LST positions? (TRUE  then we do)
*     LST_DATA(N_POS) = DOUBLE (Returned)
*         LST for each position.
*     WAVEPLATE_ANG = REAL (Given)
*         waveplate position angle (Nasmyth degrees) (Given & Returned)
*         The supplied value is the nasmyth angle in degrees and the
*         return value the sky rotation angle for each position.
*         (ie parallactic angle - elevation) in degrees (REMIP only)
*     BOL_IP_DATA(8, NUM_CHAN, NUM_ADC) = REAL (Given)
*        The IP data. This array contains the ip data for each
*        bolometer (specified by a combination of CHAN and ADC).
*        The 4 slices represent: P0, Pslope, Theta0 and ThetaSlope (REMIP only)
*        The next 4 slices are for the related variance (same order)
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Prior Requirements:

*  Notes:
*     MAP_X and MAP_Y are only used for JIGGLE modes.
*     The SCAN/MAP offsets are wrapped into the RA1,DEC1,RA2,DEC2
*     numbers by the on-line system.
*     The offsets are added to the map centre every time round the
*     loop. This is because it is possible to have AZ offsets for
*     RA,Dec centres.
*
*     This routine tries to deal with  the different versions of
*     SCUCD (only affects SCAN/MAP data).
*     For version 0:
*
*       RA/Decs of the scan were incorrectly stored as RJ. They
*       are converted to RD before further processing.
*       The coordinate frame of RA1, RA2, DEC1 and DEC2 fro SCAN/MAP
*       depends on the CENTRE_COORDS of the observation. For CENTRE=RD
*       the scan positions are in RD; for CENTRE=RB,RJ,GA the scans
*       are in RJ and for centre=AZ the scans are in AZ.
*       (SCULIB_SCAN_2_RD)

*     For version 1.0:
*
*       A bug was introduced concerning the calculation of the
*       ends of the scans. The bug is recreated and inverted in order
*       to compensate. (SCULIB_FIX_SCAN_V10)

*     Also, LO chopping for jiggle maps was broken until 19980730
*     such that the initial chop pa was calculated in Az but never
*     updated as the sky rotated. This fix is only important for SCUBA2MEM
*     where the positions of the off-beams are returned.

*     For data taken on 19970405 and later the data headers are
*     checked for internal consistency to determine possible clock
*     errors on the acquisition computer. If the error is greater than
*     20 seconds all LST's and the MJD are corrected accordingly.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995-2002 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.
*
*  History:
*     1997 March 20 (TIMJ)
*        Extract from main tasks
*     $Log$
*     Revision 1.16  2005/03/19 01:41:03  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.15  2004/09/01 01:02:48  timj
*     mark SLA_DRANRM as external
*
*     Revision 1.14  2002/09/14 03:58:13  timj
*     Update copyright
*
*     Revision 1.13  2002/03/11 21:18:48  timj
*     - finally fix the problem with SC chopping and SCUBA2MEM [the IF clause was
*     wrong]
*
*     Revision 1.12  2001/02/22 03:12:27  timj
*     Trap AZ centre coords for clock error correction
*
*     Revision 1.11  2001/02/22 02:49:35  timj
*     Don't reverse the sign of DATA when using SCUBA2MEM
*
*     Revision 1.10  2000/08/10 20:59:47  timj
*     Remove code to put the LST into range 0..2PI when applying clock correction
*     since that does not work with sources that cross LST day boundaries.
*
*     Revision 1.9  2000/06/24 01:04:55  timj
*     Calculate and correct for possible clock error
*
*     Revision 1.8  1999/08/19 03:37:49  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.7  1999/08/06 02:29:06  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.6  1999/08/03 19:32:51  timj
*     Add copyright message to header.
*
*     Revision 1.5  1999/07/29 03:50:29  timj
*     Fix call to sculib_calc_apparent
*
*     Revision 1.4  1999/07/14 20:13:33  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.3  1999/07/13 20:55:58  timj
*     Pass ra/dec centre to SCULIB_ADD_CHOP
*
*     Revision 1.2  1999/07/13 06:31:23  timj
*     Correct for CHOP TRACKING problem.
*     Pass in LAT_OBS to some routines.
*
*     Revision 1.1  1999/02/27 04:33:47  timj
*     Transfer from SCULIB_PROCESS_BOLS.
*     Move waveplate and IP information through file.
*
*     Revision 1.15  1998/10/06 21:10:19  timj
*     Check the range of JIGGLE to make sure that it does not exceed the
*     size of the jiggle pattern (happens when DEM_PNTR is truncated)
*
*     Revision 1.14  1998/05/05 21:27:41  timj
*     Only call sculib_fix_scan_v10 when using RJ centre
*
*     Revision 1.13  1998/04/27 20:57:07  timj
*     Fix bug in CHOP_PA calculation (relevant for SCULIB_ADD_CHOP). Also ensure
*     that LO chopping is RJ for SCAN/MAP.
*
*     Revision 1.12  1998/04/25 03:41:15  timj
*     Include fix for incorrect header in version 1.0 of SCUCD.
*

*  Bugs:
*     - Does not deal with chop beam positions for 2 bolometer phot
*     observations.
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER          NUM_CHAN
      INTEGER          NUM_ADC
      INTEGER          N_EXPOSURES
      INTEGER          N_FITS
      INTEGER          N_INTEGRATIONS
      INTEGER          N_MEASUREMENTS
      INTEGER          N_SWITCHES
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
      INTEGER          END_EXP
      INTEGER          END_INT
      INTEGER          END_MEAS
      CHARACTER*(*)    FITS(N_FITS)
      DOUBLE PRECISION FIRST_LST
      REAL             FIRST_TAU
      CHARACTER*(*)    FOCAL_STATION
      DOUBLE PRECISION IN_ROTATION
      DOUBLE PRECISION IN_UT1
      INTEGER          JIGGLE_COUNT
      INTEGER          JIGGLE_P_SWITCH
      INTEGER          JIGGLE_REPEAT
      REAL             JIGGLE_X(JIGGLE_COUNT)
      REAL             JIGGLE_Y(JIGGLE_COUNT)
      DOUBLE PRECISION LST_STRT(N_SWITCHES, N_EXPOSURES,
     :                          N_INTEGRATIONS, N_MEASUREMENTS)
      DOUBLE PRECISION LAT1
      DOUBLE PRECISION LAT2
      CHARACTER*(*)    LOCAL_COORDS
      DOUBLE PRECISION LONG1
      DOUBLE PRECISION LONG2
      DOUBLE PRECISION MAP_X
      DOUBLE PRECISION MAP_Y
      DOUBLE PRECISION MJD_STANDARD
      DOUBLE PRECISION MJD1
      DOUBLE PRECISION MJD2
      INTEGER          N_BEAMS
      INTEGER          N_BOL
      INTEGER          N_MAP
      INTEGER          N_POINT
      INTEGER          N_POS
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
      INTEGER          START_EXP
      INTEGER          START_INT
      INTEGER          START_MEAS
      DOUBLE PRECISION SECOND_LST
      REAL             SECOND_TAU
      CHARACTER*(*)    TSKNAME
      LOGICAL          USE_LST
      REAL             BOL_IP_DATA(8,NUM_CHAN,NUM_ADC)
      REAL             WAVEPLATE_ANG(N_POS)

*  Given & Returned
      REAL             NDATA(N_BOL, N_POS, N_BEAMS)
      REAL             NVARIANCE(N_BOL, N_POS, N_BEAMS)
      INTEGER          N_POS_BEAMS

*  Arguments Returned:
      DOUBLE PRECISION BOL_DEC(N_BOL, N_POS, N_POS_BEAMS)
      DOUBLE PRECISION BOL_RA(N_BOL, N_POS, N_POS_BEAMS)
      DOUBLE PRECISION LST_DATA ( N_POS )


*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DRANRM
      EXTERNAL SLA_DRANRM

*  Local constants:
      DOUBLE PRECISION ARCSEC2RAD ! arcsec 2 radians conversion
      PARAMETER (ARCSEC2RAD = 4.84813681110D-6)
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.14159265359D0)
      DOUBLE PRECISION DR2S     ! Radians to seconds of time
      PARAMETER ( DR2S = 1.37509870831397570104315571553852408797773D4)
      DOUBLE PRECISION D2PI     ! 2 * PI
      PARAMETER ( D2PI = 6.283185307179586476925286766559005768394339 )


*  Local Variables:
      LOGICAL          AZNASCAN         ! Am I using RASTER with AZ or NA?
      LOGICAL          AZNASCAN_PL      ! Is the source moving with Az/NA/SCAN?
      DOUBLE PRECISION ARRAY_DEC_CENTRE ! apparent DEC of array centre (rads)
      DOUBLE PRECISION ARRAY_RA_CENTRE  ! apparent RA of array centre (rads)
      INTEGER          BM               ! Loop counter for beam pos
      INTEGER          BEAM             ! Loop counter for N_BEAMS
      CHARACTER*(1)    BEAMS(5)         ! Beam names
      INTEGER          BOL_COORDS_OFFSET! Offset used in CALC_BOL_COORDS
      CHARACTER*(10)   CENTRE_COORDS    ! Centre coords of observation centre
      REAL             CENTRE_DU3       ! dU3 Nasmyth coordinate of point on
                                        ! focal plane that defines tel axis
      REAL             CENTRE_DU4       ! dU3 Nasmyth coordinate of point on
                                        ! focal plane that defines tel axis
      CHARACTER*(5)    CHOP_CRD         ! Chop coordinate system
      CHARACTER*(20)   CHOP_FUN         ! Chop function
      REAL             CHOP_PA          ! Chop position angle
      REAL             CHOP_THROW       ! Chop in arcseconds
      DOUBLE PRECISION CLOCK_ERR        ! Clock error in radians
      DOUBLE PRECISION CURR_MJD         ! MJD for current sample
      INTEGER          DATA_OFFSET      ! Offset in BOL_RA and BOL_DEC
      REAL             DEC_START        ! declination at start of SCAN
      REAL             DEC_END          ! declination at end of SCAN
      DOUBLE PRECISION DTEMP            ! Scratch double
      DOUBLE PRECISION ELAPSED          ! Elapsed time since start of obsn
      DOUBLE PRECISION ELEVATION        ! Elevation from CALC_BOL_COORDS
      DOUBLE PRECISION ETA              ! Tangent plane Y offset
      INTEGER          EXPOSURE         ! Loop counter for N_EXPOSURES
      INTEGER          EXP_END          ! Position of end of exposure
      DOUBLE PRECISION EXP_LST          ! LST of exposure
      REAL             EXP_TIME         ! Exposure time per measurement
      INTEGER          EXP_START        ! Position of start of exposure
      LOGICAL          EXTINCTION       ! Extinction correcting?
      LOGICAL          FLIP             ! Multiply data by -1
      INTEGER          I                ! Loop counter
      INTEGER          INTEGRATION      ! Loop counter for N_INTEGRATIONS
      INTEGER          ITEMP            ! Temp int
      INTEGER          JIGGLE           !
      DOUBLE PRECISION LAT_OBS          ! Latitude of observatory (radians)
      DOUBLE PRECISION LST              ! LST of switch
      DOUBLE PRECISION MAP_DEC_CEN      ! Dec of Map centre
      DOUBLE PRECISION MAP_RA_CEN       ! RA of map centre
      INTEGER          MEASUREMENT      ! Loop counter for N_MEASUREMENTS
      DOUBLE PRECISION MYLAT            ! Lat for use with MAP_X/Y offsets
      DOUBLE PRECISION MYLONG           ! Long for use with MAP_X/Y offsets
      DOUBLE PRECISION NEW_ARRAY_DEC    ! Map centre of beam
      DOUBLE PRECISION NEW_ARRAY_RA     ! Map centre of beam
      DOUBLE PRECISION NEW_DEC_END      ! Corrected scan pos (Dec/End)
      DOUBLE PRECISION NEW_DEC_START    ! Corrected scan pos (Dec/Start)
      DOUBLE PRECISION NEW_RA_END       ! Corrected scan pos (RA/End)
      DOUBLE PRECISION NEW_RA_START     ! Corrected scan pos (RA/Start)
      INTEGER          NLOOPS           ! Number of loops to calculate clock err
      CHARACTER *(2)   OFFSET_COORDS    ! Coordinate system of offsets
      REAL             OFFSET_X         ! X offset of measurement in OFF_COORDS
      REAL             OFFSET_Y         ! Y offset of measurement in OFF_COORDS
      CHARACTER *(2)   OUTCRDS          ! Coordinate system of output map
      DOUBLE PRECISION PAR_ANGLE        ! Par Angle from CALC_BOL_COORDS
      REAL             RA_END           ! RA at end of SCAN
      REAL             RA_START         ! RA at start of SCAN
      LOGICAL          REMIP            ! Correcting for Instrumental Pol?
      REAL             RTEMP            ! Temp real
      REAL             SCUCD_VERSION    ! Version of SCUCD file
      LOGICAL          SCUOVER          ! Running SCUOVER?
      LOGICAL          SET_STATUS       ! True is status has been set by sub
      LOGICAL          SOME_DATA        ! True if data was found
      INTEGER          STORED_OFFSET    ! Data offset at start of exposure
      REAL             TAUZ             ! Tau at zenith for given Bol elevation
      DOUBLE PRECISION UT1_REF          ! Temp MJD variable
      DOUBLE PRECISION XI               ! Tangent plane x offset
*.

      IF (STATUS .NE. SAI__OK) RETURN

      SOME_DATA = .FALSE.
      SET_STATUS = .FALSE.

*     Set the task flags (they are mutually exclusive)
      EXTINCTION = .FALSE.
      SCUOVER    = .FALSE.
      REMIP      = .FALSE.
      IF (TSKNAME .EQ. 'EXTINCTION') THEN
         EXTINCTION = .TRUE.
      ELSE IF (TSKNAME .EQ. 'REMIP') THEN
         REMIP = .TRUE.
      ELSE IF (TSKNAME .EQ. 'SCUOVER') THEN
         SCUOVER = .TRUE.
      END IF

*     Check input data

      IF (START_MEAS .GT. N_MEASUREMENTS .OR. START_MEAS .LT. 1
     :     .OR. END_MEAS .LT. 1 .OR. END_MEAS .GT. N_MEASUREMENTS) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('SM', START_MEAS)
         CALL MSG_SETI('EM', END_MEAS)
         CALL MSG_SETI('NM', N_MEASUREMENTS)
         CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: Start and end '//
     :        'values for measurements (^SM and ^EM) are  '//
     :        'not consistent with actual number of '//
     :        'measurements (^NM)', STATUS)
      END IF
      IF (START_INT .GT. N_INTEGRATIONS .OR. START_INT .LT. 1
     :     .OR. END_INT .LT. 1 .OR. END_INT .GT. N_INTEGRATIONS) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('SM', START_INT)
         CALL MSG_SETI('EM', END_INT)
         CALL MSG_SETI('NM', N_INTEGRATIONS)

         CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: Start and end '//
     :        'values for integrations (^SM and ^EM) are  '//
     :        'not consistent with actual number of '//
     :        'integrations (^NM)', STATUS)
      END IF

      IF (START_EXP .GT. N_EXPOSURES .OR. START_EXP .LT. 1
     :     .OR. END_EXP .LT. 1 .OR. END_EXP .GT. N_EXPOSURES) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('SM', START_EXP)
         CALL MSG_SETI('EM', END_EXP)
         CALL MSG_SETI('NM', N_EXPOSURES)
         CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: Start and end '//
     :        'values for exposures (^SM and ^EM) are  '//
     :        'not consistent with actual number of '//
     :        'exposures (^NM)', STATUS)
      END IF

*     Sort out beam names

      IF (N_POS_BEAMS .EQ. 1) THEN
         BEAMS(1) = 'M'
      ELSE IF (N_POS_BEAMS .EQ. 2) THEN
         BEAMS(1) = 'L'
         BEAMS(2) = 'R'
      ELSE IF (N_POS_BEAMS .EQ. 3) THEN
         BEAMS(1) = 'M'
         BEAMS(2) = 'L'
         BEAMS(3) = 'R'
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI('N',N_POS_BEAMS)
            CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: The requested '//
     :           'number of beams (^N) is out of range (1-3)',
     :           STATUS)
         END IF
      END IF

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

*     Cheat a little bit here and read CENTRE_COORDS from the
*     FITS header rather than passing it in as an argument.

      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'CENT_CRD', CENTRE_COORDS, STATUS)

*     Get the file version number
      CALL SCULIB_GET_FITS_R(N_FITS, N_FITS, FITS,
     :     'VERSION', SCUCD_VERSION, STATUS)

*     Need some information on the CHOP
      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'CHOP_THR', CHOP_THROW, STATUS)
      CHOP_THROW = CHOP_THROW * REAL(ARCSEC2RAD)
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'CHOP_CRD', CHOP_CRD, STATUS)
      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'CHOP_PA', CHOP_PA, STATUS)
      CHOP_PA = CHOP_PA * REAL(PI) / 180.0

      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'CHOP_FUN', CHOP_FUN, STATUS)

*     Check for an inconsistency in the times stored in the
*     headers. We do this by comparing the AZ/EL in the header
*     to the LST in the header - see also SURF_SCUCLKERR
*     We can only do this for data taken 19970405 or later
*     (MJD 50543)

      IF (IN_UT1 .GT. 50543.0D0) THEN

*     For PLANET observation we need to iterate over the calculation
*     of the clock error since the source is moving and we need to
*     work out where the apparent RA/Dec of the moving source taking
*     into account the clock error. Only need to loop once for stationary
*     objects and twice for moving sources

         IF (CENTRE_COORDS .EQ. 'PLANET') THEN
            NLOOPS = 2
         ELSE
            NLOOPS = 1
         END IF

*     Reset clock error and store the uncorrected MJD
         CLOCK_ERR = 0.0D0
         UT1_REF = IN_UT1

         DO I = 1, NLOOPS

*     Calculate new MJD (no change first time through loop)
            IN_UT1 = UT1_REF + ( CLOCK_ERR / D2PI )

*     For planet observations we need to recalculate the
*     apparent ra/dec since the time has changed

            IF (CENTRE_COORDS .EQ. 'PLANET') THEN

               CALL SCULIB_CALC_APPARENT (LAT_OBS, LONG1, LAT1,
     :              LONG2, LAT2, 0.0D0, 0.0D0,
     :              CENTRE_COORDS, LST_STRT, IN_UT1,
     :              MJD1, MJD2, RA_CEN, DEC_CEN, IN_ROTATION,
     :              STATUS)

            END IF

*     Now get the clock error - ignore this if centre coords
*     is AZ since we are then not tracking and we can therfore
*     not determine the error from the headers
*     Alternative to this is to make SCULIB_CALC_CLOCKERR
*     ignore AZ or check status and flush
            IF (CENTRE_COORDS .EQ. 'AZ') THEN
               CALL MSG_OUTIF(MSG__NORM, ' ', 'PROCESS_BOLS: '//
     :              'Clock error can not be determined when '//
     :              'centre coords is set to AZ', STATUS)
            ELSE
               CALL SCULIB_CALC_CLOCKERR( N_FITS, FITS, RA_CEN,
     :              LST_STRT, CLOCK_ERR, DTEMP, STATUS )
            END IF

         END DO

*     If the correction is greater than 20 seconds and less than
*     about 1000 secs we apply the correction. Else we either do not
*     want to bother (the accuaracy of the calculation means that a
*     20 seconds error is not known well enough to make a difference) or
*     the value is so large we dont believe it)

         RTEMP = CLOCK_ERR * DR2S ! Convert to seconds
         IF (RTEMP .GT. 1000.0) THEN

            CALL MSG_SETR( 'ERR', RTEMP)
            CALL MSG_OUTIF(MSG__QUIET, ' ','PROCESS_BOLS: Clock error'//
     :           ' calculated to be ^ERR seconds. Too large so '//
     :           'ignoring.', STATUS)

         ELSE IF ( RTEMP .GT. 20.0) THEN

            CALL MSG_SETR( 'ERR', RTEMP)
            CALL MSG_OUTIF(MSG__VERB, ' ','PROCESS_BOLS: Correct '//
     :           'times by adding ^ERR seconds', STATUS)

*     Calculate the new MJD reference
            IN_UT1 = UT1_REF + ( CLOCK_ERR / D2PI )

*     Now calculate the new LST values
            DO MEASUREMENT = 1, N_MEASUREMENTS
               DO INTEGRATION = 1, N_INTEGRATIONS
                  DO EXPOSURE = 1, N_EXPOSURES
                     DO I = 1, N_SWITCHES

*     Add clock error (do not want to correct range to 0..2PI
*     since that will not distinguish between today and tomorrow)
                        LST_STRT(I,EXPOSURE,INTEGRATION,MEASUREMENT) =
     :                    LST_STRT(I,EXPOSURE,INTEGRATION,MEASUREMENT) +
     :                    CLOCK_ERR
                     END DO
                  END DO
               END DO
            END DO

         END IF

      END IF

*     Decide whether we have to fix the chop tracking bug or not
*     The chop tracking bug for LO Jiggle maps was fixed on
*     19981230 - the specified MJD is from the observation 104 taken
*     on that night to prove the fix.
      IF (IN_UT1 .LT. 51024.805D0 .AND. SAMPLE_MODE .EQ. 'JIGGLE'
     :     .AND. CHOP_CRD .EQ. 'LO') THEN

*     Okay this data requires the chop track fix for the off-beams
*     first need to calculate the Azimuth angle for the start of the
*     observation - use CALC_BOL_COORDS since that already calculates
*     par_angle

         CALL SCULIB_CALC_BOL_COORDS('AZ', FOCAL_STATION,RA_CEN,DEC_CEN,
     :        LST_STRT(1,1,1,1), LAT_OBS, 'AZ', 0.0, 0.0, 0.0D0,
     :        0, 0, 0.0D0, 0.0, 0.0, 0, 0, 0, 0, 0, 0.0, 0.0,
     :        0.0, 0.0, 0.0D0, 0.0D0, ELEVATION, PAR_ANGLE, STATUS)

*     The PAR_ANGLE as returned can now be treated as the new
*     CHOP_PA in a AZ coordinate frame
         CHOP_PA = SNGL(PAR_ANGLE) - CHOP_PA
         CHOP_CRD = 'AZ'

*     If this information is relevant, print some information
         IF (N_POS_BEAMS .NE. 1 .OR. BEAMS(1) .NE. 'M') THEN
            CALL MSG_SETR('CHOPPA', CHOP_PA * 180.0 / SNGL(PI) )
            CALL MSG_OUT(' ','SURFLIB_PROCESS_BOLS: Chop tracking'//
     :           ' bug detected - converting chop parameters to'//
     :           ' AZ at PA ^CHOPPA', STATUS)
         END IF

      END IF

*     Translate LO to a real coordinate system
*     Remember that for SCAN map data LO ALWAYS means RJ
      IF (CHOP_CRD .EQ. 'LO') THEN
         IF (SAMPLE_MODE .EQ. 'RASTER') THEN
            CHOP_CRD = 'RJ'
         ELSE
            CHOP_CRD = LOCAL_COORDS
         END IF
      END IF

*     Divide chop throw by two if we are doing a SCAN/MAP
*     (since the middle beam is not a real beam)

      IF (SAMPLE_MODE .EQ. 'RASTER') THEN
         CHOP_THROW = CHOP_THROW / 2.0
      END IF

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

*     Check if I am using a moving source with NA or AZ

      AZNASCAN_PL = .FALSE.

      IF (AZNASCAN) THEN
         IF (MJD1 .GT. 1.0D0 .AND. MJD2 .GT. MJD1) THEN
            AZNASCAN_PL = .TRUE.
         END IF
      END IF


*     Set up the default value for flipping
*     This assumes the first scan is the reference
      FLIP = .FALSE.

*     Store the map centre
      MAP_RA_CEN = RA_CEN
      MAP_DEC_CEN = DEC_CEN

*     Set the DATA OFFSET to 1. This is needed if I want to start
*     calculation from some arbritrary integration. If a subset of
*     ints/exps etc is specified then the data is still stored
*     sequentially. This will cause confusion in other routines
*     if you start doing this sort of thing
*     This feature is really to give SCUOVER more flexibility. REBIN
*     and EXTINCTION should not notice a change.
*     Do not confuse START_EXP with EXP_START. START_EXP is the exposure
*     to start with, whereas EXP_START is the position in the data
*     at which the exposure starts.

      DATA_OFFSET = 0

*     now go through the various exposures of the observation calculating the
*     observed positions

      IF (STATUS .NE. SAI__OK) RETURN

      DO MEASUREMENT = START_MEAS, END_MEAS
         DO INTEGRATION = START_INT, END_INT
            DO EXPOSURE = START_EXP, END_EXP

*     Check status at start of loop
               IF (STATUS .NE. SAI__OK .AND. .NOT. SET_STATUS) THEN
                  CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: Error '//
     :                 'occurred whilst looping', STATUS)
                  SET_STATUS = .TRUE.
               ELSE

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
                  CALL MSG_OUT (' ', 'SURFLIB_PROCESS_BOLS: no data '//
     :                 'for exp ^E in int ^I, meas ^M', STATUS)

*     Else check for the possibility that EXP_END is too large
*     (eg aborted data) where EXP_END - EXP_START > JIGGLE_COUNT
*     This is only valid for jiggle maps though...

               ELSE

*     OK, there is some data, first calculate mean LST for the switch
*     sequence making up the exposure

                  SOME_DATA = .TRUE.   ! I have processed some data

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
*     This assumes that RA is apparent RA/Dec

                     RA_START = RA_START * REAL (PI) / 12.0
                     RA_END = RA_END * REAL (PI) / 12.0
                     DEC_START = DEC_START * REAL (PI) / 180.0
                     DEC_END = DEC_END * REAL (PI) / 180.0


*     If we are at version 1.0 of SCUCD we need to correct these
*     header values so long as this  is not a moving source
*     or we have an RD centre. RD centre should work anyway.
*     Only appears to be a problem with RJ data (at least
*     we can only correct RJ data and the problem was never
*     noticed with RB scan data)

                     IF (SCUCD_VERSION .EQ. 1.0
     :                    .AND. CENTRE_COORDS .EQ. 'RJ'
     :                    .AND. CENTRE_COORDS .NE. 'RD'
     :                    .AND. CENTRE_COORDS .NE. 'PLANET') THEN

                        CALL SCULIB_FIX_SCAN_V10(CENTRE_COORDS,
     :                       LAT_OBS, RA_CEN, DEC_CEN, IN_UT1,
     :                       DBLE(RA_START), DBLE(DEC_START),
     :                       DBLE(RA_END), DBLE(DEC_END),
     :                       NEW_RA_START, NEW_DEC_START,
     :                       NEW_RA_END, NEW_DEC_END,
     :                       STATUS)

                        RA_START = SNGL(NEW_RA_START)
                        RA_END   = SNGL(NEW_RA_END)
                        DEC_START = SNGL(NEW_DEC_START)
                        DEC_END  = SNGL(NEW_DEC_END)

                     END IF

                  END IF

*     Store the first position of this exposure (in the data array

                  STORED_OFFSET = DATA_OFFSET + 1

*     Add kludge for SCUOVER
*     Only want to calculate the position for the first sample of the
*     exposure. Note the array is displayed for the start of a SCAN
*     in SCAN/MAP or the zero jiggle offset for JIGGLE/MAP.

                  IF (SCUOVER) THEN
                     EXP_END = EXP_START
                  END IF

*     cycle through the measurements in the exposure

                  DO I = EXP_START, EXP_END

*     Increment the data position
                     DATA_OFFSET = DATA_OFFSET + 1

*     calculate the LST at which the measurement was made (hardly worth the
*     bother because it's averaged over the switches anyway)

                     LST = EXP_LST + DBLE(I - EXP_START) *
     :                    DBLE(EXP_TIME) * 1.0027379D0 *
     :                    2.0D0 * PI / (3600.0D0 * 24.0D0)


*     Store the LST

                     IF (USE_LST) LST_DATA(I) = LST


*     Now need to work out the actual map centre at this time if
*     we are rebinning a moving source (PL)
*     Also need to do this if I am rebinning AZ or NA with SCAN map.

                     IF (OUT_COORDS .EQ. 'PL' .OR. AZNASCAN_PL) THEN

*     First calculate the elapsed time since the start of the observation
*     Note that we have to be careful about LST wrapping round to zero
*     part way through an observation.
*     In this case if LST is less than the start LST I assume that we
*     have been observing long enough that LST has wrapped round.
*     Hopefully this will be okay unless we take an obvservation lasting
*     for 24 hours!

                        ELAPSED = LST - LST_STRT(1,1,1,1)

                        IF (ELAPSED .LT. 0.0D0) THEN
                           ELAPSED = (2.0D0 * PI) + ELAPSED
                        END IF

*     Convert this to days
                        ELAPSED = ELAPSED / (2.0D0 * PI)


*     Current MJD is MJD at start of observation plus the elapsed
*     time to this exposure. This is only correct if the IN_UT1 refers
*     to the first LST_STRT entry (should be okay as of V1.6 of SURF
*     where the startup time is calculated)

                        CURR_MJD = IN_UT1 + ELAPSED

                        IF (MJD2 .EQ. MJD1) THEN
                           MAP_RA_CEN = LONG1
                           MAP_DEC_CEN = LAT1
                        ELSE

                           MAP_RA_CEN =  LONG1 + (LONG2 - LONG1) *
     :                          (CURR_MJD - MJD1)/ (MJD2 - MJD1)
                           MAP_DEC_CEN = LAT1 + (LAT2 - LAT1) *
     :                          (CURR_MJD - MJD1) / (MJD2 - MJD1)
                        END IF
                     END IF

*     work out the offset at which the measurement was made in arcsec

                     IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
                        ARRAY_RA_CENTRE = MAP_RA_CEN
                        ARRAY_DEC_CENTRE = MAP_DEC_CEN

*     add on the offsets if needed
                        IF ((MAP_X .NE. 0.0D0) .OR.
     :                       (MAP_Y .NE. 0.0D0)) THEN

*     convert to position in local_coords frame
*     at IN_UT1 and current LST
*     Extinction must pass in the correct UT even if it never
*     needs to deal with MJD_STANDARD.
*     Planets should be done in RD local coords
                           CALL SCULIB_APPARENT_2_MP(MAP_RA_CEN,
     :                          MAP_DEC_CEN, LOCAL_COORDS,
     :                          LST, IN_UT1, LAT_OBS, MYLONG, MYLAT,
     :                          STATUS)

*     now add on the tangent plane offset in this frame and convert
*     back to apparent RA,Dec
                           CALL SCULIB_CALC_APPARENT(LAT_OBS, MYLONG,
     :                          MYLAT, 0.0D0, 0.0D0, MAP_X, MAP_Y,
     :                          LOCAL_COORDS, LST, IN_UT1, 0.0D0, 0.0D0,
     :                          ARRAY_RA_CENTRE, ARRAY_DEC_CENTRE,
     :                          DTEMP, STATUS)

                        END IF

                        IF (JIGGLE_REPEAT .EQ. 1) THEN
                           JIGGLE = (EXPOSURE-1) *
     :                          JIGGLE_P_SWITCH +
     :                          I - EXP_START + 1
                        ELSE
                           JIGGLE = MOD (I - EXP_START,
     :                          JIGGLE_COUNT) + 1
                        END IF

*     Check that offset is in range

                        IF (JIGGLE .LE. JIGGLE_COUNT) THEN
                           OFFSET_X = JIGGLE_X (JIGGLE)
                           OFFSET_Y = JIGGLE_Y (JIGGLE)
                        ELSE
                           IF (STATUS .EQ. SAI__OK) THEN
                              STATUS = SAI__ERROR
                              CALL MSG_SETI('JIG', JIGGLE)
                              CALL MSG_SETI('COUNT',JIGGLE_COUNT)
                              CALL ERR_REP(' ',
     :                             'SURFLIB_PROCESS_BOLS: '//
     :                             'Jiggle index out of range - was ' //
     :                             '^JIG when max is ^COUNT', STATUS)
                              CALL MSG_SETI ('E', EXPOSURE)
                              CALL MSG_SETI ('I', INTEGRATION)
                              CALL MSG_SETI ('M', MEASUREMENT)
                              CALL ERR_REP(' ',
     :                             'SURFLIB_PROCESS_BOLS: '//
     :                             '- for exp ^E in int ^I, meas ^M',
     :                             STATUS)

                           END IF
                        END IF


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

*     This array centre now has to be converted to apparent RA/DEC
*     The on-line system uses different coordinate systems for the
*     start and end of the scans depending on the CENTRE_COORDS
*     so we need to convert the map centres to Apparent RA/Dec
*     before proceeding.
*     If CENTRE_COORDS is 'PLANET' then we do nothing since it is
*     already in apparent RA/Dec.
*     In fact the modification is really small.
*     After December 1997 the on-line system really does store apparent
*     RA/Dec correctly so this is not required (version 1.0 of the
*     SCUCD file).

                        CALL SCULIB_SCAN_2_RD(SCUCD_VERSION,
     :                       CENTRE_COORDS, MAP_RA_CEN, MAP_DEC_CEN,
     :                       ARRAY_RA_CENTRE, ARRAY_DEC_CENTRE,
     :                       LST, IN_UT1, LAT_OBS,
     :                       ARRAY_RA_CENTRE, ARRAY_DEC_CENTRE,
     :                       STATUS)

                        OFFSET_X = 0.0
                        OFFSET_Y = 0.0

*     Now need to calculate tangent plane offsets of this array centre
*     from the reference map centre stored in RA_CEN and DEC_CEN
*     This implies that shift will not work unless I pass it into
*     this routine at the top

                        IF (OUT_COORDS .EQ. 'PL' .OR. AZNASCAN_PL) THEN

                           XI = ARRAY_RA_CENTRE
                           ETA = ARRAY_DEC_CENTRE

                           CALL SCULIB_APPARENT_2_TP(1, XI,
     :                          ETA, RA_CEN, DEC_CEN,
     :                          0.0D0, 0.0D0, 0.0D0, STATUS)

*     These offsets are actually the offsets from the new map centre
*     (and not from the RA_CEN,DEC_CEN!) This is what we think the
*     on-line system is giving us.
*     Now need to convert these tangent plane offsets from moving map centre
*     to apparent RA/Dec.

                           CALL SLA_DTP2S (XI, ETA, MAP_RA_CEN,
     :                          MAP_DEC_CEN, ARRAY_RA_CENTRE,
     :                          ARRAY_DEC_CENTRE)

                        END IF

                     END IF

*     EXTINCTION and REMIP only store one exposures worth of bol positions
*     Can only get away with this because neither EXTINCTION nor REMIP use
*     the bolometer positions returned by CALC_BOL_COORDS

                     IF (EXTINCTION .OR. REMIP) THEN
                        BOL_COORDS_OFFSET = 1
                     ELSE
                        BOL_COORDS_OFFSET = DATA_OFFSET
                     END IF

*     Loop over each beam for the output positions

                     DO BM = 1, N_POS_BEAMS

*     Add on the chop throw to the map centre

                        IF (BEAMS(BM) .NE. 'M') THEN

                           CALL SCULIB_ADD_CHOP(
     :                          BEAMS(BM), RA_CEN, DEC_CEN,
     :                          ARRAY_RA_CENTRE, ARRAY_DEC_CENTRE,
     :                          CHOP_CRD, CHOP_PA, CHOP_FUN, CHOP_THROW,
     :                          LST, IN_UT1, LAT_OBS,
     :                          RA_START, RA_END, DEC_START, DEC_END,
     :                          NEW_ARRAY_RA, NEW_ARRAY_DEC,
     :                          STATUS)

                        ELSE

                           NEW_ARRAY_RA = ARRAY_RA_CENTRE
                           NEW_ARRAY_DEC = ARRAY_DEC_CENTRE

                        END IF

*     For each beam position I calculate a new map centre
*     and then calculate bolometer offsets from that.
*     Essentially just another LOCAL_COORDS addition (with the
*     complication of SC and CHOP_PA)


*     now call a routine to work out the apparent RA,Dec of the measured
*     bolometers at this position

                        ELEVATION = VAL__BADD
                        PAR_ANGLE = VAL__BADD

                        CALL SCULIB_CALC_BOL_COORDS (OUTCRDS,
     :                       FOCAL_STATION,
     :                       NEW_ARRAY_RA, NEW_ARRAY_DEC, LST,
     :                       LAT_OBS, OFFSET_COORDS, OFFSET_X,
     :                       OFFSET_Y, IN_ROTATION, N_POINT,
     :                       N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :                       NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN,
     :                       BOL_ADC, BOL_DU3, BOL_DU4,
     :                       CENTRE_DU3, CENTRE_DU4,
     :                       BOL_RA(1, BOL_COORDS_OFFSET, BM),
     :                       BOL_DEC(1, BOL_COORDS_OFFSET, BM),
     :                       ELEVATION, PAR_ANGLE, STATUS)

                        IF (EXTINCTION) THEN
*     work out the zenith sky opacity at this LST

                           IF (LST .LE. FIRST_LST) THEN
                              TAUZ = FIRST_TAU
                           ELSE IF (LST .GE. SECOND_LST) THEN
                              TAUZ = SECOND_TAU
                           ELSE
                              TAUZ = FIRST_TAU + (SECOND_TAU-FIRST_TAU)
     :                             * (LST - FIRST_LST) /
     :                             (SECOND_LST - FIRST_LST)
                           END IF

*     correct the bolometer data for the sky opacity
*     Check for status here since the bounds can be exceeded
*     before we enter the subroutine (DATA_OFFSET can be too large)

                           IF (STATUS .EQ. SAI__OK) THEN

                              DO BEAM = 1, N_BEAMS


                                 CALL SCULIB_CORRECT_EXTINCTION (
     :                                NUM_ADC * NUM_CHAN, N_BOL,
     :                                NDATA(1, DATA_OFFSET, BEAM),
     :                                NVARIANCE(1, DATA_OFFSET, BEAM),
     :                                BOL_RA,BOL_DEC,LST, LAT_OBS, TAUZ,
     :                                STATUS)
                              END DO
                           END IF

                        ELSE IF (REMIP) THEN

*     Correct for instrumental polarisation
*     Note that we do not calculate the elevation of each bolometer
*     We just use the elevation of the centre of the array

                           DO BEAM = 1, N_BEAMS

                              CALL SURFLIB_REMOVE_IP(ELEVATION,
     :                             NUM_CHAN, NUM_ADC, N_BOL,
     :                             WAVEPLATE_ANG(DATA_OFFSET), BOL_CHAN,
     :                             BOL_ADC, BOL_IP_DATA,
     :                             NDATA(1, DATA_OFFSET, BEAM),
     :                             NVARIANCE(1,DATA_OFFSET,BEAM),
     :                             STATUS)

*     At this point we could correct the WAVEPLATE angle
*     for parallactic angle and elevation. This will be needed
*     during the regrid phase
*     At this point we replace the waveplate angle with the
*     sky rotation angle (over-writing any reference to the waveplate
*     angle). We overwrite to save space since at this stage the rotation
*     angle is more important for the polarimetry than the waveplate
*     angle itself (which has already been stored in a higher level
*     routine). Convert the angle to degrees for storage

                              WAVEPLATE_ANG(DATA_OFFSET) =
     :                             SNGL ((PAR_ANGLE - ELEVATION) *
     :                             180.0D0 / PI )

*                              print *,DATA_OFFSET,
*     :                             SNGL(PAR_ANGLE*180.0D0/PI),
*     :                             SNGL(ELEVATION*180.0D0/PI)

                           END DO

                        ELSE

*     Convert the apparent RA/Decs to tangent plane offsets
*     from the moving centre if we are using the 'PL' output system
*     or want to calculate Az and NA

                           IF (OUT_COORDS .EQ. 'PL' .OR. AZNASCAN) THEN

                              IF (STATUS .EQ. SAI__OK) THEN
                                 CALL SCULIB_APPARENT_2_TP (N_BOL,
     :                                BOL_RA(1,DATA_OFFSET, BM),
     :                                BOL_DEC(1,DATA_OFFSET, BM),
     :                                MAP_RA_CEN, MAP_DEC_CEN, 0.0D0,
     :                                0.0D0, 0.0D0, STATUS)
                              END IF

*     If we are trying to do NA/AZ for RASTER data then we have to convert
*     the coordinates from apparent RA/Dec to tangent plane offsets and
*     then into Az or NA coordinates using an elevation dependent rotation.
*     Convert these TP offsets to Az or NA
                              IF (AZNASCAN) THEN


                                 IF (STATUS .EQ. SAI__OK) THEN
                                    CALL SCULIB_SCAN_APPARENT_TP_2_AZNA(
     :                                   OUT_COORDS,1, N_BOL, ELEVATION,
     :                                   PAR_ANGLE,
     :                                   BOL_RA(1, DATA_OFFSET, BM),
     :                                   BOL_DEC(1,DATA_OFFSET, BM),
     :                                   STATUS)

                                 END IF

                              END IF

                           ELSE
*     convert the coordinates to apparent RA,Dec on MJD_STANDARD
                              IF (OUTCRDS .EQ. 'RA') THEN
                                 IF (N_MAP .NE. 1 .AND.
     :                                STATUS .EQ. SAI__OK) THEN
                                    CALL SCULIB_STANDARD_APPARENT (
     :                                   N_BOL,
     :                                   BOL_RA(1,DATA_OFFSET, BM),
     :                                   BOL_DEC(1, DATA_OFFSET, BM),
     :                                   IN_UT1, MJD_STANDARD, STATUS)
                                 END IF
                              END IF
                           END IF
                        END IF

                     END DO  ! End the BM loop
                  END DO

*     If we are using SCAN_REVERSAL then multiply every other
*     exposure by -1 (but only is CHOP is SC)
*     We only want to do this if we are rebinning the image.
*     If this is SCUBA2MEM and we are requesting multiple beams
*     then we do not want to fiddle with the data
                  IF (SCAN_REVERSAL .AND. CHOP_CRD .EQ. 'SC'
     :                 .AND. N_POS_BEAMS .EQ. 1) THEN
                     IF (FLIP) THEN
                        IF (STATUS .EQ. SAI__OK) THEN
                           ITEMP = N_BOL * (EXP_END - EXP_START + 1)
                           RTEMP = -1.0

                           CALL SCULIB_MULCAR(ITEMP,
     :                          NDATA(1,STORED_OFFSET,1), RTEMP,
     :                          NDATA(1, STORED_OFFSET,1))
                        END IF
                        FLIP = .FALSE.
                     ELSE
                        FLIP = .TRUE.
                     END IF
                  END IF

               END IF

               END IF   ! This is the endif that goes with STATUS check

            END DO
         END DO
      END DO

*     Check that some data was processed
      IF (STATUS .EQ. SAI__OK .AND. .NOT. SOME_DATA) THEN
         STATUS = SAI__WARN
         CALL ERR_REP (' ', 'SURFLIB_PROCESS_BOLS: No data were '//
     :        'processed', STATUS)
         SET_STATUS = .TRUE.
      END IF

*     Check final status so that I can inform people where the error
*     occurred.
      IF (STATUS .NE. SAI__OK .AND. .NOT.SET_STATUS) THEN
         CALL ERR_REP(' ','SURFLIB_PROCESS_BOLS: Error '//
     :        'occurred somewhere', STATUS)
      END IF


      END
