      SUBROUTINE SURF_REMIP( STATUS )
*+
*  Name:
*     REMIP

*  Purpose:
*     Remove instrumental polarisation from SCUBA pol data

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_REMIP( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This task calculates and removes the instrumental polarisation
*     signal from SCUBA polarimeter data.
*
*       Actual flux = measured flux * ( 1 - fractional IP)
*
*     where IP = P * (1 + cos (4 WP - 2 THETA ))
*     and P is the (elevation dependent) instrumental fractional
*     polarisation, WP is the position angle of the wave plate and
*     THETA is the (elevation dependant) position angle of the IP.
*
*     This is an approximation of 
*
*       Actual flux = measured flux - mean flux * frac IP
*
*     where 
*
*        IP = P * (1 + cos (4 WP - 2 THETA ) ),
*
*     and we have assumed that the instrumental polarisation is small
*     such that the measured flux and the mean fluxed are
*     approximately equal. This approximation is required since
*     the mean flux can not be calculated trivially since the
*     bolometers are jiggling on and off the source.

*     because the mean flux can not be calculated trivially since
*     the bolometers are jiggling on and off the source.


*  Usage:
*     remip in ipfile out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input data file.
*     IPFILE = FILE (Read)
*        File containing the IP `flatfield'
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Options are QUIET, NORMAL and
*        VERBOSE. Default is NORM.
*     OUT = NDF (Write)
*        Output file containing IP removed data. Default output
*        filename is `_ip' (`i' for short form).

*  Examples:
*     remip file1 ipfile.dat \\
*       Correct file1.sdf using ipfile.dat and write IP corrected
*       data to the default output file (eg file1_ip).

*  Authors:
*     Tim Jenness (t.jenness@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.11  2005/03/19 01:41:02  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.10  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.9  2003/04/07 23:48:34  timj
*     Change %age to 'frac' in documentation header
*
*     Revision 1.8  2003/04/03 03:19:09  timj
*     Calculation now fixed to S(meas)*(1-P(1+cos[])
*
*     Revision 1.7  2002/09/19 21:12:14  timj
*     Fix problem with terminated polarimetery observations where the
*     WPLATE array size did not match the aborted measurement count
*
*     Revision 1.6  2002/09/09 21:43:33  timj
*     Fix "use of unintialized value" warning with LONG2_RAD
*
*     Revision 1.5  2000/06/16 01:25:16  timj
*     Use new-format SCULIB_GET_MJD
*
*     Revision 1.4  1999/08/03 20:01:38  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.3  1999/07/14 20:13:30  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.2  1999/05/15 01:46:30  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*
*     Revision 1.1  1999/02/27 04:59:34  timj
*     First version
*

*  Notes:
*     - Variance is propagated correctly.
*     - This task writes out the waveplate angles and rotation angles.
*       The waveplate angle per integration is written to .MORE.REDS.WPLATE.
*       The rotation angle (waveplate 0 to X pixel axis) is written
*       to .MORE.REDS.ANGROT (angle per integration). The angle
*       between nasmyth and the ra/dec frame (ie ANGROT - 90 degrees)
*       is stored in .MORE.REDS.NASMYTH_ANG (angle per sample).
*       These are written as NDFs and so can be displayed in the normal way.
*       The angles are in degrees.
*     - An array containing the fast axis angle is also written
*       to the REDS extension (FAST_AXIS). The size of this array
*       matches the number of sub-instruments in the file.
*     - The focal station of the instrument (THUMPER, SCUBA etc) is not
*       taken into account when calculting the elevation of the individual
*       pixels. The effect of array rotation on the elevation component is tiny. 

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'SURF_PAR'        ! For SCUBA__N_SUFFIX
      INCLUDE 'PRM_PAR'         ! For VAL_NB*
      INCLUDE 'DAT_PAR'         ! For DAT__SZLOC
      INCLUDE 'MSG_PAR'         ! For MSG__NORM
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN

*  Local Constants:
      CHARACTER * 10   TSKNAME  ! Name of task
      PARAMETER (TSKNAME = 'REMIP') 
      INTEGER MAXDIM            ! Max dimensions of data array
      PARAMETER (MAXDIM = 4)

*  Local Variables:
      LOGICAL ABORTED           ! Was the obsn aborted?
      BYTE    BADBIT            ! Badbit mask of input data
      INTEGER BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                ! A/D numbers of bolometers measured in
                                ! input file
      INTEGER BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                ! channel numbers of bolometers
                                ! measured in input file
      DOUBLE PRECISION BOL_DEC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                ! apparent Dec of the bolometers at
                                ! the time of a measurement
      REAL    BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                ! dU3 Nasmyth coord of bolometers
      REAL    BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                ! dU4 Nasmyth coord of bolometers
      REAL    BOL_IP_DATA (8, SCUBA__NUM_CHAN, SCUBA__NUM_ADC) 
                                ! IP measurements 1: P 2:SlopeP
                                ! 3: Theta 4:SlopeTheta. The next four
                                ! slots are for the corresponding variance
      DOUBLE PRECISION BOL_RA (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                ! apparent RA of the bolometers at
                                ! the time of a measurement
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                ! bolometer types
      CHARACTER*15 CENTRE_COORDS ! coord system of telescope centre
      DOUBLE PRECISION DEC_CENTRE ! apparent declination of map centre
                                ! (radians)
      INTEGER DEC1_PTR          ! pointer to .SCUCD.DEC1
      INTEGER DEC2_PTR          ! pointer to .SCUCD.DEC2
      INTEGER DIM(MAXDIM)       ! Dimensions of data array
      REAL    FAST_AXIS(SCUBA__MAX_SUB) ! Fast axis angle
      INTEGER FD                ! File descriptor
      CHARACTER* 10 FILTERS(SCUBA__MAX_SUB) ! Filter names
      CHARACTER* 80 FITS(SCUBA__MAX_FITS) ! FITS information
      CHARACTER*132 FNAME       ! Name of input file
      INTEGER I                 ! counter
      INTEGER IERR              ! position of error in VEC copy
      INTEGER IN_DEM_PNTR_PTR   ! Pointer to DEM_PNTR array
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC ! locator to input FITS extension
      INTEGER IN_NDF            ! Input NDF identifier
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC ! locator to input SCUBA extension
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC ! locator to input SCUCD extension
      INTEGER ITEMP             ! Scratch integer
      INTEGER JIGGLE_COUNT      ! number of jiggles in pattern
      INTEGER JIGGLE_P_SWITCH   ! number of jiggles per switch
      INTEGER JIGGLE_REPEAT     ! number of times jiggle pattern is
                                ! repeated in a switch
      REAL    JIGGLE_X (SCUBA__MAX_JIGGLE)
                                ! x jiggle offsets (arcsec)
      REAL    JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                ! y jiggle offsets (arcsec)
      INTEGER LAST_EXP          ! the last exposure number in
                                ! an aborted observation
      INTEGER LAST_INT          ! the last integration number in
                                ! an aborted observation
      INTEGER LAST_MEAS         ! the last measurement number in
                                ! an aborted observation
      DOUBLE PRECISION LAT_OBS          ! Latitude of observatory (radians)
      DOUBLE PRECISION LAT_RAD  ! latitude of telescope centre (radians)
      DOUBLE PRECISION LAT2_RAD ! latitude of telescope centre at MJD2
      INTEGER LBND(2)           ! Lower bounds of an NDF
      DOUBLE PRECISION LONG_RAD ! longitude of telescope centre 
                                ! (radians)
      DOUBLE PRECISION LONG2_RAD ! apparent RA of telescope centre at
                                ! MJD2 (radians)
      CHARACTER*(15)   LOCAL_COORDS ! Coordinate system of MAP_X and MAP_Y
      INTEGER LST_STRT_PTR      ! pointer to .SCUCD.LST_STRT
      REAL             MAP_X    ! x offset of map centre from telescope
                                ! centre (arcsec)
      REAL             MAP_Y    ! y offset of map centre from telescope
                                ! centre (arcsec)
      DOUBLE PRECISION MJD1     ! modified Julian day at which object 
                                ! was at LAT,LONG for PLANET centre
                                ! coordinate system
      DOUBLE PRECISION MJD2     ! modified Julian day at which object
                                ! was at LAT2,LONG2 for PLANET centre
                                ! coordinate system
      INTEGER NDIM              ! Actual number of dimensions in NDF
      INTEGER NERR              ! Number of errors from VEC copy
      INTEGER NREC              ! Number of history records
      INTEGER N_BEAM            ! Number of beams (photometry)
      INTEGER N_BOLS            ! Number of bolometers (X-axis)
      INTEGER N_EXPOSURES       ! Number of exposures in observation
      INTEGER N_FITS            ! Number of fits keywords in header
      INTEGER N_INTEGRATIONS    ! Number of integrations in observation
      INTEGER N_MEASUREMENTS    ! Number of measurements in observation
      INTEGER N_POS             ! Number of samples (Y-axis)
      INTEGER N_SWITCHES        ! Number of switches
      INTEGER N_SUB             ! Number of sub-instruments in observation
      INTEGER N_WP_POS          ! Number of values in WPLATE array
      CHARACTER*40  OBJECT      ! name of object
      CHARACTER*40  OBSERVING_MODE ! observing mode of input file
      CHARACTER*132 OUTFILE     ! Default name for output file
      INTEGER OUT_DATA_PTR      ! Pointer to output data
      INTEGER OUT_NDF           ! Output ndf identifier
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC ! locator to output REDS extension
      INTEGER OUT_VARIANCE_PTR  ! Pointer to output variance
      CHARACTER*132 PATH       ! location of default IP file
      INTEGER PLACE             ! place holder in HDS system
      REAL    POINT_DAZ (SCUBA__MAX_POINT)
                                ! azimuth pointing corrections (arcsec)
      REAL    POINT_DEL (SCUBA__MAX_POINT)
                                ! elevation pointing corrections (arcsec)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                ! LST of pointing corrections (radians)
      DOUBLE PRECISION RA_CENTRE ! apparent RA of map centre (radians)
      INTEGER RA1_PTR           ! pointer to .SCUCD.RA1
      INTEGER RA2_PTR           ! pointer to .SCUCD.RA2
      LOGICAL REDUCE_SWITCH     ! Has REDUCE_SWITCH been run
      LOGICAL REMIP             ! Has REMIP already been run on the file
      DOUBLE PRECISION ROTATION ! angle between apparent north and 
                                ! north of input coord system (radians,
                                ! measured clockwise from input north) 
      REAL    RTEMP             ! Scratch real
      INTEGER RUN_NUMBER        ! run number of input file
      CHARACTER*15     SAMPLE_COORDS ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE ! SAMPLE_MODE of observation
      INTEGER SCUCD_WPLATE_PTR  ! pntr to WPLATE array in SCUCD extension
      CHARACTER * 40 STATE      ! State of observation at end
      CHARACTER * 80 STEMP      ! Temporary string
      CHARACTER * 15 SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! file Suffices
      LOGICAL THERE             ! Is a component there?
      INTEGER UBND(2)           ! Upper bounds of an NDF
      LOGICAL USE_WP            ! Use the WPLATE array in SCUCD extension?
      DOUBLE PRECISION UT1      ! UT1 of start of observation expressed
                                ! as modified Julian day
      INTEGER WP_NDF            ! waveplate NDF id in REDS extension
      INTEGER WP_DATA_PTR       ! pointer to mapped data from WP_NDF
      INTEGER WP_VAR_PTR        ! Pointer to mapped variance from WP_NDF
      INTEGER WPLATE_END        ! End of WPLATE data
      INTEGER WPLATE_PER_INT_END ! pntr to end of WPLATE_PER_INT_PTR
      INTEGER WPLATE_PER_INT_PTR ! pntr to waveplate pos per integration
      INTEGER WPLATE_PTR        ! Pntr to wplate(N_POS)array (pos in degrees)


*  Local Data:
      DATA SUFFIX_STRINGS /'!_ip','i','_ip'/

*.

      IF (STATUS .NE. SAI__OK) RETURN





*     Initialise NDF
      CALL NDF_BEGIN

*     Get the input file name
      CALL NDF_ASSOC ('IN', 'READ', IN_NDF, STATUS)

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

*     get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file contains '//
     :           'too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :     STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :     RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :     OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :     OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_MODE',
     :     SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_SETC ('SAMPLE', SAMPLE_MODE)
      CALL MSG_OUTIF (MSG__NORM,' ', 
     :     '^PKG: run ^RUN was a ^MODE observation '//
     :     'with ^SAMPLE sampling of object ^OBJECT', STATUS)

*     Look at the history of the file to check that REMIP
*     has not been run on this already


      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         REDUCE_SWITCH = .FALSE.
         REMIP = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP,
     :           STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP(:5) .EQ. 'REMIP') THEN
                  REMIP = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'REDUCE_SWITCH application has not been run on '//
     :           'the input file', STATUS)
            END IF

            IF (REMIP) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'REMIP application has already been run on '//
     :           'the input file', STATUS)
            END IF
         END IF
      END IF

*  coordinate system and coords of telescope `centre'

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'CENT_CRD',
     :  CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LONG_RAD, STATUS)

*     initialise so that we do not get a warning with valgrind
*     when we multiply LONG2_RAD by 15.0
      LONG2_RAD = 0.0D0
      LAT2_RAD = 0.0D0
      MJD1 = 0.0D0
      MJD2 = 0.0D0

      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LONG2_RAD, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD1',
     :     MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD2',
     :     MJD2, STATUS)
      ELSE
         LAT2_RAD = 0.0D0
         LONG2_RAD = 0.0D0
         MJD2 = 0.0D0     
      END IF

      IF ((CENTRE_COORDS .NE. 'AZ')  .AND.
     :    (CENTRE_COORDS .NE. 'GA')) THEN
         LONG_RAD = LONG_RAD * 15.0D0
         LONG2_RAD = LONG2_RAD * 15.0D0
      END IF

*     Read the latitude of the observatory
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*     telescope offset from telescope centre
      
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_X',
     :     MAP_X, STATUS)
      MAP_X = MAP_X / REAL (R2AS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_Y',
     :        MAP_Y, STATUS)
      MAP_Y = MAP_Y / REAL (R2AS)

*     and the coordinate frame of these offsets
*     not sure whether old files have this parameter so test for status
*     If it is not available then assume it is CENTRE_COORDS

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LOCL_CRD', LOCAL_COORDS, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            LOCAL_COORDS = CENTRE_COORDS
         END IF
      END IF



*     Expected number of bolometers

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*     Now find the dimensions of the input NDF (N_POS)

      CALL NDF_DIM ( IN_NDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOLS)         .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 '//
     :           '^DIM3', STATUS)
            END IF

            N_BEAM = DIM(3)

         ELSE
            IF ((NDIM .NE. 2)          .OR.
     :          (DIM(1) .NE. N_BOLS) .OR.
     :          (DIM(2) .LT. 1))       THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :           STATUS)
            END IF

            N_BEAM = 1

         END IF
      END IF

      N_POS = DIM (2)

*     Get a locator to the SCUBA and SCUCD extensions
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)

*     map the DEM_PNTR array and check its dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS, 
     :     N_MEASUREMENTS, STATUS)

*     map the .SCUCD.LST_STRT array and check its dimensions

      CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, %VAL(CNF_PVAL(LST_STRT_PTR)), 
     :                    UT1,
     :     RTEMP, RTEMP, STATUS)

*     see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF
      
*     Print out information on observation

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      IF (.NOT. ABORTED) THEN
         CALL MSG_SETC ('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        '^PKG: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integration(s) in '//
     :        '^N_M measurement(s)', STATUS)
      ELSE

*     get the exposure, integration, measurement numbers at which the abort
*     occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_SETC ('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        '^PKG: the observation should have '//
     :        'had ^N_E exposure(s) in ^N_I integration(s) in ^N_M '//
     :        'measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration ^N_I '//
     :        'of measurement ^N_M', STATUS)
      END IF

*     Read in general info concerning LST and jiggle/scan pattern

*  calculate the apparent RA and Dec of the object for the time of the
*  observation

      CALL SCULIB_CALC_APPARENT (LAT_OBS, LONG_RAD, LAT_RAD, LONG2_RAD,
     :     LAT2_RAD, 0.0D0, 0.0D0, CENTRE_COORDS, 
     :     %VAL(CNF_PVAL(LST_STRT_PTR)), UT1,
     :     MJD1, MJD2, RA_CENTRE, DEC_CENTRE, ROTATION, STATUS)


*  If the sampling was done by jiggling the secondary then read in the
*  relevant jiggle information

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN

         CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :        N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT, 
     :        JIGGLE_P_SWITCH, RTEMP, SAMPLE_COORDS, JIGGLE_X,
     :        JIGGLE_Y, STATUS)

*  likewise for RASTER

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

         CALL SCULIB_GET_RASTER(IN_SCUCDX_LOC, N_SWITCHES,
     :        N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :        RA1_PTR, RA2_PTR, DEC1_PTR, DEC2_PTR,
     :        STATUS)

      END IF

*  get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOLS, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)



*     See if there is a WPLATE array in the SCUCD extension in the 
*     The WPLATE array should contain the wave plate positions
*     per measurement.

      N_WP_POS = 0
      CALL DAT_THERE(IN_SCUCDX_LOC, 'WPLATE', USE_WP, STATUS)

      IF (USE_WP) THEN
         CALL CMP_MAPV(IN_SCUCDX_LOC, 'WPLATE', '_REAL',
     :        'READ', SCUCD_WPLATE_PTR, N_WP_POS, STATUS)

*     Check dimension [it has not been decided whether
*     the WPLATE array will match N_MEAS or whether it will auto
*     wrap round]. We allow the observation to be terminated (ie
*     number of wplates greater than number of measurements
         IF (STATUS .EQ. SAI__OK .AND. N_WP_POS.NE.N_MEASUREMENTS) THEN

            IF (N_WP_POS .GT. N_MEASUREMENTS) THEN
*     Make sure they agree for the rest of the run
               N_WP_POS = N_MEASUREMENTS
            ELSE
*     We do not yet allow wraparound
               STATUS = SAI__ERROR
               CALL MSG_SETC('TSK',TSKNAME)
               CALL MSG_SETI('NW', N_WP_POS)
               CALL MSG_SETI('NM', N_MEASUREMENTS)
               CALL ERR_REP(' ','^TSK: Dimensions of WPLATE array '//
     :              '(^NW) is less than number of measurements (^NM)',
     :              STATUS)
            END IF
         END IF

      END IF

*     Allocate some memory for the waveplate positions
*     Also store the waveplate positions for each integration
*     and measurement
      WPLATE_PTR = 0
      WPLATE_END = 0
      CALL SCULIB_MALLOC(N_POS * VAL__NBR, WPLATE_PTR, WPLATE_END,
     :     STATUS)

      WPLATE_PER_INT_PTR = 0
      WPLATE_PER_INT_END = 0
      CALL SCULIB_MALLOC(N_MEASUREMENTS * N_INTEGRATIONS * VAL__NBR,
     :     WPLATE_PER_INT_PTR, WPLATE_PER_INT_END, STATUS)

*     fill the waveplate array
*     with a waveplate position per sample (per N_POS).
*     The return array is then filled with the waveplate position
*     angle in degrees

      CALL SURFLIB_FILL_WPLATE(N_WP_POS, 
     :                         %VAL(CNF_PVAL(SCUCD_WPLATE_PTR)),
     :     N_POS, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), %VAL(CNF_PVAL(WPLATE_PTR)),
     :     %VAL(CNF_PVAL(WPLATE_PER_INT_PTR)), STATUS)

*     Free the SCUCD WPLATE array
      IF (USE_WP) CALL CMP_UNMAP(IN_SCUCDX_LOC, 'WPLATE', STATUS)

*     Read in badbit mask
      CALL NDF_BB(IN_NDF, BADBIT, STATUS)

*     Now we can read in the IP information from a file
*     and populate the BOL_IP_DATA array.

*     Specify a default value (this will be a dynamic default
*     based on sub-instrument when the polarimeter is commissioned
*     at 350/750 microns
*     Expect the file to be in $SURF_DIR/ipfile.dat
*     Have to convert SURF_DIR env var to a string

      IF (STATUS .EQ. SAI__OK) THEN
         CALL PSX_GETENV('SURF_DIR', PATH, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            PATH = 'ipfile.dat'
            CALL ERR_ANNUL(STATUS)
         ELSE
            ITEMP = CHR_LEN(PATH)
            CALL CHR_APPND('/ipfile.dat', PATH, ITEMP)
         END IF
      END IF
         
      CALL PAR_DEF0C('IPFILE',PATH, STATUS)

*     Get the name of the IP file

      CALL FIO_ASSOC ('IPFILE', 'READ', 'LIST', 0, FD, STATUS)

*     Since the IPFILE sets the fast axis angle we need to read
*     the filter names from the FITS header
      N_SUB = 0
      CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS, 'N_SUBS',
     :     N_SUB, STATUS)
      
*     Code copied from SCULIB_GET_SUB_INST
      STEMP = 'FILT_'
      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, N_SUB
            ITEMP = 5
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :           STEMP, FILTERS(I), STATUS)
            CALL CHR_UCASE (FILTERS(I))
         END DO
      END IF

*     Now read it
      
      CALL SURFLIB_READ_IPFILE(FD, SCUBA__NUM_CHAN, SCUBA__NUM_ADC,
     :     N_SUB, FILTERS, FAST_AXIS, BOL_IP_DATA, STATUS)

*     Close the file
      CALL FIO_ANNUL(FD, STATUS)

*     Now need to create the output file

*     First the name
*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)
      
*     OK, propagate the input ndf to the output

      CALL NDF_PROP (IN_NDF, 'Units,Axis,DATA,QUALITY,VARIANCE', 'OUT',
     :     OUT_NDF, STATUS)


*     Map the output data
      CALL NDF_SQMF(.FALSE., OUT_NDF, STATUS)
      CALL NDF_MAP(OUT_NDF, 'DATA', '_REAL', 'UPDATE', OUT_DATA_PTR,
     :     ITEMP, STATUS)
      CALL NDF_MAP(OUT_NDF, 'VARIANCE', '_REAL', 'UPDATE', 
     :     OUT_VARIANCE_PTR, ITEMP, STATUS)


*     Process each of the data points and remove the IP
*     Assume a Left Nasmyth since it makes no difference to the IP
*     calculation since the array size is much smaller than the elevation effect

      CALL SURFLIB_PROCESS_BOLS(TSKNAME, N_BEAM, N_BOLS,
     :     N_POS, 1, N_SWITCHES, N_EXPOSURES, 
     :     N_INTEGRATIONS, N_MEASUREMENTS, 
     :     1, N_EXPOSURES, 1, N_INTEGRATIONS, 1, N_MEASUREMENTS,
     :     1, N_FITS, FITS,
     :     %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), 
     :     %VAL(CNF_PVAL(LST_STRT_PTR)),
     :     ROTATION, SAMPLE_MODE,
     :     SAMPLE_COORDS, 'RA', JIGGLE_REPEAT,
     :     JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :     'LNASMYTH', RA_CENTRE, DEC_CENTRE,
     :     %VAL(CNF_PVAL(RA1_PTR)), %VAL(CNF_PVAL(RA2_PTR)),
     :     %VAL(CNF_PVAL(DEC1_PTR)), %VAL(CNF_PVAL(DEC2_PTR)), UT1, UT1,
     :     MJD1, LONG_RAD, LAT_RAD, MJD2, LONG2_RAD, LAT2_RAD,
     :     LOCAL_COORDS, DBLE(MAP_X), DBLE(MAP_Y),
     :     0, POINT_LST, POINT_DAZ, POINT_DEL,
     :     SCUBA__NUM_CHAN, SCUBA__NUM_ADC,BOL_ADC,BOL_CHAN,
     :     BOL_DU3, BOL_DU4, .FALSE., 0.0D0,0.0D0,0.0,0.0,
     :     BOL_RA, BOL_DEC,
     :     %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :     %VAL(CNF_PVAL(OUT_VARIANCE_PTR)), .FALSE., 0,
     :     %VAL(CNF_PVAL(WPLATE_PTR)), BOL_IP_DATA,
     :     STATUS)

*     Now that the data (and variance) have been corrected for
*     instrumental polarisation we can write the Waveplate positions
*     out to the output file in a DEM_PNTR style array
*     This saves me having to do anything clever in the rebin
*     stage since the waveplate angles will already have been
*     1. corrected for elevation and parallactic angle
*     2. converted into a waveplate angle per integration
*     I am assuming that people are only interested in the polarimetry
*     data for ra/dec regrids. GA or NA/AZ regrids will not be
*     supported

*     The WPLATE_PER_INT_PTR array contains the half-wave plate
*     position angles for each integration and measurement

*     The WPLATE_PTR array should now contain the difference
*     between parallactic angle and elevation for each sample (degrees)
*     since SURFLIB_PROCESS_BOLS modifies the values in the array

*     We can write out the waveplate positions and the rotation
*     angles (PA - El). Write them out as NDFs


*     Get locator to REDS extension (creating if necessary)
      
      CALL NDF_XSTAT(OUT_NDF, 'REDS', THERE, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (THERE) THEN
            CALL NDF_XLOC(OUT_NDF, 'REDS', 'UPDATE', OUT_REDSX_LOC, 
     :           STATUS)
         ELSE
            CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION',
     :           0, 0, OUT_REDSX_LOC, STATUS)
         END IF
      END IF

*     Write the fast axis information to the REDS extension
      IF (N_SUB .EQ. 1) THEN
         CALL NDF_XPT0R(FAST_AXIS(1), OUT_NDF, 'REDS','FAST_AXIS',
     :        STATUS)
      ELSE

*     Create the FAST_AXIS array and copy the data
         CALL CMP_MOD(OUT_REDSX_LOC, 'FAST_AXIS', '_REAL', 1,
     :        N_SUB, STATUS)
         CALL CMP_PUT1R(OUT_REDSX_LOC, 'FAST_AXIS', N_SUB,
     :        FAST_AXIS, STATUS)
      END IF


*     Create the output extension for the HWP angle
      
      CALL NDF_PLACE (OUT_REDSX_LOC, 'WPLATE',PLACE, STATUS)
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = N_INTEGRATIONS
      UBND(2) = N_MEASUREMENTS
      CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, WP_NDF, STATUS)

*     Map the new NDF
      CALL NDF_MAP(WP_NDF, 'DATA', '_REAL', 'WRITE', WP_DATA_PTR,
     :     ITEMP, STATUS)

*     Write the waveplate angles to the extension

      CALL VEC_RTOR(.FALSE., ITEMP, %VAL(CNF_PVAL(WPLATE_PER_INT_PTR)),
     :     %VAL(CNF_PVAL(WP_DATA_PTR)), IERR, NERR, STATUS)

*     Unmap and annul the WP_NDF
      CALL NDF_UNMAP(WP_NDF,'DATA', STATUS)
      CALL NDF_ANNUL(WP_NDF, STATUS)

*     Now write the WPLATE_PTR array. This should be the rotation angles
*     ANGROT in POLPACK

      CALL NDF_PLACE (OUT_REDSX_LOC, 'ANGROT',PLACE, STATUS)

      CALL NDF_NEW('_REAL', 2, LBND, UBND, PLACE, WP_NDF, STATUS)

*     Map the array
      CALL NDF_MAP(WP_NDF, 'DATA', '_REAL', 'WRITE', WP_DATA_PTR,
     :     ITEMP, STATUS)
      CALL NDF_MAP(WP_NDF,'VARIANCE','_REAL','WRITE',WP_VAR_PTR,
     :     ITEMP, STATUS)
      

*     We don't want to write all the angles to the output file
*     at this time. Polarimetry is only interested in the
*     average rotation angle for a single integration. This means that
*     we should average over an integration before storing the
*     rotation angle. Note also that POLPACK requires the angle
*     to be the angle anti-clockwise from the X pixel axis.
*     Since RA regrids have the X AXIS reversed relative to the
*     X PIXEL axis we have to take that into account when calculating
*     the angle

      CALL SURFLIB_CALC_POLPACK_ANGROT(
     :     N_POS, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), %VAL(CNF_PVAL(WPLATE_PTR)),
     :     %VAL(CNF_PVAL(WP_DATA_PTR)), %VAL(CNF_PVAL(WP_VAR_PTR)), 
     :     STATUS)
      
*     Unmap and annul the ANGROT NDF
      CALL NDF_UNMAP(WP_NDF,'*', STATUS)
      CALL NDF_ANNUL(WP_NDF, STATUS)

*----------CUT-----------

*     This section of code write the full rotation angle array
*     to disk. Mainly for testing purposes so that the angles can
*     be compared with the averaged values stored in ANGROT

      CALL NDF_PLACE (OUT_REDSX_LOC, 'NASMYTH_ANG',PLACE, STATUS)
      CALL NDF_NEW('_REAL', 1,1,N_POS, PLACE, WP_NDF, STATUS)

      CALL NDF_MAP(WP_NDF, 'DATA', '_REAL', 'WRITE', WP_DATA_PTR,
     :     ITEMP, STATUS)

      CALL VEC_RTOR(.FALSE., ITEMP, %VAL(CNF_PVAL(WPLATE_PTR)),
     :     %VAL(CNF_PVAL(WP_DATA_PTR)), IERR, NERR, STATUS)

*     Unmap and annul the Namyth angle  NDF
      CALL NDF_UNMAP(WP_NDF,'DATA', STATUS)
      CALL NDF_ANNUL(WP_NDF, STATUS)

*-----------CUT-------------

*     Free memory
      CALL SCULIB_FREE('WPLATE', WPLATE_PTR, WPLATE_END, STATUS)
      CALL SCULIB_FREE('WPLATE_P_INT', WPLATE_PER_INT_PTR,
     :     WPLATE_PER_INT_END, STATUS)

*     Close the locators
      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL CMP_UNMAP (IN_SCUCDX_LOC, 'LST_STRT', STATUS)

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)

*     Unmap the data
      CALL NDF_UNMAP(OUT_NDF, '*', STATUS)

*     Close data files
      CALL NDF_ANNUL(IN_NDF, STATUS)
      CALL NDF_ANNUL(OUT_NDF, STATUS)

      END
