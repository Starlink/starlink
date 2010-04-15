      SUBROUTINE SURF_SCUCLKERR ( STATUS )
*+
*  Name:
*     SCUCLKERR

*  Purpose:
*     Determine the possible error in the times stored in the data header

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SCUCLKERR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine calculates the error in the times stored in the
*     data header. It performs a self-consistency check to determine
*     the local sidereal time from the Azimuth and elevation information
*     (that comes directly from the telescope) and compares this to
*     the LST stored in the header.

*  Usage:
*     scuclkerr filename

*  ADAM Parameters:
*     CLOCKERR = REAL (Write)
*        On exit, the clock error, in seconds,  determined from the header.
*     DANG = REAL (Write)
*        Error in the array rotation angle due to the clock error (degrees)
*     DR = REAL (Write)
*        Positional error at the edge of the array for this particular
*        observation. In arcseconds.
*        Edge is defined as a radius of 70 arcseconds.
*     IN = NDF (Read)
*        The name of the NDF containing to be tested.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     LAG = REAL (Write).
*        The discrepancy between the LST stored in the FITS header
*        and the LST when data acquisition begins. This provides
*        a measure of the lag in starting up the observation (including
*        slew times). The value is stored in seconds.
*     MJD = DOUBLE (Write)
*        Modified Julian Date of start of observation corrected for the
*        lag time and the clock error.

*  Notes:
*     - The calculated clock error is only accurate to about 15 seconds.

*  Reference:
*     Jenness, T., 2000, JCMT Technical Report TR/001/84

*  Authors:
*     TIMJ: Tim Jenness (JAC)


*  Copyright:
*     Copyright (C) 2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  2005/03/19 01:41:02  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.3  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.2  2000/07/07 03:24:15  timj
*     Doc fixes
*
*     Revision 1.1  2000/06/22 23:56:21  timj
*     First version
*

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'DAT_PAR'         ! DAT__
      INCLUDE 'SURF_PAR'        ! SURF constants
      INCLUDE 'MSG_PAR'         ! MSG__ constants
      INCLUDE 'PRM_PAR'         ! bad values
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  Local Constants:
      DOUBLE PRECISION DR2S     ! Radians to seconds of time
      PARAMETER ( DR2S = 1.37509870831397570104315571553852408797773D4)
      DOUBLE PRECISION DR2AS     ! Radians to arcseconds
      PARAMETER ( DR2AS= 2.0626480624709635515647335733077861319666D5 )
      DOUBLE PRECISION DS2R     ! seconds of time to radians
      PARAMETER ( DS2R = 7.2722052166430399038487115353692196393453D-5 )
      DOUBLE PRECISION D2PI     ! 2 * PI
      PARAMETER ( D2PI = 6.283185307179586476925286766559005768394339 )
      DOUBLE PRECISION DR2D     ! Radians to degrees
      PARAMETER ( DR2D = 57.29577951308232087679815481410517033240547 )
      CHARACTER * 9 TSKNAME            ! SCUCLKERR name
      PARAMETER (TSKNAME = 'SCUCLKERR')

*  Local variables:
      DOUBLE PRECISION ANG      ! Angular error (radians)
      DOUBLE PRECISION CLOCK_ERR ! Error in clocks (radians)
      DOUBLE PRECISION DEC_CEN  ! apparent dec of input file map centre
                                ! (radians)
      DOUBLE PRECISION DTEMP    ! Scratch double
      DOUBLE PRECISION ETA      ! Y offset between bol 1 and 2 (radians)
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                ! array of FITS keywords

      INTEGER          IERR     ! For VEC_
      INTEGER          I        ! Loop variable
      INTEGER          INDF     ! NDF id for input file
      CHARACTER*15     IN_CENTRE_COORDS ! coord system of telescope centre in
                                ! an input file
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                ! locator to FITS extension in input file
      DOUBLE PRECISION IN_LAT_RAD ! latitude of telescope centre in input
                                ! file (radians)
      DOUBLE PRECISION IN_LAT2_RAD ! latitude of telescope centre at MJD2
                                ! (radians)
      DOUBLE PRECISION IN_LONG_RAD ! longitude of telescope centre in
                                ! input file (radians)
      DOUBLE PRECISION IN_LONG2_RAD ! longitude of telescope centre at MJD2
                                ! (radians)
      DOUBLE PRECISION IN_MJD1  ! modified Julian day at which object
                                ! was at IN_LAT,IN_LONG for PLANET
                                ! centre coordinate system
      DOUBLE PRECISION IN_MJD2  ! modified Julian day at which object
                                ! was at IN_LAT2,IN_LONG2 for PLANET
                                ! centre coordinate system
      DOUBLE PRECISION IN_ROTATION ! angle between apparent N and N of
                                ! input coord system (radians)
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                ! locator to SCUCD extension in input
                                ! file
      INTEGER          ITEMP    ! Scratch int
      REAL             LAG      ! Start lag
      DOUBLE PRECISION LAT_OBS  ! Latitude of observatory
      DOUBLE PRECISION LST      ! An LST time for testing
      DOUBLE PRECISION LST_AZEL ! LST derived from az el
      INTEGER          LST_STRT_PTR ! pointer to .SCUBA.LST_STRT
      INTEGER          NERR     ! For VEC_
      INTEGER          NLOOPS   ! Number of iterations for clock err
      INTEGER          N_FITS   ! Number of fits header items
      CHARACTER*40     OBJECT   ! name of object
      CHARACTER*40     OBSERVING_MODE ! observing mode of input file
      DOUBLE PRECISION POSERR   ! Positional error at edge of array
      DOUBLE PRECISION RA_CEN   ! apparent RA of input file map centre
                                ! (radians)
      REAL             RTEMP    ! Scratch real
      INTEGER          RUN_NUMBER ! run number of input file
      INTEGER          SLA_STATUS ! Slalib status
      CHARACTER*80     STATE    ! 'state' of SCUCD at the end of
                                ! the observation
      CHARACTER*80     STEMP    ! Scratch character string
      REAL             U3_OFF   ! NA X position of test bolometer
      REAL             U4_OFF   ! NA Y position of test bolometer
      DOUBLE PRECISION UT1      ! UT1 at start of an input observation,
                                ! expressed as modified Julian day
      DOUBLE PRECISION UT1_REF  ! UT1 at start of an input observation,
                                ! uncorrected (MJD)
      DOUBLE PRECISION XI       ! X offset between bol 1 and 2 (radians)
      DOUBLE PRECISION XPOS(2)  ! app RA position of bols/ arcsec offsets
      DOUBLE PRECISION YPOS(2)  ! app Dec position of bols/arcsec offsets

*.

      IF (STATUS .NE. SAI__OK) RETURN




*     start up the NDF system and read in the input demodulated file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

*     Get the SCUCD and FITS extensions
      CALL NDF_XLOC (INDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)

*     Read the FITS array
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
      CALL DAT_ANNUL( IN_FITSX_LOC, STATUS )

*     Some general information
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :     RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :     OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :     OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM,' ',
     :     '^PKG: run ^RUN was a ^MODE observation of ^OBJECT',
     :     STATUS)

*     Indicate that we cant do this calculation for all obs modes
      IF (STATUS .EQ. SAI__OK) THEN
         IF ( OBSERVING_MODE .EQ. 'NOISE' .OR.
     :        OBSERVING_MODE .EQ. 'SKYDIP') THEN
            CALL MSG_SETC('MODE', OBSERVING_MODE)
            CALL MSG_SETC('TSK', TSKNAME)
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','^TSK: Can not determine clock'//
     :           ' error from observations of mode ^MODE',
     :           STATUS)
         END IF
      END IF

*     see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         CALL MSG_OUTIF(MSG__NORM, ' ',
     :        ' - Observation was aborted', STATUS)
      END IF

*     Read LST_STRT array
      CALL CMP_MAPV(IN_SCUCDX_LOC, 'LST_STRT', '_DOUBLE', 'READ',
     :     LST_STRT_PTR, ITEMP, STATUS)

*     Copy the LST_STRT value to a scalar
      CALL VEC_DTOD( .FALSE., 1, %VAL(CNF_PVAL(LST_STRT_PTR)), LST,
     :     IERR, NERR, STATUS)

*     ...and check to see that it contains something valid (non zero)
*     Zero probably indicates an observation that was aborted before
*     any data was taken.

      IF (STATUS .EQ. SAI__OK) THEN
         IF (LST .LT. 1.0D-200) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC('TSK',TSKNAME)
            CALL ERR_REP( ' ', '^TSK: LST_STRT array has no '//
     :           'values - can not determine clock error',
     :           STATUS)

         END IF

      END IF

*     UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, %VAL(CNF_PVAL(LST_STRT_PTR)),
     :                    UT1,
     :     RTEMP, LAG, STATUS)


*     Read the latitude of the observatory
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*     coords of telescope centre

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CENT_CRD', IN_CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (IN_CENTRE_COORDS)

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LAT', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LONG', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG_RAD, STATUS)

      IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LONG2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG2_RAD,
     :        STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MJD1', IN_MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MJD2', IN_MJD2, STATUS)
      END IF

      IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :     (IN_CENTRE_COORDS .NE. 'GA') .AND.
     :     (IN_CENTRE_COORDS .NE. 'PL') .AND.
     :     (IN_CENTRE_COORDS .NE. 'NA')) THEN
         IN_LONG_RAD = IN_LONG_RAD * 15.0D0
         IN_LONG2_RAD = IN_LONG2_RAD * 15.0D0
      END IF

*     For PLANET observation we need to iterate over the calculation
*     of the clock error since the source is moving and we need to
*     work out where the apparent RA/Dec of the moving source taking
*     into account the clock error. Only need to loop once for stationary
*     objects and twice for moving sources

      IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
         NLOOPS = 2
      ELSE
         NLOOPS = 1
      END IF

*     Reset clock error and store the uncorrected MJD
      CLOCK_ERR = 0.0D0
      UT1_REF = UT1

      DO I = 1, NLOOPS

*     Calculate new MJD
         UT1 = UT1_REF + ( CLOCK_ERR / D2PI )

*     Calculate the apparent RA/Dec of the tracking centre
*     This does not take into account MAP_X or MAP_Y offsets
*     Would have to call SURFLIB_PROCESS_BOLS for that. This
*     is a good enough approximation. Note that the MJD will be
*     corrected for errors in the start up time but, obviously,
*     not for the clock error we are trying to determine.
*     Hopefully this will be good enough.

         CALL SCULIB_CALC_APPARENT (LAT_OBS, IN_LONG_RAD, IN_LAT_RAD,
     :        IN_LONG2_RAD, IN_LAT2_RAD, 0.0D0, 0.0D0,
     :        IN_CENTRE_COORDS, %VAL(CNF_PVAL(LST_STRT_PTR)), UT1,
     :        IN_MJD1, IN_MJD2, RA_CEN, DEC_CEN, IN_ROTATION,
     :        STATUS)

*         print *,'MJD', UT1, RA_CEN, DEC_CEN
*         print *,'INPUT', IN_LONG_RAD, IN_LAT_RAD

*     Now get the clock error
         CALL SCULIB_CALC_CLOCKERR( N_FITS, FITS, RA_CEN,
     :        %VAL(CNF_PVAL(LST_STRT_PTR)),
     :        CLOCK_ERR, LST_AZEL, STATUS )

      END DO

*     Calculate the new MJD reference
      UT1 = UT1_REF + ( CLOCK_ERR / D2PI )

*     It is useful to translate this clock error into a real
*     SCUBA error by calculating the rotation angle and translating
*     this to arcseconds on the edge of the array

*     Choose an arbritrary position towards the edge of the array

      U3_OFF = 0.0
      U4_OFF = 70.0

*     First calculate the position for the incorrect time
*     Focal station is not relevant for this usage of calc_bol_coords
      CALL SCULIB_CALC_BOL_COORDS( 'RA', 'LNASMYTH',RA_CEN, DEC_CEN,
     :     LST, LAT_OBS, 'NA', 0.0, 0.0,
     :     IN_ROTATION, 0, 0, DTEMP, RTEMP, RTEMP, 1, 1,
     :     1, 1, 1, U3_OFF, U4_OFF, 0.0, 0.0, XPOS, YPOS, DTEMP,
     :     DTEMP, STATUS)

*     Now calculate the position when compensating for the clock error
      LST = LST + CLOCK_ERR

      CALL SCULIB_CALC_BOL_COORDS( 'RA', 'LNASMYTH',RA_CEN, DEC_CEN,
     :     LST, LAT_OBS, 'NA', 0.0, 0.0,
     :     IN_ROTATION, 0, 0, DTEMP, RTEMP, RTEMP, 1, 1,
     :     1, 1, 1, U3_OFF, U4_OFF, 0.0, 0.0, XPOS(2),YPOS(2), DTEMP,
     :     DTEMP, STATUS)

*     Calculate the position error of one position centred
*     at the other

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SLA_DS2TP( XPOS(1), YPOS(1), XPOS(2), YPOS(2), XI,
     :        ETA, SLA_STATUS)

         IF (SLA_STATUS .EQ. 0) THEN
*     Calculate the distance
            POSERR = SQRT( XI**2 + ETA**2 )

         ELSE
*     Had an error
            STATUS = SAI__ERROR
            CALL MSG_SETC('TSK',TSKNAME)
            CALL MSG_SETI('S', SLA_STATUS)
            CALL ERR_REP(' ','^TSK: Error determining tangent plane'//
     :           ' offset of test bolometer (SLA status=^S)', STATUS)
         END IF

      END IF

*     Calculate the angular error (more important for polarimetry)
*     Convert the two apparent ra/dec positions to arcsec offsets
*     from the map centre

      CALL SCULIB_APPARENT_2_TP( 2, XPOS, YPOS, RA_CEN, DEC_CEN,
     :     IN_ROTATION, 0.0D0, 0.0D0, STATUS)

*     Calculate the distance (should be poserr)
      DTEMP = SQRT( (XPOS(1)-XPOS(2))**2 + (YPOS(1)-YPOS(2))**2)

*     Isosceles triangle
      ANG = 2.0D0 * ASIN( DR2AS * DTEMP / ( 2.0D0 * DBLE(U4_OFF)))

*     Print out all the times from the 3 different header items

*     STSTART
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'STSTART', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, DTEMP, STATUS)
      DTEMP = DTEMP * 15.0D0
      CALL SCULIB_RAD2STRING(DTEMP, 1, .TRUE., STEMP,
     :     STATUS )
      CALL MSG_SETC('LST', STEMP)
      CALL MSG_OUTIF(MSG__NORM, ' ',' Time stored in STSTART  : ^LST',
     :     STATUS )

*     LST_STRT
      CALL SCULIB_RAD2STRING(%VAL(CNF_PVAL(LST_STRT_PTR)),
     :                       1, .TRUE., STEMP,
     :     STATUS )
      CALL MSG_SETC( 'LST', STEMP )
      CALL MSG_OUTIF(MSG__NORM, ' ',' Time stored in LST_STRT : ^LST',
     :     STATUS )

*     Time derived from AZEL
      CALL SCULIB_RAD2STRING(LST_AZEL, 1, .TRUE., STEMP,
     :     STATUS )
      CALL MSG_SETC( 'LST', STEMP )
      CALL MSG_OUTIF(MSG__NORM, ' ',' Time derived from header: ^LST',
     :     STATUS )

*     Unmap LST array
      CALL CMP_UNMAP( IN_SCUCDX_LOC, 'LST_STRT', STATUS )

*     annul file
      CALL DAT_ANNUL( IN_SCUCDX_LOC, STATUS)
      CALL NDF_ANNUL( INDF, STATUS)

      CALL NDF_END( STATUS )

*     Now update the output parameters
*     Clock error in seconds
      CALL PAR_PUT0R( 'CLOCKERR', REAL( CLOCK_ERR * DR2S ), STATUS)

*     Startup time
      CALL PAR_PUT0R( 'LAG', LAG, STATUS )

*     Error in position
      CALL PAR_PUT0R( 'DR', REAL( POSERR * DR2AS ), STATUS )

*     Put error in angle
      CALL PAR_PUT0R( 'DANG', REAL( ANG * DR2D), STATUS )

*     Corrected modified julian date
      CALL PAR_PUT0D( 'MJD', UT1, STATUS )

*     Messages
      CALL MSG_SETR( 'ERR', REAL( CLOCK_ERR * DR2S ) )
      CALL MSG_OUTIF( MSG__NORM, ' ', ' - Clock error: ^ERR', STATUS)
      CALL MSG_SETR( 'LAG', LAG )
      CALL MSG_OUTIF( MSG__NORM, ' ', ' - Startup lag: ^LAG', STATUS)
      CALL MSG_SETR( 'DANG', REAL( ANG * DR2D) )
      CALL MSG_OUTIF(MSG__NORM, ' ', ' - Error in rotation angle: '//
     :     '^DANG degrees', STATUS)
      CALL MSG_SETR( 'DX', REAL( POSERR * DR2AS ) )
      CALL MSG_SETR( 'RAD', U4_OFF )
      CALL MSG_OUTIF( MSG__NORM, ' ', ' - Error at ^RAD arcsec from '//
     :     'centre of array: ^DX arcsec ', STATUS )

      END
