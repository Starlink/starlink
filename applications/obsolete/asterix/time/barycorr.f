      SUBROUTINE BARYCORR( STATUS )
*+
*  Name:
*     BARYCORR

*  Purpose:
*     Performs barycentric corrections on binned and event data

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL BARYCORR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     In SIMPLE mode (hidden default = TRUE), then the mid-point of the
*     observation is determined. That time, together with the RA, Dec of
*     the observation is used to barycentrically correct the header components
*     BASE_TAI and BASE_UTC of the file. Handles event and binned data.
*
*     Otherwise, if a simple (global) approach is *not* wanted, the
*     application Barycentrically corrects timetag lists in event datasets.
*     If the event dataset is from ROSAT, then the orbit file is searched
*     for the relevant orbital parameters and the barycentric corrections
*     are made to the position of the satellite. The user dictates how often
*     the orbital position is updated. No provision is made for interpolating
*     at a higher frequency than the orbit data is available. (you expecting
*     a millisecond pulsar?).
*     If the event dataset is not ROSAT then the orbit file interface will
*     not be known, so the program can correct the timetags assuming zero
*     orbital height. This assumption is inaccurate by up to +/-50msec.
*
*      STRUCTURE /POS_REC/
*         INTEGER UT/0/				! hk clock (1/2s) time
*         REAL SATGEO(3)/0.,0.,0./		! Sat vector in geo frame
*         INTEGER*2 IXRT(3)/0,0,0/		! RA, dec, roll, arcmin
*         INTEGER*2 IBGLONG                      ! long and lat
*         INTEGER*2 IBGLAT                       !
*         INTEGER*2 IBSIZ_EM8T                   ! B field. 10^{-8} Tesla
*         INTEGER*2 LVAL_MILLERAD
*      END STRUCTURE

*  Usage:
*     barycorr {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     barycorr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DB: Doug Bertram (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Jun 1990 V1.4-0 (DB):
*        Original version.
*     23 Nov 1990 V1.5-0 (DB):
*        Upgrade.
*     28 Mar 1994 V1.7-0 (DB):
*        Ditto (DB)
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     16 Jan 1996 V2.0-0 (DJA):
*        ADI port on event handling, put ROSAT orbit stuff back in.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Constants:
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'BARYCORR Version V2.0-0' )

*  Local Variables:
      CHARACTER*40             	INSTRUM     		! Instrument string
      CHARACTER*40             	OBSY           		! Observatory string
      CHARACTER*20		TLIST			! Name of time list

      DOUBLE PRECISION		EQUINOX			! Equinox of obs'n
      DOUBLE PRECISION 		MJDOBS			! Base MJD of i/p obs'n
      DOUBLE PRECISION		NPOINT(2)		! Nominal pointing
      DOUBLE PRECISION 		NU_MJDOBS		! Base MJD of o/p obs'n
      DOUBLE PRECISION          NU_BASE_TAI		!
      DOUBLE PRECISION 		TMIN, TMAX		! Input time range

      INTEGER			DETID			! Detector info
      INTEGER			IFID			! Input dataset
      INTEGER			NVAL			! # values read
      INTEGER			OFID			! Output dataset
      INTEGER			PIXID, PRJID, SYSID	! Astrometry
      INTEGER			TIMID			! Timing info
      INTEGER			TAX			! Time axis number

        INTEGER I
        INTEGER LUN_POS          ! Log Unit for ATT_POS_SAT output (rosat specific)
*        DOUBLE PRECISION BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*        INTEGER BARY_MJD2HK ! integer function MJD to HK 1/2 secs

      CHARACTER*80             POS_FILE       ! Name of orbit file
      CHARACTER*80             HISTXT(10)     ! add to HISTORY

      DOUBLE PRECISION         MJD_START      !MJD start of observation
      DOUBLE PRECISION         MJD_STOP       !MJD stop of observation
      DOUBLE PRECISION         BASE_UTC       !SECS since start of currentMJD
      DOUBLE PRECISION         BASE_TAI       !continous time (in days) from 1/1/72

      REAL                     RA,DEC         ! REAL RA nad Dec
      REAL                     OBS_LENGTH     !Obs Length of event dataset
      REAL                     UPDATE_PERIOD  ! update period for bary corr
      REAL                     DEF_UPD_PRD    ! 1% of max - min
      REAL                     BASE,SCALE,DS  ! Base & Scale of time axis


      INTEGER                  WIDTHPTR       !PTR to WIDTH time axis
      INTEGER                  TIMEPTR        !PTR to RAW_TIMETAG or TIME axis
      INTEGER                  ITIMEPTR       !PTR to input time events/axis
      INTEGER                  DUMMYPTR       !PTR to DUMMY
      INTEGER                  LDIM(ADI__MXDIM) ! input dimensions
      INTEGER                  NDIMS          ! number of dimensions
      INTEGER                  T_BINS         ! number of time bins

      INTEGER                  N_LINES        ! number of lines to HISTORY
*      INTEGER                  LUN_POS        ! LUN for Orbit File.
*      INTEGER                  FIRST_KEY      ! first key in POS file
*      INTEGER                  SECOND_KEY     ! second key in POS file
*      INTEGER                  BASE_KEY       ! first relevant KEY in pos file
*      INTEGER                  END_KEY        ! last relevant KEY in pos file
      INTEGER                  RECORD_SEP     ! The POS file update period (sec)
      INTEGER                  ACTVAL         ! LIST MAPV reads
      INTEGER                  NEVENT       ! No of events in list
      INTEGER                  FILE_STATUS    ! IOSTAT for ATT POS SAT open
      INTEGER                  FILE_START     !   "     "   "   "   " base key
      INTEGER                  FILE_END       !   "     "   "   "   " end key
      INTEGER                  BASE_MJD       ! MJD whole part only

      LOGICAL                  OK,OK1,OK2     ! various checks
      LOGICAL                  VALID_ROSAT    ! file is a ROSAT file
      LOGICAL                  SATCORR        ! do satellite corrections ?
      LOGICAL                  POS_FILE_OK    ! ATT POS OPEN OK
      LOGICAL                  NOT_COVERED    ! ATT POS FILE doesn't cover..
      LOGICAL                  IGNORE_POS     ! if pos file not OK for all data
      LOGICAL                  PRESS_ON       ! press on regardless
      LOGICAL                  RAW_TIME_OK    ! raw timetags used
      LOGICAL                  TIME_OK        ! `TIMETAG'  list used.
      LOGICAL                  NOT_ROS_GO     ! not a rosat file ... but GO!!
      LOGICAL                  SIMPLE    ! Run BARYCORR in SIMPLE
      LOGICAL                  ULTRA_SIMPLE   ! run BARYCORR in ultra_simple mode
      LOGICAL                  ABORT          ! abort barycorr
      LOGICAL                  AX_REG         ! is the axis regular
      LOGICAL                  BINNED         ! complex mode, data binned
      LOGICAL                  AX_WID_OK      ! AXIS widths are present
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Initialise
      UPDATE_PERIOD = 0.0
      POS_FILE_OK = .FALSE.
      IGNORE_POS = .FALSE.
      TIME_OK = .FALSE.
      ULTRA_SIMPLE = .FALSE.

*  Get input data
      CALL USI_ASSOC( 'INP', 'EventDS|BinDS', 'READ', IFID, STATUS )
      CALL USI_SHOW(' Input data {INP}',status)

*   What kind of object did the user supply?
      CALL ADI_DERVD( IFID, 'BinDS', BINNED, STATUS )

*  Get output dataset as clone of input
      CALL USI_CLONE( 'INP', 'OUT', '*', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Does the user want simple mode?
      CALL USI_GET0L( 'SIMPLE', SIMPLE, STATUS )
      IF ( SIMPLE ) THEN
        SATCORR = .FALSE.
        POS_FILE_OK = .FALSE.
      END IF

*  Obtaining the observatory information is not necessary in simple mode
      IF ( .NOT. SIMPLE ) THEN

*    Check for ROSATish things
        CALL DCI_GETID( IFID, DETID, STATUS )
        CALL ADI_CGET0C( DETID, 'Mission', OBSY, STATUS )
        VALID_ROSAT = .FALSE.
        IF ( OBSY(1:6) .EQ. 'ROSAT' ) THEN
          VALID_ROSAT = .TRUE.
        ELSE
          CALL ADI_CGET0C( DETID, 'Instrument', INSTRUM, STATUS )
          VALID_ROSAT = CHR_INSET( 'WFC,HRI,PSPC', INSTRUM )
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        END IF

*    Not ROSAT
        IF ( .NOT. VALID_ROSAT ) THEN
          CALL MSG_PRNT( 'File is not a recognisable ROSAT file. No '/
     :                   /'orbit data can be taken into account. '/
     :                   /'Earth-centred Barycentric corrections will'/
     :                   /'be applied if all relevant info is present.')
          SIMPLE = .TRUE.
        END IF

      END IF

*  Binned data
      IF ( BINNED ) THEN

*    Get dimensionality
        CALL BDI_GETSHP( OFID, ADI__MXDIM, LDIM, NDIMS, STATUS )

*    Locate time axis
        CALL BDI0_FNDAXC( OFID, 'T', TAX, STATUS )

*    No time axis means ultra simple mode
        IF ( STATUS .LE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          ULTRA_SIMPLE = .TRUE.

        ELSE

*      Map input time axis
          NEVENT = LDIM(TAX)
          CALL BDI_AXMAPD( IFID, TAX, 'Data', 'READ', ITIMEPTR, STATUS )

*      Map output
          CALL BDI_AXMAPD( OFID, TAX, 'Data', 'UPDATE', TIMEPTR,
     :                     STATUS )

*      Widths present?
          CALL BDI_AXCHK( IFID, TAX, 'Width', AX_WID_OK, STATUS )
          IF ( AX_WID_OK ) THEN
            CALL BDI_AXMAPD( OFID, TAX, 'Width', 'UPDATE', WIDTHPTR,
     :                       STATUS )
          ELSE
            CALL MSG_PRNT(' Time axis WIDTH array is missing')
            CALL MSG_PRNT(' BARYCORR applied only on data array')
          END IF

        END IF

      ELSE

*    Mark as event dataset
        BINNED = .FALSE.

*    Get event dataset info
        CALL EDI_GETNS( IFID, NEVENT, NVAL, STATUS )

*    Define the default to the TLIST parameter
        CALL EDI_DEFLD( IFID, 'TLIST', 'T', 'name', STATUS )

*    Get name of list to use as source of event time offsets
        CALL EDI_SELCTN( IFID, 'TLIST', TLIST, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map the time list
        CALL EDI_MAPR( OFID, TLIST, 'UPDATE', 0, 0, TIMEPTR, STATUS )

*    Trap status if timetag not ok
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'L', TLIST )
          CALL ERR_REP( ' ', 'FATAL error mapping ^L list', STATUS )
          GOTO 99
        END IF

      END IF

*  Find range of times supplied
      IF ( .NOT. ULTRA_SIMPLE ) THEN
        CALL ARR_RANG1D( NEVENT, %VAL(TIMEPTR), TMIN, TMAX, STATUS )
        CALL MSG_SETI( 'EVENTS', NEVENT )
        IF ( BINNED ) THEN
          CALL MSG_PRNT( 'There are ^EVENTS bins in this file' )
        ELSE
          CALL MSG_PRNT( 'There are ^EVENTS events in this file' )
        END IF
      END IF

*  Check that we haven't already applied the correction
      CALL PRF_GET( OFID, 'BARY_CORR_DONE', OK2, STATUS )
      IF ( OK2 ) THEN
        CALL MSG_PRNT( 'This file has already been corrected, '/
     :                 /'exiting...')
        GOTO 99
      END IF

*  Obtain RA, DEC of field centre, and equinox of coordinates
      CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL ADI_CGET1D( PRJID, 'NPOINT', 2, NPOINT, NVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI_CGET1D( PRJID, 'SPOINT', 2, NPOINT, NVAL, STATUS )
      END IF
      CALL ADI_CGET0D( SYSID, 'EQUINOX', EQUINOX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT(' FATAL error obtaining RA and DEC of'//
     :                    ' field centre')
        GOTO 99
      ELSE
        RA = NPOINT(1)
        DEC = NPOINT(2)
      END IF

*  Get TAI offsets in the day
      CALL TCI_GETID( OFID, TIMID, STATUS )
      CALL ADI_CGET0D( TIMID, 'MJDObs', MJDOBS, STATUS )
      BASE_MJD = INT(MJDOBS)
      BASE_UTC = (MJDOBS - DBLE(BASE_MJD))*86400D0
      CALL ADI_CGET0D( TIMID, 'TAIObs', BASE_TAI, STATUS )
      CALL ADI_CGET0D( TIMID, 'ObsLength', OBS_LENGTH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'FATAL error obtaining timing parameters',
     :                STATUS )
        GOTO 99
      END IF

*if ROSAT then -> get LUN for position file.  ATT_POS_SAT output
*        IF(VALID_ROSAT .AND.( .NOT. SIMPLE) )THEN
*
* Ask if orbit correction is required
*          CALL USI_GET0L('SATCORR', SATCORR, STATUS)
* The unix version of barycorr doesn't support the orbital file format
           SATCORR = .FALSE.
*
*          IF (STATUS .NE. SAI__OK) GOTO 99
*
*   Is satellite correction required ?
*          IF (SATCORR) THEN
*
*    Get the name of the orbit file
*             CALL USI_GET0C('POS_FILE', POS_FILE, STATUS)
*
*             IF (STATUS .NE. SAI__OK) GOTO 99
*
*             CALL FIO_GUNIT(LUN_POS,STATUS)
*
*   open the keyed access file
*   	     OPEN (UNIT=LUN_POS, FILE=POS_FILE, STATUS='OLD', READONLY,
*     :	     ORGANIZATION='INDEXED', ACCESS='KEYED', FORM='UNFORMATTED',
*     :	     KEY=(1:4:INTEGER), IOSTAT=FILE_STATUS)
*
*   If file can't be opened type an error message and exit.
*   	     IF (FILE_STATUS .NE. 0) THEN
*                CALL MSG_PRNT('**Error opening position record file**')
*                GOTO 99
*             ENDIF
*
*          ENDIF
*
*       ELSE
*
*         CALL MSG_PRNT('No orbital data, Performing Earth-centred '/
*     &                /'correction')
*         SATCORR = .FALSE.
*
*       ENDIF
*
* Read the orbit file if satellite correction wanted
       IF (SATCORR) THEN
*
* compute start key in pos file from the MIN and MAX times
*          MJD_START = DBLE(BASE_MJD) +
*     :                       (BASE_UTC+DBLE(TMIN))/(8.64D04)
*          MJD_STOP  = DBLE(BASE_MJD) +
*     :                       (BASE_UTC + DBLE(TMAX))/8.64D04
*          BASE_KEY = BARY_MJD2HK(MJD_START)
*          END_KEY = BARY_MJD2HK(MJD_STOP)
*
* trial read of ATT_POS_SAT file to ensure event dataset ON time is included
*          READ(LUN_POS, KEYGE=BASE_KEY, IOSTAT=FILE_START)POS
*          BASE_KEY = POS.UT
*          READ(LUN_POS, KEYGE=END_KEY, IOSTAT=FILE_END)POS
*          END_KEY = POS.UT
*
*          IF (FILE_START .NE. 0 .OR. FILE_END .NE. 0 ) THEN
*             POS_FILE_OK = .FALSE.
*             NOT_COVERED = .TRUE.
*             CALL MSG_PRNT
*     :              (' POS file does not cover whole observation')
*
*       pos file doesn't cover period. So do Earth centred corrections or not ?
*             CALL  USI_DEF0L('GO_ON', .TRUE. ,STATUS)
*             CALL  USI_GET0L('GO_ON', IGNORE_POS, STATUS)
*
*             IF (STATUS .NE. SAI__OK) GOTO 99
*
*             IF( .NOT. IGNORE_POS )THEN
*               CALL MSG_PRNT(' exiting ... ')
*                GOTO 99
*             ELSE
*               PRESS_ON = .TRUE.
*                SATCORR = .FALSE.
*             ENDIF
*
*          ELSE
*             POS_FILE_OK = .TRUE.
*
*  locate temporal separataion between records
*             READ(UNIT=LUN_POS,IOSTAT=FILE_STATUS)POS
*             FIRST_KEY = POS.UT
*             READ(UNIT=LUN_POS,IOSTAT=FILE_STATUS)POS
*             SECOND_KEY = POS.UT
* keyed in 1/2 second units
*             RECORD_SEP = (SECOND_KEY - FIRST_KEY)/2
*
* obtain Barycentric calculation frequency
*             CALL USI_DEF0R('UPDATE_PRD', REAL(RECORD_SEP), STATUS)
*             CALL USI_GET0R('UPDATE_PRD', UPDATE_PERIOD, STATUS)
*
* Eliminate Stupid ideas
*             IF(UPDATE_PERIOD .LT. REAL(RECORD_SEP))THEN
*                CALL MSG_PRNT('The corrections will be updated at')
*                CALL MSG_PRNT('the highest available frequency')
*               UPDATE_PERIOD = REAL (RECORD_SEP)
*             ENDIF
*          ENDIF
*
       ELSE         ! Not SATCORR
*
          POS_FILE_OK = .FALSE.
          IGNORE_POS = .TRUE.
*
       ENDIF
*
*  avoid handing undefined LUN_POS to sub-routine
      IF( .NOT. POS_FILE_OK ) THEN
         LUN_POS=0
      ENDIF

*  Reset the desired update period, if Earth centred corrections required
      IF ( SIMPLE ) THEN
        UPDATE_PERIOD = (TMAX-TMIN)/100.0
      ELSE IF (.NOT. SATCORR)THEN
        IF(BINNED .AND. AX_WID_OK)THEN
          UPDATE_PERIOD = 0.0
* update period not used in this mode
        ELSE
          DEF_UPD_PRD = (TMAX - TMIN)/100.0
          CALL USI_DEF0R('UPDATE_INT',DEF_UPD_PRD,STATUS)
          CALL USI_GET0R('UPDATE_INT', UPDATE_PERIOD, STATUS)
        END IF
      END IF

*  Call main routine
      IF ( SIMPLE ) THEN
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_FLUSH( STATUS )
        END IF

        CALL BARY_SIMPLE( MJDOBS, BASE_TAI, RA, DEC,
     :                   EQUINOX, ULTRA_SIMPLE, TMAX, TMIN,
     :                   NU_BASE_TAI, NU_MJDOBS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Has timing origin changed?
        IF ( NU_MJDOBS .NE. MJDOBS ) THEN
          CALL ADI_CPUT0D( TIMID, 'MJDObs', NU_MJDOBS, STATUS )
        END IF
        CALL ADI_CPUT0D( TIMID, 'TAIObs', NU_BASE_TAI, STATUS )

*    Write back updated timing data
        CALL TCI_PUTID( OFID, TIMID, STATUS )

*    Update HISTORY
        HISTXT(1) = 'Barycorr was run in simple mode. '

        HISTXT(2) =
     :    'Single Barycentric correction applied to header components'
        N_LINES = 2

      ELSE
       IF (.NOT. BINNED)THEN
         AX_WID_OK = .FALSE.
         ITIMEPTR = TIMEPTR
         WIDTHPTR = TIMEPTR
       ENDIF
       CALL BARY_CORR( %VAL(ITIMEPTR), %VAL(TIMEPTR), %VAL(WIDTHPTR),
     :        NEVENT,RA,DEC,BINNED,
     :        LUN_POS,POS_FILE_OK,SATCORR,IGNORE_POS,AX_WID_OK,BASE_MJD,
     :        BASE_UTC,BASE_TAI,UPDATE_PERIOD,EQUINOX,STATUS)

* sub routine Barr_corr takes the appropriate action to the time list
*
* update HISTORY
       CALL HSI_ADD(OFID, VERSION, STATUS)
       IF(POS_FILE_OK)THEN
        HISTXT(1) = ' a valid orbit data file was found and used'
        WRITE ( HISTXT(2), '(A,F9.1,A)')
     :  ' The corrections were updated every ',UPDATE_PERIOD, ' seconds'
        N_LINES = 2
       ELSE
        N_LINES = 0
        IF(IGNORE_POS)THEN
          N_LINES = N_LINES + 1
          HISTXT(N_LINES) =
     :' The orbit file was not available or not appropriate'
          N_LINES = N_LINES + 1
          IF(UPDATE_PERIOD .LT. (TMAX-TMIN))THEN
            WRITE ( HISTXT(N_LINES), '(A,F9.1,A)')
     :  ' The corrections were updated every ',UPDATE_PERIOD, ' seconds'
          ELSE
            HISTXT(N_LINES) =
     :     ' single Barycentric correction applied to all timetags'
          ENDIF
        ENDIF
        IF(.NOT. SATCORR)THEN
          N_LINES = N_LINES + 1
          HISTXT(N_LINES) =
     :'EARTH-CENTRED barycentric correction applied'
          N_LINES = N_LINES + 1
          HISTXT(N_LINES) =
     :' The motions of the satellite were ignored'
          N_LINES = N_LINES + 1
          IF(UPDATE_PERIOD .LT. (TMAX-TMIN))THEN
            WRITE ( HISTXT(N_LINES), '(A,F9.1,A)')
     :  ' The corrections were updated every ',UPDATE_PERIOD, ' seconds'
          ELSE
            HISTXT(N_LINES) =
     :     ' single Barycentric correction applied to all timetags'
          ENDIF
        ENDIF
       ENDIF
      ENDIF

*  Update the history
      CALL HSI_PTXT( OFID, N_LINES, HISTXT, STATUS )

*  Create new flag in file
      CALL PRF_SET( OFID, 'BARY_CORR_DONE', .TRUE., STATUS )

*  Tidy up and exit
 99   IF ( BINNED ) THEN
        CALL EDI_UNMAP( OFID, TLIST, STATUS )
      END IF

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+ BARY_BARCOR Computes barycentric/heliocentric coordinates of Earth.
	SUBROUTINE BARY_BARCOR(DCORH,DCORB)
	IMPLICIT DOUBLE PRECISION (D)
	DIMENSION DCORH(3),DCORB(3)
*	Input	Common block /BARY_CMN/ computed by prior call of BARY_BARVEL.
*DCORH	Output	Heliocentric coordinates of Earth
*		in x,y,z directions in A.U.
*DCORB	Output	Barycentric coordinates as above.
*-
*Accuracy: The largest deviations from the JPL-DE96 are
*     0.000011 A.U. (5 millisecs) for the heliocentric,
*     and 0.000046 A.U. (23 millisecs) for the barycentric coordinates.
*Must by preceded by CALL BARY_BARVEL to set values in common block.
*
* Written by P.Stumpff	1979 July 15.
* Modified for Standard Fortran by C.G. Page 1983 Sept 27.
*
	DIMENSION CCPAM(4)
	COMMON /BARY_CMN/ DPREMA(3,3), DPSI, D1PDRO, DSINLS, DCOSLS,
     &   DSINEP, DCOSEP, FORBEL(7), SORBEL(17), SINLP(4), COSLP(4),
     &   SINLM, COSLM, SIGMA, IDEQ

*   CCPAM(K) = A*M(PLANETS),CCIM = INCLINATION(MOON),DC1MME = 1-MASS(EMB)

	PARAMETER (CCIM = 8.978749E-2, DC1MME = 0.99999696D0)
	DATA CCPAM/4.960906E-3,2.727436E-3,8.392311E-4,1.556861E-3/

*     HELIOCENTRIC COORDINATES OF THE EARTH (BARVL197-232)

	DR    = DPSI*D1PDRO
	FLATM = CCIM*SIN(FORBEL(3))
	A     = SIGMA*COS(FLATM)
	DXH   = DR*DCOSLS - A*COSLM
	DYH   = DR*DSINLS - A*SINLM
	DZH   = -SIGMA*SIN(FLATM)

*     BARYCENTRIC COORDINATES OF THE EARTH (BARVL234-248)

	DXB = DXH*DC1MME
	DYB = DYH*DC1MME
	DZB = DZH*DC1MME
	DO 10, K = 1,4
	    FLAT = SORBEL(K+13)*SIN(FORBEL(K+3)-SORBEL(K+5))
	    A = CCPAM(K)*(1.0-SORBEL(K+9)*COS(FORBEL(K+3)-
     &		SORBEL(K+1)))
	    B = A*COS(FLAT)
	    DXB = DXB-B*COSLP(K)
	    DYB = DYB-B*SINLP(K)
	    DZB = DZB-A*SIN(FLAT)
10  	CONTINUE

*    TRANSITION TO MEAN EQUATOR OF DATE (BARVL250-256)

	DYAH = DCOSEP*DYH-DSINEP*DZH
	DZAH = DSINEP*DYH+DCOSEP*DZH
	DYAB = DCOSEP*DYB-DSINEP*DZB
	DZAB = DSINEP*DYB+DCOSEP*DZB

	IF(IDEQ .EQ. 0) THEN
	    DCORH(1) = DXH
	    DCORH(2) = DYAH
	    DCORH(3) = DZAH
	    DCORB(1) = DXB
	    DCORB(2) = DYAB
	    DCORB(3) = DZAB
	ELSE

*     GENERAL PRECESSION FROM EPOCH DJE TO DEQ (BARVL267-275)

	    DO 30, N = 1,3
		DCORH(N) = DXH*DPREMA(N,1)+DYAH*DPREMA(N,2)+
     &			DZAH*DPREMA(N,3)
		DCORB(N) = DXB*DPREMA(N,1)+DYAB*DPREMA(N,2)+
     &			DZAB*DPREMA(N,3)
30  	    CONTINUE
	END IF
	END

*+BARY_BARPRE	Computes matrix of general precession from epochs DEQ1 to DEQ2
	SUBROUTINE BARY_BARPRE(DEQ1,DEQ2,DPREMA)
	IMPLICIT DOUBLE PRECISION (D)
	DIMENSION DPREMA(3,3)
*DEQ1	Input	First epoch, years.
*DEQ2	Input	Second epoch, years.
*DPREMA	Output	Precession matrix.
*-
*Author P.Stumpff, Astron & Astophys 1979 July 15.
*    THE PRECESSION ANGLES (DZETA,DZETT,DTHET) ARE COMPUTED FROM THE
*    constants (dc1-dc9) corresponding to the definitions in the
*    explanatory supplement to the astronomical emphemeris (1961,p30f).
*
	PARAMETER (DCSAR = 4.848136812D-6,DC1900 = 1900.0D0,
     &     DC1M2 = 0.01D0,
     &     DC1 = 2304.25D0,DC2 = 1.396D0,DC3 = 0.302D0,DC4 = 0.018D0,
     &     DC5 = 0.791D0,DC6 = 2004.683D0,DC7 = -0.853D0,DC8 = -0.426D0,
     &     DC9 = -0.042D0)
	DT0 = (DEQ1-DC1900)*DC1M2
	DT  = (DEQ2-DEQ1)*DC1M2
	DTS = DT*DT
	DTC = DTS*DT
	DZETA = ((DC1+DC2*DT0)*DT+DC3*DTS+DC4*DTC)*DCSAR
	DZETT = DZETA + DC5*DTS*DCSAR
	DTHET = ((DC6+DC7*DT0)*DT+DC8*DTS+DC9*DTC)*DCSAR
	DSZETA = SIN(DZETA)
	DCZETA = COS(DZETA)
	DSZETT = SIN(DZETT)
	DCZETT = COS(DZETT)
	DSTHET = SIN(DTHET)
	DCTHET = COS(DTHET)
	DA = DSZETA*DSZETT
	DB = DCZETA*DSZETT
	DC = DSZETA*DCZETT
	DD = DCZETA*DCZETT
	DPREMA(1,1) = DD*DCTHET - DA
	DPREMA(1,2) = -DC*DCTHET - DB
	DPREMA(1,3) = -DSTHET*DCZETT
	DPREMA(2,1) = DB*DCTHET + DC
	DPREMA(2,2) = -DA*DCTHET + DD
	DPREMA(2,3) = -DSTHET*DSZETT
	DPREMA(3,1) = DCZETA*DSTHET
	DPREMA(3,2) = -DSZETA*DSTHET
	DPREMA(3,3) = DCTHET
	END

*+BARY_BARVEL	Returns barycentric/heliocentric velocity cmpts of Earth.
	SUBROUTINE BARY_BARVEL(DMJDE,DEQ,DVELH,DVELB)
	IMPLICIT DOUBLE PRECISION(D)
	DIMENSION DVELH(3),DVELB(3)
*DMJDE	Input	Specifies Modified Julian Date (ephemeris time).
*DEQ	Input	Specifies epoch of mean equator and equinox to use,
*		e.g. 1950.0D0; if DEQ = 0D0 then the mean equator
*		and equinox of DJE will be used.
*DVELH	Output	Heliocentric velocity components
*		for dx/dt, dy/dt, dz/dt in A.U. per second.
*DVELB	Output	Barycentric components as above.
*-
*    The COMMON block /BARY_CMN/ contains those intermediate results of
*    BARVEL which can be used to compute the heliocentric and
*    barycentric coordinates (SUBROUTINE BARCOR).
* Author	P. Stumpff, 1979 July 15,	Astron & Astrophys
*  Converted to standard Fortran by C.G.Page.
*Modified CGP 1985 Jan 24: takes JD or MJD of epoch.
*
	DIMENSION SN(4),DCFEL(3,8),DCEPS(3),CCSEL(3,17),DCARGS(2,15),
     &    CCAMPS(5,15),CCSEC(3,4),DCARGM(2,3),CCAMPM(4,3),CCPAMV(4)
	EQUIVALENCE (SORBEL(1),E),(FORBEL(1),G)

	COMMON /BARY_CMN/ DPREMA(3,3), DPSI, D1PDRO, DSINLS, DCOSLS,
     &   DSINEP, DCOSEP, FORBEL(7), SORBEL(17), SINLP(4), COSLP(4),
     &   SINLM, COSLM, SIGMA, IDEQ

	PARAMETER (DC2PI = 6.2831853071796D0, CC2PI = 6.283185,
     &	 DC1 = 1.0D0, DCT0 = 2415020.0D0, DCJUL = 36525.0D0,
     &   DCBES = 0.313D0, DCTROP = 365.24219572D0,
     &   DC1900 = 1900.0D0)

* SIDEREAL RATE DCSLD IN LONGITUDE, RATE CCSGD IN MEAN ANOMALY
	PARAMETER (DCSLD = 1.990987D-07, CCSGD = 1.990969E-07)

* SOME CONSTANTS USED IN THE CALCULATION OF THE LUNAR CONTRIBUTION
	PARAMETER(CCKM = 3.122140E-05, CCMLD = 2.661699E-06,
     &   CCFDI = 2.399485E-07)

* CONSTANTS DCFEL(I,K) OF FAST CHANGING ELEMENTS
*		       I = 1		I = 2		I = 3
	DATA DCFEL/ 1.7400353D+00, 6.2833195099091D+02, 5.2796D-06,
     &		    6.2565836D+00, 6.2830194572674D+02,-2.6180D-06,
     &              4.7199666D+00, 8.3997091449254D+03,-1.9780D-05,
     &              1.9636505D-01, 8.4334662911720D+03,-5.6044D-05,
     &              4.1547339D+00, 5.2993466764997D+01, 5.8845D-06,
     &              4.6524223D+00, 2.1354275911213D+01, 5.6797D-06,
     &              4.2620486D+00, 7.5025342197656D+00, 5.5317D-06,
     &              1.4740694D+00, 3.8377331909193D+00, 5.6093D-06/

* CONSTANTS DCEPS AND CCSEL(I,K) OF SLOWLY CHANGING ELEMENTS
*		      I = 1           I = 2           I = 3
	DATA DCEPS/ 4.093198D-01,-2.271110D-04,-2.860401D-08/
	DATA CCSEL/ 1.675104E-02,-4.179579E-05,-1.260516E-07,
     &		    2.220221E-01, 2.809917E-02, 1.852532E-05,
     &              1.589963E+00, 3.418075E-02, 1.430200E-05,
     &              2.994089E+00, 2.590824E-02, 4.155840E-06,
     &              8.155457E-01, 2.486352E-02, 6.836840E-06,
     &              1.735614E+00, 1.763719E-02, 6.370440E-06,
     &		    1.968564E+00, 1.524020E-02,-2.517152E-06,
     &              1.282417E+00, 8.703393E-03, 2.289292E-05,
     &		    2.280820E+00, 1.918010E-02, 4.484520E-06,
     &		    4.833473E-02, 1.641773E-04,-4.654200E-07,
     &		    5.589232E-02,-3.455092E-04,-7.388560E-07,
     &		    4.634443E-02,-2.658234E-05, 7.757000E-08,
     &		    8.997041E-03, 6.329728E-06,-1.939256E-09,
     &		    2.284178E-02,-9.941590E-05, 6.787400E-08,
     &		    4.350267E-02,-6.839749E-05,-2.714956E-07,
     &		    1.348204E-02, 1.091504E-05, 6.903760E-07,
     &		    3.106570E-02,-1.665665E-04,-1.590188E-07/

*   CONSTANTS OF THE ARGUMENTS OF THE SHORT-PERIOD PERTURBATIONS
*   BY THE PLANETS:   DCARGS(I,K)
*			I = 1		 I = 2
	DATA DCARGS/ 5.0974222D+00,-7.8604195454652D+02,
     &               3.9584962D+00,-5.7533848094674D+02,
     &		     1.6338070D+00,-1.1506769618935D+03,
     &		     2.5487111D+00,-3.9302097727326D+02,
     &		     4.9255514D+00,-5.8849265665348D+02,
     &               1.3363463D+00,-5.5076098609303D+02,
     &		     1.6072053D+00,-5.2237501616674D+02,
     &		     1.3629480D+00,-1.1790629318198D+03,
     &   	     5.5657014D+00,-1.0977134971135D+03,
     &		     5.0708205D+00,-1.5774000881978D+02,
     &		     3.9318944D+00, 5.2963464780000D+01,
     &		     4.8989497D+00, 3.9809289073258D+01,
     &		     1.3097446D+00, 7.7540959633708D+01,
     &		     3.5147141D+00, 7.9618578146517D+01,
     & 		     3.5413158D+00,-5.4868336758022D+02/

*   AMPLITUDES CCAMPS(N,K) OF THE SHORT-PERIOD PERTURBATIONS
*	      N = 1         N = 2          N = 3         N = 4          N = 5
	DATA CCAMPS/
     & -2.279594E-5, 1.407414E-5, 8.273188E-6, 1.340565E-5,-2.490817E-7,
     & -3.494537E-5, 2.860401E-7, 1.289448E-7, 1.627237E-5,-1.823138E-7,
     &  6.593466E-7, 1.322572E-5, 9.258695E-6,-4.674248E-7,-3.646275E-7,
     &  1.140767E-5,-2.049792E-5,-4.747930E-6,-2.638763E-6,-1.245408E-7,
     &  9.516893E-6,-2.748894E-6,-1.319381E-6,-4.549908E-6,-1.864821E-7,
     &  7.310990E-6,-1.924710E-6,-8.772849E-7,-3.334143E-6,-1.745256E-7,
     & -2.603449E-6, 7.359472E-6, 3.168357E-6, 1.119056E-6,-1.655307E-7,
     & -3.228859E-6, 1.308997E-7, 1.013137E-7, 2.403899E-6,-3.736225E-7,
     &  3.442177E-7, 2.671323E-6, 1.832858E-6,-2.394688E-7,-3.478444E-7,
     &  8.702406E-6,-8.421214E-6,-1.372341E-6,-1.455234E-6,-4.998479E-8,
     & -1.488378E-6,-1.251789E-5, 5.226868E-7,-2.049301E-7, 0.0E0,
     & -8.043059E-6,-2.991300E-6, 1.473654E-7,-3.154542E-7, 0.0E0,
     &  3.699128E-6,-3.316126E-6, 2.901257E-7, 3.407826E-7, 0.0E0,
     &  2.550120E-6,-1.241123E-6, 9.901116E-8, 2.210482E-7, 0.0E0,
     & -6.351059E-7, 2.341650E-6, 1.061492E-6, 2.878231E-7, 0.0E0/

*   CONSTANTS OF THE SECULAR PERTURBATIONS IN LONGITUDE
*   CCSEC3 AND CCSEC(N,K)
*		       N = 1	    N = 2		  N = 3
	DATA CCSEC3/-7.757020E-08/
	DATA CCSEC/ 1.289600E-06, 5.550147E-01, 2.076942E+00,
     &              3.102810E-05, 4.035027E+00, 3.525565E-01,
     &              9.124190E-06, 9.990265E-01, 2.622706E+00,
     &              9.793240E-07, 5.508259E+00, 1.559103E+01/

*   CONSTANTS DCARGM(I,K) OF THE ARGUMENTS OF THE PERTURBATIONS
*   OF THE MOTION OF THE MOON
*			I = 1			I = 2
	DATA DCARGM/  5.1679830D+00, 8.3286911095275D+03,
     &                5.4913150D+00,-7.2140632838100D+03,
     &		      5.9598530D+00, 1.5542754389685D+04/

*     AMPLITUDES  CCAMPM(N,K) OF THE PERTURBATIONS OF THE MOON
*	     N = 1     N = 2	   N = 3           N = 4
	DATA CCAMPM/
     & 1.097594E-01, 2.896773E-07, 5.450474E-02, 1.438491E-07,
     &-2.223581E-02, 5.083103E-08, 1.002548E-02,-2.291823E-08,
     & 1.148966E-02, 5.658888E-08, 8.249439E-03, 4.063015E-08/

*     CCPAMV(K) = A*M*DL/DT (PLANETS), DC1MME = 1-MASS(EARTH+MOON)

	DATA CCPAMV/8.326827E-11, 1.843484E-11, 1.988712E-12,
     &   1.881276E-12/,  DC1MME/0.99999696D0/

*     EXECUTION
*     CONTROL-PARAMETER IDEQ, AND TIME-ARGUMENTS

	IDEQ = NINT(DEQ)
*Test whether original or Modified Julian Date provided.
	IF(DMJDE .LT. 100000.0D0) THEN
	   DJE = DMJDE + 2400000.5D0
	ELSE
	   DJE = DMJDE
	END IF
	DT = (DJE-DCT0)/DCJUL
	T = DT
	DTSQ = DT*DT
	TSQ = DTSQ

*     VALUES OF ALL ELEMENTS FOR THE INSTANT DJE

	DO 100, K = 1,8
	  DLOCAL = MOD(DCFEL(1,K)+DT*DCFEL(2,K)+DTSQ*DCFEL(3,K), DC2PI)
	  IF(K .EQ. 1) THEN
		DML = DLOCAL
	  ELSE
		FORBEL(K-1) = DLOCAL
	  END IF
100 	CONTINUE

	DEPS = MOD(DCEPS(1)+DT*DCEPS(2)+DTSQ*DCEPS(3), DC2PI)

	DO 200, K = 1,17
	  SORBEL(K) = MOD(CCSEL(1,K)+T*CCSEL(2,K)+TSQ*CCSEL(3,K), CC2PI)
200 	CONTINUE

*     SECULAR PERTURABTIONS IN LONGITUDE

	DO 300, K = 1,4
	  A = MOD(CCSEC(2,K)+T*CCSEC(3,K), CC2PI)
	  SN(K) = SIN(A)
300 	CONTINUE

*     PERIODIC PERTURBATIONS OF THE EMB (EARTH-MOON BARYCENTER)

	PERTL = CCSEC(1,1) * SN(1) +CCSEC(1,2)*SN(2) +
     &         (CCSEC(1,3)+T*CCSEC3) * SN(3) + CCSEC(1,4)*SN(4)
	PERTLD = 0.0
	PERTR = 0.0
	PERTRD = 0.0
	DO 400, K = 1,15
	  A = MOD(DCARGS(1,K)+DT*DCARGS(2,K), DC2PI)
	  COSA = COS(A)
	  SINA = SIN(A)
	  PERTL = PERTL + CCAMPS(1,K)*COSA+CCAMPS(2,K)*SINA
	  PERTR = PERTR + CCAMPS(3,K)*COSA+CCAMPS(4,K)*SINA
	  IF (K .LE. 10)  THEN
	    PERTLD = PERTLD+(CCAMPS(2,K)*COSA-CCAMPS(1,K)*SINA)*
     &		CCAMPS(5,K)
	    PERTRD = PERTRD+(CCAMPS(4,K)*COSA-CCAMPS(3,K)*SINA)*
     &		CCAMPS(5,K)
	  END IF
400 	CONTINUE

*     ELLIPTIC PART OF THE MOTION OF THE EMB

	ESQ = E*E
	DPARAM = DC1-ESQ
	PARAM = DPARAM
	TWOE = E+E
	TWOG = G+G
	PHI = TWOE*((1.0-ESQ*0.125) * SIN(G) + E * 0.625 * SIN(TWOG)
     &         + ESQ * 0.5416667 * SIN(G+TWOG))
	F = G+PHI
	SINF = SIN(F)
	COSF = COS(F)
	DPSI = DPARAM/(DC1+E*COSF)
	PHID = TWOE*CCSGD*((1.0+ESQ*1.5)*COSF+E*(1.25-SINF*SINF*0.5))
	PSID = CCSGD*E*SINF/SQRT(PARAM)

*     PERTURBED HELIOCENTIRC MOTION OF THE EMB.

	D1PDRO = (DC1+PERTR)
	DRD    = D1PDRO*(PSID+DPSI*PERTRD)
	DRLD   = D1PDRO*DPSI*(DCSLD+PHID+PERTLD)
	DTL    = MOD(DML+PHI+PERTL, DC2PI)
	DSINLS = SIN(DTL)
	DCOSLS = COS(DTL)
	DXHD   = DRD*DCOSLS-DRLD*DSINLS
	DYHD   = DRD*DSINLS+DRLD*DCOSLS

*    INFLUENCE OF ECCENTRICITY, EVECTION AND VARIATION ON THE
*	GEOCENTRIC MOTION OF THE MOON

	PERTL  = 0.0
	PERTLD = 0.0
	PERTP  = 0.0
	PERTPD = 0.0
	DO 500, K = 1,3
	  A = MOD(DCARGM(1,K)+DT*DCARGM(2,K), DC2PI)
	  SINA = SIN(A)
	  COSA = COS(A)
	  PERTL = PERTL +CCAMPM(1,K)*SINA
	  PERTLD = PERTLD+CCAMPM(2,K)*COSA
	  PERTP = PERTP +CCAMPM(3,K)*COSA
	  PERTPD = PERTPD-CCAMPM(4,K)*SINA
500	CONTINUE

*    HELIOCENTRIC MOTION OF THE EARTH

	TL    = FORBEL(2)+PERTL
	SINLM = SIN(TL)
	COSLM = COS(TL)
	SIGMA = CCKM/(1.0+PERTP)
	A     = SIGMA*(CCMLD+PERTLD)
	B     = SIGMA*PERTPD
	DXHD  = DXHD+A*SINLM+B*COSLM
	DYHD  = DYHD-A*COSLM+B*SINLM
	DZHD  = -SIGMA*CCFDI*COS(FORBEL(3))

*    BARYCENTRIC MOTION OF THE EARTH

	DXBD = DXHD*DC1MME
	DYBD = DYHD*DC1MME
	DZBD = DZHD*DC1MME
	DO 600, K = 1,4
	  PLON = FORBEL(K+3)
	  POMG = SORBEL(K+1)
	  PECC = SORBEL(K+9)
	  TL   = MOD(PLON+2.0*PECC*SIN(PLON-POMG), CC2PI)
	  SINLP(K) = SIN(TL)
	  COSLP(K) = COS(TL)
	  DXBD = DXBD+CCPAMV(K)*(SINLP(K)+PECC*SIN(POMG))
	  DYBD = DYBD-CCPAMV(K)*(COSLP(K)+PECC*COS(POMG))
	  DZBD = DZBD-CCPAMV(K)*SORBEL(K+13)*COS(PLON-SORBEL(K+5))
600	CONTINUE

*    TRANSITION TO MEAN EQUATOR OF DATE

	DCOSEP = COS(DEPS)
	DSINEP = SIN(DEPS)
	DYAHD = DCOSEP*DYHD-DSINEP*DZHD
	DZAHD = DSINEP*DYHD+DCOSEP*DZHD
	DYABD = DCOSEP*DYBD-DSINEP*DZBD
	DZABD = DSINEP*DYBD+DCOSEP*DZBD

	IF(IDEQ .EQ. 0) THEN
		DVELH(1) = DXHD
		DVELH(2) = DYAHD
		DVELH(3) = DZAHD
		DVELB(1) = DXBD
		DVELB(2) = DYABD
		DVELB(3) = DZABD
	ELSE

*    GENERAL PRECESSION FROM EPOCH DJE TO DEQ

		DEQDAT = (DJE-DCT0-DCBES)/DCTROP + DC1900
		CALL BARY_BARPRE (DEQDAT,DEQ,DPREMA)
		DO 710, N = 1,3
		    DVELH(N) = DXHD*DPREMA(N,1)+DYAHD*DPREMA(N,2)+
     &			DZAHD*DPREMA(N,3)
		    DVELB(N) = DXBD*DPREMA(N,1)+DYABD*DPREMA(N,2)+
     &			DZABD*DPREMA(N,3)
710		CONTINUE
	END IF
	END


*+ BARY_CORR - Does the barycentric corrections according to user spec
      SUBROUTINE BARY_CORR(INTIMES,OUTTIMES,WIDTHS,NEVENT,RA,DEC,
     :    BINNED,LUN_POS,
     :    POS_FILE_OK,SATCORR,IGNORE_POS,AX_WID_OK,BASE_MJD,
     :    BASE_UTC,BASE_TAI,UPDATE_PERIOD,EQUINOX,STATUS)
*    Description :
*     <description of what the subroutine does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
c      STRUCTURE /POS_REC/
c         INTEGER UT				! hk clock (1/2s) time
c         REAL SATGEO(3)		! Sat vector in geo frame
c         INTEGER*2 IXRT(3)		! RA, dec, roll, arcmin
c         INTEGER*2 IBGLONG                      ! long and lat
c         INTEGER*2 IBGLAT                       !
c      END STRUCTURE
c      STRUCTURE /POS_REC/
c         INTEGER UT				! hk clock (1/2s) time
c         REAL SATGEO(3)		! Sat vector in geo frame
c         INTEGER*2 IXRT(3)		! RA, dec, roll, arcmin
c         INTEGER*2 IBGLONG                      ! long and lat
c         INTEGER*2 IBGLAT                       !
c      END STRUCTURE
*
*    Local constants :
*

c      RECORD /POS_REC/ POS
c      RECORD /POS_REC/ POS
      REAL BARY                         ! Barycentric  correction in Secs
      DOUBLE PRECISION ROSAT_MATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

        INTEGER UT
        REAL SATGEO(3)           ! GEOGRAPHIC XYZ
        REAL SAT_GEOCENTRIC(3)   ! GEOCENTRIC XYZ
        INTEGER*2 IXRT(3)        ! Restored XRT pointing RA DEC Roll (arcmin)
        INTEGER*2 ILONG
        INTEGER*2 ILAT
        INTEGER*2 B_EM8T
        INTEGER*2 LVAL_MILLERAD
        INTEGER KEY              ! Key for indexed read
        INTEGER LUN_POS          ! Log Unit for ATT_POS_SAT output (rosat specific)
        DOUBLE PRECISION BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*     <specification of FORTRAN structures>
*    Import :
      INTEGER              NEVENT              ! No events in list
      INTEGER              BASE_MJD              ! whole part MJD only
*      INTEGER              LUN_POS               ! LOG unit for ATT_POS_SAT

      REAL                 UPDATE_PERIOD         ! desired update times
      REAL                 RA,DEC                ! RA DEC FOR  BARY CORRECTIONS

      DOUBLE PRECISION     BASE_UTC              ! secs offset in base_mjd
      DOUBLE PRECISION     BASE_TAI              ! continuous time (in days) from Jan72
      DOUBLE PRECISION     EQUINOX               ! equinox of ra dec system

      LOGICAL              SATCORR               ! Correct for Satellite ?
      LOGICAL              POS_FILE_OK           ! ATT_POS_SAT data fine
      LOGICAL              IGNORE_POS            ! pos_sat info to be ignored
      LOGICAL              BINNED                ! AXIS is binned
      LOGICAL              AX_WID_OK             ! AXIS widths OK
*    Import-Export :
      DOUBLE PRECISION     INTIMES(NEVENT)     ! timelist
      DOUBLE PRECISI0N     OUTTIMES(NEVENT)    ! timelist
      DOUBLE PRECISION     WIDTHS(NEVENT)      ! timelist binwidths
*    Export :
*     <declarations and descriptions for exported arguments>

*  Status :

*  Status :
      INTEGER STATUS

*  External References:
      EXTERNAL			BARY_MJD2HK
        INTEGER 		BARY_MJD2HK
*  External References:
      EXTERNAL			BARY_MJD2HK
        INTEGER 		BARY_MJD2HK

*  Local constants :
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )

*  Local variables :
      DOUBLE PRECISION		LB, UB			! Lower/upper bounds
      DOUBLE PRECISION     	MJD_CURRENT           	! MJD for current event
      DOUBLE PRECISION     	MJD_START,MJD_END     	! MJD for start/stop

      REAL                 	TRIGGER               !current trigger time

      INTEGER              	I,J,K                 ! DO loop variables
      INTEGER              	CURRENT_KEY           ! for KEYed access to POS file
*  Local constants :
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )

*  Local variables :
      DOUBLE PRECISION		LB, UB			! Lower/upper bounds
      DOUBLE PRECISION     	MJD_CURRENT           	! MJD for current event
      DOUBLE PRECISION     	MJD_START,MJD_END     	! MJD for start/stop

      REAL                 	TRIGGER               !current trigger time

      INTEGER              	I,J,K                 ! DO loop variables
      INTEGER              	CURRENT_KEY           ! for KEYed access to POS file
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If binned and axis width present, then do not use trigger times as gaps
*  will appear in the time series due to differential barycentric corrections
*  during the observation
*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If binned and axis width present, then do not use trigger times as gaps
*  will appear in the time series due to differential barycentric corrections
*  during the observation
      IF(BINNED .AND. AX_WID_OK)THEN
*       BOUNDS(NEVENT+1) = INTIMES(NEVENT)+0.5*WIDTHS(NEVENT)
         LB = INTIMES(1) - WIDTHS(1) / 2
         LB = INTIMES(1) - WIDTHS(1) / 2
         LB = INTIMES(1) - WIDTHS(1) / 2
         CALL BARY_CORR_INT(LB,RA,DEC,LUN_POS,
     :   POS_FILE_OK,BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)
         LB = LB + BARY
         UB = INTIMES(1) + WIDTHS(1) / 2
         UB = INTIMES(1) + WIDTHS(1) / 2
         UB = INTIMES(1) + WIDTHS(1) / 2
         CALL BARY_CORR_INT(UB,RA,DEC,LUN_POS,
     : POS_FILE_OK,BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)
       UB = UB + BARY
       OUTTIMES(1)= 0.5 * (UB+LB)
       WIDTHS(1) = ABS(UB-LB)
       DO  I = 2,NEVENT
         LB = UB
         UB = INTIMES(I) + 0.5*WIDTHS(I)
         CALL BARY_CORR_INT(UB,RA,DEC,LUN_POS,POS_FILE_OK,
     :   BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)

         UB = UB + BARY
         OUTTIMES(I)= 0.5 * (UB+LB)
         WIDTHS(I) = ABS(UB-LB)

       ENDDO
      ELSE
*comput bary as often as desired. use att pos sat data
* initialise TRIGGER
         TRIGGER = INTIMES(1) - 1.1 * UPDATE_PERIOD
         DO I = 1, NEVENT
           IF(ABS(INTIMES(I)-TRIGGER) .GE. UPDATE_PERIOD)THEN

             TRIGGER = INTIMES(I)
             CALL BARY_CORR_INT(INTIMES(I),RA,DEC,LUN_POS,POS_FILE_OK,
     : BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)

           ENDIF
           OUTTIMES(I) = INTIMES(I) + BARY
         ENDDO
      ENDIF
      END


*+  BARY_CORR_INT - Does the barycentric corrections according to user spec
*+  BARY_CORR_INT - Does the barycentric corrections according to user spec
      SUBROUTINE BARY_CORR_INT(TIME,RA,DEC,LUN_POS,POS_FILE_OK,
     : BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)
*    Description :
*     <description of what the subroutine does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Structure definitions :
      STRUCTURE /POS_REC/
         INTEGER UT				! hk clock (1/2s) time
         REAL SATGEO(3)		! Sat vector in geo frame
         INTEGER*2 IXRT(3)		! RA, dec, roll, arcmin
         INTEGER*2 IBGLONG                      ! long and lat
         INTEGER*2 IBGLAT                       !
      END STRUCTURE

*
*    Local constants :
*
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )
*-

      RECORD /POS_REC/ POS

      REAL BARY                         ! Barycentric  correction in Secs
      DOUBLE PRECISION RMATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

        INTEGER UT
        REAL SATGEO(3)           ! GEOGRAPHIC XYZ
        REAL SAT_GEOCENTRIC(3)   ! GEOCENTRIC XYZ
        INTEGER*2 IXRT(3)        ! Restored XRT pointing RA DEC Roll (arcmin)
        INTEGER*2 ILONG
        INTEGER*2 ILAT
        INTEGER KEY              ! Key for indexed read
        INTEGER LUN_POS          ! Log Unit for ATT_POS_SAT output (rosat specific)
        DOUBLE PRECISION BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*    Import :
      INTEGER              BASE_MJD              ! whole part MJD only

      DOUBLE PRECISION     RA,DEC                ! RA DEC FOR  BARY CORRECTIONS

      DOUBLE PRECISION     BASE_UTC              ! secs offset in base_mjd
      DOUBLE PRECISION     BASE_TAI              ! continuous time (in days) from Jan72
      DOUBLE PRECISION     EQUINOX               ! equinox of ra dec system

      LOGICAL              POS_FILE_OK           ! ATT_POS_SAT data fine
      LOGICAL              IGNORE_POS            ! pos_sat info to be ignored
*    Import-Export :
      REAL                 TIME     ! timeTAG
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER BARY_MJD2HK
*    Local variables :
      DOUBLE PRECISION     MJD_CURRENT           ! MJD for current event
      DOUBLE PRECISION     MJD_START,MJD_END     ! MJD for start/stop

      REAL                 TRIGGER               !current trigger time

      INTEGER              I,J,K                 ! DO loop variables
      INTEGER              CURRENT_KEY           ! for KEYed access to POS file
*-

*if binned and axis width present, then do not use trigger times as gaps will
* appear in the time series due to differential barycentric correections
* during thee observation

*  Compute appropriate MJD or KEY
      MJD_CURRENT = DBLE(BASE_MJD) + (BASE_UTC + TIME)/8.64D04

*  Got orbit position file? Load ROSAT position matrix
      IF( POS_FILE_OK)THEN
          CURRENT_KEY = BARY_MJD2HK(MJD_CURRENT)
* read ATT POS SAT file
*          READ(LUN_POS,KEYGE=CURRENT_KEY)POS
*read ATT POS SAT record into local variables
          UT = POS.UT
          DO J = 1,3
            SATGEO(J) = REAL(POS.SATGEO(J))
            IXRT(J) =   INT(POS.IXRT(J))
          ENDDO

*          MJD_CURRENT = BARY_HK2MJD(UT)
        CALL BARY_GEO2GEI(MJD_CURRENT,SATGEO,SAT_GEOCENTRIC)
        DO J = 1,3
          RMATRIX(J)=DBLE(SAT_GEOCENTRIC(J))
          RMATRIX(J+3)=0.0D00
        END DO

*  Fake ROSAT matrix
      ELSE
        DO J = 1, 6
          RMATRIX(J) = 0.0D00
        END DO

      END IF

*  Get BARY time
      CALL BARY_ROSAT( MJD_CURRENT, RA, DEC, BARY, RMATRIX, EQUINOX )

      END


*+  BARY_SIMPLE - Does the simple, one off, barycentric correction
      SUBROUTINE BARY_SIMPLE( MJDOBS, BASE_TAI, RA, DEC,
     :                        EQUINOX, ULTRA_SIMPLE, TMAX, TMIN,
     :                        NU_BASE_TAI, NU_MJDOBS, STATUS )
*
*    Description :
*     <description of what the subroutine does - for user info>
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (institution::username)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*-
* IMPORTS
      DOUBLE PRECISION S2_REF_MJD
      PARAMETER(S2_REF_MJD=47892.0D0)
      REAL BARY                         ! Barycentric  correction in Secs
      DOUBLE PRECISION RMATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

*    Import :
      DOUBLE PRECISION		MJDOBS, TMAX, TMIN

      INTEGER              BASE_MJD              ! whole part MJD only

      REAL                 RA,DEC                ! RA DEC FOR  BARY CORRECTIONS
      DOUBLE PRECISION     BASE_UTC              ! secs offset in base_mjd
      DOUBLE PRECISION     BASE_TAI              ! continuous time (in days) from Jan72
      DOUBLE PRECISION     EQUINOX               ! equinox of ra dec system

      LOGICAL              ULTRA_SIMPLE          ! TMAX=TMIN=0
*    Export :
*     <declarations and descriptions for exported arguments>
      DOUBLE PRECISION NU_BASE_TAI,NU_MJDOBS
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
      DOUBLE PRECISION SLA_DAT
*    Local constants :
      DOUBLE PRECISION        LEAPS_AT_1970
      PARAMETER              (LEAPS_AT_1970 = 10.0D0)
      DOUBLE PRECISION        MJD_AT_1970
      PARAMETER              (MJD_AT_1970 = 41317.0D0)
      DOUBLE PRECISION        SECONDS_IN_DAY
      PARAMETER              (SECONDS_IN_DAY = 86400.0D0)

*    Local variables :
      DOUBLE PRECISION     MJD_CURRENT           ! MJD for current event
      DOUBLE PRECISION MJDOBS_OLD,MJDOBS_NEW
      DOUBLE PRECISION TRIAL_TIME1, TRIAL_TIME2  ! trial solutions. TAI->MJD
      DOUBLE PRECISION TEST_TAI

      REAL BARY_MID,BARY_START,BARY_END          ! various Barycentric corr times
      REAL DIFFER                                ! differential bary corr
      REAL DIFF_T                                ! diff between trial and answer
      INTEGER              I,J                 ! DO loop variables
      INTEGER LS_BEF,LS_AFT                      ! leapsecs before and after
      INTEGER UPPER_LIMIT                        ! search bounds for solution

      LOGICAL			DSPRT			! Desparate measures?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load dummy rosat matrix
      DO J = 1,6
        RMATRIX(J) = 0.0D00
      END DO

*  Base the correct only on the start time of the observation?
      IF ( ULTRA_SIMPLE ) THEN
        MJD_CURRENT = MJDOBS

        CALL BARY_ROSAT( MJD_CURRENT, RA, DEC, BARY, RMATRIX, EQUINOX )
        CALL MSG_SETR('BARY', BARY)
        CALL MSG_PRNT('A single correction of ^BARY seconds ')
        CALL MSG_PRNT('has been applied to the header structure')
        CALL MSG_PRNT(' ')

        NU_BASE_TAI = BASE_TAI + DBLE(BARY)/SECONDS_IN_DAY
        BARY_MID = BARY

*  Otherwise some half way point
      ELSE

        MJD_CURRENT = MJDOBS + 0.5D0*(TMAX+TMIN) / SECONDS_IN_DAY

        CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,RMATRIX,EQUINOX)
        BARY_MID = BARY

*    As TAI is continous, it is impervious to leap second complications
        NU_BASE_TAI = BASE_TAI + DBLE(BARY)/SECONDS_IN_DAY
        CALL MSG_SETR('BARY', BARY)
        CALL MSG_PRNT('A single correction of ^BARY seconds ')
        CALL MSG_PRNT('has been applied to the header structure')

        MJD_CURRENT = MJDOBS + TMIN/SECONDS_IN_DAY
        CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,RMATRIX,EQUINOX)

        MJD_CURRENT = MJDOBS + TMAX/SECONDS_IN_DAY
        BARY_START = BARY
        CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,RMATRIX,EQUINOX)
        BARY_END = BARY

*    Announce differential correction
        DIFFER = ABS(BARY_START - BARY_END)
        CALL MSG_SETR( 'DIFFER', DIFFER )
        CALL MSG_PRNT( 'There exists (an uncorrected) differential '/
     :                 /'barycentric shift of ^DIFFER seconds '/
     :                 /'across your dataset' )

      END IF

*  And a guess of the new MJD, complicated by the existence of leapseconds
      MJDOBS_NEW = MJDOBS + DBLE(BARY_MID)/SECONDS_IN_DAY

*  Number of leap seconds before and after change in barycentre
      DSPRT = .FALSE.
      LS_BEF = INT(SLA_DAT(MJDOBS))
      LS_AFT = INT(SLA_DAT(MJDOBS_NEW))

*  No leapseconds took part during the geocentric-barycentric shift?
      IF ( (LS_BEF-LS_AFT) .EQ. 0 ) THEN

        CALL TCI_MJD2TAI(MJDOBS_NEW,TEST_TAI)
        DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
        IF ( DIFF_T .LT. 0.5 ) THEN
          NU_MJDOBS = MJDOBS_NEW
        ELSE

*     AARGH! code shouldn't come here! if it has, then only an error of
*     1 second is possible
          TRIAL_TIME1 = MJDOBS_NEW - 1.0D0/SECONDS_IN_DAY
          TRIAL_TIME2 = MJDOBS_NEW + 1.0D0/SECONDS_IN_DAY

          CALL TCI_MJD2TAI(TRIAL_TIME1,TEST_TAI)
          DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
          IF(DIFF_T .LT. 0.5)THEN
            NU_MJDOBS = INT(TRIAL_TIME1) + (MJDOBS_NEW -
     :              DBLE(INT(TRIAL_TIME1)) )
            GOTO 99
          END IF

* if it is not trial time 2, then i cannot explain what has happened
          CALL TCI_MJD2TAI(TRIAL_TIME2,TEST_TAI)
          DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
          IF(DIFF_T .LT. 0.5)THEN
            NU_MJDOBS = INT(TRIAL_TIME2) + (MJDOBS_NEW -
     :             DBLE(INT(TRIAL_TIME2)) )
          ELSE
* we have had no success in getting selfconsistency. We will jump into a brute
* force technique for searching for the correct solution
            DSPRT = .TRUE.
          END IF
        END IF
      ELSE
        DSPRT = .TRUE.
      END IF

* Apologies for the ugly code here, but this is emergency stuff!
      IF ( DSPRT ) THEN

        UPPER_LIMIT = 10+IABS(LS_BEF-LS_AFT) + 10
        DO I = -UPPER_LIMIT,UPPER_LIMIT,1
          TRIAL_TIME1 = MJDOBS_NEW + DBLE(I)/SECONDS_IN_DAY
          CALL TCI_MJD2TAI(TRIAL_TIME1,TEST_TAI)
          DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
          IF ( DIFF_T .LT. 0.5 ) THEN
            NU_MJDOBS = INT(TRIAL_TIME1) + (MJDOBS_NEW -
     :             DBLE(INT(TRIAL_TIME1)) )
            GOTO 99
          END IF
        END DO

*    If we are at this point in the code, then something horrible has gone wrong
        CALL MSG_PRNT('    HELP! ')
        CALL MSG_SETI('BARY',BARY_MID)
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'The correction is ^BARY seconds. '/
     :        /'BARYCORR has screwed up and NOT updated your file. '/
     :        /'Please contact refer to the authors', STATUS )

      END IF

*  Abort point
 99   CONTINUE

      END



*+ BARY_ROSAT performs  barycentric corrections.
	SUBROUTINE BARY_ROSAT(MJD,XRA,XDEC,BARY,GKROS,EQUINOX)
        IMPLICIT NONE

	DOUBLE PRECISION MJD,EQUINOX,XRA,XDEC
	REAL  BARY
*MJD	input	Modified Julian Date (IAU definition) of observation.
*XRA	input	Source Right Ascension, epoch 1950.0, degrees.
*XDEC	input	Source Declination, epoch 1950.0, degrees.
*BARY	output	Barycentric correction to be ADDED to observed time, secs.
*ISTAT	output	Status code, 0=successful, non-zero values mean errors in
*		accessing orbit data.
*Special requirement: a ROSAT orbit data file valid for range of MJDs in
*use must have been opened before any call to this routine.
*-
*Author		Clive Page	1985 FEB 1.
*mods for ROSAT Doug Bertram    1990 JUN 1.

	DOUBLE PRECISION GKROS(6), VELH(3), VELB(3), CORH(3),
     &   BAEAR(3), BKROS(3), VX(3), SUM
        INTEGER	I

      INCLUDE 'MATH_PAR'

*CLIGHT is velocity of light in km/sec
*AUKM  converts from Astronomical Units to kilometres.

      DOUBLE PRECISION		CLIGHT, AUKM
	PARAMETER (CLIGHT = 2.99792458E5, AUKM = 1.495979D8)

*  Then get Barycentric coords in AU of EARth in BAEAR
      CALL BARY_BARVEL( MJD, EQUINOX, VELH, VELB )
      CALL BARY_BARCOR( CORH, BAEAR )

*  Convert these from AU to km, and add to geocentric coords of Rosat to
*  get barycentric coords of Rosat in km in BKROS
      DO I = 1,3
	BKROS(I) = GKROS(I) + BAEAR(I) * AUKM
      END DO

*  Find unit vector in direction of the source, take dot product with
*  barycentric vector of ROSAT and divide by speed of light to get time
*  difference.
      CALL CONV_DONA2V( XRA*MATH__DDTOR, XDEC*MATH__DDTOR, VX )
      SUM = 0.0D0
      DO I = 1,3
	SUM = SUM + VX(I) * BKROS(I)
      END DO
      BARY = SUM / CLIGHT

      END


*+  BARY_HK2MJD - Convert to HK UT 1/2 seconds to MJD
      DOUBLE PRECISION FUNCTION BARY_HK2MJD( HKT )
*
*    History :
*
*     ?? Nov 89 : Original (LTVAD::MD)
*     25 Apr 90 : Modified (RAL::MJR)
*
*    Type declarations :
*
      IMPLICIT NONE
*
*    Local constants :
*
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )
      DOUBLE PRECISION		SECS2
	PARAMETER               ( SECS2 = 86400.D0*2.D0 )
*
*    Import :
*
      INTEGER			HKT			!
*-

      BARY_HK2MJD = S2_REF_MJD + DBLE(HKT)/SECS2

      END


*+  BARY_MJD2HK - Convert MJD to HK UT 1/2 seconds
	INTEGER	FUNCTION BARY_MJD2HK (MJD)
* Parameters
        DOUBLE PRECISION S2_REF_MJD
        PARAMETER(S2_REF_MJD=47892.0D0)
* Input
	DOUBLE PRECISION		MJD
* M. Denby Nov 89	Modified MJR 25/4/90
*-
* Local
*
	DOUBLE PRECISION		SECS2
	PARAMETER      (SECS2 = 86400.D0*2.D0)
*
	BARY_MJD2HK = INT((MJD - S2_REF_MJD)*SECS2)

	END

*+  BARY_GEO2GEI - Converts vector from Geographic frame to GEI
      SUBROUTINE BARY_GEO2GEI(DMJD,VGEO,VGEI)

*  Type Declarations
      IMPLICIT NONE

*  Calling Arguments
*     Input
      DOUBLE PRECISION DMJD		! Date, MJD
      REAL VGEO(3)		! Vector, Geog.

*     Output
      REAL VGEI(3)		! Vector, GEI


*  History
*     1st version	M Ricketts	1987 july, based on GEI2GEO
*     1989 May		::		Corrected error, GEI, GEO reversed in
*     					call to BARY_ROTEZ
*-

*  Local Variables
      REAL ROGTOC

*  Get angle between GEI and geographic frames
      CALL BARY_MJDGMST(DMJD,ROGTOC)

*  Rotate GEI vector to Geographic
      CALL BARY_ROTEZ(VGEO,ROGTOC,VGEI)

      END


*+  BARY_ROTEZ - Rotates vector about Z-axis by given angle
      SUBROUTINE BARY_ROTEZ(VA,PHI,VB)
      IMPLICIT NONE

*  Calling Arguments
      REAL VA(3)	! In	Vector
      REAL PHI		!	Rotation angle, radian
      REAL VB(3)	!	Rotated vector, may be same as input vector
*  Author	M J Ricketts	RAL	Oct 85
*-

*  Local Variables
      REAL CPHI,SPHI,VB1

*  Executable Code
      CPHI = COS(PHI)
      SPHI = SIN(PHI)
      VB1   = VA(1) * CPHI - VA(2) * SPHI
      VB(2) = VA(1) * SPHI + VA(2) * CPHI
      VB(1) = VB1
      VB(3) = VA(3)

      END


*+  BARY_MJDGMST - Computes Greenwich Mean Sidereal Time for any date 1950-'99
	SUBROUTINE BARY_MJDGMST(ZMJD,GMST)

	IMPLICIT DOUBLE PRECISION (Z)

*  Calling Arguments
	DOUBLE PRECISION ZMJD		! In	Modified Julian date
	REAL GMST			! Out	Greenwich Mean Sidereal Time
				!i.e. R.A. of Greenwich meridian, radians.
*-
*Author	Clive Page	1984-JUN-17. - from AX_ library

	ZDAYS = ZMJD - 33282D0
	ZINT  = AINT(ZDAYS)
	ZMST  = 1.7466477033819D0 + 1.72027914861503D-2 * ZINT +
     &		6.30038809866574D0 * (ZDAYS - ZINT)
	GMST  = MOD(ZMST,6.28318530717957D0)
	END
