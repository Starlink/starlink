*+  BARYCORR - Performs barycentric corrections to timetag
      SUBROUTINE BARYCORR ( STATUS )
*
*    Description :
*
*     in SIMPLE mode (hidden default = TRUE), then the mid-point of the
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
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*
*     get MJD of observation. Get orbital elements of satellite.
*     get RA DEC of field centre. Apply Barycentric correction to timetag
*            DO ABOVE AS OFTEN AS USER DEMANDS
*     write relevant components into HDS structure and HISTORY
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (Doug Bertram,BHVAD::DB)
*    History :
*
*     20-Jun-90 : Original (DB)
*     23-Nov-90 : V1.5-0 Upgrade (DB)
*     28 Mar 94 : V1.7-0 Ditto (DB)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
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
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )
*
*    Local variables :
*
*      RECORD /POS_REC/ 		POS

      DOUBLE PRECISION 		ROSAT_MATRIX(6)		!XYZ & DX/DT DY/DT, DZ/DT

      REAL 			BARY			! Barycentric correction
							! in seconds
      REAL 			SAT_GEOCENTRIC(3)   	! Geocentric XYZ
      REAL 			SATGEO(3)           	! Geographic XYZ

      INTEGER 			UT
*-
*         INTEGER*2 IXRT(3)        ! Restored XRT pointing RA DEC Roll (arcmin)
*        INTEGER*2 ILONG
*        INTEGER*2 ILAT
*        INTEGER*2 B_EM8T
*        INTEGER*2 LVAL_MILLERAD
*        INTEGER KEY              ! Key for indexed read
        INTEGER I
        INTEGER LUN_POS          ! Log Unit for ATT_POS_SAT output (rosat specific)
*        REAL*8 BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*        INTEGER BARY_MJD2HK ! integer function MJD to HK 1/2 secs
* END INCLUDE SECTION
*    Function declarations :
*      INTEGER BARY_MJD2HK ! integer function MJD to HK 1/2 secs
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*(DAT__SZLOC)   INLOC          ! locater to event dataset
      CHARACTER*(DAT__SZLOC)   OUTLOC         ! locater to event dataset
      CHARACTER*(DAT__SZLOC)   MORELOC        ! locater to .MORE
      CHARACTER*(DAT__SZLOC)   ASTLOC         ! loc to .MORE.ASTERIX
      CHARACTER*(DAT__SZLOC)   HEADLOC        ! loc to .MORE.ASTERIX.HEADER
      CHARACTER*(DAT__SZLOC)   INSTRLOC       ! loc to .MORE.ASTERIX.INSTRUMENT
      CHARACTER*(DAT__SZLOC)   PROCLOC        ! loc to .MORE.ASTERIX.PROCESSING
      CHARACTER*(DAT__SZLOC)   TIMLOC         ! loc to RAW_TIMETAG, TIME*.*
      CHARACTER*(DAT__SZTYP)   TYPE           ! for DAT_TYPE

      CHARACTER*40             OBSY           ! OBSERVATORY string
      CHARACTER*40             INSTRUMENT     ! instrument string
      CHARACTER*80             POS_FILE       ! Name of orbit file
      CHARACTER*80             HISTXT(10)     ! add to HISTORY
      CHARACTER*11             DATE           ! Date string

      DOUBLE PRECISION         MJD_START      !MJD start of observation
      DOUBLE PRECISION         MJD_STOP       !MJD stop of observation
      DOUBLE PRECISION         RA_CENTRE      !RA  of centre FOV
      DOUBLE PRECISION         DEC_CENTRE     !Dec of centre FOV
      DOUBLE PRECISION         BASE_UTC       !SECS since start of currentMJD
      DOUBLE PRECISION         BASE_TAI       !continous time (in days) from 1/1/72
      DOUBLE PRECISION         EQUINOX        ! equinox
      DOUBLE PRECISION         NU_BASE_UTC    ! equinox
      DOUBLE PRECISION         NU_BASE_TAI    ! equinox

      REAL                     RA,DEC         ! REAL RA nad Dec
      REAL                     OBS_LENGTH     !Obs Length of event dataset
      REAL                     UPDATE_PERIOD  ! update period for bary corr
      REAL                     TIME_MIN       !smallest `T' in raw_timetags
      REAL                     TIME_MAX       ! largest  "    "   "   "
      REAL                     DEF_UPD_PRD    ! 1% of max - min
      REAL                     BASE,SCALE,DS  ! Base & Scale of time axis


      INTEGER                  WIDTHPTR       !PTR to WIDTH time axis
      INTEGER                  TIMEPTR        !PTR to RAW_TIMETAG or TIME axis
      INTEGER                  ITIMEPTR       !PTR to input time events/axis
      INTEGER                  DUMMYPTR       !PTR to DUMMY
      INTEGER                  LDIM(DAT__MXDIM) ! input dimensions
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
      INTEGER                  N_EVENTS       ! No of events in list
      INTEGER                  FILE_STATUS    ! IOSTAT for ATT POS SAT open
      INTEGER                  FILE_START     !   "     "   "   "   " base key
      INTEGER                  FILE_END       !   "     "   "   "   " end key
      INTEGER                  BASE_MJD       ! MJD whole part only
      INTEGER                  NU_BASE_MJD    ! replacement for above (SIMPLE)
      INTEGER                  EQUI           ! equinox of RA DEC system
      INTEGER                  T_AXIS         ! index number of time axis
      INTEGER                  AXN            ! axis index

      LOGICAL                  OK,OK1,OK2     ! various checks
      LOGICAL                  PRIM           ! object primitive? (USI_ASSOC)
      LOGICAL                  VALID_ROSAT    ! file is a ROSAT file
      LOGICAL                  SATCORR        ! do satellite corrections ?
      LOGICAL                  POS_FILE_OK    ! ATT POS OPEN OK
      LOGICAL                  NOT_COVERED    ! ATT POS FILE doesn't cover..
      LOGICAL                  IGNORE_POS     ! if pos file not OK for all data
      LOGICAL                  PRESS_ON       ! press on regardless
      LOGICAL                  RAW_TIME_OK    ! raw timetags used
      LOGICAL                  TIME_OK        ! `TIMETAG'  list used.
      LOGICAL                  POK            ! PROCESSING THERE
      LOGICAL                  NOT_ROS_GO     ! not a rosat file ... but GO!!
      LOGICAL                  SIMPLE_MODE    ! Run BARYCORR in simple_mode
      LOGICAL                  ULTRA_SIMPLE   ! run BARYCORR in ultra_simple mode
      LOGICAL                  ABORT          ! abort barycorr
      LOGICAL                  AX_REG         ! is the axis regular
      LOGICAL                  BINNED         ! complex mode, data binned
      LOGICAL                  AX_WID_OK      ! AXIS widths are present
      character*80 text(10)
*
*    Local data :
*
      DATA UPDATE_PERIOD /0.0/
      DATA POS_FILE_OK /.FALSE./
      DATA IGNORE_POS /.FALSE./
      DATA TIME_OK  /.FALSE./
*
*    Version :
*
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'BARYCORR Version 1.8-0')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

* get input data
        CALL USI_ASSOCI('INP', 'READ', INLOC, PRIM, STATUS)
        CALL USI_SHOW(' Input data {INP}',status)
* see if file is a valid HDS file
        CALL BDA_CHKAST(INLOC, OK, STATUS)
        IF(.NOT.OK)THEN
         CALL MSG_PRNT(' Input file not a valid HDS file')
         GOTO 999
        ENDIF
        CALL DAT_TYPE(INLOC, TYPE, STATUS)
* create a copy
        CALL USI_ASSOCO('OUT', TYPE, OUTLOC, STATUS)

        CALL MSG_PRNT(' copying input to output ')

        CALL HDX_COPY(INLOC, OUTLOC, STATUS)

* get locators to MORE & ASTERIX etc
*find MORE -> ASTERIX -> HEADER
        CALL BDA_LOCAST(OUTLOC, ASTLOC, STATUS)
        CALL BDA_LOCHEAD(OUTLOC, HEADLOC, STATUS)
        CALL BDA_LOCINSTR(OUTLOC, INSTRLOC, STATUS)
* does the user want simple mode?

        CALL USI_DEF0L('SIMPLE_MODE', .TRUE. , STATUS)
        CALL USI_GET0L('SIMPLE_MODE', SIMPLE_MODE, STATUS)
        IF(SIMPLE_MODE)THEN
          SATCORR = .FALSE.
          POS_FILE_OK = .FALSE.
        ENDIF
        CALL DAT_THERE(ASTLOC, 'PROCESSING', POK, STATUS)

* obtaining the observatory information is not necessary in simple mode
        IF( .NOT. SIMPLE_MODE)THEN
*attempt to perform correction on event by even basis,
* Get OBSERVATORY information
         CALL DAT_THERE(HEADLOC, 'OBSERVATORY', OK, STATUS)
*         IF(OK)THEN
*           CALL CMP_GET0C(HEADLOC, 'OBSERVATORY', OBSY, STATUS)
*           CALL CMP_GET0C(HEADLOC, 'INSTRUMENT', INSTRUMENT, STATUS)
*           IF(OBSY(1:6) .EQ. 'ROSAT')THEN
*            VALID_ROSAT = .TRUE.
*           ELSEIF(INSTRUMENT(1:3) .EQ. 'WFC')THEN
*            VALID_ROSAT = .TRUE.
*           ELSEIF(INSTRUMENT(1:3) .EQ. 'HRI')THEN
*            VALID_ROSAT = .TRUE.
*           ELSEIF(INSTRUMENT(1:4) .EQ. 'PSPC')THEN
*            VALID_ROSAT = .TRUE.
*           ELSE
*            VALID_ROSAT = .FALSE.
*           ENDIF
*         ELSE
*          VALID_ROSAT = .FALSE.
*          CALL MSG_PRNT(' file is not a valid ROSAT file')
*          CALL MSG_PRNT(' No orbit data can be taken into account')
*          CALL MSG_PRNT(' Earth-centred Barycentric corrections will')
*          CALL MSG_PRNT(' be applied if all relevant info is present')
*         ENDIF


	 CALL DAT_THERE(OUTLOC, 'RAW_TIMETAG', OK, STATUS)
         RAW_TIME_OK = OK
         IF( .NOT. OK ) THEN
* no raw_timetag, so there must be a time axis (if the user is competent)
* find it and map it.
            CALL BDA_CHKDATA(OUTLOC, OK, NDIMS, LDIM, STATUS)
            IF (.NOT. OK)THEN
              CALL MSG_PRNT('RAW_TIMETAG absent & no Data present')
              CALL MSG_PRNT(' ** Rerun BARYCORR in SIMPLE mode')
              GOTO 999
            ENDIF
            BINNED = .TRUE.
            CALL BARY_FINDTIME(INLOC, OK, AX_REG, AXN, TIME_MAX,
     :                                 TIME_MIN, DUMMYPTR, N_EVENTS)
            CALL BDA_MAPAXVAL(INLOC, 'READ', AXN, ITIMEPTR, STATUS)
            IF(AX_REG)THEN
              CALL BDA_GETAXVAL(INLOC,AXN,BASE,SCALE,N_EVENTS,STATUS)
              DS = ABS(SCALE)
* going to use this as the width for the regular array mapped as irregular
              CALL BDA_CREAXVAL(OUTLOC, AXN, .FALSE., N_EVENTS, STATUS)
              CALL BDA_CREAXWID(OUTLOC, AXN, .FALSE., N_EVENTS, STATUS)
            ENDIF

            CALL BDA_CHKAXWID(OUTLOC,AXN,OK,OK1,I,STATUS)
            IF(I .GT. 0)THEN
              AX_WID_OK = .TRUE.
              CALL BDA_MAPAXVAL(OUTLOC, 'WRITE', AXN, TIMEPTR, STATUS)
              CALL BDA_MAPAXWID(OUTLOC, 'WRITE', AXN, WIDTHPTR, STATUS)
              IF(AX_REG)THEN
               CALL ARR_INIT1R(DS,N_EVENTS,%VAL(WIDTHPTR),STATUS)
               CALL ARR_REG1R(BASE,SCALE,N_EVENTS,%VAL(TIMEPTR),STATUS)
*               call nonsense(n_events,%val(timeptr))
              ENDIF
            ELSE
              AX_WID_OK = .FALSE.
              CALL BDA_MAPAXVAL(OUTLOC, 'WRITE', AXN, TIMEPTR, STATUS)
              CALL MSG_PRNT(' Time axis WIDTH array is missing')
              CALL MSG_PRNT(' BARYCORR applied only on data array')
            ENDIF
         ELSE
* map Timetag list
           BINNED = .FALSE.
           CALL LIST_MAPV(OUTLOC, 'RAW_TIMETAG', '_REAL',
     :     'UPDATE',TIMEPTR,N_EVENTS,STATUS)
           CALL DAT_FIND(OUTLOC, 'RAW_TIMETAG', TIMLOC, STATUS)
*trap status if timetag not ok
           IF(STATUS .NE. SAI__OK)THEN
             CALL ERR_FLUSH(STATUS)
             CALL MSG_PRNT(' FATAL error mapping RAW_TIMETAG list')
             GOTO 999
           ENDIF
         ENDIF

         CALL ARR_RANG1R(N_EVENTS,%VAL(TIMEPTR),TIME_MIN,
     :                                          TIME_MAX,STATUS)
         CALL MSG_SETI('EVENTS', N_EVENTS)
         IF(BINNED)THEN
           CALL MSG_PRNT('There are ^EVENTS bins in this file')
         ELSE
           CALL MSG_PRNT('There are ^EVENTS events in this file')
         ENDIF
*if SIMPLE_MODE is chosen, then the object of the exercise is to find the
*mid point (defined by (stop_time - start_time)/2) and to use that
*time to calculate the barycentric correction and apply it to the
*relevant components in the header.
*
*    in *NON_SIMPLE* mode, then ...
*if the object is not a valid ROSAT  file then we must try to get as
* much information as possible about the file, perhaps even with user
* input in the last resort.  MUST BE ASTERIX88 STANDARD FORMAT

* get required time data from VALID ROSAT event file
* check that RAW_TIMETAG is available
* MUST find a raw_timetag  or timetag list in either case
*
* Check that the data exists
        ELSE
*SIMPLE MODE JUMP POINT
* look for raw_timetag (event data set)
	 CALL DAT_THERE(OUTLOC, 'RAW_TIMETAG', OK, STATUS)
         RAW_TIME_OK = OK
         IF (.NOT. OK)THEN
*  binned dataset, find out dimensionality
           CALL BDA_CHKDATA(OUTLOC, OK, NDIMS, LDIM, STATUS)
           IF(.NOT. OK)THEN
             CALL MSG_PRNT(' No RAW_TIMETAG OR DATA present!')
             CALL MSG_PRNT(' if you continue, barycentric correction')
             CALL MSG_PRNT(' will occur on basis of header data only')
             CALL USI_DEF0L('ABORT', .FALSE. , STATUS)
             CALL USI_GET0L('ABORT', ABORT, STATUS)
             IF (ABORT)THEN
               CALL MSG_PRNT(' BARYCORR EXITING...')
               GOTO 999
             ELSE
               ULTRA_SIMPLE = .TRUE.
               TIME_MIN = 0
               N_EVENTS = 0
               TIME_MAX = 0
               GOTO 500
             ENDIF
           ENDIF
            CALL BARY_FINDTIME(INLOC, OK, AX_REG, AXN, TIME_MAX,
     :                                 TIME_MIN, DUMMYPTR, N_EVENTS)
* output will be a binned dataset, as the barycentric corrections will
* vary as a function of time, then the o/p file will not have regular
* spacing.

           IF(.NOT. OK)THEN
             CALL MSG_PRNT(' no RAW_TIMETAG & no time axis present!')
             CALL MSG_PRNT(' if you continue, barycentric correction')
             CALL MSG_PRNT(' will occur on basis of header data only')
             CALL USI_DEF0L('ABORT', .TRUE. , STATUS)
             CALL USI_GET0L('ABORT', ABORT, STATUS)
             IF (ABORT)THEN
               CALL MSG_PRNT(' BARYCORR EXITING...')
               GOTO 999
             ELSE
               ULTRA_SIMPLE = .TRUE.
             ENDIF
           ENDIF
         ELSE
*LIST and axis meeting point
* map Timetag list
          CALL LIST_MAPV(OUTLOC, 'RAW_TIMETAG', '_REAL',
     :    'UPDATE',TIMEPTR,N_EVENTS,STATUS)
          CALL DAT_FIND(OUTLOC, 'RAW_TIMETAG', TIMLOC, STATUS)
*trap status if timetag not ok
          IF(STATUS .NE. SAI__OK)THEN
            CALL ERR_FLUSH(STATUS)
            CALL MSG_PRNT(' FATAL error mapping RAW_TIMETAG list')
            GOTO 999
          ENDIF

          CALL ARR_RANG1R(N_EVENTS,%VAL(TIMEPTR),TIME_MIN,
     :                                   TIME_MAX,STATUS)
          CALL MSG_SETI('EVENTS', N_EVENTS)
          CALL MSG_PRNT('There are ^EVENTS events in this file')
         ENDIF
        ENDIF
*SIMPLE AND COMPLEX MEETING POINT.

500      CONTINUE
* if PROCESSING not there then create it.
        IF(.NOT. POK)THEN
          CALL MSG_PRNT(' Creating PROCESSING structure')
          CALL DAT_NEW(ASTLOC, 'PROCESSING', 'EXTENSION', 0, 0,
     :                                    STATUS)
        ENDIF

        CALL DAT_FIND(ASTLOC, 'PROCESSING', PROCLOC, STATUS)

        CALL DAT_THERE(PROCLOC, 'BARY_CORR_DONE', OK1, STATUS)
        IF(OK1)THEN
          CALL CMP_GET0L(PROCLOC, 'BARY_CORR_DONE', OK2, STATUS)
          IF(OK2)THEN
            CALL MSG_PRNT(' This file has already been corrected')
            CALL MSG_PRNT('     exiting....')
            GOTO 999
          ENDIF
        ENDIF
* obtain RA DEC of field centre
        CALL CMP_GET0D(HEADLOC, 'FIELD_RA', RA_CENTRE, STATUS)
        CALL CMP_GET0D(HEADLOC, 'FIELD_DEC', DEC_CENTRE, STATUS)
        CALL CMP_GET0I(HEADLOC, 'EQUINOX', EQUI, STATUS)
        EQUINOX = DBLE (EQUI)

        IF(STATUS .NE. SAI__OK)THEN
          CALL MSG_PRNT(' FATAL error obtaining RA and DEC of
     : field centre')
          GOTO 999
        ELSE
          RA = REAL(RA_CENTRE)
          DEC = REAL(DEC_CENTRE)
        ENDIF

* get Integer MJD number
        CALL CMP_GET0I(HEADLOC, 'BASE_MJD', BASE_MJD, STATUS)

* get TAI offsets in the day
        CALL CMP_GET0D(HEADLOC, 'BASE_TAI', BASE_TAI, STATUS)
        CALL CMP_GET0D(HEADLOC, 'BASE_UTC', BASE_UTC, STATUS)
        CALL CMP_GET0R(HEADLOC, 'OBS_LENGTH', OBS_LENGTH, STATUS)

        IF(STATUS .NE. SAI__OK)THEN
          CALL MSG_PRNT(' FATAL error obtaining Time offsets')
          GOTO 999
        ENDIF

*if ROSAT then -> get LUN for position file.  ATT_POS_SAT output
*        IF(VALID_ROSAT .AND.( .NOT. SIMPLE_MODE) )THEN
*
* Ask if orbit correction is required
*          CALL USI_GET0L('SATCORR', SATCORR, STATUS)
* The unix version of barycorr doesn't support the orbital file format
           SATCORR = .FALSE.
*
*          IF (STATUS .NE. SAI__OK) GOTO 999
*
*   Is satellite correction required ?
*          IF (SATCORR) THEN
*
*    Get the name of the orbit file
*             CALL USI_GET0C('POS_FILE', POS_FILE, STATUS)
*
*             IF (STATUS .NE. SAI__OK) GOTO 999
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
*                GOTO 999
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
*     :                       (BASE_UTC+DBLE(TIME_MIN))/(8.64D04)
*          MJD_STOP  = DBLE(BASE_MJD) +
*     :                       (BASE_UTC + DBLE(TIME_MAX))/8.64D04
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
*             IF (STATUS .NE. SAI__OK) GOTO 999
*
*             IF( .NOT. IGNORE_POS )THEN
*               CALL MSG_PRNT(' exiting ... ')
*                GOTO 999
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
*
* reset the desired update  period, if Earth centred corrections required
       IF(SIMPLE_MODE)THEN
       UPDATE_PERIOD = (TIME_MAX-TIME_MIN)/100.0
       ELSEIF (.NOT. SATCORR)THEN
         IF(BINNED .AND. AX_WID_OK)THEN
          UPDATE_PERIOD = 0.0
* update period not used in this mode
         ELSE
          DEF_UPD_PRD = (TIME_MAX - TIME_MIN)/100.0
          CALL USI_DEF0R('UPDATE_INT',DEF_UPD_PRD,STATUS)
          CALL USI_GET0R('UPDATE_INT', UPDATE_PERIOD, STATUS)
         ENDIF
       ENDIF
*
* Call main routine
      IF (SIMPLE_MODE)THEN
       IF(STATUS .NE. SAI__OK)THEN
         CALL ERR_FLUSH(STATUS)
         STATUS = SAI__OK
       ENDIF
       CALL BARY_SIMPLE(BASE_MJD, BASE_UTC, BASE_TAI, RA, DEC,
     : EQUINOX, ULTRA_SIMPLE, TIME_MAX, TIME_MIN,
     : NU_BASE_TAI, NU_BASE_MJD, NU_BASE_UTC,STATUS)

               IF(STATUS .NE. SAI__OK)GOTO 999

       IF(NU_BASE_MJD .NE. BASE_MJD)THEN
        MJD_START = DBLE(NU_BASE_MJD) + NU_BASE_UTC/86400.0D0
        CALL CONV_MJDDAT(MJD_START,DATE)
        CALL CMP_PUT0C(HEADLOC,'BASE_DATE',DATE,STATUS)
        CALL CMP_PUT0I(HEADLOC,'BASE_MJD',NU_BASE_MJD,STATUS)
       ENDIF
       CALL CMP_PUT0D(HEADLOC,'BASE_TAI', NU_BASE_TAI,STATUS)
       CALL CMP_PUT0D(HEADLOC,'BASE_UTC',NU_BASE_UTC,STATUS)
*update HISTORY
       HISTXT(1) =
     :     ' Barycorr was run in simple mode. '

       HISTXT(2) =
     :     ' single Barycentric correction applied to header components'
       N_LINES = 2
      ELSE
       IF (.NOT. BINNED)THEN
         AX_WID_OK = .FALSE.
         ITIMEPTR = TIMEPTR
         WIDTHPTR = TIMEPTR
       ENDIF
       CALL BARY_CORR(%VAL(ITIMEPTR),%VAL(TIMEPTR),%VAL(WIDTHPTR),
     :        N_EVENTS,RA,DEC,BINNED,
     :        LUN_POS,POS_FILE_OK,SATCORR,IGNORE_POS,AX_WID_OK,BASE_MJD,
     :        BASE_UTC,BASE_TAI,UPDATE_PERIOD,EQUINOX,STATUS)

* sub routine Barr_corr takes the appropriate action to the time list
*
* update HISTORY
       CALL HIST_ADD(OUTLOC, VERSION, STATUS)
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
          IF(UPDATE_PERIOD .LT. (TIME_MAX-TIME_MIN))THEN
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
          IF(UPDATE_PERIOD .LT. (TIME_MAX-TIME_MIN))THEN
            WRITE ( HISTXT(N_LINES), '(A,F9.1,A)')
     :  ' The corrections were updated every ',UPDATE_PERIOD, ' seconds'
          ELSE
            HISTXT(N_LINES) =
     :     ' single Barycentric correction applied to all timetags'
          ENDIF
        ENDIF
       ENDIF
      ENDIF
* update the history
      CALL HIST_PTXT(OUTLOC, N_LINES, HISTXT, STATUS)
* create new flag in file
      CALL DAT_NEW0L(PROCLOC, 'BARY_CORR_DONE' ,STATUS)
      CALL CMP_PUT0L(PROCLOC, 'BARY_CORR_DONE', .TRUE., STATUS)
999   CONTINUE

* tidy up and exit
      IF(RAW_TIME_OK)THEN
        CALL LIST_UNMAP(OUTLOC, 'RAW_TIMETAG', STATUS)
      ELSEIF(TIME_OK)THEN
        CALL LIST_UNMAP(OUTLOC, 'TIMETAG', STATUS)
      ENDIF
* Annul input locator
      CALL BDA_RELEASE(INLOC, STATUS)
      CALL DAT_ANNUL(INLOC, STATUS)
*               call nonsense(n_events,%val(timeptr))

*annul output locator
      CALL BDA_RELEASE(OUTLOC, STATUS)
*      CALL USI_ANNUL(OUTLOC,STATUS)
      CALL DAT_ANNUL(OUTLOC, STATUS)
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



*+    BARY_CORR   Does the barycentric corrections according to user spec
      SUBROUTINE BARY_CORR(INTIMES,OUTTIMES,WIDTHS,N_EVENTS,RA,DEC,
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
      STRUCTURE /POS_REC/
         INTEGER UT/0/				! hk clock (1/2s) time
         REAL SATGEO(3)/0.,0.,0./		! Sat vector in geo frame
         INTEGER*2 IXRT(3)/0,0,0/		! RA, dec, roll, arcmin
         INTEGER*2 IBGLONG                      ! long and lat
         INTEGER*2 IBGLAT                       !
         INTEGER*2 IBSIZ_EM8T                   ! B field. 10^{-8} Tesla
         INTEGER*2 LVAL_MILLERAD
      END STRUCTURE

*
*    Local constants :
*
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )
*-

      RECORD /POS_REC/ POS
      REAL BARY                         ! Barycentric  correction in Secs
      REAL*8 ROSAT_MATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

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
        REAL*8 BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*     <specification of FORTRAN structures>
*    Import :
      INTEGER              N_EVENTS              ! No events in list
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
*     <declarations and descriptions for imported arguments>
*    Import-Export :
      REAL                 INTIMES(N_EVENTS)     ! timelist
      REAL                 OUTTIMES(N_EVENTS)    ! timelist
      REAL                 WIDTHS(N_EVENTS)      ! timelist binwidths
      REAL                 LB,UB                 ! lower and upper bounds
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
      INTEGER BARY_MJD2HK
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      DOUBLE PRECISION     MJD_CURRENT           ! MJD for current event
      DOUBLE PRECISION     MJD_START,MJD_END     ! MJD for start/stop

      REAL                 TRIGGER               !current trigger time

      INTEGER              I,J,K                 ! DO loop variables
      INTEGER              CURRENT_KEY           ! for KEYed access to POS file
*     <declarations for local variables>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>
*if binned and axis width present, then do not use trigger times as gaps will
* appear in the time series due to differential barycentric correections
* during thee observation
      IF(BINNED .AND. AX_WID_OK)THEN
*       BOUNDS(N_EVENTS+1) = INTIMES(N_EVENTS)+0.5*WIDTHS(N_EVENTS)
         LB = INTIMES(1) - 0.5*WIDTHS(1)
         CALL BARY_CORR_INT(LB,RA,DEC,LUN_POS,
     :   POS_FILE_OK,BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)
         LB = LB + BARY
         UB = INTIMES(1) + 0.5*WIDTHS(1)
         CALL BARY_CORR_INT(UB,RA,DEC,LUN_POS,
     : POS_FILE_OK,BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)
       UB = UB + BARY
       OUTTIMES(1)= 0.5 * (UB+LB)
       WIDTHS(1) = ABS(UB-LB)
       DO  I = 2,N_EVENTS
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
         DO I = 1, N_EVENTS
           IF(ABS(INTIMES(I)-TRIGGER) .GE. UPDATE_PERIOD)THEN

             TRIGGER = INTIMES(I)
             CALL BARY_CORR_INT(INTIMES(I),RA,DEC,LUN_POS,POS_FILE_OK,
     : BASE_MJD,BASE_UTC,BASE_TAI,EQUINOX,BARY,STATUS)

           ENDIF
           OUTTIMES(I) = INTIMES(I) + BARY
         ENDDO
      ENDIF
      END
*+    BARY_CORR   Does the barycentric corrections according to user spec
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
         INTEGER UT/0/				! hk clock (1/2s) time
         REAL SATGEO(3)/0.,0.,0./		! Sat vector in geo frame
         INTEGER*2 IXRT(3)/0,0,0/		! RA, dec, roll, arcmin
         INTEGER*2 IBGLONG                      ! long and lat
         INTEGER*2 IBGLAT                       !
         INTEGER*2 IBSIZ_EM8T                   ! B field. 10^{-8} Tesla
         INTEGER*2 LVAL_MILLERAD
      END STRUCTURE

*
*    Local constants :
*
      DOUBLE PRECISION		S2_REF_MJD
        PARAMETER               ( S2_REF_MJD = 47892.0D0 )
*-

      RECORD /POS_REC/ POS

      REAL BARY                         ! Barycentric  correction in Secs
      REAL*8 ROSAT_MATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

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
        REAL*8 BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*     <specification of FORTRAN structures>
*    Import :
      INTEGER              BASE_MJD              ! whole part MJD only

      REAL                 RA,DEC                ! RA DEC FOR  BARY CORRECTIONS

      DOUBLE PRECISION     BASE_UTC              ! secs offset in base_mjd
      DOUBLE PRECISION     BASE_TAI              ! continuous time (in days) from Jan72
      DOUBLE PRECISION     EQUINOX               ! equinox of ra dec system

      LOGICAL              POS_FILE_OK           ! ATT_POS_SAT data fine
      LOGICAL              IGNORE_POS            ! pos_sat info to be ignored
*     <declarations and descriptions for imported arguments>
*    Import-Export :
      REAL                 TIME     ! timeTAG
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
      INTEGER BARY_MJD2HK
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      DOUBLE PRECISION     MJD_CURRENT           ! MJD for current event
      DOUBLE PRECISION     MJD_START,MJD_END     ! MJD for start/stop

      REAL                 TRIGGER               !current trigger time

      INTEGER              I,J,K                 ! DO loop variables
      INTEGER              CURRENT_KEY           ! for KEYed access to POS file
*     <declarations for local variables>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>
*if binned and axis width present, then do not use trigger times as gaps will
* appear in the time series due to differential barycentric correections
* during thee observation

* compute appropriate MJD or KEY
         MJD_CURRENT = DBLE(BASE_MJD) +
     :          (BASE_UTC + TIME)/8.64D04
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
            ROSAT_MATRIX(J)=DBLE(SAT_GEOCENTRIC(J))
            ROSAT_MATRIX(J+3)=0.0D00
          ENDDO
          CALL BARY_ROSAT(mjd_current,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)
         ELSE
          DO J = 1,6
           ROSAT_MATRIX(J) = 0.0D00
          END DO
* get BARY time
          CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)
         ENDIF
      END
*+  name - BARY_FINDTIME
      SUBROUTINE BARY_FINDTIME(LOC, OK, REG, AXN, MAX, MIN, PTR, N)
*    Description :
*     <description of what the subroutine does - for user info>
*         finds and maps a time axis
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (BHVAD::DB)
*    History :
*     date:  changes (BHVAD::DB) 02-Feb-1994     (Original)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*
      CHARACTER*(DAT__SZLOC)   LOC         ! locater to event dataset

*     <declarations and descriptions for imported arguments>
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
      LOGICAL                  OK             !  was a time axis found?
      LOGICAL                  REG            !  is the time axis regular?

      INTEGER                  PTR            ! ptr to axis
      INTEGER                  LDIM(DAT__MXDIM) ! input dimensions
      INTEGER                  AXN            ! axis number
      INTEGER                  N              ! Number of events
      REAL                     MIN,MAX        ! min/max of array

*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER                  NDIMS          ! number of dimensions
      INTEGER                  X_AXIS,Y_AXIS  ! dummy integers
      INTEGER                  T_AXIS         ! time axis
      INTEGER                  N_EVENTS       ! number of bins
*     <declarations for local variables>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>
        ok = .true.
        CALL BDA_CHKAST(LOC, OK, STATUS)

        CALL BDA_CHKDATA(LOC, OK, NDIMS, LDIM, STATUS)

        CALL AXIS_FIND(LOC, 'TIME',NDIMS,T_AXIS,STATUS)

        IF(T_AXIS.EQ.0)THEN
          OK = .FALSE.
          AXN = 0
        ELSEIF(T_AXIS.GT.0)THEN
          CALL BDA_CHKAXVAL(LOC, T_AXIS, OK, REG,
     :                                        N_EVENTS, STATUS)
        ELSEIF(T_AXIS.LT.0)THEN
          CALL AXIS_GET(LOC, 'TIME', 'WHICH_TIME', NDIMS,
     :                                        T_AXIS, STATUS)
          CALL BDA_CHKAXVAL(LOC, T_AXIS, OK, REG,
     :                                        N_EVENTS, STATUS)
        ENDIF
* Have now got a time axis, time to map it.

           IF(OK)THEN
* map time data, whether regular or irregular
             CALL BDA_MAPAXVAL(LOC, 'READ', T_AXIS, PTR, STATUS)
             CALL ARR_RANG1R(N_EVENTS, %VAL(PTR), MIN,
     :                                      MAX, STATUS)
           AXN = T_AXIS
           N = N_EVENTS
           ELSE
             AXN = -1
           ENDIF

      END


*+  BARY_SIMPLE - Does the simple, one off, barycentric correction
      SUBROUTINE BARY_SIMPLE(BASE_MJD, BASE_UTC, BASE_TAI, RA, DEC,
     : EQUINOX, ULTRA_SIMPLE, TIME_MAX, TIME_MIN,
     : NU_BASE_TAI, NU_BASE_MJD, NU_BASE_UTC,STATUS)
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
      REAL*8 S2_REF_MJD
      PARAMETER(S2_REF_MJD=47892.0D0)
      REAL BARY                         ! Barycentric  correction in Secs
      REAL*8 ROSAT_MATRIX(6)            !XYZ & DX/DT DY/DT, DZ/DT

        INTEGER UT
        REAL SATGEO(3)           ! GEOGRAPHIC XYZ
        REAL SAT_GEOCENTRIC(3)   ! GEOCENTRIC XYZ
        REAL*8 BARY_HK2MJD !DP FUNCTION HK SECS TO MJD
*     <specification of FORTRAN structures>
*    Import :
      INTEGER              BASE_MJD              ! whole part MJD only

      REAL                 RA,DEC                ! RA DEC FOR  BARY CORRECTIONS
      REAL                 TIME_MAX,TIME_MIN     ! min/max times offset from base
      DOUBLE PRECISION     BASE_UTC              ! secs offset in base_mjd
      DOUBLE PRECISION     BASE_TAI              ! continuous time (in days) from Jan72
      DOUBLE PRECISION     EQUINOX               ! equinox of ra dec system

      LOGICAL              ULTRA_SIMPLE          ! TIME_MAX=TIME_MIN=0
*     <declarations and descriptions for imported arguments>
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
      DOUBLE PRECISION NU_BASE_TAI
      DOUBLE PRECISION NU_BASE_UTC
      INTEGER NU_BASE_MJD
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
      DOUBLE PRECISION SLA_DAT
      INTEGER BARY_MJD2HK
*    Local constants :
      DOUBLE PRECISION        LEAPS_AT_1970
      PARAMETER              (LEAPS_AT_1970 = 10.0D0)
      DOUBLE PRECISION        MJD_AT_1970
      PARAMETER              (MJD_AT_1970 = 41317.0D0)
      DOUBLE PRECISION        SECONDS_IN_DAY
      PARAMETER              (SECONDS_IN_DAY = 86400.0D0)

*     <local constants defined by PARAMETER>
*    Local variables :
      DOUBLE PRECISION     MJD_CURRENT           ! MJD for current event
      DOUBLE PRECISION     MJD_START,MJD_END     ! MJD for start/stop
      DOUBLE PRECISION MJDOBS_OLD,MJDOBS_NEW
      DOUBLE PRECISION TRIAL_TIME1, TRIAL_TIME2  ! trial solutions. TAI->MJD
      DOUBLE PRECISION TEST_TAI

      REAL BARY_MID,BARY_START,BARY_END          ! various Barycentric corr times
      REAL DIFFER                                ! differential bary corr
      REAL DIFF_T                                ! diff between trial and answer
      INTEGER              I,J,K                 ! DO loop variables
      INTEGER LS_BEF,LS_AFT                      ! leapsecs before and after
      INTEGER UPPER_LIMIT                        ! search bounds for solution
*     <declarations for local variables>
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>
* load dummy rosat matrix



             DO J = 1,6
              ROSAT_MATRIX(J) = 0.0D00
             END DO


             IF (ULTRA_SIMPLE)THEN
              MJD_CURRENT = DBLE(BASE_MJD) + BASE_UTC/SECONDS_IN_DAY

              CALL BARY_ROSAT(mjd_current,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)
              CALL MSG_SETR('BARY', BARY)
              CALL MSG_PRNT('A single correction of ^BARY seconds ')
              CALL MSG_PRNT('has been applied to the header structure')
              CALL MSG_PRNT(' ')

              NU_BASE_TAI = BASE_TAI + DBLE(BARY)/SECONDS_IN_DAY
              BARY_MID = BARY
             ELSE
              MJD_CURRENT = DBLE(BASE_MJD) +
     :            (BASE_UTC+(DBLE(TIME_MAX)+
     :            (TIME_MIN))/2.0D0 )/SECONDS_IN_DAY
              CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)
              BARY_MID = BARY
* as TAI is continous, it is impervious to leap second complications
              NU_BASE_TAI = BASE_TAI + DBLE(BARY)/SECONDS_IN_DAY
              CALL MSG_SETR('BARY', BARY)
              CALL MSG_PRNT('A single correction of ^BARY seconds ')
              CALL MSG_PRNT('has been applied to the header structure')


              MJD_CURRENT = DBLE(BASE_MJD) +
     :            (BASE_UTC+DBLE(TIME_MIN))/SECONDS_IN_DAY
              CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)

              MJD_CURRENT = DBLE(BASE_MJD) +
     :           (BASE_UTC+DBLE(TIME_MAX))/SECONDS_IN_DAY
              BARY_START = BARY
              CALL BARY_ROSAT(MJD_CURRENT,RA,DEC,BARY,ROSAT_MATRIX,
     :                                                 EQUINOX)
              BARY_END = BARY
             ENDIF

            IF (.NOT. ULTRA_SIMPLE)THEN
              DIFFER = ABS(BARY_START - BARY_END)
           CALL MSG_PRNT('There exists (an uncorrected) differential')
           CALL MSG_SETR('DIFFER', DIFFER)
           CALL MSG_PRNT('barycentric shift of ^DIFFER seconds')
              CALL MSG_PRNT('across your dataset')

            ENDIF
*recalculate the new MJD UTC etc, complicated by the existence of leapseconds
       MJDOBS_OLD = DBLE(BASE_MJD) + BASE_UTC/SECONDS_IN_DAY
*and a guess of the new mjd
       MJDOBS_NEW = DBLE(BASE_MJD) +
     :          (BASE_UTC+DBLE(BARY_MID))/SECONDS_IN_DAY
       LS_BEF = INT(SLA_DAT(MJDOBS_OLD))
       LS_AFT = INT(SLA_DAT(MJDOBS_NEW))
       IF(LS_BEF-LS_AFT .EQ. 0)THEN
*no leapseconds took part during the geocentric-barycentric shift
         CALL  TIM_MJD2TAI(MJDOBS_NEW,TEST_TAI)
         DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
         IF(DIFF_T.LT.0.5)THEN
           NU_BASE_MJD = INT(MJDOBS_NEW)
           NU_BASE_UTC = SECONDS_IN_DAY *
     :                   (MJDOBS_NEW - DBLE(NU_BASE_MJD))
         ELSE
* AARGH! code shouldn't come here! if it has, then only an error of
* 1 second is possible
           TRIAL_TIME1 = MJDOBS_NEW - 1.0D0/SECONDS_IN_DAY
           TRIAL_TIME2 = MJDOBS_NEW + 1.0D0/SECONDS_IN_DAY

           CALL TIM_MJD2TAI(TRIAL_TIME1,TEST_TAI)
           DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
           IF(DIFF_T .LT. 0.5)THEN
             NU_BASE_MJD = INT(TRIAL_TIME1)
             NU_BASE_UTC = SECONDS_IN_DAY *
     :                   (MJDOBS_NEW - DBLE(NU_BASE_MJD))
             GOTO 100
           ENDIF
* if it is not trial time 2, then i cannot explain what has happened
           CALL TIM_MJD2TAI(TRIAL_TIME2,TEST_TAI)
           DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
           IF(DIFF_T .LT. 0.5)THEN
             NU_BASE_MJD = INT(TRIAL_TIME2)
             NU_BASE_UTC = SECONDS_IN_DAY *
     :                   (MJDOBS_NEW - DBLE(NU_BASE_MJD))
           ELSE
* we have had no success in getting selfconsistency. We will jump into a brute
* force technique for searching for the correct solution
             GOTO 500
           ENDIF
         ENDIF
       ELSE
* Apologies for the ugly code here, but this is emergency stuff!
500    CONTINUE
         UPPER_LIMIT = 10+IABS(LS_BEF-LS_AFT) + 10
         DO I = -UPPER_LIMIT,UPPER_LIMIT,1
          TRIAL_TIME1 = MJDOBS_NEW + DBLE(I)/SECONDS_IN_DAY
          CALL TIM_MJD2TAI(TRIAL_TIME1,TEST_TAI)
          DIFF_T = SECONDS_IN_DAY * ABS(TEST_TAI-NU_BASE_TAI)
          IF(DIFF_T .LT. 0.5)THEN
            NU_BASE_MJD = INT(TRIAL_TIME1)
            NU_BASE_UTC = SECONDS_IN_DAY *
     :                   (MJDOBS_NEW - DBLE(NU_BASE_MJD))
             GOTO 100
          ENDIF
         ENDDO
* If we are at this point in the code, then something horrible has gone wrong
         CALL MSG_PRNT('    HELP! ')
         CALL MSG_SETI('BARY',BARY_MID)
         CALL MSG_PRNT('The correction is ^BARY seconds')
         call MSG_PRNT('BARYCORR has screwed up and NOT updated')
         CALL MSG_PRNT('your file. Please contact BHVAD::DB ')
         CALL MSG_PRNT('aka db@xun4.sr.bham.ac.uk. Pronto!')
         STATUS = SAI__ERROR
       ENDIF
100    CONTINUE

       END



*+ BARY_ROSAT performs  barycentric corrections.
	SUBROUTINE BARY_ROSAT(MJD,XRA,XDEC,BARY,GKROS,EQUINOX)
	DOUBLE PRECISION MJD,EQUINOX
	REAL XRA, XDEC, BARY
	INTEGER ISTAT
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
     &   BAEAR(3), AUKM, BKROS(3), VX(3), SUM

*CLIGHT is velocity of light in km/sec
*AUKM  converts from Astronomical Units to kilometres.

	PARAMETER (PI = 3.1415926535, DTOR = PI/180.0,
     &   CLIGHT = 2.99792458E5, AUKM = 1.495979D8)

*Then get Barycentric coords in AU of EARth in BAEAR

	CALL BARY_BARVEL(MJD,EQUINOX,VELH,VELB)
	CALL BARY_BARCOR(CORH,BAEAR)

*Convert these from AU to km, and add to geocentric coords of Rosat to
*get barycentric coords of Rosat in km in BKROS

	DO I = 1,3
	     BKROS(I) = GKROS(I) + BAEAR(I) * AUKM
	END DO

*Find unit vector in direction of the source, take dot product with barycentric
*vector of ROSAT and divide by speed of light to get time difference.

	CALL CONV_DONA2V(DBLE(XRA*DTOR), DBLE(XDEC*DTOR), VX)
	SUM = 0.0D0
	DO I = 1,3
	   SUM = SUM + VX(I) * BKROS(I)
	END DO
	BARY = SUM / CLIGHT

90	CONTINUE
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
        REAL*8 S2_REF_MJD
        PARAMETER(S2_REF_MJD=47892.0D0)
* Input
	REAL*8		MJD
* M. Denby Nov 89	Modified MJR 25/4/90
*-
* Local
*
	REAL*8		SECS2
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
      REAL*8 DMJD		! Date, MJD
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
