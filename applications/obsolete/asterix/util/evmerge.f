*+  EVMERGE - event dataset merge
      SUBROUTINE EVMERGE( STATUS )
*
*    Description :
*
*     Merges a number of input event datasets and forms a new current
*     dataset.
*
*    Parameters :
*
*     INP1,INP2,INP3,INP4,...INP10 = UNIV(R)
*           Input datasets
*     OUT=UNIV(W)
*           Output dataset
*
*    Method :
*
*     Each of the user-supplied list of datasets is checked against the first
*     to see that its structure is similar.
*     The output dataset is formed using the total lengths of the lists
*     found in the input datasets.
*     Data are copied from each input dataset.
*     The 'header' data items are generally taken from the first dataset, but
*     in some cases like EXPOSURE_TIME, the output value is calculated from
*     all the input values.
*     Timing information is taken from the dataset with the earliest BASE_TAI.
*     Note that the instrument response component used is that supplied in
*     the first dataset.
*     LIVE_TIMEs are copied only if they are present in all datasets. They are
*     sorted in terms of increasing ON time, and duplicates removed.
*     If pointing orientations are different, the X_CORR and Y_CORR values of
*     the those differing datasets are shifted to the same frame as the first
*     dataset. FIELD_MIN and MAX are adjusted to account for these shifts.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*     Alan McFadzean (BHVAD::ADM)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 84 : Original (JCMP)
*     23 Oct 84 : Zero compensation enabled (JCMP)
*     20 Nov 84 : Handles case of single input dataset (JCMP)
*     21 Nov 84 : Live time handling, field size handling (JCMP)
*      5 Feb 85 : Max # of lists increased (JCMP)
*     16 May 85 : V0.3-1 merging of CORRECTIONS components (JCMP)
*     18 Jun 85 : V0.3-2 bug fix - don't rcopy CORRECTIONS (JCMP)
*     28 Jan 86 : V0.4-1 ADAM version (JCMP)
*     23 Sep 86 : V0.5-1 EXO_MOVBYT -> GEN_COPY (JCMP)
*      8 Oct 86 : V0.5-2 Input parameters changed (JCMP)
*     11 Mar 88 : V0.6-1 ROSAT version
*     13 Sep 88 : v1.0-1 ASTERIX88 version (ADM)
*     12 Oct 88 : Slight rewrite of code to improve efficiency
*      5 Jun 89 : Small modifications (ADM)
*      1 Aug 89 : V1.0-2 Bug in live time sort fixed (DJA)
*      8 Mar 90 : V1.2-0 Added check for non-existent units in input lists (DJA)
*     17 Feb 91 : V1.4-0 Allows merging of files with different pointing
*                        directions. LIVE_TIME duplicates removed. Lists may
*                        be in any order - extra lists are omitted (DJA)
**      6 Mar 91 : V1.4-1 Bug when 1st file not earliest fixed (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      INTEGER                CHR_LEN
      LOGICAL                EVMERGE_BADLIVE
*
*    Local Constants :
*
      INTEGER                MXINP                   ! maximum number of input datasets
         PARAMETER         ( MXINP = 10 )
      INTEGER                MXTEXT                  ! maximum line text
         PARAMETER         ( MXTEXT = MXINP*2 )
      INTEGER                LIST_MXNL               ! maximum number of lists in a dataset
         PARAMETER         ( LIST_MXNL = 30 )
*
*    Local variables :
*
      CHARACTER              C                       ! Input number
      CHARACTER*(DAT__SZLOC) HLOC(MXINP)             ! Locator to header info
      CHARACTER*(DAT__SZLOC) OHLOC                   ! Locator to output header info
      CHARACTER*(DAT__SZLOC) ILOC(MXINP)             ! Input dataset locators
      CHARACTER*(DAT__SZLOC) LIST(LIST_MXNL)         ! Output list locators
      CHARACTER*(DAT__SZLOC) LIVELOC(MXINP)          ! Input LIVE_TIME locators
      CHARACTER*(DAT__SZLOC) LLOC(LIST_MXNL,MXINP)   ! Input list locators
      CHARACTER*(DAT__SZLOC) LOC                     ! General locators
      CHARACTER*(DAT__SZNAM) NAME                    ! Object name
      CHARACTER*(DAT__SZLOC) OLIVELOC                ! Output LIVE_TIME locator
      CHARACTER*(DAT__SZLOC) OLOC                    ! Output dataset locator
      CHARACTER*(DAT__SZLOC) QLOC(LIST_MXNL)         ! Locator to vectorised QUANTUM
      CHARACTER*(DAT__SZNAM) LNAME(LIST_MXNL,MXINP)  ! Names of input lists (list,dataset)
      CHARACTER*(DAT__SZNAM) UNIT(LIST_MXNL,MXINP)   ! Units of input lists (list,dataset)
      CHARACTER*(DAT__SZNAM) PARNAM                  ! Parameter name
      CHARACTER*(DAT__SZTYP) TYPE                    ! Object type
      CHARACTER*20           BASE_DATE               ! Output base date
      CHARACTER*80           TEXTI(MXTEXT)           ! Input files
      CHARACTER*80           TEXT(22)                ! History text

      DOUBLE PRECISION       AXIS_DEC(MXINP)         ! Dec of field centre
      DOUBLE PRECISION       AXIS_RA(MXINP)          ! RA of field centre
      DOUBLE PRECISION       BASE_TAI(LIST_MXNL)     ! List zero values
      DOUBLE PRECISION       BASE_UTC                ! New UTC for output
      DOUBLE PRECISION       CTOS(3,3)               ! Conv to sky matrix
      DOUBLE PRECISION       FCTOS(3,3)              ! Conv to #1 matrix
      DOUBLE PRECISION       POSN_A(MXINP)           ! Position angle of datasets
      DOUBLE PRECISION       TOFFSET                 ! Offset to TAI

      REAL                   DMAX(LIST_MXNL)         ! Output list max's
      REAL                   DMIN(LIST_MXNL)         ! Output list mins
      REAL                   EXP_TIM                 ! Exposure time
      REAL                   FMAX(LIST_MXNL)         ! Output list field max
      REAL                   FMIN(LIST_MXNL)         ! Output list field min
      REAL                   FRADF(2)                ! Factors for 1st dataset
      REAL                   FX(4), FY(4)            ! Extrema in XCORR,YCORR
      REAL                   OBSL                    ! Observation length
      REAL                   OFFSET                  ! Value to add to list values when copying to output list
      REAL                   VALUE                   ! Current value
      REAL                   RADF(2)                 ! Radian conversion factor

      INTEGER                BASE_MJD                ! New MJD for output
      INTEGER                DURPTR                  ! Pointer to output LIVE_TIME DURATION
      INTEGER                IPTR                    ! Pointer to mapped input list
      INTEGER                ICOMP                   ! Loop over components
      INTEGER                IFILE                   ! Loop over datasets
      INTEGER                ILIST                   ! Loop over lists
      INTEGER                INLINES                 ! No. lines  input object
      INTEGER                INLIVE                  ! # input live time slots
      INTEGER                IONPTR, IOFFPTR, IDURPTR! Input live time data
      INTEGER                LEN                     ! Length of mapped data
      INTEGER                LLENGTH(MXINP)          ! Length of lists
      INTEGER                LPOS                    ! Index of a list in table
      INTEGER                NCHAR                   ! # characters
      INTEGER                NCOMP                   ! # components
      INTEGER                NIN                     ! Number of input datasets
      INTEGER                NLIST                   ! # lists in a dataset
      INTEGER                NLIST1                  ! # lists in 1st dataset
      INTEGER                NVLIST1                 ! # valid lists in 1st
      INTEGER                OFFPTR                  ! Pointer to LIVE_TIME OFF
      INTEGER                ONLIVE                  ! # output live time slots
      INTEGER                ONPTR                   ! Pointer to LIVE_TIME ON
      INTEGER                OPTR(LIST_MXNL)         ! Pointer to mapped output lists
      INTEGER                ONELM                   ! Length of output lists
      INTEGER                QDIMS                   ! Number of dimensions of QUANTUM
      INTEGER                QLEN(DAT__MXDIM)        ! Length of QUANTUM vector
      INTEGER                QOPTR                   ! Pointer to mapped output QUANTUM list
      INTEGER                QPTR(LIST_MXNL)         ! Pointer to mapped input QUANTUM list
      INTEGER                RTL                     ! RAW_TIMETAG list
      INTEGER                START                   ! Start position for copy
      INTEGER                TBASE                   ! File to use for BASE_TAI
      INTEGER                XCL, YCL                ! X_CORR,Y_CORR lists nos.

      LOGICAL                ANY_EMOVE               ! Any events to move?
      LOGICAL                BADLIVE                 ! Bad live times
      LOGICAL                DMAXOK(LIST_MXNL)       ! is DATA_MAX ok?
      LOGICAL                DMINOK(LIST_MXNL)       ! is DATA_MIN ok?
      LOGICAL                DUROK                   ! LIVE_TIME DURATION ok?
      LOGICAL                EMOVE(MXINP)            ! Move events XCORR,YCORR?
      LOGICAL                EXPOSOK                 ! EXPOSURE_TIM ok?
      LOGICAL                FXCLDEC                 ! First X_CORR decreasing?
      LOGICAL                INPRIM                  ! Is input primitive?
      LOGICAL                INPUT                   ! Used to control input loop
      LOGICAL                LCOPY(LIST_MXNL)        ! Copy these lists?
      LOGICAL                LIVEOK                  ! LIVE_TIME ON values set (assume OFF set too)
      LOGICAL                OBSOK                   ! OBS_LENGTH ok?
      LOGICAL                OK                      ! Data item acceptable?
      LOGICAL                QUANTUM(LIST_MXNL)      ! Is QUANTUM a vector?
      LOGICAL                XCLDEC                  ! #n X_CORR decreasing?
*
*    Version id :
*
      CHARACTER*22           VERSION
         PARAMETER         ( VERSION = 'EVMERGE Version 1.4-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )
      CALL AST_INIT( STATUS )

*    Get input datasets
      NIN = 1
      ANY_EMOVE = .FALSE.
      EXPOSOK = .TRUE.
      EXP_TIM = 0.0
      OBSOK = .TRUE.
      OBSL = 0.0
      LIVEOK = .TRUE.
      INPUT = .TRUE.
      ONELM = 0
      INLIVE = 0
      DUROK = .TRUE.
      DO WHILE ( INPUT )

*      Associate input object
        CALL CHR_ITOC( NIN, C, NCHAR )
        PARNAM = 'INP'//C(1:NCHAR)
        CALL USI_ASSOCI( PARNAM(:(3+NCHAR)), 'READ', ILOC(NIN),
     :                                         INPRIM, STATUS )
        OK = .TRUE.

*      Check status
        IF ( STATUS .EQ. PAR__ABORT  ) THEN
          INPUT =.FALSE.
        ELSE IF ( STATUS .EQ. PAR__NULL  ) THEN
          IF ( NIN .LT. 3 ) THEN
            CALL MSG_PRNT( 'Need at least 2 files to merge - use '/
     :                                             /'!! to abort' )
            OK = .FALSE.
          ELSE
            INPUT = .FALSE.
            CALL ERR_ANNUL( STATUS )
          END IF
        ELSE IF ( STATUS .NE. SAI__OK  ) THEN
          GOTO 99
        ELSE IF ( INPRIM ) THEN
          CALL MSG_PRNT( 'Input is not an event dataset - try again' )
          OK = .FALSE.
        END IF

*      Process file
        IF ( OK .AND. INPUT ) THEN

          EMOVE(NIN) = .FALSE.

*        First input?
          IF ( NIN .EQ. 1 ) THEN

*          Read all lists
            CALL LIST_FINDALLOK( ILOC(1), .FALSE., LLOC(1,1),
     :               LNAME(1,1), NLIST1, LLENGTH(1), STATUS )
            IF ( NLIST1 .LT. 1 ) THEN
              CALL MSG_PRNT( 'FATAL ERROR: No valid lists.' )
              STATUS = SAI__ERROR
              GOTO 99
            END IF
            NVLIST1 = NLIST1

*          Set all lists to be copied
            DO ILIST = 1, NLIST1
               LCOPY(ILIST) = .TRUE.
            END DO

*          Get list units
            DO ILIST = 1, NLIST1
              CALL HDX_OK( LLOC(ILIST,1), 'UNITS', OK, STATUS )
              IF ( OK ) THEN
                CALL CMP_GET0C( LLOC(ILIST,1), 'UNITS', UNIT(ILIST,1),
     :                                                        STATUS )
                IF ( STATUS .NE. SAI__OK ) THEN
                  UNIT(ILIST,1) = ' '
                  CALL ERR_FLUSH( STATUS )
                END IF
              ELSE
                UNIT(ILIST,1) = ' '
              END IF
            END DO

          END IF

*        Get timing and pointing data from header
          CALL BDA_LOCHEAD( ILOC(NIN), HLOC(NIN), STATUS )
          CALL CMP_GET0D( HLOC(NIN), 'BASE_TAI', BASE_TAI(NIN), STATUS )
          CALL CMP_GET0D( HLOC(NIN), 'AXIS_RA',  AXIS_RA(NIN), STATUS )
          CALL CMP_GET0D( HLOC(NIN), 'AXIS_DEC', AXIS_DEC(NIN), STATUS )
          CALL CMP_GET0D( HLOC(NIN), 'POSITION_ANGLE', POSN_A(NIN),
     :                                                     STATUS )

*        Second or subsequent input
          IF ( NIN .GT. 1 ) THEN

*          Check pointing direction
            IF ( ( AXIS_RA(NIN) .NE. AXIS_RA(1) ) .OR.
     :            ( AXIS_DEC(NIN) .NE. AXIS_DEC(1) ) ) THEN
              CALL MSG_SETI( 'N', NIN )
              CALL MSG_PRNT( 'Datasets 1 and ^N have different '/
     :          /'pointing directions - events will be shifted' )
              CALL MSG_PRNT( 'to the pointing direction of '/
     :                                         /'dataset 1' )
              EMOVE(NIN) = .TRUE.
            ELSE IF (POSN_A(NIN) .NE. POSN_A(1)) THEN
              CALL MSG_SETI( 'N', NIN )
              CALL MSG_PRNT( 'Datasets 1 and ^N have different '/
     :                       /'position angles wrt pointing dir'/
     :                                   /'ection. Events will' )
              CALL MSG_PRNT( 'be rotated to position angle of '/
     :                                            /'dataset 1' )
              EMOVE(NIN) = .TRUE.
            END IF
            ANY_EMOVE = ( ANY_EMOVE .OR. EMOVE(NIN) )

*          Check lists in file
            CALL DAT_NCOMP( ILOC(NIN), NCOMP, STATUS )
            NLIST = 0
            DO ICOMP = 1, NCOMP

*            Locate and type component
              CALL DAT_INDEX( ILOC(NIN), ICOMP, LOC, STATUS )
              CALL DAT_TYPE( LOC, TYPE, STATUS )

*            If a list...
              IF ( TYPE(1:4) .EQ. 'LIST' ) THEN

*              If list is present in the the set of the first dataset...
                CALL DAT_NAME( LOC, NAME, STATUS )
                CALL EVMERGE_LFIND( NAME, 1, LNAME, LPOS, STATUS )
                IF ( LPOS .GT. 0 ) THEN

*                A valid list. Copy into array
                  NLIST = NLIST + 1
                  CALL DAT_CLONE( LOC, LLOC(LPOS,NIN), STATUS )
                  LNAME(LPOS,NIN) = NAME

*                Get units and check against 1st dataset
                  CALL HDX_OK( LLOC(LPOS,1), 'UNITS', OK, STATUS )
                  IF ( OK ) THEN
                    CALL CMP_GET0C( LLOC(LPOS,NIN), 'UNITS',
     :                              UNIT(LPOS,NIN), STATUS )
                    IF ( STATUS .NE. SAI__OK ) THEN
                      UNIT(LPOS,NIN) = ' '
                      CALL ERR_FLUSH( STATUS )
                    END IF
                  ELSE
                    UNIT(LPOS,NIN) = ' '
                  END IF
                  IF ( UNIT(LPOS,NIN) .NE. UNIT(LPOS,1) ) THEN
                    CALL MSG_SETC( 'LN', NAME )
                    CALL MSG_PRNT( 'WARNING : Units of list ^LN do '/
     :                    /'not do agree with those in 1st dataset' )
                  END IF

*                Get length if first list
                  IF ( NLIST .EQ. 1 ) THEN
                    CALL CMP_SIZE( LLOC(LPOS,NIN), 'DATA_ARRAY',
     :                                    LLENGTH(NIN), STATUS )
                  END IF

                ELSE
                  CALL MSG_SETI( 'IF', NIN )
                  CALL MSG_SETC( 'LN', NAME )
                  CALL MSG_PRNT( 'List ^LN from dataset ^IF is not '/
     :              /'present in dataset 1, and will not be merged' )
                END IF

              END IF

*            Next component
              CALL DAT_ANNUL( LOC, STATUS )
              IF ( STATUS .NE. SAI__OK ) GOTO 99

            END DO

*          Check for no lists
            IF ( NLIST .EQ. 0 ) THEN

              CALL MSG_PRNT( 'No lists in this dataset!' )
              OK = .FALSE.

*          and lists in 1st input not present in this
            ELSE IF ( NLIST .NE. NVLIST1 ) THEN
              DO ILIST = 1, NLIST1
                IF ( LCOPY(ILIST) ) THEN
                  CALL EVMERGE_LFIND( LNAME(ILIST,1), NIN, LNAME,
     :                                             LPOS, STATUS )
                  IF ( LPOS .EQ. 0 ) THEN
                    CALL MSG_SETC( 'LN', LNAME(ILIST,1) )
                    CALL MSG_SETI( 'IF', NIN )
                    CALL MSG_PRNT( 'List ^LN is not present in'/
     :                 /' dataset ^IF and will not appear in output' )
                    LCOPY(ILIST) = .FALSE.
                    NVLIST1 = NVLIST1 - 1

*                  Free locators to the list which is being ditched
                    DO IFILE = 1, NIN - 1
                      CALL DAT_ANNUL( LLOC(ILIST,IFILE), STATUS )
                    END DO

                  END IF
                END IF
              END DO
            END IF

          END IF

*        Get components dependent on being in present in all inputs
          IF ( OK .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*          Exposure time
            IF ( EXPOSOK ) THEN
              CALL HDX_OK( HLOC(NIN), 'EXPOSURE_TIME', OK, STATUS)
              IF ( OK ) THEN
                CALL CMP_GET0R( HLOC(NIN), 'EXPOSURE_TIME', VALUE,
     :                                                    STATUS )
                EXP_TIM = EXP_TIM + VALUE
              ELSE
                EXPOSOK = .FALSE.
              END IF
            END IF

*          Observation length
            IF ( OBSOK ) THEN
              CALL HDX_OK( HLOC(NIN), 'OBS_LENGTH', OK, STATUS )
              IF ( OK ) THEN
                CALL CMP_GET0R( HLOC(NIN), 'OBS_LENGTH', VALUE, STATUS )
                OBSL = OBSL + VALUE
              ELSE
                OBSOK = .FALSE.
              END IF
            END IF

*          Live times structure
            CALL BDA_CHKLIVE( ILOC(NIN), LIVEOK, STATUS )
            IF ( LIVEOK ) THEN
              CALL BDA_LOCLIVE( ILOC(NIN), LIVELOC(NIN), STATUS )
              CALL HDX_OK( LIVELOC(NIN), 'ON', OK, STATUS )
              IF ( OK ) THEN
                CALL CMP_SIZE( LIVELOC(NIN), 'ON', LEN, STATUS )
                INLIVE = INLIVE + LEN
                CALL HDX_OK( LIVELOC(NIN), 'OFF', LIVEOK, STATUS )
                IF ( DUROK ) THEN
                  CALL HDX_OK( LIVELOC(NIN), 'DURATION', DUROK, STATUS )
                END IF
              ELSE
                LIVEOK = .FALSE.
              END IF
            END IF

          END IF

*        Bump up counter
          IF ( STATUS .EQ. SAI__OK ) THEN
            ONELM = ONELM + LLENGTH(NIN)
            NIN = NIN + 1
            IF ( NIN .GT. MXINP ) THEN
              CALL MSG_PRNT( 'No more inputs allowed!' )
              INPUT = .FALSE.
            END IF
          END IF

        END IF

      END DO

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NIN = NIN - 1

*    Create output dataset
      CALL DAT_CREAT( 'OUT', 'EVDS', 0, 0, STATUS )
      CALL USI_ASSOCO( 'OUT', 'EVDS', OLOC, STATUS )
      CALL USI_NAMEI( INLINES, TEXTI, STATUS )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy first input dataset to output
*    This will copy HEADER and LIVE_TIME info which will need modifying later
      CALL HDX_COPY( ILOC(1), OLOC, STATUS )
      CALL BDA_LOCHEAD( OLOC, OHLOC, STATUS )

*    Extend all the valid lists
      DO ILIST = 1, NLIST1
        IF ( LCOPY(ILIST) ) THEN

*        Locate output list
          CALL DAT_FIND( OLOC, LNAME(ILIST,1), LIST(ILIST), STATUS )

*        Change length and map
          CALL EVMERGE_ALTER( LIST(ILIST), 'DATA_ARRAY', ONELM, STATUS )
          CALL BDA_MAPDATA( LIST(ILIST), 'UPDATE', OPTR(ILIST), STATUS )

*        Get DATA_MIN
          CALL HDX_OK( LIST(ILIST), 'DATA_MIN', DMINOK(ILIST), STATUS )
          IF ( DMINOK(ILIST) ) THEN
            CALL CMP_GET0R( LIST(ILIST), 'DATA_MIN', DMIN(ILIST),
     :                                                   STATUS )
          ELSE
            DMIN(ILIST) = 1.0E+37
          END IF

*        Get DATA_MAX
          CALL HDX_OK( LIST(ILIST), 'DATA_MAX', DMAXOK(ILIST), STATUS )
          IF ( DMAXOK(ILIST) ) THEN
            CALL CMP_GET0R( LIST(ILIST), 'DATA_MAX', DMAX(ILIST),
     :                                                   STATUS )
          ELSE
            DMAX(ILIST) = 1.0E-37
          END IF

*        and FIELD limits
          CALL CMP_GET0R( LIST(ILIST), 'FIELD_MAX', FMAX(ILIST),
     :                                                  STATUS )
          CALL CMP_GET0R( LIST(ILIST), 'FIELD_MIN', FMIN(ILIST),
     :                                                  STATUS )

          CALL HDX_OK( LIST(ILIST), 'QUANTUM', OK, STATUS )
          QUANTUM(ILIST) = .FALSE.

          IF ( OK ) THEN
            CALL CMP_SHAPE( LIST(ILIST), 'QUANTUM', DAT__MXDIM, QDIMS,
     :                                                  QLEN, STATUS )
            IF ( QDIMS .EQ. 1 ) THEN
              CALL DAT_FIND( LIST(ILIST), 'QUANTUM', QLOC, STATUS )
              CALL DAT_MOULD( QLOC, 1, ONELM, STATUS )
              CALL DAT_MAPV( QLOC, '_REAL', 'UPDATE', QPTR(ILIST),
     :                                                    STATUS )
              QUANTUM(ILIST) = .TRUE.
            END IF
          END IF

        ELSE
           CALL DAT_ERASE( OLOC, LNAME(ILIST,1), STATUS )
        END IF
      END DO

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    If any of the 2..NIN datasets require event moving...
      IF ( ANY_EMOVE ) THEN

*       Locate X_CORR and Y_CORR lists, and get radian conversion factors
         CALL EVMERGE_LFIND( 'X_CORR', 1, LNAME, XCL, STATUS )
         CALL EVMERGE_LFIND( 'Y_CORR', 1, LNAME, YCL, STATUS )
         IF ( ( XCL .EQ. 0 ) .OR. ( YCL .EQ. 0 ) ) THEN
            CALL MSG_PRNT( 'Input dataset 1 has no X_CORR or Y_CORR'/
     :                      /' lists - no event shifting performed' )
            ANY_EMOVE = .FALSE.
         ELSE IF ( .NOT. ( LCOPY(XCL) .AND. LCOPY(YCL) ) ) THEN
            CALL MSG_PRNT( 'X_CORR and Y_CORR are not present in '/
     :                /'all inputs - no event shifting performed' )
            ANY_EMOVE = .FALSE.
         ELSE
            CALL CONV_UNIT2R( UNIT(XCL,1), FRADF(1), STATUS )
            CALL CONV_UNIT2R( UNIT(YCL,1), FRADF(2), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_PRNT( 'Invalid X_CORR or Y_CORR units - '/
     :                                        /'assuming arcmin' )
               CALL ERR_ANNUL( STATUS )
            END IF

*          Is the X_CORR decreasing flag set?
            FXCLDEC = .FALSE.
            CALL HDX_OK( LLOC(XCL,1), 'DECREASING', OK, STATUS )
            IF ( OK ) THEN
               CALL CMP_GET0L( LLOC(XCL,1), 'DECREASING', FXCLDEC,
     :                                                    STATUS )
            END IF

*          Find coordinate transformation matrix for dataset 1
            CALL CONV_GENDMAT( AXIS_RA(1)*MATH__DTOR,
     :                         AXIS_DEC(1)*MATH__DTOR,
     :                         POSN_A(1)*MATH__DTOR, FCTOS, STATUS )

         END IF

*       Reset event move flags if any errors
         IF ( .NOT. ANY_EMOVE ) THEN
            DO IFILE = 1, NIN
               EMOVE(IFILE) = .FALSE.
            END DO
         END IF

      END IF

*    Write exposure time
      IF ( EXPOSOK ) THEN
         CALL CMP_PUT0R( OHLOC, 'EXPOSURE_TIME', EXP_TIM, STATUS )
      ELSE
         CALL DAT_ERASE( OHLOC, 'EXPOSURE_TIME', STATUS )
         STATUS = SAI__OK
      END IF

*    Write observation length
      IF ( OBSOK ) THEN
        CALL CMP_PUT0R( OHLOC, 'OBS_LENGTH', OBSL, STATUS )
      ELSE
        CALL DAT_ERASE( OHLOC, 'OBS_LENGTH', STATUS )
        STATUS = SAI__OK
      END IF

*    Have we a RAW_TIMETAG list to copy
      CALL EVMERGE_LFIND( 'RAW_TIMETAG', 1, LNAME, RTL, STATUS )
      IF ( ( RTL .GT. 0 ) .AND. ( LCOPY(RTL) ) ) THEN

*      Find dataset with earliest BASE_TAI
        TBASE = 1
        DO IFILE = 2, NIN
          IF ( BASE_TAI(IFILE) .LT. BASE_TAI(TBASE) ) TBASE = IFILE
        END DO

*      If it's not the first dataset, have to add the offset here
        IF ( TBASE .NE. 1 ) THEN

          CALL MSG_SETI( 'N', TBASE )
          CALL MSG_PRNT( 'Timing origin taken from dataset ^N' )

          OFFSET = REAL((BASE_TAI(1)-BASE_TAI(TBASE))*8.6D4 )
          CALL ARR_SUMS( '+', LLENGTH(1), %VAL(OPTR(RTL)), 0, 0, 1,
     :                   OFFSET, 0, 0, LLENGTH(1), %VAL(OPTR(RTL)),
     :                                               0, 0, STATUS )

*        Get other timing data
          CALL CMP_GET0I( HLOC(TBASE), 'BASE_MJD', BASE_MJD, STATUS )
          CALL CMP_GET0D( HLOC(TBASE), 'BASE_UTC', BASE_UTC, STATUS )
          CALL CMP_GET0C( HLOC(TBASE), 'BASE_DATE', BASE_DATE, STATUS )

*        Write the new values
          CALL CMP_PUT0D( OHLOC, 'BASE_TAI', BASE_TAI(TBASE), STATUS )
          CALL CMP_PUT0I( OHLOC, 'BASE_MJD', BASE_MJD, STATUS )
          CALL CMP_PUT0D( OHLOC, 'BASE_UTC', BASE_UTC, STATUS )
          CALL CMP_PUT0C( OHLOC, 'BASE_DATE', BASE_DATE, STATUS )

        END IF

      ELSE
        RTL = 0

      END IF

*    Copy data from input datasets applying offsets to X_CORR, Y_CORR, & RAW_TIMETAG lists
      START = 0
      DO IFILE = 2, NIN
        START = START + LLENGTH(IFILE-1)

        DO ILIST = 1, NLIST1

          IF ( LCOPY( ILIST ) ) THEN

*          Adjust RAW_TIMETAG list if present - assumes units are seconds
            IF ( ILIST .EQ. RTL ) THEN
              OFFSET = REAL((BASE_TAI(IFILE)-BASE_TAI(TBASE))*8.6D4 )
            ELSE
              OFFSET = 0.0
            END IF

            CALL BDA_MAPDATA( LLOC(ILIST,IFILE), 'READ', IPTR, STATUS )
            CALL EVMERGE_COPY( START, LLENGTH(IFILE),.TRUE., %VAL(IPTR),
     :                               OFFSET, %VAL(OPTR(ILIST)), STATUS )
            CALL BDA_UNMAPDATA( LLOC(ILIST,IFILE), STATUS )

            IF ( QUANTUM(ILIST) ) THEN
              CALL CMP_MAPV( LLOC(ILIST,IFILE), 'QUANTUM', '_REAL',
     :                                     'UPDATE', IPTR, STATUS )
              IF ( STATUS .EQ. SAI__OK ) THEN
                CALL EVMERGE_COPY( START, LLENGTH(IFILE), .FALSE.,
     :                    %VAL(IPTR), 0.0, %VAL(QPTR(ILIST)), STATUS )
                CALL CMP_UNMAP( LLOC(ILIST,IFILE), 'QUANTUM', STATUS )
              ELSE
                STATUS = SAI__OK
                CALL DAT_ERASE( LLOC(ILIST,IFILE), 'QUANTUM', STATUS )
                QUANTUM(ILIST) = .FALSE.
              END IF
            END IF

*          Adjust DATA_MIN if present and necessary
            CALL HDX_OK( LLOC(ILIST,IFILE), 'DATA_MIN', OK, STATUS )
            IF ( OK ) THEN
              CALL CMP_GET0R( LLOC(ILIST,IFILE), 'DATA_MIN', VALUE,
     :                                                     STATUS )
              DMIN(ILIST) = MIN( DMIN(ILIST), VALUE )
            END IF

*          same for DATA_MAX
            CALL HDX_OK( LLOC(ILIST,IFILE), 'DATA_MAX', OK, STATUS )
            IF ( OK ) THEN
              CALL CMP_GET0R( LLOC(ILIST,IFILE), 'DATA_MAX', VALUE,
     :                                                     STATUS )
              DMAX(ILIST) = MAX( DMAX(ILIST), VALUE )
            END IF

*          and FIELD_MIN
            CALL CMP_GET0R( LLOC(ILIST,IFILE), 'FIELD_MIN', VALUE,
     :                                                    STATUS )
            FMIN(ILIST) = MIN( FMIN(ILIST), VALUE )

*          and FIELD_MAX
            CALL CMP_GET0R( LLOC(ILIST,IFILE), 'FIELD_MAX', VALUE,
     :                                                    STATUS )
            FMAX(ILIST) = MAX( FMAX(ILIST), VALUE )

*          Check status
            IF ( STATUS .NE. SAI__OK ) GOTO 99

           END IF

         END DO

*       Moving events due to pointing mis-matches?
         IF ( EMOVE(IFILE) ) THEN

*          Find conversion factors
            CALL CONV_UNIT2R( UNIT(XCL,IFILE), RADF(1), STATUS )
            CALL CONV_UNIT2R( UNIT(YCL,IFILE), RADF(2), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_PRNT( 'Invalid X_CORR or Y_CORR units - '/
     :                                        /'assuming arcmin' )
               CALL ERR_ANNUL( STATUS )
            END IF

*          Is the X_CORR decreasing flag set?
            XCLDEC = .FALSE.
            CALL HDX_OK( LLOC(XCL,IFILE), 'DECREASING', OK, STATUS )
            IF ( OK ) THEN
               CALL CMP_GET0L( LLOC(XCL,IFILE), 'DECREASING', XCLDEC,
     :                                                       STATUS )
            END IF

*          Generate transform matrix for this dataset
            CALL CONV_GENDMAT( AXIS_RA(IFILE)*MATH__DTOR,
     :                         AXIS_DEC(IFILE)*MATH__DTOR,
     :                         POSN_A(IFILE)*MATH__DTOR, CTOS, STATUS )

*          Shift events to new centre
            CALL EVMERGE_SHIFT( XCLDEC, RADF, CTOS, FXCLDEC, FRADF,
     :               FCTOS, START, LLENGTH(IFILE), %VAL(OPTR(XCL)),
     :                                    %VAL(OPTR(YCL)), STATUS )

*          Create a list of 4 points, corresponding to the 4 corners
*          defined by FIELD_MIN and FIELD_MAX. Correct these for the
*          coordinate shift, and adjust FMIN and FMAX.
            FX(1) = FMIN(XCL)
            FY(1) = FMIN(YCL)
            FX(2) = FMIN(XCL)
            FY(2) = FMAX(YCL)
            FX(3) = FMAX(XCL)
            FY(3) = FMAX(YCL)
            FX(4) = FMAX(XCL)
            FY(4) = FMIN(YCL)
            CALL EVMERGE_SHIFT( XCLDEC, RADF, CTOS, FXCLDEC, FRADF,
     :                                FCTOS, 1, 4, FX, FY, STATUS )
            FMIN(XCL) = MIN( FMIN(XCL), FX(1), FX(2), FX(3), FX(4) )
            FMAX(XCL) = MAX( FMAX(XCL), FX(1), FX(2), FX(3), FX(4) )
            FMIN(YCL) = MIN( FMIN(YCL), FY(1), FY(2), FY(3), FY(4) )
            FMAX(YCL) = MAX( FMAX(YCL), FY(1), FY(2), FY(3), FY(4) )

         END IF

      END DO

*    Set up DATA_MIN, _MAX, FIELD_MIN, _MAX for output dataset
      DO ILIST = 1, NLIST1
        IF ( LCOPY(ILIST) ) THEN
          IF ( DMINOK(ILIST) ) THEN
C             CALL CMP_PUT0R( LIST(ILIST), 'DATA_MIN', DMIN(ILIST), STATUS )
          END IF
          IF ( DMAXOK(ILIST) ) THEN
C             CALL CMP_PUT0R( LIST(ILIST), 'DATA_MAX', DMAX(ILIST), STATUS )
          END IF
          CALL CMP_PUT0R( LIST(ILIST), 'FIELD_MIN', FMIN(ILIST),
     :                                                  STATUS )
          CALL CMP_PUT0R( LIST(ILIST), 'FIELD_MAX', FMAX(ILIST),
     :                                                  STATUS )
          CALL DAT_ANNUL( LIST(ILIST), STATUS )
          DO IFILE = 1, NIN
            CALL DAT_ANNUL( LLOC(ILIST,IFILE), STATUS )
          END DO
        END IF
      END DO

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Now, the Live time (assumes live time is specified in all or no datasets)
      IF ( LIVEOK ) THEN

*      Erase existing LIVE_TIME components
        CALL BDA_LOCLIVE( OLOC, OLIVELOC, STATUS )

        CALL DAT_ERASE( OLIVELOC, 'ON', STATUS )
        CALL DAT_NEW1R( OLIVELOC, 'ON', INLIVE, STATUS )
        CALL CMP_MAPV( OLIVELOC, 'ON',  '_DOUBLE', 'WRITE', ONPTR,
     :                                               LEN, STATUS )

        CALL DAT_ERASE( OLIVELOC, 'OFF', STATUS )
        CALL DAT_NEW1R( OLIVELOC, 'OFF', INLIVE, STATUS )
        CALL CMP_MAPV( OLIVELOC, 'OFF', '_DOUBLE', 'WRITE', OFFPTR,
     :                                                LEN, STATUS )

        IF ( DUROK )THEN
           CALL DAT_ERASE( OLIVELOC, 'DURATION', STATUS )
           CALL DAT_NEW1R( OLIVELOC, 'DURATION', INLIVE, STATUS )
           CALL CMP_MAPV( OLIVELOC, 'DURATION', '_DOUBLE', 'WRITE',
     :                                        DURPTR, LEN, STATUS )
        END IF

*      Check status
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Copy all live times into output
        START = 0
        BADLIVE = .FALSE.
        DO IFILE = 1, NIN

          CALL CMP_MAPV( LIVELOC(IFILE), 'ON', '_DOUBLE', 'READ',
     :                                      IONPTR, LEN, STATUS )
          CALL CMP_MAPV( LIVELOC(IFILE), 'OFF', '_DOUBLE', 'READ',
     :                                      IOFFPTR, LEN, STATUS )

*        Convert to absolute TAI if ON/OFF and DURATION units are consistent
          IF ( .NOT. BADLIVE ) TOFFSET = BASE_TAI(IFILE)
          IF ( DUROK ) THEN

            CALL CMP_MAPV( LIVELOC(IFILE), 'DURATION', '_DOUBLE',
     :                             'READ', IDURPTR, LEN, STATUS )

            IF ( IFILE .EQ. 1 ) THEN
              IF ( EVMERGE_BADLIVE( %VAL(IONPTR), %VAL(IOFFPTR),
     :                                      %VAL(IDURPTR) ) ) THEN
                BADLIVE = .TRUE.
                CALL MSG_PRNT( 'Live time ON and OFF units are'/
     :            /' inconsistent with DURATION values in dataset 1' )
                TOFFSET = 0.0D0
              END IF
            END IF
          END IF

          CALL EVMERGE_COPYD( START, LEN, .FALSE., %VAL(IONPTR),
     :                            TOFFSET, %VAL(ONPTR), STATUS )
          CALL EVMERGE_COPYD( START, LEN, .FALSE., %VAL(IOFFPTR),
     :                            TOFFSET, %VAL(OFFPTR), STATUS )

          CALL CMP_UNMAP( LIVELOC(IFILE), 'ON', STATUS )
          CALL CMP_UNMAP( LIVELOC(IFILE), 'OFF', STATUS )

          IF ( DUROK ) THEN
            CALL EVMERGE_COPYD( START, LEN, .FALSE., %VAL(IDURPTR), 0.0,
     :                                            %VAL(DURPTR), STATUS )
            CALL CMP_UNMAP( LIVELOC(IFILE), 'DURATION', STATUS )
          END IF
          START = START + LEN

*        Check status
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END DO

*      Sort live_time components into increasing order of the ON times
*      and subtract off the BASE_TAI of the output dataset
        IF ( BADLIVE ) THEN
          CALL EVMERGE_SORT( INLIVE, DUROK, %VAL(ONPTR), %VAL(OFFPTR),
     :                           %VAL(DURPTR), 0.0D0, ONLIVE, STATUS )
        ELSE
          CALL EVMERGE_SORT( INLIVE, DUROK, %VAL(ONPTR), %VAL(OFFPTR),
     :                 %VAL(DURPTR), BASE_TAI(TBASE), ONLIVE, STATUS )
        END IF

*      Unmap
        CALL CMP_UNMAP( OLIVELOC, 'ON', STATUS )
        CALL CMP_UNMAP( OLIVELOC, 'OFF', STATUS )
        IF ( DUROK ) CALL CMP_UNMAP( OLIVELOC, 'DURATION', STATUS )

*      If total number of distinct live time slots is less than the sum
*      of those of the inputs, truncate arrays
        IF ( ONLIVE .LT. INLIVE ) THEN
           CALL EVMERGE_ALTER( OLIVELOC, 'ON', ONLIVE, STATUS )
           CALL EVMERGE_ALTER( OLIVELOC, 'OFF', ONLIVE, STATUS )
           IF ( DUROK ) THEN
              CALL EVMERGE_ALTER( OLIVELOC, 'DURATION', ONLIVE, STATUS )
           END IF
        END IF

      ELSE
        CALL BDA_LOCLIVE( OLOC, OLIVELOC, STATUS )
        CALL DAT_PAREN( OLIVELOC, LOC, STATUS )
        CALL DAT_ERASE( LOC, 'LIVE_TIME', STATUS )
        CALL DAT_ANNUL( LOC, STATUS )
        STATUS = SAI__OK

      END IF

*    Set up history - add in names of datasets merged
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      CALL HIST_PTXT( OLOC, INLINES, TEXTI, STATUS )

*    Clean up
 99   CONTINUE

      CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVMERGE_COPY - Copy from one real vector to another with START & OFFSET.
      SUBROUTINE EVMERGE_COPY( START, LENGTH, USEOFFSET, IN, OFFSET,
     :                                                 OUT, STATUS )
*    Description :
*
*     IN is copied to OUT starting at START, with OFFSET added to the values
*
*    History :
*
*     12 Oct 88 : Original
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
*    Import :
*
      INTEGER                START                    ! Index value for start of copy
      INTEGER                LENGTH                   ! Length of IN array

      LOGICAL                USEOFFSET

      REAL                   IN(LENGTH)               ! Array to be copied
      REAL                   OFFSET                   ! Value to add to IN
*
*    Import-Export :
*
      REAL                   OUT(*)                   ! Array to be written
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      INTEGER                I                        ! Loop counter
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( USEOFFSET ) THEN
        DO I = 1, LENGTH
          OUT(START+I) = IN(I) + OFFSET
        END DO
      ELSE
        DO I = 1, LENGTH
          OUT(START+I) = IN(I)
        END DO
      END IF

      END



*+  EVMERGE_COPYD - Copy from one real vector to another with START & OFFSET.
      SUBROUTINE EVMERGE_COPYD( START, LENGTH, USEOFFSET, IN, OFFSET,
     :                                                  OUT, STATUS )
*    Description :
*
*     IN is copied to OUT starting at START, with OFFSET added to the values
*
*    History :
*
*     12 Oct 88 : Original
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
*    Import :
*
      INTEGER                START                    ! Index value for start of copy
      INTEGER                LENGTH                   ! Length of IN array

      LOGICAL                USEOFFSET

      DOUBLE PRECISION       IN(LENGTH)               ! Array to be copied
      DOUBLE PRECISION       OFFSET                   ! Value to add to IN
*
*    Import-Export :
*
      DOUBLE PRECISION       OUT(*)                   ! Array to be written
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      INTEGER                I                        ! Loop counter
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( USEOFFSET ) THEN
        DO I = 1, LENGTH
          OUT(START+I) = IN(I) + OFFSET
        END DO
      ELSE
        DO I = 1, LENGTH
          OUT(START+I) = IN(I)
        END DO
      END IF

      END



*+  EVMERGE_SHIFT - Move events from one field centre to another
      SUBROUTINE EVMERGE_SHIFT( IXDEC, IFAC, IMAT, OXDEC, OFAC, OMAT,
     :                                   START, NUM, XC, YC, STATUS )
*
*    Description :
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Feb 91 : Original
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
*    Import :
*
      REAL                          IFAC(2)         ! Input radian factors
      DOUBLE PRECISION              IMAT(3,3)       ! Input transform
      REAL                          OFAC(2)         ! Output radian factors
      DOUBLE PRECISION              OMAT(3,3)       ! Conversion transform
      INTEGER                       START           ! First event to do
      INTEGER                       NUM             ! # of events to do
      LOGICAL                       IXDEC, OXDEC    ! X axes decreasing?
*
*    Import-Export :
*
      REAL                          XC(*), YC(*)    ! Event positions
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      DOUBLE PRECISION              RA, DEC         ! Sky position

      INTEGER                       I               ! Loop over events
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Loop over events
         DO I = START, START+NUM-1

*          Get RA and DEC of event
            CALL CONV_XY2EQU( XC(I)*IFAC(1), YC(I)*IFAC(2), IXDEC, IMAT,
     :                                                 RA, DEC, STATUS )

*          Transform
            CALL CONV_EQU2XY( RA, DEC, OXDEC, OMAT, XC(I), YC(I),
     :                                                   STATUS )
            XC(I) = XC(I) / OFAC(1)
            YC(I) = YC(I) / OFAC(2)

         END DO

      END IF

      END



*+  EVMERGE_ALTER - Alter length of an objects named sub-component
      SUBROUTINE EVMERGE_ALTER( LOC, SUBCOMP, NEWLEN, STATUS )
*
*    Method  :
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Feb 91 : Original
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER        LOC*(DAT__SZLOC)       ! Parent object
      CHARACTER*(*)    SUBCOMP                ! Component name
      INTEGER          NEWLEN                 ! New length
*
*    Local variables :
*
      CHARACTER        SUBLOC*(DAT__SZLOC)    ! Sub-component locator
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Locate subcomponent
      CALL DAT_FIND( LOC, SUBCOMP, SUBLOC, STATUS )

*    Alter size
      CALL DAT_ALTER( SUBLOC, 1, NEWLEN, STATUS )

*    Free object
      CALL DAT_ANNUL( SUBLOC, STATUS )

      END



*+  EVMERGE_SORT - Sorts LIVE_TIME components using ON values
      SUBROUTINE EVMERGE_SORT( N, DUROK, ON, OFF, DUR, BASE_TAI,
     :                                           NDIFF, STATUS )
*
*    Method  :
*
*     After the sort, identical live time slots are removed.
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Feb 91 : Original
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER          N                      ! Number of data points
      LOGICAL          DUROK                  ! Duration OK?
      DOUBLE PRECISION BASE_TAI               ! New base time
*
*    Import/Export :
*
      DOUBLE PRECISION ON(N), OFF(N), DUR(N)  ! The data to sort
*
*    Export :
*
      INTEGER          NDIFF                  ! Number of different live time
*
*    Local variables :
*
      DOUBLE PRECISION SWAPON,SWAPOFF,SWAPDUR ! Temporary data values

      INTEGER          I,J,L,IR               !

      LOGICAL          SAME                   ! Slots the same?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      NDIFF = 1
      L = N/2+1
      IR = N
      IF ( N .EQ. 1 ) GOTO 50

*    Sort into ascending time order
 10   CONTINUE
         IF ( L .GT. 1 ) THEN
            L = L - 1
            SWAPON = ON(L)
            SWAPOFF = OFF(L)
            IF ( DUROK ) SWAPDUR = DUR(L)

         ELSE
            SWAPON = ON(IR)
            SWAPOFF = OFF(IR)
            ON(IR) = ON(1)
            OFF(IR) = OFF(1)
            IF ( DUROK ) THEN
               SWAPDUR = DUR(IR)
               DUR(IR) = DUR(1)
            END IF
            IR = IR - 1
            IF ( IR .EQ. 1 ) THEN
               ON(1) = SWAPON
               OFF(1) = SWAPOFF
               IF ( DUROK ) DUR(1) = SWAPDUR
               GOTO 30
            END IF
         END IF
         I = L
         J = L + L
 20      IF ( J .LE. IR ) THEN
            IF ( J .LT. IR ) THEN
               IF ( ON(J) .LT. ON(J+1) ) J = J + 1
            END IF
            IF ( SWAPON .LT. ON(J)) THEN
               ON(I) = ON(J)
               OFF(I) = OFF(J)
               IF ( DUROK ) DUR(I) = DUR(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            END IF
            GOTO 20
         END IF
         ON(I) = SWAPON
         OFF(I) = SWAPOFF
         IF ( DUROK ) DUR(I) = SWAPDUR
      GOTO 10

*    Remove identical slots
 30   NDIFF = 1
      DO I = 2, N

*       Compare with last slot
         IF ( DUROK ) THEN
            SAME = ( ON(I) .EQ. ON(NDIFF) ) .AND.
     :             ( OFF(I) .EQ. OFF(NDIFF) ) .AND.
     :             ( DUR(I) .EQ. DUR(NDIFF) )
         ELSE
            SAME = ( ON(I) .EQ. ON(NDIFF) ) .AND.
     :                        ( OFF(I) .EQ. OFF(NDIFF) )
         END IF

*       If different store the slot
         IF ( .NOT. SAME ) THEN
            NDIFF = NDIFF + 1
            IF ( I .NE. NDIFF ) THEN
               ON(NDIFF) = ON(I)
               OFF(NDIFF) = OFF(I)
               IF ( DUROK ) DUR(NDIFF) = DUR(I)
            END IF
         END IF

      END DO

*    Adjust times to offsets from BASE_TAI
 50   DO I = 1, NDIFF
        ON(I) = ON(I) - BASE_TAI
        OFF(I) = OFF(I) - BASE_TAI
      END DO

      END



*+  EVMERGE_LFIND - Find the named list in the list table
      SUBROUTINE EVMERGE_LFIND( LIST, ROW, LTAB, INDEX, STATUS )
*
*    Method  :
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Feb 91 : Original
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
      INTEGER          LIST_MXNL              ! Max number of lists in a dataset
         PARAMETER     ( LIST_MXNL = 30 )
*
*    Import :
*
      CHARACTER*(*)    LIST                   ! The list to look for
      INTEGER          ROW                    ! Row to search
      CHARACTER*(*)    LTAB(LIST_MXNL,*)      ! List table
*
*    Export :
*
      INTEGER          INDEX                  ! Index of list name in table
*
*    Functions :
*
      INTEGER          CHR_LEN
*
*    Local variables :
*
      INTEGER          LLEN                   ! List length

      LOGICAL          FOUND                  ! Found a match yet?
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

         INDEX = 1
         FOUND = .FALSE.
         LLEN = CHR_LEN( LIST )
         DO WHILE ( ( INDEX .LE. LIST_MXNL ) .AND. .NOT. FOUND )
            IF ( LIST(:LLEN) .EQ. LTAB(INDEX,ROW)(:CHR_LEN
     :                             (LTAB(INDEX,ROW)))) THEN
               FOUND = .TRUE.
            ELSE
               INDEX = INDEX + 1
            END IF
         END DO
         IF ( .NOT. FOUND ) INDEX = 0
      END IF

      END



*+  EVMERGE_BADLIVE - Are live time units up the creek
      LOGICAL FUNCTION EVMERGE_BADLIVE( ON, OFF, DUR )
*    Description :
*
*     If the duration as derived by OFF-ON is within 1% of DUR corrected
*     to days, then the ON and OFF times are in days and therefore suspect.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      DOUBLE PRECISION         ON,OFF,DUR
*-

      EVMERGE_BADLIVE = ( ABS((OFF-ON)*86400.0D0/DUR-1.0) .LT. 0.01 )

      END
