*+  EVSUBSET - Subset an EVENT dataset
      SUBROUTINE EVSUBSET( STATUS )
*
*    Description :
*
*     Produces a subset of an EVENT dataset. Upto 10 of the LISTs in the
*     input evds may have ranges imposed.
*
*    Environment Parameters :
*
*     INP
*     OUT
*     RANGES1..7
*
*    Method :
*    Deficiencies :
*     Can only select ranges for 10 of the LISTs - this is to limit the
*     number of parameters required.
*    Bugs :
*    Authors :
*
*     Phil Andrews (BHVAD::PLA)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     25 May 89 : V1.0-0  Original. (PLA)
*     11 Jan 90 : V1.0-3  DATA_MIN and DATA_MAX references removed.
*                         Structure references put in include file (DJA)
*     21 Nov 90 : V1.3-0  No longer crashes if FIELD_MIN/MAX absent (DJA)
*      8 Jul 92 : V1.6-0  Much tidying (DJA)
*      9 Aug 93 : V1.7-0  Use PRM constant to write bad observation length (DJA)
*      1 Nov 93 : V1.7-1  INDEX mode added. Data transfer in D.P. (DJA)
*      2 Dec 93 : V1.7-2  Removed Fortran structures. Used byte array for
*                         event selection control. (DJA)
*     26 Jan 94 : V1.7-3  Minor bug fix to contiguous block handling. Allocate
*                         section table dynamically. (DJA)
*     28 Jan 94 : V1.7-4  Rejected contigous block algorithm in favour of
*                         simply copying in BLOCKSIZE lumps - much simpler
*                         and more efficient for sparse selection. (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'LIST_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER                 MXSEL                ! Maximum number of
        PARAMETER            (MXSEL = 10)          ! LISTs to select
      INTEGER                 MXBNDS               ! Maximum number of bounds
        PARAMETER            (MXBNDS = 1000)       ! => 500 ranges
      INTEGER                 MAXBLOCK		   ! max no. of blocks
        PARAMETER	     (MAXBLOCK=512)	   ! Gives
      INTEGER                 BLOCKSIZE            ! Maximum no. elements per
        PARAMETER            (BLOCKSIZE = 262144 ) ! section
*
*    Functions :
*
      INTEGER                 HDX_TYPINT
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)  CLOC                 ! Component locator
      CHARACTER*(DAT__SZLOC)  DLOC                 ! List DATA_ARRAY locator
      CHARACTER*(DAT__SZLOC)  HEADER               ! Locator to HEADER info
      CHARACTER*(DAT__SZLOC)  ILLOC (LIST__MXNL)   ! Locators to input LISTs
      CHARACTER*(DAT__SZLOC)  INPL                 ! Locator to input dataset
      CHARACTER*(DAT__SZLOC)  OLLOC                ! Locators to output LISTs
      CHARACTER*(DAT__SZLOC)  OUTL                 ! Locator to output dataset
      CHARACTER*(DAT__SZNAM)  NAME (LIST__MXNL)    ! Names of input
      CHARACTER*(DAT__SZNAM)  CNAME                ! Name of component
      CHARACTER*(DAT__SZTYP)  CTYPE                ! Type of component
      CHARACTER*(DAT__SZTYP)  MTYPE                ! Mapping type
      CHARACTER*(DAT__SZLOC)  ODLOC                ! Output DATA_ARRAY locator
      CHARACTER*(DAT__SZLOC)  OQLOC                ! Output QUANTUM locator
      CHARACTER*12            PAR                  ! Parameter name
      CHARACTER*(DAT__SZLOC)  QLOC                 ! List QUANTUM locator
      CHARACTER*(DAT__SZLOC)  SDLOC                ! DATA_ARRAY slice locator
      CHARACTER*(DAT__SZLOC)  SODLOC               ! o/p DATA_ARRAY slice
      CHARACTER*(DAT__SZLOC)  SOQLOC               ! p/p QUANTUM slice
      CHARACTER*(DAT__SZLOC)  SQLOC                ! QUANTUM slice locator
      CHARACTER*200           TXT                  ! Input text

      REAL                    FMIN(LIST__MXNL)     ! List lower bound
      REAL                    FMAX(LIST__MXNL)     ! List upper bound
      REAL                    INDEX(2*MXSEL)       ! LISTs to apply
                                                   ! ranges
      REAL                    OBS_LENGTH           ! Observation length
      REAL                    OLD_OBS_LENGTH       ! obs length of
                                                   ! original dataset
      REAL                    RANGES(MXBNDS,LIST__MXNL) ! Range boundaries
      REAL                    QUANTUM              ! Value of scalar quantum

      INTEGER                 ADPTR                ! A vector data array
      INTEGER                 AQPTR                ! A vector quantum array
      INTEGER                 BSTART      	   ! Block start index
      INTEGER                 BEND      	   ! Block end index
      INTEGER                 BLEN      	   ! Block length
      INTEGER		      CCOUNT(MAXBLOCK)	   ! Events to copy per block
      INTEGER                 COPY                 ! Pointer to copy array
      INTEGER                 DIMS                 ! Number of dimensions
      INTEGER                 EVENTS               ! Number of events in EVDS
      INTEGER                 I, J, K              ! Loop counters
      INTEGER                 IBLOCK               ! Loop over blocks
      INTEGER                 ICOPY                ! Pointer to copy array
      INTEGER                 IDPTR(LIST__MXNL)    ! List data pointers
      INTEGER                 IQPTR(LIST__MXNL)    ! Pointer to input quantum
      INTEGER                 LDIMS(DAT__MXDIM)    ! Length of each dimension
      INTEGER                 LEN                  ! Length of various things
      INTEGER                 NCOMP                ! Number of components
      INTEGER                 NLISTS               ! Number of LISTs
      INTEGER                 NRANGES(LIST__MXNL)  ! Number of ranges per list
      INTEGER                 NSEL                 ! Number of LISTs
                                                   ! with ranges applied
                                                   ! to them
      INTEGER                 ODPTR                ! Pointers to output
                                                   ! DATA_ARRAY
      INTEGER		      OFFSET		   ! Output data offset
      INTEGER                 OQPTR                ! Pointers to output
                                                   ! vector QUANTUM

      BYTE		      BVAL                 ! Array initialisation value

      LOGICAL                 INDEXMODE            ! Index mode on?
      LOGICAL                 INPRIM               ! Is input primitive?
      LOGICAL                 INT_TYPE             ! Is list of integer type
      LOGICAL                 KEEP(LIST__MXNL)     ! Keep selected ranges?
      LOGICAL                 OK		   ! General validity check
      LOGICAL                 QOK(LIST__MXNL)      ! Quantum ok?
      LOGICAL                 QVEC(LIST__MXNL)     ! Vector quantum?
      LOGICAL                 SELECTED(LIST__MXNL) ! List selected?
*
*    Version :
*
      CHARACTER*80            VERSION
        PARAMETER            ( VERSION = 'EVSUBSET Version 1.8-0' )
*-

*    Display version
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT()
      DO I = 1, LIST__MXNL
        QVEC(I) = .FALSE.
        SELECTED(I)  = .FALSE.
      END DO

*    Associate input & output datasets
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', INPL, OUTL, INPRIM, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

      IF ( INPRIM ) THEN
        CALL MSG_PRNT ('FATAL ERROR: Input must be an EVENT dataset')
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Index mode?
      CALL USI_GET0L( 'INDEX', INDEXMODE, STATUS )

*    Find all valid LISTs in the input, and display them
      IF ( .NOT. INDEXMODE ) THEN
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( 'The availible LISTs are:' )
      END IF
      CALL LIST_FINDALLOK( INPL, (.NOT. INDEXMODE), ILLOC, NAME, NLISTS,
     :                     EVENTS, STATUS )

*    Find out which events are to be copied. We use a BYTE here rather than
*    a LOGICAL to save dynamic memory.
      CALL DYN_MAPB( 1, EVENTS, COPY, STATUS )
      IF ( INDEXMODE ) THEN
        BVAL = 0
      ELSE
        BVAL = 1
      END IF
      CALL ARR_INIT1B( BVAL, EVENTS, %VAL(COPY), STATUS )

*    Select LISTs to choose ranges for
      IF ( .NOT. INDEXMODE ) THEN

        CALL MSG_PRNT (' ')
        CALL MSG_PRNT ('Enter the LISTs to have ranges applied, by '//
     :                                     'entering the index numbers')
        CALL MSG_PRNT ('E.g. 1 2 4')
        CALL PRS_GETRANGES ('LISTS', 2*NLISTS, 1, 1, MXSEL, INDEX, NSEL,
     :                                                          STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99

*    Obtain ranges for the selected LISTs
      DO I = 1, NSEL
        J = NINT (INDEX((2*I) - 1))
        SELECTED(J) = .TRUE.

*      Map list
        CALL CMP_MAPV( ILLOC(J), 'DATA_ARRAY', '_REAL', 'READ',
     :                 IDPTR(J), LEN, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL LIST_GFLDR( ILLOC(J), FMIN(J), FMAX(J), STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      If no FIELD_MIN or FIELD__MAX, then use data min and max
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL ARR_RANG1R( LEN, %VAL(IDPTR(J)), FMIN(J),
     :                                 FMAX(J), STATUS )
        END IF

        CALL MSG_PRNT (' ')
        CALL MSG_SETC ('NAME', NAME(J))
        CALL MSG_PRNT ('       ^NAME list:')

        CALL MSG_SETC( 'NAME', NAME(J))
        CALL MSG_SETR( 'MIN', FMIN(J) )
        CALL MSG_SETR( 'MAX', FMAX(J) )
        CALL MSG_PRNT( 'The ^NAME range is from ^MIN to ^MAX.' )

        CALL CHR_ITOC (J, PAR, LEN)
        PAR = 'KEEP'//PAR(1:LEN)
        CALL USI_GET0L( PAR, KEEP(J), STATUS )

        CALL CHR_ITOC (J, PAR, LEN)
        PAR = 'RANGES'//PAR(1:LEN)
        CALL PRS_GETRANGES( PAR, MXBNDS, 1, FMIN(J), FMAX(J),
     :                      RANGES(1,J), NRANGES(J), STATUS )

*      Check STATUS - exit if bad
        IF (STATUS .NE. SAI__OK) GOTO 99

*      Check ranges are increasing
        CALL ARR_INCR( 2*NRANGES(J), RANGES(1,J), OK )

        IF ( .NOT. OK ) THEN
          CALL MSG_PRNT ('FATAL ERROR: Ranges must be increasing')
          STATUS = SAI__ERROR
          GOTO 99
        END IF

*      Alter bounds according to QUANTUM
        CALL HDX_OK( ILLOC(J), 'QUANTUM', QOK(J), STATUS )

        IF ( QOK(J) ) THEN
          CALL CMP_SHAPE( ILLOC(J), 'QUANTUM', DAT__MXDIM, LDIMS,
     :                                             DIMS, STATUS )

          IF (DIMS .EQ. 0) THEN
            CALL CMP_GET0R( ILLOC(J), 'QUANTUM', QUANTUM, STATUS )
            CALL EVSUBSET_ALTER_RNG_S( QUANTUM, NRANGES(J), RANGES(1,J),
     :                               FMIN(J), FMAX(J), KEEP(J), STATUS )

          ELSE

*          Map input vector QUANTUM component
            QVEC(I) = .TRUE.
            CALL CMP_MAPV( ILLOC(I), 'QUANTUM', '_REAL', 'READ',
     :                                 IQPTR(J), LDIMS, STATUS )
            CALL EVSUBSET_ALTER_RNG_V( EVENTS, %VAL(IQPTR(J)),
     :                         NRANGES(J), RANGES(1,J), FMIN(J),
     :                         FMAX(J), STATUS )

          END IF
        END IF
      END DO

*    Check each event to see if it should be copied
      LEN = 0
      DO I = 1, EVENTS
        J  = 1
        OK = .TRUE.
        DO WHILE ( (J.LE.NLISTS) .AND. OK )
          CALL EVSUBSET_CHECK_RANGE( I, KEEP(J), NRANGES(J),
     :                                 RANGES(1,J), %VAL(IDPTR(J)),
     :                                 %VAL(COPY), OK, STATUS )
          J = J + 1
        END DO
        IF ( OK ) LEN = LEN + 1
      END DO

*    Else index mode
      ELSE

*      Report number of events
        CALL MSG_SETI( 'N', EVENTS )
        CALL MSG_PRNT( '^N events present in input. Select range '/
     :                                       /'of output events.' )

*      Array holding events to be copied
        CALL DYN_MAPI( 1, EVENTS, ICOPY, STATUS )

*      Get events to be kept
        CALL MSG_SETI( 'N', EVENTS )
        CALL MSG_MAKE( '1:^N', TXT, LEN )
        CALL USI_DEF0C( 'EVENTS', TXT(1:LEN), STATUS )
        CALL PRS_GETLIST( 'EVENTS', EVENTS, %VAL(ICOPY), LEN, STATUS )

*      Convert list of integers into logical array
        CALL EVSUBSET_CONVL( LEN, %VAL(ICOPY), EVENTS, %VAL(COPY),
     :                                                    STATUS )
        CALL DYN_UNMAP( ICOPY, STATUS )

      END IF

*    Check that at least one event has been selected
      IF ( LEN .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'All events have been excluded!', STATUS )
        GOTO 99
      END IF

*    Create output dataset.
      CALL DAT_NCOMP( INPL, NCOMP, STATUS )

      DO I = 1, NCOMP
        CALL DAT_INDEX( INPL, I, CLOC, STATUS )
        CALL DAT_TYPE( CLOC, CTYPE, STATUS )
        CALL CHR_UCASE( CTYPE )

        IF ( CTYPE .NE. 'LIST' ) THEN

*        Copy non LIST components
          CALL DAT_NAME( CLOC, CNAME, STATUS )
          CALL DAT_COPY( CLOC, OUTL, CNAME, STATUS )

        END IF
        CALL DAT_ANNUL (CLOC, STATUS)

      END DO

*    Count number of events to be copied in each block
      BSTART = 1
      DO IBLOCK = 1, EVENTS/BLOCKSIZE + 1

*      End of the block
        BEND = MIN(BSTART + BLOCKSIZE -1, EVENTS)
        BLEN = BEND - BSTART + 1

*      Count non-zeros
        CALL EVSUBSET_COUNT( BLEN, %VAL(COPY+VAL__NBB*(BSTART-1)),
     :                       CCOUNT(IBLOCK), STATUS )

*      Next block
        BSTART = BSTART + BLOCKSIZE

      END DO

*    Write output LISTs
      DO I = 1, NLISTS

*      Create output list
        CALL CHR_UCASE( NAME(I) )
        CALL DAT_NEW( OUTL, NAME(I), 'LIST', 0, 0, STATUS )
        CALL DAT_FIND( OUTL, NAME(I), OLLOC, STATUS )

*      Copy decreasing
        CALL HDX_OK( ILLOC(I), 'DECREASING', OK, STATUS )
        IF ( OK ) THEN
          CALL HDX_CCOPY( ILLOC(I), OLLOC, 'DECREASING', STATUS )
        END IF

*      Copy units
        CALL HDX_OK( ILLOC(I), 'UNITS', OK, STATUS )
        IF ( OK ) THEN
          CALL HDX_CCOPY( ILLOC(I), OLLOC, 'UNITS', STATUS )
        END IF

*      Create DATA_ARRAY
        CALL CMP_TYPE( ILLOC(I), 'DATA_ARRAY', CTYPE, STATUS )
        CALL DAT_NEW( OLLOC, 'DATA_ARRAY', CTYPE, 1, LEN, STATUS )

*      Decide on mapping type
        INT_TYPE = HDX_TYPINT( CTYPE )
        IF ( INT_TYPE ) THEN
          MTYPE = '_INTEGER'
        ELSE
          MTYPE = '_DOUBLE'
        END IF

*      Selected lists are already mapped. Unmap them.
        IF ( SELECTED(I) ) THEN
          CALL CMP_UNMAP( ILLOC(I), 'DATA_ARRAY', STATUS )
        END IF

*      Create output QUANTUM
        IF ( SELECTED(I) ) THEN
          IF ( QVEC(I) ) THEN
            CALL DAT_NEW( OLLOC, 'QUANTUM', '_UBYTE', 1, LEN, STATUS )

          ELSE IF ( QOK(I) ) THEN
            CALL HDX_CCOPY( ILLOC(I), OLLOC, 'QUANTUM', STATUS )

          ELSE
            FMIN(I) = RANGES(1,I)
            FMAX(I) = RANGES(2*NRANGES(I),I)

          END IF
        ELSE
          CALL HDX_OK (ILLOC(I), 'QUANTUM', QOK(I), STATUS)

          IF ( QOK(I) ) THEN
            CALL CMP_SHAPE( ILLOC(I), 'QUANTUM', DAT__MXDIM, LDIMS,
     :                                               DIMS, STATUS )

            IF (DIMS .EQ. 0) THEN
              CALL HDX_CCOPY( ILLOC(I), OLLOC, 'QUANTUM', STATUS )

            ELSE IF (DIMS .EQ. 1) THEN
              CALL DAT_NEW( OLLOC, 'QUANTUM', '_UBYTE', 1, LEN, STATUS )
              QVEC(I) = .TRUE.

            END IF

          END IF
        END IF

*      Locate the list arrays
        CALL DAT_FIND( ILLOC(I), 'DATA_ARRAY', DLOC, STATUS )
        CALL DAT_FIND( OLLOC, 'DATA_ARRAY', ODLOC, STATUS )

*      Locate QUANTUM if vector
        IF ( QVEC(I) ) THEN
          CALL DAT_FIND( ILLOC(I), 'QUANTUM', QLOC, STATUS )
          CALL DAT_FIND( OLLOC, 'QUANTUM', OQLOC, STATUS )
        END IF

*      Loop over blocks
        OFFSET = 0
        BSTART = 1
        DO IBLOCK = 1, EVENTS/BLOCKSIZE + 1

*        End of the block
          BEND = MIN(BSTART + BLOCKSIZE -1, EVENTS)
          BLEN = BEND - BSTART + 1

*        Any in this block?
          IF ( CCOUNT(IBLOCK) .EQ. 0 ) GOTO 50

*        Access and map the slice
          CALL DAT_SLICE( DLOC, 1, BSTART, BEND, SDLOC, STATUS )
          CALL DAT_MAPV( SDLOC, MTYPE, 'READ', ADPTR, LDIMS, STATUS )
          CALL DAT_SLICE( ODLOC, 1, OFFSET+1, OFFSET+CCOUNT(IBLOCK),
     :                    SODLOC, STATUS )
          CALL DAT_MAPV( SODLOC, MTYPE, 'WRITE', ODPTR, LDIMS, STATUS )

*        Ditto the quantum
          IF ( QVEC(I) ) THEN
            CALL DAT_SLICE( QLOC, 1, BSTART, BEND, SQLOC, STATUS )
            CALL DAT_MAPV( SQLOC, MTYPE, 'READ', AQPTR, LDIMS, STATUS )
            CALL DAT_SLICE( OQLOC, 1, OFFSET+1, OFFSET+CCOUNT(IBLOCK),
     :                      SOQLOC, STATUS )
            CALL DAT_MAPV( SOQLOC, MTYPE, 'WRITE', OQPTR, LDIMS,
     :                     STATUS )
          END IF

*        Copy the data
          IF ( INT_TYPE ) THEN
            CALL EVSUBSET_QCOPYI( BLEN,
     :                            %VAL(ADPTR), QVEC(I), %VAL(AQPTR),
     :                            %VAL(COPY+(BSTART-1)*VAL__NBB),
     :                            %VAL(ODPTR), %VAL(OQPTR), STATUS )
          ELSE
            CALL EVSUBSET_QCOPYD( BLEN,
     :                            %VAL(ADPTR), QVEC(I), %VAL(AQPTR),
     :                            %VAL(COPY+(BSTART-1)*VAL__NBB),
     :                            %VAL(ODPTR), %VAL(OQPTR), STATUS )
          END IF

*        Annul slice
          CALL DAT_UNMAP( SODLOC, STATUS )
          CALL DAT_ANNUL( SODLOC, STATUS )
          CALL DAT_UNMAP( SDLOC, STATUS )
          CALL DAT_ANNUL( SDLOC, STATUS )
          IF ( QVEC(I) ) THEN
            CALL DAT_UNMAP( SOQLOC, STATUS )
            CALL DAT_ANNUL( SOQLOC, STATUS )
            CALL DAT_UNMAP( SQLOC, STATUS )
            CALL DAT_ANNUL( SQLOC, STATUS )
          END IF

*        Adjust output pointer
 50       BSTART = BSTART + BLOCKSIZE
          OFFSET = OFFSET + CCOUNT(IBLOCK)

*      Next block
        END DO

*      Free the list components
        CALL DAT_ANNUL( DLOC, STATUS )
        CALL DAT_ANNUL( ODLOC, STATUS )
        IF ( QVEC(I) ) THEN
          CALL DAT_ANNUL( QLOC, STATUS )
          CALL DAT_ANNUL( OQLOC, STATUS )
        END IF

*      Write FIELD_MIN/MAX values
        IF ( SELECTED(I) ) THEN

*        Create new FIELD_MIN/MAX
          CALL DAT_NEW( OLLOC, 'FIELD_MIN', CTYPE, 0, 0, STATUS )
          CALL DAT_NEW( OLLOC, 'FIELD_MAX', CTYPE, 0, 0, STATUS )

          CALL CMP_PUT0R( OLLOC, 'FIELD_MIN', FMIN(I), STATUS )
          CALL CMP_PUT0R( OLLOC, 'FIELD_MAX', FMAX(I), STATUS )

        ELSE

*        Copy old FIELD_MIN/MAX values
          CALL HDX_OK (ILLOC(I), 'FIELD_MIN', OK, STATUS)
          IF ( OK ) THEN
            CALL HDX_CCOPY( ILLOC(I), OLLOC, 'FIELD_MIN', STATUS )
          END IF

          CALL HDX_OK (ILLOC(I), 'FIELD_MAX', OK, STATUS)
          IF ( OK ) THEN
            CALL HDX_CCOPY( ILLOC(I), OLLOC, 'FIELD_MAX', STATUS )
          END IF

        END IF

*      Alter Obs length if neccessary
        IF ( SELECTED(I) .AND. (NAME(I)(1:11) .EQ. 'RAW_TIMETAG' .OR.
     :                                NAME(I)(1:7) .EQ. 'TIMETAG')) THEN

          OBS_LENGTH = 0.0

          DO J = 1, NRANGES(I)
            K = (2 * J) - 1
            OBS_LENGTH = OBS_LENGTH + (RANGES(K+1,I) - RANGES(K,I))
          END DO

          CALL BDA_CHKHEAD( INPL, OK, STATUS )
          IF ( OK ) THEN
            CALL BDA_LOCHEAD( INPL, HEADER, STATUS )
            CALL HDX_OK( HEADER, 'OBS_LENGTH', OK, STATUS)

            IF ( OK .AND. .NOT. KEEP(I) ) THEN
              CALL CMP_GET0R( HEADER, 'OBS_LENGTH', OLD_OBS_LENGTH,
     :                                                     STATUS )
              OBS_LENGTH = OLD_OBS_LENGTH - OBS_LENGTH
            ELSE IF (.NOT. OK .AND. .NOT. KEEP(I) ) THEN
              OBS_LENGTH = VAL__BADR
            END IF
            CALL BDA_LOCHEAD( OUTL, HEADER, STATUS )
            CALL CMP_PUT0R( HEADER, 'OBS_LENGTH', OBS_LENGTH, STATUS )
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF

        END IF
      END DO

*    History
      CALL HIST_ADD( OUTL, VERSION, STATUS )

*    Exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVSUBSET_CHECK_RANGE - Checks event is within allowed ranges
      SUBROUTINE EVSUBSET_CHECK_RANGE( EVENT, KEEP, NRANGES, RANGES,
     :                                 DATA, COPY, OK, STATUS )
*    Description :
*
*     Checks to see if the current event is within the specified ranges.
*
*    Authors :
*
*     Phil Andrews (BHVAD::PLA)
*
*    History :
*
*      1 Jun 89 : Original (PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER        EVENT                         ! Index of current event
      LOGICAL        KEEP                          ! Keep this list?
      INTEGER        NRANGES                       ! Number of ranges
      REAL           RANGES(*)                     ! Ranges supplied
      REAL           DATA (*)                      ! Mapped LIST DATA_ARRAY
*
*    Export :
*
      BYTE           COPY (*)                      ! copy this event?
      LOGICAL        OK                            ! If true, copy event
*
*    Local variables :
*
      INTEGER        I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( (COPY(EVENT).NE.0) .AND. KEEP ) THEN
        I  = 1
        OK = .FALSE.

        DO WHILE ( (I.LE.NRANGES) .AND. .NOT. OK )
          J = (2 * I) - 1
          IF ( (RANGES(J).LE.DATA(EVENT)) .AND.
     :         (RANGES(J+1) .GT. DATA(EVENT)) ) THEN
            OK = .TRUE.
          END IF
          I = I + 1
        END DO

        IF ( .NOT. OK ) COPY(EVENT) = 0

      ELSE IF ( COPY(EVENT) .NE. 0 ) THEN
        I  = 1
        OK = .TRUE.

        DO WHILE ( (I.LE.NRANGES) .AND. OK )
          J = (2 * I) - 1

          IF ( (DATA(EVENT).GT.RANGES(J)) .AND.
     :         (DATA(EVENT).LE.RANGES(J+1)) ) THEN
            OK = .FALSE.
            COPY(EVENT) = 0
          END IF
          I = I + 1

        END DO

      END IF

      END


*+  EVSUBSET_QCOPYD - Copy DATA & QUANTUM values
      SUBROUTINE EVSUBSET_QCOPYD( EVENTS, IN, QOK, QIN, COPY, OUT, QOUT,
     :                            STATUS )
*    Description :
*     Copies from IN to OUT if COPY is true
*    Authors :
*     Phil Andrews
*    History :
*      1-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER        EVENTS

      DOUBLE PRECISION IN(*)
      LOGICAL          QOK
      DOUBLE PRECISION QIN(*)

      BYTE            COPY(*)
*
*    Export :
*
      DOUBLE PRECISION QOUT(*)
      DOUBLE PRECISION OUT(*)
*
*    Local variables :
*
      INTEGER        I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = 0

      IF ( QOK ) THEN
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
            QOUT(J) = QIN(J)
          END IF
        END DO
      ELSE
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
          END IF
        END DO
      END IF

      END



*+  EVSUBSET_QCOPYI - Copy DATA & QUANTUM values
      SUBROUTINE EVSUBSET_QCOPYI( EVENTS, IN, QOK, QIN, COPY, OUT, QOUT,
     :                            STATUS )
*    Description :
*     Copies from IN to OUT if COPY is true
*    Authors :
*     Phil Andrews
*    History :
*      1-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER        EVENTS
      LOGICAL        QOK
      INTEGER        IN(*)
      INTEGER        QIN(*)

      BYTE           COPY(*)
*
*    Export :
*
      INTEGER        QOUT(*)
      INTEGER        OUT(*)
*
*    Local variables :
*
      INTEGER        I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = 0

      IF ( QOK ) THEN
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
            QOUT(J) = QIN(J)
          END IF
        END DO
      ELSE
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
          END IF
        END DO
      END IF

      END


*+  EVSUBSET_ALTER_RNG_S - Alter range values according to SCALAR quantum
      SUBROUTINE EVSUBSET_ALTER_RNG_S( QUANTUM, NRANGES, RANGES,
     :                                 FMIN, FMAX, KEEP, STATUS )
*    Description :
*     Moves the specified RANGE boundaries to intrinsic bin centers
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*
*      6 Jun 89 : Original (PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      REAL                    QUANTUM              ! Value of scalar quantum
      INTEGER                 NRANGES              !
      LOGICAL                 KEEP                 !
*
*    Import-Export :
*
      REAL                    FMIN, FMAX           ! List extrema
      REAL                    RANGES(*)
*
*    Local variables :
*
      INTEGER                 I, J                 ! Loop counters

      REAL                    N                    ! Number of quanta
                                                   ! from FMIN to bound
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over the ranges
      DO I = 1, 2 * NRANGES
        IF ( QUANTUM .GT. 0.0 ) THEN
          N = (RANGES(I) - FMIN) / QUANTUM
          IF (N .NE. REAL(INT(N))) THEN
            RANGES(I) = (REAL(INT(N) + 1) * QUANTUM) + FMIN
          END IF
        END IF
      END DO

*    Make sure lower bound is not too low
      RANGES(1) = MAX(RANGES(1),FMIN)
      I = 2 * NRANGES

*    Make sure upper bound is not too high
      RANGES(I) = MIN(RANGES(I),FMAX)

*    Alter FMIN & FMAX to output values
      IF ( KEEP) THEN
        FMIN = RANGES(1)
        FMAX = RANGES(I)

      ELSE
        IF ( RANGES(1) .EQ. FMIN ) THEN
          J = 2
          DO WHILE ( RANGES(J) .EQ. RANGES(J+1) )
            J = J + 2
          END DO
          FMIN = RANGES(J)
        END IF

        IF ( RANGES(I) .EQ. FMAX ) THEN
          J = I - 2
          DO WHILE ( RANGES(J) .EQ. RANGES(J+1) )
            J = J - 2
          END DO
          FMAX = RANGES(J + 1)
        END IF

      END IF

      END


*+  EVSUBSET_ALTER_RNG_V - Adjust range bounds for vector quantum.
      SUBROUTINE EVSUBSET_ALTER_RNG_V( EVENTS, QUANTUM, NRANGES, RANGES,
     :                                 FMIN, FMAX, STATUS )
*    Description :
*     Adjusts range bounds to bin centers, for vector quantum
*    Authors :
*     Phil Andrews
*    History :
*      6-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                 EVENTS               ! Number of events in
      REAL                    QUANTUM(EVENTS)      ! Value of vector quantum
      INTEGER                 NRANGES
      REAL                    FMIN, FMAX           ! List bounds
*
*    Import-Export :
*
      REAL                    RANGES(*)
*
*    Local variables :
*
      INTEGER                 I, J

      REAL                    POSITION
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Adjust ends
      RANGES(1) = MAX(FMIN,RANGES(1))
      RANGES(2*NRANGES) = MIN(FMAX,RANGES(2*NRANGES))

*    Loop over bounds
      J = 1
      POSITION = FMIN
      DO I = 1, 2 * NRANGES
        DO WHILE ( POSITION .LT. RANGES(I) )
          POSITION = POSITION + ((QUANTUM(J) + QUANTUM(J+1)) / 2.0)
        END DO
        RANGES(I) = POSITION
      END DO

      END


*+  EVSUBSET_CONVL - Convert list of good events to array of event selectors
      SUBROUTINE EVSUBSET_CONVL( NIN, IN, NOUT, OUT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method:
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*      1 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER NIN
      INTEGER IN(NIN)
*
*    Export :
*
      INTEGER NOUT
      BYTE    OUT(NOUT)
*
*    Status :
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over selected events
      DO I = 1, NIN
        OUT(IN(I)) = 1
      END DO

      END


*+  EVSUBSET_COUNT - Count selected events
      SUBROUTINE EVSUBSET_COUNT( NIN, COPY, NOUT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method:
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*      1 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER NIN
      BYTE    COPY(NIN)
*
*    Export :
*
      INTEGER NOUT
*
*    Status :
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER I
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over selected events
      NOUT = 0
      DO I = 1, NIN
        IF ( COPY(I) .NE. 0 ) NOUT = NOUT + 1
      END DO

      END
