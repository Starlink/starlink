      SUBROUTINE EVSUBSET( STATUS )
*+
*  Name:
*     EVSUBSET

*  Purpose:
*     Subset an EVENT dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVSUBSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Produces a subset of an EVENT dataset. Up to 10 of the lists in the
*     input evds may have ranges imposed. All lists are copied from the
*     input file to the output file.

*  Usage:
*     evsubset {parameter_usage}

*  Environment Parameters:
*     INP = LITERAL (read)
*        Name of input event dataset
*     OUT = LITERAL (read)
*        Name of the output event dataset
*     RANGES1..7

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
*     Can only select ranges for 10 of the LISTs - this is to limit the
*     number of parameters required.

*  References:
*     {task_references}...

*  Keywords:
*     evsubset, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     PLA: Phil Andrews (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     25 May 1989 V1.0-0 (PLA).
*        Original version.
*     11 Jan 1990 V1.0-3 (DJA):
*        DATA_MIN and DATA_MAX references removed.
*        Structure references put in include file
*     21 Nov 1990 V1.3-0 (DJA):
*        No longer crashes if FIELD_MIN/MAX absent
*      8 Jul 1992 V1.6-0 (DJA):
*        Much tidying (DJA)
*      9 Aug 1993 V1.7-0 (DJA):
*        Use PRM constant to write bad observation length
*      1 Nov 1993 V1.7-1 (DJA):
*        INDEX mode added. Data transfer in D.P. (DJA)
*      2 Dec 1993 V1.7-2 (DJA):
*        Removed Fortran structures. Used byte array for
*        event selection control. (DJA)
*     26 Jan 1994 V1.7-3 (DJA):
*        Minor bug fix to contiguous block handling. Allocate
*        section table dynamically.
*     28 Jan 1994 V1.7-4 (DJA):
*        Rejected contigous block algorithm in favour of simply copying
*        in BLOCKSIZE lumps - much simpler
*        and more efficient for sparse selection.
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface (DJA)
*     14 Dec 1995 V2.0-0 (DJA):
*        Full ADI port.
*      3 Jan 1996 V2.0-1 (DJA):
*        Main body of copying moved to EDI_SUBSET (DJA)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER                 	MXSEL                	! Maximum number of
        PARAMETER            	(MXSEL = 10)          	! LISTs to select

      INTEGER                 	MXBNDS               	! Maximum number of bounds
        PARAMETER            	(MXBNDS = 1000)       	! => 500 ranges

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVSUBSET Version 2.1-0' )

*  Local Variables:
      CHARACTER*20  		NAME(MXSEL)    		! Names of selected lists
      CHARACTER*12            	PAR                  	! Parameter name
      CHARACTER*20		TNAME			! Time list name
      CHARACTER*200             TXT                  	! Input text

      REAL			FMIN(MXSEL)		! List lower bound
      REAL                    	FMAX(MXSEL)     	! List upper bound
      REAL                    	OBS_LENGTH           	! Observation length
      REAL                    	OLD_OBS_LENGTH       	! obs length of
							! original dataset
      REAL                    	RANGES(MXBNDS,MXSEL) 	! Range boundaries
      REAL                    	QUANTUM              	! Value of scalar quantum

      INTEGER                 	COPY                 	! Pointer to copy array
      INTEGER                 	EVENTS               	! Number of events in EVDS
      INTEGER			EVID			! New event object
      INTEGER                 	I, J, K              	! Loop counters
      INTEGER                 	ICOPY                	! Pointer to copy array
      INTEGER                 	IDPTR(MXSEL)    	! List data pointers
      INTEGER			IFID			! Input dataset id
      INTEGER                   INDEX(MXSEL)       	! Lists to apply ranges
      INTEGER                 	IQPTR(MXSEL)    	! Pointer to input quantum
      INTEGER                 	LEN                  	! Length of various things
      INTEGER			LID			! List identifier
      INTEGER                 	NLISTS               	! Number of LISTs
      INTEGER                 	NRANGES(MXSEL)  	! Number of ranges per list
      INTEGER                 	NSEL                 	! # selected lists
      INTEGER			OFID			! Output dataset id
      INTEGER			OLID			! Output list id
      INTEGER			TIMID			! Timing info
      INTEGER			TLIST			! Time list number

      BYTE			BVAL			! Byte initialiser

      LOGICAL                 	INDEXMODE            	! Index mode on?
      LOGICAL                 	KEEP(MXSEL)     	! Keep selected ranges?
      LOGICAL                 	OK		   	! General validity check
      LOGICAL                 	QOK(MXSEL)      	! Quantum ok?
      LOGICAL                 	QVEC(MXSEL)     	! Vector quantum?
      LOGICAL                 	SELECT 			! List selected?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Initialise
      DO I = 1, MXSEL
        QVEC(I) = .FALSE.
      END DO

*  Associate input
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Index mode?
      CALL USI_GET0L( 'INDEX', INDEXMODE, STATUS )

*  Get number of events and lists
      CALL EDI_GETNS( IFID, EVENTS, NLISTS, STATUS )

*  Associate output dataset
      CALL ADI_NEW0( 'EventDS', EVID, STATUS )
      CALL USI_CREAT( 'OUT', EVID, OFID, STATUS )

*  Display lists
      IF ( .NOT. INDEXMODE ) THEN
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( 'The availible LISTs are:' )
        CALL EDI_DISP( IFID, STATUS )
      END IF

*  Find out which events are to be copied. We use a BYTE here rather than
*  a LOGICAL to save dynamic memory.
      CALL DYN_MAPB( 1, EVENTS, COPY, STATUS )
      BVAL = 1
      IF ( INDEXMODE ) BVAL = 0
      CALL ARR_INIT1B( BVAL, EVENTS, %VAL(COPY), STATUS )

*  Select LISTs to choose ranges for
      IF ( .NOT. INDEXMODE ) THEN

        CALL MSG_PRNT (' ')
        CALL MSG_PRNT ('Enter the LISTs to have ranges applied, by '//
     :                                     'entering the index numbers')
        CALL MSG_PRNT ('E.g. 1 2 4')
        CALL EDI_SELCT( 'LISTS', NLISTS, 1, MXSEL, INDEX, NSEL, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Obtain ranges for the selected LISTs
        DO I = 1, NSEL

*      Locate this list, and gets its name
          CALL EDI_IDX( IFID, INDEX(I), LID, STATUS )
          CALL ADI_CGET0C( LID, 'Name', NAME(I), STATUS )

*      Map list
          CALL EDI_MAPR( IFID, NAME(I), 'READ', 0, 0, IDPTR(I), STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Get extreme values
          CALL ADI_CGET0R( LID, 'Min', FMIN(I), STATUS )
          CALL ADI_CGET0R( LID, 'Max', FMAX(I), STATUS )

*      If no field extrema present, use data minimum and maximum
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL ARR_RANG1R( LEN, %VAL(IDPTR(I)), FMIN(I),
     :                                   FMAX(I), STATUS )
          END IF

          CALL MSG_PRNT (' ')
          CALL MSG_SETC ('NAME', NAME(I))
          CALL MSG_PRNT ('       ^NAME list:')

          CALL MSG_SETC( 'NAME', NAME(I))
          CALL MSG_SETR( 'MIN', FMIN(I) )
          CALL MSG_SETR( 'MAX', FMAX(I) )
          CALL MSG_PRNT( 'The ^NAME range is from ^MIN to ^MAX.' )

          CALL CHR_ITOC( I, PAR, LEN )
          CALL USI_GET0L( 'KEEP'//PAR(:LEN), KEEP(I), STATUS )

          CALL PRS_GETRANGES( 'RANGES'//PAR(:LEN), MXBNDS, 1, FMIN(I),
     :                      FMAX(I), RANGES(1,I), NRANGES(I), STATUS )

*      Check STATUS - exit if bad
          IF (STATUS .NE. SAI__OK) GOTO 99

*      Check ranges are increasing
          CALL ARR_INCR( 2*NRANGES(I), RANGES(1,I), OK )
          IF ( .NOT. OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR: Ranges must be increasing',
     :                  STATUS )
            GOTO 99
          END IF

*      Alter bounds according to QUANTUM
          QVEC(I) = .FALSE.
          CALL ADI_THERE( LID, 'Quantum', QOK(I), STATUS )
          IF ( QOK(I) ) THEN
            CALL ADI_CGET0R( LID, 'Quantum', QUANTUM, STATUS )
            CALL EVSUBSET_ALTER_RNG_S( QUANTUM, NRANGES(I), RANGES(1,I),
     :                               FMIN(I), FMAX(I), KEEP(I), STATUS )
          ELSE
            CALL ADI_CGET0L( LID, 'VectorQuantum', QVEC(I), STATUS )
            IF ( QVEC(I) ) THEN
              CALL EDI_QMAP( IFID, NAME(I), 'REAL', 'READ', 0, 0,
     :                                        IQPTR(I), STATUS )
              CALL EVSUBSET_ALTER_RNG_V( EVENTS, %VAL(IQPTR(I)),
     :                         NRANGES(I), RANGES(1,I), FMIN(I),
     :                         FMAX(I), STATUS )
            END IF
          END IF

        END DO

*    Check each event to see if it should be copied
        LEN = 0
        DO I = 1, EVENTS
          J  = 1
          OK = .TRUE.
          DO WHILE ( (J.LE.NSEL) .AND. OK )
            CALL EVSUBSET_CHECK_RANGE( I, KEEP(J), NRANGES(J),
     :                                 RANGES(1,J), %VAL(IDPTR(J)),
     :                                 %VAL(COPY), OK, STATUS )
            J = J + 1
          END DO
          IF ( OK ) LEN = LEN + 1
        END DO

*    Unmap the selected lists
        DO I = 1, NSEL
          CALL EDI_UNMAP( IFID, NAME(I), STATUS )
          IF ( QVEC(I) ) THEN
            CALL EDI_QUNMAP( IFID, NAME(I), STATUS )
          END IF
        END DO

*    Else index mode
      ELSE

*    Report number of events
        CALL MSG_SETI( 'N', EVENTS )
        CALL MSG_PRNT( '^N events present in input. Select range '/
     :                                       /'of output events.' )

*    Array holding events to be copied
        CALL DYN_MAPI( 1, EVENTS, ICOPY, STATUS )

*    Get events to be kept
        CALL MSG_SETI( 'N', EVENTS )
        CALL MSG_MAKE( '1:^N', TXT, LEN )
        CALL USI_DEF0C( 'EVENTS', TXT(1:LEN), STATUS )
        CALL PRS_GETLIST( 'EVENTS', EVENTS, %VAL(ICOPY), LEN, STATUS )

*    Convert list of integers into logical array
        CALL EVSUBSET_CONVL( LEN, %VAL(ICOPY), EVENTS, %VAL(COPY),
     :                                                    STATUS )
        CALL DYN_UNMAP( ICOPY, STATUS )

      END IF

*  Check that at least one event has been selected
      IF ( LEN .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'All events have been excluded!', STATUS )
        GOTO 99
      END IF

*  Set the length of the output object
      CALL ADI_CPUT0I( OFID, 'NEVENT', LEN, STATUS )

*  Copy ancillaries from input
      CALL UDI_COPANC( IFID, ' ', OFID, STATUS )

*  Copy lists to output
      CALL EDI_SUBSET( IFID, %VAL(COPY), OFID, STATUS )

*  Loop over selected lists to update ranges
      DO I = 1, NSEL

*    Locate input list
        CALL EDI_IDX( OFID, INDEX(I), OLID, STATUS )

*    Update lists extrema for selected lists
        FMIN(I) = RANGES(1,I)
        FMAX(I) = RANGES(2*NRANGES(I),I)
        CALL ADI_CPUT0R( OLID, 'Min', FMIN(I), STATUS )
        CALL ADI_CPUT0R( OLID, 'Max', FMAX(I), STATUS )

*    Update list fields
        CALL EDI_LUPDT( OFID, OLID, 'Min,Max', STATUS )

*    Release the list
        CALL ADI_ERASE( OLID, STATUS )

*  Next list
      END DO

*  Time list present?
      CALL EDI_QFND( IFID, 'T', TNAME, TLIST, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Was it selected?
        SELECT = .FALSE.
        J = 1
        DO WHILE ( (J.LE.NSEL) .AND. .NOT. SELECT )
          IF ( J .EQ. INDEX(J) ) THEN
            SELECT = .TRUE.
          ELSE
            J = J + 1
          END IF
        END DO
        IF ( SELECT ) THEN

*      Accumulate selected ranges
          OBS_LENGTH = 0.0
          DO I = 1, NRANGES(J)
            K = (2 * I) - 1
            OBS_LENGTH = OBS_LENGTH +
     :               (RANGES(K+1,TLIST) - RANGES(K,TLIST))
          END DO

*      Extract existing observation length
          CALL TCI_GETID( IFID, TIMID, STATUS )
          CALL ADI_THERE( TIMID, 'ObsLength', OK, STATUS )
          IF ( OK .AND. .NOT. KEEP(I) ) THEN
            CALL ADI_CGET0R( TIMID, 'ObsLength', OLD_OBS_LENGTH,
     :                       STATUS )
            OBS_LENGTH = OLD_OBS_LENGTH - OBS_LENGTH
          ELSE IF ( .NOT. OK .AND. .NOT. KEEP(J) ) THEN
            OBS_LENGTH = VAL__BADR
          END IF

*      Write new length
          CALL ADI_CPUT0D( TIMID, 'ObsLength', OBS_LENGTH, STATUS )
          CALL TCI_PUTID( OFID, TIMID, STATUS )

        END IF

*  No time list, but not a problem
      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  History
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
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
