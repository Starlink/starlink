      SUBROUTINE EVMERGE( STATUS )
*+
*  Name:
*     EVMERGE

*  Purpose:
*     Merge event datasets together

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVMERGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
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

*  Usage:
*     evmerge {parameter_usage}

*  Environment Parameters:
*     INP<n> = CHAR (read)
*        The input event datasets, <n> = 1 to 10
*     OUT = CHAR (readd)
*        The output event dataset

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
*     evmerge, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden (University of Birmingham)
*     ADM: Alan McFadzean (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Jul 1984 (JCMP):
*        Original version
*     23 Oct 1984 (JCMP):
*        Zero compensation enabled
*     20 Nov 1984 (JCMP):
*        Handles case of single input dataset
*     21 Nov 1984 (JCMP):
*        Live time handling, field size handling
*      5 Feb 1985 (JCMP):
*        Max # of lists increased
*     16 May 1985 V0.3-1 (JCMP):
*        Merging of CORRECTIONS components
*     18 Jun 1985 V0.3-2 (JCMP):
*        Bug fix - don't rcopy CORRECTIONS (JCMP)
*     28 Jan 1986 V0.4-1 (JCMP):
*        ADAM version
*     23 Sep 1986 V0.5-1 (JCMP):
*        EXO_MOVBYT -> GEN_COPY
*      8 Oct 1986 V0.5-2 (JCMP):
*        Input parameters changed
*     11 Mar 1988 V0.6-1 (ADM):
*        ROSAT version
*     13 Sep 1988 V1.0-1 (ADM):
*        ASTERIX88 version
*      1 Aug 1989 V1.0-2 (DJA):
*        Bug in live time sort fixed
*      8 Mar 1990 V1.2-0 (DJA):
*        Added check for non-existent units in input lists
*     17 Feb 1991 V1.4-0 (DJA):
*        Allows merging of files with different pointing directions. LIVE_TIME
*        duplicates removed. Lists may be in any order - extra lists are omitted
*      6 Mar 1991 V1.4-1 (DJA):
*        Bug when 1st file not earliest fixed
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PRM_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      INTEGER                	MXINP                   ! Max # input datasets
        PARAMETER         	( MXINP = 10 )

      INTEGER                	MXTEXT                  ! maximum line text
        PARAMETER         	( MXTEXT = MXINP*2 )

      INTEGER			MXLST               	! Max # of lists in a dataset
        PARAMETER         	( MXLST = 50 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVMERGE Version V2.0-0' )

*  Local Variables:
      CHARACTER*20  	        LNAME(MXLST)  		! Names of input lists
      CHARACTER*40		LUNIT, UNIT(MXLST)   	! Units of input lists
      CHARACTER*7               MTYPE			! List mapping type
      CHARACTER*80           	TEXTI(MXTEXT)           ! Input files
      CHARACTER*20		TLNAME			! Time list name
      CHARACTER*20		XLNAME, YLNAME	        ! X, Y list names

      DOUBLE PRECISION		TOFFS(MXINP)		! Offsets to TAI(TBASE)

      REAL                   	FMAX(MXLST)         	! Output list field max
      REAL                   	FMIN(MXLST)         	! Output list field min
      REAL                   	FX(4), FY(4)            ! Extrema in X,Y lists
      REAL			VALUE			! ADI object value

      INTEGER           	IFID(MXINP)		! I/p file identifiers
      INTEGER               	IPTR(MXLST)          	! I/p list data
      INTEGER			IQPTR			! I/p list quanta
      INTEGER                	IFILE                   ! Loop over datasets
      INTEGER                	ILIST                   ! Loop over lists
      INTEGER                	INLINES                 ! # input object
      INTEGER			LID(MXLST,MXINP)
      INTEGER                	NEVENT(MXINP)          	! Length of lists
      INTEGER                	NIN                     ! # input datasets
      INTEGER                	NLIST(MXINP)     	! # lists in a dataset
      INTEGER			OFID			! O/p file identifier
      INTEGER			OLID(MXLST)		! O/p list identifiers
      INTEGER                	OPTR(MXLST)         	! O/p list data
      INTEGER			OQPTR			! O/p list quanta
      INTEGER                	ONELM                   ! # o/p events
      INTEGER			OTIMID			! O/p timing info
      INTEGER			PIXID(MXINP),		! World coordinates
     :                 		 PRJID(MXINP), SYSID(MXINP)
      INTEGER			T_AX, X_AX, Y_AX	! List numbers
      INTEGER                	START                   ! Start position for copy
      INTEGER                	TBASE                   ! Timing origin file #
      INTEGER			TIMID(MXINP)		! Timing data

      LOGICAL               	ANY_EMOVE           	! Any events to move?
      LOGICAL                	INPUT                   ! Used to control input loop
      LOGICAL                	LCOPY(MXLST)        	! Copy these lists?
      LOGICAL                	OK                      ! Data item acceptable?
      LOGICAL          		QUANVEC(MXLST)      	! Is QUANTUM a vector?
      LOGICAL			SAME   			! WCS identical?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input datasets
      NIN = 1
      ANY_EMOVE = .FALSE.
      INPUT = .TRUE.
      ONELM = 0
      DO WHILE ( INPUT )

*    Associate input object
        CALL USI_IASSOC( 'INP', NIN, 'EventDS', 'READ',
     :                              IFID(NIN), STATUS )
        OK = .TRUE.

*      Check status
        IF ( STATUS .EQ. PAR__ABORT  ) THEN
          INPUT = .FALSE.
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
        END IF

*    Process file
        IF ( OK .AND. INPUT ) THEN

*      Get number of events and lists
          CALL EDI_GETNS( IFID(NIN), NEVENT(NIN), NLIST(NIN), STATUS )

*      First input?
          IF ( NIN .EQ. 1 ) THEN

*        For each list in the first input
            DO ILIST = 1, NLIST(1)

*          Index the list by number
              CALL EDI_IDX( IFID(1), ILIST, LID(ILIST,1), STATUS )

*          Extract its units
              CALL ADI_CGET0C( LID(ILIST,1), 'Units', UNIT(ILIST),
     :                         STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                UNIT(ILIST) = ' '
                CALL ERR_ANNUL( STATUS )
              END IF

*          Get its name
              CALL ADI_CGET0C( LID(ILIST,1), 'Name', LNAME(ILIST),
     :                         STATUS )

*          Vector quantum?
              CALL ADI_CGET0L( LID(ILIST,1), 'VectorQuantum',
     :                         QUANVEC(ILIST), STATUS )

*          Mark to be copied
              LCOPY(ILIST) = .TRUE.

            END DO

          END IF

*      Get timing and pointing data from header
          CALL TCI_GETID( IFID(NIN), TIMID(NIN), STATUS )
          CALL WCI_GETIDS( IFID(NIN), PIXID(NIN), PRJID(NIN),
     :                     SYSID(NIN), STATUS )

*      Second or subsequent input
          IF ( NIN .GT. 1 ) THEN

*        Check pointing direction
            CALL WCI_SAME( PIXID(1), PRJID(1), SYSID(1),
     :                     PIXID(NIN), PRJID(NIN), SYSID(NIN),
     :                     SAME, STATUS )

            IF ( .NOT. SAME ) THEN
              CALL MSG_SETI( 'N', NIN )
              CALL MSG_PRNT( 'Datasets 1 and ^N have different '/
     :           /'coordinate systems - events will be shifted '/
     :           /'to the pointing direction of dataset 1' )
     :
              ANY_EMOVE = .TRUE.
            END IF

*        Check each list we're still trying to copy
            DO ILIST = 1, NLIST(1)

*          Initialise
              LID(ILIST,NIN) = ADI__NULLID

*          Still trying to copy this one?
              IF ( LCOPY(ILIST) ) THEN

*            List is present in this input file?
                CALL EDI_CHK( IFID(NIN), LNAME(ILIST), LCOPY(ILIST),
     :                        STATUS )

*            Its ok, so locate it
                IF ( LCOPY(ILIST) ) THEN

*              Locate it
                  CALL EDI_IDXNAM( IFID(NIN), LNAME(ILIST),
     :                             LID(ILIST,NIN), STATUS )

*              Extract its units and compare with first input
                  CALL ADI_CGET0C( LID(ILIST,1), 'Units', LUNIT,
     :                             STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                    LUNIT = ' '
                    CALL ERR_ANNUL( STATUS )
                  END IF
                  IF ( .NOT. CHR_SIMLR( LUNIT, UNIT(ILIST)) ) THEN
                    CALL MSG_SETI( 'IF', NIN )
                    CALL MSG_SETC( 'LN', LNAME(ILIST) )
                    CALL MSG_PRNT( 'WARNING : Units of list ^LN in '/
     :                            /'dataset ^IF not do agree with '/
     :                            /'those in 1st dataset' )
                  END IF

*              Still vector quantum
                  IF ( QUANVEC(ILIST) ) THEN
                    CALL ADI_CGET0L( LID(ILIST,NIN), 'VectorQuantum',
     :                             QUANVEC(ILIST), STATUS )
                  END IF

*            Otherwise warn user than it will be lost
                ELSE
                  CALL MSG_SETI( 'IF', NIN )
                  CALL MSG_SETC( 'LN', LNAME(ILIST) )
                  CALL MSG_PRNT( 'List ^LN is not present in dataset'/
     :                              /' ^IF, and will not be merged' )

                END IF

              END IF

            END DO

          END IF

*      Bump up counter
          IF ( STATUS .EQ. SAI__OK ) THEN
            ONELM = ONELM + NEVENT(NIN)
            NIN = NIN + 1
            IF ( NIN .GT. MXINP ) THEN
              CALL MSG_PRNT( 'No more inputs allowed!' )
              INPUT = .FALSE.
            END IF
          END IF

        END IF

      END DO

*  Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NIN = NIN - 1

*  Create output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL EDI_LINK( 'EventDS', ONELM, 'Merged events', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Copy the lists from the 1st dataset to the output
      DO ILIST = 1, NLIST(1)
        IF ( LCOPY(ILIST) ) THEN
          CALL ADI_COPY( LID(ILIST,1), OLID(ILIST), STATUS )
          CALL EDI_CREAT( OFID, OLID(ILIST), STATUS )

*      Extract field extrema
          CALL ADI_CGET0R( LID(ILIST,1), 'Min', FMIN(ILIST), STATUS )
          CALL ADI_CGET0R( LID(ILIST,1), 'Max', FMAX(ILIST), STATUS )

        END IF
      END DO

*  If any of the datasets require event moving...
      IF ( ANY_EMOVE ) THEN

*    Locate the lists WCI has chosen for the local coordinates
        DO IFILE = 1, NIN

*      If error, abandon event moving
          CALL EDI_QFND( IFID(IFILE), 'X', XLNAME, X_AX, STATUS )
          CALL EDI_QFND( IFID(IFILE), 'Y', YLNAME, Y_AX, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL MSG_SETI( 'N', IFILE )
            CALL MSG_PRNT( 'Input dataset ^N has no spatial coordinate'/
     :                         /' lists - no event shifting performed' )
            ANY_EMOVE = .FALSE.
          END IF

        END DO

      END IF

*  Merge timing data
      CALL TCI_MERGE( NIN, TIMID, OTIMID, TBASE, TOFFS, STATUS )
      CALL TCI_PUTID( OFID, OTIMID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  If we have a time list to copy we must shift the event times
      CALL EDI_QFND( IFID(1), 'T', TLNAME, T_AX, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( .NOT. LCOPY(T_AX) ) T_AX = 0
      ELSE
        CALL ERR_ANNUL( STATUS )
        T_AX = 0
      END IF
      IF ( T_AX .GT. 0 ) THEN
        CALL MSG_SETI( 'N', TBASE )
        CALL MSG_PRNT( 'Timing origin taken from dataset ^N' )
      END IF

*    Copy data from input datasets applying offsets to X_CORR, Y_CORR, & RAW_TIMETAG lists

*  Loop over input files
      START = 1
      DO IFILE = 1, NIN

*    Loop over lists
        DO ILIST = 1, NLIST(1)

*      Copying this list?
          IF ( LCOPY(ILIST) ) THEN

*        Get its preferred mapping type
            CALL EDI_MTYPE( LID(ILIST,1), MTYPE, STATUS )

*        Map the input data
            CALL EDI_MAP( IFID(IFILE), LNAME(ILIST), MTYPE, 'READ',
     :                    1, NEVENT(IFILE), IPTR(ILIST), STATUS )

*        Map the output data
            IF ( IFILE .EQ. 1 ) THEN
              CALL EDI_MAP( OFID, LNAME(ILIST), MTYPE, 'WRITE',
     :                      0, 0, OPTR(ILIST), STATUS )
            END IF

*        Move the data
            IF ( MTYPE .EQ. 'DOUBLE' ) THEN
              CALL ARR_COP1D( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OPTR(ILIST) + (START-1)*VAL__NBD), STATUS )
            ELSE IF ( MTYPE .EQ. 'REAL' ) THEN
              CALL ARR_COP1R( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OPTR(ILIST) + (START-1)*VAL__NBR), STATUS )
            ELSE IF ( MTYPE .EQ. 'INTEGER' ) THEN
              CALL ARR_COP1I( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OPTR(ILIST) + (START-1)*VAL__NBI), STATUS )
            END IF

*        Vector quantum?
            IF ( QUANVEC(ILIST) ) THEN

*          Map the input quanta
              CALL EDI_QMAP( IFID(IFILE), LNAME(ILIST), MTYPE, 'READ',
     :                              1, NEVENT(IFILE), IQPTR, STATUS )

*          Map the output quanta
              IF ( IFILE .EQ. 1 ) THEN
                CALL EDI_QMAP( OFID, LNAME(ILIST), MTYPE, 'WRITE',
     :                         0, 0, OQPTR, STATUS )
              END IF

*          Move the quanta
              IF ( MTYPE .EQ. 'DOUBLE' ) THEN
                CALL ARR_COP1D( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OQPTR + (START-1)*VAL__NBD), STATUS )
              ELSE IF ( MTYPE .EQ. 'REAL' ) THEN
                CALL ARR_COP1R( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OQPTR + (START-1)*VAL__NBR), STATUS )
              ELSE IF ( MTYPE .EQ. 'INTEGER' ) THEN
                CALL ARR_COP1I( NEVENT(IFILE), %VAL(IPTR(ILIST)),
     :               %VAL(OQPTR + (START-1)*VAL__NBI), STATUS )
              END IF

*          Unmap the quanta
              CALL EDI_QUNMAP( IFID(IFILE), LNAME(ILIST), STATUS )

            END IF

*        Get field extrema
            CALL ADI_CGET0R( LID(ILIST,IFILE), 'Min', VALUE, STATUS )
            FMIN(ILIST) = MIN( FMIN(ILIST), VALUE )
            CALL ADI_CGET0R( LID(ILIST,IFILE), 'Max', VALUE, STATUS )
            FMAX(ILIST) = MAX( FMAX(ILIST), VALUE )

*        Unmap list
            CALL EDI_UNMAP( IFID(IFILE), LNAME(ILIST), STATUS )

*        Check status
            IF ( STATUS .NE. SAI__OK ) GOTO 99

           END IF

*     Next list to be copied
         END DO

*    Start index position for events in this file
        START = START + NEVENT(IFILE)

*  Next file to be merged
      END DO

*  Release output lists and vector quanta
      DO ILIST = 1, NLIST(1)

*    List being copied?
        IF ( LCOPY(ILIST) ) THEN
          CALL EDI_UNMAP( OFID, LNAME(ILIST), STATUS )
          IF ( QUANVEC(ILIST) ) THEN
            CALL EDI_QUNMAP( OFID, LNAME(ILIST), STATUS )
          END IF
        END IF

*  Next output list
      END DO

*  Update timing and event positions
      START = 1
      DO IFILE = 1, NIN

*    Add the time offset if the current file is not the time reference
*    dataset, the time lists exists, and the time offset is non-zero
        IF ( (IFILE.NE.TBASE) .AND.
     :       (T_AX .GT. 0) .AND.
     :       (TOFFS(IFILE) .NE. 0.0D0 ) ) THEN
          CALL EDI_MAPR( OFID, TLNAME, 'UPDATE', START,
     :                   START + NEVENT(IFILE) - 1, OPTR(T_AX), STATUS )
          CALL ARR_SUMS( '+', NEVENT(IFILE), %VAL(OPTR(T_AX)), 0, 0, 1,
     :                    REAL(TOFFS(IFILE)), 0, 0, NEVENT(IFILE),
     :                    %VAL(OPTR(T_AX)), 0, 0, STATUS )
          CALL EDI_UNMAP( OFID, TLNAME, STATUS )
        END IF

*    Moving events due to pointing mis-match?
        IF ( ANY_EMOVE .AND. (IFILE.GT.1) ) THEN

*      Remap the spatial lists
          CALL EDI_MAPR( OFID, XLNAME, 'UPDATE', START,
     :                   START + NEVENT(IFILE) - 1, OPTR(X_AX), STATUS )
          CALL EDI_MAPR( OFID, YLNAME, 'UPDATE', START,
     :                   START + NEVENT(IFILE) - 1, OPTR(Y_AX), STATUS )

*       Shift events to new centre
           CALL EVMERGE_SHIFT( PIXID(2), PRJID(2), SYSID(2),
     :                         PIXID(1), PRJID(1), SYSID(1),
     :                         NEVENT(IFILE), %VAL(OPTR(X_AX)),
     :                         %VAL(OPTR(Y_AX)), STATUS )

*      Create a list of 4 points, corresponding to the 4 corners defined by
*      the field extrema for the spatial axes. Correct these for the
*      coordinate shift, and adjust FMIN and FMAX.
          FX(1) = FMIN(X_AX)
          FY(1) = FMIN(Y_AX)
          FX(2) = FMIN(X_AX)
          FY(2) = FMAX(Y_AX)
          FX(3) = FMAX(X_AX)
          FY(3) = FMAX(Y_AX)
          FX(4) = FMAX(X_AX)
          FY(4) = FMIN(Y_AX)
          CALL EVMERGE_SHIFT( PIXID(2), PRJID(2), SYSID(2),
     :                        PIXID(1), PRJID(1), SYSID(1),
     :                        4, FX, FY, STATUS )
          FMIN(X_AX) = MIN( FMIN(X_AX), FX(1), FX(2), FX(3), FX(4) )
          FMAX(X_AX) = MAX( FMAX(X_AX), FX(1), FX(2), FX(3), FX(4) )
          FMIN(Y_AX) = MIN( FMIN(Y_AX), FY(1), FY(2), FY(3), FY(4) )
          FMAX(Y_AX) = MAX( FMAX(Y_AX), FY(1), FY(2), FY(3), FY(4) )

*      Release spatial lists
          CALL EDI_UNMAP( OFID, XLNAME, STATUS )
          CALL EDI_UNMAP( OFID, YLNAME, STATUS )

        END IF

*    Start index position for events in this file
        START = START + NEVENT(IFILE)

*  Next file to be merged
      END DO

*  For each list
      DO ILIST = 1, NLIST(1)

*    Update field extrema in output fields
        IF ( LCOPY(ILIST) ) THEN
          CALL ADI_CPUT0R( OLID(ILIST), 'Min', FMIN(ILIST), STATUS )
          CALL ADI_CPUT0R( OLID(ILIST), 'Max', FMAX(ILIST), STATUS )
          CALL EDI_LUPDT( OFID, OLID(ILIST), 'Min,Max', STATUS )
        END IF

*    Release input lists
        DO IFILE = 1, NIN
          IF ( LID(ILIST,IFILE) .NE. ADI__NULLID ) THEN
            CALL ADI_ERASE( LID(ILIST,IFILE), STATUS )
          END IF
        END DO

      END DO

*  Set up history - add in names of datasets merged
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( INLINES, TEXTI, STATUS )
      CALL HSI_PTXT( OFID, INLINES, TEXTI, STATUS )

*  Clean up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVMERGE_SHIFT - Move events from one field centre to another
      SUBROUTINE EVMERGE_SHIFT( PIX1, PRJ1, SYS1, PIX2, PRJ2, SYS2,
     :                                       NUM, XC, YC, STATUS )
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
*
*    Import :
*
      INTEGER			PIX1, PRJ1, SYS1, PIX2, PRJ2, SYS2
      INTEGER                       NUM             ! # of events to do
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
      DOUBLE PRECISION		SYPOS1(2)
      DOUBLE PRECISION		SYPOS(2)

      REAL			APOS(2)		   	! Event position

      INTEGER                   I               	! Loop over events
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over events
      DO I = 1, NUM

*    Make event position
        APOS(1) = XC(I)
        APOS(2) = YC(I)

*    Convert to standard frame
        CALL WCI_CNA2S( APOS, PIX2, PRJ2, SYPOS, STATUS )

*    Convert to frame of dataset 1
        CALL WCI_CNS2S( SYS2, SYPOS, SYS1, SYPOS1, STATUS )

*    Convert to dataset 1 axis units
        CALL WCI_CNS2A( SYPOS1, PIX1, PRJ1, APOS, STATUS )

*    Extract converted position
        XC(I) = APOS(1)
        YC(I) = APOS(2)

*  Next event position
      END DO

      END
