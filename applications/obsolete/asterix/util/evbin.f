      SUBROUTINE EVBIN( STATUS )
*+
*  Name:
*     EVBIN

*  Purpose:
*     Bins N lists into an N dimensional object. [N < = 7]

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EVBIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     An event dataset is binned up into a binned dataset. Each event list
*     selected (except QUALITY) will be binned into an output dimension.
*
*     The binning of each axis may be either regularly or irregularly spaced.
*     If regularly spaced, then bins may be specified either by bin
*     width, or by the number of bins required. Also, each axis may have
*     either increasing or decreasing axis values. This is not controlled by
*     the user, but by the decreasing flag in each list.
*
*     All bins are inclusive of their lower bound and exclusive of their
*     upper bound. Note that if the axis values are decreasing the lower
*     bound will have a larger axis value than the upper bound.
*
*     The way QUALITY lists are handled is controlled by 2 parameters QVAL,
*     and QKEEP. Values present in the quality list > QVAL are treated as bad
*     quality values. If QKEEP is true, then bad events are written to the
*     output data array, and the corresponding element of the output quality
*     array is set to bad (i.e.1). If QKEEP is false, then all bad events are
*     simply ignored, and no output quality array is produced.

*  Usage:
*     evbin {parameter_usage}

*  Environment Parameters:
*     INP            - Name of input EVDS                            (UNIV)
*     LISTS          - Index No(s) of input list(s)                  (_CHAR)
*     OPT1           - Allow irregular binning.                      (_LOGICAL)
*     OPT2           - Select how each regular axis is to be binned  (_LOGICAL)
*     OPT3           - TRUE  = specify all regular axes by No of bins(_LOGICAL)
*                      FALSE = specify all regular axes by bin width
*     QVAL           - Event quality > QVAL = bad                    (_INTEGER)
*     QKEEP          - Produce output QUALITY array?                 (_LOGICAL)
*     REG1..7        - Is this axis to be regularly spaced?          (_LOGICAL)
*     USEBINSIZE1..7 - Is this axis to be specified by bin width?    (_LOGICAL)
*     BASE1..7       - Base value if regular axis                    (_REAL)
*     BINSIZE1..7    - Actual axis bin width                         (_REAL)
*     NBINS1..7      - Actual number of bins required                (_INTEGER)
*     RANGES1..7     - Actual bin limits for irregular bins          (_CHAR)
*     OUT            - Name of output dataset                        (UNIV)
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
*     Limited to 1000 irregular bins per axis. Limit will be less than this
*     if, on average,  more than 15 characters are used to specify each bin -
*     including delimiters.
*     Irregular bins must not overlap.

*  References:
*     {task_references}...

*  Keywords:
*     evbin, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     PLA: Phillip Andrews (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Feb 1988 (PLA):
*        TWODBIN based on IMAGEBIN by Jim Peden. Changes: Counts lists
*        deleted, bin_upper deleted, code simplified, HDS type conversion
*        used on mapping, new calc of default bin sizes, new binning
*        routine. ASTERIX88 structures.
*     13 Apr 1988 (PLA):
*        TWODBIN converted to ASTERIX88
*     20 Apr 1988 (PLA):
*        Original EVENTBIN started
*     15 Apr 1988 (PLA):
*        Extra HISTORY info added
*     14 Oct 1988 (PLA):
*        X_ axis bin inversion removed
*      4 Apr 1989 (PLA):
*        Rewrite to accomodate irregular bins
*     25 Sep 1989 V1.0-7 (PLA):
*        Altered to allow increasing or decreasing axes
*     11 Dec 1989 V1.0-8 (DJA):
*        Put structure definition in EVBIN_STR. Re-written
*        to avoid excessive paging
*     18 Dec 1989 V1.0-9 (DJA):
*        Rationalised EVBIN_BIN7Q to avoid naming list arrays explicitly
*      8 Jan 1990 V1.0-10 (DJA):
*        Assumes 0.0 for QUANTUM if not present in list when
*        the DATA_ARRAY is of floating type, otherwise 1.0
*     17 Jan 1990 V1.0-11 (DJA):
*        Informs user of number of events after binning
*     24 May 1990 V1.2-0 (DJA):
*        Handles datasets with only one event. Explicitly resets axis
*        normalisation.
*     29 Aug 1990 V1.3-0 (DJA):
*        BASE<1..7> parameters added. Quality handling improved
*     20 Sep 1990 V1.3-1 (DJA):
*        BASE option allowed when OPT3 true or false
*     10 Sep 1991 V1.5-0 (DJA):
*        Final re-write. History improved, free all objects
*        and works twice as fast.
*      5 Jun 1992 V1.6-0 (DJA):
*        Fixed bug if irregular axis wasn't first axis
*     25 Feb 1994 V1.7-0 (DJA):
*        Updated quality handling
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Apr 1995 V1.8-1 (DJA):
*        New data interface for output
*     19 Sep 1995 V2.0-0 (DJA):
*        Full ADI port. Removed Fortran structures
*      7 Nov 1995 V2.0-1 (DJA):
*        Allow field extrema to be absent, but warn if so.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER                   SP_SIZE          ! Default # spatial bins
         PARAMETER              ( SP_SIZE = 512 )

      INTEGER                   MX_HTEXT         ! Max lines of history text
         PARAMETER              ( MX_HTEXT = ADI__MXDIM + 7 )

      INTEGER                   MXRANGE          ! 2 x max No of irregular bins
         PARAMETER              ( MXRANGE = 2000 )

      INTEGER			MAXNAX			! Max # axes + 1 for
        PARAMETER		( MAXNAX = ADI__MXDIM+1)! quality

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EVBIN Version 2.2-0' )

*  Local Variables:
      CHARACTER*80              ACTION(MX_HTEXT) 	! History text
      CHARACTER*20		LNAME			! List name string
      CHARACTER*20		OCLASS			! Output object class
      CHARACTER*20		O_NAME(MAXNAX)		! Output axis name
      CHARACTER*40		O_UNITS(MAXNAX)		! Output axis units
      CHARACTER*12              PAR              	! Parameter name
      CHARACTER*80              TEMP             ! Temporary store for LABEL
                                                 ! & UNITS info

      REAL                      ARANGE           ! Axis range, diff of RHS,LHS
      REAL			O_BSIZE(MAXNAX)		! Regular bin widths
      REAL			O_LHS(MAXNAX)		! Lower field extreme
      REAL			O_RHS(MAXNAX)		! Upper field extreme
      REAL			O_QUANT(MAXNAX)		! Quanta
      REAL                      RANGES(MXRANGE,ADI__MXDIM)! Irregular bin ranges.
      REAL			SPARR(2)		! Spaced array values

      INTEGER                   AXPTR            	! Output axis array
      INTEGER			ALEN			! Action length
      INTEGER			ANUM			! List # from EDI_QFND
      INTEGER                   BADQUAL          	! Bad event quality
							! threshold
      INTEGER			BID			! Output BinDS object
      INTEGER                   I, J             	! Loop counters
      INTEGER			LBIN(ADI__MXDIM)	! Lists to bin
      INTEGER			IFID			! Input dataset id
      INTEGER                   NACT             	! # used history lines
      INTEGER                   NEVENT           	! # input events
      INTEGER                   NINDEX           	! # lists selected
      INTEGER                   INLIST           	! # lists in input
      INTEGER			LID			! List identifier
      INTEGER			O_LID(MAXNAX)		! List id per axis
      INTEGER			O_PTR(MAXNAX)		! Mapped list per axis
      INTEGER			OFID			! Output dataset id
      INTEGER                   ONDIM            	! Output dimensionality
      INTEGER                   ODIMS(ADI__MXDIM)	! Output dimensions
      INTEGER                   ONELM            	! # output elements
      INTEGER                   ODPTR            	! Output data array
      INTEGER                   OQPTR            	! Output quality
      INTEGER                   WPTR             	! Output axis widths

      LOGICAL                   OK			! General validity test
      LOGICAL                   USEBINSIZE       ! Users supplies binsize ?
                                                 ! (or No bins)
      LOGICAL                   IRREG            	! Allow irregular binning?
      LOGICAL                   IDVSEL           ! Select how to bin each
                                                 ! regular axis individualy.
      LOGICAL                   ALLNUM           ! If TRUE specify regular axis
                                                 ! by No of bins. Else by width.
      LOGICAL			O_DECR(MAXNAX)		! Axis decreasing?
      LOGICAL			O_REG(MAXNAX)		! Axis regular?
      LOGICAL			O_QOK(MAXNAX)		! Quantum present?
      LOGICAL                   QKEEP            	! Produce o/p quality?
      LOGICAL                   QUALITY          	! QUALITY list present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get event dataset
      CALL USI_ASSOC( 'INP', 'EventDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get number of events and number of lists
      CALL EDI_GETNS( IFID, NEVENT, INLIST, STATUS )
      IF ( INLIST .EQ. 0 ) THEN
        CALL MSG_PRNT( 'Dataset does not contain any valid lists!' )
        GOTO 99
      END IF

*  Display list of list names to user
      CALL MSG_PRNT( 'The available lists are:' )
      CALL EDI_DISP( IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Select the lists
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'Select lists to be binned by entering the '//
     :                                  'index numbers, eg. 1 2 3')
      CALL EDI_SELCT( 'LISTS', INLIST, 1, ADI__MXDIM, LBIN, NINDEX,
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Write chosen lists into structure
      QUALITY = .FALSE.
      ONDIM = 0
      DO I = 1, NINDEX

*    Extract index element
        J = LBIN(I)

*    Index the list
        CALL EDI_IDX( IFID, J, LID, STATUS )

*    Get list name
        CALL ADI_NAME( LID, LNAME, STATUS )

*    Is it the quality list?
        IF ( LNAME .EQ. 'QUALITY' ) THEN

*      We don't bin this list, but store at the end of the binnable lists
          QUALITY = .TRUE.
          O_NAME(NINDEX) = LNAME
          O_LID(NINDEX) = LID

*      Map list DATA_ARRAY
          CALL EDI_MAPI( IFID, LNAME, 'READ', 0, 0, O_PTR(NINDEX),
     :                   STATUS )

        ELSE

          ONDIM = ONDIM + 1
          O_NAME(ONDIM) = LNAME
          O_LID(ONDIM) = LID

*      Map list DATA_ARRAY
          CALL EDI_MAPR( IFID, LNAME, 'READ', 0, 0, O_PTR(ONDIM),
     :                   STATUS )

*      See if UNITS is present
          CALL ADI_THERE( LID, 'Units', OK, STATUS )
          IF ( OK ) THEN
            CALL ADI_CGET0C( LID, 'Units', O_UNITS(ONDIM), STATUS )
          ELSE
            O_UNITS(ONDIM) = ' '
          END IF

*      See if DECREASING is present
          CALL ADI_THERE( LID, 'Decreasing', OK, STATUS )
          IF ( OK ) THEN
            CALL ADI_CGET0L( LID, 'Decreasing', O_DECR(ONDIM), STATUS )
          ELSE
            O_DECR(ONDIM) = .FALSE.
          END IF

        END IF

*    Check status
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END DO

*    See if ONDIM is OK
      IF ( ONDIM .LT. 1 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR: No LIST to bin!', STATUS )
      END IF

*    Obtain user input
      CALL USI_GET0L( 'OPT1', IRREG, STATUS )
      CALL USI_GET0L( 'OPT2', IDVSEL, STATUS )
      CALL USI_GET0L( 'OPT3', ALLNUM, STATUS )
      IF ( QUALITY ) THEN
        CALL USI_GET0I( 'QVAL', BADQUAL, STATUS )
        CALL USI_GET0L( 'QKEEP', QKEEP, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get binning info from user
      DO I = 1, ONDIM

*      Title for the axis
        CALL MSG_BLNK()
        CALL MSG_SETC( 'NAME', O_NAME(I) )
        CALL MSG_PRNT( 'Axis ^NAME :' )

*      Regular bins for this axis?
        IF ( IRREG ) THEN

*        Find out if this axis is to be irregularly binned
          WRITE( PAR, '(A3,I1)' ) 'REG', I
          CALL USI_GET0L( PAR, O_REG(I), STATUS )
          CALL USI_CANCL( PAR, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        ELSE
          O_REG(I) = .TRUE.

        END IF

*      Get field range
        CALL EVBIN_GETRANGE( O_LID(I), O_NAME(I), O_DECR(I), O_QOK(I),
     :                       O_QUANT(I), NEVENT, %VAL(O_PTR(I)),
     :                       O_LHS(I), O_RHS(I), STATUS )

*      Tell user the data range
        CALL MSG_SETR( 'LHS', O_LHS(I) )
        CALL MSG_SETR( 'RHS', O_RHS(I) )
        CALL MSG_SETC( 'UNITS', O_UNITS(I) )
        CALL MSG_PRNT( '  Data range is ^LHS to ^RHS ^UNITS' )

*      and the intrinsic width if present
        IF ( O_QOK(I) ) THEN
          CALL MSG_SETR( 'QUANTUM', O_QUANT(I) )
          CALL MSG_SETC( 'UNITS', O_UNITS(I) )
          CALL MSG_PRNT( '  Intrinsic width is ^QUANTUM ^UNITS' )
        END IF

        IF ( O_REG(I) ) THEN

*        Find out if BINSIZE or number of bins are to be specified
          IF ( IDVSEL ) THEN
            WRITE( PAR, '(A10,I1)' ) 'USEBINSIZE', I
            CALL USI_GET0L( PAR, USEBINSIZE, STATUS )
            CALL USI_CANCL( PAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

          ELSE IF ( ALLNUM ) THEN
            USEBINSIZE = .FALSE.

          ELSE
            USEBINSIZE = .TRUE.

          END IF

*        Is user going to override bin base?
          WRITE( PAR, '(A4,I1)' ) 'BASE', I
          CALL USI_DEF0R( PAR, O_LHS(I), STATUS )
          CALL USI_GET0R( PAR, O_LHS(I), STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Set range based on left and right bounds
          ARANGE = ABS( O_RHS(I) - O_LHS(I) )

          IF ( USEBINSIZE ) THEN

*          Anyone think of a more sensible default?
            O_BSIZE(I) = ARANGE / SP_SIZE
            IF ( O_QOK(I) .AND. (O_BSIZE(I) .LT. O_QUANT(I)) ) THEN
              O_BSIZE(I) = O_QUANT(I)
            END IF

*          Get binsize from user
            WRITE( PAR, '(A7,I1)' ) 'BINSIZE', I
            CALL USI_DEF0R( PAR, O_BSIZE(I), STATUS )
            CALL USI_GET0R( PAR, O_BSIZE(I), STATUS )
            CALL USI_CANCL( PAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

            IF ( O_QOK(I) .AND. (O_BSIZE(I) .LT. O_QUANT(I)) ) THEN
              CALL MSG_PRNT( 'WARNING: Bin size less than QUANTUM' )
            END IF
            ODIMS(I) = NINT( ARANGE / O_BSIZE(I))

            IF ( ODIMS(I) .GT. 1 ) THEN
              CALL MSG_SETI( 'NBIN', ODIMS(I) )
              CALL MSG_SETC( 'NAME', O_NAME(I) )
              CALL MSG_PRNT( 'This will give ^NBIN ^NAME bins' )
            ELSE
              CALL MSG_PRNT( 'WARNING: You have chosen only one bin!' )
              ODIMS(I) = 1
            END IF

          ELSE

            WRITE( PAR, '(A5,I1)' ) 'NBINS', I
            ODIMS(I)  = 512
            O_BSIZE(I) = ARANGE / REAL(ODIMS(I))

            IF ( O_QOK(I) .AND. (O_BSIZE(I) .LT. O_QUANT(I)) ) THEN
              ODIMS(I) = INT(ARANGE / O_QUANT(I) )
            END IF
            CALL USI_DEF0I( PAR, ODIMS(I), STATUS )
            CALL USI_GET0I( PAR, ODIMS(I), STATUS )
            CALL USI_CANCL( PAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

            IF ( ODIMS(I) .LE. 1 ) THEN
              CALL MSG_PRNT( 'WARNING: You have chosen only one bin !')
              ODIMS(I) = 1
            END IF

            O_BSIZE(I) = ARANGE / REAL(ODIMS(I))

            IF ( O_QOK(I) .AND. (O_BSIZE(I) .LT. O_QUANT(I)) ) THEN
              CALL MSG_PRNT( 'WARNING: Bin size less than QUANTUM' )
            END IF
            CALL MSG_SETR( 'BSIZE', O_BSIZE(I) )
            CALL MSG_SETC( 'UNITS', O_UNITS(I) )
            CALL MSG_PRNT( 'This will give a bin width of ^BSIZE '//
     :                                                    '^UNITS' )

          END IF

*        Set sign of bin size
          IF ( O_DECR(I) ) O_BSIZE(I) = - O_BSIZE(I)

*      Irregular axis
        ELSE

          IF ( O_DECR(I) ) THEN
            CALL MSG_PRNT( 'You must specify DECREASING ranges e.g. '//
     :                                                       '30:20:10')
          ELSE
            CALL MSG_PRNT( 'You must specify INCREASING ranges e.g. '//
     :                                                       '10:20:30')
          END IF

*        Get bin boundaries
          WRITE( PAR, '(A6,I1)' ) 'RANGES', I
          CALL PRS_GETRANGES( PAR, MXRANGE, 1, O_RHS(I), O_LHS(I),
     :                            RANGES(1,I), ODIMS(I), STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END IF

      END DO

*  Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Try to guess o/p dataset class my looking at the lists which have been
*  chosen
      OCLASS = 'BinDS'
      IF ( ONDIM .EQ. 1 ) THEN
        CALL EDI_QFND( IFID, 'T', LNAME, ANUM, STATUS )
        IF ( ANUM .EQ. LBIN(1) ) THEN
          OCLASS = 'TimeSeries'
        ELSE
          CALL EDI_QFND( IFID, 'E', LNAME, ANUM, STATUS )
          IF ( ANUM .EQ. LBIN(1) ) THEN
            OCLASS = 'Spectrum'
          END IF
        END IF

      ELSE IF ( ONDIM .EQ. 2 ) THEN
        CALL EDI_QFND( IFID, 'X', LNAME, ANUM, STATUS )
        IF ( ANUM .EQ. LBIN(1) ) THEN
          CALL EDI_QFND( IFID, 'Y', LNAME, ANUM, STATUS )
          IF ( ANUM .EQ. LBIN(2) ) THEN
            OCLASS = 'XYimage'
          END IF
        END IF

      END IF
      CALL USI_DEF0C( 'OUTFORM', OCLASS, STATUS )
      CALL USI_GET0C( 'OUTFORM', OCLASS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Create output dataset
      CALL MSG_BLNK()
      CALL BDI_NEW( OCLASS, ONDIM, ODIMS, 'REAL', BID, STATUS )
      CALL USI_CREAT( 'OUT', BID, OFID, STATUS )

*  Find total number of output elements
      CALL ARR_SUMDIM( ONDIM, ODIMS, ONELM )

*  Map and initialise data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE/ZERO', ODPTR, STATUS )

*  Map quality
      IF ( QUALITY .AND. QKEEP ) THEN
        CALL BDI_MAPUB( OFID, 'Quality', 'WRITE/QMISSING', OQPTR,
     :                  STATUS )
        CALL BDI_PUT0UB( OFID, 'QualityMask', QUAL__MASK, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Loop over AXIS structure writing the values.
      DO I = 1, ONDIM

*    Write text
        CALL BDI_AXPUT0C( OFID, I, 'Label', O_NAME(I), STATUS )
        CALL BDI_AXPUT0C( OFID, I, 'Units', O_UNITS(I), STATUS )
        CALL BDI_AXPUT0L( OFID, I, 'Normalised', .FALSE., STATUS )

*    Write bin characteristics
        IF ( O_REG(I) ) THEN

*      Find leftmost bin centre and signed bin width
          SPARR(1) = O_LHS(I) + O_BSIZE(I)/2.0
          SPARR(2) = O_BSIZE(I)
          CALL BDI_AXPUT1R( OFID, I, 'SpacedData', 2, SPARR, STATUS )

        ELSE

*      Centres and widths if irregular
          CALL BDI_AXMAPR( OFID, I, 'Data', 'WRITE', AXPTR, STATUS )
          CALL BDI_AXMAPR( OFID, I, 'Width', 'WRITE', WPTR, STATUS )

          CALL ARR_BND2CWR( ODIMS(I), RANGES, %VAL(AXPTR),
     :                                %VAL(WPTR), STATUS )

          CALL BDI_AXUNMAP( OFID, I, 'Data', AXPTR, STATUS )
          CALL BDI_AXUNMAP( OFID, I, 'Width', WPTR, STATUS )

        END IF

      END DO

*  Copy header info
      CALL ADI_CGET0C( IFID, 'Title', TEMP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        TEMP = ' '
      END IF
      IF ( TEMP .GT. ' ' ) THEN
        CALL BDI_PUT0C( OFID, 'Title', TEMP, STATUS )
      END IF
      CALL BDI_PUT0C( OFID, 'Label', 'Events', STATUS )

*  Copy the ancillaries
      CALL UDI_COPANC( IFID, ' ', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Pad dimensions with ones to simulate 7D data
      CALL AR7_PAD( ONDIM, ODIMS, STATUS )

*  Bin the data
      CALL EVBIN_INT( ONDIM, O_REG, O_LHS, O_BSIZE, O_DECR, O_PTR,
     :                ODIMS, ODIMS(1), ODIMS(2), ODIMS(3),
     :                ODIMS(4), ODIMS(5), ODIMS(6), ODIMS(7),
     :                NEVENT, RANGES, %VAL(O_PTR(MAXNAX)), BADQUAL,
     :                (QKEEP.AND.QUALITY), %VAL(OQPTR),
     :                %VAL(ODPTR), STATUS )

*    Copy history from input
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*   Write essential data to history

*    ...Input filename
      ACTION(1) = 'Input {INP}'
      NACT = MX_HTEXT
      CALL USI_TEXT( 1, ACTION, NACT, STATUS )

*    ...List names
      NACT = NACT + 1
      ACTION(NACT) = ' '
      NACT = NACT + 1
      IF ( ONDIM .GT. 1 ) THEN
        ACTION(NACT) = 'Dataset created from the lists,'
        DO I = 1, ONDIM
          NACT = NACT + 1
          WRITE( ACTION(NACT), '(15X,A)') O_NAME(I)
        END DO
      ELSE
        ACTION(NACT) = 'Dataset created from the list '//O_NAME(1)
      END IF

*    ...Number of events
      NACT = NACT + 1
      ACTION(NACT) = ' '
      NACT = NACT + 1
      CALL MSG_SETI( 'NEV', NEVENT )
      CALL MSG_MAKE( 'Containing data on ^NEV events', ACTION(NACT),
     :               ALEN )

*    ...Quality processing
      IF ( QUALITY ) THEN
        NACT = NACT + 1
        IF ( QKEEP ) THEN
          ACTION(NACT) = 'QUALITY array created'
        ELSE
          ACTION(NACT) = 'Bad quality events excluded'
        END IF
      END IF

*  Write the text
      CALL HSI_PTXT( OFID, NACT, ACTION, STATUS )

*  Free input lists
      DO I = 1, NINDEX
        CALL EDI_UNMAP( IFID, O_NAME(I), STATUS )
        CALL ADI_ERASE( O_LID(I), STATUS )
      END DO

*  Free output
      CALL USI_ANNUL( 'OUT', STATUS )

*  Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVBIN_GETRANGE - Obtains min, max & range of data
      SUBROUTINE EVBIN_GETRANGE( LID, NAME, DECR, QOK, QUANT, LLEN,
     :                           LDAT, LHS, RHS, STATUS )
*    Description :
*     Obtains FIELD_MIN/MAX & range for the list pointed to by AXN.
*     QUANTUM is taken into account if present.
*    Method :
*     Obtains field_min, field_max from dataset & adjusts using
*     QUANTUM.
*    Deficiencies :
*     Only uses QUANTUM if it is SCALAR.
*    Bugs :
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*
*     23-MAR-1989 : Original (PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			LID			! List identifier
      CHARACTER*(*)		NAME			! List name
      LOGICAL			DECR
      INTEGER			LLEN
      REAL			LDAT(*)
*
*    Export :
*
      LOGICAL			QOK
      REAL			QUANT
      REAL			LHS, RHS		! Field extrema
*
*    Status :
*
      INTEGER                   STATUS
*
*    Local variables :
*
      CHARACTER*8               TYPE			! Basic list type

      REAL			DIR			! Direction vector
      REAL                      FMIN, FMAX       	! Field extrema

      LOGICAL			QVEC			! Vector quantum?
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to get field extrema
      CALL ADI_CGET0R( LID, 'Min', FMIN, STATUS )
      CALL ADI_CGET0R( LID, 'Max', FMAX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_SETC( 'NAME', NAME )
        CALL MSG_PRNT( 'WARNING : Error reading field min/max for '/
     :             /'list ^NAME, data range will be used instead' )
        CALL ARR_RANG1R( LLEN, LDAT, FMIN, FMAX, STATUS )
      END IF

*  Does quantum exist?
      CALL ADI_THERE( LID, 'Quantum', QOK, STATUS )
      IF ( QOK ) THEN

*    Make sure its a scalar quantum
        CALL ADI_CGET0L( LID, 'VectorQuantum', QVEC, STATUS )
        IF ( QVEC ) THEN
          QOK = .FALSE.
        ELSE
          CALL ADI_CGET0R( LID, 'Quantum', QUANT, STATUS )
        END IF

      ELSE
        CALL ADI_CGET0C( LID, 'TYPE', TYPE, STATUS )
        IF ( (TYPE(1:4) .EQ. 'REAL') .OR.
     :       (TYPE(1:6) .EQ. 'DOUBLE') ) THEN
          QUANT = 0.0
        ELSE
          QUANT = 1.0
          QOK = .TRUE.
        END IF
        CALL MSG_SETC( 'LIST', NAME )
        CALL MSG_SETR( 'VAL', QUANT )
        CALL MSG_PRNT( 'WARNING : No QUANTUM component in list '/
     :                              /'^LIST, have assumed ^VAL' )

      END IF

*  Set left and right centres of primitive bins
      IF ( DECR ) THEN
        LHS = FMAX
        RHS = FMIN
      ELSE
        LHS = FMIN
        RHS = FMAX
      END IF

*  Convert from centres to edges if quantum present
      IF ( QOK ) THEN
        IF ( DECR ) THEN
          DIR = -1.0
        ELSE
          DIR = 1.0
        END IF
        LHS = LHS - DIR*QUANT/2.0
        RHS = RHS + DIR*QUANT/2.0
      END IF

*  Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'EVBIN_GETRANGE', STATUS )
      END IF

      END



*+  EVBIN_REGIDX - Return output pixel
      SUBROUTINE EVBIN_REGIDX( LHS, BSIZE, NBIN, IN, VALID, INDEX )
*    Description :
*     Calculates the output pixel value
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*
*     23 Mar 89 :  Original  (PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL			LHS, BSIZE
      INTEGER			NBIN
      REAL                      IN               ! Input value
*
*    Export :
*
      LOGICAL                   VALID            ! Is output pixel valid

      INTEGER                   INDEX            ! Index
*-

      INDEX = 1 + INT((IN - LHS) / BSIZE)
      VALID = (INDEX .LE. NBIN .AND. INDEX .GT. 0)

      END



*+  EVBIN_IRREGIDX - Returns output pixel for irregularly binned axis
      SUBROUTINE EVBIN_IRREGIDX( DECR, START, STOP, INC1, RANGES, IN,
     :                                                 VALID, INDEX )
*    Description :
*    Authors :
*     Phil Andrews (BHVAD::PLA)
*    History :
*
*     23 Mar 89 :  Original (PLA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      LOGICAL			DECR			! Values decreasing?
      INTEGER                   START            ! Initial value for J
      INTEGER                   STOP             ! Last element of RANGES
      INTEGER                   INC1             ! initial value for INC

      REAL                      RANGES(2000)     ! Irregular bins
      REAL                      IN               ! Event value

*    Export :
      LOGICAL                   VALID            ! Is output pixel valid

      INTEGER                   INDEX            ! Index

*    Local variables :
      INTEGER                   J                ! Current search position
      INTEGER                   INC              ! Increment for J

*-
      IF (.NOT. DECR ) THEN
        IF (RANGES(1) .LE. IN .AND. IN .LT. RANGES(STOP)) THEN
          VALID = .TRUE.
          INDEX = 0
          J     = START
          INC   = INC1 * 2

          DO WHILE (INDEX .EQ. 0)
            IF (IN .LT. RANGES(j)) THEN
              J = J - INC

              IF (INC .GT. 2) INC = INC / 2

            ELSE IF (IN .GE. RANGES(J+1)) THEN
              J = J + INC

              IF (INC .GT. 2) INC = INC / 2

            ELSE
              INDEX = 1 + ((J - 1) / 2)

            END IF
          END DO
        ELSE
          VALID = .FALSE.

        END IF
      ELSE
        IF (RANGES(1) .GE. IN .AND. IN .GT. RANGES(STOP)) THEN
          VALID = .TRUE.
          INDEX = 0
          J     = START
          INC   = INC1 * 2

          DO WHILE (INDEX .EQ. 0)
            IF (IN .GT. RANGES(j)) THEN
              J = J - INC

              IF (INC .GT. 2) INC = INC / 2

            ELSE IF (IN .LE. RANGES(J+1)) THEN
              J = J + INC

              IF (INC .GT. 2) INC = INC / 2

            ELSE
              INDEX = 1 + ((J - 1) / 2)

            END IF
          END DO
        ELSE
          VALID = .FALSE.

        END IF
      END IF

      END



*+  EVBIN_INT - Performs binning operation for 7D output with QUALITY.
      SUBROUTINE EVBIN_INT( NDIM, O_REG, O_LHS, O_BSIZE, O_DECR, O_PTR,
     :                      ODIMS, L1, L2, L3, L4, L5, L6, L7,
     :                      EVENTS, RANGES, QIN, BADQUAL, QKEEP,
     :                      QUAL, OUT, STATUS )
*    Description :
*     Bins the event information in IN and writes it to OUT
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan ( BHVAD::DJA )
*
*    History :
*
*      11 Dec 89 : Original (DJA)
*      6  Jul 92 : removed intrinsic fn so that it compiles on SUN (RDS)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                   NDIM             ! Output dimensionality
      LOGICAL			O_REG(*)		! Axis is regular?
      REAL			O_LHS(*)		! 1st bin lower bounds
      REAL			O_BSIZE(*)		! Regular bin widths
      LOGICAL			O_DECR(*)
      INTEGER			O_PTR(*)
      INTEGER			ODIMS(NDIM)		! Dimensions array
      INTEGER                   L1,L2,L3,L4,L5,L6,L7
      INTEGER                   EVENTS           ! Number of events in input lists
      INTEGER                   BADQUAL          ! Ignore events with quality
                                                 ! > this value.
      INTEGER                   QIN(EVENTS)      ! QUALITY list data

      REAL                      RANGES(2000,ADI__MXDIM)   ! Irregular bins

      LOGICAL                   QKEEP            ! Keep bad quality events?
*
*    Export :
*
      BYTE                      QUAL(L1,L2,L3,L4,L5,L6,L7)
      REAL                      OUT (L1,L2,L3,L4,L5,L6,L7)

*    Status:
      INTEGER			STATUS

*    Local Variables :
      INTEGER                   I,J              ! loop counters
      INTEGER                   BN(ADI__MXDIM)   ! Index to OUT
      INTEGER                   START(ADI__MXDIM)! Start bin for search if irreg
      INTEGER                   INC1(ADI__MXDIM) ! Initial increment for search
      INTEGER                   STOP(ADI__MXDIM) ! No of elements in RANGES(x,n)
      INTEGER                   TBIN             ! Total bin counter
      LOGICAL                   ALL_REG          ! All axes regular?
      LOGICAL                   VALID            ! Has a valid bin been found?
*
*    Inline functions :
*
      INTEGER                   EVD
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Boundaries for irregular axes
      ALL_REG = .TRUE.
      DO I = 1, NDIM
        IF ( .NOT. O_REG(I) ) THEN
          START(I) = 1 + (2 * ((ODIMS(I) / 2) - 1))
          STOP(I) = 2 * ODIMS(I)
          INC1(I) = START (I) / 2
          ALL_REG = .FALSE.
        END IF
      END DO

*  Initialise BN array for dummy dimensions
      CALL AR7_PAD( NDIM, BN, STATUS )

*  Initialise total bin counter
      TBIN = 0

*  Loop over the event lists
      IF ( QKEEP ) THEN

        IF ( ALL_REG ) THEN

          DO I = 1, EVENTS

*          Find bin on each dimension
            DO J = 1, NDIM
              EVD = O_PTR(J) + (I-1)*VAL__NBR
              CALL EVBIN_REGIDX( O_LHS(J), O_BSIZE(J), ODIMS(J),
     :                           %VAL(EVD), VALID, BN(J) )
              IF ( .NOT. VALID ) GOTO 10
            END DO

*          Bump up bin counter
            OUT(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7)) =
     :           OUT(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7))+1.0
            TBIN = TBIN + 1
            IF ( QIN(I) .GT. BADQUAL ) THEN
              QUAL(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7)) =
     :                QUAL__BAD
            END IF

 10         CONTINUE

          END DO

        ELSE

          DO I = 1, EVENTS

*          Find bin on each dimension
            DO J = 1, NDIM

              EVD = O_PTR(J) + (I-1)*VAL__NBR
              IF ( O_REG(J) ) THEN
                CALL EVBIN_REGIDX( O_LHS(J), O_BSIZE(J), ODIMS(J),
     :                             %VAL(EVD), VALID, BN(J) )
              ELSE
                CALL EVBIN_IRREGIDX( O_DECR(J), START(J),STOP(J),
     :                  INC1(J),RANGES(1,J), %VAL(EVD), VALID, BN(J) )
              END IF
              IF ( .NOT. VALID ) GOTO 20

            END DO

*          Bump up bin counter
            OUT(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7)) =
     :           OUT(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7))+1.0
            QUAL(BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7)) =
     :              QUAL__GOOD
            TBIN = TBIN + 1

 20         CONTINUE

          END DO

        END IF

      ELSE

        IF ( ALL_REG ) THEN

          DO I = 1, EVENTS

*          Find bin on each dimension
            DO J = 1, NDIM
              EVD = O_PTR(J) + (I-1)*VAL__NBR
              CALL EVBIN_REGIDX( O_LHS(J), O_BSIZE(J), ODIMS(J),
     :                             %VAL(EVD), VALID, BN(J) )
              IF ( .NOT. VALID ) GOTO 30
            END DO

*          Bump up bin counter
            OUT( BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7) ) =
     :         OUT( BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7) )+1.0
            TBIN = TBIN + 1

 30         CONTINUE

          END DO

        ELSE

          DO I = 1, EVENTS

*          Find bin on each dimension
            DO J = 1, NDIM

              EVD = O_PTR(J) + (I-1)*VAL__NBR
              IF ( O_REG(J) ) THEN
                CALL EVBIN_REGIDX( O_LHS(J), O_BSIZE(J), ODIMS(J),
     :                             %VAL(EVD), VALID, BN(J) )
              ELSE
                CALL EVBIN_IRREGIDX( O_DECR(J), START(J),STOP(J),
     :                  INC1(J),RANGES(1,J), %VAL(EVD), VALID, BN(J) )
              END IF
              IF ( .NOT. VALID ) GOTO 40

            END DO

*          Bump up bin counter
            OUT( BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7) ) =
     :            OUT( BN(1),BN(2),BN(3),BN(4),BN(5),BN(6),BN(7) )+1.0
            TBIN = TBIN + 1

 40         CONTINUE

          END DO

        END IF

      END IF

      IF ( TBIN .NE. 0 ) THEN
        CALL MSG_SETI( 'N', TBIN )
        IF ( TBIN .NE. EVENTS ) THEN
          CALL MSG_SETI( 'NP', EVENTS )
          CALL MSG_PRNT( 'A total of ^N events were binned out'/
     :                                        /' of ^NP input' )
        ELSE
          CALL MSG_PRNT( 'A total of ^N events were binned' )
        END IF
      END IF

      END
