*+  EVBIN - Bins N lists into an N dimensional object. [N < = 7]
      SUBROUTINE EVBIN( STATUS )
*
*    Description :
*
*     An event dataset is binned up into an NDF. Each EVENT LIST selected
*     (except QUALITY) will be binned into an output dimension.
*
*     The binning of EACH axis may be either REGULARLY or IRREGULARLY spaced.
*     If regularly spaced, then bins may be specified EITHER by bin
*     width, OR by the number of bins required.
*     Also, each axis may have either increasing or decreasing axis values.
*     This is NOT controlled by the user, but by the DECREASING flag in each
*     list.
*
*     All bins are INCLUSIVE of their lower bound and EXCLUSIVE of
*     their upper bound. Note that if the axis values are decreasing the
*     lower bound will have a larger axis value than the upper bound.
*
*     The way QUALITY lists are handled is controlled by 2 parameters QVAL,
*     and QKEEP. Values present in the quality list > QVAL are treated as bad
*     quality values. If QKEEP is true, then bad events are written to the
*     output DATA_ARRAY, and the corresponding element of the output QUALITY
*     array is set to bad (i.e.1). If QKEEP is false, then all bad events are
*     simply ignored, and no output QUALITY array is produced.
*
*    Parameters :
*
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
*
*    Method :
*     Obtain LISTs to be binned
*     Obtain binning information
*     Loop over EVDS binning the data
*
*    Deficiencies :
*     Limited to 1000 irregular bins per axis. Limit will be less than this
*     if, on average,  more than 15 characters are used to specify each bin -
*     including delimiters.
*     Irregular bins must not overlap.
*
*    Bugs :
*    Authors :
*
*     Phillip Andrews (BHVAD::PLA)
*     David J. Allan  (BHVAD::DJA)
*
*    Ancient History :
*
*     29 Feb 88: TWODBIN based on IMAGEBIN by Jim Peden.
*                Changes: Counts lists deleted, bin_upper
*                deleted, code simplified, HDS type conversion
*                used on mapping, new calc of default bin sizes,
*                new binning routine. ASTERIX88 structures.
*     13 Apr 88: TWODBIN converted to ASTERIX88
*
*    History :
*
*     20 Apr 88 : Original EVENTBIN started (PLA)
*     15 Apr 88 : Extra HISTORY info added (PLA)
*     14 Oct 88 : X_ axis bin inversion removed (PLA)
*      4 Apr 89 : Rewrite to accomodate irregular bins (PLA)
*     25 Sep 89 : V1.0-7  Altered to allow increasing or decreasing axes (PLA)
*     11 Dec 89 : V1.0-8  Put structure definition in EVBIN_STR. Re-written
*                         to avoid excessive paging. (BHVAD::DJA)
*     18 Dec 89 : V1.0-9  Rationalised EVBIN_BIN7Q to avoid naming list arrays
*                         explicitly (BHVAD::DJA)
*      8 Jan 90 : V1.0-10 Assumes 0.0 for QUANTUM if not present in list when
*                         the DATA_ARRAY is of floating type, otherwise 1.0
*                         Should be removed after EDA_ routines there (DJA)
*     17 Jan 90 : V1.0-11 Informs user of number of events after binning (DJA)
*     24 May 90 : V1.2-0  Handles datasets with only one event. Explicitly
*                         resets axis normalisation. (DJA)
*     29 Aug 90 : V1.3-0  BASE<1..7> parameters added. Quality handling
*                         improved (DJA)
*     20 Sep 90 : V1.3-1  BASE option allowed when OPT3 true or false (DJA)
*     10 Sep 91 : V1.5-0  Final re-write. History improved, free all objects
*                         and works twice as fast. (DJA)
*      5 Jun 92 : V1.6-0  Fixed bug if irregular axis wasn't first axis (DJA)
*     25 Feb 94 : V1.7-0  Updated quality handling (DJA)
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
      INCLUDE 'LIST_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Strucure definitions:
*
      INCLUDE 'EVBIN_STR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER                   SP_SIZE          ! Default # spatial bins
         PARAMETER              ( SP_SIZE = 512 )

      INTEGER                   MX_HTEXT         ! Max lines of history text
         PARAMETER              ( MX_HTEXT = DAT__MXDIM + 7 )

      INTEGER                   MXRANGE          ! 2 x max No of irregular bins
         PARAMETER              ( MXRANGE = 2000 )
*
*    Local variables :
*
      RECORD /ORDINATE/         D(8)             ! Axis and chosen list info

      CHARACTER*(DAT__SZTYP)    DATASET          ! TYPE of output dataset.
      CHARACTER*(DAT__SZLOC)    ILOC             ! Locator to input dataset
      CHARACTER*(DAT__SZLOC)    LLOC(LIST__MXNL) ! Locator to lists in the evds
      CHARACTER*(DAT__SZNAM)    NAME(LIST__MXNL) ! Names of lists in input
      CHARACTER*(DAT__SZLOC)    OLOC             ! Locator to output image

      CHARACTER*80              ACTION(MX_HTEXT) ! History text
      CHARACTER*80              AXLABEL(DAT__MXDIM) ! Axis labels for UTIL_GETTYPE
      CHARACTER*12              PAR              ! Parameter name
      CHARACTER*80              TEMP             ! Temporary store for LABEL
                                                 ! & UNITS info

      REAL                      ARANGE           ! Axis range, diff of RHS,LHS
      REAL                      RANGES(MXRANGE,7)! Irregular bin ranges.

      INTEGER                   AXPTR            ! Pointer to Axis array
      INTEGER                   BADQUAL          ! Exclude events with quality
                                                 ! > this value.
      INTEGER                   I, J             ! Loop counters
      INTEGER                   INDEX(LIST__MXNL)! Index number of selected lists
      INTEGER                   NACT             ! # of history lines used
      INTEGER                   NEVENT           ! # input events
      INTEGER                   NINDEX           ! Number of lists selected.
      INTEGER                   INLIST           ! Number of lists in input
                                                 ! EVDS
      INTEGER                   ONDIM            ! Output dimensionality
      INTEGER                   ODIMS(DAT__MXDIM)! Dimensions of output image
      INTEGER                   ONELM            ! Total no. of output elements
      INTEGER                   ODPTR            ! Pointer to output data_array
      INTEGER                   OQPTR            ! Pointer to output quality
      INTEGER                   WPTR             ! Pointer to axis width array

      LOGICAL                   OK
      LOGICAL                   USEBINSIZE       ! Users supplies binsize ?
                                                 ! (or No bins)
      LOGICAL                   INPRIM           ! Input object primitive?
      LOGICAL                   IRREG            ! Allow irregular binning?
      LOGICAL                   IDVSEL           ! Select how to bin each
                                                 ! regular axis individualy.
      LOGICAL                   ALLNUM           ! If TRUE specify regular axis
                                                 ! by No of bins. Else by width.
      LOGICAL                   QKEEP            ! If true produce output
                                                 ! quality array.
      LOGICAL                   QUALITY          ! Is QUALITY list present?
*
*    Version id :
*
      CHARACTER*24              VERSION
         PARAMETER              ( VERSION = 'EVBIN Version 1.8-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version anouncement
      CALL MSG_PRNT( VERSION )

*    Initialize ASTERIX
      CALL AST_INIT

*    Get event dataset
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Input is not an event dataset' )
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find all valid lists in INP. Display them to user.
      CALL MSG_PRNT( 'The available lists are:' )
      CALL LIST_FINDALLOK(ILOC, .TRUE., LLOC, NAME, INLIST, NEVENT,
     :                                                     STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Select the lists
      CALL MSG_BLNK()
      CALL MSG_PRNT( 'Select lists to be binned by entering the '//
     :                                  'index numbers, eg. 1 2 3')
      CALL PRS_GETLIST( 'LISTS', INLIST, INDEX, NINDEX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check that at leaset one list has been selected
      IF ( NINDEX .LT. 1 ) THEN
        CALL MSG_PRNT('FATAL ERROR: No selection made')
        STATUS = SAI__ERROR
      ELSE IF ( NINDEX .GT. (DAT__MXDIM+1) ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: Too many selections made' )
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write chosen lists into structure
      QUALITY = .FALSE.
      ONDIM = 0
      DO I = 1, NINDEX

        J=INDEX(I)

        IF ( NAME(J) .EQ. 'QUALITY' ) THEN
          QUALITY = .TRUE.
          D(NINDEX).LLOC = LLOC(J)
          D(NINDEX).NAME = NAME(J)

*        Map list DATA_ARRAY
          CALL CMP_MAPV( D(NINDEX).LLOC, 'DATA_ARRAY', '_INTEGER',
     :                     'READ', D(NINDEX).PTR, NEVENT, STATUS )

        ELSE

          ONDIM = ONDIM + 1
          D(ONDIM).LLOC = LLOC(J)
          D(ONDIM).NAME = NAME(J)

*        Map list DATA_ARRAY
          CALL CMP_MAPV( D(ONDIM).LLOC, 'DATA_ARRAY', '_REAL', 'READ',
     :                                  D(ONDIM).PTR, NEVENT, STATUS )

*        See if UNITS is present
          CALL HDX_OK( LLOC(J), 'UNITS', OK, STATUS )
          IF ( OK ) THEN
            CALL CMP_GET0C( LLOC(J), 'UNITS', D(ONDIM).UNITS, STATUS )
          ELSE
            D(ONDIM).UNITS = ' '
          END IF

*        See if DECREASING is present
          CALL HDX_OK( LLOC(J), 'DECREASING', OK, STATUS )
          IF ( OK ) THEN
            CALL CMP_GET0L( LLOC(J), 'DECREASING', D(ONDIM).DECREASING,
     :                                                         STATUS )
          ELSE
            D(ONDIM).DECREASING = .FALSE.
          END IF
          IF ( D(ONDIM).DECREASING ) THEN
            D(ONDIM).DIR = -1.0
          ELSE
            D(ONDIM).DIR = 1.0
          END IF

        END IF

*      Check status
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END DO

*    See if ONDIM is OK
      IF ( ONDIM .LT. 1 ) THEN
        CALL MSG_PRNT( 'FATAL ERROR: No LIST to bin!' )
        STATUS = SAI__ERROR
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
        CALL MSG_SETC( 'NAME', D(I).NAME )
        CALL MSG_PRNT( 'Axis ^NAME :' )

*      Regular bins for this axis?
        IF ( IRREG ) THEN

*        Find out if this axis is to be irregularly binned
          WRITE( PAR, '(A3,I1)' ) 'REG', I
          CALL USI_GET0L( PAR, D(I).REG, STATUS )
          CALL USI_CANCL( PAR, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        ELSE
          D(I).REG = .TRUE.

        END IF

*      Get field range
        CALL EVBIN_GETRANGE( I, D, STATUS )

*      Tell user the data range
        CALL MSG_SETR( 'LHS', D(I).LHS )
        CALL MSG_SETR( 'RHS', D(I).RHS )
        CALL MSG_SETC( 'UNITS', D(I).UNITS )
        CALL MSG_PRNT( '  Data range is ^LHS to ^RHS ^UNITS' )

*      and the intrinsic width if present
        IF ( D(I).QOK ) THEN
          CALL MSG_SETR( 'QUANTUM', D(I).QUANTUM )
          CALL MSG_SETC( 'UNITS', D(I).UNITS )
          CALL MSG_PRNT( '  Intrinsic width is ^QUANTUM ^UNITS' )
        END IF

        IF ( D(I).REG ) THEN

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
          CALL USI_DEF0R( PAR, D(I).LHS, STATUS )
          CALL USI_GET0R( PAR, D(I).LHS, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Set range based on left and right bounds
          ARANGE = ABS( D(I).RHS - D(I).LHS )

          IF ( USEBINSIZE ) THEN

*          Anyone think of a more sensible default?
            D(I).BSIZE = ARANGE / SP_SIZE
            IF (D(I).QOK .AND. (D(I).BSIZE .LT. D(I).QUANTUM)) THEN
              D(I).BSIZE = D(I).QUANTUM
            END IF

*          Get binsize from user
            WRITE( PAR, '(A7,I1)' ) 'BINSIZE', I
            CALL USI_DEF0R( PAR, D(I).BSIZE, STATUS )
            CALL USI_GET0R( PAR, D(I).BSIZE, STATUS )
            CALL USI_CANCL( PAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

            IF (D(I).QOK .AND. (D(I).BSIZE .LT. D(I).QUANTUM)) THEN
              CALL MSG_PRNT( 'WARNING: Bin size less than QUANTUM' )
            END IF
            D(I).NBIN = NINT( ARANGE / D(I).BSIZE)

            IF ( D(I).NBIN .GT. 1 ) THEN
              CALL MSG_SETI( 'NBIN', D(I).NBIN )
              CALL MSG_SETC( 'NAME', D(I).NAME )
              CALL MSG_PRNT( 'This will give ^NBIN ^NAME bins' )
            ELSE
              CALL MSG_PRNT( 'WARNING: You have chosen only one bin!' )
              D(I).NBIN = 1
            END IF

          ELSE

            WRITE( PAR, '(A5,I1)' ) 'NBINS', I
            D(I).NBIN  = 512
            D(I).BSIZE = ARANGE / REAL(D(I).NBIN)

            IF (D(I).QOK .AND. (D(I).BSIZE .LT. D(I).QUANTUM)) THEN
              D(I).NBIN = INT(ARANGE / D(I).QUANTUM)
            END IF
            CALL USI_DEF0I( PAR, D(I).NBIN, STATUS )
            CALL USI_GET0I( PAR, D(I).NBIN, STATUS )
            CALL USI_CANCL( PAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99

            IF ( D(I).NBIN .LE. 1 ) THEN
              CALL MSG_PRNT( 'WARNING: You have chosen only one bin !')
              D(I).NBIN = 1
            END IF

            D(I).BSIZE = ARANGE / REAL(D(I).NBIN)

            IF (D(I).QOK .AND. (D(I).BSIZE .LT. D(I).QUANTUM)) THEN
              CALL MSG_PRNT( 'WARNING: Bin size less than QUANTUM' )
            END IF
            CALL MSG_SETR( 'BSIZE', D(I).BSIZE )
            CALL MSG_SETC( 'UNITS', D(I).UNITS )
            CALL MSG_PRNT( 'This will give a bin width of ^BSIZE '//
     :                                                    '^UNITS' )

          END IF

*        Set sign of bin size
          D(I).BSIZE = D(I).BSIZE * D(I).DIR

*      Irregular axis
        ELSE

          IF ( D(I).DECREASING ) THEN
            CALL MSG_PRNT( 'You must specify DECREASING ranges e.g. '//
     :                                                       '30:20:10')
          ELSE
            CALL MSG_PRNT( 'You must specify INCREASING ranges e.g. '//
     :                                                       '10:20:30')
          END IF

*        Get bin boundaries
          WRITE( PAR, '(A6,I1)' ) 'RANGES', I
          CALL PRS_GETRANGES( PAR, MXRANGE, 1, D(I).RHS, D(I).LHS,
     :                            RANGES(1,I), D(I).NBIN, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

        END IF

      END DO

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set up AXLABEL and ODIMS arrays.
      DO I = 1, ONDIM
        AXLABEL(I) = D(I).NAME
        ODIMS(I) = D(I).NBIN
      END DO

*    Determine type for output dataset.
      CALL UTIL_GETTYPE( ONDIM, AXLABEL, DATASET, STATUS )

*    Create output dataset
      CALL MSG_BLNK()
      CALL USI_ASSOCO( 'OUT', DATASET, OLOC, STATUS )

*    Create, fill, or map components
      CALL BDA_CREDATA( OLOC, ONDIM, ODIMS, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )

*    Create & map QUALITY
      IF ( QUALITY .AND. QKEEP ) THEN
        CALL BDA_CREQUAL( OLOC, ONDIM, ODIMS, STATUS )
        CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
        CALL BDA_PUTMASK( OLOC, QUAL__MASK, STATUS )
      END IF

*    Create AXIS structure
      CALL BDA_CREAXES( OLOC, ONDIM, STATUS )

*    Loop over AXIS structure writing the values.
      DO I = 1, ONDIM

*      Write text
        CALL BDA_PUTAXLABEL( OLOC, I, D(I).NAME,  STATUS)
        CALL BDA_PUTAXUNITS( OLOC, I, D(I).UNITS, STATUS)
        CALL BDA_PUTAXNORM( OLOC, I, .FALSE., STATUS )

*      Write bin characteristics
        IF ( D(I).REG ) THEN

*        Find leftmost bin centre and signed bin width
          CALL BDA_PUTAXVAL( OLOC, I, D(I).LHS + D(I).BSIZE/2.0,
     :                           D(I).BSIZE, D(I).NBIN, STATUS )

        ELSE

*        Centres and widths if irregular
          CALL BDA_CREAXVAL( OLOC, I, .FALSE., D(I).NBIN, STATUS )
          CALL BDA_CREAXWID( OLOC, I, .FALSE., D(I).NBIN, STATUS )
          CALL BDA_MAPAXVAL( OLOC, 'WRITE', I, AXPTR, STATUS )
          CALL BDA_MAPAXWID( OLOC, 'WRITE', I, WPTR, STATUS )

          CALL AXIS_RNG2VALW( D(I).NBIN, RANGES, %VAL(AXPTR),
     :                                   %VAL(WPTR), STATUS )

          CALL BDA_UNMAPAXVAL( OLOC, I, STATUS )
          CALL BDA_UNMAPAXWID( OLOC, I, STATUS )

        END IF

      END DO

*    Copy header info
      CALL BDA_GETTITLE( ILOC, TEMP, STATUS )
      IF ( TEMP .GT. ' ' ) THEN
        CALL BDA_PUTTITLE( OLOC, TEMP, STATUS )
      END IF
      CALL BDA_PUTLABEL( OLOC, 'Events', STATUS )

*    Copy the MORE structure
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find total number of output elements
      CALL ARR_SUMDIM( ONDIM, ODIMS, ONELM )

*    Initialise output array
      CALL ARR_INIT1R( 0.0, ONELM, %VAL(ODPTR), STATUS )

*    ...and quality if needed
      IF ( QUALITY ) THEN
        CALL ARR_INIT1B( QUAL__MISSING, ONELM, %VAL(OQPTR), STATUS )
      END IF

*    Pad dimensions with ones to simulate 7D data
      IF ( ONDIM .LT. DAT__MXDIM ) THEN
        DO I = ONDIM+1, DAT__MXDIM
          D(I).NBIN = 1
        END DO
      END IF

*    Bin the data
      CALL EVBIN_INT( D, ONDIM, D(1).NBIN, D(2).NBIN, D(3).NBIN,
     :               D(4).NBIN, D(5).NBIN, D(6).NBIN, D(7).NBIN,
     :               NEVENT, RANGES, %VAL(D(8).PTR), BADQUAL,
     :               (QKEEP.AND.QUALITY ), %VAL(OQPTR), %VAL(ODPTR) )

*    Copy history from input
      CALL HIST_COPY( ILOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )

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
          WRITE( ACTION(NACT), '(15X,A)') D(I).NAME
        END DO
      ELSE
        ACTION(NACT) = 'Dataset created from the list '//D(1).NAME
      END IF

*    ...Number of events
      NACT = NACT + 1
      ACTION(NACT) = ' '
      NACT = NACT + 1
      WRITE( ACTION(NACT), '(A,I,A)') 'Containing data on ',
     :                                                NEVENT, ' events'

*    ...Quality processing
      IF ( QUALITY ) THEN
        NACT = NACT + 1
        IF ( QKEEP ) THEN
          ACTION(NACT) = 'QUALITY array created'
        ELSE
          ACTION(NACT) = 'Bad quality events excluded'
        END IF
      END IF

*    Write the text
      CALL HIST_PTXT( OLOC, NACT, ACTION, STATUS )

*    Free input lists
      DO I = 1, NINDEX
        CALL CMP_UNMAP( D(I).LLOC, 'DATA_ARRAY', STATUS )
        CALL DAT_ANNUL( D(I).LLOC, STATUS )
      END DO

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  EVBIN_GETRANGE - Obtains min, max & range of data
      SUBROUTINE EVBIN_GETRANGE( AXN, D, STATUS )
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
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'EVBIN_STR'
*
*    Import :
*
      INTEGER                   AXN              ! Axis number
*
*    Import-Export :
*
      RECORD /ORDINATE/         D(DAT__MXDIM)    ! Info on lists
*
*    Status :
*
      INTEGER                   STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZTYP)    TYPE             ! Type of data_array

      REAL                      FMIN, FMAX       ! FIELD_MIn and MAX values

      INTEGER                   QNELM            ! Number of QUANTUM elements
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL LIST_GFLDR( D(AXN).LLOC, FMIN, FMAX, STATUS )
      CALL HDX_OK( D(AXN).LLOC, 'QUANTUM', D(AXN).QOK, STATUS )

      IF ( D(AXN).QOK ) THEN
        CALL CMP_SIZE( D(AXN).LLOC, 'QUANTUM', QNELM, STATUS )
        IF ( QNELM .EQ. 1 ) THEN
          CALL CMP_GET0R( D(AXN).LLOC, 'QUANTUM', D(AXN).QUANTUM,
     :                                                   STATUS )
        END IF

      ELSE
        CALL MSG_SETC( 'LIST', D(AXN).NAME )
        CALL CMP_TYPE( D(AXN).LLOC, 'DATA_ARRAY', TYPE, STATUS )
        IF ( ( TYPE(1:5) .EQ. '_REAL' ) .OR.
     :       ( TYPE(1:7) .EQ. '_DOUBLE' ) ) THEN
          D(AXN).QUANTUM = 0.0
        ELSE
          D(AXN).QUANTUM = 1.0
        END IF
        CALL MSG_SETR( 'VAL', D(AXN).QUANTUM )
        CALL MSG_PRNT( 'WARNING : No QUANTUM component in list '/
     :                              /'^LIST, have assumed ^VAL' )

      END IF

*    Set left and right centres of primitive bins
      IF ( D(AXN).DECREASING ) THEN
        D(AXN).LHS = FMAX
        D(AXN).RHS = FMIN
      ELSE
        D(AXN).LHS = FMIN
        D(AXN).RHS = FMAX
      END IF

*    Convert from centres to edges if quantum present
      IF ( D(AXN).QOK ) THEN
        D(AXN).LHS = D(AXN).LHS - D(AXN).DIR*D(AXN).QUANTUM/2.0
        D(AXN).RHS = D(AXN).RHS + D(AXN).DIR*D(AXN).QUANTUM/2.0
      END IF

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from EVBIN_GETRANGE', STATUS )
      END IF

      END



*+  EVBIN_REGIDX - Return output pixel
      SUBROUTINE EVBIN_REGIDX (D, AXN, IN, VALID, INDEX)
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
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'EVBIN_STR'
*
*    Import :
*
      RECORD /ORDINATE/         D (DAT__MXDIM)   ! Info on lists

      INTEGER                   AXN              ! Output axis

      REAL                      IN               ! Input value
*
*    Export :
*
      LOGICAL                   VALID            ! Is output pixel valid

      INTEGER                   INDEX            ! Index
*-

      INDEX = 1 + INT((IN - D(AXN).LHS) / D(AXN).BSIZE)
      VALID = (INDEX .LE. D(AXN).NBIN .AND. INDEX .GT. 0)

      END



*+  EVBIN_IRREGIDX - Returns output pixel for irregularly binned axis
      SUBROUTINE EVBIN_IRREGIDX( D, AXN, START, STOP, INC1, RANGES, IN,
     :                                                   VALID, INDEX )
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
      INCLUDE 'DAT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'EVBIN_STR'
*
*    Import :
*
      RECORD /ORDINATE/         D (DAT__MXDIM)   ! Info on lists

      INTEGER                   AXN              ! Output axis
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
      IF (.NOT. D(AXN).DECREASING) THEN
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
      SUBROUTINE EVBIN_INT( D, NDIM, L1, L2, L3, L4, L5, L6, L7,
     :               EVENTS, RANGES, QIN, BADQUAL, QKEEP, QUAL, OUT )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Structure definitions :
*
      INCLUDE 'EVBIN_STR'
*
*    Import :
*
      RECORD /ORDINATE/         D(DAT__MXDIM)    ! Info on lists

      INTEGER                   NDIM             ! Output dimensionality
      INTEGER                   L1,L2,L3,L4,L5,L6,L7
      INTEGER                   EVENTS           ! Number of events in input lists
      INTEGER                   BADQUAL          ! Ignore events with quality
                                                 ! > this value.
      INTEGER                   QIN(EVENTS)      ! QUALITY list data

      REAL                      RANGES(2000,DAT__MXDIM)   ! Irregular bins

      LOGICAL                   QKEEP            ! Keep bad quality events?
*
*    Export :
*
      BYTE                      QUAL(L1,L2,L3,L4,L5,L6,L7)
      REAL                      OUT (L1,L2,L3,L4,L5,L6,L7)
*
*    Local variables :
*
      INTEGER                   I,J              ! loop counters
      INTEGER                   BN(DAT__MXDIM)   ! Index to OUT
      INTEGER                   START(DAT__MXDIM)! Start bin for search if irreg
      INTEGER                   INC1(DAT__MXDIM) ! Initial increment for search
      INTEGER                   STOP(DAT__MXDIM) ! No of elements in RANGES(x,n)
      INTEGER                   TBIN             ! Total bin counter
      LOGICAL                   ALL_REG          ! All axes regular?
      LOGICAL                   VALID            ! Has a valid bin been found?
*
*    Inline functions :
*
      INTEGER                   EVD
*-

*    Boundaries for irregular axes
      ALL_REG = .TRUE.
      DO I = 1, NDIM
        IF ( .NOT. D(I).REG ) THEN
          START(I) = 1 + (2 * ((D(I).NBIN / 2) - 1))
          STOP(I) = 2 * D(I).NBIN
          INC1(I) = START (I) / 2
          ALL_REG = .FALSE.
        END IF
      END DO

*    Initialise BN array for dummy dimensions
      IF ( NDIM .LT. DAT__MXDIM ) THEN
        DO I = NDIM+1, DAT__MXDIM
          BN(I) = 1
        END DO
      END IF

*    Initialise total bin counter
      TBIN = 0

*    Loop over the event lists
      IF ( QKEEP ) THEN

        IF ( ALL_REG ) THEN

          DO I = 1, EVENTS

*          Find bin on each dimension
            DO J = 1, NDIM
              EVD = D(J).PTR + (I-1)*VAL__NBR
              CALL EVBIN_REGIDX( D, J, %VAL(EVD), VALID, BN(J) )
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

              EVD = D(J).PTR + (I-1)*VAL__NBR
              IF ( D(J).REG ) THEN
                CALL EVBIN_REGIDX(D, J, %VAL(EVD), VALID, BN(J) )
              ELSE
                CALL EVBIN_IRREGIDX(D, J, START(J),STOP(J),INC1(J),
     :                  RANGES(1,J), %VAL(EVD), VALID, BN(J) )
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
              EVD = D(J).PTR + (I-1)*VAL__NBR
              CALL EVBIN_REGIDX(D, J, %VAL(EVD), VALID, BN(J) )
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

              EVD = D(J).PTR + (I-1)*VAL__NBR
              IF ( D(J).REG ) THEN
                CALL EVBIN_REGIDX(D, J, %VAL(EVD), VALID, BN(J) )
              ELSE
                CALL EVBIN_IRREGIDX(D, J, START(J),STOP(J),INC1(J),
     :                  RANGES(1,J), %VAL(EVD), VALID, BN(J) )
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
