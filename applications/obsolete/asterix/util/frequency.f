      SUBROUTINE FREQUENCY( STATUS )
*+
*  Name:
*     FREQUENCY

*  Purpose:
*     Obtains a frequency distribution, or histogram, of its input

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL FREQUENCY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Displays the relative frequency of occurrence of items in a data object.
*     The input data object can be primitive or structured ( an NDF ). Data
*     items are collected into regular or irregularly spaced bins.
*
*     For the regular case, the first bin starts at the minimum of the data
*     in the input object so its centre is at {min + 0.5 binsize} and so on.
*
*     The normalisation option, when used, makes the sum (not a numerical
*     integral) of the frequency bins equal to unity.
*
*     Data quality is taken into account if present in the input data object -
*     items in the input object whose quality is not perfect are ignored by
*     the algorithm.

*  Usage:
*     frequency {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input data object
*     REG = LOGICAL (read)
*        Whether output bins are to be regularly spaced
*     SPACING = REAL (read)
*        (Regular) spacing between output bin centres
*     BOUNDARIES() = REAL (read)
*        (Irregularly spaced) output bin boundaries
*     NORMALISE = LOGICAL (read)
*        Normalisation required (default=Y)
*     OUT = CHAR (read)
*        Output dataset
*     USEGRP = LOGICAL (read)
*        Use grouping information if present?

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
*     Associate input object
*     Calculate input object data range
*     Prompt for (regular) bin spacing
*     Bin up data
*     Normalise if required
*     Tidy up

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
*     Only 2000 irregular output bins are allowed

*  References:
*     {task_references}...

*  Keywords:
*     frequency, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden (SL2, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     21 Jun 1983 (JCMP):
*        Original version.
*     20 Dec 1983 (JCMP):
*        Modified for new SSE
*     22 Oct 1984 (JCMP):
*        Optional display to ARGS
*     16 Jan 1985 (JCMP):
*        Max # of lists parameterised
*      5 Feb 1985 (JCMP):
*        Change to list name output format; version id added
*      4 Sep 1985 V0.3-1 (JCMP):
*        List field range used; check on bin index
*      7 Nov 1985 V0.4-1 (JCMP):
*        Bin numbering changed; bin descriptors added; normalisation option
*     17 Dec 1985 V0.4-2 (JCMP):
*        ADAM version
*      1 May 1986 V0.4-3 (JCMP):
*        Better parameter status handling
*     12 Jun 1986 V1.0-1 (JCMP):
*        Modified from DISTR
*     24 Jun 1986 V1.0-2 (JCMP):
*        Histogram drawing style
*     29 Sep 1986 V0.5-1 (JCMP):
*        Bug fix - arguments of CMP_MAPV
*     23 May 1988 V0.6-1 (ADM):
*        Bug fix - bins no longer include upper limit
*     30 Aug 1988 V1.0-1 (DJA):
*        Asterix88 up-grade. Documentation spruced up a bit -
*        now uses BDA_ routines for structure access
*     12 Dec 1988 V1.0-2 (DJA):
*        Code review. USI replacing ASSOC calls
*      6 Jun 1989 V1.0-3 (DJA):
*        Revised subroutine structure - 2 routine for regular and irregular
*        data - fixes bug causing hanging for large datasets
*      8 Dec 1989 V1.0-4 (DJA):
*        Code tidied up a bit
*     14 Jan 1990 V1.0-5 (DJA):
*        Bug at BDA_UNMAPLQUAL fixed
*     28 Feb 1990 V1.2-0 (DJA):
*        Removed _GIRB subroutine - now uses PRS_GETRANGES to get bin bounds
*     26 Apr 1990 V1.2-1 (DJA):
*        Bug with irregular bin widths sorted
*     19 Nov 1992 V1.7-0 (DJA):
*        Corrected upper bin boundary problem
*     14 Apr 1993 V1.7-1 (DJA):
*        Write data label rather than dataset name to axis label
*      9 Feb 1994 V1.7-2 (DJA):
*        Writes new form GCB attributes
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     26 Mar 1995 V1.8-1 (DJA):
*        Use new data interface
*     12 Sep 1995 V2.0-0 (DJA):
*        Full ADI port.
*      3 Apr 1996 V2.0-1 (DJA):
*        Use grouping info if available
*      1 May 1996 V2.0-2 (DJA):
*        Explcitly write widths
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

*  Local Constants:
      INTEGER                   MAXLINES         ! Max no. of history text lines
        PARAMETER               ( MAXLINES=12 )

      INTEGER                  	MXBIN		! maximum number of bins
        PARAMETER	        ( MXBIN=2000 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'FREQUENCY Version 2.1-0' )

*  Local Variables:
      CHARACTER			TEXT(MAXLINES)*80	! History text
      CHARACTER*80     		AXTXT(2)                ! O/p axis label,units

      REAL          		BOUNDS(MXBIN+1)         ! Bin boundaries
      REAL          		DMIN,DMAX		! Input object range
      REAL          		MAXSIZ	 	        ! Maximum bin size
      REAL			RAXP(2)			! Spaced array data
      REAL			SPACING			! Bin spacing

      INTEGER 	             	APTR	 		! Output axis data
      INTEGER 	             	AWPTR	 		! Output axis widths
      INTEGER                	BPTR        		! Pointer to output bins
      INTEGER		     	DIMS(ADI__MXDIM)	! Input dimensions
      INTEGER		     	DPTR	 		! Output data array
      INTEGER                   DSID                    ! New output object
      INTEGER                	DUMMY       		! Dummy string length
      INTEGER			GDPTR, GQPTR		! Grouped data
      INTEGER			IFID			! Input dataset id
      INTEGER		     	IPTR	 		! Input data
      INTEGER		     	NBIN	 		! Required # bins
      INTEGER		     	NDIM	 		! Input dimensionality
      INTEGER		     	NELM	 		! # input values
      INTEGER                   NREC                    ! Returned from USI_xxxNAME
      INTEGER			OFID			! Output dataset id
      INTEGER		     	QPTR	 		! Quality info

      LOGICAL		     	DQL	 		! Data quality present?
      LOGICAL			GRPED			! Data is grouped?
      LOGICAL                	INCREASING  		! Bounds increase
							! monotonically?
      LOGICAL		     	NORMALISE	 	! Norm'n required to unit frequency?
      LOGICAL		     	OK	         	! Object there and set?
      LOGICAL 	             	REG         		! Regular output bins?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input object
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )

*  Check data object
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( OK ) THEN

        CALL BDI_MAPR( IFID, 'Data', 'READ', IPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check for quality
        CALL BDI_CHK( IFID, 'Quality', DQL, STATUS )
        IF ( DQL ) THEN
          CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QPTR, STATUS )
        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Numeric data required....', STATUS )
        GOTO 99

      END IF

*  Get number of input points
      CALL BDI_GETNEL( IFID, NELM, STATUS )

*  Handle grouping
      CALL UTIL_GRPR( IFID, 'USEGRP', IPTR, .FALSE., 0, DQL, QPTR,
     :                GRPED, NELM, GDPTR, 0, GQPTR, STATUS )
      IF ( GRPED ) THEN
        IPTR = GDPTR
        QPTR = GQPTR
      END IF

*  Get data range
      IF ( DQL ) THEN

*      Get range subject to quality
        CALL ARR_RANG1RLQ( NELM, %VAL(IPTR), %VAL(QPTR), DMIN, DMAX,
     :                                                      STATUS )

      ELSE

*      Get range
        CALL ARR_RANG1R( NELM, %VAL(IPTR), DMIN, DMAX, STATUS )

      END IF
      MAXSIZ = DMAX - DMIN

*  Zero the BOUNDS array
      CALL ARR_INIT1R( 0.0, MXBIN, BOUNDS, STATUS )

*  Get data label and units
      CALL BDI_GET0C( IFID, 'Label,Units', AXTXT, STATUS )
      IF ( AXTXT(2) .LE. ' ' ) AXTXT(2) = 'unitless'

*  Tell user valid regular bin sizes
      CALL MSG_SETR( 'MAXSIZ', MAXSIZ )
      CALL MSG_SETC( 'UNITS', AXTXT(2) )
      CALL MSG_PRNT( 'Regular bin sizes can up to ^MAXSIZ (^UNITS)' )

*  Tell environment what data range is
      CALL MSG_SETR( 'DMIN', DMIN )
      CALL MSG_SETR( 'DMAX', DMAX )
      CALL MSG_PRNT( 'The data range is ^DMIN to ^DMAX' )

*    Regularly spaced bins required?
      CALL USI_GET0L( 'REG', REG, STATUS )
      CALL USI_CANCL( 'REG', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( REG ) THEN

*   	Obtain bin size
        OK = .FALSE.
        DO WHILE ( (.NOT. OK) .AND. (STATUS .EQ. SAI__OK) )

 10       CALL USI_GET0R( 'SPACING', SPACING, STATUS )
          CALL USI_CANCL( 'SPACING', STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Deduce number of bins
          NBIN = 1 + INT((DMAX-DMIN)/SPACING)

*        Assume one bin if NBIN is less than 1
	  IF ( NBIN .LT. 1 ) THEN
            NBIN = 1
            SPACING = DMAX - DMIN
            CALL MSG_PRNT( 'One bin assumed' )

          ELSE

*          Tell environment
            CALL MSG_SETI( 'NBIN', NBIN )
            CALL MSG_PRNT( 'This will give ^NBIN bins' )

          END IF

          OK = .TRUE.

        END DO

      ELSE

*      Get bin boundaries
        CALL PRS_GETRANGES( 'BOUNDARIES', MXBIN, 1, DMIN, DMAX,
     :                                   BOUNDS, NBIN, STATUS )

*      Check bounds increase monotonically
        CALL ARR_INCR( NBIN, BOUNDS, INCREASING, STATUS )
	IF ( ( STATUS .EQ. SAI__OK ) .AND. .NOT. INCREASING ) THEN
          CALL ERR_REP( 'INC', 'Bounds must increase', STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Tell environment
        CALL MSG_SETI( 'NBIN', NBIN )
        CALL MSG_PRNT( 'This will give ^NBIN bins' )
        CALL MSG_PRNT( 'NOTE - receptor bins assumed contiguous' )

      END IF

*  Obtain whether normalisation is required
      CALL USI_DEF0L( 'NORM', .TRUE., STATUS )
      CALL USI_GET0L( 'NORM', NORMALISE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map data for bins
      CALL DYN_MAPR( 1, NBIN, BPTR, STATUS )

*    Zero the output bins
      CALL ARR_INIT1R( 0.0, NBIN, %VAL(BPTR), STATUS )

*    Set up boundaries for REG data
      IF ( REG ) THEN
        CALL FREQUENCY_REGBIN( NELM, %VAL(IPTR), DMIN, SPACING,
     :              NBIN, DQL, %VAL(QPTR), %VAL(BPTR), STATUS )
      ELSE
        CALL FREQUENCY_IRREGBIN( NELM, %VAL(IPTR), BOUNDS, DQL,
     :                   %VAL(QPTR), NBIN, %VAL(BPTR), STATUS )
      END IF

      IF ( NORMALISE ) THEN
        CALL ARR_NORM1R( NBIN, %VAL(BPTR), STATUS )
      END IF

*  Create output object
      CALL BDI_NEW( 'BinDS', 1, NBIN, 'REAL', DSID, STATUS )
      CALL BDI_SETDST( DSID, 'DISTRIBUTION', STATUS )

*  Create output object
      CALL USI_CREAT( 'OUT', DSID, OFID, STATUS )

*  Fill in component values
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', DPTR, STATUS )
      CALL ARR_COP1R( NBIN, %VAL(BPTR), %VAL(DPTR), STATUS )

      IF ( NORMALISE ) THEN
        CALL BDI_PUT0C( OFID, 'Label', 'Relative Frequency', STATUS )
      ELSE
        CALL BDI_PUT0C( OFID, 'Label', 'Frequency', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Now do axis values.
      IF ( REG ) THEN

*    Set up regular axis data
        RAXP(1) = DMIN + SPACING/2.0
        RAXP(2) = SPACING
        CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, RAXP, STATUS )
        CALL BDI_AXPUT0R( OFID, 1, 'ScalarWidth', SPACING, STATUS )

      ELSE

*    Otherwise find bin centres and write to output object
        CALL BDI_AXMAPR( OFID, 1, 'Data', 'WRITE', APTR, STATUS )
        CALL BDI_AXMAPR( OFID, 1, 'Width', 'WRITE', AWPTR, STATUS )
        CALL ARR_BND2CWR( NBIN, BOUNDS, %VAL(APTR), %VAL(AWPTR),
     :                                                  STATUS )

      END IF
      CALL BDI_AXPUT0C( OFID, 1, 'Label,Units', AXTXT, STATUS )

*  Set axis normalisation
      CALL BDI_AXPUT0L( OFID, 1, 'Normalised', NORMALISE, STATUS )

*  Inherit everything bar graphics and grouping
      CALL UDI_COPANC( IFID, 'grf,grp', OFID, STATUS )

*  Set up histogram style
      CALL GCB_LCONNECT( STATUS )
      CALL GCB_SETL( 'STEP_FLAG', .TRUE., STATUS )
      CALL GCB_FSAVE( OFID, STATUS )
      CALL GCB_DETACH( STATUS )

*  Copy, set up and update history
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Fancy stuff - put input parameters into HISTORY
      TEXT(1) = 'Input dataset {INP}'

*  Do data on bins
      IF ( REG ) THEN
        CALL MSG_SETR( 'SPACE', SPACING )
        TEXT(2) = 'Bins regularly spaced'
        CALL MSG_MAKE( 'Bin Spacing = ^SPACE', TEXT(3), DUMMY )
      ELSE
        TEXT(2) = 'Bins irregularly spaced'
        TEXT(3) = 'Bin boundaries given by axis data values'
      END IF
      NREC = MAXLINES
      IF ( GRPED ) THEN
        TEXT(4) = 'Used grouped data when binning'
        CALL USI_TEXT( 4, TEXT, NREC, STATUS )
      ELSE
        CALL USI_TEXT( 3, TEXT, NREC, STATUS )
      END IF

*  Write this into history structure
      CALL HSI_PTXT( OFID, NREC, TEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  FREQUENCY_REGBIN - Bins up data for regular bins
      SUBROUTINE FREQUENCY_REGBIN( NDAT, DATA, BASE, SCALE, NBIN, DQL,
     :                                            QUAL, BINS, STATUS )
*
*    Description :
*
*     Bins up data into NBIN bins which are SCALE wide. The left edge of
*     the leftmost bin has value BASE. Assumes that BINS is large enough
*     to cope with all possible bin values.
*
*    History :
*
*      8 Jun 89 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER           STATUS
*
*    Import :
*
      INTEGER           NDAT                 ! Number of input data values
      REAL              DATA(NDAT)           ! The input data
      REAL              BASE, SCALE          ! Left edge of left bin, and width
      INTEGER           NBIN                 ! Number of output bins
      LOGICAL           DQL                  ! Are we worrying about quality?
      LOGICAL           QUAL(*)              ! Quality if present
*
*    Export :
*
      REAL              BINS(NBIN)           ! The output bins
*
*    Local variables :
*
      INTEGER           BIN                  ! Bin in which to put data value
      INTEGER           I                    ! Loop over data
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Choose method depending on whether we have quality
      IF ( DQL ) THEN
        DO I = 1, NDAT
          IF ( QUAL(I) ) THEN
            BIN = 1 + INT( ( DATA(I) - BASE ) / SCALE )
            BINS(BIN) = BINS(BIN) + 1.0
          END IF
        END DO

      ELSE
        DO I = 1, NDAT
          BIN = 1 + INT( ( DATA(I) - BASE ) / SCALE )
          BINS(BIN) = BINS(BIN) + 1.0
        END DO

      END IF

      END



*+  FREQUENCY_IRREGBIN - Bins up data for FREQUENCY application
      SUBROUTINE FREQUENCY_IRREGBIN(NVAL,DAT,BOUNDS,
     :                DQL,QUAL,BINCNT,BINS, STATUS )
*    Description :
*
*     Sorts the NVAL REAL values in DAT(NVAL) into a frequency distribution
*     of BINCNT bins in the REAL array BIN(BINCNT).
*     Data quality presence is indicated by DQL, and the data quality
*     is imported in the LOGICAL array QUAL(*).
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Jim Peden   (BHVAD::JCMP)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     12 Jun 86 : Modified from DISTR (BHVAD::JCMP)
*     23 May 88 : Binning modified- upper limit no longer included (ADM)
*     30 Aug 88 : Now uses logical quality (DJA)
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
      INTEGER    STATUS
*
*      Import :
*
	INTEGER NVAL		! # input values
	REAL DAT(NVAL)		! input values
        REAL BOUNDS(*)          ! bin boundaries
	LOGICAL DQL		! quality present
	LOGICAL QUAL(*)	        ! Quality values
	INTEGER BINCNT		! # output bins
*    Import-Export :
*    Export :
	REAL BINS(BINCNT)	! output bins
*    Local variables :
	INTEGER I		! index variable
	INTEGER K		! output bin index
*-

        IF ( STATUS .NE. SAI__OK ) RETURN

*      Bin up the data
	IF ( DQL ) THEN

*         Take quality into account
	   DO I = 1, NVAL
	      IF ( QUAL(I) ) THEN
                 CALL FREQUENCY_BSEARCH( BINCNT, BOUNDS, DAT(I), K,
     :                                                     STATUS )
                 IF ( K .NE. 0 ) BINS(K) = BINS(K) + 1
              END IF
	   END DO

	ELSE

*	  No quality to worry about
	   DO I = 1, NVAL
              CALL FREQUENCY_BSEARCH( BINCNT, BOUNDS, DAT(I), K,
     :                                                  STATUS )
              IF ( K .NE. 0 ) BINS(K) = BINS(K) + 1
	   END DO

	END IF

	END


*+  FREQUENCY_BSEARCH - Binary search of bin bounds
      SUBROUTINE FREQUENCY_BSEARCH( N, ARRAY, VALUE, BIN, STATUS )
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
      INTEGER   STATUS
*
*    Import :
*
      INTEGER    N		! Number of elements of data array.
      REAL       ARRAY(2,N)	! Data array in ascending order.
      REAL       VALUE		! Trial value.
*
*    Export :
*
      INTEGER    BIN            ! Bin into which VALUE falls - zero if none
*
*    Local variables :
*
      INTEGER    K,L
*-
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ( VALUE.LT.ARRAY(1,1) ).OR.( VALUE .GE. ARRAY(2,N) ) ) THEN
         BIN = 0

      ELSE

         K = 1
         L = N
         BIN = N/2

 10      IF ( VALUE .GE. ARRAY(2,BIN) ) THEN
            K = BIN + 1
            BIN = (K+L)/2
            GOTO 10

         ELSE IF ( VALUE .LT. ARRAY(1,BIN) ) THEN
            L = BIN - 1
            BIN = (K+L)/2
            GOTO 10

         END IF

      END IF

      END
