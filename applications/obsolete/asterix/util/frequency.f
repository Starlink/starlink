*+  FREQUENCY - Obtains a frequency distribution
      SUBROUTINE FREQUENCY( STATUS )
*
*    Description :
*
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
*
*    Parameters :
*
*     INP=UNIV(R)
*           Input data object
*     REG_SPACED=LOGICAL(R)
*           Whether output bins are to be regularly spaced
*     BIN_SPACING=REAL(R)
*           (Regular) spacing between output bin centres
*     BIN_BOUNDARIES()=REAL(R)
*           (Irregularly spaced) output bin boundaries
*     NORMALISE=LOGICAL(R)
*           Normalisation required (default=.true.)
*     OUT=UNIV(W)
*           Output dataset
*
*    Method :
*
*     Associate input object
*     Calculate input object data range
*     Prompt for (regular) bin spacing
*     Bin up data
*     Normalise if required
*     Tidy up
*
*    Deficiencies :
*     Only 2000 irregular output bins are allowed
*
*    Bugs :
*    Authors :
*
*     Jim Peden   (BHVAD::JCMP)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     21 Jun 83 : Original (BHVAD::JCMP)
*     20 Dec 83 : Modified for new SSE (BHVAD::JCMP)
*     22 Oct 84 : Optional display to ARGS (BHVAD::JCMP)
*     16 Jan 85 : Max # of lists parameterised (BHVAD::JCMP)
*      5 Feb 85 : Change to list name output format;
*                 version id added (BHVAD::JCMP)
*      4 Sep 85 : V0.3-1 list field range used; check on bin index (BHVAD::JCMP)
*      7 Nov 85 : V0.4-1 Bin numbering changed; bin descriptors added;
*                        normalisation option (BHVAD::JCMP)
*     17 Dec 85 : V0.4-2 ADAM version (BHVAD::JCMP)
*      1 May 86 : V0.4-3 Better parameter status handling (BHVAD::JCMP)
*     12 Jun 86 : V1.0-1 Modified from DISTR (BHVAD::JCMP)
*     24 Jun 86 : V1.0-2 Histogram drawing style (BHVAD::JCMP)
*     29 Sep 86 : V0.5-1 Bug fix - arguments of CMP_MAPV (BHVAD::JCMP)
*     23 May 88 : V0.6-1 Bug fix - bins no longer include upper limit (ADM)
*     30 Aug 88 : V1.0-1 Asterix88 up-grade. Documentation spruced up a bit -
*                        now uses BDA_ routines for structure access (DJA)
*     12 Dec 88 : V1.0-2 Code review. USI replacing ASSOC calls (DJA)
*      6 Jun 89 : V1.0-3 Revised subroutine structure - 2 routine for regular
*                        and irregular data - fixes bug causing hanging for
*                        large datasets ( DJA )
*      8 Dec 89 : V1.0-4 Code tidied up a bit (DJA)
*     14 Jan 90 : V1.0-5 Bug at BDA_UNMAPLQUAL fixed (DJA)
*     28 Feb 90 : V1.2-0 Removed _GIRB subroutine - now uses PRS_GETRANGES no
*                        get bin boundaries. (DJA)
*     26 Apr 90 : V1.2-1 Bug with irregular bin widths sorted (DJA)
*     19 Nov 92 : V1.7-0 Corrected upper bin boundary problem (DJA)
*     14 Apr 93 : V1.7-1 Write data label rather than dataset name to axis
*                        label (DJA)
*      9 Feb 94 : V1.7-2 Writes new form GCB attributes (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE     'SAE_PAR'
      INCLUDE     'DAT_PAR'
*
*    Status :
*
      INTEGER      STATUS
*
*    Local Constants :
*
      INTEGER                  MAXLINES         ! Max no. of history text lines
        PARAMETER              (MAXLINES=12)
      INTEGER                  MXBIN		! maximum number of bins
        PARAMETER	       (MXBIN=2000)
*
*    Local variables :
*
      CHARACTER     LOC*(DAT__SZLOC)	        ! input object locator
      CHARACTER     OLOC*(DAT__SZLOC)	        ! output object locator
      CHARACTER     TEXT(MAXLINES)*80           ! History text
      CHARACTER     NAME*(DAT__SZNAM)	        ! name of input object
      CHARACTER     LABEL*80	                ! Data label
      CHARACTER     UNITS*80	                ! Data units

      REAL          BOUNDS(MXBIN+1)             ! bin boundaries

      REAL          DMIN,DMAX		        ! input object range
      REAL          MAXSIZ	 	        ! maximum bin size
      REAL          SPACING                     ! regular bin spacing

      INTEGER                DUMMY       ! Dummy string length parameter
      INTEGER 	             APTR	 ! Ptr to output axis data
      INTEGER 	             AWPTR	 ! Ptr to output axis data
      INTEGER                BPTR        ! Pointer to output bins
      INTEGER		     DIMS(DAT__MXDIM)! Sizes of dimensions of object
      INTEGER		     DPTR	 ! Ptr to output data array
      INTEGER		     IPTR	 ! pointer to input data
      INTEGER		     NBIN	 ! required number of bins
      INTEGER		     NDIM	 ! Dimensionality of input arrays
      INTEGER		     NELM	 ! # input values
      INTEGER                NREC        ! Returned from USI_xxxNAME
      INTEGER		     QPTR	 ! pointer to quality info.

      LOGICAL 	             ANYBAD      ! Any points with bad quality
      LOGICAL		     DQL	 ! data quality present
      LOGICAL                INCREASING  ! Bounds increase monotonically
      LOGICAL		     NORMALISE	 ! norm'n required to unit frequency?
      LOGICAL		     OK	         ! object there and set
      LOGICAL		     PRIM	 ! primitive input object
      LOGICAL 	             REG         ! regular output bins?
*
*    Version id :
*
      CHARACTER              VERSION*30
        PARAMETER            (VERSION = 'FREQUENCY Version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX common blocks
      CALL AST_INIT()

*    Associate input object
      CALL USI_ASSOCI( 'INP', 'READ', LOC, PRIM, STATUS )

*    Get its name
      CALL DAT_NAME( LOC, NAME, STATUS )

*    Check data object
      CALL BDA_CHKDATA( LOC, OK, NDIM, DIMS, STATUS )
      CALL ARR_SUMDIM( NDIM, DIMS, NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( OK ) THEN

        CALL BDA_MAPDATA( LOC, 'READ', IPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Check for quality
        CALL BDA_CHKQUAL( LOC, DQL, NDIM, DIMS, STATUS )

        IF ( DQL ) THEN
          CALL BDA_MAPLQUAL( LOC, 'READ', ANYBAD, QPTR, STATUS )
          IF ( .NOT. ANYBAD ) THEN
            CALL BDA_UNMAPLQUAL( LOC, STATUS )
            DQL = .FALSE.
          END IF
        END IF

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Numeric data required....', STATUS )
        GOTO 99

      END IF

      IF ( DQL ) THEN

*      Get range subject to quality
        CALL ARR_RANG1RLQ( NELM, %VAL(IPTR), %VAL(QPTR), DMIN, DMAX,
     :                                                      STATUS )

      ELSE

*      Get range
        CALL ARR_RANG1R( NELM, %VAL(IPTR), DMIN, DMAX, STATUS )

      END IF
      MAXSIZ = DMAX - DMIN

*    Zero the BOUNDS array
      CALL ARR_INIT1R( 0.0, MXBIN, BOUNDS, STATUS )

*    Get data label
      CALL BDA_GETLABEL( LOC, LABEL, STATUS )
      IF ((LABEL .LE. ' ') .OR. ( STATUS .NE. SAI__OK )) THEN
        LABEL = NAME
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      END IF

*    Get data units
      CALL BDA_GETUNITS( LOC, UNITS, STATUS )
      IF ((UNITS .LE. ' ') .OR. ( STATUS .NE. SAI__OK )) THEN
        UNITS = 'unitless'
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
      END IF

*    Tell user valid regular bin sizes
      CALL MSG_SETR( 'MAXSIZ', MAXSIZ )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_PRNT( 'Regular bin sizes can up to ^MAXSIZ (^UNITS)' )

*    Tell environment what data range is
      CALL MSG_SETR( 'DMIN', DMIN )
      CALL MSG_SETR( 'DMAX', DMAX )
      CALL MSG_PRNT( 'The data range is ^DMIN to ^DMAX' )

*    Regularly spaced bins required?
      CALL USI_GET0L( 'REG_SPACED', REG, STATUS )
      CALL USI_CANCL( 'REG_SPACED', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( REG ) THEN

*   	Obtain bin size
        OK = .FALSE.
        DO WHILE ( (.NOT. OK) .AND. (STATUS .EQ. SAI__OK) )

 10       CALL USI_GET0R( 'BIN_SPACING', SPACING, STATUS )
          CALL USI_CANCL( 'BIN_SPACING', STATUS )
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
        CALL PRS_GETRANGES( 'BIN_BOUNDARIES', MXBIN, 1, DMIN, DMAX,
     :                                       BOUNDS, NBIN, STATUS )

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

*    Obtain whether normalisation is required
      CALL USI_DEF0L( 'NORMALISE', .TRUE., STATUS )
      CALL USI_GET0L( 'NORMALISE', NORMALISE, STATUS )
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

*    Create output object
      CALL USI_ASSOCO( 'OUT', 'DISTRIBUTION', OLOC, STATUS )

*    Create components
      CALL BDA_CREDATA( OLOC, 1, NBIN, STATUS )
      CALL BDA_CREAXES( OLOC, 1, STATUS )

*    Fill in component values
      CALL BDA_MAPDATA( OLOC, 'WRITE', DPTR, STATUS )
      CALL ARR_COP1R( NBIN, %VAL(BPTR), %VAL(DPTR), STATUS )

      IF ( NORMALISE ) THEN
        CALL BDA_PUTLABEL( OLOC, 'Relative Frequency', STATUS )
      ELSE
        CALL BDA_PUTLABEL( OLOC, 'Frequency', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Now do axis values.
      IF ( REG ) THEN

*      If regularly spaced then set up spaced axis array
        CALL BDA_PUTAXVAL( OLOC, 1, DMIN+SPACING/2.0, SPACING,
     :                                          NBIN, STATUS )

      ELSE

        CALL BDA_CREAXVAL( OLOC, 1, .FALSE., NBIN, STATUS )
        CALL BDA_CREAXWID( OLOC, 1, .FALSE., NBIN, STATUS )

*      Otherwise find bin centres and write to output object
        CALL BDA_MAPAXVAL( OLOC, 'WRITE', 1, APTR, STATUS )
        CALL BDA_MAPAXWID( OLOC, 'WRITE', 1, AWPTR, STATUS )
        CALL AXIS_RNG2VALW( NBIN, BOUNDS, %VAL(APTR), %VAL(AWPTR),
     :                                                    STATUS )

      END IF
      CALL BDA_PUTAXLABEL( OLOC, 1, LABEL, STATUS )
      CALL BDA_PUTAXUNITS( OLOC, 1, UNITS, STATUS )

*    Set axis normalisation
      CALL BDA_PUTAXNORM( OLOC, 1, NORMALISE, STATUS )

*    Copy MORE structure across.
      CALL BDA_COPMORE( LOC, OLOC, STATUS )

*    Set up histogram style
      CALL GCB_LCONNECT(STATUS)
      CALL GCB_SETL('STEP_FLAG',.TRUE.,STATUS)
      CALL GCB_SAVE(OLOC,STATUS)
      CALL GCB_DETACH(STATUS)

*    Copy, set up and update history
      CALL HIST_COPY( LOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Fancy stuff - put input parameters into HISTORY
      TEXT(1) = 'Input dataset {INP}'

*    Do data on bins
      IF ( REG ) THEN
        CALL MSG_SETR( 'SPACE', SPACING )
        TEXT(2) = 'Bins regularly spaced'
        CALL MSG_MAKE( 'Bin Spacing = ^SPACE', TEXT(3), DUMMY )
      ELSE
        TEXT(2) = 'Bins irregularly spaced'
        TEXT(3) = 'Bin boundaries given by axis data values'
      END IF
      NREC = MAXLINES
      CALL USI_TEXT( 3, TEXT, NREC, STATUS )

*    Write this into history structure
      CALL HIST_PTXT( OLOC, NREC, TEXT, STATUS )

*    Release datasets
      CALL BDA_RELEASE( LOC, STATUS )
      CALL BDA_RELEASE( OLOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
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
      INCLUDE 'DAT_PAR'
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
      INCLUDE 'DAT_PAR'
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
      INCLUDE     'DAT_PAR'
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
