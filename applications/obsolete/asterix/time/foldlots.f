*+  FOLDLOTS - Folds binned data at a range of periods into phase bins
      SUBROUTINE FOLDLOTS( STATUS )
*    Description :
*     Folds 1-D data into phase bins at a range of trial periods. Outputs 2
*     sdf files. The first contains period vs. chi-squared, the second is
*     a folded 1-D data set at the period of maximum chi-squared.
*     (weighted means available if errors are available)
*     Note - if axis zero is defined then this must be incorporated in the
*     phase zero epoch.
*    Method :
*     The basic technique is straightforward.
*     Arrays are mapped so there is no size limitation.
*     Bad quality data are excluded.
*     If time is not AXIS1 then the axes are swapped.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*     Phillip Andrews (PLA_ROSAT@uk.ac.bham.sr.star)
*     David J. Allan (BHVAD::DJA)
*     Simon Duck (BHVAD::SRD)
*     Andrew Norton (SOTON::AJN)
*     Richard Saxton (LTVAD::RDS)
*    History :
*     26 Mar 86: Original (TJP)
*     29 Aug 86: Renamed (JCMP)
*
*     10 Sep 87: Accepts primitive input object. Checks validity of locators
*                before attempting to annul them. Displays name of input object.
*                Displays AXIS(1) UNITS. Displays max No of output bins (and
*                makes sure this is not exceeded). Program suggests defaults
*                for phase zero epoch and No of phase bins
*                (= No of input bins in phase period). ACTIONS added to
*                History record. (PLA)
*
*     10 Dec 89: Ghastly spelling mistakes removed! Axis(1) text now correct.
*                Uses MSG_PRNT now. (DJA)
*
*     12 Jan 90: V1.0-3 Re-write to use ASTLIB! No longer generates negative
*                       variances. Doesn't re-prompt after invalid input.
*                       The TIM_ routines now use IMPLICIT NONE. (BHVAD::DJA)
*     10 Apr 90: V1.0-4 Accepts nD data and folds each dimension over time. If
*                       time is not the AXIS(1) then the axes are swapped so
*                       that it is. (SRD)
*
*     19 Jul 90: bug fixes in unweighted fold subroutine (AJN) :-
*                BVAR & YBAR arrays now initialised in TIM_FOLDU subroutine
*
*     24 Jul 90: bug fix - epochzero is carried over to subroutines
*          	 as a number of days - now converts to seconds within
*                the subroutines before calculation (AJN)
*
*     30 Sep 90: Converted to do range folding on a 1-D data set (AJN)
*                Nb. not all of the code referring to n-D sets has been
*                removed - it all works okay but is maybe a little untidy !
*
*     17 Jun 91: Rationalised to use the same input as the other time
*                series routines. Fixed some bugs in parameter passing
*                into the main subroutines  (RDS)
*      7 Oct 92: V1.7-0 TIM_FOLDx routines renamed FOLDLOTS_FOLDx to avoid
*                       confusion with incompatible versions in FOLDBIN (DJA)
*     10 Apr 93: V1.7-1 Removed superfluous STATUS in call to MSG_SET (DJA)
*     17 AUG 93: V1.7-2 Trivial changes to ARR_REG routines (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local Constants :
*
      INTEGER            MAXHTXT                ! Maximum amount of history
         PARAMETER       (MAXHTXT = 10)
*
*    Local variables :
*
      CHARACTER          ILOC*(DAT__SZLOC)      ! Input object locator
      CHARACTER          FLOC*(DAT__SZLOC)      ! Folded data object locator
      CHARACTER		 CLOC*(DAT__SZLOC)      ! Chi-sq data object locator
      CHARACTER          HEADER*(DAT__SZLOC)    ! Locator to HEADER

      CHARACTER*132      HTXT(MAXHTXT)          ! History text
      CHARACTER*80       UNITS                  ! Value of AXIS(1) UNITS.

      DOUBLE PRECISION   BASE_TAI               ! Atomic time
      DOUBLE PRECISION   ZEROEPOCH              ! Epoch of phase zero
      DOUBLE PRECISION   AXZERO                 ! Axis1 zero time

      REAL               BASE, SCALE            ! Axis attributes
      REAL               SUM                    ! General summation variable
      REAL               PERIOD                 ! Original period variable
      REAL               PSTART			! Start period
      REAL 		 PINC			! Period increment for range

      REAL               CHISQ			! Chi-squared fit to constant
                                                ! level (i.e.phase indept.)
      REAL               TEM                    ! Dummy
      REAL               TMIN, TMAX             ! Min and max times
      REAL               TSCALE                 ! Mean time increment

      INTEGER            HLEN                   ! Length of a history string
      INTEGER            I                      ! Dummy variable for loops.
      INTEGER            N
      INTEGER            L
      INTEGER            NTOT                   ! Total no.of data points
      INTEGER            NGOOD                  ! Total no.of good data points
      INTEGER		 NPER			! No. of periods to fold at
      INTEGER            IERR                   ! Error flag from TIM_FOLD
      INTEGER            NBINS                  ! No. of phase bins
      INTEGER            NELM
      INTEGER            NELM2
      INTEGER            NACTDIM                ! Dimensionality of data
      INTEGER            NVAL                   ! No.of values read
      INTEGER            VNDIM                  ! Dimensions of variance
      INTEGER            VDIMS(DAT__MXDIM)      ! size of variance
      INTEGER            VDIM(DAT__MXDIM)
      INTEGER            DPNTR                  ! Pointer to data array
      INTEGER            TPNTR                  ! Pointer to axis data
      INTEGER            TWPNTR                 ! Pointer to width data
      INTEGER            VPNTR                  ! Pointer to variances
      INTEGER            FDPTR                  ! Pointer to folded data array
      INTEGER            FXPTR                  ! Pointer to folded axis data
      INTEGER            FVPTR                  ! Pointer to folded error array
      INTEGER            FNPTR                  ! Pointer to folded no. array
      INTEGER            FQPTR                  ! Pointer to folded quality array
      INTEGER		 CDPTR			! Pointer to chi-squared array
      INTEGER		 CXPTR			! Pointer to chi axis data
      INTEGER	 	 CQPTR			! Pointer to chi quality data
      INTEGER            WPTR                   ! Pointer to work array
      INTEGER            DELPTR
      INTEGER            PTR
      INTEGER            NDOF                   ! No. of d.o.f. for chi-squared
      INTEGER            ODIM(DAT__MXDIM)
      INTEGER            INBINS                 ! Number of input bins/ period.
      INTEGER            SIZ
      INTEGER            USED                   ! # history text lines

      BYTE               MASK                   ! Quality mask

      LOGICAL            LVAR                   ! Is variance present ?
      LOGICAL            LOG,OK                 ! things ok?
      LOGICAL            BOK                    ! BASE_TAI present
      LOGICAL            BAD                    ! Any BAD data?
      LOGICAL            WEIGHT
*
*    Version id
*
      CHARACTER*30       VERSION
         PARAMETER       ( VERSION = 'FOLDLOTS Version 1.7-2')
*
*    Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT()

*    Version
      CALL MSG_PRNT( VERSION )

*    Get input data
      CALL TIM_GETDATA(ILOC, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                                        VPNTR, STATUS )
*
*    Map axis widths. NB: this routine sets axis widths assuming
*    bin boundaries are equidistant between bins if widths are
*    missing.
      CALL BDA_MAPAXWID( ILOC, 'READ', 1, TWPNTR, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Create a folded data object.
      CALL USI_ASSOCO( 'FOLD_OBJ', 'FOLDED_SERIES', FLOC, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Create an object for the chi-squared array.
      CALL USI_ASSOCO( 'CHI_OBJ', 'DISTRIBUTION', CLOC,STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    If AXIS(1) UNITS are defined, then display them.
      UNITS = 'Units'
      CALL BDA_GETAXUNITS( ILOC, 1, UNITS, STATUS )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_PRNT( 'TIME UNITS are ^UNITS' )
*
*    Find the axis array range
      CALL BDA_MAPAXVAL(ILOC, 'READ', 1, DELPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error accessing axis array')
         GOTO 999
      ENDIF
*
      CALL ARR_RANG1R( NTOT, %val(DELPTR), TMIN, TMAX, STATUS )
*
      TSCALE = (TMAX - TMIN) / REAL(NTOT - 1)
      CALL MSG_SETR('TS', TSCALE)
      CALL MSG_PRNT( 'Mean time increment : ^TS')
*
      IF (STATUS .NE. SAI__OK) STATUS = SAI__OK
*
*    Get start period
      CALL USI_GET0R( 'PERIOD', PERIOD, STATUS )
*
      PSTART=PERIOD

*    Get period increment
      CALL USI_GET0R( 'PINC', PINC, STATUS )

*    Get number of periods to fold at
      CALL USI_GET0I( 'NPER', NPER, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
*    Define base epoch
      CALL BDA_LOCHEAD( ILOC, HEADER, STATUS )

      CALL DAT_THERE(HEADER,'BASE_TAI',BOK,STATUS)

      IF(BOK) THEN
        CALL CMP_GET0D( HEADER, 'BASE_TAI', BASE_TAI, STATUS )
      ELSE
        CALL MSG_PRNT( 'No atomic time: default epoch set to 0' )
        BASE_TAI=0.0
      ENDIF

      CALL USI_DEF0D( 'PHASE_0_EPOCH', BASE_TAI, STATUS )
      CALL USI_GET0D( 'PHASE_0_EPOCH', ZeroEpoch, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      ZEROEPOCH = ZEROEPOCH - BASE_TAI
*
* Calculate default number of phase bins
      IF (NTOT .GT. 1 .AND. TMAX .NE. TMIN) THEN
*
         INBINS = INT(PERIOD / TSCALE)
*
      ELSE
         CALL MSG_PRNT('Error in data array or axis')
         GOTO 999
      ENDIF
*
C      CALL MSG_SETI('NB', INBINS)
C      CALL MSG_PRNT( 'Maximum No of phase bins : ^NB')
*
      CALL USI_DEF0I( 'NPHASE', INBINS, STATUS )
      CALL USI_GET0I( 'NPHASE', NBINS, STATUS )

      IF ( NBINS .GT. INBINS ) THEN
C          CALL MSG_PRNT( 'Too many output bins: ' )
C          CALL MSG_PRNT( ' Max. permissible used instead.' )
C          NBINS = INBINS
         CALL MSG_PRNT('The data is being oversampled - this is'/
     &               /' only valid if the time series is long compared'/
     &               /' to the folding period')
      END IF

*    Ask if weighted mean required
      CALL USI_GET0L('WEIGHT',WEIGHT,STATUS)
*
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Map workspace
      CALL DYN_MAPR(1,NBINS,WPTR,STATUS)
*
*    Set up and map components in output object - folded data
      CALL BDA_CREDATA(FLOC,1,NBINS,STATUS)
      CALL BDA_CREVAR(FLOC,1,NBINS,STATUS)
      CALL BDA_CREAXES(FLOC,1,STATUS)
      CALL BDA_CREAXVAL(FLOC,1,.FALSE.,NBINS,STATUS)
      CALL BDA_CREQUAL(FLOC,1,NBINS,STATUS)
      CALL BDA_MAPDATA(FLOC,'WRITE',FDPTR,STATUS)
      CALL BDA_MAPVAR(FLOC,'WRITE',FVPTR,STATUS)
      CALL BDA_MAPAXVAL(FLOC,'WRITE',1,FXPTR,STATUS)
      CALL BDA_MAPQUAL(FLOC,'WRITE',FQPTR,STATUS)
      CALL BDA_PUTMASK(FLOC,MASK,STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_FLUSH(STATUS)
      END IF
      CALL DAT_NEW(FLOC,'N_OCCUPANTS','_INTEGER',1,NBINS,STATUS)
      CALL CMP_MAPN( FLOC, 'N_OCCUPANTS', '_INTEGER',
     :                       'WRITE', 1, FNPTR,NBINS,STATUS)
*    Set output quality to GOOD (as all bad data has been excluded)
      CALL ARR_SUMDIM(1,NBINS,NELM2)
      CALL ARR_INIT1B(QUAL_GOOD,NELM2,%VAL(FQPTR),STATUS)

*    Set up and map components in output object - chi-sq vs. period  data
      CALL BDA_CREDATA(CLOC,1,NPER,STATUS)
      CALL BDA_CREAXES(CLOC,1,STATUS)
      CALL BDA_CREAXVAL(CLOC,1,.FALSE.,NPER,STATUS)
      CALL BDA_CREQUAL(CLOC,1,NPER,STATUS)
      CALL BDA_MAPDATA(CLOC,'WRITE',CDPTR,STATUS)
      CALL BDA_MAPAXVAL(CLOC,'WRITE',1,CXPTR,STATUS)
      CALL BDA_MAPQUAL(CLOC,'WRITE',CQPTR,STATUS)
      CALL BDA_PUTMASK(CLOC,MASK,STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_FLUSH(STATUS)
      END IF
*    Set output quality to GOOD (as all bad data has been excluded)
      CALL ARR_SUMDIM(1,NPER,NELM2)
      CALL ARR_INIT1B(QUAL_GOOD,NELM2,%VAL(CQPTR),STATUS)

*  Fold the data
      IF (WEIGHT)THEN
*
*       Weighted

         CALL FOLDLOTS_FOLDW(NGOOD, %VAL(TPNTR), %VAL(TWPNTR),
     :             %VAL(DPNTR), %VAL(VPNTR), PERIOD, PINC, NPER,
     :             REAL(ZEROEPOCH),NBINS,%VAL(WPTR),
     :             %VAL(FNPTR), %VAL(FDPTR), %VAL(FVPTR),
     :             NDOF, %VAL(CDPTR), IERR, CHISQ, STATUS)
      ELSE

*       Unweighted

         CALL FOLDLOTS_FOLDU(NGOOD, %VAL(TPNTR),%VAL(TWPNTR),
     :             %VAL(DPNTR), %VAL(VPNTR), PERIOD, PINC, NPER,
     :             REAL(ZEROEPOCH), NBINS, %VAL(WPTR),
     :             %VAL(FNPTR), %VAL(FDPTR), %VAL(FVPTR),
     :             NDOF, %VAL(CDPTR), IERR, CHISQ, STATUS)
*
      ENDIF
*
*   Nb. On returning from the subroutines, PERIOD contains the best
*       period - ie. that with the highest chi-squared, and CHISQ
*       contains this highest value.

      IF ( IERR .NE. 0 ) THEN
          CALL MSG_SETI( 'IERR', IERR )
          CALL MSG_PRNT( 'Bad phase bin - IERR = ^IERR' )
      END IF

*   Put phase bin boundaries into an array
      CALL ARR_REG1R( 0.5/REAL(NBINS), 1.0/REAL(NBINS), NBINS,
     :                                   %VAL(FXPTR), STATUS )
*   Put period bin boundaries into an array
      CALL ARR_REG1R( PSTART, PINC, NPER, %VAL(CXPTR), STATUS )

*    Unmap the Input data
      CALL BDA_UNMAPDATA(ILOC,STATUS)

*    And the Output (2 files)
      CALL BDA_UNMAPDATA(FLOC,STATUS)
      CALL BDA_UNMAPVAR(FLOC,STATUS)
      CALL BDA_UNMAPAXVAL(FLOC,1,STATUS)
      CALL BDA_UNMAPQUAL(FLOC,STATUS)
      CALL CMP_UNMAP(FLOC,'N_OCCUPANTS',STATUS)

      CALL BDA_UNMAPDATA(CLOC,STATUS)
      CALL BDA_UNMAPAXVAL(CLOC,1,STATUS)
      CALL BDA_UNMAPQUAL(CLOC,STATUS)

*    Write components to output objects (2 files)
      CALL DAT_NEW0R(FLOC,'CHISQUARED',STATUS)
      CALL CMP_PUT0R(FLOC,'CHISQUARED',CHISQ,STATUS)
      CALL DAT_NEW0I(FLOC,'DEG_OF_FREEDOM',STATUS)
      CALL CMP_PUT0I(FLOC,'DEG_OF_FREEDOM',NDOF,STATUS)

      CALL BDA_COPTEXT( ILOC, FLOC, STATUS )
      CALL BDA_COPTEXT( ILOC, CLOC, STATUS )
      CALL BDA_PUTLABEL( CLOC, 'Chi-squared', STATUS )
      CALL BDA_PUTUNITS( CLOC, ' ', STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH(STATUS)
      END IF
      CALL BDA_PUTAXLABEL(FLOC,1,'Phase',STATUS)
      CALL BDA_PUTAXNORM(FLOC,1,.TRUE.,STATUS)
      CALL BDA_PUTAXLABEL(CLOC,1,'Period',STATUS)
      CALL BDA_PUTAXNORM(CLOC,1,.TRUE.,STATUS)
*    Copy other info from input to output files
      CALL BDA_COPMORE( ILOC, FLOC, STATUS )
      CALL BDA_COPMORE( ILOC, CLOC, STATUS )

*    Add history records
      CALL HIST_COPY( ILOC, FLOC, STATUS )
      CALL HIST_ADD( FLOC, VERSION, STATUS )
      CALL HIST_COPY( ILOC, CLOC, STATUS )
      CALL HIST_ADD( CLOC, VERSION, STATUS )

      HTXT(1) = 'Input {INP}'
      HTXT(2) = 'Folded output {FOLD_OBJ}'
      CALL MSG_SETR( 'PERIOD', PERIOD )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_MAKE( 'The data has been folded into a period of '/
     :                          /'^PERIOD ^UNITS', HTXT(3), HLEN )
      CALL MSG_SETR( 'EPOCH', REAL(ZEROEPOCH+BASE_TAI) )
      CALL MSG_MAKE('The epoch of phase zero was ^EPOCH', HTXT(4),
     :                                                       HLEN)
      USED = MAXHTXT
      CALL USI_TEXT( 4, HTXT, USED, STATUS )
      CALL HIST_PTXT(FLOC,USED,HTXT,STATUS)

      HTXT(1) = 'Input {INP}'
      HTXT(2) = 'Chi-sq vs. period output {CHI_OBJ}'
      CALL MSG_SETR( 'PSTART', PSTART )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_MAKE( 'The starting period was ^PSTART ^UNITS', HTXT(3),
     :                                                         HLEN)
      CALL MSG_SETR( 'PINC', PINC )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_MAKE( 'The period increment was ^PINC ^UNITS', HTXT(4),
     :                                                        HLEN)
      CALL MSG_SETI( 'NPER', NPER )
      CALL MSG_MAKE( '^NPER periods were searched', HTXT(5), HLEN)
      CALL MSG_SETR( 'EPOCH', REAL(ZEROEPOCH+BASE_TAI) )
      CALL MSG_MAKE('The epoch of phase zero was ^EPOCH', HTXT(6),
     :                                                       HLEN)
      USED = MAXHTXT
      CALL USI_TEXT( 6, HTXT, USED, STATUS )
      CALL HIST_PTXT(CLOC,USED,HTXT,STATUS)

*    Output maximum CHISQ & best PERIOD values to user
      CALL MSG_SETR( 'PERIOD', PERIOD )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_PRNT( 'Best fit period = ^PERIOD ^UNITS')
      CALL MSG_SETR( 'CHISQ', CHISQ )
      CALL MSG_PRNT( 'Chi-sq value of best fit to mean = ^CHISQ')

*    Release datasets
      CALL BDA_RELEASE( ILOC, STATUS )
      CALL BDA_RELEASE( FLOC, STATUS )
      CALL BDA_RELEASE( CLOC, STATUS )

*   Tidy up
999   CONTINUE

      CALL AST_CLOSE

*    Exit
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP('EXERR','Error report from FOLDLOTS',STATUS)
      END IF

      END



*+  FOLDLOTS_FOLDW - folds data Y(X) about a period in X
      SUBROUTINE FOLDLOTS_FOLDW(NPTS, X, AXWID, Y, VAR,
     :                    PERIOD, PINC, NPER, X0, NBINS, W, IB,
     :                    BMN, BMNVAR, NDOF, CHI_VS_P,
     :                    IERR, CHISQ, STATUS)
*    Description :
*
*	Folds the data Y(X) about a range of periods P in X.
*	Data are binned into
*	NBINS phase bins and the mean and standard deviation of Y computed
*	for each bin. X0 corresponds to phase zero. Weighted means used.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*      Data is folded at a range of periods, the best one is selected
*      (highest chi-squared) and the fold repeated once more at this
*      single period for output.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     David J. Allan
*    History :
*     12 Aug 83 : FOLDS V.1
*     26 Mar 86 : TIM_FOLD V.1
*     16 Sep 86 : Phases corrected for negative X
*     13 Jan 90 : Recode in FORTRAN 77!!! (BHVAD::DJA)
*      4 Apr 90 : Accept nD datasets (BHVAD::SRD)
*        Jul 90 : Bug fixes (SOTON::AJN)
*        Sep 90 : Conversion to range folding (SOTON::AJN)
*        Jun 91 : Sorted input parameters out
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      INTEGER            NPTS                  ! No. of data points
      REAL               X(NPTS)                ! Independent variable
      REAL               AXWID(NPTS)
      REAL               Y(NPTS)		! Dependent variable
      REAL               VAR(NPTS)		! Variance in Y
      INTEGER		 NPER			 ! No. of periods to fold at
      INTEGER            NBINS                  ! Number of phase bins
      REAL               PERIOD                 ! Start Folding period
      REAL 		 PINC			! Increment for period
      REAL               X0                     ! Epoch of phase zero
      REAL               W(NBINS)		! work space
*
*    Export :
      REAL		 CHI_VS_P(NPER)         ! array of chi values
      INTEGER            IB(NBINS)		! Number of points per bin
      INTEGER            IERR                   ! 0=O.K. 1=less than 2 points
                                                ! in some bins. For 1 point
                                                ! BMNVAR is zero, for 0 BMN too.
      REAL               BMN(NBINS)		! Mean Y value weighted
      REAL               BMNVAR(NBINS)		! Standard deviation per bin
      REAL               CHISQ			! Chi-squared fit of means to
                                                ! constants intensity.
      INTEGER            NDOF			! Number of degrees of freedom
                                                ! of CHISQ - NBINS-1 unless
                                                ! some bins contain <2 points
*    Status :
      INTEGER            STATUS
*
*    Local variables
*
      INTEGER            A
      INTEGER            I,J,NB,NUM,PCOUNTER
      REAL               DVAR,YBAR,PHA
      REAL               SUMVAR
      REAL               CHIBEST
      REAL 		 PSTART,PBEST
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN


*    Convert epoch of phase zero to seconds
      X0=X0*86400.0


*    Range stuff
      PSTART=PERIOD
      CHIBEST=0.0

      DO PCOUNTER=1,NPER
	PERIOD=PSTART+(PINC*REAL(PCOUNTER-1))

*    Initialise
	NUM=NBINS
	CALL ARR_INIT1I(0,NUM,IB,STATUS)
	CALL ARR_INIT1R(0.0,NUM,BMN,STATUS)
	CALL ARR_INIT1R(0.0,NUM,BMNVAR,STATUS)
	CALL ARR_INIT1R(0.0,NUM,W,STATUS)
	IERR=0

*    Bin up data
	DO A=1,NPTS
          PHA=AMOD((X(A)-X0)/PERIOD,1.0)
          IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
          NB = INT( PHA*NBINS ) + 1
*
          IB(NB) = IB(NB) + 1
*
          IF ( VAR(A).LE.0.0 ) THEN
             DVAR=1/(AXWID(A)*AXWID(A))
          ELSE
             DVAR=VAR(A)
	  END IF
*
          BMN(NB)=BMN(NB)+Y(A)/DVAR
          W(NB)=W(NB)+1.0/DVAR
*
	END DO
*
*    Analyse phase bins
        YBAR=0.0
        SUMVAR=0.0
        NDOF=NBINS-1
        DO A=1,NBINS
		IF ( IB(A) .LT. 2 )THEN
                      IERR=1
	              NDOF=NDOF-1
                ELSE
                      BMN(A)=BMN(A)/W(A)
                      BMNVAR(A) = 1.0/W(A)
                      YBAR=YBAR+BMN(A)/BMNVAR(A)
                      SUMVAR=SUMVAR+1.0/BMNVAR(A)
	        END IF
        END DO
        YBAR=YBAR/SUMVAR

*    Find CHISQ
	CHISQ=0.0
        DO A=1,NBINS
                  IF ( IB(A) .GE. 2 )THEN
			CHISQ=CHISQ+((BMN(A)-YBAR)**2/BMNVAR(A))
                  END IF
	END DO

	CHI_VS_P(PCOUNTER)=CHISQ

        IF (CHISQ.GE.CHIBEST) THEN
		CHIBEST = CHISQ
		PBEST   = PERIOD
	ENDIF

*    end of period loop
      ENDDO


      PERIOD = PBEST
      CHISQ  = CHIBEST

*    Do the binning again with the best period to output the best fold

*    Initialise
      NUM=NBINS
      CALL ARR_INIT1I(0,NUM,IB,STATUS)
      CALL ARR_INIT1R(0.0,NUM,BMN,STATUS)
      CALL ARR_INIT1R(0.0,NUM,BMNVAR,STATUS)
      CALL ARR_INIT1R(0.0,NUM,W,STATUS)
      IERR=0

*    Bin up data
      DO A=1,NPTS
*
        PHA=AMOD((X(A)-X0)/PERIOD,1.0)
*
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
*
        NB = INT( PHA*NBINS ) + 1
*
	IB(NB) = IB(NB) + 1
*
        IF ( VAR(A).LE.0.0 ) THEN
           DVAR=1/(AXWID(A)*AXWID(A))
        ELSE
           DVAR=VAR(A)
	END IF
*
        BMN(NB)=BMN(NB)+Y(A)/DVAR
        W(NB)=W(NB)+1.0/DVAR
*
      END DO

*    Analyse phase bins
        YBAR=0.0
        SUMVAR=0.0
        NDOF=NBINS-1
*
        DO A=1,NBINS
*
           IF ( IB(A) .LT. 2 )THEN
              IERR=1
	      NDOF=NDOF-1
           ELSE
              BMN(A)=BMN(A)/W(A)
              BMNVAR(A) = 1.0/W(A)
	   END IF

        ENDDO
*
      X0=X0/86400.0
*
      END



*+  FOLDLOTS_FOLDU - folds data Y(X) about a period in X
      SUBROUTINE FOLDLOTS_FOLDU(NPTS, X, AXWID, Y, VAR,
     :                    PERIOD, PINC, NPER,
     :                    X0, NBINS, W, IB, BINAV, VARB,
     :			  NDOF, CHI_VS_P, IERR, CHISQ, STATUS)
*    Description :
*
*	Folds the data Y(X) about a range of periods in X. Data are binned into
*	NBINS phase bins and the mean and standard deviation of Y computed
*	for each bin. X0 corresponds to phase zero. Non-weighted means used.

*    Method :
*      Data is folded at a range of periods, the best one is selected
*      (highest chi-squared) and the fold repeated once more at this
*      single period for output.*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     David J. Allan
*    History :
*     12 Aug 83 : FOLDS V.1
*     26 Mar 86 : TIM_FOLD V.1
*     16 Sep 86 : Phases corrected for negative X
*     13 Jan 90 : Recode in FORTRAN 77!!! (BHVAD::DJA)
*      4 Apr 90 : Accept nD datasets (BHVAD::SRD)
*        Jul 90 : Bug fixes - array zeroing & epoch in sec (SOTON::AJN)
*        Sep 90 : Conversion to range folding (SOTON::AJN)
*        Jun 91 : Rationalised the input params (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      INTEGER            NPTS                   ! No. of data points
      REAL               X(NPTS)                ! Independent variable
      REAL               AXWID(NPTS)
      REAL               Y(NPTS)		! Dependent variable
      REAL               VAR(NPTS)		! Variance in Y
      INTEGER		 NPER			! No. of periods to fold at
      INTEGER            NBINS                  ! Number of phase bins

      REAL               PERIOD                 ! Folding period
      REAL 		 PINC			! Period increment
      REAL               X0                     ! Epoch of phase zero
      REAL               W(NBINS)		! work space
*
*    Export :

      REAL               CHI_VS_P(NPER)		! Array of chi-values
      INTEGER            IB(NBINS)		! Number of points per bin
      INTEGER            IERR                   ! 0=O.K. 1=less than 2 points
                                                ! in some bins. For 1 point
                                                ! BMNVAR is zero, for 0 BMN too.
      REAL               BINAV(NBINS)		! Mean Y value unweighted
      REAL               VARB(NBINS)
      REAL               CHISQ			! Chi-squared fit of means to
                                                ! constants intensity.
      INTEGER            NDOF			! Number of degrees of freedom
                                                ! of CHISQ - NBINS-1 unless
                                                ! some bins contain <2 points
*    Status :
      INTEGER            STATUS
*
*    Local variables
*
      INTEGER            A
      INTEGER            I,J,NB,PCOUNTER
      REAL               DVAR, YBAR,PHA
      REAL               SUMVAR
      REAL		 CHIBEST
      REAL 	 	 PSTART,PBEST
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Convert epoch of phase zero to seconds
      X0=X0*86400.0

*    Range stuff

      PSTART=PERIOD
      CHIBEST=0.0

      DO PCOUNTER=1,NPER
	PERIOD=PSTART+(PINC*REAL(PCOUNTER-1))

*    Initialise
        CALL ARR_INIT1I(0,NBINS,IB,STATUS)
        CALL ARR_INIT1R(0.0,NBINS,W,STATUS)
        CALL ARR_INIT1R(0.0,NBINS,VARB,STATUS)
        YBAR=0.0
        IERR=0

*    Bin up data
        DO A=1,NPTS
          PHA=AMOD((X(A)-X0)/PERIOD,1.0)
          IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
          NB = INT( PHA*NBINS ) + 1
*
	  IB(NB) = IB(NB) + 1
	  W(NB)=W(NB)+Y(A)
        END DO


        DO NB=1,NBINS
	  IF(IB(NB).LT.2)THEN
		IERR=1
          ELSE
		BINAV(NB)=W(NB)/IB(NB)
          ENDIF
          NDOF=NBINS-1
        END DO


        DO A=1,NPTS
          PHA=AMOD((X(A)-X0)/PERIOD,1.0)
          IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
          NB = INT( PHA*NBINS ) + 1
*
	  IF ( IB(NB).GE.2 ) THEN
             VARB(NB)=VARB(NB)+((Y(A)-BINAV(NB))**2)/
     :            (IB(NB)*(IB(NB)-1))
          ENDIF
        END DO

	DO A=1,NBINS
		IF(IB(A).LT.2)THEN
                      IERR=1
                      NDOF=NDOF-1
		ELSE
                      YBAR=YBAR+BINAV(A)
		ENDIF
        ENDDO

        YBAR=YBAR/NBINS

	CHISQ=0.0

        DO A=1,NBINS
*
* Change by RDS to stop /0 errors - may not be mathematically correct.
          IF ( IB(A) .GE. 2 )THEN
            IF (VARB(A) .NE. 0.0) THEN
		CHISQ=CHISQ+(((BINAV(A)-YBAR)**2)/VARB(A))
D            ELSE
D                WRITE(*,*)'Varb is zero - ', Y(A),BINAV(NB)
	    ENDIF
	  ENDIF
        ENDDO

	CHI_VS_P(PCOUNTER)=CHISQ

	IF (CHISQ.GE.CHIBEST) THEN
		CHIBEST = CHISQ
	        PBEST	= PERIOD
	ENDIF

*    end of period loop
      ENDDO

      PERIOD = PBEST
      CHISQ  = CHIBEST

*    do the binning again at the best period so that output is the best fold

*    Initialise
      CALL ARR_INIT1I(0,NBINS,IB,STATUS)
      CALL ARR_INIT1R(0.0,NBINS,W,STATUS)
      CALL ARR_INIT1R(0.0,NBINS,VARB,STATUS)
      YBAR=0.0
      IERR=0

*    Bin up data
      DO A=1,NPTS
        PHA=AMOD((X(A)-X0)/PERIOD,1.0)
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
        NB = INT( PHA*NBINS ) + 1
	IB(NB) = IB(NB) + 1
	W(NB)=W(NB)+Y(A)
      END DO


      DO NB=1,NBINS
	IF(IB(NB).LT.2)THEN
		IERR=1
        ELSE
		BINAV(NB)=W(NB)/IB(NB)
	ENDIF
        NDOF=NBINS-1
      END DO

      DO A=1,NPTS
        PHA=AMOD((X(A)-X0)/PERIOD,1.0)
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
        NB = INT( PHA*NBINS ) + 1
	IF(IB(NB).GE.2)THEN
			VARB(NB)=VARB(NB)+((Y(A)-BINAV(NB))**2)/
     :                           (IB(NB)*(IB(NB)-1))
	ENDIF
      END DO

      X0=X0/86400.0

      END
