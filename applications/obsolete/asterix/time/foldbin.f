      SUBROUTINE FOLDBIN( STATUS )
*+
*  Name:
*     FOLDBIN

*  Purpose:
*     Folds binned data into phase bins

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL FOLDBIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Folds nD data into phase bins and generates a folded nD data set
*     containing the bin means (weighted if errors are available) and standard
*     deviations, also the chi-squared fits to constant levels.
*     Note - if axis zero is defined then this must be incorporated in the
*     phase zero epoch.

*  Usage:
*     foldbin {parameter_usage}

*  Environment Parameters:
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
*     The basic technique is straightforward.
*     Arrays are mapped so there is no size limitation.
*     Bad quality data are excluded.
*     If time is not AXIS1 then the axes are swapped.

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
*     foldbin, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmngham)
*     SRD: Simon Duck (University of Birmngham)
*     AJN: Andrew Norton (University of Southampton)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Mar 1986 (TJP):
*        Original version
*     29 Aug 1986 (JCMP):
*        Renamed
*     10 Sep 1987 V1.0-0 (PLA):
*        Accepts primitive input object. Checks validity of locators before
*        attempting to annul them. Displays name of input object. Displays
*        AXIS(1) UNITS. Displays max No of output bins (and makes sure this
*        is not exceeded). Program suggests defaults for phase zero epoch and
*        No of phase bins (= No of input bins in phase period). ACTIONS added
*        to History record.
*     10 Dec 1989 V1.0-1 (DJA):
*        Ghastly spelling mistakes removed! Axis(1) text now correct.
*     12 Jan 1990 V1.0-3 (DJA):
*        Re-write to use ASTLIB! No longer generates negative variances.
*        Doesn't re-prompt after invalid input.
*     10 Apr 1990 V1.0-4 (DJA):
*        Accepts nD data and folds each dimension over time. If time is not
*        the AXIS(1) then the axes are swapped so that it is.
*     19 Jul 1990 V1.0-5 (AJN):
*        Bugs fixed in TIM_FOLDU : arrays  BVAR & YBAR had
*        not been initialised to zero
*     24 Jul 1990 V1.0-6 (AJN):      -
*        Serious bug fixed - epoch zero was carried over to subroutines in
*        days and calculation was done in seconds - now converted to seconds
*        before use, also used in dble precision for accuracy with short
*        periods & long offsets (AJN)
*     31 Jul 1991 V1.0-7 (RDS):
*        Prints a warning when the max. no. of phase bine
*        for ordinary sampling is used.
*      7 Oct 1992 V1.7-0 (DJA):
*        TIM_FOLDx routine renamed FOLDBIN_FOLDx to avoid clash with FOLDLOTS
*        routines of same name but different function
*     30 Mar 1993 V1.7-1 (DJA):
*        Updated by someone (not me!)
*     17 Aug 1993 V1.7-2 (DJA):
*        Trivial changes to ARR_REG routines
*     13 Dec 1995 V2.0-0 (DJA):
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
      INCLUDE 'QUAL_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER            MAXHTXT                ! Maximum amount of history
         PARAMETER       (MAXHTXT = 10)

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'FOLDBIN Version V2.0-0' )

*  Local Variables:
      CHARACTER*132      HTXT(MAXHTXT)          ! History text
      CHARACTER*80       UNITS                  ! Value of AXIS(1) UNITS.

      DOUBLE PRECISION   BASE_TAI               ! Atomic time
      DOUBLE PRECISION   ZEROEPOCH              ! Epoch of phase zero
      DOUBLE PRECISION   ZEROOFFSET             ! offset of zeroepoch from base_tai

      REAL               BASE, SCALE            ! Axis attributes
      REAL               PERIOD                 ! Folding period
      REAL               CHISQ                  ! Chi-squared fit to constant
                                                ! level (i.e.phase indept.)
      REAL			SPARR(2)		! Spaced array info

      INTEGER			FFID			! Folded dataset id
      INTEGER			IFID			! Input dataset id
      INTEGER			TIMID			! Timing info
      INTEGER            HLEN                   ! Length of a history string
      INTEGER            I                      ! Dummy variable for loops.
      INTEGER            N
      INTEGER            NDATA                  ! No.of data points
      INTEGER            IERR                   ! Error flag from TIM_FOLD
      INTEGER            NBINS                  ! No. of phase bins
      INTEGER            NELM
      INTEGER            NELM2
      INTEGER            NACTDIM                ! Dimensionality of data
      INTEGER            DPTR                   ! Pointer to data array
      INTEGER            XPTR                   ! Pointer to axis data
      INTEGER            XWPTR                  ! Pointer to width data
      INTEGER            VPTR                   ! Pointer to variances
      INTEGER            QPTR                   ! Pointer to quality array
      INTEGER            FDPTR                  ! Pointer to folded data array
      INTEGER            FVPTR                  ! Pointer to folded error array
      INTEGER            FNPTR                  ! Pointer to folded no. array
      INTEGER            FQPTR                  ! Pointer to folded quality array
      INTEGER            FFPTR
      INTEGER            FCPTR
      INTEGER            WPTR                   ! Pointer to work array
      INTEGER            PTR
      INTEGER            QAPTR
      INTEGER            VAPTR
      INTEGER            YBARPTR
      INTEGER            SUMPTR
      INTEGER            NDOF                   ! No. of d.o.f. for chi-squared
      INTEGER            IDIM(ADI__MXDIM)       ! Size of each dimension
      INTEGER            ODIM(ADI__MXDIM)
      INTEGER            OAX(ADI__MXDIM)
      INTEGER            	AXDIM			! Time axis number
      INTEGER            INBINS                 ! Number of input bins/ period.
      INTEGER            	USED                   	! # history text lines

      BYTE               	MASK                   	! Quality mask

      LOGICAL            	OK                 	! things ok?
      LOGICAL            	VOK                    	! Variance present?
      LOGICAL            	QOK                    	! Data quality present?
      LOGICAL            	REGULAR                	! Is input regularly binned?
      LOGICAL            	BOK                    	! BASE_TAI present
      LOGICAL            	WEIGHT			!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Obtain data object, access and check it.
      CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check and map input data
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, ODIM, NACTDIM, STATUS )
      IF ( OK ) THEN

*    Map input data
        CALL BDI_MAPR( IFID, 'Data', 'READ', PTR, STATUS )

*    Locate time axis
        CALL BDI0_FNDAXC( IFID, 'T', AXDIM, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          IF ( NACTDIM .GT. 1 ) THEN
            CALL MSG_PRNT( 'WARNING : Unable to recognise a time '/
     :            /'axis, assuming its axis 1' )
          END IF
          AXDIM = 1
        END IF

*    Swap axes if necessary
         IF ( AXDIM .NE. 1 ) THEN
            OAX(1)=AXDIM
            DO I=2,AXDIM
               OAX(I)=I-1
            ENDDO
            DO I=AXDIM+1,ADI__MXDIM
               OAX(I)=I
            ENDDO
            IDIM(1)=ODIM(AXDIM)
            DO I=2,AXDIM
               IDIM(I)=ODIM(I-1)
            ENDDO
            DO I=AXDIM+1,NACTDIM
               IDIM(I)=ODIM(I)
            ENDDO
            CALL DYN_MAPR(NACTDIM,IDIM,DPTR,STATUS)
            DO I=NACTDIM,ADI__MXDIM
               IF(ODIM(I).EQ.0)ODIM(I)=1
               IF(IDIM(I).EQ.0)IDIM(I)=1
            ENDDO
            CALL AR7_AXSWAP_R(ODIM,%VAL(PTR),OAX,IDIM,%VAL(DPTR),STATUS)
            CALL DYN_UNMAP(PTR,STATUS)
         ELSE
            DPTR=PTR
            DO I=1,NACTDIM
              OAX(I)=I
              IDIM(I)=ODIM(I)
            ENDDO
         ENDIF
         NDATA=IDIM(1)
      ELSE
         CALL MSG_PRNT( 'FATAL ERROR: No data!' )
      END IF

*  Check number of dimensions
      CALL MSG_SETI('DIMS',NACTDIM)
      CALL MSG_PRNT( 'Data is ^DIMS dimensional' )

*  Create a folded data object.
      CALL USI_CREAT( 'OUT', ADI__NULLID, FFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  If AXIS(1) UNITS are defined, then display them.
      CALL BDI_AXGET0C( IFID, AXDIM, 'Units', UNITS, STATUS )
      IF ( UNITS .GT. ' ' ) THEN
        CALL MSG_SETC( 'UNITS', UNITS )
        CALL MSG_PRNT( 'TIME UNITS are ^UNITS' )
      END IF

*  Get period
      CALL USI_GET0R( 'PERIOD', PERIOD, STATUS )

*  Define base epoch
      CALL TCI_GETID( IFID, TIMID, STATUS )
      CALL ADI_THERE( TIMID, 'TAIObs', BOK, STATUS )
      IF ( BOK ) THEN
        CALL ADI_CGET0D( TIMID, 'TAIObs', BASE_TAI, STATUS )
      ELSE
        CALL MSG_PRNT( 'No atomic time: set to zero' )
        BASE_TAI = 0.0D0
      ENDIF
      CALL USI_DEF0D( 'EPOCH', BASE_TAI, STATUS )
      CALL USI_GET0D( 'EPOCH', ZeroEpoch, STATUS )
      ZEROOFFSET = (ZEROEPOCH - BASE_TAI) * 86400.D0

*  Check axis and data match
      CALL BDI_AXCHK( IFID, AXDIM, 'Data', OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'No time axis data'
     :                //' - proceeding assuming unit spacing' )
      END IF
      CALL BDI_AXMAPR( IFID, AXDIM, 'Data', 'READ', XPTR, STATUS )
      CALL BDI_AXMAPR( IFID, AXDIM, 'Width', 'READ', XWPTR, STATUS )
      CALL ARR_CHKREG( %VAL(XPTR), INBINS, REGULAR, BASE, SCALE,
     :                 STATUS )

      CALL USI_GET0I( 'BINS', NBINS, STATUS )

      IF ( REGULAR .AND. (NBINS .GT. INBINS) ) THEN
         CALL MSG_PRNT('The data is being oversampled - this is'/
     &               /' only valid if the time series is long compared'/
     &               /' to the folding period')
      END IF

*  Ask if weighted mean required
      CALL USI_GET0L('WEIGHT',WEIGHT,STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Data error ... set up scratch area for variances
      CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'READ', VAPTR, STATUS )
        IF ( AXDIM .NE. 1 ) THEN
          CALL DYN_MAPR( NACTDIM, IDIM, VPTR, STATUS )
          CALL AR7_AXSWAP_R( ODIM, %VAL(VAPTR), OAX, IDIM,
     :                                %VAL(VPTR), STATUS )
          CALL DYN_UNMAP( VAPTR, STATUS )
        ELSE
          VPTR = VAPTR
        END IF
      ELSE
        CALL DYN_MAPR(NACTDIM,IDIM,VPTR,STATUS)
        CALL ARR_SUMDIM(NACTDIM,IDIM,NELM)
        CALL ARR_INIT1R(0.0,NELM,%VAL(VPTR),STATUS)
      END IF

*    and data quality
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK ) THEN
        CALL BDI_GET0UB( IFID, 'QualityMask', MASK, STATUS )
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', QAPTR, STATUS )
        IF ( AXDIM .NE. 1 ) THEN
          CALL DYN_MAPL( NACTDIM, IDIM, QPTR, STATUS )
          CALL AR7_AXSWAP_B( ODIM, %VAL(QAPTR), OAX, IDIM,
     :                                %VAL(QPTR), STATUS )
          CALL BDI_UNMAP( IFID, 'QualityMask', QAPTR, STATUS )
        ELSE
          QPTR = QAPTR
        ENDIF
      ELSE
        MASK = QUAL__MASK
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_FLUSH( STATUS )
      END IF

*  Map workspace
      IDIM(1) = NBINS
      CALL DYN_MAPR(NACTDIM,IDIM,WPTR,STATUS)
      CALL DYN_MAPR(NACTDIM,IDIM,YBARPTR,STATUS)
      CALL DYN_MAPR(NACTDIM,IDIM,SUMPTR,STATUS)

*  Create interface object
      CALL BDI_LINK( 'BinDS', FFID, NACTDIM, IDIM, FFID, STATUS )
      CALL BDI_MAPR( FFID, 'Data', 'WRITE', FDPTR, STATUS )
      CALL BDI_MAPR( FFID, 'Variance', 'WRITE',FVPTR, STATUS )
      CALL BDI_MAPUB( FFID, 'Quality', 'WRITE', FQPTR, STATUS )
      CALL BDI_PUT0UB( FFID, 'QualityMask', MASK, STATUS )

*  Workspace
      CALL DYN_MAPI( NACTDIM, IDIM, FNPTR, STATUS )
      IDIM(1)=1
      CALL DYN_MAPR( NACTDIM, IDIM, FCPTR, STATUS )
      CALL DYN_MAPI( NACTDIM, IDIM, FFPTR, STATUS )
      IDIM(1)=NBINS

*  Set output quality to GOOD (as all bad data has been excluded)
      CALL ARR_SUMDIM( NACTDIM, IDIM, NELM2 )
      CALL ARR_INIT1B( QUAL__GOOD, NELM2, %VAL(FQPTR), STATUS )

*  Pad dimensions to 7-D
      IDIM(1)=NDATA
      CALL AR7_PAD( 1, IDIM, STATUS )

*    Fold
      IF(WEIGHT)THEN
        CALL FOLDBIN_FOLDW(%VAL(XPTR),%VAL(XWPTR),%VAL(DPTR),
     :             %VAL(VPTR),NACTDIM,
     :             IDIM,IDIM(1),IDIM(2),IDIM(3),IDIM(4),IDIM(5),
     :             IDIM(6),IDIM(7),QOK,%VAL(QPTR),PERIOD,
     :             ZEROOFFSET,NBINS,%VAL(WPTR),
     :             %VAL(YBARPTR),%VAL(SUMPTR),%VAL(FNPTR),
     :             %VAL(FDPTR),%VAL(FVPTR),%VAL(FCPTR),
     :             %VAL(FFPTR),%VAL(FQPTR),IERR,CHISQ,NDOF,STATUS)
      ELSE
        CALL FOLDBIN_FOLDU(%VAL(XPTR),%VAL(XWPTR),%VAL(DPTR),
     :             %VAL(VPTR),NACTDIM,
     :             IDIM,IDIM(1),IDIM(2),IDIM(3),IDIM(4),IDIM(5),
     :             IDIM(6),IDIM(7),QOK,%VAL(QPTR),PERIOD,
     :             ZEROOFFSET,NBINS,%VAL(WPTR),
     :             %VAL(YBARPTR),%VAL(SUMPTR),%VAL(FNPTR),
     :             %VAL(FDPTR),%VAL(FVPTR),%VAL(FCPTR),
     :             %VAL(FFPTR),%VAL(FQPTR),IERR,CHISQ,NDOF,STATUS)
      ENDIF
      IF ( IERR .NE. 0 ) THEN
        CALL MSG_SETI( 'IERR', IERR )
        CALL MSG_PRNT( 'Bad phase bin - IERR = ^IERR' )
      END IF
      SPARR(1) = 0.5/REAL(NBINS)
      SPARR(2) = 1.0/REAL(NBINS)
      CALL BDI_AXPUT1R( FFID, 1, 'SpacedData', 2, SPARR, STATUS )

*  Write components to output object
      DO N = 2, NACTDIM
        CALL BDA_AXCOPY( IFID, I, ' ', OAX(N), STATUS )
      END DO
      CALL BDI_COPY( IFID, 'Title,Label,Units', FFID, ' ', STATUS )
      IF (STATUS .NE. SAI__OK) THEN
        CALL ERR_FLUSH(STATUS)
      END IF
      CALL BDI_AXPUT0C( FFID, 1, 'Label', 'Phase', STATUS )
      CALL BDI_AXPUT0L( FFID, 1, 'Normalised', .TRUE., STATUS )

*  Copy other info from input to output file
      CALL UDI_COPANC( IFID, 'grf', FFID, STATUS )

*  Write auxilliary stuff
      CALL AUI_PUTNI( FFID, 'N_OCCUPANTS', NACTDIM, IDIM, %VAL(FNPTR),
     :                STATUS )
      IDIM(1) = 1
      CALL AUI_PUTNR( FFID, 'CHISQUARED', NACTDIM, IDIM, %VAL(FCPTR),
     :                STATUS )
      CALL AUI_PUTNI( FFID, 'DEG_OF_FREEDOM', NACTDIM, IDIM,
     :                %VAL(FFPTR), STATUS )
      IDIM(1)=NBINS

*  Add history records
      CALL HSI_COPY( IFID, FFID, STATUS )
      CALL HSI_ADD( FFID, VERSION, STATUS )
      HTXT(1) = 'Input {INP}'
      HTXT(2) = 'Folded output {OUT}'
      CALL MSG_SETR( 'PERIOD', PERIOD )
      CALL MSG_SETC( 'UNITS', UNITS )
      CALL MSG_MAKE( 'The data has been folded into a period of '/
     :                          /'^PERIOD ^UNITS', HTXT(3), HLEN )
      CALL MSG_SETD( 'EPOCH', ZEROEPOCH )
      CALL MSG_MAKE('The epoch of phase zero was ^EPOCH', HTXT(4),
     :                                                       HLEN)
      USED = MAXHTXT
      CALL USI_TEXT( 4, HTXT, USED, STATUS )
      CALL HSI_PTXT(FFID,USED,HTXT,STATUS)

*  Output CHISQR value to user if 1D dataset
      IF ( NACTDIM .EQ. 1 ) THEN
        CALL MSG_SETR('CHISQR',CHISQ )
        CALL MSG_SETI('NDOF',NDOF )
        CALL MSG_PRNT( 'Chi squared value of fit to mean = ^CHISQR'/
     :                 /' with ^NDOF degrees of freedom')
      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE FOLDBIN_FOLDW(X,AXWID,Y,VAR,NACTDIM,IDIM,L1,L2,L3,
     :                    L4,L5,L6,L7,QOK,QUAL,P,X0,NBINS,W,YBAR,
     :                    SUMVAR,IB,BMN,BMNVAR,CHISQ,NDOF,FQUAL,
     :                    IERR,CHI,ND,STATUS)
*    Description :
*
*	Folds the data Y(X) about a period P in X. Data are binned into
*	NBINS phase bins and the mean and standard deviation of Y computed
*	for each bin. X0 corresponds to phase zero. Weighted means used.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
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
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER            NACTDIM                ! Number of input data dims
      INTEGER            IDIM(ADI__MXDIM)       ! Dimensions of data points
      INTEGER            L1,L2,L3,L4,L5,L6,L7
      INTEGER            L8
      INTEGER            A,B,C,D,E,F,G
      REAL               X(L1)                   ! Independent variable
      REAL               AXWID(L1)
      REAL               Y(L1,L2,L3,L4,L5,L6,L7) ! Dependent variable
      REAL               VAR(L1,L2,L3,L4,L5,L6,L7)! Variance in Y
      LOGICAL            QUAL(L1,L2,L3,L4,L5,L6,L7)! Quality
      LOGICAL            QOK
      INTEGER            NBINS                  ! Number of phase bins
      REAL               P                      ! Folding period
      DOUBLE PRECISION   X0                     ! offset of Epoch of phase zero
      REAL               W(NBINS,L2,L3,L4,L5,L6,L7)! work space
*
*    Export :
*
      REAL               CHI
      INTEGER            IB(NBINS,L2,L3,L4,L5,L6,L7)! Number of points per bin
      INTEGER            IERR                   ! 0=O.K. 1=less than 2 points
                                                ! in some bins. For 1 point
                                                ! BMNVAR is zero, for 0 BMN too.
      REAL               BMN(NBINS,L2,L3,L4,L5,L6,L7)! Mean Y value weighted
      REAL               BMNVAR(NBINS,L2,L3,L4,L5,L6,L7)! Standard deviation per bin
      REAL               CHISQ(1,L2,L3,L4,L5,L6,L7)! Chi-squared fit of means to
                                                ! constants intensity.
      INTEGER            NDOF(1,L2,L3,L4,L5,L6,L7)! Number of degrees of freedom
                                                ! of CHISQ - NBINS-1 unless
                                                ! some bins contain <2 points
      BYTE               FQUAL(NBINS,L2,L3,L4,L5,L6,L7)
      INTEGER            ND
*    Status :
      INTEGER            STATUS
*
*    Local variables
*
      INTEGER            NB,NUM
      REAL               DVAR, YBAR(1,L2,L3,L4,L5,L6,L7), PHA
      REAL               SUMVAR(1,L2,L3,L4,L5,L6,L7)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      L8=NBINS
      NUM=NBINS*L2*L3*L4*L5*L6*L7
      CALL ARR_INIT1I(0,NUM,IB,STATUS)
      CALL ARR_INIT1R(0.0,NUM,BMN,STATUS)
      CALL ARR_INIT1R(0.0,NUM,BMNVAR,STATUS)
      CALL ARR_INIT1R(0.0,NUM,W,STATUS)
      IERR=0

*    Bin up data
      DO A=1,L1
        PHA=REAL(DMOD((DBLE(X(A))-X0)/DBLE(P),1.D0))
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
        NB = INT( PHA*NBINS ) + 1
        DO B=1,L2
          DO C=1,L3
            DO D=1,L4
              DO E=1,L5
                DO F=1,L6
                  DO G=1,L7
                    IF(.NOT.QOK.OR.(QOK.AND.QUAL(A,B,C,D,E,F,G)))THEN
                      IB(NB,B,C,D,E,F,G) = IB(NB,B,C,D,E,F,G) + 1
                      IF ( VAR(A,B,C,D,E,F,G).LE.0.0 ) THEN
                        DVAR=1/(AXWID(A)*AXWID(A))
                      ELSE
                        DVAR=VAR(A,B,C,D,E,F,G)
	              END IF
                      BMN(NB,B,C,D,E,F,G)=BMN(NB,B,C,D,E,F,G)+
     :                                       Y(A,B,C,D,E,F,G)/DVAR
                      W(NB,B,C,D,E,F,G)=W(NB,B,C,D,E,F,G)+1.0/DVAR
                    ENDIF
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
*    Analyse phase bins
      DO B=1,L2
        DO C=1,L3
          DO D=1,L4
            DO E=1,L5
              DO F=1,L6
                DO G=1,L7
                  YBAR(1,B,C,D,E,F,G)=0.0
                  SUMVAR(1,B,C,D,E,F,G)=0.0
                  NDOF(1,B,C,D,E,F,G)=NBINS-1
                  DO A=1,NBINS
                    IF ( IB(A,B,C,D,E,F,G) .LT. 2 )THEN
                      IERR=1
                      FQUAL(A,B,C,D,E,F,G)=1
	              NDOF(1,B,C,D,E,F,G)=NDOF(1,B,C,D,E,F,G)-1
                    ELSE
                      BMN(A,B,C,D,E,F,G)=BMN(A,B,C,D,E,F,G)/
     :                                            W(A,B,C,D,E,F,G)
                      BMNVAR(A,B,C,D,E,F,G) = 1.0/W(A,B,C,D,E,F,G)
                      YBAR(1,B,C,D,E,F,G)=YBAR(1,B,C,D,E,F,G)+
     :                      BMN(A,B,C,D,E,F,G)/BMNVAR(A,B,C,D,E,F,G)
                      SUMVAR(1,B,C,D,E,F,G)=SUMVAR(1,B,C,D,E,F,G)+
     :                                     1.0/BMNVAR(A,B,C,D,E,F,G)
	            END IF
                  END DO
                  YBAR(1,B,C,D,E,F,G)=YBAR(1,B,C,D,E,F,G)/
     :                                          SUMVAR(1,B,C,D,E,F,G)
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Find CHISQ
      DO B=1,L2
        DO C=1,L3
          DO D=1,L4
            DO E=1,L5
              DO F=1,L6
                DO G=1,L7
                  CHISQ(1,B,C,D,E,F,G)=0.0
                  DO A=1,NBINS
                  IF ( IB(A,B,C,D,E,F,G) .GE. 2 )THEN
                  CHISQ(1,B,C,D,E,F,G)=CHISQ(1,B,C,D,E,F,G)
     :            +((BMN(A,B,C,D,E,F,G)-YBAR(1,B,C,D,E,F,G))**2
     :            /BMNVAR(A,B,C,D,E,F,G))
                  END IF
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
*
      IF (NACTDIM.EQ.1) THEN
         CHI=CHISQ(1,1,1,1,1,1,1)
         ND = NDOF(1,1,1,1,1,1,1)
      ENDIF
*
      END



*+  FOLDBIN_FOLDU - folds data Y(X) about a period in X
      SUBROUTINE FOLDBIN_FOLDU(X,AXWID,Y,VAR,NACTDIM,IDIM,L1,L2,L3,
     :                    L4,L5,L6,L7,QOK,QUAL,P,X0,NBINS,W,YBAR,
     :                    SUMVAR,IB,BINAV,VARB,CHISQ,NDOF,FQUAL,
     :                    IERR,CHI,ND,STATUS)
*    Description :
*
*	Folds the data Y(X) about a period P in X. Data are binned into
*	NBINS phase bins and the mean and standard deviation of Y computed
*	for each bin. X0 corresponds to phase zero. Non-weighted means used.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
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
*     23 Jul 90 : array initialisation bugs fixed (SOTON::AJN)
*     24 Jul 90 : epoch of phase zero bug fixed (SOTON::AJN)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER            NACTDIM                ! Number of input data dims
      INTEGER            IDIM(ADI__MXDIM)       ! Dimensions of data points
      INTEGER            L1,L2,L3,L4,L5,L6,L7
      INTEGER            L8
      INTEGER            A,B,C,D,E,F,G
      REAL               X(L1)                   ! Independent variable
      REAL               AXWID(L1)
      REAL               Y(L1,L2,L3,L4,L5,L6,L7) ! Dependent variable
      REAL               VAR(L1,L2,L3,L4,L5,L6,L7)! Variance in Y
      LOGICAL            QUAL(L1,L2,L3,L4,L5,L6,L7)! Quality
      LOGICAL            QOK
      INTEGER            NBINS                  ! Number of phase bins
      REAL               P                      ! Folding period
      DOUBLE PRECISION   X0                     ! offset of Epoch of phase zero
      REAL               W(NBINS,L2,L3,L4,L5,L6,L7)! work space
      REAL               SUMVAR(1,L2,L3,L4,L5,L6,L7)
*
*    Export :
*
      REAL               CHI
      INTEGER            IB(NBINS,L2,L3,L4,L5,L6,L7)! Number of points per bin
      INTEGER            IERR                   ! 0=O.K. 1=less than 2 points
                                                ! in some bins. For 1 point
                                                ! BMNVAR is zero, for 0 BMN too.
      REAL               BINAV(NBINS,L2,L3,L4,L5,L6,L7)! Mean Y value weighted
      REAL               VARB(NBINS,L2,L3,L4,L5,L6,L7)
      REAL               CHISQ(1,L2,L3,L4,L5,L6,L7)! Chi-squared fit of means to
                                                ! constants intensity.
      INTEGER            NDOF(1,L2,L3,L4,L5,L6,L7)! Number of degrees of freedom
                                                ! of CHISQ - NBINS-1 unless
                                                ! some bins contain <2 points
      BYTE FQUAL(NBINS,L2,L3,L4,L5,L6,L7)
      INTEGER ND
*    Status :
      INTEGER            STATUS
*
*    Local variables
*
      INTEGER            NB,NUM,NUM1,NBAD
      REAL               YBAR(1,L2,L3,L4,L5,L6,L7), PHA
      LOGICAL LMESS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      L8=NBINS
      NUM=NBINS*L2*L3*L4*L5*L6*L7
      NUM1=1*L2*L3*L4*L5*L6*L7
      CALL ARR_INIT1I(0,NUM,IB,STATUS)
      CALL ARR_INIT1R(0.0,NUM,W,STATUS)
      CALL ARR_INIT1R(0.0,NUM,VARB,STATUS)
      CALL ARR_INIT1R(0.0,NUM1,YBAR,STATUS)
      IERR=0

*    Bin up data
      DO A=1,L1
        PHA=REAL(DMOD((DBLE(X(A))-X0)/DBLE(P),1.D0))
*       PHA=AMOD((X(A)-X0)/P,1.0)
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
        NB = INT( PHA*NBINS ) + 1
        DO B=1,L2
          DO C=1,L3
            DO D=1,L4
              DO E=1,L5
                DO F=1,L6
                  DO G=1,L7
                    IF(.NOT.QOK.OR.(QOK.AND.QUAL(A,B,C,D,E,F,G)))THEN
                      IB(NB,B,C,D,E,F,G) = IB(NB,B,C,D,E,F,G) + 1
                      W(NB,B,C,D,E,F,G)=W(NB,B,C,D,E,F,G)+
     :                                   Y(A,B,C,D,E,F,G)
                    ENDIF
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO


      DO NB=1,NBINS
         DO B=1,L2
           DO C=1,L3
             DO D=1,L4
               DO E=1,L5
                 DO F=1,L6
                   DO G=1,L7
                     IF(IB(NB,B,C,D,E,F,G).LT.2)THEN
                       IERR=1
                     ELSE
                       BINAV(NB,B,C,D,E,F,G)=W(NB,B,C,D,E,F,G)/
     :                                     IB(NB,B,C,D,E,F,G)
                     ENDIF
                     NDOF(1,B,C,D,E,F,G)=NBINS-1
                   END DO
                 END DO
              END DO
            END DO
          END DO
        END DO
      END DO
      DO A=1,L1
        PHA=REAL(DMOD((DBLE(X(A))-X0)/DBLE(P),1.D0))
*       PHA=AMOD((X(A)-X0)/P,1.0)
        IF ( PHA .LT. 0.0 ) PHA = PHA + 1.0
        NB = INT( PHA*NBINS ) + 1
        DO B=1,L2
          DO C=1,L3
            DO D=1,L4
              DO E=1,L5
                DO F=1,L6
                  DO G=1,L7
                    IF(.NOT.QOK.OR.(QOK.AND.QUAL(A,B,C,D,E,F,G)))THEN
                      IF(IB(NB,B,C,D,E,F,G).GE.2)THEN
                       VARB(NB,B,C,D,E,F,G)=VARB(NB,B,C,D,E,F,G)+
     :                 ((Y(A,B,C,D,E,F,G)-BINAV(NB,B,C,D,E,F,G))**2)/
     :                 (IB(NB,B,C,D,E,F,G)*(IB(NB,B,C,D,E,F,G)-1))
                      ENDIF
                    ENDIF
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      DO B=1,L2
        DO C=1,L3
          DO D=1,L4
            DO E=1,L5
              DO F=1,L6
                DO G=1,L7
                  NBAD = 0
                  DO A=1,NBINS
                    IF(IB(A,B,C,D,E,F,G).LT.2)THEN
                      IERR=1
                      NDOF(1,B,C,D,E,F,G)=NDOF(1,B,C,D,E,F,G)-1
                      FQUAL(A,B,C,D,E,F,G)=1
                      NBAD = NBAD + 1
                    ELSE
                      YBAR(1,B,C,D,E,F,G)=YBAR(1,B,C,D,E,F,G)+
     :                          BINAV(A,B,C,D,E,F,G)
                    ENDIF
                  ENDDO
*
                  IF ( (NBINS - NBAD) .GT. 0 ) THEN
                    YBAR(1,B,C,D,E,F,G)=YBAR(1,B,C,D,E,F,G)/(NBINS-NBAD)
                  ENDIF
*
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      DO B=1,L2
        DO C=1,L3
          DO D=1,L4
            DO E=1,L5
              DO F=1,L6
                DO G=1,L7
                  CHISQ(1,B,C,D,E,F,G)=0.0
                  DO A=1,NBINS
                    IF(IB(A,B,C,D,E,F,G).GT.2)THEN
                      IF (VARB(A,B,C,D,E,F,G) .GT. 0.0) THEN
                        CHISQ(1,B,C,D,E,F,G)=CHISQ(1,B,C,D,E,F,G)
     :                  +(((BINAV(A,B,C,D,E,F,G)-
     :                  YBAR(1,B,C,D,E,F,G))**2)/VARB(A,B,C,D,E,F,G))
                      ELSE
*
*         If variances are zero output message once
                        IF (.NOT. LMESS) THEN
                           CALL MSG_PRNT('** VARB is zero - CHISQ may'/
     &                           /' not be correct in output file ** ')
*
                           LMESS = .TRUE.
                         ENDIF
*
                      ENDIF
                    ENDIF
                  ENDDO
                END DO
              END DO
            END DO
          END DO
        END DO
      END DO
*
      IF (NACTDIM.EQ.1) THEN
         CHI=CHISQ(1,1,1,1,1,1,1)
         ND=NDOF(1,1,1,1,1,1,1)
      ENDIF
*
      END
