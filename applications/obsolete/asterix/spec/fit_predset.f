*+  FIT_PREDSET - Sets up components of <PREDICTION> structure
	SUBROUTINE FIT_PREDSET(DLOC,NDS,WORKSPACE,OBDAT,PREDDAT,STATUS)
*    Description :
*     All components of the FORTRAN structure PREDDAT (of type <PREDICTION>),
*     apart from PREDDAT.CONVOLVE which must already be defined, are set up.
*     The model space dimension and bin boundary values are extracted either
*     from the instrument response structure (if PREDDAT.CONVOLVE is TRUE)
*     or from the axis values of the dataset (if it is FALSE).
*     The logical WORKSPACE determines whether the workspace required for
*     chi-squared gradient calculations is set up, this is only required if
*     fitting is to be performed, not for generation of predicted data or
*     evaluation of chi-squared.
*    Method :
*     Note that if instrument convolution is to be performed then the only
*     components needed from OBDAT are OBDAT.NDAT and OBDAT.SETINDEX (which is
*     zero unless the dataset is a spectral set). Otherwise the dimension
*     information is also required.
*    Deficiencies :
*     The only type of instrument response catered for at present is energy
*     response.
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*
*     14 Apr 88 : FIT_MSPACE adapted from FIT_DATGET (TJP)
*      8 Jul 88 : Dimensioning bug fixed (TJP)
*     21 Oct 88 : Renamed to FIT_PREDSET (TJP)
*     22 Feb 89 : ASTERIX88 version (TJP)
*     27 Sep 89 : Mod. to handle scalar axis WIDTH (TJP)
*     15 Mar 94 : Don't allocated DFDPPTR. If NMDAT is large then NPAMAX*NMDAT
*                 can get enormous! (DJA)
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
	INCLUDE 'FIT_PAR'
*    Global variables :
*    Structure definitions :
	INCLUDE 'FIT_STRUC'
*    Import :
	CHARACTER*(DAT__SZLOC) DLOC	! Locator to input dataset
	INTEGER NDS			! Current dataset number
	LOGICAL WORKSPACE		! Set up chisq gradient workspace?
	RECORD /DATASET/ OBDAT		! Observed datasets
*    Import-Export :
	RECORD /PREDICTION/ PREDDAT	! Data predicted by model
*    Export :
*    Status :
	INTEGER STATUS
*    Local constants :
*    Local variables :
	CHARACTER*(DAT__SZLOC) EBLOC	! Energy bound locators
	CHARACTER*(DAT__SZLOC) ERLOC	! Locator to ENERGY_RESP structure
	CHARACTER*(DAT__SZLOC) INSLOC	! Locator to instrument energy response
	INTEGER MBINPTR			! Pointer to model bin centres
	INTEGER MBNDPTR			! Pointer to bounds in response object
	INTEGER I			! Index
	INTEGER NVAL			! No of values mapped
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Instrument folding case (model space bins taken from response structure)
	IF(PREDDAT.CONVOLVE)THEN

*    Get model dimensions
	  CALL BDA_LOCERESP(DLOC,ERLOC,STATUS)

*       If spectral_set then take appropriate slice
	  IF(OBDAT.SETINDEX.GT.0)THEN
	    CALL DAT_CELL(ERLOC,1,OBDAT.SETINDEX,INSLOC,STATUS)
	  ELSE
	    CALL DAT_CLONE(ERLOC,INSLOC,STATUS)
	  ENDIF
	  IF(STATUS.NE.SAI__OK) GO TO 9000

	  CALL INSR_MAP(INSLOC,'ENERGY_SPEC','_REAL','READ',MBINPTR,
     :    PREDDAT.NMDAT,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000
	  PREDDAT.NMDIM=1			! 1D specific
	  PREDDAT.IDIMM(1)=PREDDAT.NMDAT	! 1D specific
	  PREDDAT.NMBOUND=PREDDAT.NMDAT		! 1D specific

*    Set up model space bounds
	  CALL DYN_MAPR(1,PREDDAT.NMBOUND,PREDDAT.MLBNDPTR,STATUS)
	  CALL DYN_MAPR(1,PREDDAT.NMBOUND,PREDDAT.MUBNDPTR,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000
	  CALL INSR_FIND(INSLOC,'ENERGY_BOUNDS',EBLOC,STATUS)
	  IF(STATUS.EQ.SAI__OK)THEN
	    CALL DAT_MAPV(EBLOC,'_REAL','READ',MBNDPTR,NVAL,STATUS)

*       Bounds found, decompose into lower and upper
	    CALL FIT_PREDSET_LUBND(PREDDAT.NMBOUND,%VAL(MBNDPTR),
     :      %VAL(PREDDAT.MLBNDPTR),%VAL(PREDDAT.MUBNDPTR))
	    CALL DAT_ANNUL(EBLOC,STATUS)
	  ELSE

*      Model channel bounds not present - construct them from centre values
	    CALL ERR_REP('NOBNDS','Response contains no ENERGY_BOUNDS -'//
     :      ' constructing bounds from centre values',STATUS)
	    CALL ERR_FLUSH(STATUS)
	    CALL FIT_PREDSET_LUCEN(PREDDAT.NMDAT,%VAL(MBINPTR),
     :      %VAL(PREDDAT.MLBNDPTR),%VAL(PREDDAT.MUBNDPTR))
	    IF(STATUS.NE.SAI__OK) GO TO 9000
	  ENDIF

*      Storage for predicted model array
	  CALL DYN_MAPR(1,PREDDAT.NMDAT,PREDDAT.MPTR,STATUS)

*      Tidy up
	  CALL DAT_ANNUL(INSLOC,STATUS)

* No convolution - model space is identical to data space. Set up only
* those components of PREDDAT required (not MPTR), using data axis values.
	ELSE
	  PREDDAT.MPTR=0			! Flag for `not set'
	  PREDDAT.NMDIM=OBDAT.NDIM
	  PREDDAT.NMBOUND=0
	  DO I=1,OBDAT.NDIM
	    PREDDAT.IDIMM(I)=OBDAT.IDIM(I)
	    PREDDAT.NMBOUND=PREDDAT.NMBOUND+PREDDAT.IDIMM(I)
	  ENDDO
	  PREDDAT.NMDAT=OBDAT.NDAT

*      Set up model bounds using axis values from observed data
	  CALL DYN_MAPR(1,PREDDAT.NMBOUND,PREDDAT.MLBNDPTR,STATUS)
	  CALL DYN_MAPR(1,PREDDAT.NMBOUND,PREDDAT.MUBNDPTR,STATUS)
	  CALL FIT_PREDSET_AXBOUND(DLOC,NDS,PREDDAT.NMDIM,
     :    PREDDAT.IDIMM,PREDDAT.NMBOUND,%VAL(PREDDAT.MLBNDPTR),
     :    %VAL(PREDDAT.MUBNDPTR),STATUS)
	ENDIF
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Storage for predicted data and if necessary workspace for chisq gradient
	CALL DYN_MAPR(1,OBDAT.NDAT,PREDDAT.DPTR,STATUS)
	IF(WORKSPACE)THEN
	  CALL DYN_MAPR(1,PREDDAT.NMDAT,PREDDAT.PREDPTR(1),STATUS)
	  CALL DYN_MAPR(1,PREDDAT.NMDAT,PREDDAT.PREDPTR(2),STATUS)
c	  CALL DYN_MAPR(1,PREDDAT.NMDAT*NPAMAX,PREDDAT.DFDPPTR,STATUS)
          PREDDAT.DFDPPTR = VAL__BADI
	ELSE
	  PREDDAT.PREDPTR(1)=0		! Set explicitly to zero as a
	  PREDDAT.PREDPTR(2)=0		! flag for `workspace not mapped'
	  PREDDAT.DFDPPTR=0
	ENDIF

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from FIT_PREDSET',
     :  STATUS)
	END

*+  FIT_PREDSET_AXBOUND - Generates bin boundaries from data axis values
      SUBROUTINE FIT_PREDSET_AXBOUND(LOC,NDS,NMDIM,IDIMM,NBOUND,LBOUND,
     :UBOUND,STATUS)
*    Description :
*     Uses axis information to set up bin boundaries for data modelling.
*     For data of dimension >1 it is assumed that there are just NBIN(i)
*     lower and upper bounds for each dimension, and these are concatenated
*     into the LBOUND and UBOUND arrays.
*    History :
*     30 Mar 87: Original (TJP)
*     31 Mar 88: Lower and upper axis bounds, zero width if no axis_error (TJP)
*     15 Feb 89: ASTERIX88 version (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
	INCLUDE 'DAT_PAR'
*    Import :
	CHARACTER*(DAT__SZLOC) LOC	! Dataset locator
	INTEGER NDS			! Current dataset number
	INTEGER NMDIM			! Dimensionality of data
	INTEGER IDIMM(NMDIM)		! No of bounds
	INTEGER NBOUND			! No of bin bounds
*    Import-Export :
*    Export :
	REAL LBOUND(NBOUND)		! Data bin lower bounds
	REAL UBOUND(NBOUND)		! Data bin upper bounds
*    Status :
	INTEGER STATUS
*    Local variables :
	INTEGER AXPTR			! Pointer to axis data
	INTEGER AXWPTR			! Pointer to axis width
	INTEGER AXNO			! Axis number
	INTEGER N			! Current bound number
	INTEGER NVAL			! No of values

	LOGICAL UNIF			! Uniform sized axis bins?
	LOGICAL WID			! Axis width available?
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Loop through data dimensions
	N=1
	DO AXNO=1,NMDIM

*    Get axis values
	  CALL BDA_MAPAXVAL(LOC,'READ',AXNO,AXPTR,STATUS)
	  IF(STATUS.NE.SAI__OK)THEN
*       No axis data available, assume spot values 0,1,2,3,4... (i.e. both
*       sets of bounds the same)
	    CALL ERR_ANNUL(STATUS)
	    CALL ARR_REG1R(0.0,1.0,IDIMM(AXNO),LBOUND(N),STATUS)
	    CALL ARR_REG1R(0.0,1.0,IDIMM(AXNO),UBOUND(N),STATUS)
	    CALL MSG_SETI('NDS',NDS)
	    CALL MSG_PRNT( 'No axis values found in dataset ^NDS - '//
     :                               'assuming spot values 0,1,2...' )
	  ELSE

*    Get axis bin size info
	    CALL BDA_CHKAXWID(LOC,AXNO,WID,UNIF,NVAL,STATUS)
	    IF(WID)THEN
	      CALL BDA_MAPAXWID(LOC,'READ',AXNO,AXWPTR,STATUS)
	      IF((.NOT.UNIF).AND.(NVAL.NE.IDIMM(AXNO)))THEN
	        CALL MSG_SETI('NDS',NDS)
	        CALL MSG_OUT('BAD_ERR','Axis width array is wrong size in'//
     :          ' dataset ^NDS - not used',STATUS)
	        WID=.FALSE.
	      ENDIF
	    ENDIF

*    Set up bounds
	    CALL FIT_PREDSET_AXBOUND_SET(IDIMM(AXNO),%VAL(AXPTR),WID,
     :      %VAL(AXWPTR),LBOUND(N),UBOUND(N))
	  ENDIF
	  N=N+IDIMM(AXNO)
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR',
     :  'from FIT_PREDSET_AXBOUND',STATUS)
	END

*+  FIT_PREDSET_AXBOUND_SET - Sets up bin boundaries for one axis
      SUBROUTINE FIT_PREDSET_AXBOUND_SET(NBIN,AX,WID,AXW,
     :                                  LBOUND,UBOUND)
*    Description :
*     Uses axis data and  width if available to set up bin boundaries.
*     If no bin width information is available then spot values
*     are assumed, and the lower and upper bounds are equal.
*    History :
*     31 Mar 87 : Original (TJP)
*     13 Apr 88 : Modified to return lower and upper bounds (TJP)
*     17 Feb 89 : ASTERIX88 version, axis width (rather than error/errorm) (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Global constants :
*    Import :
	INTEGER NBIN			! No of axis bins
	REAL AX(NBIN)			! Axis data values
	LOGICAL WID			! Axis widths available?
	REAL AXW(*)			! Axis width values
*    Import-Export :
*    Export :
	REAL LBOUND(*)			! Lower bin bounds
	REAL UBOUND(*)			! Upper bin bounds
*    Status :
*    Local variables :
	INTEGER I
*------------------------------------------------------------------------

	DO I=1,NBIN

* Lower bounds
	  IF(WID)THEN
	    LBOUND(I)=AX(I)-0.5*AXW(I)
	  ELSE
	    LBOUND(I)=AX(I)		! Zero width
	  ENDIF

* Upper bounds
	  IF(WID)THEN
	    UBOUND(I)=LBOUND(I)+AXW(I)
	  ELSE
	    UBOUND(I)=AX(I)		! Zero width
	  ENDIF
	ENDDO
	END

*+  FIT_PREDSET_LUBND - Extracts lower and upper bounds from single bound array
      SUBROUTINE FIT_PREDSET_LUBND(NBND,MBND,LBOUND,UBOUND)
*    Description :
*     Decomposes an array of model bin bounds (assuming contiguous bins) into
*     separate lower & upper bin bound arrays.
*    History :
*     21 Mar 88: Original (TJP)
*      8 Jul 88: Dimensioning bug fixed (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NBND			! No of output bounds
	REAL MBND(NBND+1)		! Input bounds
*    Import-Export :
*    Export :
	REAL LBOUND(NBND)		! Lower bin bounds
	REAL UBOUND(NBND)		! Upper bin bounds
*    Local constants :
*    Local variables :
	INTEGER I
*------------------------------------------------------------------------

	DO I=1,NBND
	  LBOUND(I)=MBND(I)
	  UBOUND(I)=MBND(I+1)
	ENDDO
	END

*+  FIT_PREDSET_LUCEN - Sets up model bounds from bin centre values
      SUBROUTINE FIT_PREDSET_LUCEN(NBIN,BIN,LBOUND,UBOUND)
*    Description :
*     Sets up lower & upper model bin bound arrays from bin centre values,
*     assuming contiguous bins.
*     The ambiguity in defining N+1 bounds from N bin values is resolved by
*     assuming that the second bound lies mid-way between the first two bin
*     centres.
*    History :
*     30 Mar 87: Original (TJP)
*      8 Jul 87: Bounds correctly handled where no base value is defined (TJP)
*     21 Mar 88: Modified to give lower & upper bounds (TJP)
*     15 Feb 89: ASTERIX88 version, no base value passed in (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NBIN			! No of bins
	REAL BIN(NBIN)			! Bin centres
*    Import-Export :
*    Export :
	REAL LBOUND(NBIN)		! Lower bin bounds
	REAL UBOUND(NBIN)		! Upper bin bounds
*    Local constants :
*    Local variables :
	INTEGER I
	REAL BASE			! Base value
*------------------------------------------------------------------------

* Base value
	BASE=(3*BIN(1)-BIN(2))/2.0	! Corresponds to definition in header
	LBOUND(1)=BASE

* Other bounds
	DO I=1,NBIN
	  UBOUND(I)=2*BIN(I)-LBOUND(I)		! i.e. BIN are bin centres
	  IF(I.LT.NBIN)THEN
	    LBOUND(I+1)=UBOUND(I)
	  ENDIF
	ENDDO
	END
