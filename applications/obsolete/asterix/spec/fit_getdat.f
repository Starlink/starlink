      SUBROUTINE FIT_GETDAT( ID, GENUS, FSTAT, WORKSPACE, WEIGHTS,
     :                       NDS, OBDAT, NGOOD, SSCALE, PREDDAT,
     :                       INSTR, STATUS )
*+
*  Name:
*     FIT_GETDAT

*  Purpose:
*     Get data for fitting

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_GETDAT( ID, GENUS, FSTAT, WORKSPACE, WEIGHTS, NDS, OBDAT,
*                      NGOOD, SSCALE, PREDDAT, INSTR, STATUS )

*  Description:
*     Gets data arrays, errors etc and response matrices (if available) from
*     multiple datasets. In the case of likelihood fitting, background
*     datasets and effective exposure times (=factor by which data must be
*     multiplied to get back to raw counts) are also acquired.
*     Sets up data weights for use in chi-squared evaluation if WEIGHTS=true.
*     If WORKSPACE is set true then workspace for the chi-squared gradient
*     calculation (performed by FIT_DERIVS) is set up in a temporary object.
*     (Note that this is not required for a simple fitstat evaluation,
*     only where gradients are needed.)
*     NOTE: If an instrument response is found in a dataset then it is assumed
*     that the model data require folding through it. If no INSTR_RESP is
*     present then the model and data spaces are assumed to be identical.

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

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
*     The locator ILOC may point to either a single dataset, or an object
*     containing a collection of references, for input of multiple datasets.
*     Data quality is taken into account if present, so that bad data are
*     given zero weight. Good data take a weight equal to their inverse
*     variance for chisq fitting, whilst for likelihood, the data are
*     scaled up by TEFF (and any b/g subtracted is added back on) to recover
*     the raw counts (so that the errors can be treated as Poissonian).
*     Background data are found via a BGREF reference in each dataset,
*     and are checked to ensure that they have the same dimensions as the
*     main data array. For spectral sets, the background is required to
*     be a parallel set. B/g data may be the RAW b/g counts
*     predicted in the data, or they can be time normalised, in which
*     case they are scaled by TEFF by the software.
*     The response matrix is acquired, if it is present in the
*     dataset, and its channel range is checked against that of the data.
*     In the special case of spectral sets, a 2D array is accessed as a set
*     of 1D spectra, AXIS(1) being the spectral dimension. In this case
*     OBDAT.DLOC points to the same container file for each member of the
*     set, and OBDAT.SETINDEX indicates the position in the set. For normal
*     spectra OBDAT.SETINDEX is set to zero.
*     The quantity SSCALE is used to scale the fit statistic to give a number
*     closer to unity within FIT_MIN. For the likelihood case this is set
*     to the total number of counts in the data, whilst for the chisq case,
*     it need not be set up at this stage, since the number of degrees of
*     freedom will be used.
*     NOTE:
*     Temporary storage is set up by the DYN_ system; this should be
*     initialised in the calling program with an AST_INIT, and cleared up
*     at the end of execution with AST_CLOSE. This will also take care of
*     initialisation and closing of the BDA_ system.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Mar 87 : Original (FIT_DATGET) (BHVAD::TJP)
*     11 Jun 87 : Data object names found and returned (TJP)
*     26 Jun 87 : Various minor fixes (TJP)
*     14 Apr 88 : Changed structures - renamed to FIT_DATINGET (TJP)
*     12 Aug 88 : DLOC incorporated in OBDAT structure (TJP)
*     16 Aug 88 : WEIGHTS argument allows disabling of weights (TJP)
*     21 Oct 88 : FIT_MSPACE renamed to FIT_PREDSET (TJP)
*     31 Oct 88 : Bug with bad quality (e.g. IGNOREd) data fixed (TJP)
*     24 May 89 : ASTERIX88 conversion, handling of SPECTRAL_SETs (TJP)
*      7 Jun 89 : Selection of detectors from spectral sets (TJP)
*     19 Jun 89 : Spectral number included in OBDAT.DATNAME for sets (TJP)
*     23 Jun 89 : Slice mapping for SPECTRAL_SETs fixed (TJP)
*      7 Jul 89 : DCLOC not annulled (TJP)
*     18 May 90 : Use of SPEC_SETSEARCH to establish SPECTRAL_SET type (TJP)
*     10 Jun 92 : Error handling corrected. Obj.name replaced by filename (TJP)
*     18 Jun 92 : Likelihood fitting catered for (TJP)
*     19 Jun 92 : Handle case of b/g subtracted data for LIK fitting (TJP)
*     24 Jun 92 : Both NGOOD and SSCALE passed back (TJP)
*      1 Jul 92 : Bug fix when handling SPECTRAL_SET directly (TJP)
*      8 Jul 92 : Don't get instrument response when GENUS is 'CLUS' (DJA)
*     26 Nov 92 : Use quality when finding SSCALE (DJA)
*     15 Nov 93 : Use PRO_GET routine to read PROCESSING flags (DJA)
*     21 Jul 94 : Store background locator in dataset structure (DJA)
*     10 Mar 1995 (DJA):
*        Adapted from old FIT_DATINGET.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'

*  Structure Declarations:
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER			ID			! Input data (or ref)
      CHARACTER*(*)             GENUS           	! Model GENUS
      INTEGER                   FSTAT           	! Fit statistic flag
      LOGICAL                   WORKSPACE               ! Set up workspace?

*  Arguments Given and Returned:
      LOGICAL 			WEIGHTS                 ! Set up data weights?

*  Arguments Returned:
      INTEGER 			NDS                     ! No of datasets
      RECORD /DATASET/ 		OBDAT(NDSMAX)  		! Observed datasets
      INTEGER 			NGOOD                   ! No of good data elements
      INTEGER 			SSCALE                  ! Factor for scaling fitstat
      RECORD /PREDICTION/ 	PREDDAT(NDSMAX) 	! Data predicted by model
      RECORD /INSTR_RESP/ 	INSTR(NDSMAX) 		! Instrument responses

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER 		CHR_LEN

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! HDS locator

      INTEGER			I			! Loop over datasets


	CHARACTER*(DAT__SZLOC) LOC	! Component locator
	CHARACTER*(DAT__SZLOC) DCLOC(NDSMAX)	! Data container file locators
	CHARACTER*(DAT__SZLOC) CLOC	! Temporary component locator
	CHARACTER*(DAT__SZLOC) ALOC	! Locator to ASTERIX box
	CHARACTER*(DAT__SZLOC) BRLOC	! Locator to b/g file reference
	CHARACTER*(DAT__SZLOC) BLOC(NDSMAX)	! Locator to b/g files
	CHARACTER*(DAT__SZLOC) SLOC(NDSMAX)	! Data array slice locators
	CHARACTER*(DAT__SZLOC) BSLOC(NDSMAX)	! B/g array slice locators
	CHARACTER*(DAT__SZLOC) SVLOC(NDSMAX)	! Data variance slice locators
	CHARACTER*(DAT__SZLOC) SQLOC(NDSMAX)	! Data quality slice locators
	CHARACTER*(DAT__SZNAM) NAME	! Object name
	CHARACTER*(DAT__SZTYP) TYPE	! Object type
	CHARACTER*100 FILE		! File name for HDS_TRACE
	CHARACTER*5 SNAME		! Select component name
	CHARACTER*2 FILENO		! File number
	CHARACTER*2 SPECH		! Spectrum number (within set)
	BYTE MASK			! Quality mask byte
	LOGICAL LOG			! General purpose logical
	LOGICAL REF			! Input from reference file?
	LOGICAL OK			! Data present and defined?
	LOGICAL VALID			! Locator valid?
	LOGICAL SPECSET(NDSCMAX)	! Container is a SPECTRAL_SET?
	LOGICAL QUAL			! Data quality info available?
	LOGICAL BAD			! Bad points present?
	LOGICAL GOOD			! Good points present?
	LOGICAL LIKSTAT			! Likelihood fitting?
	LOGICAL CHISTAT			! Chi-squared fitting?
	LOGICAL BG			! B/g data file found?
	LOGICAL BGSUB			! B/g subtracted flag set in data?
	LOGICAL BGCOR			! Has b/g data been exposure corrected?
	INTEGER PTR			! General mapping pointer
	INTEGER I			! Index
	INTEGER NDSC			! No of dataset container files
	INTEGER NDSTOP			! NDS at end of current container
	INTEGER SETSIZE			! No of spectra in spectral set
	INTEGER NDIM			! Data array dimensionality
	INTEGER NBDIM			! B/g array dimensionality
	INTEGER DIMS(DAT__MXDIM)	! Data array dimensions
	INTEGER BDIMS(DAT__MXDIM)	! B/g array dimensions
	INTEGER NCOMP			! No of components in dataset
	INTEGER N			! Dataset index
	INTEGER INDEX			! Current spectral set selection no
	INTEGER LDIM(2)			! Lower bound for array slice
	INTEGER UDIM(2)			! Upper bound for array slice
	INTEGER NGDAT			! No of good data in dataset
	INTEGER NACT			! No of values accessed
	INTEGER NCH			! No of characters in string
	INTEGER DETNO(NDSCMAX)		! No of detectors selected from set
	INTEGER DETSEL(NDETMAX,NDSCMAX)	! Detectors selected from set
        INTEGER SPECNO			! Current spectrum no (from set)
	INTEGER IERR,NERR		! Error arguments for VEC_* routines
	REAL TEFF			! Effective exposure time
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator for dataset
      CALL ADI1_GETLOC( ID, ILOC, STATUS )

*  Set statistic flags
      IF ( FSTAT .EQ. FIT__LOGL ) THEN
	LIKSTAT = .TRUE.
        CHISTAT = .FALSE.
        WEIGHTS = .FALSE.
      ELSE
	LIKSTAT = .FALSE.
	CHISTAT = .TRUE.
      END IF

* Find datasets - get locators
	CALL DAT_TYPE(ILOC,TYPE,STATUS)
	IF(TYPE.NE.'REF_FILE')THEN

*    Single input dataset container (directly referenced)
	  REF=.FALSE.
	  NDSC=1
	  CALL DAT_CLONE(ILOC,DCLOC(1),STATUS)
*       Spectral set?
	  CALL SPEC_SETSEARCH(DCLOC(NDSC),SPECSET(NDSC),STATUS)
	  IF(SPECSET(1)) DETNO(1)=0		! Flag to use all spectra
	ELSE

*    Multiple input containers, find how many & get locators
	  REF=.TRUE.
	  CALL DAT_NCOMP(ILOC,NCOMP,STATUS)
	  NDSC=0
	  DO I=1,NCOMP
	    CALL DAT_INDEX(ILOC,I,LOC,STATUS)
	    CALL DAT_TYPE(LOC,TYPE,STATUS)
	    IF(TYPE.EQ.'REFERENCE_OBJ')THEN
	      NDSC=NDSC+1
	      CALL REF_GET(LOC,'READ',DCLOC(NDSC),STATUS)

*       Spectral set?
	      CALL SPEC_SETSEARCH(DCLOC(NDSC),SPECSET(NDSC),STATUS)
	      IF(SPECSET(NDSC))THEN
*         Find which spectra are to be used
	        CALL DAT_NAME(LOC,NAME,STATUS)	! Name should be REFnnn
	        SNAME='SEL'//NAME(4:CHR_LEN(NAME))
	        CALL CMP_GET1I(ILOC,SNAME,NDETMAX,DETSEL(1,NDSC),
     :          DETNO(NDSC),STATUS)
	        IF(STATUS.NE.SAI__OK)THEN
	          CALL ERR_ANNUL(STATUS)
	          DETNO(NDSC)=0			! Flag for `all spectra'
	        ENDIF
	      ENDIF
	    ENDIF
	    CALL DAT_ANNUL(LOC,STATUS)
	  ENDDO
	ENDIF
	IF(STATUS.NE.SAI__OK) GOTO 9000

* Abort if maximum permitted number of datasets is exceeded
	IF(NDSC.GT.NDSMAX)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BADNDS','Maximum number of input datasets exceeded',
     :    STATUS)
	  GOTO 9000
	ENDIF

* Loop through dataset containers
	NGOOD=0
	SSCALE=0
	NDS=0
	DO N=1,NDSC

*    Get dataset container file name and display
	  IF(NDSC.EQ.1)THEN
	    CALL UTIL_SHOW(DCLOC(N),'Dataset',FILE,NAME,I,STATUS)
	  ELSE
	    CALL CHR_ITOC(N,FILENO,NCH)
	    CALL UTIL_SHOW(DCLOC(N),'Dataset '//FILENO(1:NCH),FILE,
     :      NAME,NCH,STATUS)
	  ENDIF
	  IF(STATUS.NE.SAI__OK) CALL ERR_FLUSH(STATUS)

*    Check main data array
	  CALL BDA_CHKDATA(DCLOC(N),OK,NDIM,DIMS,STATUS)
	  IF(STATUS.NE.SAI__OK) GOTO 9000
	  IF(.NOT.OK)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BADDAT','Error accessing data array',STATUS)
	    GOTO 9000
	  ENDIF

*      Check that spectrum is exposure corrected (i.e in ct/s)
          CALL PRO_GET( DCLOC(N), 'CORRECTED.EXPOSURE', LOG, STATUS )
	  IF(.NOT.LOG)THEN
	    CALL MSG_PRNT(' ')
	    CALL MSG_PRNT('!! Warning - data must be corrected to ct/s')
	    CALL MSG_PRNT(' ')
	  ENDIF

*    For likelihood case, get eff.exposure time from dataset and locate b/g data
	  IF ( LIKSTAT ) THEN
	    CALL BDA_LOCHEAD(DCLOC(N),LOC,STATUS)
	    CALL CMP_GET0R(LOC,'EFF_EXPOSURE',TEFF,STATUS)
	    IF(STATUS.EQ.SAI__OK)THEN
	      CALL MSG_SETR('TEFF',TEFF)
	      CALL MSG_PRNT('    Effective exposure time ^TEFF')
	    ELSE
	      CALL ERR_ANNUL(STATUS)
	      CALL CMP_GET0R(LOC,'EXPOSURE_TIME',TEFF,STATUS)
	      IF(STATUS.EQ.SAI__OK)THEN
	        CALL MSG_SETR('TEFF',TEFF)
	        CALL MSG_PRNT('Effective exposure time not found - '//
     :          'using EXPOSURE_TIME value of ^TEFF')
	      ELSE
	        CALL ERR_REP(' ','No exposure times found in dataset',
     :          STATUS)
	        GOTO 9000
	      ENDIF
	    ENDIF

*      Have data been b/g subtracted?
            CALL PRO_GET( DCLOC(N), 'BGND_SUBTRACTED', BGSUB, STATUS )
	    IF ( BGSUB ) THEN
	      CALL MSG_PRNT('    Background-subtracted data')
	    ELSE
	      CALL MSG_PRNT('    Not background-subtracted')
	    ENDIF

*      Locate b/g data
	    CALL BDA_LOCAST(DCLOC(N),ALOC,STATUS)
	    CALL DAT_FIND(ALOC,'BGREF',BRLOC,STATUS)
	    CALL REF_GET(BRLOC,'READ',BLOC(N),STATUS)
	    IF(STATUS.NE.SAI__OK)THEN
	      CALL ERR_ANNUL(STATUS)
	      BG=.FALSE.
	    ELSE
	      BG=.TRUE.
	    ENDIF
	    CALL DAT_VALID(BRLOC,VALID,STATUS)
	    IF(VALID) CALL DAT_ANNUL(BRLOC,STATUS)

*      Check that background data array is same size as main data array
	    IF(BG)THEN
	      CALL BDA_CHKDATA(BLOC(N),OK,NBDIM,BDIMS,STATUS)
	      IF(STATUS.NE.SAI__OK)THEN
	        CALL ERR_FLUSH(STATUS)
	        BG=.FALSE.
	      ELSE IF(.NOT.OK)THEN
	        CALL MSG_PRNT('Error accessing b/g data array')
	        BG=.FALSE.
	      ENDIF
	      IF(NBDIM.EQ.NDIM)THEN
	        DO I=1,NDIM
	          IF(BDIMS(I).NE.DIMS(I)) BG=.FALSE.
	        ENDDO
	      ELSE
	        BG=.FALSE.
	      ENDIF

*         Warn user and exit if it doesn't match
	      IF(.NOT.BG)THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP(' ','Background array does not match'//
     :          ' data array',STATUS)
	        GOTO 9000
	      ENDIF

*      Abort if b/g has been subtracted, but is not available
	      IF(BGSUB.AND.(.NOT.BG))THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP('NOBG','Subtracted background data is'//
     :          ' not available',STATUS)
	        GOTO 9000
	      ENDIF

*      Has background been exposure corrected? (We will require raw count
*                               contribution from b/g expected in the data)
	      CALL HDX_FIND(BLOC(N),
     :        'MORE.ASTERIX.PROCESSING.CORRECTED.EXPOSURE',LOC,STATUS)
	      IF(STATUS.EQ.SAI__OK)THEN
	        CALL DAT_GET0L(LOC,BGCOR,STATUS)
	        CALL DAT_ANNUL(LOC,STATUS)
	      ELSE
	        CALL ERR_ANNUL(STATUS)
	        BGCOR=.FALSE.
	      ENDIF
	    ENDIF

	    IF(.NOT.BG)THEN
	      CALL MSG_PRNT('    No background data found -'//
     :        ' assumed negligible')
	    ENDIF
	  ENDIF

*    Is container a spectral set?
	  IF(SPECSET(N))THEN

*    Spectral set - find number of component spectra
	    IF(NDIM.NE.2)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADDIM','Spectral set has incorrect '//
     :        'dimensionality',STATUS)
	    ENDIF
	    IF(STATUS.NE.SAI__OK) GOTO 9000
	    CALL MSG_SETI('NSPEC',DIMS(2))
	    CALL MSG_PRNT('Spectral set containing ^NSPEC spectra')
	    IF(DETNO(N).GT.0)THEN
	      IF(DETNO(N).LT.DIMS(2))THEN
	        SETSIZE=DETNO(N)
	      ELSE
	        SETSIZE=DIMS(2)
	        DETNO(N)=0		! Flag for `use all spectra'
	      ENDIF
	      IF(SETSIZE.EQ.1)THEN
	        WRITE(*,100) (DETSEL(I,N),I=1,SETSIZE)
 100	        FORMAT(' Using component number: ',I3)
	      ELSE
	        WRITE(*,110) (DETSEL(I,N),I=1,SETSIZE)
 110	        FORMAT(' Using numbers: ',<SETSIZE>(I3))
	      ENDIF
	    ELSE
	      SETSIZE = DIMS(2)
	      CALL MSG_PRNT( 'Using all spectra' )
	    ENDIF
	    CALL BDA_UNMAP(DCLOC(N),STATUS)
	  ELSE

*    Not a spectral set
	    SETSIZE=1
	  ENDIF

*    Loop through component spectra required from a given dataset
	  INDEX=0
	  NDSTOP=NDS+SETSIZE
	  DO WHILE(NDS.LT.NDSTOP)
	    NDS=NDS+1

*       Abort if maximum permitted number of datasets is exceeded
	    IF(NDS.GT.NDSMAX)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADNDS','Maximum number of input datasets '//
     :        'exceeded',STATUS)
	      GOTO 9000
	    ENDIF

*       Set up name, locator and set position
	    CALL DAT_CLONE(DCLOC(N),OBDAT(NDS).DLOC,STATUS)
	    IF(SPECSET(N))THEN
	      INDEX=INDEX+1
	      IF(DETNO(N).GT.0)THEN
	        SPECNO=DETSEL(INDEX,N)
	      ELSE
	        SPECNO=INDEX
	      ENDIF
	      CALL CHR_ITOC(SPECNO,SPECH,NCH)
	      OBDAT(NDS).DATNAME=FILE(1:CHR_LEN(FILE))//' Spectrum '/
     :                         /SPECH
	    ELSE
	      SPECNO=0
	      OBDAT(NDS).DATNAME=FILE
	    ENDIF
	    OBDAT(NDS).SETINDEX=SPECNO
D	    print *,'index,specno:- ',index,specno

*       Find and map data array
	    IF(SPECSET(N))THEN

*          Map slice for spectral set
	      LDIM(1)=1
	      LDIM(2)=SPECNO
	      UDIM(1)=DIMS(1)
	      UDIM(2)=SPECNO
	      CALL BDA_LOCDATA(OBDAT(NDS).DLOC,CLOC,STATUS)
	      CALL DAT_SLICE(CLOC,2,LDIM,UDIM,SLOC(NDS),STATUS)

D	      print *,'dat__mxdim,obdat(n).idim,obdat(n).ndim: ',dat__mxdim,
D    :        obdat(nds).idim,obdat(nds).ndim
	      OBDAT(NDS).NDIM=1
	      OBDAT(NDS).IDIM(1)=DIMS(1)
	      OBDAT(NDS).NDAT=DIMS(1)
	      CALL DAT_MAPV(SLOC(NDS),'_REAL','READ',OBDAT(NDS).DPTR,
     :        NACT,STATUS)
	    ELSE

*          Simple BDA_ map
	      OBDAT(NDS).NDIM=NDIM
	      OBDAT(NDS).NDAT=1
	      DO I=1,OBDAT(NDS).NDIM
	        OBDAT(NDS).IDIM(I)=DIMS(I)
	        OBDAT(NDS).NDAT=OBDAT(NDS).NDAT*DIMS(I)
	      ENDDO
	      CALL BDA_MAPDATA(OBDAT(NDS).DLOC,'READ',OBDAT(NDS).DPTR,
     :        STATUS)
	      IF(STATUS.NE.SAI__OK) GOTO 9000
	    ENDIF

*         For likelihood case scale to give raw counts, & accumulate SSCALE
	    IF(LIKSTAT)THEN
	      PTR=OBDAT(NDS).DPTR
	      CALL DYN_MAPR(1,OBDAT(NDS).NDAT,OBDAT(NDS).DPTR,STATUS)
	      CALL ARR_COP1R(OBDAT(NDS).NDAT,%VAL(PTR),
     :        %VAL(OBDAT(NDS).DPTR),STATUS)
	      IF(STATUS.NE.SAI__OK) GOTO 9000
	      CALL ARR_MULTR(TEFF,OBDAT(NDS).NDAT,%VAL(OBDAT(NDS).DPTR))
	    ENDIF

*       Map variance and quality and use to set up array of data weights

*          Get variance (slice in case of spectral set)
	    CALL BDA_CHKVAR(OBDAT(NDS).DLOC,OK,NDIM,DIMS,STATUS)
	    IF(OK)THEN
	      IF(SPECSET(N))THEN
	        CALL BDA_LOCVAR(OBDAT(NDS).DLOC,CLOC,STATUS)
	        CALL DAT_SLICE(CLOC,2,LDIM,UDIM,SVLOC(NDS),STATUS)
	        CALL DAT_MAPV(SVLOC(NDS),'_REAL','READ',OBDAT(NDS).VPTR,
     :          NACT,STATUS)
	      ELSE
	        CALL BDA_MAPVAR(OBDAT(NDS).DLOC,'READ',
     :          OBDAT(NDS).VPTR,STATUS)
	      ENDIF
	    ELSE
	      IF(WEIGHTS)THEN
	        CALL MSG_SETI('NDS',NDS)
	        STATUS=SAI__ERROR
	        CALL ERR_REP( ' ', 'No error information available '//
     :          'in dataset ^NDS',STATUS)
	      ELSE
	        OBDAT(NDS).VPTR=0			! Flag
	      ENDIF
	    ENDIF
	    IF(STATUS.NE.SAI__OK) GOTO 9000

*          Get quality
	    CALL BDA_CHKQUAL(OBDAT(NDS).DLOC,QUAL,NDIM,DIMS,STATUS)
	    IF(QUAL)THEN
	      IF(SPECSET(N))THEN
	        CALL BDA_GETMASK(OBDAT(NDS).DLOC,MASK,STATUS)
	        CALL BDA_LOCQUAL(OBDAT(NDS).DLOC,CLOC,STATUS)
	        CALL DAT_SLICE(CLOC,2,LDIM,UDIM,SQLOC(NDS),STATUS)
	        CALL DAT_MAPV(SQLOC(NDS),'_UBYTE','READ',PTR,NACT,STATUS)
	        CALL DYN_MAPL(1,OBDAT(NDS).NDAT,OBDAT(NDS).QPTR,STATUS)
	        CALL ARR_LOGMASK(%VAL(PTR),OBDAT(NDS).NDAT,MASK,
     :          %VAL(OBDAT(NDS).QPTR),BAD,GOOD,STATUS)
	      ELSE
	        CALL BDA_MAPLQUAL(OBDAT(NDS).DLOC,'READ',BAD,
     :          OBDAT(NDS).QPTR,STATUS)
	      ENDIF

*         Set quality flag if bad values are present
	      OBDAT(NDS).QFLAG = BAD

	    ELSE
	      OBDAT(NDS).QFLAG=.FALSE.
	      OBDAT(NDS).QPTR=0
	    ENDIF

	    IF(STATUS.NE.SAI__OK)THEN
D	      call err_flush(status)
	      CALL ERR_ANNUL(STATUS)
	      QUAL=.FALSE.
	    ENDIF
D	    print *,'datget;ndim,ndat,qual:',obdat(nds).ndim,
D    :      obdat(nds).ndat,qual
D	    print *,'qual,qflag,qptr : ',qual,obdat(nds).qflag,obdat(nds).qptr
D	    print *,'ldim,udim :',ldim,udim

*          Accumulate counts for data in likelihood case
            IF ( LIKSTAT ) THEN
              IF ( OBDAT(NDS).QFLAG ) THEN
	        CALL FIT_DATINGET_COUNTSQ(OBDAT(NDS).NDAT,
     :                      %VAL(OBDAT(NDS).DPTR),
     :                      %VAL(OBDAT(NDS).QPTR),SSCALE)
              ELSE
	        CALL FIT_DATINGET_COUNTS(OBDAT(NDS).NDAT,
     :                      %VAL(OBDAT(NDS).DPTR),SSCALE)
              END IF
            END IF

*          Map weights as 1D array
	    IF(WEIGHTS)THEN
	      CALL DYN_MAPR(1,OBDAT(NDS).NDAT,OBDAT(NDS).WPTR,STATUS)
	      IF(STATUS.NE.SAI__OK) GOTO 9000
	    ELSE
	      OBDAT(NDS).WPTR=0				! Flag
	    ENDIF

*          Enter weights (=inverse variances)
	    CALL FIT_DATINGET_WTS(WEIGHTS,OBDAT(NDS).NDAT,
     :      %VAL(OBDAT(NDS).VPTR),QUAL,%VAL(OBDAT(NDS).QPTR),
     :      %VAL(OBDAT(NDS).WPTR),NGDAT)
	    IF(NGDAT.EQ.0)THEN
	      CALL MSG_SETI('NDS',NDS)
	      STATUS=SAI__ERROR
	      CALL ERR_REP('NO_GOOD','No good data in dataset ^NDS',STATUS)
	      GOTO 9000
	    ELSE
	      NGOOD=NGOOD+NGDAT
	    ENDIF

*          Default value for background object
            OBDAT(NDS).BLOC = DAT__NOLOC

*       For likelihood case, Set up OBDAT.TEFF and get background data
	    IF(LIKSTAT)THEN
	      OBDAT(NDS).TEFF=TEFF
	      IF(BG)THEN

*              Store background object
                OBDAT(NDS).BLOC = BLOC(N)

*         Find and map b/g data array
	        IF(SPECSET(N))THEN

*           Map slice for b/g spectral set
	          CALL BDA_LOCDATA(BLOC(N),CLOC,STATUS)
	          CALL DAT_SLICE(CLOC,2,LDIM,UDIM,BSLOC(NDS),STATUS)
	          CALL DAT_MAPV(BSLOC(NDS),'_REAL','READ',OBDAT(NDS).BPTR,
     :            NACT,STATUS)
	        ELSE

*           Simple BDA_ map for straight b/g spectrum
	          CALL BDA_MAPDATA(BLOC(N),'READ',OBDAT(NDS).BPTR,STATUS)
	          IF(STATUS.NE.SAI__OK) GOTO 9000
	        ENDIF

*           Scale b/g to give raw counts if it has been exposure corrected
	        IF(BGCOR)THEN
	          PTR=OBDAT(NDS).BPTR
	          CALL DYN_MAPR(1,OBDAT(NDS).NDAT,OBDAT(NDS).BPTR,STATUS)
	          CALL ARR_COP1R(OBDAT(NDS).NDAT,%VAL(PTR),
     :            %VAL(OBDAT(NDS).BPTR),STATUS)
	          IF(STATUS.NE.SAI__OK) GOTO 9000
	          CALL ARR_MULTR(TEFF,OBDAT(NDS).NDAT,%VAL(OBDAT(NDS).BPTR))
	        ENDIF

*            If b/g has been subtracted then add it back into data and SSCALE
	        IF(BGSUB)THEN
	          CALL VEC_ADDR(.FALSE.,OBDAT(N).NDAT,%VAL(OBDAT(N).BPTR),
     :            %VAL(OBDAT(N).DPTR),%VAL(OBDAT(N).DPTR),IERR,NERR,
     :            STATUS)
	        ENDIF

*            Accumulate counts for data in likelihood case. Use quality if
*            present in input data (rather than bgnd)
                IF ( OBDAT(NDS).QFLAG ) THEN
	          CALL FIT_DATINGET_COUNTSQ(OBDAT(NDS).NDAT,
     :                      %VAL(OBDAT(NDS).BPTR),
     :                      %VAL(OBDAT(NDS).QPTR),SSCALE)
                ELSE
	          CALL FIT_DATINGET_COUNTS(OBDAT(NDS).NDAT,
     :                      %VAL(OBDAT(NDS).BPTR),SSCALE)
                END IF

*         No background data - set up array of zeros
	      ELSE
	        CALL DYN_MAPR(1,OBDAT(NDS).NDAT,OBDAT(NDS).BPTR,STATUS)
	        CALL ARR_INIT1R(0.0,OBDAT(NDS).NDAT,
     :                      %VAL(OBDAT(NDS).BPTR),STATUS)
	      ENDIF
	    ENDIF

*............ OBDAT set up ..............................................

*    Hardwire CONVOLVE flag false in cluster fitting.
            IF ( GENUS .EQ. 'CLUS' ) THEN
              PREDDAT(N).CONVOLVE = .FALSE.
            ELSE
*      Look for instrument response, set up INSTR if found, and report
	      CALL FIT_GETINS( OBDAT(NDS).DLOC, SPECNO,
     :               PREDDAT(NDS).CONVOLVE, INSTR(NDS), STATUS )
	      IF(STATUS.NE.SAI__OK) GOTO 9000
            END IF

	    IF(PREDDAT(NDS).CONVOLVE)THEN

*      Check that data are 1D
	      IF(OBDAT(NDS).NDIM.GT.1)THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP('BAD_DIM','Convolution with instrument response'
     :          //' is only supported for 1D data at present',STATUS)
	        GOTO 9000
	      ENDIF
	    ENDIF

* Find remaining components of PREDDAT(NDS)
	    CALL FIT_PREDSET(OBDAT(NDS).DLOC,NDS,WORKSPACE,OBDAT(NDS),
     :      PREDDAT(NDS),STATUS)
	  ENDDO
	ENDDO

*  Set up ADI stuff
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Input is not a set?
        IF ( OBDAT(1).SETINDEX .EQ. 0 ) THEN
          CALL ADI_CLONE( ID, OBDAT(1).D_ID, STATUS )
        ELSE
          DO I = 1, NDS
            CALL ADI1_PUTLOC( OBDAT(I).DLOC, OBDAT(I).D_ID, STATUS )
            CALL ADI1_PUTLOC( OBDAT(I).BLOC, OBDAT(I).B_ID, STATUS )
          END DO
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FIT_GETDAT', STATUS )

      END
