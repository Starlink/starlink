      SUBROUTINE FIT_GETDAT( MPCID, ID, GENUS, FSTAT, WORKSPACE,
     :                       WEIGHTS, NDS, NGOOD, SSCALE, LNDFAC,
     :                       STATUS )
*+
*  Name:
*     FIT_GETDAT

*  Purpose:
*     Get data for fitting

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_GETDAT( MPCID, ID, GENUS, FSTAT, WORKSPACE, WEIGHTS, NDS, OBDAT,
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
*     MPCID = INTEGER (given)
*        Multi-processing control object
*     ID = INTEGER (given)
*        Top level fit dataset, either a FileSet or a fit source file
*     GENUS = CHARACTER*(*) (given)
*        The fit genus
*     FSTAT = INTEGER (given)
*        Fit statistic code, either FIT__CHISQ or FIT__LOGL
*     WORKSPACE = LOGICAL (given)
*        Set up workspace for minimisation?
*     STATUS = INTEGER (given and returned)
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
*     The identifier ID may refer to either a single dataset, or an object
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
*     OBDAT.D_ID points to the same container file for each member of the
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
*     at the end of execution with AST_CLOSE.

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
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Mar 1987 (TJP):
*        Original version (FIT_DATGET)
*     11 Jun 1987 (TJP):
*        Data object names found and returned
*     26 Jun 1987 (TJP):
*        Various minor fixes
*     14 Apr 1988 (TJP):
*        Changed structures - renamed to FIT_DATINGET
*     12 Aug 1988 (TJP):
*        DLOC incorporated in OBDAT structure
*     16 Aug 1988 (TJP):
*        WEIGHTS argument allows disabling of weights
*     21 Oct 1988 (TJP):
*        FIT_MSPACE renamed to FIT_PREDSET
*     31 Oct 1988 (TJP):
*        Bug with bad quality (e.g. IGNOREd) data fixed
*     24 May 1989 (TJP):
*        ASTERIX88 conversion, handling of spectral sets
*      7 Jun 1989 (TJP):
*        Selection of detectors from spectral sets
*     19 Jun 1989 (TJP):
*        Spectral number included in OBDAT.DATNAME for sets
*     23 Jun 1989 (TJP):
*        Slice mapping for SPECTRAL_SETs fixed
*      7 Jul 1989 (TJP):
*        DCLOC not annulled
*     18 May 1990 (TJP):
*        Use of SPEC_SETSEARCH to establish SPECTRAL_SET type
*     10 Jun 1992 (TJP):
*        Error handling corrected. Obj.name replaced by filename
*     18 Jun 1992 (TJP):
*        Likelihood fitting catered for
*     19 Jun 1992 (TJP):
*        Handle case of b/g subtracted data for LIK fitting
*     24 Jun 1992 (TJP):
*        Both NGOOD and SSCALE passed back
*      1 Jul 1992 (TJP):
*        Bug fix when handling SPECTRAL_SET directly
*      8 Jul 1992 (DJA):
*        Don't get instrument response when GENUS is 'CLUS'
*     26 Nov 1992 (DJA):
*        Use quality when finding SSCALE
*     15 Nov 1993 (DJA):
*        Use PRO_GET routine to read PROCESSING flags
*     21 Jul 1994 (DJA):
*        Store background locator in dataset structure
*     10 Mar 1995 (DJA):
*        Adapted from old FIT_DATINGET. PRO_ -> PRF_ etc
*      1 Aug 1995 (DJA):
*        Added ability to read vignetting file.
*     29 Nov 1995 (DJA):
*        ADI port. Use new BDI routines, FSI for spectral set access.
*      4 Mar 1996 (DJA):
*        Added group loading capability
*      5 August 1997 (RB):
*        Map all arrays before slicing, pass FileSet to FSI.
*      7 Nov 1997 (ELD):
*        LNDFAC calculated

*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'

*  Structure Declarations:
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER			MPCID, ID, FSTAT
      CHARACTER*(*)             GENUS
      LOGICAL                   WORKSPACE

*  Arguments Given and Returned:
      LOGICAL 			WEIGHTS                 ! Set up data weights?

*  Arguments Returned:
      INTEGER 			NDS                     ! No of datasets
c     RECORD /DATASET/ 		OBDAT(NDSMAX)  		! Observed datasets
      INTEGER 			NGOOD                   ! No of good data elements
      INTEGER 			SSCALE                  ! Factor for scaling fitstat
c     RECORD /PREDICTION/ 	PREDDAT(NDSMAX) 	! Data predicted by model
c     RECORD /INSTR_RESP/ 	INSTR(NDSMAX) 		! Instrument responses
      DOUBLE PRECISION          LNDFAC                  ! lnd!

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER 		CHR_LEN
      EXTERNAL			MPC_QSLAVE
        LOGICAL			MPC_QSLAVE

*  Local Variables:
      CHARACTER*100 		FILE			! File name for HDS_TRACE
      CHARACTER*2 		SPECH			! Spectrum number (within set)

      REAL			RSUM			! Real SSCALE
      REAL 			TEFF			! Effective exposure time

      INTEGER 			BDIMS(ADI__MXDIM)	! B/g array dimensions
      INTEGER			BFID(NDSMAX)		! Bgnd datasets
      INTEGER			CNGOOD			! Current # good points
      INTEGER			DCFID(NDSMAX)		! Source datasets
      INTEGER 			DETNO(NDSCMAX)		! # detectors selected from set
      INTEGER 			DETSEL(NDETMAX,NDSCMAX)	! Detectors selected from set
      INTEGER 			DIMS(ADI__MXDIM)	! Data array dimensions
      INTEGER 			I			! Index
      INTEGER 			IERR,NERR		! Error args for VEC_*
      INTEGER 			INDEX			! Current spectral set selection no
      INTEGER			LDIM(2), UDIM(2)	! Slice pixel bounds
      INTEGER			N			! Dataset index
      INTEGER			NBDIM			! Bgnd dimensionality
      INTEGER 			NCH			! String length
      INTEGER 			NDIM			! I/p dimensionality
      INTEGER 			NDSC			! # dataset files
      INTEGER 			NDSTOP			! NDS at end of current container
      INTEGER			NDUFVAR			! # duff variances
      INTEGER			NVDIM			! Vignetting dim'ality
      INTEGER 			PTR			! General pointer
      INTEGER 			SETSIZE			! # spectra in set
      INTEGER			SPECNO			! Current spec in set
      INTEGER			TIMID			! Timing info
      INTEGER			TPTR			! Temp pointer
      INTEGER			VDIMS(ADI__MXDIM)	! Vignetting dims
      INTEGER			VFID(NDSMAX)		! Vignetting datasets

      LOGICAL 			BG			! B/g data file found?
      LOGICAL 			BGCOR			! B/g data been exposure corrected?
      LOGICAL 			BGSUB			! B/g subtracted flag set in data?
      LOGICAL 			CHISTAT			! Chi-squared fitting?
      LOGICAL			GROUPS			! Grouping available
      LOGICAL 			LIKSTAT			! Likelihood fitting?
      LOGICAL 			LOG			! General purpose logical
      LOGICAL 			OK			! Data present and defined?
      LOGICAL 			QUAL			! Data quality info available?
      LOGICAL 			REF			! Input from ref file?
      LOGICAL			SLAVE			! Slave process?
      LOGICAL 			SPECSET(NDSCMAX)	! I/p is spectral set?
      LOGICAL			VIG			! Vignetting present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Slave process?
      SLAVE = MPC_QSLAVE( MPCID, STATUS )

*  Is the input a file set?
      CALL ADI_DERVD( ID, 'FileSet', REF, STATUS )

*  Set statistic flags
      IF ( FSTAT .EQ. FIT__LOGL ) THEN
	LIKSTAT = .TRUE.
        CHISTAT = .FALSE.
        WEIGHTS = .FALSE.
      ELSE
	LIKSTAT = .FALSE.
	CHISTAT = .TRUE.
      END IF

*  Initialize variables
      NDUFVAR = 0

*  Find datasets - get identifiers
      IF ( .NOT. REF ) THEN

*    Single input dataset container (directly referenced)
	NDSC = 1
        CALL ADI_CLONE( ID, DCFID(1), STATUS )

*    Spectral set?
        CALL SPEC_SETSRCH( DCFID(NDSC), SPECSET(NDSC), STATUS )

*    Flag to use all spectra
	IF (SPECSET(1)) DETNO(1) = 0

      ELSE

*    User has supplied a file set
        CALL ADI_CGET0I( ID, 'NFILE', NDSC, STATUS )

*    Loop over each referenced file in the file set
        DO I = 1, NDSC

*      Open the referenced file
          CALL FSI_FOPEN( ID, I, 'BinDS', DCFID(I), STATUS )

*      Is it a spectral set?
	  CALL SPEC_SETSRCH( DCFID(I), SPECSET(I), STATUS )

*      Find which spectra to use in a spectral set
          IF ( SPECSET(I) ) THEN

*        Get selection
c DCFID(I)
            CALL FSI_GETSEL( ID, I, NDETMAX, DETSEL(1,I),
     :                       DETNO(I), STATUS )

*        Trap selection absent
            IF ( STATUS .NE. SAI__OK ) THEN
              DETNO(I) = 0
              CALL ERR_ANNUL( STATUS )
            END IF

          END IF

        END DO

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Abort if maximum permitted number of datasets is exceeded
      IF ( NDSC .GT. NDSMAX ) THEN
	STATUS=SAI__ERROR
        CALL ERR_REP(' ','Maximum number of input datasets exceeded',
     :    STATUS)
	GOTO 99
      END IF

*  Loop through dataset containers
      NGOOD = 0
      SSCALE = 0
      NDS = 0
      DO N = 1, NDSC

*    Get dataset container file name and display
        CALL ADI_FOBNAM( DCFID(N), FILE, I, STATUS )
        IF ( .NOT. SLAVE ) THEN
	  IF ( NDSC .EQ. 1 ) THEN
            CALL MSG_PRNT( 'Dataset :-' )
            CALL MSG_PRNT( '   File : '//FILE(:I) )
	  ELSE
            CALL MSG_SETI( 'N', N )
            CALL MSG_PRNT( 'Dataset ^N :-' )
            CALL MSG_PRNT( '      File : '//FILE(:I) )
          END IF
        END IF
	IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*    Check main data array
        CALL BDI_CHK( DCFID(N), 'Data', OK, STATUS )
        CALL BDI_GETSHP( DCFID(N), ADI__MXDIM, DIMS, NDIM, STATUS )
	IF ( .NOT. OK ) THEN
	  STATUS = SAI__ERROR
	  CALL ERR_REP(' ','Error accessing data array',STATUS )
        END IF
	IF (STATUS.NE.SAI__OK) GOTO 99

*    Check that spectrum is exposure corrected (i.e in ct/s)
        CALL PRF_GET( DCFID(N), 'CORRECTED.EXPOSURE', LOG, STATUS )
        IF ( .NOT. SLAVE .AND. .NOT. LOG ) THEN
	  CALL MSG_PRNT(' ')
	  CALL MSG_PRNT('!! Warning - data must be corrected to ct/s')
          CALL MSG_PRNT(' ')
	END IF

*    Check existance of vignetting data
        CALL FRI_CHK( DCFID(N), 'VIGN', VIG, STATUS )
        IF ( VIG ) THEN

*      Open vignetting data
          CALL FRI_FOPEN( DCFID(N), 'VIGN', 'BinDS', 'READ', VFID(N),
     :                    STATUS )
	  IF ( STATUS .NE. SAI__OK ) THEN
	    CALL ERR_ANNUL( STATUS )
	    VIG = .FALSE.
          END IF

        END IF

*    Check that vignetting data array is same size as main data array
        IF ( VIG ) THEN
          CALL BDI_CHK( VFID(N), 'Data', OK, STATUS )
          CALL BDI_GETSHP( VFID(N), ADI__MXDIM, VDIMS, NVDIM, STATUS )
          IF (STATUS.NE.SAI__OK) THEN
	    CALL ERR_FLUSH(STATUS)
	    VIG=.FALSE.
	  ELSE IF (.NOT.OK) THEN
	    CALL MSG_PRNT('Error accessing vignetting data array')
	    VIG=.FALSE.
          END IF
          IF ( VIG ) THEN
	    IF (NVDIM.EQ.NDIM) THEN
	      DO I=1,NDIM
	        IF (VDIMS(I).NE.DIMS(I)) VIG=.FALSE.
	      END DO
            ELSE
	      VIG=.FALSE.
            END IF
            IF ( .NOT. VIG ) THEN
	      CALL MSG_PRNT('Ignoring vignetting data array as '/
     :                           /'dimensions don''t match data')
            END IF
          END IF
        END IF

*    Likelihood case?
	IF ( LIKSTAT ) THEN

*      Get effective exposure time
          CALL TCI_GETID( DCFID(N), TIMID, STATUS )
          CALL ADI_CGET0R( TIMID, 'EffExposure', TEFF, STATUS )

	  IF (STATUS.EQ.SAI__OK) THEN
	    CALL MSG_SETR('TEFF',TEFF)
            CALL MSG_PRNT('    Effective exposure time ^TEFF')
	  ELSE
	    CALL ERR_ANNUL(STATUS)
            CALL ADI_CGET0R( TIMID, 'Exposure', TEFF, STATUS )
	    IF (STATUS.EQ.SAI__OK) THEN
	      CALL MSG_SETR('TEFF',TEFF)
	      CALL MSG_PRNT('Effective exposure time not found - '//
     :          'using EXPOSURE_TIME value of ^TEFF')
            ELSE
              CALL ERR_ANNUL( STATUS )
              CALL MSG_PRNT( 'WARNING : No exposure times found in'/
     :                   /' dataset, exposure of 1 second assumed' )
              TEFF = 1.0
	    END IF
          END IF

*      Have data been b/g subtracted?
          CALL PRF_GET( DCFID(N), 'BGND_SUBTRACTED', BGSUB, STATUS )
          IF ( .NOT. SLAVE ) THEN
	    IF ( BGSUB ) THEN
	      CALL MSG_PRNT('    Background-subtracted data')
	    ELSE
	      CALL MSG_PRNT('    Not background-subtracted')
	    END IF
          END IF

*      Check existance of b/g data
          CALL FRI_CHK( DCFID(N), 'BGND', BG, STATUS )
          IF ( BG ) THEN

*        Open b/g data
            CALL FRI_FOPEN( DCFID(N), 'BGND', 'BinDS', 'READ', BFID(N),
     :                      STATUS )
	    IF ( STATUS .NE. SAI__OK ) THEN
	      CALL ERR_ANNUL( STATUS )
	      BG = .FALSE.
            END IF
          END IF

*      Check that background data array is same size as main data array
	    IF ( BG ) THEN
              CALL BDI_CHK( BFID(N), 'Data', OK, STATUS )
              CALL BDI_GETSHP( BFID(N), ADI__MXDIM, BDIMS, NBDIM,
     :                         STATUS )
	      IF (STATUS.NE.SAI__OK) THEN
	        CALL ERR_FLUSH(STATUS)
	        BG=.FALSE.
	      ELSE IF (.NOT.OK) THEN
	        CALL MSG_PRNT('Error accessing b/g data array')
	        BG=.FALSE.
	      END IF
	      IF (NBDIM.EQ.NDIM) THEN
	        DO I=1,NDIM
	          IF (BDIMS(I).NE.DIMS(I)) BG=.FALSE.
	        END DO
	      ELSE
	        BG=.FALSE.
	      END IF

*         Warn user and exit if it doesn't match
	      IF (.NOT.BG) THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP(' ','Background array does not match'//
     :          ' data array',STATUS)
	        GOTO 99
	      END IF

*      Abort if b/g has been subtracted, but is not available
	      IF (BGSUB.AND.(.NOT.BG)) THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP('NOBG','Subtracted background data is'//
     :          ' not available',STATUS)
	        GOTO 99
	      END IF

*      Has background been exposure corrected? (We will require raw count
*                               contribution from b/g expected in the data)
              CALL PRF_GET( BFID(N), 'CORRECTED.EXPOSURE', BGCOR,
     :                      STATUS )

	    END IF

	    IF (.NOT.BG.AND..NOT.SLAVE) THEN
	      CALL MSG_PRNT('    No background data found -'//
     :        ' assumed negligible')
	    END IF
	  END IF

*    Is container a spectral set?
	  IF (SPECSET(N)) THEN

*    Spectral set - find number of component spectra
	    IF (NDIM.NE.2) THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP(' ','Spectral set has incorrect '//
     :        'dimensionality',STATUS)
	    END IF
	    IF (STATUS.NE.SAI__OK) GOTO 99
	    CALL MSG_SETI('NSPEC',DIMS(2))
	    CALL MSG_PRNT('Spectral set containing ^NSPEC spectra')
	    IF (DETNO(N).GT.0) THEN
	      IF (DETNO(N).LT.DIMS(2)) THEN
	        SETSIZE=DETNO(N)
	      ELSE
	        SETSIZE=DIMS(2)
	        DETNO(N)=0		! Flag for `use all spectra'
	      END IF
	      IF (SETSIZE.EQ.1) THEN
	        WRITE(*,100) (DETSEL(I,N),I=1,SETSIZE)
 100	        FORMAT(' Using component number: ',I3)
	      ELSE
	        WRITE(*,110) (DETSEL(I,N),I=1,SETSIZE)
 110	        FORMAT(' Using numbers: ',(I3))
c110	        FORMAT(' Using numbers: ',<SETSIZE>(I3))
	      END IF
	    ELSE
	      SETSIZE = DIMS(2)
	      CALL MSG_PRNT( 'Using all spectra' )
	    END IF

	  ELSE

*    Not a spectral set
	    SETSIZE = 1

	  END IF

*    Loop through component spectra required from a given dataset
	  INDEX = 0
	  NDSTOP = NDS + SETSIZE
	  DO WHILE ( NDS .LT. NDSTOP )
	    NDS=NDS+1

*       Abort if maximum permitted number of datasets is exceeded
	    IF (NDS.GT.NDSMAX) THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP(' ','Maximum number of input datasets '//
     :        'exceeded',STATUS)
	      GOTO 99
	    END IF

*       Set up name, locator and set position
            CALL ADI_CLONE( DCFID(N), DATASET_D_ID(NDS), STATUS )

	    IF (SPECSET(N)) THEN
	      INDEX=INDEX+1
	      IF (DETNO(N).GT.0) THEN
	        SPECNO=DETSEL(INDEX,N)
	      ELSE
	        SPECNO=INDEX
	      END IF
	      CALL CHR_ITOC(SPECNO,SPECH,NCH)
	      DATASET_DATNAME(NDS)=
     :          FILE(1:CHR_LEN(FILE))//' Spectrum '//SPECH
	    ELSE
	      SPECNO=0
	      DATASET_DATNAME(NDS) = FILE
	    END IF
	    DATASET_SETINDEX(NDS) = SPECNO

*       Map the data array
            CALL BDI_MAPR( DATASET_D_ID(NDS), 'Data', 'READ', TPTR,
     :                     STATUS )

*       Find and map data array
	    IF ( SPECSET(N) ) THEN

*          Map dynamic memory for slice
              CALL DYN_MAPR( 1, DIMS(1), DATASET_DPTR(NDS), STATUS )

*          Define the slice
	      LDIM(1) = 1
	      LDIM(2) = SPECNO
	      UDIM(1) = DIMS(1)
	      UDIM(2) = SPECNO
	      DATASET_NDIM(NDS) = 1
	      DATASET_IDIM(NDS,1) = DIMS(1)
	      DATASET_NDAT(NDS) = DIMS(1)

*          Copy the slice
              CALL ARR_SLCOPR( 2, DIMS, %VAL(TPTR), LDIM, UDIM,
     :                         %VAL(DATASET_DPTR(NDS)), STATUS )

	    ELSE

*          Simple mapping
	      DATASET_NDIM(NDS) = NDIM
	      DO I = 1,DATASET_NDIM(NDS)
	        DATASET_IDIM(NDS,I) = DIMS(I)
	      END DO
              DATASET_DPTR(NDS) = TPTR
              CALL ARR_SUMDIM( NDIM, DIMS, DATASET_NDAT(NDS) )

	    END IF
	    IF ( STATUS .NE. SAI__OK ) GOTO 99

*         For likelihood case scale to give raw counts, & accumulate SSCALE
	    IF ( LIKSTAT ) THEN
	      PTR = DATASET_DPTR(NDS)
	      CALL DYN_MAPR(1,DATASET_NDAT(NDS),DATASET_DPTR(NDS),
     :                                                          STATUS)
	      CALL ARR_COP1R(DATASET_NDAT(NDS),%VAL(PTR),
     :        %VAL(DATASET_DPTR(NDS)),STATUS)
	      IF (STATUS.NE.SAI__OK) GOTO 99
	      CALL ARR_MULTR(TEFF,DATASET_NDAT(NDS),
     :           %VAL(DATASET_DPTR(NDS)),STATUS )
	    END IF

*       Map variance and quality and use to set up array of data weights

*          Get variance (slice in case of spectral set)
            CALL BDI_CHK( DATASET_D_ID(NDS), 'Variance', OK, STATUS )
	    IF ( OK ) THEN
	      CALL BDI_MAPR(DATASET_D_ID(NDS),'Variance','READ',TPTR,
     :                      STATUS)
	      IF (SPECSET(N)) THEN
                CALL DYN_MAPR( 1, DIMS(1), DATASET_VPTR(NDS), STATUS )
                CALL ARR_SLCOPR( 2, DIMS, %VAL(TPTR), LDIM, UDIM,
     :                           %VAL(DATASET_VPTR(NDS)), STATUS )
	      ELSE
                DATASET_VPTR(NDS) = TPTR
	      END IF
	    ELSE
	      IF ( WEIGHTS ) THEN
	        CALL MSG_SETI('NDS',NDS)
	        STATUS=SAI__ERROR
	        CALL ERR_REP( ' ', 'No error information available '//
     :          'in dataset ^NDS',STATUS)
	      ELSE
	        DATASET_VPTR(NDS)=0			! Flag
	      END IF
	    END IF
	    IF (STATUS.NE.SAI__OK) GOTO 99

*        Get quality
            CALL BDI_CHK( DATASET_D_ID(NDS), 'Quality', QUAL, STATUS )
	    IF (QUAL) THEN
	      CALL BDI_MAPL( DATASET_D_ID(NDS), 'LogicalQuality',
     :                       'READ', TPTR, STATUS )
	      IF (SPECSET(N)) THEN
                CALL DYN_MAPR( 1, DIMS(1), DATASET_QPTR(NDS), STATUS )
                CALL ARR_SLCOPL( 2, DIMS, %VAL(TPTR), LDIM, UDIM,
     :                           %VAL(DATASET_QPTR(NDS)), STATUS )
	      ELSE
                DATASET_QPTR(NDS) = TPTR
	      END IF

*          Set quality flag
	      DATASET_QFLAG(NDS) = .TRUE.

	    ELSE
	      DATASET_QFLAG(NDS)=.FALSE.
	      DATASET_QPTR(NDS)=0
	    END IF
	    IF (STATUS.NE.SAI__OK) THEN
	      CALL ERR_ANNUL(STATUS)
	      QUAL=.FALSE.
	    END IF

*        Check grouping?
            CALL BDI_CHK( DATASET_D_ID(NDS), 'Grouping', GROUPS,
     :                    STATUS )
            IF ( GROUPS ) THEN

	      CALL BDI_MAPI( DATASET_D_ID(NDS), 'Grouping',
     :                       'READ', TPTR, STATUS )
	      IF (SPECSET(N)) THEN
                CALL DYN_MAPR( 1, DIMS(1), DATASET_GPTR(NDS), STATUS )
                CALL ARR_SLCOPI( 2, DIMS, %VAL(TPTR), LDIM, UDIM,
     :                           %VAL(DATASET_GPTR(NDS)), STATUS )
	      ELSE
                DATASET_GPTR(NDS) = TPTR

	      END IF

*          Count number of groups
              CALL UTIL_CNTGRP( DATASET_NDAT(NDS),
     :                          %VAL(DATASET_GPTR(NDS)),
     :                          DATASET_NGDAT(NDS), STATUS )
              IF ( .NOT. SLAVE ) THEN
                CALL MSG_SETI( 'NG', DATASET_NGDAT(NDS) )
	        CALL MSG_PRNT('    Loaded grouping array with '/
     :                         /'^NG groups')
              END IF

*          Create workspace for grouped data
              CALL DYN_MAPR( 1, DATASET_NGDAT(NDS),
     :                       DATASET_GDPTR(NDS), STATUS )
              IF ( DATASET_QFLAG(NDS) ) THEN
                CALL DYN_MAPL( 1, DATASET_NGDAT(NDS),
     :                         DATASET_GQPTR(NDS), STATUS )
              END IF
              IF ( DATASET_VPTR(NDS) .NE. 0 ) THEN
                CALL DYN_MAPR( 1, DATASET_NGDAT(NDS),
     :                         DATASET_GVPTR(NDS), STATUS )
              END IF

*          Set grouping flag
	      DATASET_GFLAG(NDS) = .TRUE.

*        In the absence of grouping make the group data pointers point
*        to the input data
            ELSE
	      DATASET_GFLAG(NDS) = .FALSE.
              DATASET_GDPTR(NDS) = DATASET_DPTR(NDS)
              DATASET_GVPTR(NDS) = DATASET_VPTR(NDS)
              DATASET_GQPTR(NDS) = DATASET_QPTR(NDS)
              DATASET_NGDAT(NDS) = DATASET_NDAT(NDS)

            END IF

*        Map weights as 1D array
	    IF ( WEIGHTS ) THEN
	      CALL DYN_MAPR(1,DATASET_NDAT(NDS),DATASET_WPTR(NDS),
     :                      STATUS)
              IF ( DATASET_GFLAG(NDS) ) THEN
                CALL DYN_MAPR( 1, DATASET_NGDAT(NDS),
     :                         DATASET_GWPTR(NDS), STATUS )
              ELSE
                DATASET_GWPTR(NDS) = DATASET_WPTR(NDS)
              END IF
	      IF (STATUS.NE.SAI__OK) GOTO 99
	    ELSE
	      DATASET_WPTR(NDS) = 0				! Flag
              DATASET_GWPTR(NDS) = 0
	    END IF

*        Default value for vignetting object
            DATASET_V_ID(NDS) = ADI__NULLID
            IF ( VIG ) THEN

*        Store identifier
              DATASET_V_ID(NDS) = VFID(N)

*        Map vignetting array
              CALL BDI_MAPR( VFID(N), 'Data', 'READ', DATASET_GPTR(NDS),
     :                          STATUS )
	      CALL MSG_PRNT('    Loaded associated vignetting array')

            END IF

*        Default value for background object
            DATASET_B_ID(NDS) = ADI__NULLID

*        For likelihood case, Set up OBDAT.TEFF and get background data
	    IF ( LIKSTAT ) THEN
	      DATASET_TEFF(NDS)=TEFF
	      IF (BG) THEN

*            Store background object
                DATASET_B_ID(NDS) = BFID(N)

*            Map background data
	        CALL BDI_MAPR( BFID(N), 'Data', 'READ', TPTR, STATUS )

*            Find and map b/g data array
	        IF (SPECSET(N)) THEN
                  CALL DYN_MAPR( 1, DIMS(1), DATASET_BPTR(NDS), STATUS )
                  CALL ARR_SLCOPR( 2, DIMS, %VAL(TPTR), LDIM, UDIM,
     :                             %VAL(DATASET_BPTR(NDS)), STATUS )
	        ELSE
                  DATASET_BPTR(NDS) = TPTR

	        END IF

*           Scale b/g to give raw counts if it has been exposure corrected
	        IF ( BGCOR ) THEN
	          PTR=DATASET_BPTR(NDS)
	          CALL DYN_MAPR(1,DATASET_NDAT(NDS),DATASET_BPTR(NDS),
     :                          STATUS)
	          CALL ARR_COP1R(DATASET_NDAT(NDS),%VAL(PTR),
     :                           %VAL(DATASET_BPTR(NDS)),STATUS)
	          IF (STATUS.NE.SAI__OK) GOTO 99
	          CALL ARR_MULTR(TEFF,DATASET_NDAT(NDS),
     :                           %VAL(DATASET_BPTR(NDS)),STATUS)
	        END IF

*            If b/g has been subtracted then add it back into data and SSCALE
	        IF ( BGSUB ) THEN
	          CALL VEC_ADDR(.FALSE.,DATASET_NDAT(N),%VAL(DATASET_BPTR(N)),
     :            %VAL(DATASET_DPTR(N)),%VAL(DATASET_DPTR(N)),IERR,NERR,
     :            STATUS)
	        END IF

*         No background data - set up array of zeros
	      ELSE
	        CALL DYN_MAPR(1,DATASET_NDAT(NDS),DATASET_BPTR(NDS),STATUS)
	        CALL ARR_INIT1R(0.0,DATASET_NDAT(NDS),
     :                      %VAL(DATASET_BPTR(NDS)),STATUS)
	      END IF

	    END IF

*        Grouping specified?
            IF ( DATASET_GFLAG(NDS) ) THEN
              CALL UTIL_GRPVR( DATASET_NDAT(NDS),
     :               %VAL(DATASET_DPTR(NDS)),
     :               (DATASET_VPTR(NDS).NE.0), %VAL(DATASET_VPTR(NDS)),
     :               DATASET_QFLAG(NDS), %VAL(DATASET_QPTR(NDS)),
     :               %VAL(DATASET_GPTR(NDS)), DATASET_NGDAT(NDS),
     :               %VAL(DATASET_GDPTR(NDS)), %VAL(DATASET_GVPTR(NDS)),
     :               %VAL(DATASET_GQPTR(NDS)), STATUS )
            END IF

*        Enter weights (=inverse variances)
            IF ( WEIGHTS ) THEN
	      CALL FIT_GETDAT_WTS( DATASET_NGDAT(NDS),
     :                   %VAL(DATASET_GVPTR(NDS)), DATASET_QFLAG(NDS),
     :                   %VAL(DATASET_GQPTR(NDS)),
     :                   %VAL(DATASET_GWPTR(NDS)), NDUFVAR, STATUS )
            END IF

*        Compute number of good points
            IF ( DATASET_QFLAG(NDS) ) THEN
              CALL ARR_CNT1L( DATASET_NGDAT(NDS),
     :                        %VAL(DATASET_GQPTR(NDS)),
     :                        .TRUE., CNGOOD, STATUS )
            ELSE
              CNGOOD = DATASET_NGDAT(NDS)
            END IF
            CNGOOD = CNGOOD - NDUFVAR
            IF ( CNGOOD .GT. 0 ) THEN
              NGOOD = NGOOD + CNGOOD
            ELSE
	      CALL MSG_SETI('NDS',NDS)
	      STATUS=SAI__ERROR
	      CALL ERR_REP(' ', 'No good data in dataset ^NDS', STATUS )
	      GOTO 99
            END IF

*        Accumulate counts for data in likelihood case. Use quality if
*        present in input data (rather than bgnd)
            IF ( LIKSTAT ) THEN

*          Accumulate counts for data
              IF ( DATASET_QFLAG(NDS) ) THEN
	        CALL FIT_GETDAT_COUNTSQ(DATASET_NDAT(NDS),
     :                      %VAL(DATASET_DPTR(NDS)),
     :                      %VAL(DATASET_QPTR(NDS)),SSCALE)
                CALL FIT_GETDAT_LNDFACQ(DATASET_NDAT(NDS),
     :                      %VAL(DATASET_DPTR(NDS)),
     :                      %VAL(DATASET_QPTR(NDS)),
     :                      LNDFAC)
              ELSE
	        CALL ARR_SUM1R( DATASET_NDAT(NDS),
     :                          %VAL(DATASET_DPTR(NDS)),
     :                          RSUM, STATUS )
                SSCALE = SSCALE + NINT(RSUM)
                CALL FIT_GETDAT_LNDFAC(DATASET_NDAT(NDS),
     :                                 %VAL(DATASET_DPTR(NDS)),
     :                                 LNDFAC)
              END IF

            END IF

*............ OBDAT set up ..............................................

*    Hardwire CONVOLVE flag false in cluster fitting.
            IF ( GENUS .EQ. 'CLUS' ) THEN
              PREDICTION_CONVOLVE(N) = .FALSE.
            ELSE
*      Look for instrument response, set up INSTR if found, and report
	      CALL FIT_GETINS( DATASET_D_ID(NDS), SPECNO, 1,
     :               PREDICTION_CONVOLVE(NDS), NDS, STATUS )
	      IF (STATUS.NE.SAI__OK) GOTO 99
            END IF

	    IF (PREDICTION_CONVOLVE(NDS)) THEN

*      Check that data are 1D
	      IF (DATASET_NDIM(NDS).GT.1) THEN
	        STATUS=SAI__ERROR
	        CALL ERR_REP(' ','Convolution with instrument response'
     :          //' is only supported for 1D data at present',STATUS)
	        GOTO 99
	      END IF
	    END IF

*      Find remaining components of PREDDAT(NDS)
	  CALL FIT_PREDSET( DATASET_D_ID(NDS), NDS, WORKSPACE,
     :                      NDS, NDS, STATUS )

	END DO

      END DO

*  Exit point
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_GETDAT', STATUS )
      END IF

      END


*+  FIT_GETDAT_COUNTSQ - Sums raw counts in dataset
      SUBROUTINE FIT_GETDAT_COUNTSQ(NDAT,DATA,QUAL,COUNTS)
*    Description :
*     Counts from the (REAL) DATA array are accumulated in COUNTS
*    History :
*     15 Jun 92: Original (TJP)
*    Type definitions :
	IMPLICIT NONE
*    Import :
	INTEGER NDAT		! No of data values
	REAL DATA(*)		! Data array
        LOGICAL QUAL(*)         ! Quality array
*    Import-Export :
	INTEGER COUNTS		! Counts accumulator
*    Export :
*    Local constants :
*    Local variables :
	INTEGER I
*-

	DO I=1,NDAT
          IF ( QUAL(I) ) COUNTS=COUNTS+NINT(DATA(I))
	END DO
	END


*+  FIT_GETDAT_WTS - Sets up array of data weights
      SUBROUTINE FIT_GETDAT_WTS( NDAT, VARR, QUAL, QARR, WTARR, NDUF,
     :                           STATUS )
*    Description :
*     Array of data weights equal to inverse variance for good data and
*     zero for bad, is set up.
*     Data with errors less than or equal to zero are treated as bad.
*    History :
*     30 Mar 87: Original (TJP)
*     16 Aug 88: WEIGHTS argument added (TJP)
*     14 Feb 89: ASTERIX88 version, variance and logical quality passed in (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
        INTEGER			STATUS
*    Import :
	INTEGER NDAT		! No of data values
	REAL VARR(*)		! Array of data variances
	LOGICAL QUAL		! Quality info present?
	LOGICAL QARR(*)		! Quality array
*    Export :
	REAL WTARR(*)		! Data weights
	INTEGER NDUF		! # bad variances in otherwise good points
*    Local variables :
	INTEGER I
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No duffers initially
      NDUF = 0

*  Set up weights
      IF ( QUAL ) THEN
	DO I = 1, NDAT
	  IF ( QARR(I) ) THEN
            IF ( VARR(I) .GT. 0.0 ) THEN
	      WTARR(I) = 1.0 / VARR(I)
	    ELSE
	      NDUF = NDUF + 1
	      WTARR(I) = 0.0
	    END IF
	  END IF
        END DO

*   Otherwise no quality
      ELSE
	DO I = 1, NDAT
          IF ( VARR(I) .GT. 0.0 ) THEN
	    WTARR(I) = 1.0 / VARR(I)
	  ELSE
	    NDUF = NDUF + 1
	    WTARR(I) = 0.0
	  END IF
        END DO

*  End of switch on quality
      END IF

      END


*+  FIT_GETDAT_LNDFACQ - Calculate lnd! for dataset
      SUBROUTINE FIT_GETDAT_LNDFACQ(NDAT,DATA,QUAL,LNDFAC)
*    Description :
*     lnd! from the (REAL) DATA array are accumulated in LNDFAC
*    History :
*     7 Nov 97: Original (ELD)
*    Type definitions :
        IMPLICIT NONE
*    Import :
        INTEGER NDAT            ! No of data values
        REAL DATA(*)            ! Data array
        LOGICAL QUAL(*)         ! Quality array
*    Import-Export :
        DOUBLE PRECISION LNDFAC ! lnd!
*    Export :
*    Local constants :
*    Local variables :
        INTEGER I,J
*-

        DO I=1,NDAT
          IF ( QUAL(I) ) THEN
            IF (NINT(DATA(I)).GT.0) THEN
              DO J=1,NINT(DATA(I))
                LNDFAC=LNDFAC+LOG(DBLE(J))
              END DO
            END IF
          END IF
        END DO
        END


*+  FIT_GETDAT_LNDFAC - Calculate lnd! for dataset
      SUBROUTINE FIT_GETDAT_LNDFAC(NDAT,DATA,LNDFAC)
*    Description :
*     lnd! from the (REAL) DATA array are accumulated in LNDFAC
*    History :
*     7 Nov 97: Original (ELD)
*    Type definitions :
        IMPLICIT NONE
*    Import :
        INTEGER NDAT            ! No of data values
        REAL DATA(*)            ! Data array
*    Import-Export :
        DOUBLE PRECISION LNDFAC ! lnd!
*    Export :
*    Local constants :
*    Local variables :
        INTEGER I,J
*-

        DO I=1,NDAT
          IF (NINT(DATA(I)).GT.0) THEN
            DO J=1,NINT(DATA(I))
              LNDFAC=LNDFAC+LOG(DBLE(J))
            END DO
          END IF
        END DO
        END
