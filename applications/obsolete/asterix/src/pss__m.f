*+  PSS_M_SEARCH - Executes SEARCH mode for PSS
      SUBROUTINE PSS_M_SEARCH( NFILE, IFILE, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 89 : Original (DJA)
*      6 Aug 92 : Added ENCPSF field (DJA)
*     30 Apr 93 : Permit parameter errors to be switched off. Helps speed
*                 on large simulations. (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SRCLIB(POI_STR)'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NFILE                   ! Number of input files
      INTEGER                  IFILE                   ! File being processed
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      REAL                     AXV
      INTEGER                  CHR_LEN
      LOGICAL                  PSS_IN_SLICE
*
*    Local constants :
*
      INTEGER                  ZOOM_BOX                ! Box size on second pass
        PARAMETER              ( ZOOM_BOX = 3 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SSDS dataset
      CHARACTER*80             TEXT                    !
      CHARACTER*40             UNITS                   ! Background units

      DOUBLE PRECISION         FLEV(PSS__MXEL)         ! Flux error levels in %
      DOUBLE PRECISION         PLEV(PSS__MXEL)         ! Pos error levels in %

      REAL                     FVEC(2)		       ! Psf shift vector
      REAL                     MULREJ                  ! Multiple rejection
      REAL                     SIGMIN(2)               ! Significance threshold
      REAL                     SMIN, SMAX              ! Min and max of SMAP

      INTEGER                  ID                      ! A source's id number
      INTEGER                  ISRC                    ! Loop over source list
      INTEGER                  ITER                    ! Iteration counter
      INTEGER                  NFLEV                   ! # flux error levels
      INTEGER                  NPLEV                   ! # pos error levels
      INTEGER                  NED                     ! # items per error level
      INTEGER                  NSIG                    ! # sigmin values
      INTEGER                  O_BK, O_EBK             ! Source bgnd list
      INTEGER                  O_DC                    ! Source delta C
      INTEGER                  O_EXS                   ! Source extension signif
      INTEGER                  O_EXT, O_EEXT           ! Source extension + err
      INTEGER                  O_FLX, O_EFLX           ! Source flux + error
      INTEGER                  O_EPSF                  ! Source psf enclosed
      INTEGER                  O_SIG, O_ESIG           ! Source significances
      INTEGER                  O_XC, O_YC              ! Source X & Y coords
      INTEGER                  O_EXC, O_EYC            ! Source X & Y errors
      INTEGER                  O_PERR                  ! Source image pos errors
      INTEGER                  O_PP                    ! Source Poisson prob'y
      INTEGER                  O_RA                    ! Source image RA
      INTEGER                  O_DEC                   ! Source image DEC
      INTEGER                  OSIGDIM(2)              ! Last significance dims
      INTEGER                  SIGDIM(2)               ! Significance dims
      INTEGER                  MPTR                    ! Ptr to SMAP data
      INTEGER                  TLEN                    ! Length of TEXT used
      INTEGER                  TOO_MANY_SRC            ! Too many sources

      LOGICAL                  ANYOUT                  ! Source found out slice?
      LOGICAL                  ASYMET                  ! Asymmetric errors?
      LOGICAL                  RESTART                 ! Restart with sig map?
      LOGICAL                  TRY_AGAIN               ! Try again > SIGMIN ?
*
*    Preserve to next call :
*
      SAVE                     ASYMET,MULREJ,SIGMIN,OSIGDIM,MPTR
*
*    Local data :
*
      DATA                     FVEC/0.0,0.0/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Grab a slot for the initial box
      LI_NSRC = 0

*    Running in restart mode?
      IF ( CP_MULTI ) THEN
        RESTART = .FALSE.
      ELSE
        CALL PAR_GET0L( 'RESTART', RESTART, STATUS )
      END IF

*    Initialise grid
      CALL PSS_GRID_INIT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( IFILE .GT. 1 ) GR_PASS = 2

*    Significance map dimensions
      SIGDIM(1) = MAX( ZOOM_BOX, GR_DIMS(1)+1 )
      SIGDIM(2) = MAX( ZOOM_BOX, GR_DIMS(2)+1 )

*    Map memory for significance - get enough for zoom mode AND search. In
*    MULTI mode, if this is the 2nd or subsequent file and the space requested
*    is not larger than the last lot of space grabbed, use that. Otherwise
*    unmap the oldspace and remap.
      IF ( CP_MULTI .AND. (IFILE.GT.1) ) THEN
        IF ( SIGDIM(1)*SIGDIM(2) .GT. OSIGDIM(1)*OSIGDIM(2) ) THEN
          CALL DYN_UNMAP( MPTR, STATUS )
          CALL DYN_MAPR( 2, SIGDIM, MPTR, STATUS )
        END IF
      ELSE
        CALL DYN_MAPR( 2, SIGDIM, MPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set up the psf access
      IF ( PSF_CONSTANT ) THEN
        PSF_ACCESS = PER_FILE
      ELSE
        PSF_ACCESS = PER_PIXEL
      END IF

*    Map extra psf space for gaussian convolution
      IF ( CP_FITWIDTH ) THEN
        CALL DYN_MAPR( 2, PSF_DIMS, PSF_CONPTR, STATUS )
        CALL DYN_MAPR( 1, (PSF_DIMS(1)*2+1)**2, PSF_CONWPTR,STATUS )
      END IF

*    Zero the source list
      LI_NSRC = 0

*    Define significance and flux convergence to absolute 0.05. Cannot
*    do this for rescaling (due to bgnd), so do relative
      IF ( CP_RESCALE ) THEN
        GE_CONVERGE_METHOD = PSS__RELATIVE
        GE_CONVERGE_TOL = 0.01
      ELSE
        GE_CONVERGE_METHOD = PSS__ABSOLUTE
        GE_CONVERGE_TOL = 0.05
      END IF

*    Construct significance map
      IF ( RESTART ) THEN

*      Read map from file and copy to workspace
        CALL PSS_MAP_IN( SIGDIM, MPTR, STATUS )

*      Evaluate PSF at centre of interest
        CALL PSS_PSF_SUBSET( GR_CC, FVEC, %VAL(PSF_STORE), STATUS )

*      Make copy if constant across field
        CALL ARR_COP1R( PSF_UDIMS(1)*PSF_UDIMS(2), %VAL(PSF_STORE),
     :                  %VAL(PSF_DATA), STATUS )

      ELSE

*      Announce grid spacing
        CALL MSG_SETR( 'GRID', GR_DX/AX_DR(1) )
        CALL MSG_PRNT( 'First pass - grid spacing ^GRID pixels' )

*      Evaluate the statistic in the image area specified by the
*      components in the IDth box.
        CALL PSS_STAT( %VAL(GR_ROUTINE), 1, 0.0, MPTR, STATUS )

      END IF

*    Decide how many sources would be too many. Divide area of image
*    by area of psf at 68% enclosed energy.
      TOO_MANY_SRC = NINT(REAL(GR_NELM)/MATH__PI/
     :                               REAL(PSF_PIXL(2,1)**2.0))
      TOO_MANY_SRC = MAX( TOO_MANY_SRC, 10 )

*    Tell user about properties of statistic evaluated - first time
*    through only. Find min and max in significance map
      CALL ARR_RANG1R( GR_NELM, %VAL(MPTR), SMIN, SMAX, STATUS )

*    Output a map of the statistic
      IF ( .NOT. (RESTART.OR.CP_MULTI) ) THEN
        CALL PSS_MAP_OUT( 'MAP', %VAL(MPTR), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Tell user significance range
      CALL MSG_SETR( 'SMIN', SMIN )
      CALL MSG_SETR( 'SMAX', SMAX )
      CALL MSG_PRNT( 'Significance varies from ^SMIN to ^SMAX' )

 49   IF ( IFILE .EQ. 1 ) THEN

*      Get threshold(s) for search
        CALL PAR_GET1R( 'SIGMIN', 2, SIGMIN, NSIG, STATUS )
        CALL PAR_CANCL( 'SIGMIN', STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      If only one threshold, copy to second
        IF ( NSIG .EQ. 1 ) SIGMIN(2) = SIGMIN(1)

      END IF

*    Search the box for sources
      CALL PSS_MAP_SEARCH( GR_DIMS(1), GR_DIMS(2), %VAL(MPTR),
     :                                 SIGMIN(1)*0.8, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    No sources found?
      IF ( LI_NSRC .EQ. 0 ) THEN
        CALL MSG_SETR( 'SIG', SIGMIN )
        CALL MSG_PRNT( 'No sources found with significance > ^SIG' )
        IF ( CP_MULTI ) THEN
          TRY_AGAIN = .FALSE.
        ELSE
          CALL PAR_GET0L( 'TRY_AGAIN', TRY_AGAIN, STATUS )
          CALL PAR_CANCL( 'TRY_AGAIN', STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE IF ( TRY_AGAIN ) THEN
          GOTO 49
        ELSE
          LI_NSRC = 0
          GOTO 69
        END IF

*    Too many sources found?
      ELSE IF ( LI_NSRC .GT. TOO_MANY_SRC ) THEN
        CALL MSG_SETI( 'N', LI_NSRC )
        CALL MSG_PRNT( 'Found a lot of sources! ( ^N )' )
        CALL PAR_PROMT( 'TRY_AGAIN', 'Try again with a'/
     :       /' higher significance threshold', STATUS )
        IF ( CP_MULTI ) THEN
          TRY_AGAIN = .FALSE.
        ELSE
          CALL PAR_GET0L( 'TRY_AGAIN', TRY_AGAIN, STATUS )
          CALL PAR_CANCL( 'TRY_AGAIN', STATUS )
        END IF
        IF ( ( STATUS .EQ. SAI__OK ) .AND. TRY_AGAIN ) THEN

*        Free all objects in source list
          DO ISRC = 1, LI_NSRC
            CALL PSS_SRC_FREE( ISRC, STATUS )
          END DO
          LI_NSRC = 0
          GOTO 49

        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        END IF

      END IF

*    Check sources lie in slice
      ANYOUT = .FALSE.
      DO ISRC = 1, LI_NSRC
        IF ( .NOT. PSS_IN_SLICE(S_CP(1,LI_ID(ISRC))) ) THEN
          LI_ACTIVE(ISRC) = .FALSE.
          ANYOUT = .TRUE.
        END IF
      END DO
      IF ( ANYOUT ) CALL PSS_SRC_SQUEEZE( STATUS )

*    Get multiplier for critical radius
      IF ( IFILE .EQ. 1 ) THEN
        CALL PAR_GET0R( 'MULREJ', MULREJ, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Reject multiples
      CALL PSS_CHK_MULT( MULREJ, STATUS )

*    Tell user about sources
      CALL PSS_SRC_DUMP( 1, STATUS )

*    Optimise positions in order to apply sig threshold precisely
      PSF_RESAM = .TRUE.
      CALL MSG_PRNT( 'Second pass' )
      GE_CONVERGE_TOL = GE_CONVERGE_TOL / 10.0
      GR_PASS = 2
      IF ( .NOT. PSF_CONSTANT ) PSF_ACCESS = PER_BOX

*    Loop over each box in the LIST
      DO ISRC = 1, LI_NSRC

*      Locate source in SRC data
        ID = LI_ID(ISRC)

*      Adjust source position a couple of times
        DO ITER = 1, 2

*        Put a box round the source, decide on grid size etc.
          CALL PSS_SRC_POS( S_CP(1,ID), ZOOM_BOX, AX_DR(1),
     :                                   AX_DR(2), STATUS )

*        Evaluate the statistic in the image area specified by the
*        components in the Ith box
          CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0, MPTR, STATUS )

*        Find best peak in box
          CALL PSS_MAP_PEAK( ID, GR_DIMS(1), GR_DIMS(2),
     :                                  %VAL(MPTR), STATUS )

        END DO

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Skip this bit if no sources
      IF ( LI_NSRC .GT. 0 ) THEN

*      Last check that sources are above threshold and in slice
        DO ISRC = 1, LI_NSRC
          ID = LI_ID(ISRC)
          IF ( (S_SIG(ID).LT.SIGMIN(1)) .OR. .NOT. PSS_IN_SLICE
     :                                        (S_CP(1,ID)) ) THEN
            LI_ACTIVE(ISRC) = .FALSE.
          END IF
        END DO

*      Compactify the list
        CALL PSS_SRC_SQUEEZE( STATUS )

*      Reject multiples again
        CALL PSS_CHK_MULT( MULREJ, STATUS )

      END IF

*    Filter due to too many sources?
      IF ( DI_MAX_FILTER ) THEN
        CALL PSS_SRC_MAXFILT( STATUS )
      END IF

*    Symmetric errors?
      IF ( CP_EXPERT ) THEN
        IF ( IFILE .EQ. 1 ) THEN
          CALL PAR_GET0L( 'ASYMMETRIC', ASYMET, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      ELSE
        ASYMET = .FALSE.
      END IF

*    Number of data elements per error level
      IF ( ASYMET ) THEN
        NED = 2
      ELSE
        NED = 1
      END IF

*    90% symmetric errors in IDIOT mode, prompt in EXPERT
      IF ( IFILE .EQ. 1 ) THEN
        IF ( CP_EXPERT ) THEN

*        Flux errors
          CALL PAR_PROMT( 'FERL', 'Flux error confidence level',
     :                                                  STATUS )
          CALL PSS_GET_CONF('FERL', PSS__MXEL, TEXT, FLEV, NFLEV,
     :                                                   STATUS )

*        Positional errors
          CALL PSS_GET_CONF('PERL', PSS__MXEL, TEXT, PLEV, NPLEV,
     :                                                   STATUS )

        ELSE

          NFLEV = 1
          FLEV(1) = 68.0D0
          CALL MSG_PRNT( 'Symmetric flux errors at 68% confidence, '/
     :                                /'and position errors at 90%' )
          NPLEV = 1
          PLEV(1) = 90.0D0
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Perform last pass on sources
      GE_CONVERGE_METHOD = PSS__RELATIVE
      GE_CONVERGE_TOL = 1.0E-5
      DO ISRC = 1, LI_NSRC

*      Locate source in SRC data. Set up data for condition handler
	ID = LI_ID(ISRC)
	GE_EXEC_ISRC = ID
	GE_EXEC_NSRC = ISRC

*      Set background scale factor if not rescaling
        IF ( .NOT. CP_RESCALE ) S_BSCALE(ID) = 1.0

*      Evaluate statistic at position
        CALL PSS_SRC_POS( S_CP(1,ID), 1, AX_DR(1), AX_DR(2), STATUS )
        CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0, MPTR, STATUS )

*      Optimise parameters
        CALL PSS_FIT( ID, .FALSE., ASYMET, NFLEV, FLEV, NPLEV, PLEV,
     :                                                      STATUS )

      END DO

*   Remove any sources still below threshold after fitting
*     Mark the weedy ones
      DO ISRC = 1, LI_NSRC
        LI_ACTIVE(ISRC) = ( S_SIG(LI_ID(ISRC)) .GE. SIGMIN(2) )
      END DO

*    Compactify the list
      CALL PSS_SRC_SQUEEZE( STATUS )

*    Perform diagnostic mode outputs
      DO ISRC = 1, LI_NSRC

*      Locate source in SRC data. Set up data for condition handler
	ID = LI_ID(ISRC)
	GE_EXEC_ISRC = ID
	GE_EXEC_NSRC = ISRC

*      Evaluate statistic at position
C        CALL PSS_SRC_POS( S_CP(1,ID), 1, AX_DR(1), AX_DR(2), STATUS )
C        CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0, MPTR, STATUS )

*      Width fitting required?
        IF ( CP_FITWIDTH ) THEN

*        Find width
          CALL PSS_FITWID( ID, ASYMET, STATUS )

*        Re-compute parameter errors

        END IF

*      Find celestial coordinates
        IF ( GE_POINT.OK ) THEN
          CALL CONV_XY2EQU( S_CP(1,ID), S_CP(2,ID),
     :                   (AX_DR(1) .LT. 0.0), GE_POINT.CTOS,
     :				S_RA(ID), S_DEC(ID), STATUS )
        ELSE
          S_RA(ID) = S_RA(ID)*RTOD
          S_DEC(ID) = S_DEC(ID)*RTOD
        END IF

      END DO

*    Sort into RA order
      IF ( GE_POINT.OK ) THEN
        CALL PSS_SRC_SORT( 'RA', .TRUE., STATUS )
      ELSE
        CALL MSG_SETC( 'IFILE', IM_FILE )
        CALL PSS_OP( 'WARNING', 'Insufficient attitude information'/
     :                                               /' in ^IFILE' )
        CALL PSS_OP( 'WARNING', 'No celestial coordinates will be '/
     :                                                 /'produced' )
      END IF

*    Print the list for the last time
      CALL PSS_SRC_DUMP( 3, STATUS )

*    Get name of user's output file
 69   CALL PSS_OUT_OPEN( NFILE, IFILE, SLOC, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        GOTO 94
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      END IF

*    Write number of sources
      CALL SSO_PUTNSRC( SLOC, LI_NSRC, STATUS )

*    Only create lists if sources found!
      IF ( LI_NSRC .GT. 0 ) THEN

*      Image coordinate fields
        CALL PSS_CRERF( SLOC, 'X_CORR', '_REAL', AX_UNITS(1),
     :						   O_XC, STATUS )
        CALL PSS_CRERF( SLOC, 'Y_CORR', '_REAL', AX_UNITS(2),
     :						   O_YC, STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'X_CORR', 'LABEL', 20,
     :			        AX_LABEL(1), STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'Y_CORR', 'LABEL', 20,
     :			        AX_LABEL(2), STATUS )

*      Write celestial coordinates if pointing data there
        IF ( GE_POINT.OK ) THEN
          CALL PSS_CRERF( SLOC, 'RA', '_DOUBLE', 'degrees', O_RA,
     :							 STATUS )
          CALL PSS_CRERF( SLOC, 'DEC', '_DOUBLE', 'degrees', O_DEC,
     :							   STATUS )
        END IF

*      Create FLUX structure
        CALL PSS_CRERF( SLOC, 'FLUX', '_REAL', IM_UNITS,
     :					     O_FLX, STATUS )

*      ENCPSF field
        CALL PSS_CRERF( SLOC, 'ENCPSF', '_REAL', ' ', O_EPSF, STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'ENCPSF', 'LABEL', 30,
     :			        'Enclosed psf fraction', STATUS )

*      Background list if background supplied
        IF ( BG_OK ) THEN
          IF ( IM_UNITS .GT. ' ' ) THEN
            UNITS = IM_UNITS(:CHR_LEN(IM_UNITS))//'/pix'
          ELSE
            UNITS = 'count/pix'
          END IF
          CALL PSS_CRERF( SLOC, 'BACK', '_REAL', UNITS, O_BK, STATUS )
        END IF

*      Fields and errors if doing positional errors
        IF ( NPLEV .GT. 0 ) THEN

*        Positional error field.
          CALL MSG_SETI( 'NLEV', NPLEV )
          CALL MSG_MAKE( '_REAL[^NLEV]', TEXT, TLEN )
          CALL PSS_CRERF( SLOC, 'ERRORS', TEXT(:TLEN), 'arcmin',
     :                                          O_PERR, STATUS )
          CALL SSO_PUTFITEM1D( SLOC, 'ERRORS', 'ELEVS', NPLEV,
     :                                            PLEV, STATUS )

*        Errors on position fields
          IF ( NPLEV .GT. 0 ) THEN
            CALL PSS_CRERFE( SLOC, 'X_CORR', NED, NPLEV, PLEV,
     :                                         O_EXC, STATUS )
            CALL PSS_CRERFE( SLOC, 'Y_CORR', NED, NPLEV, PLEV,
     :                                         O_EYC, STATUS )
          END IF

        END IF

*      Flux error
        IF ( NFLEV .GT. 0 ) THEN
          CALL PSS_CRERFE( SLOC, 'FLUX', NED, NFLEV, FLEV,
     :                                    O_EFLX, STATUS )
        END IF

*      Significance
        CALL PSS_CRERF( SLOC, 'SIGNIF', '_REAL', 'sigma', O_SIG,
     :                                                  STATUS )
        CALL PSS_CRERFE( SLOC, 'SIGNIF', 1, 1, 68.0D0,
     :                                   O_ESIG, STATUS )

*      Extension fields
        IF ( CP_FITWIDTH ) THEN
          CALL PSS_CRERF( SLOC, 'EXTEN', '_REAL', 'arcmin', O_EXT,
     :                                                    STATUS )
          CALL PSS_CRERFE( SLOC, 'EXTEN', NED, 1, 68.0D0,
     :                                   O_EEXT, STATUS )
          CALL PSS_CRERF( SLOC, 'EXSIG', '_REAL', 'sigma', O_EXS,
     :                                                   STATUS )
        END IF

*      Background errors if allowed to rescale
        IF ( CP_RESCALE .AND. BG_OK .AND. (NFLEV.GT.0) ) THEN
          CALL PSS_CRERFE( SLOC, 'BACK', NED, NFLEV, FLEV,
     :                                     O_EBK, STATUS )
        END IF

*      Diagnostic parameters
        IF ( DI_FREE_FIT ) THEN
          CALL PSS_CRERF( SLOC, 'DELTA_C', '_REAL', 'none', O_DC,
     :                                                   STATUS )
        END IF
        IF ( DI_POISS_PROB ) THEN
          CALL PSS_CRERF( SLOC, 'PPROB', '_DOUBLE', 'none', O_PP,
     :                                                   STATUS )
        END IF

*      Error form
        CALL SSO_PUTPAR0L( SLOC, 1, 'SYMMETRIC', (.NOT.ASYMET), STATUS )

      ELSE
        GOTO 89

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Output all the source parameters
      DO ISRC = 1, LI_NSRC

*      Source number
        ID = LI_ID(ISRC)

*      Write image coordinate data
        CALL PSS_SRC_SETR( AXV(1,S_CP(1,ID)), ISRC, O_XC, STATUS )
        CALL PSS_SRC_SETR( AXV(2,S_CP(2,ID)), ISRC, O_YC, STATUS )

*      Pointing data
        IF ( GE_POINT.OK ) THEN
          CALL PSS_SRC_SETD( S_RA(ID), ISRC, O_RA, STATUS )
          CALL PSS_SRC_SETD( S_DEC(ID), ISRC, O_DEC, STATUS )
	END IF

*      Background data
        IF ( BG_OK ) THEN
          CALL PSS_SRC_SETR( S_BACK(ID)*S_BSCALE(ID), ISRC,
     :                                           O_BK, STATUS )
          IF ( CP_RESCALE .AND. (NFLEV.GT.0) ) THEN
            CALL PSS_OSL_MER( NED, NFLEV, S_BERR(1,1,ID), ISRC, O_EBK,
     :                                                        STATUS )
          END IF
        END IF

*      Write source strength parameters
        CALL PSS_SRC_SETR( S_FLUX(ID), ISRC, O_FLX, STATUS )
        IF ( NFLEV .GT. 0 ) THEN
          CALL PSS_OSL_MER( NED, NFLEV, S_FERR(1,1,ID), ISRC, O_EFLX,
     :                                                       STATUS )
        END IF
        CALL PSS_SRC_SETR( S_SIG(ID), ISRC, O_SIG, STATUS )
        CALL PSS_SRC_SETR( S_SIGERR(ID), ISRC, O_ESIG, STATUS )
        CALL PSS_SRC_SETR( S_EPSF(ID), ISRC, O_EPSF, STATUS )

*      Positional errors
        IF ( NPLEV .GT. 0 ) THEN
          CALL ARR_COP1R( NPLEV, S_PERR(1,ID), %VAL(O_PERR+
     :                   (ISRC-1)*NPLEV*VAL__NBR), STATUS )
        END IF

*      Extension tests
        IF ( CP_FITWIDTH ) THEN
          CALL PSS_SRC_SETR( S_EXTENSIG(ID), ISRC, O_EXS, STATUS )
          CALL PSS_SRC_SETR( S_EXTEN(ID), ISRC, O_EXT, STATUS )
          CALL PSS_OSL_MER( NED, 1, S_EXTENERR(1,ID), ISRC, O_EEXT,
     :                                                     STATUS )
        END IF

*      Diagnostics
        IF ( DI_FREE_FIT ) THEN
          CALL PSS_SRC_SETR( S_DSTAT(ID), ISRC, O_DC, STATUS )
        END IF
        IF ( DI_POISS_PROB ) THEN
          CALL PSS_SRC_SETD( S_PPROB(ID), ISRC, O_PP, STATUS )
        END IF

      END DO

*    Release results dataset
 89   CALL PSS_OUT_CLOSE( IFILE, SLOC, STATUS )

*    Write source subtracted image
 94   IF ( CP_EXPERT ) THEN
        CALL PSS_OUT_SSUB( STATUS )
      END IF

*    Last file?
      IF ( IFILE .EQ. NFILE ) THEN
        CALL DYN_UNMAP( MPTR, STATUS )

      ELSE

*      Preserve significance map dimensions for next iteration
        OSIGDIM(1) = SIGDIM(1)
        OSIGDIM(2) = SIGDIM(2)

      END IF

*    Unmap convolution workspace
      IF ( CP_FITWIDTH ) THEN
        CALL DYN_UNMAP( PSF_CONPTR, STATUS )
        CALL DYN_UNMAP( PSF_CONWPTR, STATUS )
      END IF

*    Reset source storage
      CALL PSS_SRC_RESET( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_M_SEARCH', STATUS )
      END IF

      END
*+  PSS_M_SENMAP - Provides PSS SENMAP mode
      SUBROUTINE PSS_M_SENMAP( NFILE, IFILE, STATUS )
*
*    Description :
*
*     Calculates a flux sensitivity map at a user supplied significance
*     level. The significance may be large (>5) in which case a series
*     expansion for the relation between delta-chisquared and significance
*     is used.
*
*    Method :
*
*     IF significance < 5 THEN
*       Find probability from SENSIG
*       Convert to delta chisquared
*     ELSE
*       Can't use above as intermediate probability too small. Instead
*       use result of series expansion
*
*           -sig^2/2 - ln sig = ln 2 - DELCHI/2 - ln DELCHI^0.5
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Original (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IFILE                   ! File being processed
      INTEGER                  NFILE                   ! # input files
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Function definitions :
*
      REAL                     PSS_SIG2CHI
*
*    Local variables :
*
      REAL                     DELSTAT                 ! Delta Cash of SENSIG
      REAL                     SENSIG                  ! Map sensitivity
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise mapping grid
      CALL PSS_GRID_INIT( STATUS )

*    Sensitivity significance
      CALL PAR_GET0R( 'SENSIG', SENSIG, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Define significance and flux convergence to absolute 0.05. Cannot
*    do this for rescaling (due to bgnd), so do relative
      IF ( CP_RESCALE ) THEN
        GE_CONVERGE_METHOD = PSS__RELATIVE
        GE_CONVERGE_TOL = 0.01
      ELSE
        GE_CONVERGE_METHOD = PSS__ABSOLUTE
        GE_CONVERGE_TOL = 0.05
      END IF

*    Convert to a change in Cash statistic
      DELSTAT = PSS_SIG2CHI( SENSIG, STATUS )

*    Set up the psf access
      IF ( PSF_CONSTANT ) THEN
        PSF_ACCESS = PER_FILE
      ELSE
        PSF_ACCESS = PER_PIXEL
      END IF

*    Create sensitivity map
      CALL PSS_MAP_ASSOC( 'MAP', 'Sensitivity map', 'WRITE', .FALSE.,
     :                                                       STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write title
      CALL MSG_SETR( 'SENSIG', SENSIG )
      CALL PSS_MAP_TITLE( 'Flux (counts) sensitivity at '/
     :                          /'^SENSIG sigma', STATUS )

*    Announce start
      CALL MSG_PRNT( 'Constructing sensitivity map...' )

*    Evaluate the flux sensitivity map
      CALL PSS_STAT( %VAL(GR_ROUTINE), 1, DELSTAT, MP_DPTR, STATUS )

*    Close the map
      CALL PSS_MAP_CLOSE( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_M_SENMAP', STATUS )
      END IF

      END
*+  PSS_M_SIGVAR - Provides PSS SIGVAR mode
      SUBROUTINE PSS_M_SIGVAR( NFILE, IFILE, STATUS )
*
*    Description :
*
*     Finds the variance in the Cash significance at every point in the
*     user defined slice.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 92 : Original (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IFILE                   ! File being processed
      INTEGER                  NFILE                   ! # input files
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise mapping grid
      CALL PSS_GRID_INIT( STATUS )

*    Define significance and flux convergence to absolute 0.05. Cannot
*    do this for rescaling (due to bgnd), so do relative
      IF ( CP_RESCALE ) THEN
        GE_CONVERGE_METHOD = PSS__RELATIVE
        GE_CONVERGE_TOL = 0.01
      ELSE
        GE_CONVERGE_METHOD = PSS__ABSOLUTE
        GE_CONVERGE_TOL = 0.05
      END IF

*    Set up the psf access
      IF ( PSF_CONSTANT ) THEN
        PSF_ACCESS = PER_FILE
      ELSE
        PSF_ACCESS = PER_PIXEL
      END IF

*    Create sensitivity map
      CALL PSS_MAP_ASSOC( 'MAP', 'Error in significance', 'WRITE',
     :                                      .FALSE., STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write title
      CALL PSS_MAP_TITLE( 'Error in significance', STATUS )

*    Announce start
      CALL MSG_PRNT( 'Constructing significance error map...' )

*    Evaluate the flux sensitivity map
      CALL PSS_STAT( %VAL(GR_ROUTINE), 1, 0.0, MP_DPTR, STATUS )

*    Close the map
      CALL PSS_MAP_CLOSE( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_M_SIGVAR', STATUS )
      END IF

      END
*+  PSS_M_SPOTS - Implements UPLIM and PARAM modes
      SUBROUTINE PSS_M_SPOTS( NFILE, IFILE, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 89 : Original (DJA)
*     15 Oct 92 : Handles case where flux is supplied for each source (DJA)
*      9 Dec 92 : Replaced %LOC with UTIL_PLOC (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*     15 Jul 93 : Handles extension testing (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'SRCLIB(POI_STR)'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NFILE                   ! Total # files
      INTEGER                  IFILE                   ! File being processed
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      INTEGER                  UTIL_PLOC
      INTEGER                  CHR_LEN
      REAL                     AXV
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SSDS dataset
      CHARACTER*80             TEXT                    !
      CHARACTER*40             UNITS                   ! Background units
      CHARACTER*40             UPSTR                   ! Upper limit string

      DOUBLE PRECISION         CONF                    ! Upper limit confidence
      DOUBLE PRECISION         FLEV(PSS__MXEL)         ! Flux error levels in %

      REAL                     DSTAT                   ! Change in statistic
      REAL                     SFLUX                   ! Stores flux
      REAL                     STAT                    ! Value of statistic

      INTEGER                  ID                      ! A source's id number
      INTEGER                  ILEV                    ! Loop over error levels
      INTEGER                  ISRC                    ! Loop over source list
      INTEGER                  NFLEV                   ! # flux error levels
      INTEGER                  NED                     ! # items per error level
      INTEGER                  O_BK, O_EBK             ! Source bgnd list
      INTEGER                  O_DC                    ! Source delta C
      INTEGER                  O_EPSF                  ! Source enclosed psf
      INTEGER                  O_EXS                   ! Source extension signif
      INTEGER                  O_EXT, O_EEXT           ! Source extension + err
      INTEGER                  O_FLX, O_EFLX           ! Source flux + error
      INTEGER                  O_SIG, O_ESIG           ! Source significances
      INTEGER                  O_XC, O_YC              ! Source X & Y coords
      INTEGER                  O_PP                    ! Poisson probability
      INTEGER                  O_RA                    ! Source image RA
      INTEGER                  O_DEC                   ! Source image DEC

      LOGICAL                  ASYMET                  ! Asymmetric errors?
      LOGICAL                  GOT_FLUX                ! Read flux from list?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      PSF_RESAM = .TRUE.
      GR_PASS = 1

*    Set up the psf access
      IF ( PSF_CONSTANT ) THEN
        PSF_ACCESS = PER_FILE
      ELSE
        PSF_ACCESS = PER_PIXEL
      END IF

*    Get list of positions in spot modes
      IF ( IFILE .EQ. 1 ) THEN
        CALL PSS_POSIN( GOT_FLUX, STATUS )
        IF ( GOT_FLUX ) THEN
          IF ( CP_OPT ) THEN
            CALL MSG_PRNT( 'Flux is present in input lists - will'/
     :                                              /' be frozen' )
          ELSE
            CALL MSG_PRNT( 'Ignoring fluxes present in input list' )
            GOT_FLUX = .FALSE.
          END IF
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get flux confidence levels
      IF ( .NOT. CP_EXPERT ) THEN
        CONF = 68.0D0
        IF ( .NOT. CP_OPT ) THEN
          CALL MSG_PRNT( 'Upper limits at 68% confidence')
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get confidence levels for errors
      IF ( CP_OPT ) THEN

*      Symmetric errors?
        IF ( CP_EXPERT ) THEN
          CALL PAR_GET0L( 'ASYMMETRIC', ASYMET, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
        ELSE
          ASYMET = .FALSE.
        END IF

*      Number of data elements per error level
        IF ( ASYMET ) THEN
          NED = 2
        ELSE
          NED = 1
        END IF

*      90% symmetric errors in IDIOT mode, prompt in EXPERT
        IF ( IFILE .EQ. 1 ) THEN
          IF ( CP_EXPERT ) THEN

*          Flux errors
            CALL PAR_PROMT( 'FERL', 'Flux error confidence level',
     :  						  STATUS )
            CALL PSS_GET_CONF( 'FERL', PSS__MXEL, TEXT, FLEV, NFLEV,
     :                                                      STATUS )

          ELSE

            NFLEV = 1
            FLEV(1) = 68.0D0
            CALL MSG_PRNT( 'Symmetric flux errors at 68% confidence' )

          END IF

        END IF

*    Upper limit mode?
      ELSE IF ( CP_EXPERT .AND. .NOT. CP_OPT ) THEN

        CALL PAR_PROMT( 'FERL', 'Upper limit confidence level', STATUS )
        CALL PSS_GET_CONF( 'FERL', 1, UPSTR, CONF, NFLEV, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Convert confidence to delta-chisquared
      CALL MATH_CHISQD( REAL(1.0D0-CONF/1.0D2), 1, DSTAT, STATUS )

*    Map extra psf space for gaussian convolution
      IF ( CP_FITWIDTH ) THEN
        CALL DYN_MAPR( 2, PSF_DIMS, PSF_CONPTR, STATUS )
        CALL DYN_MAPR( 1, (PSF_DIMS(1)*2+1)**2, PSF_CONWPTR,STATUS )
      END IF

*    Perform last pass on sources
      GE_CONVERGE_METHOD = PSS__RELATIVE
      GE_CONVERGE_TOL = 1.0E-5
      DO ISRC = 1, LI_NSRC

*      Locate source in SRC data. Set up data for condition handler
	ID = LI_ID(ISRC)
	GE_EXEC_ISRC = ID
	GE_EXEC_NSRC = ISRC

*      If this source is bad, pad fields with duff values
        IF ( S_FLAG(ID) ) THEN
          S_CP(1,ID) = -1.0
          S_CP(2,ID) = -1.0
          S_SIG(ID) = -1.0
          S_FLUX(ID) = -1.0
          S_BACK(ID) = -1.0
          DO ILEV = 1, NFLEV
            S_FERR(1,ILEV,ID) = BADERR
            S_FERR(2,ILEV,ID) = BADERR
            S_BERR(1,ILEV,ID) = BADERR
            S_BERR(2,ILEV,ID) = BADERR
          END DO
          GOTO 65
        END IF

*      Set background scale factor if not rescaling
        IF ( .NOT. CP_RESCALE ) S_BSCALE(ID) = 1.0

*      Got flux already?
        IF ( GOT_FLUX ) SFLUX = S_FLUX(ID)

*      Evaluate statistic at position
        CALL PSS_SRC_POS( S_CP(1,ID), 1, AX_DR(1), AX_DR(2), STATUS )
        CALL PSS_STAT( %VAL(GR_ROUTINE), ID, DSTAT, UTIL_PLOC(STAT),
     :                                                      STATUS )

*      Restore flux
        IF ( GOT_FLUX ) S_FLUX(ID) = SFLUX

*      Fitting
        IF ( CP_OPT ) THEN

*        Optimise parameters. Reset status in case of fit failure
          CALL PSS_FIT( ID, GOT_FLUX, ASYMET, NFLEV, FLEV,
     :                                  1, 0.0D0, STATUS )

*        Width fitting required?
          IF ( CP_FITWIDTH ) THEN

*          Find width
            CALL PSS_FITWID( ID, ASYMET, STATUS )

*          Re-compute parameter errors

          END IF

        ELSE

*        Find psf sum
          CALL ARR_SUM1R( DC_HI-DC_LO+1, DC_PSF(DC_LO), S_EPSF(ID),
     :                                                       STATUS )

        END IF

*      Find celestial coordinates
 65     IF ( IFILE .EQ. 1 ) THEN
          S_RA(ID) = S_RA(ID)*RTOD
          S_DEC(ID) = S_DEC(ID)*RTOD
        END IF

      END DO

*    Print the list for the last time
      CALL PSS_SRC_DUMP( 3, STATUS )

*    Get name of user's output file
 69   CALL PSS_OUT_OPEN( NFILE, IFILE, SLOC, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        GOTO 94
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      END IF

*    Write number of sources
      CALL SSO_PUTNSRC( SLOC, LI_NSRC, STATUS )

*    Only create lists if sources found!
      IF ( LI_NSRC .GT. 0 ) THEN

*      Image coordinate fields
        CALL PSS_CRERF( SLOC, 'X_CORR', '_REAL', AX_UNITS(1),
     :						   O_XC, STATUS )
        CALL PSS_CRERF( SLOC, 'Y_CORR', '_REAL', AX_UNITS(2),
     :						   O_YC, STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'X_CORR', 'LABEL', 20,
     :			        AX_LABEL(1), STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'Y_CORR', 'LABEL', 20,
     :			        AX_LABEL(2), STATUS )

*      Write celestial coordinates if pointing data there
        IF ( GE_POINT.OK ) THEN
          CALL PSS_CRERF( SLOC, 'RA', '_DOUBLE', 'degrees', O_RA,
     :							 STATUS )
          CALL PSS_CRERF( SLOC, 'DEC', '_DOUBLE', 'degrees', O_DEC,
     :							   STATUS )
        END IF

*      Create FLUX structure
        CALL PSS_CRERF( SLOC, 'FLUX', '_REAL', IM_UNITS,
     :					     O_FLX, STATUS )

*      ENCPSF field
        CALL PSS_CRERF( SLOC, 'ENCPSF', '_REAL', ' ', O_EPSF, STATUS )
        CALL SSO_PUTFITEM0C( SLOC, 'ENCPSF', 'LABEL', 30,
     :			        'Enclosed psf fraction', STATUS )

*      Background list if background supplied
        IF ( BG_OK ) THEN
          IF ( IM_UNITS .GT. ' ' ) THEN
            UNITS = IM_UNITS(:CHR_LEN(IM_UNITS))//'/pix'
          ELSE
            UNITS = 'count/pix'
          END IF
          CALL PSS_CRERF( SLOC, 'BACK', '_REAL', UNITS, O_BK, STATUS )
        END IF

*      Optimising?
        IF ( CP_OPT .AND. .NOT. GOT_FLUX ) THEN

*        Significance field
          CALL PSS_CRERF( SLOC, 'SIGNIF', '_REAL', 'sigma', O_SIG,
     :                                                    STATUS )

*        Strength errors if wanted
          IF ( NFLEV .GT. 0 ) THEN
            CALL PSS_CRERFE( SLOC, 'FLUX', NED, NFLEV, FLEV,
     :                                      O_EFLX, STATUS )
            CALL PSS_CRERFE( SLOC, 'SIGNIF', 1, 1, 68.0D0,
     :                                    O_ESIG, STATUS )
          END IF

        END IF

*      Extension fields
        IF ( CP_FITWIDTH ) THEN
          CALL PSS_CRERF( SLOC, 'EXTEN', '_REAL', 'arcmin', O_EXT,
     :                                                    STATUS )
          CALL PSS_CRERFE( SLOC, 'EXTEN', NED, 1, 68.0D0,
     :                                   O_EEXT, STATUS )
          CALL PSS_CRERF( SLOC, 'EXSIG', '_REAL', 'sigma', O_EXS,
     :                                                   STATUS )
        END IF

*      Background errors if allowed to rescale
        IF ( CP_RESCALE .AND. BG_OK .AND. CP_OPT
     :                              .AND. (NFLEV.GT.0) ) THEN
          CALL PSS_CRERFE( SLOC, 'BACK', NED, NFLEV, FLEV,
     :                                     O_EBK, STATUS )
        END IF

*      Error form
        CALL SSO_PUTPAR0L( SLOC, 1, 'SYMMETRIC', (.NOT.ASYMET),
     :                                                 STATUS )

*      Diagnostic parameters
        IF ( DI_FREE_FIT ) THEN
          CALL PSS_CRERF( SLOC, 'DELTA_C', '_REAL', 'none', O_DC,
     :                                                   STATUS )
        END IF
        IF ( DI_POISS_PROB ) THEN
          CALL PSS_CRERF( SLOC, 'PPROB', '_DOUBLE', 'none', O_PP,
     :                                                   STATUS )
        END IF

      ELSE
        GOTO 89

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Output all the source parameters
      DO ISRC = 1, LI_NSRC

*      Source number
        ID = LI_ID(ISRC)

*      Write image coordinate data
        CALL PSS_SRC_SETR( AXV(1,S_CP(1,ID)), ISRC, O_XC, STATUS )
        CALL PSS_SRC_SETR( AXV(2,S_CP(2,ID)), ISRC, O_YC, STATUS )

*      Pointing data
        IF ( GE_POINT.OK ) THEN
          CALL PSS_SRC_SETD( S_RA(ID), ISRC, O_RA, STATUS )
          CALL PSS_SRC_SETD( S_DEC(ID), ISRC, O_DEC, STATUS )
	END IF

*      Background data
        IF ( BG_OK ) THEN
          CALL PSS_SRC_SETR( S_BACK(ID)*S_BSCALE(ID), ISRC, O_BK,
     :                                                       STATUS )
          IF ( CP_RESCALE .AND. CP_OPT .AND. (NFLEV.GT.0) ) THEN
            CALL PSS_OSL_MER( NED, NFLEV, S_BERR(1,1,ID), ISRC, O_EBK,
     :                                                        STATUS )
          END IF
        END IF

*      Write source strength parameters
        CALL PSS_SRC_SETR( S_FLUX(ID), ISRC, O_FLX, STATUS )
        IF ( CP_OPT .AND. .NOT. GOT_FLUX ) THEN
          CALL PSS_SRC_SETR( S_SIG(ID), ISRC, O_SIG, STATUS )
          IF ( NFLEV .GT. 0 ) THEN
            CALL PSS_OSL_MER( NED, NFLEV, S_FERR(1,1,ID), ISRC, O_EFLX,
     :                                                         STATUS )
            CALL PSS_SRC_SETR( S_SIGERR(ID), ISRC, O_ESIG, STATUS )
          END IF
        END IF
        CALL PSS_SRC_SETR( S_EPSF(ID), ISRC, O_EPSF, STATUS )

*      Extension tests
        IF ( CP_FITWIDTH ) THEN
          CALL PSS_SRC_SETR( S_EXTENSIG(ID), ISRC, O_EXS, STATUS )
          CALL PSS_SRC_SETR( S_EXTEN(ID), ISRC, O_EXT, STATUS )
          CALL PSS_OSL_MER( NED, 1, S_EXTENERR(1,ID), ISRC, O_EEXT,
     :                                                     STATUS )
        END IF

*      Diagnostics
        IF ( DI_FREE_FIT ) THEN
          CALL PSS_SRC_SETR( S_DSTAT(ID), ISRC, O_DC, STATUS )
        END IF
        IF ( DI_POISS_PROB ) THEN
          CALL PSS_SRC_SETD( S_PPROB(ID), ISRC, O_PP, STATUS )
        END IF

      END DO

*    Unmap convolution workspace
      IF ( CP_FITWIDTH ) THEN
        CALL DYN_UNMAP( PSF_CONPTR, STATUS )
        CALL DYN_UNMAP( PSF_CONWPTR, STATUS )
      END IF

*    Close output
 89   CALL PSS_OUT_CLOSE( IFILE, SLOC, STATUS )

*    Write source subtracted image
 94   IF ( CP_EXPERT ) THEN
        CALL PSS_OUT_SSUB( STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_M_SPOTS', STATUS )
      END IF

      END
*+  PSS_M_UPMAP - Perform upper limit mapping mode
      SUBROUTINE PSS_M_UPMAP( NFILE, IFILE, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Jun 92 : Original. Adapted from PSS_2D (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IFILE                   ! File being processed
      INTEGER                  NFILE                   ! # input files
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      CHARACTER*40             STR                     ! User response to FERL

      DOUBLE PRECISION         CONF                    ! Confidence level

      REAL                     DELSTAT                 ! Change in stat for map

      INTEGER                  IDUMMY                  ! A dummy value
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise grid
      CALL PSS_GRID_INIT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set up the psf access
      IF ( PSF_CONSTANT ) THEN
        PSF_ACCESS = PER_FILE
      ELSE
        PSF_ACCESS = PER_PIXEL
      END IF

*    Get confidence level for upper limit map
      IF ( CP_EXPERT ) THEN
        CALL PAR_PROMT( 'FERL', 'Confidence level for upper'/
     :                                /' limit map', STATUS )
        CALL PSS_GET_CONF( 'FERL', 1, STR, CONF, IDUMMY, STATUS )
      ELSE
        STR = '68%'
        CONF = 68.0D0
      END IF

*    Define significance and flux convergence to absolute 0.05. Cannot
*    do this for rescaling (due to bgnd), so do relative
      IF ( CP_RESCALE ) THEN
        GE_CONVERGE_METHOD = PSS__RELATIVE
        GE_CONVERGE_TOL = 0.01
      ELSE
        GE_CONVERGE_METHOD = PSS__ABSOLUTE
        GE_CONVERGE_TOL = 0.05
      END IF

*    Open map
      CALL PSS_MAP_ASSOC( 'MAP', 'Upper limit map', 'WRITE', .FALSE.,
     :                                                       STATUS )

*    Write title
      CALL MSG_SETC( 'STR', STR )
      CALL PSS_MAP_TITLE( '^STR confidence upper limit map ', STATUS )

*    Write map units
      CALL BDA_PUTUNITS_INT( MP_BDA, IM_UNITS, STATUS )

*    Convert confidence to delta-chisquared for upper limits map
      CALL MATH_CHISQD( REAL(1.0D0-CONF/100.D0), 1, DELSTAT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Announce grid spacing
      CALL MSG_PRNT( 'Constructing upper limit map' )

*    Evaluate the upper limit map
      CALL PSS_STAT( %VAL(GR_ROUTINE), 1, DELSTAT, MP_DPTR, STATUS )

*    Close the map
      CALL PSS_MAP_CLOSE( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_M_UPMAP', STATUS )
      END IF

      END
