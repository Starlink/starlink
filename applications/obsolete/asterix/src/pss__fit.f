*+  PSS_FIT - Do fitting job to get parameter errors for a given source
      SUBROUTINE PSS_FIT( ID, FRFLUX, ASYMM, NFLEV, FLEV, NPLEV,
     :                                            PLEV, STATUS )
*
*    Description :
*
*     Gets CP_UPLIM.CONF confidence flux errors, and then find positional
*     errors at each of the requested confidence levels.
*
*     Minimises the value of
*
*      Statistic at optimum flux - Statistic at zero flux
*
*     ie. the more negative this value, the more significant the detection
*
*    Deficiencies :
*
*     Setting of STATUS by FIT_ routines does not use ERR_ hence direct
*     manipulation of status values in this code.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Oct 90 : Original (DJA)
*     17 Mar 92 : Tests whether source has moved too far (DJA)
*     29 May 92 : Use FIT_ code rather than LU fitting code (DJA)
*      6 Aug 92 : Finds sum of enclosed psf (DJA)
*      6 Sep 92 : Trap FITERR from FIT_ routines (DJA)
*     16 Oct 92 : FRFLUX lets flux be frozen externally (DJA)
*     13 Jan 93 : New PARCON facility used (DJA)
*     30 Apr 93 : Permits NFLEV and NPLEV to be -ve, signifiying that those
*                 errors are not to be found (DJA)
*     28 Jun 93 : Call FIT_PARCON iteratively to get error (DJA)
*      6 Jul 93 : Added Cash & flux versus X,Y diagnostics (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*     22 Jul 93 : Added Poisson probability calculation (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PSS_PAR'
      INCLUDE 'USER_ERR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
      INCLUDE 'PSS_FIT_CMN'
*
*    Import :
*
      INTEGER                  ID                      ! Box to search
      LOGICAL                  FRFLUX                  ! Freeze flux?
      LOGICAL                  ASYMM                   ! Asymmetrical erros
      INTEGER                  NFLEV                   ! # pos error levels
      DOUBLE PRECISION         FLEV(*)                 ! Flux error levels in %
      INTEGER                  NPLEV                   ! # pos error levels
      DOUBLE PRECISION         PLEV(*)                 ! Pos error levels in %
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      REAL                     PSS_CASH_SIG
*
*    Local constants :
*
      REAL                     EPS_FITERR              ! Convergence for errors
        PARAMETER              ( EPS_FITERR = 0.01 )

      INTEGER                  MAXEITER                ! Max no. error iter'ns
        PARAMETER              ( MAXEITER = 5 )
*
*    Local variables :
*
      RECORD /MODEL_SPEC/      MODEL			! Dummy model record

      REAL                     DPAR(PSS__FITNPAR)      ! Parameter differences
      DOUBLE PRECISION         LASTMIN                 ! Best fit value
      REAL                     PARAM(PSS__FITNPAR)     ! Parameter initial vals
      REAL                     PERR(PSS__FITNPAR)      ! Parameter errors
      REAL                     PGOOD(PSS__FITNPAR)     ! Best fit parameters
      REAL                     PLO(PSS__FITNPAR),      ! Confidence levels
     :                         PHI(PSS__FITNPAR)       !
      REAL                     LB(PSS__FITNPAR),       ! Parameter bounds
     :                         UB(PSS__FITNPAR)        !
      REAL                     LPLO, LPHI              ! Last error guesses
      REAL                     MINSLO                  ! Chisq slope
      REAL                     SOFF                    ! Offset in statistic
      DOUBLE PRECISION         STAT0                   ! Fit at zero flux
      DOUBLE PRECISION         STATMIN                 ! Best fit value
      REAL                     XERR, YERR              ! X,Y pos errors

      INTEGER                  CP                      ! Loop over cache
      INTEGER                  EITER                   ! # error iterations
      INTEGER                  FITERR                  ! Fitting error code
      INTEGER                  FVEC(2)                 ! Psf offset vector
      INTEGER                  IDAT                    ! Loop over error data
      INTEGER                  ILEV                    ! Loop over error levels
      INTEGER                  IP                      ! Loop over parameters
      INTEGER                  ISTAT                   ! Fit statistic
      INTEGER                  ITER                    ! # iterations
      INTEGER                  PSCALE                  ! Statistic scale factor
      INTEGER                  NIT                     ! # fitting iterations
      INTEGER                  PEGCODE(PSS__FITNPAR)   ! Error peg code
      INTEGER                  PPARS(2)                ! Position par nos.

      LOGICAL                  CONVERGED               ! Errors converged yet?
      LOGICAL                  FINISHED                ! Minimum found
      LOGICAL                  FROZEN(PSS__FITNPAR)    ! Parameter frozen?
      LOGICAL                  GOOD_FIT                ! Successful FIT_ run?
      LOGICAL                  INITIALISE              ! Initialise fitting
      LOGICAL                  PEGGED(PSS__FITNPAR)    ! Parameter pegged?
*
*    External references :
*
      EXTERNAL                 PSS_FIT_GENMODEL
*
*    Local data :
*
      DATA                     FVEC/0.0,0.0/           ! Psf offset vector
      DATA                     PPARS/P__X,P__Y/	       ! Positional parameters
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Reset errors
      DO ILEV = 1, PSS__MXEL
        DO IDAT = 1, 2
          S_FERR(IDAT,ILEV,ID) = BADERR
          S_XERR(IDAT,ILEV,ID) = BADERR
          S_YERR(IDAT,ILEV,ID) = BADERR
          S_BERR(IDAT,ILEV,ID) = BADERR
        END DO
        S_PERR(ILEV,ID) = BADERR
      END DO
      S_SIGERR(ID) = BADERR

*    No good parameters yet
      GOOD_FIT = .FALSE.

*    Set up a dummy MODEL structure
      MODEL.NTIE = 0

*    Restart and reset iteration counter
      ITER = 0
      MINSLO = PSS__FITMINSLO

*    Restart point for re-fit
 10   ITER = ITER + 1
      LASTMIN = STATMIN
      IF ( ITER .GT. 8 ) THEN
        CALL MSG_SETI( 'N', GE_EXEC_NSRC )
        CALL MSG_PRNT( 'Too many iterations fitting source '/
     :                          /'parameters for source ^N' )
        GOTO 89
      END IF
      INITIALISE = .TRUE.

*    Access psf for this box if needed
      IF ( .NOT. ( PSF_CONSTANT .OR. CP_USECON ) ) THEN
        CALL PSS_PSF_SUBSET( S_CP(1,ID), FVEC, %VAL(PSF_STORE),STATUS )
      END IF

*    First iteration?
      IF ( ITER .EQ. 1 ) THEN

*      Set up fit parameters
        CALL PSS_FIT_DLOAD( ID, PARAM, LB, UB, FROZEN, PSCALE, ISTAT,
     :                                                       STATUS )

*      Store parameter set just in case
        CALL PSS_FIT_P2S( PARAM, PGOOD, ID, STATUS )

*      Produce Stat(x,y) image?
        IF ( DI_CHI_V_XY ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, 0, 'xy', ' ', STATUS )
        END IF

*      Produce Flux(x,y) image?
        IF ( DI_FLUX_V_XY ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, P__F, 'xy', ' ', STATUS )
        END IF

*      Produce Stat(x) profile?
        IF ( DI_CHI_V_X ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, 0, 'x', 'y', STATUS )
        END IF

*      Produce Flux(x) profile?
        IF ( DI_FLUX_V_X ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, P__F, 'x', 'y', STATUS )
        END IF

*      Produce p(f) profile?
        IF ( DI_P_S_PROFILE ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, -1, 'f', 'xy', STATUS )
        END IF

*      Produce p(x,y,s) cube?
        IF ( DI_P_XYS_CUBE ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, -1, 'xyf', ' ', STATUS )
        END IF

*      Produce Stat(f) profile?
        IF ( DI_CHI_V_FLUX ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, 0, 'f', 'xy', STATUS )
        END IF

*      Produce Stat(f,b) image?
        IF ( DI_CHI_V_F_B ) THEN
          CALL PSS_FIT_GRID( PARAM, MODEL, LB, UB, FROZEN,
     :                ISTAT, PSCALE, 0, 'fb', 'xy', STATUS )
        END IF

*      Freeze flux?
        IF ( FRFLUX ) FROZEN(P__F) = .TRUE.

      END IF

*    Find optimum fit
      CALL FIT_MIN( 1, FIT.DS, 0, MODEL, 0, 2*PSS__FITMXIT,
     :              PSS__FITNPAR,
     :        LB, UB, FROZEN, PSCALE, INITIALISE, MINSLO, ISTAT,
     :        PSS_FIT_GENMODEL, FIT.PRED, PARAM, DPAR, PEGGED, STATMIN,
     :        NIT, FINISHED, FITERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__OK
        GOTO 89
      ELSE IF ( FITERR .NE. 0 ) THEN
        CALL MSG_SETI( 'N', GE_EXEC_NSRC )
        CALL MSG_PRNT( 'Error fitting source parameters for source ^N' )
        GOTO 89
      ELSE IF ( .NOT. FINISHED ) THEN
        IF ( ABS((STATMIN-LASTMIN)) .LT. ABS(STATMIN*1.0E-5) ) THEN
          MINSLO = MINSLO * 10.0
        END IF
        GOTO 10
      END IF

*    Store best parameters
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL PSS_FIT_P2S( PARAM, PGOOD, ID, STATUS )
        GOOD_FIT = .TRUE.
      END IF

*    Frozen the flux?
      IF ( FRFLUX ) GOTO 89

*    First guess at errors by inverting matrix
      CALL FIT_PARERR( 1, FIT.DS, 0, MODEL, PSS__FITNPAR, PARAM, LB, UB,
     :        DPAR, FROZEN, PEGGED, PSCALE, ISTAT, PSS_FIT_GENMODEL,
     :               FIT.PRED, PERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL MSG_SETI( 'N', GE_EXEC_NSRC )
        CALL MSG_PRNT( 'Error inverting derivatives matrix for'/
     :                                           /' source ^N' )
        GOTO 89
      END IF

      DO IP = 1, PSS__FITNPAR
        PLO(IP) = PERR(IP)
        PHI(IP) = PERR(IP)
      END DO

*    For each flux error level
      DO ILEV = 1, NFLEV

*      Flux errors
        CALL MATH_CHISQD(1.0-REAL(FLEV(ILEV)/100.0D0),1,SOFF,STATUS )

*      Find confidence intervals
        EITER = 0
        CONVERGED = .FALSE.
        DO WHILE ( (EITER .LT. MAXEITER) .AND. .NOT. CONVERGED )

*        Store old values
          LPLO = PLO(P__F)
          LPHI = PHI(P__F)

*        Next guess at flux errors
          CALL FIT_PARCON( 1, FIT.DS, 0, MODEL, SOFF, 0, 1, P__F,
     :         PSS__FITMXIT, MINSLO, PSS__FITNPAR, LB, UB,
     :         FROZEN, PSCALE, DPAR,
     :         ISTAT, PSS_FIT_GENMODEL, FIT.PRED, STATMIN, PARAM,
     :         PEGGED, PLO, PHI, PEGCODE, STATUS )

*        Fit error?
          IF ( STATUS .EQ. USER__001 ) THEN
            STATUS = SAI__OK
            CALL PSS_FIT_P2S( PARAM, PGOOD, ID, STATUS )
            GOOD_FIT = .TRUE.
            ITER = MAX( 1, ITER-5 )
            GOTO 10
          ELSE IF ( FITERR .NE. 0 ) THEN
            CALL MSG_SETI( 'N', GE_EXEC_NSRC )
            CALL MSG_PRNT( 'Error fitting flux error for source ^N' )
            GOTO 89

*        Converged yet?
          ELSE
            EITER = EITER + 1
            IF ( (ABS((PLO(P__F)-LPLO)).LT.(LPLO*EPS_FITERR)) .AND.
     :           (ABS((PHI(P__F)-LPHI)).LT.(LPHI*EPS_FITERR)) ) THEN
              CONVERGED = .TRUE.
            END IF

          END IF

        END DO

*      Store flux error
        CALL PSS_FIT_ERR( PARAM(1), PLO(1), PHI(1), ASYMM,
     :                                 S_FERR(1,ILEV,ID) )

*      Background error if present
        IF ( CP_RESCALE ) THEN

*        Finds error in rescale factor
          CALL PSS_FIT_ERR( PARAM(4), PLO(4), PHI(4), ASYMM,
     :                                   S_BERR(1,ILEV,ID) )

*        Convert to background units
          IF ( S_BERR(1,ILEV,ID) .NE. BADERR ) THEN
            S_BERR(1,ILEV,ID) = S_BERR(1,ILEV,ID)*S_BACK(ID)
          END IF
          IF ( ASYMM .AND. (S_BERR(1,ILEV,ID).NE.BADERR) ) THEN
            S_BERR(2,ILEV,ID) = S_BERR(2,ILEV,ID)*S_BACK(ID)
          END IF

        END IF

      END DO
      IF ( CP_SPOT ) GOTO 89

*    Do positional errors
 40   STATUS = SAI__OK
      DO ILEV = 1, NPLEV

*      Get change in statistic required
        CALL MATH_CHISQD(1.0-REAL(PLEV(ILEV)/100.0d0),1,SOFF,STATUS)

*      Find confidence intervals
        EITER = 0
        CONVERGED = .FALSE.
        DO WHILE ( (EITER .LT. MAXEITER) .AND. .NOT. CONVERGED )

*        Store old values
          LPLO = PLO(P__X)
          LPHI = PHI(P__X)

*        Next guess at confidence intervals
          CALL FIT_PARCON( 1, FIT.DS, 0, MODEL, SOFF, 0, 2, PPARS,
     :             PSS__FITMXIT, MINSLO, PSS__FITNPAR, LB, UB,
     :            FROZEN, PSCALE, DPAR, ISTAT, PSS_FIT_GENMODEL,
     :     FIT.PRED, STATMIN, PARAM, PEGGED, PLO, PHI, PEGCODE, STATUS )

          IF ( STATUS .EQ. USER__001 ) THEN
            STATUS = SAI__OK
            CALL PSS_FIT_P2S( PARAM, PGOOD, ID, STATUS )
            GOOD_FIT = .TRUE.
            ITER = MAX( 1, ITER-5 )
            GOTO 10

          ELSE IF ( FITERR .NE. 0 ) THEN
            CALL MSG_SETI( 'N', GE_EXEC_NSRC )
            CALL MSG_PRNT( 'Error fitting positional error for'/
     :                                         /' source ^N' )
            GOTO 89

*        Converged yet?
          ELSE
            EITER = EITER + 1
            IF ( (ABS((PLO(P__X)-LPLO)).LT.(LPLO*EPS_FITERR)) .AND.
     :           (ABS((PHI(P__X)-LPHI)).LT.(LPHI*EPS_FITERR)) ) THEN
              CONVERGED = .TRUE.
            END IF

          END IF

        END DO

        IF ( STATUS .EQ. SAI__OK ) THEN

*        Find X and Y errors
          CALL PSS_FIT_ERR( PARAM(2), PLO(2), PHI(2), ASYMM,
     :                                   S_XERR(1,ILEV,ID) )
          CALL PSS_FIT_ERR( PARAM(3), PLO(3), PHI(3), ASYMM,
     :                                   S_YERR(1,ILEV,ID) )

*        Asymmetric errors?
          IF ( ASYMM ) THEN
            XERR = (S_XERR(1,ILEV,ID) + S_XERR(2,ILEV,ID))/2.0
            YERR = (S_YERR(1,ILEV,ID) + S_YERR(2,ILEV,ID))/2.0
            S_PERR(ILEV,ID) = SQRT(XERR**2+YERR**2)*MATH__DRTOD*60.0

*        Average the errors
          ELSE
            S_PERR(ILEV,ID) = SQRT(S_XERR(1,ILEV,ID)**2 +
     :                     S_YERR(1,ILEV,ID)**2)*MATH__DRTOD*60.0
          END IF

        END IF
      END DO

*    Recall best parameters
 89   DO IP = 1, PSS__FITNPAR
        PARAM(IP) = PGOOD(IP)
      END DO

*    Evaluate Cash statistic at zero flux
      PARAM(P__F) = 0.0
      CALL FIT_STAT( 1, FIT.DS, 0, 0, PARAM, ISTAT,
     :                 PSS_FIT_GENMODEL, FIT.PRED, STAT0, STATUS )

*    To get significance
      S_DSTAT(ID) = REAL(STAT0-STATMIN)
      S_SIG(ID) = PSS_CASH_SIG( S_DSTAT(ID) )

*    Evaluate model once more
      CALL PSS_FIT_GENMODEL( ISTAT, 1, FIT.DS, 0, FIT.PRED, 0,
     :                              PGOOD, 1, DC_MOD, STATUS )

*    Find error in significance
      CALL PSS_STAT_CASH_SIGERR( S_FLUX(ID), S_BSCALE(ID),
     :                              S_SIGERR(ID), STATUS )

*    Get Poisson probability of excess source counts
      IF ( DI_POISS_PROB ) THEN
        CALL PSS_FIT_PPROB( S_PPROB(ID), STATUS )
      END IF

*    Get psf sum
      S_EPSF(ID) = 0.0
      DO CP = DC_LO, DC_HI
        S_EPSF(ID) = S_EPSF(ID) + DC_PSF(CP)
      END DO

*    Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN

*      Errors for frozen flux in spot mode can be ignored
        IF ( FRFLUX .AND. CP_SPOT ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          CALL ERR_FLUSH( STATUS )
        END IF

      END IF

      END

*+  PSS_FITWID - Fit gaussian extension
      SUBROUTINE PSS_FITWID( ID, ASYMM, STATUS )
*
*    Description :
*
*     Finds optimum width convolution of the instrument psf with a gaussian.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     25 Feb 91 : Original (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
*     17 Nov 93 : Diagnostic output straight to HDS (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  ID                      ! Input box id
      LOGICAL                  ASYMM                   ! Asymmetric errors
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL			AXV
      REAL                     PSS_CASH_SIG
      INTEGER                  UTIL_PLOC
*
*    Local constants :
*
      INTEGER                  MAXWID                  ! Max width trials
         PARAMETER             ( MAXWID = 22 )
*
*    Local variables :
*
      CHARACTER*12             FNAME                   ! Diagnostic mode o/p
      CHARACTER 		RASTR*9, DECSTR*10	! Position formatted

      DOUBLE PRECISION		CELPOS(2)	       	! Source position

      REAL			APOS(2)
      REAL                      ELO, EHI                ! Width error interval
      REAL                      EWID                    ! Error width
      REAL                      T1, T2                  ! Use to centroid
      REAL                      TRW(5), TRS(5)          ! Centroid triplet
      REAL                      TSIG(0:MAXWID)          ! Test significances
      REAL                      TSTAT(0:MAXWID)         ! Test statistic values

      INTEGER		        APTR, DPTR, QPTR	! Diag o/p data pointers
      INTEGER                   B                       ! Best width index
      INTEGER                   EB                      ! Error width index
      INTEGER                   I                       ! Loop over parameters
      INTEGER                   K                       ! Convergence loop
      INTEGER			OBOX			! Output src id
      INTEGER		       	SID		       	! Diag o/p identifier
*
*    Local data :
*
      REAL                     TWID(0:MAXWID)            ! Test widths
        DATA                   TWID/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,
     :                              0.8,0.9,1.0,1.2,1.4,1.6,1.8,2.0,
     :                              2.4,2.8,3.2,4.0,5.0,6.0,8.0/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Make copy of OBOX
      CALL PSS_SRC_GRAB_INT( OBOX, STATUS )
      CALL PSS_SRC_COPY( ID, OBOX, STATUS )

*    Define grid of one test point
      CALL PSS_SRC_POS( S_CP(1,ID), 1, AX_DR(1), AX_DR(2), STATUS )

*    Tell STAT routines to use convolved psf
      CP_USECON = .TRUE.
      GR_PASS = 2

*    Cache integrity gone if we've done fitting between a call to PSS_STAT
*    and this point.
      DC_VOLATILE = .TRUE.

*    Find RA and DEC for title
      APOS(1) = AXV( 1, S_CP(1,ID) )
      APOS(2) = AXV( 2, S_CP(2,ID) )
      CALL WCI_CNA2S( APOS, GE_PIXID, GE_PRJID, CELPOS, STATUS )
      CALL STR_DRADTOC( CELPOS(1), 'HHhMMmSSs', RASTR, STATUS )
      CALL STR_DRADTOC( CELPOS(2), 'SDDhMMmSSs', DECSTR, STATUS )

*    Diagnostic mode?
      IF ( DI_SIG_V_CRAD ) THEN

*      Create significance vs. convolution file
        WRITE( FNAME, '(A,I4.4,A)' ) 'svc_', GE_EXEC_NSRC, '%hds'
        CALL ADI_FCREAT( FNAME, 'BinDS', SID, STATUS )

*      Write title containing source position
        CALL BDI_PUTTITLE( SID, 'RA : '//RASTR//', DEC : '/
     :                         /DECSTR, STATUS )

*      Create data, quality and axis
        CALL BDI_CREDATA( SID, 1, MAXWID+1, STATUS )
        CALL BDI_CREQUAL( SID, 1, MAXWID+1, STATUS )
        CALL BDI_PUTMASK( SID, QUAL_MASK, STATUS )
        CALL BDI_CREAXES( SID, 1, STATUS )
        CALL BDI_CREAXVAL( SID, 1, .FALSE., MAXWID+1, STATUS )

*      Map the 3 arrays
        CALL BDI_MAPDATA( SID, 'WRITE', DPTR, STATUS )
        CALL BDI_MAPQUAL( SID, 'WRITE', QPTR, STATUS )
        CALL BDI_MAPAXVAL( SID, 'WRITE', 1, APTR, STATUS )

*      Axis attributes
        CALL BDI_PUTAXLABEL( SID, 1, 'Gaussian convolution', STATUS )
        CALL BDI_PUTAXUNITS( SID, 1, 'arcmin', STATUS )
        CALL BDI_PUTLABEL( SID, 'Significance', STATUS )
        CALL BDI_PUTUNITS( SID, 'sigma', STATUS )

      END IF

*    Find significance at test widths
      DO I = 0, MAXWID

*      Convolve psf
        CALL PSS_PSF_CONVOLVE( PSF_UDIMS(1), PSF_UDIMS(2),
     :                 %VAL(PSF_STORE), %VAL(PSF_CONWPTR),
     :                  TWID(I), %VAL(PSF_CONPTR), STATUS )

*      Get significance
        CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0, UTIL_PLOC(TSIG(I)),
     :                                                       STATUS )
        TSTAT(I) = S_DSTAT(ID)

*      Write to file if needed
        IF ( DI_SIG_V_CRAD ) THEN
          CALL ARR_COP1R( 1, TWID(I), %VAL(APTR+I*VAL__NBR), STATUS )
          CALL ARR_COP1R( 1, TSIG(I), %VAL(DPTR+I*VAL__NBR), STATUS )
          CALL ARR_COP1B( 1, QUAL_GOOD, %VAL(QPTR+I*VAL__NBUB), STATUS )
        END IF

      END DO

*    Close diagnostic file
      IF ( DI_SIG_V_CRAD ) THEN
        CALL BDI_RELEASE( SID, STATUS )
        CALL ADI_FCLOSE( SID, STATUS )
      END IF

*    Identify the best width
      B = 0
      DO I = 1, MAXWID
        IF ( TSIG(I) .GT. TSIG(B) ) THEN
          B = I
        END IF
      END DO

*    Is optimum extension zero
      IF ( B .EQ. 0 ) THEN

*      Store extension
        S_EXTEN(OBOX) = 0.0
        S_EXTENSIG(OBOX) = 0.0

*      Rough guess at where statistic has decreased by 1.0
        EB = B
        DO WHILE ( (TSTAT(B)-TSTAT(EB)) .LT. 1.0 )
          EB = EB + 1
        END DO
        EWID = TWID(EB-1) + (TWID(EB)-TWID(EB-1))*
     :            (TSTAT(B)-1.0-TSTAT(EB-1))/(TSTAT(EB)-TSTAT(EB-1))

*      Convolve at that width
        CALL PSS_PSF_CONVOLVE( PSF_UDIMS(1), PSF_UDIMS(2),
     :                 %VAL(PSF_STORE), %VAL(PSF_CONWPTR),
     :                     EWID, %VAL(PSF_CONPTR), STATUS )

*      Evaluate statistic
        CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0,
     :            UTIL_PLOC(TSIG(MAXWID)), STATUS )

*      The value of S_DSTAT(ID) should now be roughly TSTAT(B)-1.0. Use
*      quadfit to get exact error bound
        CALL PSS_QUADFIT( TWID(B), TSTAT(B), EWID, S_DSTAT(ID),
     :                           TSTAT(B)-1.0, ELO, EHI, STATUS )

*      Store error
        IF ( ASYMM ) THEN
          S_EXTENERR(1,OBOX) = -1.0
          S_EXTENERR(2,OBOX) = EHI - TWID(B)
        ELSE
          S_EXTENERR(1,OBOX) = EHI - TWID(B)
        END IF

*    Out of range of test widths
      ELSE IF ( B .EQ. MAXWID ) THEN

        S_EXTEN(OBOX) = -1.0
        S_EXTENERR(1,OBOX) = -1.0
        S_EXTENERR(2,OBOX) = -1.0
        S_EXTENSIG(OBOX) = -1.0
        CALL MSG_SETI( 'N', GE_EXEC_NSRC )
        CALL PSS_OP( 'INFO', 'Source ^N extension too large' )

      ELSE

*      Estimate where statistic decreases by 1.0. Prefer to go to smaller
*      widths as more resolution that way
        EB = B
        IF ( (TSTAT(B)-TSTAT(0)) .GT. 1.0 ) THEN
          DO WHILE ( (TSTAT(B)-TSTAT(EB)) .LT. 1.0 )
            EB = EB - 1
          END DO
          EWID = TWID(EB+1) + (TWID(EB)-TWID(EB+1))*
     :            (TSTAT(B)-1.0-TSTAT(EB+1))/(TSTAT(EB)-TSTAT(EB+1))
        ELSE IF ( (TSTAT(B)-TSTAT(MAXWID)) .GT. 1.0 ) THEN
          DO WHILE ( (TSTAT(B)-TSTAT(EB)) .LT. 1.0 )
            EB = EB + 1
          END DO
          EWID = TWID(EB-1) + (TWID(EB)-TWID(EB-1))*
     :            (TSTAT(B)-1.0-TSTAT(EB-1))/(TSTAT(EB)-TSTAT(EB-1))
        ELSE
          EWID = 1.0
        END IF

*      Find triplet of widths and statistic values. Iterate to improve best
*      convolution
        TRW(3) = TWID(B)
        DO K = 1, 3

*        Fill values in triplet
          T1 = 0.0
          T2 = 0.0
          DO I = 1, 5
            TRW(I) = TRW(3)+REAL(I-3)*EWID/2.0
            CALL PSS_PSF_CONVOLVE( PSF_UDIMS(1), PSF_UDIMS(2),
     :                     %VAL(PSF_STORE), %VAL(PSF_CONWPTR),
     :                       TRW(I), %VAL(PSF_CONPTR), STATUS )
            CALL PSS_STAT( %VAL(GR_ROUTINE), ID, 0.0,
     :                     UTIL_PLOC(TSIG(MAXWID)), STATUS )
            TRS(I) = S_DSTAT(ID)
            T1 = T1 + TRW(I)*TRS(I)
            T2 = T2 + TRS(I)
          END DO

*        Centroid to get new optimum. Update estimate of width
          TRW(3) = T1/T2

        END DO

*      Set measures of extension
        S_EXTEN(OBOX) = TRW(3)
        S_EXTENERR(1,OBOX) = (TRW(3)-TRW(1)) / (TRS(3)-TRS(1))
        S_EXTENERR(2,OBOX) = (TRW(5)-TRW(3)) / (TRS(3)-TRS(5))

*      The geometric mean is a better estimate of the error, as the
*      lower and upper errors tend to be quite asymmetric.
        IF ( .NOT. ASYMM ) THEN
          S_EXTENERR(1,OBOX) = SQRT( S_EXTENERR(1,OBOX)**2+
     :                               S_EXTENERR(2,OBOX)**2)
        END IF

*      Significance of extension compared to zero
        S_EXTENSIG(OBOX) = PSS_CASH_SIG( TRS(3) - TSTAT(0) )

      END IF

*    Restore original source parameters
 99   CALL PSS_SRC_COPY( OBOX, ID, STATUS )
      CALL PSS_SRC_FREE_INT( OBOX, STATUS )

      CP_USECON = .FALSE.
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_FITWID', STATUS )
      END IF

      END

*+  PSS_FIT_CACHE - Produce predicted data given model parameters
      SUBROUTINE PSS_FIT_CACHE( NX, NY, IMD, IMV, IMQ, BGND, BGNDV,
     :                                                     STATUS )
*
*    Description :
*
*     Constructs a model given fit parameters.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'FIT_STRUC'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                 NX, NY                        ! Dimensions
      REAL                    IMD(NX,NY)                    ! Image data
      REAL                    IMV(NX,NY)                    ! Image variance
      BYTE                    IMQ(NX,NY)                    ! Image quality
      REAL                    BGND(NX,NY)                   ! Bgnd data
      REAL                    BGNDV(NX,NY)                  ! Bgnd variance
*
*    Status :
*
      INTEGER                 STATUS
*
*    Local variables :
*
      INTEGER                 II, JJ
      INTEGER                 CP                            ! Loop over model
*-

*    Loop over area of image in the fit
      DC_LO = 1
      CP = 0
      IF ( BDS_QUAL_OK ) THEN
        DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
          DO II = GR_RNG_LO(1), GR_RNG_HI(1)
            CP = CP + 1
            DC_BGND(CP) = BGND(II,JJ)
            DC_Q(CP) = (IMQ(II,JJ).EQ.QUAL_GOOD)
            DC_IMD(CP) = IMD(II,JJ)
          END DO
        END DO
      ELSE
        DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
          DO II = GR_RNG_LO(1), GR_RNG_HI(1)
            CP = CP + 1
            DC_IMD(CP) = IMD(II,JJ)
            DC_BGND(CP) = BGND(II,JJ)
          END DO
        END DO
      END IF
      DC_HI = CP

      END

*+  PSS_FIT_CACHE_PSF - Produce predicted data given model parameters
      SUBROUTINE PSS_FIT_CACHE_PSF( PX0, PY0, PNX, PNY, PSFV, STATUS )
*
*    Description :
*
*     Constructs a model given fit parameters.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                 PX0, PY0                      ! Psf centre
      INTEGER                 PNX, PNY                      ! Psf dimensions
      REAL                    PSFV(-PNX/2:PNX/2,	    ! Psf data
     :                             -PNY/2:PNY/2)
*
*    Status :
*
      INTEGER                 STATUS
*
*    Local variables :
*
      INTEGER                 II, JJ, PII, PJJ
      INTEGER                 CP                            ! Loop over model
*-

*    Loop over area of image in the fit
      DC_LO = 1
      CP = 0
      DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
        PJJ = JJ - PY0
        DO II = GR_RNG_LO(1), GR_RNG_HI(1)
          PII = II - PX0
          CP = CP + 1
          IF ( (ABS(PII) .LE. PSF_UPNR) .AND.
     :         (ABS(PJJ) .LE. PSF_UPNR) ) THEN
            DC_PSF(CP) = PSFV(PII,PJJ)
          ELSE
            DC_PSF(CP) = 0.0
          END IF
        END DO
      END DO

      END

*+  PSS_FIT_DLOAD - Sets up PSS_FIT_CMN for fitting package
      SUBROUTINE PSS_FIT_DLOAD( ID, PARAM, LB, UB, FROZEN, PSCALE,
     :                                            STATID, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     27 May 92 : Original (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
      INCLUDE 'PSS_FIT_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER             UTIL_PLOC           ! Replaces %LOC
      INTEGER             PIX                 ! Converts raddian -> pixels
*
*    Import :
*
      INTEGER             ID                  ! Source id
*
*    Export :
*
      REAL                PARAM(PSS__FITNPAR) ! Source parameters
      REAL                LB(PSS__FITNPAR)    ! Lower parameter bounds
      REAL                UB(PSS__FITNPAR)    ! Upper parameter bounds
      LOGICAL             FROZEN(PSS__FITNPAR)! Parameter frozen?
      INTEGER             PSCALE              ! Statistic scale factor
      INTEGER             STATID              ! Statistic to use
*
*    Local constants :
*
      INTEGER             PBORDER
        PARAMETER         ( PBORDER = 3 )
*
*    Local variables :
*
      REAL                CEN(2)              ! Psf position
      REAL                DF                  ! Flux delta
      REAL                DSUM                ! Sum over image data

      INTEGER             AXN                 ! Loop over axes
      INTEGER             I                   ! Loop over parameters
      INTEGER             IP                  ! Loop over parameters
      INTEGER             NDAT                ! Number of data pixels
      INTEGER             PCEN                ! Pixel number

      LOGICAL             FIRST               ! First time through?
*
*    Local data :
*
      SAVE                FIRST
      DATA                FIRST/.TRUE./
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    First time through?
      IF ( FIRST ) THEN
        CALL DYN_MAPR( 1, 2*PSS__CACHELEN, FIT.PRED.PREDPTR(1), STATUS )
        CALL DYN_MAPR( 1, 2*PSS__CACHELEN, FIT.PRED.PREDPTR(2), STATUS )
        CALL DYN_MAPR( 1, 2*P__E*PSS__CACHELEN, FIT.PRED.DFDPPTR,
     :                                                   STATUS )
        FIRST = .FALSE.
      END IF

*    Dataset block
      FIT.DS.D_ID = IM_ID

*    Set up flux parameter
      PARAM(P__F) = S_FLUX(ID)
      DF = MAX( 100.0, 5.0*SQRT(ABS(S_FLUX(ID))) )
      UB(P__F) = PARAM(P__F) + DF
      IF ( DI_FREE_FIT ) THEN
        IF ( UB(P__F) .EQ. 0.0 ) UB(P__F) = 100.0
        LB(P__F) = PARAM(P__F)- (UB(P__F)-PARAM(P__F))
      ELSE
        LB(P__F) = 0.0
      END IF

*    Set up X,Y parameters
      DO IP = P__X, P__Y
        PARAM(IP) = S_CP(IP-P__X+1,ID)
        LB(IP) = PARAM(IP) - REAL(PBORDER)*ABS(AX_DR(IP-1))
        UB(IP) = PARAM(IP) + REAl(PBORDER)*ABS(AX_DR(IP-1))
      END DO

*    Define grid on data
      NDAT = 1
      DO AXN = 1, 2
        CEN(AXN) = PARAM(AXN+1)
        PCEN = PIX( AXN, CEN(AXN) )
        DC_PC(AXN) = PCEN
        CALL PSS_SETRNG( AXN, PCEN, 1, BDS_DIMS(AXN),
     :                            PSF_UPNR+PBORDER )
        NDAT = NDAT * (GR_RNG_HI(AXN)-GR_RNG_LO(AXN)+1)
      END DO

*    Set up pointers into cache
      FIT.DS.NDIM = 1
      FIT.DS.NDAT = NDAT
      FIT.DS.IDIM(1) = FIT.DS.NDAT
      FIT.DS.DPTR = UTIL_PLOC( DC_IMD(DC_LO) )

*    Dataset quality if present
      FIT.DS.QFLAG = BDS_QUAL_OK
      IF ( BDS_QUAL_OK ) THEN
        FIT.DS.QPTR = UTIL_PLOC( DC_Q(DC_LO) )
      ELSE
        FIT.DS.QPTR = 0
      END IF

      IF ( .NOT. CP_CASH ) THEN
        FIT.DS.VPTR = UTIL_PLOC( DC_IMBV(DC_LO) )
      END IF
      FIT.DS.BPTR = UTIL_PLOC( DC_BGND(DC_LO) )
      FIT.DS.TEFF = 1.0

*    Prediction block
      FIT.PRED.CONVOLVE = .FALSE.
      FIT.PRED.NMDIM = 1
      IF ( CP_CASH ) THEN
        STATID = FIT__LOGL
      ELSE
        STATID = FIT__CHISQ
      END IF
      FIT.PRED.DPTR = UTIL_PLOC( DC_MOD(DC_LO) )
      FIT.PRED.IDIMM(1) = FIT.DS.NDAT
      FIT.PRED.NMDAT = FIT.DS.NDAT

*    Cache the observed data
      DC_VOLATILE = .TRUE.
      CALL PSS_FIT_CACHE( BDS_DIMS(1), BDS_DIMS(2),
     :                %VAL(IM_DATA_PTR), %VAL(IM_VAR_PTR),
     :                %VAL(BDS_QUAL_PTR), %VAL(BG_DATA_PTR),
     :                %VAL(BG_VAR_PTR), STATUS )

*    Set bgnd rescale
      IF ( CP_RESCALE ) THEN
        PARAM(P__B) = S_BSCALE(ID)
      ELSE
        PARAM(P__B) = 1.0
      END IF
      LB(P__B) = 0.0
      UB(P__B) = 2.0*S_BSCALE(ID)

*    Set frozen state
      FROZEN(P__F) = .FALSE.
      FROZEN(P__B) = (.NOT. CP_RESCALE)
      FROZEN(P__X) = CP_SPOT
      FROZEN(P__Y) = FROZEN(P__X)

*    Set parameter scaling
      IF ( CP_CASH ) THEN
        DSUM = 0.0
        IF ( BDS_QUAL_OK ) THEN
          DO I = DC_LO, DC_HI
            IF ( DC_Q(I) ) DSUM = DSUM + DC_IMD(I)
          END DO
        ELSE
          DO I = DC_LO, DC_HI
            DSUM = DSUM + DC_IMD(I)
          END DO
        END IF
        PSCALE = NINT(1.0+DSUM)			! Sum of data for Cash statistic
      ELSE
        PSCALE = NDAT - 3 			! Degrees of freedom
      END IF

      END

*+  PSS_FIT_ERR - Find error value given fitting results
      SUBROUTINE PSS_FIT_ERR( BEST, LO, HI, ASYMM, ERROR )
*
*    Description :
*
*     Given a best fit parameter value, and the lower and upper error bounds,
*     PSS_FIT_ERR returns either symmetric or asymmetric errors. Both bounds
*     are checked for good fit.
*
*    History :
*
*     22 Jul 91 : Original (BHVAD::DJA)
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
*    Import :
*
      REAL                    BEST                 ! Best fit parameter
      REAL                    LO, HI               ! Lower and upper bounds
      LOGICAL                 ASYMM                ! Asymmetric errors?
*
*    Export :
*
      REAL                    ERROR(2)             ! Error array
*-

*    Asymmetric errors?
      IF ( ASYMM ) THEN

*      Allow for bad bounds separately
        IF ( LO .EQ. BADERR ) THEN
          ERROR(1) = BADERR
        ELSE
          ERROR(1) = LO
        END IF
        IF ( HI .EQ. BADERR ) THEN
          ERROR(2) = BADERR
        ELSE
          ERROR(2) = HI
        END IF

      ELSE

*      Both bounds good
        IF ( (LO.GE.0.0) .AND. (HI.GE.0.0) ) THEN
          ERROR(1) = ( HI+LO )/2.0

        ELSE IF ( LO .GE. 0.0 ) THEN
          ERROR(1) = LO

        ELSE IF ( HI .GE. 0.0 ) THEN
          ERROR(1) = HI

        ELSE
          ERROR(1) = BADERR

        END IF

      END IF

      END

*+  PSS_FIT_GENMODEL - Produce predicted data given model parameters
      SUBROUTINE PSS_FIT_GENMODEL( FSTAT, NDS, OBDAT, INSTR, PREDDAT,
     :                                MODEL, PARAM, N, PRED, STATUS )
*
*    Description :
*
*     Constructs a model given fit parameters. The argument list is defined
*     by the requirements of the ASTERIX fitting s/w model predictor
*     argument list.
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Jul 93 : Commented for first time. Inline functions replaced
*                 with normal functions. Positions vectorised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'FIT_STRUC'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                 FSTAT                         ! Fit statistic
      INTEGER                 NDS                           ! # datasets
      RECORD /DATASET/        OBDAT(NDS)                    ! Supplied by
      RECORD /INSTR_RESP/     INSTR(NDS)                    ! fitting. Not
      RECORD /MODEL_SPEC/     MODEL                         ! used here...
      RECORD /PREDICTION/     PREDDAT                       ! Data predicted
      REAL                    PARAM(*)                      ! Fit parameters
      INTEGER                 N                             ! Dataset number
*
*    Export :
*
      REAL                    PRED(*)                       ! Model
*
*    Status :
*
      INTEGER                 STATUS
*
*    Function definitions :
*
      INTEGER                 PIX
      REAL                    DAT
*
*    Local variables :
*
      REAL                    BSCALE, FLUX                  ! Fit parameters
      REAL                    CEN(2)                        ! Psf position
      REAL                    OFF(2)                        ! PSF offset vector

      INTEGER                 AXN                           ! Loop over axes
      INTEGER                 CP                            ! Loop over cache
      INTEGER                 PCEN(2)                       ! Pixel number
      INTEGER                 POFF(2)                       ! Pixel number

      LOGICAL                 DIFPOS                        ! Different posn?
*
*    Preserve for next call :
*
      SAVE                    CEN,OFF,POFF
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get parameters into sensible names
      FLUX = PARAM(P__F)
      BSCALE = PARAM(P__B)

*    Find resample offsets
      DIFPOS = .FALSE.
      DO AXN = 1, 2
        IF ( PARAM(AXN+1) .NE. CEN(AXN) ) THEN
          CEN(AXN) = PARAM(AXN+1)
          PCEN(AXN) = PIX( AXN, CEN(AXN) )
          OFF(AXN) = (DAT(AXN,PCEN(AXN)) - CEN(AXN)) / AX_DR(AXN)
          POFF(AXN) = DC_PC(AXN) - PCEN(AXN)
          DIFPOS = .TRUE.
          DC_PC(AXN) = PCEN(AXN)
        END IF
      END DO

*    Different position from last time?
      IF ( DIFPOS ) THEN

*      Resample the psf
        CALL PSF_RESAMPLE( PSF_DIMS(1), PSF_DIMS(2), %VAL(PSF_STORE),
     :                  -OFF(1), -OFF(2), 0, %VAL(PSF_DATA), STATUS )

*      Cache the psf data
        CALL PSS_FIT_CACHE_PSF( PCEN(1), PCEN(2), PSF_DIMS(1),
     :                   PSF_DIMS(2), %VAL(PSF_DATA), STATUS )

      END IF

*    Loop over values in cache creating model values
      IF ( BSCALE .EQ. 1.0 ) THEN
        DO CP = DC_LO, DC_HI
          PRED(CP) = DC_BGND(CP) + DC_PSF(CP)*FLUX
        END DO
      ELSE
        DO CP = DC_LO, DC_HI
          PRED(CP) = DC_BGND(CP)*BSCALE + DC_PSF(CP)*FLUX
        END DO
      END IF

      END

*+  PSS_FIT_GRID - Construct a grid of GPAR against GVARS
      SUBROUTINE PSS_FIT_GRID( PARAM, MODEL, IN_LB, IN_UB, IN_FRO,
     :                 ISTAT, PSCALE, GPAR, GVARS, FPARS, STATUS )
*
*    Description :
*
*     Constructs a grid of the variable GPAR over the sub-space defined
*     by the character string GVARS. This latter string can contain any
*     of the normal fit parameters "x", "y", "f" or "b". GVAR can take
*     the value -1 (the fit statistic converted to probability), 0 (the
*     fit statistic) or if > zero, that parameter number.
*
*    History :
*
*     22 Sep 93 : Original. Derived from previous multitude of gridding
*                 routines (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
      INCLUDE 'PSS_FIT_CMN'
      INCLUDE 'PSS_ASTROM_CMN'
*
*    Import :
*
      REAL                     	PARAM(*)                ! Fit parameters
      RECORD /MODEL_SPEC/       MODEL			! Fit model spec
      LOGICAL                   IN_FRO(*)               !
      REAL                      IN_LB(*), IN_UB(*)      ! Parameter bounds
      INTEGER                   ISTAT                   ! Fit statistic
      INTEGER                   PSCALE                  ! Statistic scale factor
      INTEGER                   GPAR                    ! Thing to grid
      CHARACTER*(*)             GVARS                   ! Grid axes
      CHARACTER*(*)             FPARS                   ! Parameters to freeze
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL                      AXV
      INTEGER                   CHR_LEN
*
*    Local constants :
*
      INTEGER                   DEFAULTDIM              ! Default grid size
        PARAMETER               ( DEFAULTDIM = 50 )
      INTEGER                   MAXAX                   ! Max no. of axes
        PARAMETER               ( MAXAX = 4 )
*
*    Local variables :
*
      RECORD /GRID_AXIS/       	GAX(MAXAX)              ! The grid axes

      CHARACTER*1              	EC                      ! Axis character code
      CHARACTER*30             	FNAME                   ! Filename of grid
      CHARACTER*4              	NSTR                    ! Source number
      CHARACTER*5              	PREFIX                  ! File prefix
      CHARACTER*20             	STR                     ! Environment variable
      CHARACTER*40             	LABEL, UNITS            ! Grid axis attrs
      CHARACTER 		RASTR*9, DECSTR*10	! Position formatted

      DOUBLE PRECISION		CELPOS(2)		! Source position
      DOUBLE PRECISION         	STATMIN                 ! Best value of statistic

      REAL			APOS(2)
      REAL                     	BASE                    ! Axis base value
      REAL                     	SCALE                   ! Axis scale value
      REAL                     	LB(PSS__FITNPAR)        ! Copies of IN_LB
      REAL                     	UB(PSS__FITNPAR)        ! Copies of IN_UB

      INTEGER                  	DIMS(MAXAX)             ! Grid dimensions
      INTEGER                  	FAX, XAX, YAX, BAX      ! Axis numbers if present
      INTEGER                  	DPTR                    ! Grid data array
      INTEGER                  	DIM                     ! Size of an axis
      INTEGER                  	FIP                     ! Parameter to freeze
      INTEGER			GID			! Grid output identifer
      INTEGER                  	IAX                     ! Loop over grid axes
      INTEGER                  	IP                      ! Axis parameter number
      INTEGER                  	LGPAR                   ! Local copy of GPAR
      INTEGER                  	NAX                     ! Number of GAX used
      INTEGER                  	NELM                    ! Number of grid elements
      INTEGER                  	QPTR                    ! Grid quality array

      LOGICAL                  	FROZEN(PSS__FITNPAR)    ! Copies of FROZEN

      BYTE                     	GQMASK                  ! Grid quality mask
*
*    External references :
*
      EXTERNAL                  PSS_FIT_GENMODEL
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      FAX = 0
      XAX = 0
      YAX = 0
      BAX = 0

*    Make copies of input data
      CALL ARR_COP1R( PSS__FITNPAR, IN_LB, LB, STATUS )
      CALL ARR_COP1R( PSS__FITNPAR, IN_UB, UB, STATUS )
      CALL ARR_COP1L( PSS__FITNPAR, IN_FRO, FROZEN, STATUS )

*    Decide what we're going to grid
      UNITS = ' '
      LGPAR = GPAR
      IF ( GPAR .EQ. -1 ) THEN
        PREFIX = 'prob'
        LABEL = 'Probability'
        LGPAR = 0
      ELSE IF ( GPAR .EQ. 0 ) THEN
        PREFIX = 'cash'
        LABEL = 'Cash statistic'
      ELSE IF ( GPAR .EQ. P__F ) THEN
        PREFIX = 'flux'
        LABEL = 'Flux'
        UNITS = IM_UNITS
        FROZEN(P__F) = .FALSE.
      ELSE IF ( GPAR .EQ. P__X ) THEN
        PREFIX = 'x'
        LABEL = 'X position'
        FROZEN(P__X) = .FALSE.
      ELSE IF ( GPAR .EQ. P__Y ) THEN
        PREFIX = 'y'
        LABEL = 'Y position'
        FROZEN(P__Y) = .FALSE.
      ELSE IF ( GPAR .EQ. P__B ) THEN
        PREFIX = 'bgnd'
        LABEL = 'Background rescale factor'
        FROZEN(P__B) = .FALSE.
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'G', GPAR )
        CALL ERR_REP( ' ', 'Invalid grid parameter ^G', STATUS )
        GOTO 99
      END IF

*    Open file
      WRITE( NSTR, '(I4.4)' ) GE_EXEC_NSRC
      FNAME = PREFIX(:CHR_LEN(PREFIX))//'_'//GVARS//'_'//NSTR//'%hds'
      CALL ADI_FCREAT( FNAME, 'BinDS', GID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Find RA and DEC for title
      APOS(1) = AXV( 1, PARAM(P__X) )
      APOS(2) = AXV( 2, PARAM(P__Y) )
      CALL WCI_CNA2S( APOS, GE_PIXID, GE_PRJID, CELPOS, STATUS )
      CALL STR_DRADTOC( CELPOS(1), 'HHhMMmSSs', RASTR, STATUS )
      CALL STR_DRADTOC( CELPOS(2), 'SDDdMMmSSs', DECSTR, STATUS )

*    Loop over parameters to be frozen
      DO IP = 1, LEN(FPARS)

*      Convert to upper case
        EC = FPARS(IP:IP)
        CALL CHR_UCASE( EC )

*      Identify the parameter
        IF ( EC .EQ. 'F' ) THEN
          FIP = P__F

        ELSE IF ( EC .EQ. 'X' ) THEN
          FIP = P__X

        ELSE IF ( EC .EQ. 'Y' ) THEN
          FIP = P__Y

        ELSE IF ( EC .EQ. 'B' ) THEN
          FIP = P__B

*      Allow user to enter spaces to give  null string
        ELSE IF ( EC .EQ. ' ' ) THEN

        ELSE
          CALL MSG_SETC( 'GP', FPARS(IP:IP) )
          CALL ERR_REP( ' ', 'Unrecognised parameter designator',
     :                  STATUS )
          GOTO 99
        END IF

*      Freeze the parameter
        FROZEN(FIP) = .TRUE.

      END DO

*    Loop over requested grid parameters
      NAX = LEN(GVARS)
      DO IAX = 1, NAX

*      Convert to upper case
        EC = GVARS(IAX:IAX)
        CALL CHR_UCASE( EC )

*      Identify the parameter
        IF ( EC .EQ. 'F' ) THEN
          IP = P__F
          FAX = IAX
          BASE = REAL(NINT(PARAM(P__F)))
          IF ( BASE .LT. 10.0 ) THEN
            BASE = -20.0
          ELSE
            BASE = 1.0
          END IF
          LB(P__F) = -25.0
          SCALE = 1.0

        ELSE IF ( EC .EQ. 'X' ) THEN
          IP = P__X
          XAX = IAX
          SCALE = 0.1

        ELSE IF ( EC .EQ. 'Y' ) THEN
          IP = P__Y
          YAX = IAX
          SCALE = 0.1

        ELSE IF ( EC .EQ. 'B' ) THEN
          IP = P__B
          BAX = IAX

        ELSE
          CALL MSG_SETC( 'GP', GVARS(IAX:IAX) )
          CALL ERR_REP( ' ', 'Unrecognised grid axis designator',
     :                  STATUS )
          GOTO 99
        END IF

*      Scale specified in environment?
        CALL PSX_GETENV( 'PSS_DIAG_'//EC//'_SCALE', STR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          CALL CHR_CTOR( STR, SCALE, STATUS )
        END IF

*      Dimension specified in environment?
        CALL PSX_GETENV( 'PSS_DIAG_'//EC//'_DIM', STR, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          DIM = DEFAULTDIM
        ELSE
          CALL CHR_CTOI( STR, DIM, STATUS )
        END IF

*      Find axis base
        IF ( IP .EQ. P__F ) THEN
          IF ( DIM .EQ. DEFAULTDIM ) DIM = (PARAM(P__F)*2.0-BASE)/SCALE
        ELSE IF ( IP .EQ. P__X ) THEN
          SCALE = SCALE*AX_DR(1)
          BASE = PARAM(P__X) - SCALE*REAL(DIM)/2.0
        ELSE IF ( IP .EQ. P__Y ) THEN
          SCALE = SCALE*AX_DR(2)
          BASE = PARAM(P__Y) - SCALE*REAL(DIM)/2.0
        END IF

*      Create the grid axis
        CALL FIT_DEFREGRID( IP, DIM, .FALSE., BASE,
     :                   SCALE, GAX(IAX), STATUS  )

*      Set this dimensions
        DIMS(IAX) = DIM

      END DO

*    Create arrays
      CALL BDI_CREDATA( GID, NAX, DIMS, STATUS )
      CALL BDI_CREQUAL( GID, NAX, DIMS, STATUS )

*    Map arrays
      CALL BDI_MAPDATA( GID, 'WRITE', DPTR, STATUS )
      CALL BDI_MAPQUAL( GID, 'WRITE', QPTR, STATUS )

*    Find product of dimensions
      CALL ARR_SUMDIM( NAX, DIMS, NELM )

*    Create axes
      CALL FIT_TCREGRIDAX( GID, NAX, GAX, STATUS )

*    Axis labels
      IF ( FAX .GT. 0 ) THEN
        CALL BDI_PUTAXLABEL( GID, FAX, 'Flux', STATUS )
        CALL BDI_PUTAXUNITS( GID, FAX, IM_UNITS, STATUS )
      END IF
      IF ( XAX .GT. 0 ) THEN
        CALL BDI_PUTAXVAL( GID, XAX, AXV(1,GAX(XAX).BASE),
     :           GAX(XAX).SCALE/AX_TOR(1), DIMS(XAX), STATUS )
        CALL BDI_PUTAXLABEL( GID, XAX, AX_LABEL(1), STATUS )
        CALL BDI_PUTAXLABEL( GID, XAX, AX_UNITS(1), STATUS )
      END IF
      IF ( YAX .GT. 0 ) THEN
        CALL BDI_PUTAXVAL( GID, YAX, AXV(2,GAX(YAX).BASE),
     :           GAX(YAX).SCALE/AX_TOR(2), DIMS(XAX), STATUS )
        CALL BDI_PUTAXLABEL( GID, YAX, AX_LABEL(2), STATUS )
        CALL BDI_PUTAXLABEL( GID, YAX, AX_UNITS(2), STATUS )
      END IF
      IF ( BAX .GT. 0 ) THEN
        CALL BDI_PUTAXLABEL( GID, BAX,
     :           'Background rescale factor', STATUS )
      END IF

*    Title
      CALL BDI_PUTTITLE( GID, 'RA : '//RASTR//', DEC : '//DECSTR,
     :                       STATUS )

*    Data labels
      CALL BDI_PUTLABEL( GID, LABEL, STATUS )
      CALL BDI_PUTUNITS( GID, UNITS, STATUS )

*    Grid the statistic over the cube
      CALL FIT_GRID( 1, FIT.DS, 0, MODEL, 0, NAX, GAX, 1, LGPAR,
     :               PSS__FITMXIT, PSS__FITNPAR, LB, UB, FROZEN,
     :               PSCALE, 0.0, ISTAT, PSS_FIT_GENMODEL, FIT.PRED,
     :               PARAM, STATMIN, DPTR, %VAL(QPTR), GQMASK, STATUS )

*    Write quality mask
      CALL BDI_PUTMASK( GID, GQMASK, STATUS )

*    Convert to probability if needed
      IF ( GPAR .EQ. -1 ) THEN
        CALL PSS_FIT_P_NORM( NELM, %VAL(DPTR), %VAL(QPTR), STATUS )
      END IF

*    Free the grid object
      CALL BDI_RELEASE( GID, STATUS )
      CALL ADI_FCLOSE( GID, STATUS )

*    Abort point
 99   CONTINUE

      END

*+  PSS_FIT_P2S - Store parameters in source object
      SUBROUTINE PSS_FIT_P2S( PARAM, PGOOD, ID, STATUS )
*
*    Description :
*
*     Updates a source block with a fit parameter set
*
*    History :
*
*     25 Jun 92 : Original (BHVAD::DJA)
*      7 Sep 92 : Writes PGOOD array too (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
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
*    Status :
*
      INTEGER                 STATUS
*
*    Import :
*
      REAL                    PARAM(PSS__FITNPAR)  ! The parameters
      INTEGER                 ID                   ! Source number
*
*    Export :
*
      REAL                    PGOOD(PSS__FITNPAR)  ! The parameters
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Store parameters
      IF ( .NOT. CP_SPOT ) THEN
        S_CP(1,ID) = PARAM(P__X)
        S_CP(2,ID) = PARAM(P__Y)
      END IF
      S_FLUX(ID) = PARAM(P__F)
      S_BSCALE(ID) = PARAM(P__B)

*    Make copy of parameter set
      PGOOD(P__F) = PARAM(P__F)
      PGOOD(P__X) = PARAM(P__X)
      PGOOD(P__Y) = PARAM(P__Y)
      PGOOD(P__B) = PARAM(P__B)

      END

*+  PSS_FIT_PPROB - Get probability of seeing this number of counts, or greater
      SUBROUTINE PSS_FIT_PPROB( PPROB, STATUS )
*
*    Description :
*
*    History :
*
*     22 Jul 93 : Original (BHVAD::DJA)
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
*    Export :
*
      DOUBLE PRECISION         PPROB                   ! Upper tail probability
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      DOUBLE PRECISION         PLEK                    ! Lower tail prob
      DOUBLE PRECISION         PGTK                    ! Upper tail prob
      DOUBLE PRECISION         PEQK                    ! Equality prob

      REAL                     BSUM                    ! Sum of good bgnd points
      REAL                     DSUM                    ! Sum of good data points

      INTEGER                  CP                      ! Loop over data cache
      INTEGER                  IFAIL                   ! NAG status
*-

*    Accumulate sum of oberved counts and background
      DSUM = 0.0
      BSUM = 0.0
      IF ( BDS_QUAL_OK ) THEN
        DO CP = DC_LO, DC_HI
          IF ( DC_Q(CP) ) THEN
            DSUM = DSUM + DC_IMD(CP)
            BSUM = BSUM + DC_BGND(CP)
          END IF
        END DO
      ELSE
        DO CP = DC_LO, DC_HI
          IF ( DC_PSF(CP) .GT. 0.0 ) THEN
            DSUM = DSUM + DC_IMD(CP)
            BSUM = BSUM + DC_BGND(CP)
          END IF
        END DO
      END IF

*    Error if DSUM < BSUM
      IF ( DSUM .LT. BSUM ) THEN
        PPROB = -1.0D0

      ELSE

*      Get the various probabilities
        IFAIL = 0
        CALL G01BKF( DBLE(BSUM), NINT(DSUM), PLEK, PGTK, PEQK, IFAIL )

*      We want upper tail AND the probability of observed counts
        PPROB = PGTK + PEQK

      END IF

      END

*+  PSS_FIT_P_NORM - Normalise a delta-C map to probability
      SUBROUTINE PSS_FIT_P_NORM( N, DELC, QUAL, STATUS )
*
*    Description :
*
*    History :
*
*     24 Nov 92 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER                  N                       ! Number of values
      BYTE                     QUAL(N)                 ! Quality values
*
*    Import / Export :
*
      REAL                     DELC(N)                 ! Values to convert
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                     CMIN                    ! Minimum value in DELC
      REAL                     PTOT                    ! Normalisation constant

      INTEGER                  I                       ! Loop over DELC
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find minimum
      CMIN = VAL__MAXR
      DO I = 1, N
        IF ( (QUAL(I) .AND. QUAL_MASK) .EQ. QUAL_GOOD ) THEN
          IF ( DELC(I) .LT. CMIN ) CMIN = DELC(I)
        END IF
      END DO

*    Convert to probability
      PTOT = 0.0
      DO I = 1, N
        IF ( (QUAL(I).AND.QUAL_MASK) .EQ. QUAL_GOOD ) THEN
          DELC(I) = REAL(DEXP(DBLE(-(DELC(I)-CMIN)/2.0D0)))
          PTOT = PTOT + DELC(I)
        END IF
      END DO

*    Normalise
      DO I = 1, N
        IF ( (QUAL(I).AND.QUAL_MASK) .EQ. QUAL_GOOD )
     :                            DELC(I) = DELC(I)/PTOT
      END DO

      END
