*+  PSS_STAT - Evaluate a statistic in a box
      SUBROUTINE PSS_STAT( RTN, ID, GARG, SMAP, STATUS )
*
*    Description :
*
*     Calls statistic routine RTN to evaluate significance over a region of
*     the input file.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     22 Feb 90 : Original (DJA)
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
      EXTERNAL                 RTN                     ! Statistic routine
      INTEGER                  ID                      ! Source box number
      REAL                     GARG(*)                 ! Additional arguments
*
*    Export :
*
      INTEGER                  SMAP                    ! Sig map data
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     FVEC(2)                 ! Psf shift vector

      INTEGER                  CACHE_CTRL              ! Cache control
      INTEGER                  PPTR                    ! Pointer store
*
*    Local data :
*
      DATA                     FVEC/0.0,0.0/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Access psf once per file?
      IF ( (GR_PASS.EQ.1) .AND. (PSF_ACCESS.EQ.PER_FILE) ) THEN

*      Evaluate PSF at centre of interest
        CALL PSS_PSF_SUBSET( GR_CC, FVEC, %VAL(PSF_STORE), STATUS )

*      Make copy if constant across field
        CALL ARR_COP1R( PSF_UDIMS(1)*PSF_UDIMS(2), %VAL(PSF_STORE),
     :                  %VAL(PSF_DATA), STATUS )

*    Use convolution psf?
      ELSE IF ( CP_USECON ) THEN

*      Swap pointers
        PPTR = PSF_DATA
        PSF_DATA = PSF_CONPTR

*      Default size of psf to use
        PSF_UDIMS(1) = PSF_DIMS(1)
        PSF_UDIMS(2) = PSF_DIMS(2)

*    Access psf once per box?
      ELSE IF ( PSF_ACCESS .EQ. PER_BOX ) THEN

*      Different from last box?
        IF ( ID .NE. PSF_LASTBOX ) THEN

*        Evaluate PSF at centre of interest
          CALL PSS_PSF_SUBSET( GR_CC, FVEC, %VAL(PSF_STORE), STATUS )

*        Store last box id
          PSF_LASTBOX = ID

        END IF

      ELSE

*      Default size of psf to use
        PSF_UDIMS(1) = PSF_DIMS(1)
        PSF_UDIMS(2) = PSF_DIMS(2)

      END IF

*    Set up the cache control
      CACHE_CTRL = 0
      CACHE_CTRL = (CACHE_CTRL.OR.DC_F_IMD )       ! Always need data
      CACHE_CTRL = (CACHE_CTRL.OR.DC_F_PSF )       ! and psf
      IF ( CP_CASH ) THEN
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_BGND )    !
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_LBGND )   !
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_PSF_SUM )
      ELSE
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_IMBV )    !
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_BGDV )    !
      END IF
      IF ( CP_RESCALE .OR. .NOT. CP_OPT ) THEN
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_BGND_SUM )
        CACHE_CTRL = (CACHE_CTRL.OR.DC_F_IMD_SUM )
      END IF

*    Run the statistic routine
      CALL RTN( ID, BDS_DIMS(1), BDS_DIMS(2), PSF_DIMS(1),
     :          PSF_DIMS(2), %VAL(PSF_DATA), CACHE_CTRL, GARG,
     :          GR_DIMS(1), GR_DIMS(2), %VAL(SMAP), STATUS )

      IF ( CP_USECON ) PSF_DATA = PPTR

*    Exit
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_STAT', STATUS )
      END IF

      END

*+  PSS_STAT_CASH - Statistic routine (CASH,NO rescale,optimise)
      SUBROUTINE PSS_STAT_CASH( BOX, NX, NY, PNX, PNY, PSFV,
     :            CACHE_CTRL, GARG, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find Cash significance at each point grid point defined by BOX. The
*     maximum flux is stored in BOX at the end of the subroutine.
*
*    Method :
*
*     See ASTERIX document USER_004
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     GARG                    ! Grid argument
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      REAL                     PSS_CASH_SIG
*
*    Local variables :
*
      REAL                     DELTA_C                 ! C_f=0 - C_opt
      REAL                     LOGSUM, CBIT, TEM
      REAL                     DA, DA2                 ! 1st & 2nd Cash derivs
      REAL                     FLUX                    ! Optimum flux
      REAL                     LASTFLUX                ! Optimum flux last pixel
      REAL                     OLDVAL                  ! Last val in iteration
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Guess flux
        IF ( I .EQ. 1 ) THEN
          FLUX = 0.0
          DO CP = DC_LO, DC_HI
            FLUX = FLUX + DC_IMD(CP) - DC_BGND(CP)
          END DO
          FLUX = MAX( 0.0, FLUX ) / DC_PSF_S
        ELSE
          FLUX = LASTFLUX
        END IF

*      Iterate to find optimum flux
        OLDVAL = FLUX
        NITER = 0
 10     DA = 0.0
        DA2 = 0.0
        DO CP = DC_LO, DC_HI
          TEM = OLDVAL*DC_PSF(CP)+DC_BGND(CP)
          IF ( ABS(TEM) .GT. 1.0E-19 ) THEN
            CBIT = DC_PSF(CP) / TEM
            DA = DA + DC_IMD(CP)*CBIT
            DA2 = DA2 + DC_IMD(CP)*CBIT*CBIT
          END IF
        END DO
        NITER = NITER + 1
        IF ( DA2 .EQ. 0.0 ) THEN
          GOTO 19
        ELSE
          DA = DC_PSF_S - DA
          FLUX = OLDVAL - DA / DA2
          IF ( NITER .GT. MAXITER ) GOTO 19
          IF ( FLUX .LT. 0.0 ) THEN
            FLUX = OLDVAL/2.0
            IF ( (DA .GT. 0.0) .AND. (NITER .GT. 1) ) GOTO 19
          END IF
          IF ( .NOT. PSS_CONVERGED(FLUX,OLDVAL) ) THEN
            OLDVAL = FLUX
            GOTO 10
          END IF
        END IF

*      Good convergence?
        IF ( FLUX .GT. 0.0 ) THEN

*        Find Cash statistic at optimum flux and zero flux
          LOGSUM = 0.0
          DO CP = DC_LO, DC_HI
            TEM = FLUX*DC_PSF(CP) + DC_BGND(CP)
            IF ( ABS(TEM) .GT. 1.0E-19 ) THEN
              LOGSUM = LOGSUM + DC_IMD(CP)*(LOG(TEM)-DC_LBGND(CP))
            END IF
          END DO

*        This is C_(f=0) - C_(f=FLUX)
          DELTA_C = 2.0*( LOGSUM - FLUX*DC_PSF_S )

*        Which gives significance
          SMAP(I,J) = PSS_CASH_SIG(DELTA_C)

        END IF

*      Next point
 19     LASTFLUX = FLUX

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Store spot value parameters
      IF ( GR_NELM .EQ. 1 ) THEN
        S_FLUX(BOX) = FLUX
        S_DSTAT(BOX) = DELTA_C
      END IF

      END

*+  PSS_STAT_CASHR - Statistic routine (CASH,Rescale,optimise)
      SUBROUTINE PSS_STAT_CASHR( BOX, NX, NY, PNX, PNY, PSFV,
     :             CACHE_CTRL, GARG, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Finds Cash significance at each point defined by BOX. The background
*     is rescaled locally. No account is taken of quality.
*
*    Method :
*
*     See ASTERIX document USER_004 in AST_DOCS
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Mar 91 : Original (DJA)
*     28 May 92 : Changed to use data cache (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     GARG                    ! Grid argument
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Functions :
*
      REAL                     PSS_CASH_SIG            !
*
*    Local variables :
*
      REAL                     CSUM
      REAL                     BSCALE                  ! Bgnd rescaling factor
      REAL                     BSCALE0                 ! As BSCALE for flux = 0
      REAL                     CASHSIG                 ! Cash significance
      REAL                     DELTA_BSCALE            ! BSCALE0 - BSCALE
      REAL                     DELTA_C                 ! C_flux=0 - C_opt
      REAL                     FLUX                    ! Optimum flux
      REAL                     LASTBSCALE, LASTFLUX    ! Previous pixel values

      INTEGER                  RMAP                    ! Rescale map unit

      LOGICAL                  GOT_OPT                 ! Found optimum F,B?
      LOGICAL                  DOMAP                   ! Produce rescale map?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Rescale map wanted?
      DOMAP = ( ( GR_PASS .EQ. 1 ) .AND. DI_RESC_MAP )
      IF ( DOMAP ) THEN
        CALL PSS_DIAG_OPEN_RMAP( RMAP, STATUS )
      END IF

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Initialise
        CASHSIG = 0.0

*      Guess FLUX and BSCALE
        IF ( (I.EQ.1) .OR. .NOT. GOT_OPT ) THEN
          IF ( (GR_NELM .EQ. 1) .AND. (S_FLUX(BOX).GT.0.0) ) THEN
            FLUX = S_FLUX(BOX)
            BSCALE = S_BSCALE(BOX)
          ELSE
            BSCALE = CP_IBSCALE
            FLUX = MAX( 0.0,(DC_IMD(DC_CP_C)-
     :                    CP_IBSCALE*DC_BGND(DC_CP_C))/DC_PSF(DC_CP_C))
          END IF
        ELSE
          FLUX = LASTFLUX
          BSCALE = LASTBSCALE
        END IF

*      Find optimum flux and background
        CALL PSS_STAT_CASH_OPTFB( FLUX, BSCALE, GOT_OPT )
        IF ( .NOT. GOT_OPT ) GOTO 20

*      Find new background scale factor for zero flux. Simple Newton-Raphson
*      iteration
        CALL PSS_STAT_CASH_OPTB0( BSCALE, BSCALE0, GOT_OPT )
        IF ( .NOT. GOT_OPT ) GOTO 20

*      Find statistic at zero flux and optimum. This loop is finding the
*      difference in the value of the Cash statistic at (0,BSCALE0) and
*      (FLUX,BSCALE). The summation simplifies considerably...
        CSUM = 0.0
        DELTA_BSCALE = BSCALE0 - BSCALE
        DO CP = DC_LO, DC_HI
          IF ( DC_BGND(CP) .GT. 0.0 ) THEN
            CSUM = CSUM + DC_IMD(CP)*LOG(
     :          (FLUX*DC_PSF(CP)/DC_BGND(CP) + BSCALE) / BSCALE0 )
          END IF
        END DO

*      Difference in statistic from zero flux to optimum
        DELTA_C = 2.0*( DELTA_BSCALE*DC_BGND_S + CSUM - FLUX*DC_PSF_S )

*      Gives significance
        CASHSIG = PSS_CASH_SIG( DELTA_C )

*      Store it
 20     SMAP(I,J) = CASHSIG

*      Write rescale map value if required
        IF ( DOMAP ) WRITE( RMAP, '(1X,2I4,1X,F10.4)' ) I,J,BSCALE

*      Next map point
        LASTFLUX = FLUX
        LASTBSCALE = BSCALE

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Store parameters - correct for single point
      IF ( GR_NELM .EQ. 1 ) THEN
        S_FLUX(BOX) = FLUX
        S_DSTAT(BOX) = DELTA_C
        S_BSCALE(BOX) = BSCALE
      END IF

*    Close rescale map if created
      IF ( DOMAP ) THEN
        CLOSE( RMAP )
        CALL FIO_PUNIT( RMAP, STATUS )
      END IF

      END

*+  PSS_STAT_CASHRU - Statistic routine (CASH, Rescale, Upper limit)
      SUBROUTINE PSS_STAT_CASHRU( BOX, NX, NY, PNX, PNY, PSFV,
     :             CACHE_CTRL, DSTAT, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find upper limit flux at each point defined by SMAP. Background is
*     rescaled locally.
*
*    Method :
*
*     See ASTERIX document USER_004
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Mar 91 : Original
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     DSTAT                   ! Delta C for upper limit
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Functions :
*
      REAL                     PSS_CASH_EVAL
*
*    Local variables :
*
      REAL                     BSCALE                  ! Bgnd rescaling factor
      REAL                     CMIN                    ! Statistic at minimum
      REAL                     FLUX                    ! Optimum flux
      REAL                     UPPER_FLUX              ! Upper limit flux

      INTEGER                  RMAP                    ! Rescale map unit

      LOGICAL                  GOT_OPT                 ! Found optimum F,B?
      LOGICAL                  DOMAP                   ! Produce rescale map?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Rescale map wanted?
      DOMAP = ( ( GR_PASS .EQ. 1 ) .AND. DI_RESC_MAP )
      IF ( DOMAP ) THEN
        CALL PSS_DIAG_OPEN_RMAP( RMAP, STATUS )
      END IF

*    Standard statistic header
      INCLUDE 'PSS_STAT_BIT1'

*      Find optimum flux and background
        CALL PSS_STAT_CASH_OPTFB( FLUX, BSCALE, GOT_OPT )

*      Evaluate statistic at minimum
        CMIN = PSS_CASH_EVAL( FLUX, BSCALE )

*      Upper limit flux
        CALL PSS_STAT_CASH_UPLF( FLUX, BSCALE, CMIN, DSTAT, UPPER_FLUX )

*      Store flux in SMAP in upper limit mapping
        SMAP(I,J) = UPPER_FLUX

*      Write rescale map value if required
        IF ( DOMAP ) WRITE( RMAP, '(1X,2I4,1X,F10.4)' ) I,J,BSCALE

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Single value?
      IF ( GR_NELM .EQ. 1 ) THEN
        S_FLUX(BOX) = UPPER_FLUX
        S_BSCALE(BOX) = BSCALE
        S_BACK(BOX) = S_BACK(BOX) * BSCALE
      END IF

*    Close rescale map if created
      IF ( DOMAP ) THEN
        CLOSE( RMAP )
        CALL FIO_PUNIT( RMAP, STATUS )
      END IF

      END

*+  PSS_STAT_CASHS - Find Cash sensitivity
      SUBROUTINE PSS_STAT_CASHS( BOX, NX, NY, PNX, PNY, PSFV,
     :          CACHE_CTRL, DELSTAT, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Finds that flux which would have been detected at the user's sensitivity
*     threshold, had a source been present. DELSTAT holds the change in the
*     statistic corresponding to that threshold. We have,
*
*      DELSTAT = C(A) - C(0)
*
*              = 2 sum{ A p_ij - (b_ij + A p_ij)ln((A p_ij + b_ij)/b_ij) }
*
*     where A is the flux we require, and the summation is performed over
*     all points in the box. This value is solved for by trial and error
*     (Newton-Raphson iteration).
*
*    Method :
*
*     See ASTERIX document USER_004.
*
*     The algorithm uses the value of,
*
*      DELSTAT = - 2 A^2/b sum{p_ij^2}
*
*     where b is the mean background to find a first guess for the flux.
*     This expression is the result of simplifying the first expression,
*     assuming a weak source, a flat background and sum{p_ij}=1
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Jun 92 : Original (DJA)
*     29 Sep 92 : Fixed bug causing slow convergence (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     DELSTAT                 ! Change in statistic
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     BMEAN                   ! Mean background
      REAL                     PSQR                    ! Sum of p_ij^2

      REAL                     CBIT                    ! log(mval/bgnd)
      REAL                     MVAL                    ! Model value
      REAL                     FA, DFA                 ! Stat and derivative
      REAL                     FLUX                    ! Optimum flux
      REAL                     OLDVAL                  ! Last val in iteration
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Find mean background and sum of psf squares
        BMEAN = 0.0
        PSQR = 0.0
        DO CP = DC_LO, DC_HI
          IF ( DC_BGND(CP) .GE. 0.0 ) THEN
            BMEAN = BMEAN + DC_BGND(CP)
            PSQR = PSQR + DC_PSF(CP)*DC_PSF(CP)
          END IF
        END DO
        BMEAN = BMEAN / REAL(DC_HI-DC_LO+1)

*      Estimate required flux using weak source approximation
        FLUX = MAX( 5.0, SQRT( DELSTAT * BMEAN / 2.0 / PSQR ) )

*      Iterate to find that flux which gives a value of the Cash statistic
*      DELSTAT above that for zero flux. This simple Newton-Raphson iteration
*      terminating according to the covergence criteria used by PSS_CONVERGED,
*      silly flux or too many iterations.
        OLDVAL = FLUX
        NITER = 0
 10     FA = 0.0
        DFA = 0.0
        DO CP = DC_LO, DC_HI
          MVAL = OLDVAL*DC_PSF(CP)+DC_BGND(CP)
          IF ( (MVAL.GT.0.0) .AND. (DC_BGND(CP).GT.0.0) ) THEN
            CBIT = LOG( MVAL / DC_BGND(CP) )
            FA = FA + FLUX * DC_PSF(CP) - MVAL * CBIT
            DFA = DFA + DC_PSF(CP)*CBIT
          END IF
        END DO
        NITER = NITER + 1
        FA = 2.0*FA + DELSTAT
        DFA = -2.0*DFA
        FLUX = OLDVAL - FA / DFA
        IF ( NITER .GT. MAXITER*2 ) GOTO 50
        IF ( FLUX .LT. 0.0 ) THEN
          FLUX = OLDVAL/2.0
          IF ( (DFA .GT. 0.0) .AND. (NITER .GT. 1) ) GOTO 50
        END IF
        IF ( .NOT. PSS_CONVERGED(FLUX,OLDVAL) ) THEN
          OLDVAL = FLUX
          GOTO 10
        END IF

*      Store the flux
        SMAP(I,J) = FLUX

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

      END

*+  PSS_STAT_CASHSV - Maps variance in Cash significance
      SUBROUTINE PSS_STAT_CASHSV( BOX, NX, NY, PNX, PNY, PSFV,
     :              CACHE_CTRL, GARG, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find Cash significance at each point grid point defined by BOX. The
*     maximum flux is stored in BOX at the end of the subroutine.
*
*    Method :
*
*     See ASTERIX document USER_004
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 92 : Original
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     GARG                    ! Grid argument
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     CBIT, TEM
      REAL                     DA, DA2                 ! 1st & 2nd Cash derivs
      REAL                     FLUX                    ! Optimum flux
      REAL                     LASTFLUX                ! Optimum flux last pixel
      REAL                     OLDVAL                  ! Last val in iteration
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Guess flux
        IF ( I .EQ. 1 ) THEN
          FLUX = 0.0
          DO CP = DC_LO, DC_HI
            FLUX = FLUX + DC_IMD(CP) - DC_BGND(CP)
          END DO
          FLUX = MAX( 0.0, FLUX ) / DC_PSF_S
        ELSE
          FLUX = LASTFLUX
        END IF

*      Iterate to find optimum flux
        OLDVAL = FLUX
        NITER = 0
 10     DA = 0.0
        DA2 = 0.0
        DO CP = DC_LO, DC_HI
          TEM = OLDVAL*DC_PSF(CP)+DC_BGND(CP)
          IF ( TEM .NE. 0.0 ) THEN
            CBIT = DC_PSF(CP) / TEM
            DA = DA + DC_IMD(CP)*CBIT
            DA2 = DA2 + DC_IMD(CP)*CBIT*CBIT
          END IF
        END DO
        NITER = NITER + 1
        IF ( DA2 .EQ. 0.0 ) THEN
          GOTO 19
        ELSE
          DA = DC_PSF_S - DA
          FLUX = OLDVAL - DA / DA2
          IF ( NITER .GT. MAXITER ) GOTO 19
          IF ( FLUX .LT. 0.0 ) THEN
            FLUX = OLDVAL/2.0
            IF ( (DA .GT. 0.0) .AND. (NITER .GT. 1) ) GOTO 19
          END IF
          IF ( .NOT. PSS_CONVERGED(FLUX,OLDVAL) ) THEN
            OLDVAL = FLUX
            GOTO 10
          END IF
        END IF

*      Good convergence?
        IF ( FLUX .GT. 0.0 ) THEN

*        Find variance in Cash statistic at optimum flux
          CALL PSS_STAT_CASH_SIGERR( FLUX, 1.0, SMAP(I,J), STATUS )

        END IF

*      Next point
 19     LASTFLUX = FLUX

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

      END

*+  PSS_STAT_CASHU - Cash statistic, upper limit
      SUBROUTINE PSS_STAT_CASHU( BOX, NX, NY, PNX, PNY, PSFV,
     :             CACHE_CTRL, DSTAT, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find Cash significance at each point grid point defined by BOX. After
*     finding the optimum flux, the upper limit at confidence of CP_UCONF
*     is found. In UPLIM mode, both the upper limit flux and significance are
*     returned - in UPMAP mode only the FLUX.
*
*    Method :
*
*     See ASTERIX document USER_004 in AST_DOCS
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     DSTAT                   ! Delta C for upper limit
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic arguments, constants and variables :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Function definitions :
*
      REAL                     PSS_CASH_EVAL
*
*    Local variables :
*
      REAL                     CMIN                    ! Minimum statistic
      REAL                     FLUX                    ! Optimum flux

      LOGICAL                  GOT_OPT                 ! Got optimum flux?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set background rescale
      S_BSCALE(BOX) = 1.0

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Iterate to find optimum Cash flux
        CALL PSS_STAT_CASH_OPTF( FLUX, GOT_OPT )

*      Evaluate statistic at minimum - zero if no positive optimum
        CMIN = PSS_CASH_EVAL( FLUX, S_BSCALE(BOX) )

*      Upper limit flux, stored in SMAP array
        CALL PSS_STAT_CASH_UPLF( FLUX, S_BSCALE(BOX), CMIN, DSTAT,
     :                                                 SMAP(I,J) )

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Get flux
      IF ( GR_NELM .EQ. 1 ) THEN
        S_FLUX(BOX) = SMAP(GR_GXC,GR_GYC)
      END IF

      END

*+  PSS_STAT_CASH_OPTB0 - Finds optimum bgnd at zero flux
      SUBROUTINE PSS_STAT_CASH_OPTB0( BSCALE, BSCALE0, CONVERGED )
*
*    Description :
*
*     Tries to find optimum background for zero flux given value for
*     optimum flux.
*
*    History :
*
*     30 Jul 91 : Original (BHVAD::DJA)
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
      REAL                     BSCALE                  ! Value at optimum
*
*    Export :
*
      REAL                     BSCALE0                 ! Value at flux = 0
      LOGICAL                  CONVERGED               ! Found optimum
*
*    Functions :
*
      LOGICAL                  PSS_CONVERGED
*
*    Local constants :
*
      INTEGER                  MAXITER                 ! Drop dead value
        PARAMETER              ( MAXITER = 20 )
*
*    Local variables :
*
      REAL                     OLDB                    ! Last iteration guess

      INTEGER                  NITER                   ! # iterations used
*-

*    Initialise
      NITER = 1
      BSCALE0 = BSCALE
      CONVERGED = .FALSE.

 10   NITER = NITER + 1
      OLDB = BSCALE0
      BSCALE0 = OLDB - (DC_BGND_S - DC_IMD_S/BSCALE0) /
     :                  ( DC_IMD_S/BSCALE0/BSCALE0 )
      IF ( BSCALE0 .LT. 0.0 ) THEN
        IF ( NITER .LE. 3 ) THEN
          BSCALE0 = 1.0
          GOTO 10
        ELSE
          GOTO 99
        END IF
      ELSE IF ( NITER .GT. MAXITER ) THEN
        GOTO 99
      ELSE IF ( .NOT. PSS_CONVERGED( OLDB, BSCALE0 ) ) THEN
        GOTO 10
      END IF

*    Converged ok
      CONVERGED = .TRUE.

 99   IF ( .NOT. CONVERGED ) BSCALE0 = BSCALE

      END

*+  PSS_STAT_CASH_OPTF - Finds optimum flux
      SUBROUTINE PSS_STAT_CASH_OPTF( FLUX, CONVERGED )
*
*    Description :
*
*     Given a flux F this subroutine finds the first and second partial
*     derivatives with respect to F and uses these to find a next best
*     guess for F. This process is repeated until convergence is achieved.
*
*    History :
*
*     30 Jul 91 : Original (BHVAD::DJA)
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
      REAL                     FLUX                    ! Optimum flux
      LOGICAL                  CONVERGED               ! Found optimum
*
*    Functions :
*
      LOGICAL                  PSS_CONVERGED
*
*    Local constants :
*
      REAL                     WEAKEST_SOURCE          ! You might just believe
        PARAMETER              ( WEAKEST_SOURCE = 4.0 )
      INTEGER                  MAXITER                 ! Drop dead value
        PARAMETER              ( MAXITER = 20 )
*
*    Local variables :
*
      REAL                     DA, DA2                 ! Cash derivatives
      REAL                     MVAL                    ! Model value at point
      REAL                     OLDA                    ! Last iteration guesses
      REAL                     T1                      ! Storage temporary

      INTEGER                  CP                      ! Loop over data cache
      INTEGER                  NITER                   ! Iteration counter
*-

*    Initialise
      NITER = 1
      CONVERGED = .FALSE.

*    Abort if data all zeroes
      IF ( DC_IMD_S .LT. 1.0E-10 ) GOTO 99

*    First guess for flux
      FLUX = ( DC_IMD_S - DC_BGND_S ) / DC_PSF_S
      FLUX = MAX( WEAKEST_SOURCE, FLUX )

*    Loop until convergence on FLUX
 5    DA = 0.0
      DA2 = 0.0
      DO CP = DC_LO, DC_HI
        MVAL = FLUX*DC_PSF(CP) + DC_BGND(CP)
        IF ( MVAL .GT. 0.0 ) THEN
          T1 = DC_PSF(CP)/MVAL
          DA = DA + DC_IMD(CP)*T1
          DA2 = DA2 + DC_IMD(CP)*T1*T1
        END IF
      END DO

*    Check derivatives
      DA = DC_PSF_S - DA
      IF ( DA2 .EQ. 0.0 ) GOTO 99

*    New flux guess
      NITER = NITER + 1
      OLDA = FLUX
      FLUX = OLDA - DA / DA2

*    Physically unreasonable?
      IF ( FLUX .LT. 0.0 ) THEN

*      Allow a few duff guesses
        IF ( NITER .LE. 3 ) THEN
          FLUX = MAX( 0.0, FLUX )
          GOTO 5
        ELSE
          GOTO 99
        END IF

      ELSE IF ( NITER .GT. MAXITER ) THEN
        GOTO 99

      ELSE IF ( .NOT. PSS_CONVERGED(OLDA,FLUX) ) THEN
        GOTO 5

      END IF

*    Converged ok
      CONVERGED = .TRUE.

*    Abort point
 99   IF ( .NOT. CONVERGED ) THEN
        FLUX = 0.0
      END IF

      END

*+  PSS_STAT_CASH_OPTFB - Finds optimum flux and background scale factor
      SUBROUTINE PSS_STAT_CASH_OPTFB( FLUX, BSCALE, CONVERGED )
*
*    Description :
*
*     Given a flux F and a background B, this subroutine finds the two first
*     partial derivatives, and the 3 partial second derivatives of the Cash
*     statistic with respect to these variables. These values are used to
*     find new NF and NB, the next guesses. This process is repeated until
*     convergence is achieved.
*
*     The arguments FLUX and BSCALE contain initial guesses.
*
*    History :
*
*     30 Jul 91 : Original (BHVAD::DJA)
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
*    Import / Export :
*
      REAL                     FLUX                    ! Optimum flux
      REAL                     BSCALE                  ! Optimum rescale factor
*
*    Export :
*
      LOGICAL                  CONVERGED               ! Found optimum
*
*    Functions :
*
      LOGICAL                  PSS_CONVERGED
*
*    Local constants :
*
      REAL                     WEAKEST_SOURCE          ! You might just believe
        PARAMETER              ( WEAKEST_SOURCE = 4.0 )
      INTEGER                  MAXITER                 ! Drop dead value
        PARAMETER              ( MAXITER = 20 )
*
*    Local variables :
*
      REAL                     DA, DA2, DB, DB2, DAB   ! Cash derivatives
      REAL                     DET                     ! Determinant of matrix
      REAL                     MVAL                    ! Model value at point
      REAL                     PVAL                    ! A psf value
      REAL                     OLDA, OLDB              ! Last iteration guesses
      REAL                     T1, T2                  ! Storage temporaries

      INTEGER                  CP                      ! Loop over data cache
      INTEGER                  NITER                   ! # iterations used
*-

*    Initialise
      NITER = 1
      CONVERGED = .FALSE.

*    Loop until convergence on both FLUX and BSCALE
 5    DA = 0.0
      DB = 0.0
      DAB = 0.0
      DA2 = 0.0
      DB2 = 0.0

*    Find all the derivatives
      DO CP = DC_LO, DC_HI
        PVAL = DC_PSF(CP)
        MVAL = FLUX*PVAL + BSCALE*DC_BGND(CP)
        IF ( MVAL .GT. 0.0 ) THEN
          T1 = PVAL/MVAL
          T2 = DC_BGND(CP)/MVAL
          DA = DA + DC_IMD(CP)*T1
          DA2 = DA2 + DC_IMD(CP)*T1*T1
          DAB = DAB + DC_IMD(CP)*T1*T2
          DB = DB + DC_IMD(CP)*T2
          DB2 = DB2 + DC_IMD(CP)*T2*T2
        END IF
      END DO

*    This really means too few counts in the box
      IF ( DAB .EQ. 0.0 ) GOTO 99

      DA = DC_PSF_S - DA
      DB = DC_BGND_S - DB
      OLDA = FLUX
      OLDB = BSCALE
      DET = DA2*DB2-DAB*DAB
      IF ( ABS(DET) .LT. 1.0E-20 ) GOTO 99
      FLUX   = OLDA - (DB2*DA-DAB*DB) / DET
      BSCALE = OLDB - (DA2*DB-DAB*DA) / DET

      NITER = NITER + 1

*    Physically unreasonable?
      IF ( (FLUX.LT.0.0) .OR. (BSCALE.LT.0.0) ) THEN

*      Allow a few duff guesses
        IF ( NITER .LE. 3 ) THEN
          FLUX = MAX( 0.0, FLUX )
          IF ( BSCALE .LT. 0.0 ) BSCALE = 1.0
          GOTO 5
        ELSE
          GOTO 99
        END IF

      ELSE IF ( NITER .GT. MAXITER ) THEN
        GOTO 99

      ELSE IF ( .NOT. ( PSS_CONVERGED(OLDA,FLUX) .AND.
     :             PSS_CONVERGED(OLDB,BSCALE) ) ) THEN
        GOTO 5
      END IF

*    Converged ok
      CONVERGED = .TRUE.

*    Abort point
 99   IF ( .NOT. CONVERGED ) THEN
        FLUX = 0.0
        BSCALE = 1.0
      END IF

      END

*+  PSS_STAT_CASH_SIGERR - Returns error in significance
      SUBROUTINE PSS_STAT_CASH_SIGERR( FLUX, BSCALE, SIGERR, STATUS )
*
*    Description :
*
*     The error ( =sqrt(variance) ) of the significance at a given point
*     is calculated.
*
*    Method :
*
*     See ASTERIX document USER_004
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      6 Aug 92 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     FLUX                    ! Optimum flux
      REAL                     BSCALE                  ! Optimum bgnd scale
*
*    Export :
*
      REAL                     SIGERR                  ! Error in significance
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
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     MVAL                    ! Model value
      REAL                     VAR                     ! Variance in signif

      INTEGER                  CP                      ! Loop over data cache
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find variance in Cash statistic at optimum flux
      VAR = 0.0
      DO CP = DC_LO, DC_HI
        MVAL = FLUX*DC_PSF(CP) + DC_BGND(CP)*BSCALE
        IF ( (MVAL.GT.0.0) .AND. (DC_BGND(CP).NE.0.0) ) THEN
          VAR = VAR + DC_IMD(CP)*(LOG(MVAL/DC_BGND(CP))**2)
        END IF
      END DO

*    Convert to a significance
      SIGERR = SQRT(VAR)

      END

*+  PSS_STAT_CASH_UPLF - Find upper limit flux given minimum
      SUBROUTINE PSS_STAT_CASH_UPLF( FLUXMIN, BSCALE, CMIN, DSTAT,
     :                                                UPPER_FLUX )
*
*    Description :
*
*     Does a Newton-Raphson iteration to find the flux at which the Cash
*     statistic has increased by DSTAT above the value CMIN at the flux
*     FLUXMIN. Background rescaling is passed around but not used.
*
*    History :
*
*     30 Jul 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL                     FLUXMIN                 ! Flux at minimum
      REAL                     CMIN                    ! Statistic at minimum
      REAL                     BSCALE                  ! Background rescale
      REAL                     DSTAT                   ! Change in statistic
*
*    Export :
*
      REAL                     UPPER_FLUX              ! Upper limit flux
*
*    Local constants :
*
      REAL                     EPAR                    ! Convergence criterion
        PARAMETER              ( EPAR = 0.01 )
      INTEGER                  MAXITER                 ! Max iterations
        PARAMETER              ( MAXITER = 20 )
*
*    Functions :
*
      REAL                     PSS_CASH_EVAL
      LOGICAL                  PSS_CONVERGED
*
*    Local variables :
*
      REAL                     CTEST                   ! Test value of statistic
      REAL                     OLDA                    ! Last guess at flux

      INTEGER                  NITER                   ! Iterations to converge
*-

*    Initialise
      NITER = 0
      OLDA = MAX( 10.0, 1.1*FLUXMIN )

*    Next guess
 10   CTEST = PSS_CASH_EVAL( OLDA, BSCALE )
      UPPER_FLUX = FLUXMIN + DSTAT * (OLDA-FLUXMIN)/(CTEST-CMIN)
      NITER = NITER + 1
      IF ( (NITER.LE.MAXITER) .AND. .NOT.
     :                       PSS_CONVERGED(UPPER_FLUX,OLDA) ) THEN
        OLDA = UPPER_FLUX
        GOTO 10
      END IF

*    Check we've got upper contour
      IF ( UPPER_FLUX .LT. FLUXMIN ) UPPER_FLUX = FLUXMIN +
     :                               (FLUXMIN - UPPER_FLUX)

      END

*+  PSS_STAT_GAUSS - Gaussian statistic, both with and without quality
      SUBROUTINE PSS_STAT_GAUSS( BOX, NX, NY, PNX, PNY, PSFV,
     :             CACHE_CTRL, GARG, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find gaussian significance at each point grid point defined by BOX. The
*     maximum flux is stored in BOX at the end of the subroutine.
*
*    Method :
*
*     FOR each grid point
*       Define bounds of psf box in image pixels
*       IF psfd.varying THEN Evaluate psf
*       IF psfd.resample THEN
*         Find offset of grid point from image pixel centre
*         Resample psf to account for offset
*       END IF
*       Find sig
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     GARG                    ! Grid argument
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard header :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     ASUM                    !
      REAL                     CORR, VARIANCE          ! Gaussian stat & variance
      REAL                     FLUX                    ! Optimum flux
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Accumulate the statistic
        CORR = 0.0
        VARIANCE = 0.0
        DO CP = DC_LO, DC_HI
          PVAL = DC_PSF(CP)
          CORR = CORR + DC_IMD(CP) * PVAL / DC_IMBV(CP)
          VARIANCE = VARIANCE + PVAL * PVAL * DC_BGDV(CP)
        END DO

*      Store significance
        IF ( VARIANCE .GT. 0.000001 ) CORR = CORR / SQRT(VARIANCE)
        SMAP(I,J) = CORR

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Single point?
      IF ( GR_NELM .EQ. 1 ) THEN

*      Find sum of psf^2/var
        ASUM = 0.0
        DO CP = DC_LO, DC_HI
          ASUM = (DC_PSF(CP)*DC_PSF(CP))/DC_IMBV(CP)
        END DO
        S_FLUX(BOX) = S_SIG(BOX) / SQRT(ASUM)

      END IF

      END

*+  PSS_STAT_GAUSSU - Gaussian statistic, both with and without quality
      SUBROUTINE PSS_STAT_GAUSSU( BOX, NX, NY, PNX, PNY, PSFV,
     :              CACHE_CTRL, DSTAT, SNX, SNY, SMAP, STATUS )
*
*    Description :
*
*     Find gaussian significance at each point grid point defined by BOX. The
*     maximum flux is stored in BOX at the end of the subroutine.
*
*    Method :
*
*     FOR each grid point
*       Define bounds of psf box in image pixels
*       IF psfd.varying THEN Evaluate psf
*       IF psfd.resample THEN
*         Find offset of grid point from image pixel centre
*         Resample psf to account for offset
*       END IF
*       Find sig
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Mar 91 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      REAL                     DSTAT                   ! Delta C for upper limit
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Standard statistic stuff :
*
      INCLUDE 'PSS_STAT_BIT0'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     CORR, VARIANCE          ! Gaussian stat & variance
      REAL                     FLUX                    ! Optimum flux
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Standard statistic loop entry
      INCLUDE 'PSS_STAT_BIT1'

*      Accumulate statistic
        CORR = 0.0
        VARIANCE = 0.0
        DO CP = DC_LO, DC_HI
          PVAL = DC_PSF(CP)
          CORR = CORR + DC_IMD(CP)*PVAL/DC_IMBV(CP)
          VARIANCE = VARIANCE + PVAL*PVAL*DC_BGDV(CP)
        END DO
        IF ( VARIANCE .GT. 0.000001 ) CORR = CORR / SQRT(VARIANCE)
        SMAP(I,J) = CORR

*    Standard statistic loop exit. Contains pixel abort label 50
      INCLUDE 'PSS_STAT_BIT2'

*    Store parameters - correct for single point
      IF ( GR_NELM .EQ. 1 ) THEN
        S_FLUX(BOX) = FLUX
      END IF

      END
