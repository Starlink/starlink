*+  PSF_PROMPT - Handles the 'PSF' parameter for the PSF system
      SUBROUTINE PSF_PROMPT( USEDEFAULT, DEFAULT, SLOT, STATUS )
*
*    Description :
*
*     Interfaces with user using the "PSF" parameter. Handles display of
*     options, choice from list and validation.
*
*     The user is presented with a list of the available options. This
*     consists of a capitalised list of all the library psf functions
*     installed, plus descriptions of the syntax of the model options.
*     The latter enable efficient energy and spatial dependent access
*     by software using the psf system.
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
*     25 Oct 92 : Added channel spectrum model option (DJA)
*     23 Feb 94 : Doesn't display options if PSF parameter set (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PAR_PAR'
*
*    Import :
*
      LOGICAL                   USEDEFAULT              ! Use default psf
      CHARACTER*(*)             DEFAULT                 ! The default psf
      INTEGER                   SLOT                    ! Psf slot
*
*    Status :
*
      INTEGER                   STATUS
*
*    Function definitions :
*
      LOGICAL                   CHR_SIMLR
*
*    Local constants :
*
      CHARACTER*3               PAR                     ! Parameter name to use
	 PARAMETER              ( PAR = 'PSF' )
*
*    Local variables :
*
      CHARACTER*80             	UPSF                 	! User reply to PSF

      INTEGER                  	BEG, END                ! Useful bit of UPSF
      INTEGER			STATE			! PSF parameter state
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set the default if required
      IF ( USEDEFAULT ) THEN
	CALL USI_DEF0C( PAR, DEFAULT, STATUS )
	STATUS = SAI__OK
      ELSE
	CALL USI_DEF0C( PAR, 'ANALYTIC', STATUS )
      END IF

*    Display PSFs first time through
      CALL USI_STATE( PAR, STATE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( STATE .NE. PAR__ACTIVE ) THEN
        CALL PSF_DISPLAY( STATUS )
      END IF

*    Ask user to choose a PSF
 59   CALL USI_GET0C( PAR, UPSF, STATUS )
      CALL USI_CANCL( PAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Locate interesting bit of string
      CALL CHR_FANDL( UPSF, BEG, END )

*    Did use try one of the special options?
      IF ( CHR_SIMLR( UPSF(BEG:END), 'LIST') ) THEN
	CALL PSF_DISPLAY( STATUS )
	GOTO 59

*    Parse the psf specification
      ELSE

*      Parse model
	CALL PSF_MODEL_PARSE( UPSF(BEG:END), SLOT, STATUS )

      END IF

*    Calling routine will report status
 99   CONTINUE

      END



*+  PSF_MODEL_PARSE - Parse a psf model specification
      SUBROUTINE PSF_MODEL_PARSE( STR, SLOT, STATUS )
*
*    Description :
*
*     Parse a psf specification. The syntax of a psf specification is as
*     follows,
*
*       psf_spec        := CSPEC( spatial_model, spectrum [,nbin] )
*                       := MSPEC( spatial_model, model [,nbin] )
*                       := spatial_model
*
*       spatial_model   := POLAR( simple_psf, rbin_spec [,abin_spec] )
*                       := RECT( simple_psf, xbin_spec [,ybin_spec] )
*                       := simple_psf
*
*       simple_psf      := library routine identifier
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
*     24 Oct 92 : Added CSPEC model option (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(PSF_CMN)'
*
*    Import :
*
      CHARACTER*(*)            STR                     ! The model spec
      INTEGER                  SLOT                    ! Psf slot
*
*    Status :
*
      INTEGER                  STATUS
*
*    Function definitions :
*
      LOGICAL                  CHR_ISDIG
      LOGICAL                  CHR_SIMLR
*
*    Local variables :
*
      INTEGER                  BPOS                    ! Character pointer
      INTEGER                  IC                      ! Character pointer
      INTEGER                  NBIN                    ! # model energy bins
      INTEGER                  NB_B, NB_E              ! nbin bit
      INTEGER                  S_B, S_E                ! Spatial bit
      INTEGER                  SP_B, SP_E              ! Spectral bit

      LOGICAL                  AT_END                  ! End of parse?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Channel model option
      IF ( CHR_SIMLR( STR(1:5), 'CSPEC' ) .OR.
     :     CHR_SIMLR( STR(1:5), 'MSPEC' ) ) THEN

*      Channel spectrum option
	IF ( (STR(1:1).EQ.'C') .OR. (STR(1:1).EQ.'c') ) THEN
	  EM_MODE(SLOT) = PSF_E_CHANNEL

*      Energy model
	ELSE
	  EM_MODE(SLOT) = PSF_E_MODEL

	END IF

*      Locate bracket
	S_B = INDEX( STR, '(' )

*      Next non-blank is start of spatial section
	CALL PSF_PAR_SKIP( STR, S_B, 'psf name', STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Define spatial psf bit. Beginning of spatial psf is always
*      an identifier - either a psf name or a spatial model
*      constructor.
        IC = S_B
	AT_END = .FALSE.
	DO WHILE ( IC .LE. LEN(STR) .AND. .NOT. AT_END )
	  IC = IC + 1
	  IF ( IC .LE. LEN(STR) ) THEN
	    AT_END = ( (STR(IC:IC).LE.' ') .OR.
     :		       (INDEX(',(',STR(IC:IC)).GT.0) )
	  END IF
	END DO
        S_E = IC - 1
	IF ( .NOT. AT_END ) THEN
	  STATUS = SAI__ERROR
	  CALL MSG_SETC( 'REASON', 'Expected , while parsing'/
     :                                  /' spatial psf name' )
	  GOTO 99
	END IF

*      Skip any blanks after spatial psf word
	IC = S_E
	CALL PSF_PAR_SKIP( STR, IC, ')', STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*      If this character is an open parenthesis then we have a spatial
*      model, whose end is then found, otherwise we have found the end of
*      the spatial model.
	IF ( STR(IC:IC) .EQ. '(' ) THEN
	  BPOS = INDEX(STR(IC:),')')
	  IF ( BPOS .EQ. 0 ) THEN
	    STATUS = SAI__ERROR
	    CALL MSG_SETC( 'REASON', 'Missing )' )
	    GOTO 99
	  END IF
	  S_E = IC + BPOS - 1
	  IC = S_E

*        Skip blanks after closing parenthesis
	  CALL PSF_PAR_SKIP( STR, IC, ')', STATUS )
	  IF ( STATUS .NE. SAI__OK ) GOTO 99

	END IF

*      Parse the spatial bit to check for errors
	CALL PSF_PAR_SPATIAL( STR(S_B:S_E), SLOT, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*      IC now points to first non-blank after end of spatial model
*      specification. It should be comma.
	IF ( STR(IC:IC) .NE. ',' ) THEN
	  STATUS = SAI__ERROR
	  CALL MSG_SETC( 'REASON', 'Missing spectrum name' )
	  GOTO 99
	END IF

*      Skip blanks to spectrum
	CALL PSF_PAR_SKIP( STR, IC, 'spectrum', STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99
	SP_B = IC

*      Spectrum is contiguous sequence of characters, not including
*      spaces, commas or terminal brackets.
	AT_END = .FALSE.
	DO WHILE ( IC .LE. LEN(STR) .AND. .NOT. AT_END )
	  IC = IC + 1
	  IF ( IC .LE. LEN(STR) ) THEN
	    AT_END = ( (STR(IC:IC).LE.' ') .OR.
     :		       (INDEX(',)',STR(IC:IC)).GT.0) )
	  END IF
	END DO
        SP_E = IC - 1
	IF ( .NOT. AT_END ) THEN
	  STATUS = SAI__ERROR
	  CALL MSG_SETC( 'REASON', 'Expected ) while parsing'/
     :                                     /' spectrum name' )
	  GOTO 99
	END IF

*      Skip space if present
	IF ( STR(IC:IC) .LE. ' ' ) THEN
	  CALL PSF_PAR_SKIP( STR, IC, ')', STATUS )
	  IF ( STATUS .NE. SAI__OK ) GOTO 99
	END IF

*      Optional NBIN argument. Default to one if not present
	IF ( STR(IC:IC) .EQ. ',' ) THEN

*        Skip space
	  CALL PSF_PAR_SKIP( STR, IC, '<nbin>', STATUS )
	  IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Read in number
          NB_B = IC
	  AT_END = .FALSE.
	  DO WHILE ( IC .LE. LEN(STR) .AND. .NOT. AT_END )
	    IC = IC + 1
	    IF ( IC .LE. LEN(STR) ) THEN
	      AT_END = (.NOT. CHR_ISDIG(STR(IC:IC)) )
	    END IF
	  END DO
          NB_E = IC - 1
	  IF ( .NOT. AT_END ) THEN
	    STATUS = SAI__ERROR
	    CALL MSG_SETC( 'REASON', 'Expected ) while parsing'/
     :                                             /' <nbin>.' )
	    GOTO 99
	  END IF

*        Convert number
          CALL CHR_CTOI( STR(NB_B:NB_E), NBIN, STATUS )
	  IF ( STATUS .NE. SAI__OK ) GOTO 99

	ELSE
	  NBIN = 1

	END IF

*      Analyse the spectrum
        CALL PSF_PAR_ANSPEC( STR(SP_B:SP_E), NBIN, SLOT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
	  CALL MSG_SETC( 'REASON', 'Error reading channel spectrum' )
	  GOTO 99
        END IF

*      Check end of model
	IF ( STR(IC:IC) .NE. ')' ) THEN
	  STATUS = SAI__ERROR
	  CALL MSG_SETC( 'REASON', 'Expected )' )
	  GOTO 99
	END IF

	EM_OK(SLOT) = (STATUS.EQ.SAI__OK)

*    No spectral component
      ELSE

*      No energy model
	EM_MODE(SLOT) = PSF_E_NONE
	EM_OK(SLOT) = .FALSE.

*      Parse spatial psf
	CALL PSF_PAR_SPATIAL( STR, SLOT, STATUS )

      END IF

*    Set model flag if ok
 99   IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_PRNT( '! ^REASON in psf energy model specification' )
	CALL AST_REXIT( 'PSF_MODEL_PARSE', STATUS )
      END IF

      END


*+  PSF_PAR_ANSPEC - Analyse a model
      SUBROUTINE PSF_PAR_ANSPEC( SPEC, NBIN, SLOT, STATUS )
*
*    Description :
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(PSF_CMN)'
*
*    Import :
*
      CHARACTER*(*)            SPEC                    ! The channel spectrum
      INTEGER                  SLOT                    ! Psf slot
      INTEGER                  NBIN                    ! # channel bins
*
*    Status :
*
      INTEGER                  STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! Spectrum dataset
      CHARACTER*40             TEXT                    ! Axis label/text

      INTEGER                  APTR                    ! Axis pointer
      INTEGER                  DIMS(DAT__MXDIM)        ! Spectrum dimensions
      INTEGER                  DPTR                    ! Data pointer
      INTEGER                  NDIM                    ! Spectrum dimensionality
      INTEGER                  NSBIN                   ! # spectral bins
      INTEGER                  QPTR                    ! Quality pointer
      INTEGER                  SBDA                    ! Spectrum BDA identifier
      INTEGER                  WPTR                    ! Axis widths ptr

      LOGICAL                  ANYBAD                  ! Bad quality points?
      LOGICAL                  OK                      ! Spectrum ok?
      LOGICAL                  QOK                     ! Spectrum quality ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Try to open file
      CALL HDS_OPEN( SPEC, 'READ', SLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'Unable to open spectrum '//SPEC, STATUS )
        GOTO 99
      END IF

*    Give to BDA system
      CALL BDA_FIND( SLOC, SBDA, STATUS )

*    Check dimensions and data
      CALL BDA_CHKDATA_INT( SBDA, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'SPEC', SPEC )
        CALL ERR_REP( ' ', 'Spectrum ^SPEC contains invalid data',
     :                                                    STATUS )
        GOTO 99
      END IF
      IF ( NDIM .NE. 1 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Spectrum is not one dimensional', STATUS )
        GOTO 99
      END IF
      NSBIN = DIMS(1)

*    Check axis is channels
      CALL BDA_GETAXUNITS_INT( SBDA, 1, TEXT, STATUS )
      IF ( INDEX(TEXT,'channel') .EQ. 0 ) THEN
        CALL BDA_GETAXLABEL_INT( SBDA, 1, TEXT, STATUS )
        CALL MSG_SETC( 'UN', TEXT )
        IF ( INDEX(TEXT,'channel') .EQ. 0 ) THEN
          CALL MSG_SETC( 'LAB', TEXT )
          CALL MSG_PRNT( '! Unrecognised axis ^LAB with units ^UN,'/
     :                               /' assuming channel spectrum' )
        END IF
      END IF

*    Quality present?
      CALL BDA_CHKQUAL_INT( SBDA, QOK, NDIM, DIMS, STATUS )

*    Map input arrays
      CALL BDA_MAPAXVAL_INT( SBDA, 'READ', 1, APTR, STATUS )
      CALL BDA_MAPAXWID_INT( SBDA, 'READ', 1, WPTR, STATUS )
      CALL BDA_MAPDATA_INT( SBDA, 'READ', DPTR, STATUS )
      IF ( QOK ) THEN
        CALL BDA_MAPLQUAL_INT( SBDA, 'READ', ANYBAD, QPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDA_UNMAPLQUAL_INT( SBDA, STATUS )
          QOK = .FALSE.
        END IF
      END IF

*    Map space for channel bounds
      EM_NBIN(SLOT) = NBIN
      CALL DYN_MAPI( 1, NBIN, EM_CBPTR(SLOT), STATUS )

*    Determine model bin centres
      CALL PSF_PAR_ANSPEC_INT( NSBIN, %VAL(APTR), %VAL(WPTR),
     :                         %VAL(DPTR), QOK, %VAL(QPTR), NBIN,
     :                         %VAL(EM_CBPTR(SLOT)), STATUS )

*    Close spectrum
      CALL BDA_RELEASE_INT( SBDA, STATUS )
      CALL HDS_CLOSE( SLOC, STATUS )

*    Tidy up
 99   CONTINUE

      END



*+  PSF_PAR_ANSPEC_INT - Find energy model channel bounds
      SUBROUTINE PSF_PAR_ANSPEC_INT( N, AXIS, AWID, DATA, QOK, QUAL,
     :                                          NBIN, CBIN, STATUS )
*
*    Description :
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Oct 92 : Original (DJA)
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
      INTEGER                  N                       ! # spectral bins
      REAL                     AXIS(*)                 ! Spectral axis
      REAL                     AWID(*)                 ! Spectral axis widths
      REAL                     DATA(*)                 ! Spectral data
      LOGICAL                  QOK                     ! Quality present
      LOGICAL                  QUAL(*)                 ! Spectral quality
      INTEGER                  NBIN                    ! # energy model bins
*
*    Export :
*
      INTEGER                  CBIN(NBIN)              ! Channel centres
*
*    Status :
*
      INTEGER                  STATUS
*
*    Local variables :
*
      REAL                     CASUM                   ! Sum over DATA*AXIS
      REAL                     CSUM                    ! Sum over DATA
      REAL                     LAST_SUM                !
      REAL                     TFRAC                   !

      INTEGER                  I                       ! Loop over spectrum
      INTEGER                  J                       ! Loop over energy model
      INTEGER                  LAST_LO                 !

      LOGICAL                  DONE                    ! Got a channel yet?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Simple mean photon channel?
      IF ( NBIN .EQ. 1 ) THEN

        CASUM = 0.0
        CSUM = 0.0
        IF ( QOK ) THEN
          DO I = 1, N
            IF ( QUAL(I) ) THEN
              CASUM = CASUM + DATA(I)*AXIS(I)
              CSUM = CSUM + DATA(I)
            END IF
          END DO
        ELSE
          DO I = 1, N
            CASUM = CASUM + DATA(I)*AXIS(I)
            CSUM = CSUM + DATA(I)
          END DO
        END IF
        CBIN(1) = CASUM / CSUM

      ELSE

*      Find total in spectral bins
        CSUM = 0.0
        IF ( QOK ) THEN
          DO I = 1, N
            IF ( QUAL(I) ) CSUM = CSUM + DATA(I)
          END DO
        ELSE
          DO I = 1, N
            CSUM = CSUM + DATA(I)
          END DO
        END IF

*      Loop over energy bins
        LAST_LO = 1
        LAST_SUM = 0.0
        DO J = 1, NBIN

*        Target fraction
          TFRAC = (REAL(J-1)+0.5) * CSUM / REAL(NBIN)

*        Loop over spectral bins until required fraction found
          I = LAST_LO
          CASUM = LAST_SUM
          DONE = .FALSE.
          DO WHILE ( (I .LE. N) .AND. .NOT. DONE )
            CASUM = CASUM + DATA(I)
            IF ( CASUM .LT. TFRAC ) THEN
              I = I + 1
              LAST_LO = LAST_LO + 1
              LAST_SUM = CASUM
            ELSE
              CBIN(J) = NINT(AXIS(I) +
     :             ((TFRAC-LAST_SUM)/(CASUM-LAST_SUM)- 0.5 )*AWID(I))
              DONE = .TRUE.
            END IF
          END DO

        END DO

      END IF

      END



*+  PSF_PAR_SPATIAL - Parse a psf spatial model specification
      SUBROUTINE PSF_PAR_SPATIAL( STR, SLOT, STATUS )
*
*    Description :
*
*     Parse a psf spatial specification. The syntax is,
*
*       spatial_model   := POLAR( simple_psf, rbin_spec [,abin_spec] )
*                       := RECT( simple_psf, xbin_spec [,ybin_spec] )
*                       := simple_psf
*
*       simple_psf      := library routine identifier
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Oct 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(PSF_CMN)'
*
*    Import :
*
      CHARACTER*(*)            STR                     ! The model spec
      INTEGER                  SLOT                    ! Psf slot
*
*    Status :
*
      INTEGER                  STATUS
*
*    Function definitions :
*
      LOGICAL                  CHR_ISALF, CHR_SIMLR
*
*    Local variables :
*
      INTEGER                  BEG, IC                 ! Character pointer
      INTEGER                  IR                      ! Loop over r bins
      INTEGER                  N1, N2                  ! More pointers

      LOGICAL                  MORE                    ! More numbers?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Which model is being parsed
      IF ( CHR_SIMLR(STR(1:5),'POLAR') ) THEN
	SM_TYPE(SLOT) = PSF_PGRID

      ELSE IF ( CHR_SIMLR(STR(1:4),'RECT') ) THEN
	SM_TYPE(SLOT) = PSF_RGRID

*    Simple psf
      ELSE

*      Look for named psf
	CALL PSF_CHKLIBRTN( ' ', STR, P_LIBID(SLOT), P_MODID(SLOT),
     :                      STATUS )
        P_MODEL(SLOT) = .FALSE.

*      Report error if PSF not found
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL MSG_PRNT( 'Could not find PSF called '//STR )
	  CALL ERR_ANNUL( STATUS )
	  GOTO 59
        ELSE
          GOTO 99
	END IF

      END IF

*    Locate bracket
      IC = INDEX( STR, '(' ) + 1

*    Next item is the psf name
      CALL CHR_FIWS( STR, IC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_SETC( 'REASON', 'Missing psf name' )
	GOTO 99
      END IF

*    Find end of psf name, and look for psf in tables
      BEG = IC
      DO WHILE ( CHR_ISALF(STR(IC:IC)) .OR. (STR(IC:IC).EQ.'_') )
	IC = IC + 1
      END DO
      IC = IC - 1
      CALL PSF_CHKLIBRTN( ' ', STR(BEG:IC), P_LIBID(SLOT),
     :                             P_MODID(SLOT), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_SETC( 'REASON', 'Unknown psf /'//STR(BEG:IC) )
	GOTO 99
      END IF

*    Locate comma
      IC = IC + 1
      IF ( STR(IC:IC) .NE. ',' ) THEN
	CALL MSG_SETC( 'REASON', 'Comma expected' )
	GOTO 99
      END IF
      IC = IC + 1

*    Radial/x-axis specification
      CALL CHR_FIWS( STR, IC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN
	  CALL MSG_SETC( 'REASON', 'Missing radial bin specification')
	ELSE
	  CALL MSG_SETC( 'REASON', 'Missing x axis bin specification')
	END IF
	GOTO 99
      END IF

*    Get the 2nd argument and parse it
      BEG = IC
      DO WHILE ( INDEX('01234567890.:',STR(IC:IC)) .NE. 0 )
	IC = IC + 1
      END DO
      IC = IC - 1
      IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN
	IF ( INDEX( STR(BEG:IC), ':' ) .EQ. 0 ) THEN
	  CALL CHR_CTOR( STR(BEG:IC), SM_P_DR(SLOT), STATUS )
	  IF ( STATUS .EQ. SAI__OK ) THEN
	    SM_P_REG(SLOT) = .TRUE.
	  ELSE
	    CALL MSG_SETC( 'REASON', 'Invalid radial bin '/
     :                                   /'specification' )
	    GOTO 99
	  END IF
	ELSE

*        This argument is of the form <r1> : <r2> ...
	  MORE = .TRUE.
	  N1 = BEG
	  IR = 0
	  DO WHILE (MORE)

*          Locate end of next number
	    N2 = INDEX(STR(N1:IC),':')
	    IF ( N2 .EQ. 0 ) THEN
	      N2 = IC + 1
	      MORE = .FALSE.
	    ELSE
	      N2 = N1 + N2 - 1
	    END IF

*          Get radius
	    IR = IR + 1
	    CALL CHR_CTOR( STR(N1:N2-1), SM_P_RUP(IR,SLOT), STATUS )
	    IF ( STATUS .NE. SAI__OK ) THEN
	      CALL MSG_SETC( 'REASON', 'Invalid radial bin '/
     :                                     /'specification' )
	      GOTO 99
	    END IF

*          Radii must increase
	    IF ( IR .GT. 1 ) THEN
	      IF ( SM_P_RUP(IR,SLOT) .LE. SM_P_RUP(IR-1,SLOT) ) THEN
		STATUS = SAI__ERROR
		CALL MSG_SETC( 'REASON', 'Radial bin boundaries '/
     :                                          /'must increase' )
		GOTO 99
	      END IF
	    END IF

*          Filled available slots
	    IF ( (IR .EQ. (PGRID_MAXR-2)) .AND. MORE ) THEN
	      CALL MSG_PRNT( '! Too many radial bins, ignoring extra')
	      MORE = .FALSE.
	    END IF

*          Next number
	    IF ( MORE ) N1 = N2 + 1

	  END DO

*        Tidy up the radial bin values. First insert a zero at the
*        beginning if needed
	  IF ( SM_P_RUP(1,SLOT) .NE. 0.0 ) THEN
	    SM_P_NR(SLOT) = IR + 1
	    DO IR = SM_P_NR(SLOT)-1, 1, -1
	      SM_P_RUP(IR+1,SLOT) = SM_P_RUP(IR,SLOT)
	    END DO
	    SM_P_RUP(1,SLOT) = 0.0
	  ELSE
	    SM_P_NR(SLOT) = IR
	  END IF

*        Now terminate with a large radius (~ infinity)
	  SM_P_NR(SLOT) = SM_P_NR(SLOT) + 1
	  SM_P_RUP(SM_P_NR(SLOT),SLOT) = 1.0E10

*        Finally, square all the radii
          CALL ARR_SQR1R( SM_P_RUP(1,SLOT), SM_P_NR(SLOT), STATUS )

	END IF
	SM_P_NA(SLOT) = 1

      ELSE
	CALL CHR_CTOR( STR(BEG:IC), SM_R_DX(SLOT), STATUS )
	SM_R_DY(SLOT) = SM_R_DX(SLOT)

      END IF

*    Locate next character
      IC = IC + 1
      IF ( INDEX( ',)', STR(IC:IC)) .EQ. 0 ) THEN
	CALL MSG_SETC( 'REASON', 'Comma or bracket expected' )
	GOTO 99
      ELSE IF ( STR(IC:IC) .EQ. ')' ) THEN
	GOTO 89
      END IF
      IC = IC + 1

*    Parse 3rd argument
      BEG = IC
      DO WHILE ( INDEX('01234567890.',STR(IC:IC)) .NE. 0 )
	IC = IC + 1
      END DO
      IC = IC - 1
      IF ( SM_TYPE(SLOT) .EQ. PSF_PGRID ) THEN
	CALL CHR_CTOI( STR(BEG:IC), SM_P_NA(SLOT), STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL MSG_SETC( 'REASON', 'Invalid azimuthal bin '/
     :                                    /'specification' )
	  GOTO 99
	END IF
      ELSE
	CALL CHR_CTOR( STR(BEG:IC), SM_R_DY(SLOT), STATUS )
      END IF
      IC = IC + 1

*    Get bracket
 59   IF ( STR(IC:IC) .NE. ')' ) THEN
	CALL MSG_SETC( 'REASON', 'Right bracket expected' )
	STATUS = SAI__ERROR
      END IF

*    Model ok?
 89   P_MODEL(SLOT) = ( STATUS .EQ. SAI__OK )

*    Set model flag if ok
 99   IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_PRNT( '! ^REASON in model specification' )
	CALL ERR_REP( ' ', '...from PSF_MODEL_PARSE', STATUS )
      END IF

      END


*+  PSF_PAR_SKIP - Skip white space in string
      SUBROUTINE PSF_PAR_SKIP( STR, IC, X, STATUS )
*
*    Description :
*
*     Finds first non-blank character after present position.
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
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
      CHARACTER*(*)            STR                     ! The model spec
      CHARACTER*(*)            X                       ! What we expect
*
*    Import/export :
*
      INTEGER                  IC                      ! Cursor
*
*    Status :
*
      INTEGER                  STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

 10   IF ( IC .GT. LEN(STR) ) THEN
	CALL MSG_SETC( 'REASON', 'Missing '//X )
	STATUS = SAI__ERROR
      ELSE
	IC = IC + 1
	IF ( STR(IC:IC) .LE. ' ' ) GOTO 10
      END IF

      END
