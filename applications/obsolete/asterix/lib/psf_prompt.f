*+  PSF_PROMPT - Handles the 'PSF' parameter for the PSF system
      SUBROUTINE PSF_PROMPT( USEDEFAULT, DEFAULT, PSID, STATUS )
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
*     31 Aug 1989 (DJA):
*        Original version
*     25 Oct 1992 (DJA):
*        Added channel spectrum model option
*     23 Feb 1994 (DJA):
*        Doesn't display options if PSF parameter set
*      8 May 1996 (DJA):
*        Store everything in PSID which replaces old slot
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
      INCLUDE 'PAR_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      LOGICAL                   USEDEFAULT              ! Use default psf
      CHARACTER*(*)             DEFAULT                 ! The default psf
      INTEGER                   PSID                    ! Psf slot
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

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Automatic mode?
      IF ( PSFAUTO ) THEN

*    Just take default
        UPSF = DEFAULT

*    Locate interesting bit of string
        CALL CHR_FANDL( UPSF, BEG, END )

*    Parse model
	CALL PSF_MODEL_PARSE( UPSF(BEG:END), PSID, STATUS )

      ELSE

*      Set the default if required
        IF ( USEDEFAULT ) THEN
	  CALL USI_DEF0C( PAR, DEFAULT, STATUS )
	  STATUS = SAI__OK
        ELSE
	  CALL USI_DEF0C( PAR, 'ANALYTIC', STATUS )
        END IF

*      Display PSFs first time through
        CALL USI_STATE( PAR, STATE, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( STATE .NE. PAR__ACTIVE ) THEN
          CALL PSF_DISPLAY( STATUS )
        END IF

*      Ask user to choose a PSF
 59     CALL USI_GET0C( PAR, UPSF, STATUS )
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
	  CALL PSF_MODEL_PARSE( UPSF(BEG:END), PSID, STATUS )

        END IF

      END IF

*  Calling routine will report status
 99   CONTINUE

      END



*+  PSF_MODEL_PARSE - Parse a psf model specification
      SUBROUTINE PSF_MODEL_PARSE( STR, PSID, STATUS )
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
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      CHARACTER*(*)            STR                     ! The model spec
      INTEGER                  PSID                    ! Psf slot
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
      INTEGER                   BPOS                    ! Character pointer
      INTEGER                   IC                      ! Character pointer
      INTEGER			MODE			! Energy mode
      INTEGER                   NBIN                    ! # model energy bins
      INTEGER                   NB_B, NB_E              ! nbin bit
      INTEGER                   S_B, S_E                ! Spatial bit
      INTEGER                   SP_B, SP_E              ! Spectral bit

      LOGICAL                   AT_END                  ! End of parse?
      LOGICAL			EMOK			! Modelling ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Channel model option
      IF ( CHR_SIMLR( STR(1:5), 'CSPEC' ) .OR.
     :     CHR_SIMLR( STR(1:5), 'MSPEC' ) ) THEN

*      Channel spectrum option
	IF ( (STR(1:1).EQ.'C') .OR. (STR(1:1).EQ.'c') ) THEN
	  MODE = PSF_E_CHANNEL

*      Energy model
	ELSE
	  MODE = PSF_E_MODEL

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
	CALL PSF_PAR_SPATIAL( STR(S_B:S_E), PSID, STATUS )
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
        CALL PSF_PAR_ANSPEC( STR(SP_B:SP_E), NBIN, PSID, STATUS )
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
        EMOK = (STATUS.EQ.SAI__OK)

*  No spectral component
      ELSE

*    No energy model
	MODE = PSF_E_NONE
	EMOK = .FALSE.

*    Parse spatial psf
	CALL PSF_PAR_SPATIAL( STR, PSID, STATUS )

      END IF

*  Store energy model settings
      CALL ADI_CPUT0L( PSID, 'IsEnergyModel', EMOK, STATUS )
      CALL ADI_CPUT0I( PSID, 'NemMode', MODE, STATUS )

*  Set model flag if ok
 99   IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_PRNT( '! ^REASON in psf energy model specification' )
	CALL AST_REXIT( 'PSF_MODEL_PARSE', STATUS )
      END IF

      END


*+  PSF_PAR_ANSPEC - Analyse a model
      SUBROUTINE PSF_PAR_ANSPEC( SPEC, NBIN, PSID, STATUS )
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER*(*)            SPEC                    ! The channel spectrum
      INTEGER                  PSID                    ! Psf slot
      INTEGER                  NBIN                    ! # channel bins
*
*    Status :
*
      INTEGER                  STATUS
*
*    Local variables :
*
      INTEGER                   APTR                    ! Axis pointer
      INTEGER                   CBPTR                   ! Channel bounds ptr
      INTEGER                   NDIM, DIMS(1)        	! Spectrum dimensions
      INTEGER                   DPTR                    ! Data pointer
      INTEGER                   NSBIN                   ! # spectral bins
      INTEGER                   QPTR                    ! Quality pointer
      INTEGER			SFID			! Spectrum identifier
      INTEGER                  	WPTR                    ! Axis widths ptr

      LOGICAL                   OK                      ! Spectrum ok?
      LOGICAL                   QOK                     ! Spectrum quality ok?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to open file
      CALL ADI_FOPEN( SPEC, 'BinDS', 'READ', SFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'SP', SPEC )
        CALL ERR_REP( ' ', 'Unable to open spectrum /^SP/', STATUS )
        GOTO 99
      END IF

*  Check dimensions and data
      CALL BDI_CHK( SFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( SFID, 1, DIMS, NDIM, STATUS )
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

*  Quality present?
      CALL BDI_CHK( SFID, 'Quality', QOK, STATUS )

*  Map input arrays
      CALL BDI_AXMAPR( SFID, 1, 'Data', 'READ', APTR, STATUS )
      CALL BDI_AXMAPR( SFID, 1, 'Width', 'READ', WPTR, STATUS )
      CALL BDI_MAPR( SFID, 'Data', 'READ', DPTR, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPL( SFID, 'LogicalQuality', 'READ', QPTR, STATUS )
      END IF

*  Map space for channel bounds
      CALL ADI_CPUT0I( PSID, 'NemBin', NBIN, STATUS )
      CALL DYN_MAPI( 1, NBIN, CBPTR, STATUS )
      CALL ADI_CPUT0I( PSID, 'NemBnds', CBPTR, STATUS )

*  Determine model bin centres
      CALL PSF_PAR_ANSPEC_INT( NSBIN, %VAL(APTR), %VAL(WPTR),
     :                         %VAL(DPTR), QOK, %VAL(QPTR), NBIN,
     :                         %VAL(CBPTR), STATUS )

*  Close spectrum
      CALL ADI_FCLOSE( SFID, STATUS )

*  Tidy up
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
      SUBROUTINE PSF_PAR_SPATIAL( STR, PSID, STATUS )
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
      INCLUDE 'PSF_PAR'
*
*    Import :
*
      CHARACTER*(*)            STR                     ! The model spec
      INTEGER                  PSID			! Psf  slot
*
*    Status :
*
      INTEGER                  STATUS
*
*    Function definitions :
*
      LOGICAL                   CHR_ISALF, CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*40              RSN
      CHARACTER*15		TAG

      REAL			DR, DX, DY		! Model bin sizes
      REAL			RUP(PGRID_MAXR)

      INTEGER                   BEG, IC                 ! Character pointer
      INTEGER                   IR                      ! Loop over r bins
      INTEGER                   N1, N2                  ! More pointers
      INTEGER			NA, NR			! Model sizes
      INTEGER			SMTYPE			! Model type

      LOGICAL                   MORE                    ! More numbers?
      LOGICAL			PREG			! Regular polar model?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Which model is being parsed
      IF ( CHR_SIMLR(STR(1:5),'POLAR') ) THEN
	SMTYPE = PSF_PGRID
        PREG = .FALSE.

      ELSE IF ( CHR_SIMLR(STR(1:4),'RECT') ) THEN
	SMTYPE = PSF_RGRID

*    Simple psf
      ELSE

*      Look for named psf
	CALL PSF_CHKLIBRTN( STR, TAG, STATUS )

*      Report error if PSF not found
	IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'P', STR )
	  CALL MSG_PRNT( 'Could not find PSF called /^P/' )
	  CALL ERR_ANNUL( STATUS )
	  GOTO 59
        ELSE
          CALL ADI_CPUT0C( PSID, 'Tag', TAG, STATUS )
          GOTO 99
	END IF

      END IF

*    Locate bracket
      IC = INDEX( STR, '(' ) + 1

*  Next item is the psf name
      CALL CHR_FIWS( STR, IC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_SETC( 'REASON', 'Missing psf name' )
	GOTO 99
      END IF

*  Find end of psf name, and look for psf in tables
      BEG = IC
      DO WHILE ( CHR_ISALF(STR(IC:IC)) .OR. (STR(IC:IC).EQ.'_') )
	IC = IC + 1
      END DO
      IC = IC - 1
      CALL PSF_CHKLIBRTN( STR(BEG:IC), TAG, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        RSN = 'Unknown psf /'//STR(BEG:IC)//'/'
        CALL MSG_SETC( 'REASON', RSN )
	GOTO 99
      ELSE
        CALL ADI_CPUT0C( PSID, 'Tag', TAG, STATUS )
      END IF

*  Locate comma
      IC = IC + 1
      IF ( STR(IC:IC) .NE. ',' ) THEN
	CALL MSG_SETC( 'REASON', 'Comma expected' )
	GOTO 99
      END IF
      IC = IC + 1

*    Radial/x-axis specification
      CALL CHR_FIWS( STR, IC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	IF ( SMTYPE .EQ. PSF_PGRID ) THEN
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
      IF ( SMTYPE .EQ. PSF_PGRID ) THEN
	IF ( INDEX( STR(BEG:IC), ':' ) .EQ. 0 ) THEN
	  CALL CHR_CTOR( STR(BEG:IC), DR, STATUS )
	  IF ( STATUS .EQ. SAI__OK ) THEN
            CALL ADI_CPUT0L( PSID, 'ModelReg', .TRUE., STATUS )
            PREG = .TRUE.
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
	    CALL CHR_CTOR( STR(N1:N2-1), RUP(IR), STATUS )
	    IF ( STATUS .NE. SAI__OK ) THEN
	      CALL MSG_SETC( 'REASON', 'Invalid radial bin '/
     :                                     /'specification' )
	      GOTO 99
	    END IF

*          Radii must increase
	    IF ( IR .GT. 1 ) THEN
	      IF ( RUP(IR) .LE. RUP(IR-1) ) THEN
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
	  IF ( RUP(1) .NE. 0.0 ) THEN
	    NR = IR + 1
	    DO IR = NR-1, 1, -1
	      RUP(IR+1) = RUP(IR)
	    END DO
	    RUP(1) = 0.0
	  ELSE
	    NR = IR
	  END IF

*        Now terminate with a large radius (~ infinity)
	  NR = NR + 1
	  RUP(NR) = 1.0E10

*        Finally, square all the radii
          CALL ARR_SQR1R( RUP, NR, STATUS )

	END IF
	NA = 1

      ELSE
	CALL CHR_CTOR( STR(BEG:IC), DX, STATUS )
        DY = DX

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
      IF ( SMTYPE .EQ. PSF_PGRID ) THEN
	CALL CHR_CTOI( STR(BEG:IC), NA, STATUS )
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL MSG_SETC( 'REASON', 'Invalid azimuthal bin '/
     :                                    /'specification' )
	  GOTO 99
	END IF
      ELSE
	CALL CHR_CTOR( STR(BEG:IC), DY, STATUS )
      END IF
      IC = IC + 1

*  Get bracket
 59   IF ( STR(IC:IC) .NE. ')' ) THEN
	CALL MSG_SETC( 'REASON', 'Right bracket expected' )
	STATUS = SAI__ERROR
      END IF

*  Model ok?
 89   CALL ADI_CPUT0L( PSID, 'IsModel', (STATUS.EQ.SAI__OK),
     :                 STATUS )
      CALL ADI_CPUT0I( PSID, 'ModelType', SMTYPE, STATUS )
      IF ( SMTYPE .EQ. PSF_PGRID ) THEN
        CALL ADI_CPUT0I( PSID, 'ModelNr', NR, STATUS )
        CALL ADI_CPUT0I( PSID, 'ModelNaz', NA, STATUS )
        CALL ADI_CPUT0I( PSID, 'ModelReg', PREG, STATUS )
        IF ( PREG ) THEN
          CALL ADI_CPUT0R( PSID, 'ModelDr', DR, STATUS )
        ELSE
          CALL ADI_CPUT1R( PSID, 'ModelRup', NR, RUP, STATUS )
        END IF
      ELSE
        CALL ADI_CPUT0R( PSID, 'ModelDx', DX, STATUS )
        CALL ADI_CPUT0R( PSID, 'ModelDy', DY, STATUS )
      END IF

*  Set model flag if ok
 99   IF ( STATUS .NE. SAI__OK ) THEN
	CALL MSG_PRNT( '! ^REASON in model specification' )
	CALL AST_REXIT( 'PSF_MODEL_PARSE', STATUS )
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
*
*    Local Variables:
*
      CHARACTER*80		TXT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

 10   IF ( IC .GT. LEN(STR) ) THEN
        TXT = 'Missing '
        TXT(9:) = X
	CALL MSG_SETC( 'REASON', TXT )
	STATUS = SAI__ERROR
      ELSE
	IC = IC + 1
	IF ( STR(IC:IC) .LE. ' ' ) GOTO 10
      END IF

      END
