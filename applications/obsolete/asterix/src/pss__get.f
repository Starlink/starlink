*+  PSS_GET_AXES - Get axis data about input dataset from psf system
      SUBROUTINE PSS_GET_AXES( STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     28 Jun 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
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
      INTEGER                  DIM                     ! Unused argument
      INTEGER                  I                       ! Loop over axes
      INTEGER		       X_AX, Y_AX, E_AX, T_AX  ! Axis identifiers
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Primitive input?
      IF ( IM_PRIM ) THEN

*      Issue warning...
        CALL MSG_PRNT( 'Primitive input data. Assuming axes regula'/
     :                                /'rly spaced and same scale' )

*      Check not cubic or higher
        IF ( BDS_NDIM .GT. 2 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Cannot handle cubic or hypercubic '//
     :                   'primitive data. Project to 2D.', STATUS )
        END IF

      END IF

*    Get axis id's from psf system
      CALL PSF_QAXES( PSF_HAN, X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Retrieve information about spatial axes
      IF ( STATUS .EQ. SAI__OK ) THEN
        DO I = X_AX, Y_AX, (Y_AX-X_AX)
          CALL PSF_QAXIS( PSF_HAN, I, DIM, AX_REG(I), AX_PTR(I),
     :                    AX_BR(I), AX_DR(I), AX_LABEL(I),
     :                    AX_UNITS(I), AX_TOR(I), STATUS )
        END DO
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_GET_AXES', STATUS )
      END IF

      END

*+  PSS_GET_BVAL - Get a value from the background model
      SUBROUTINE PSS_GET_BVAL( X, Y, BVAL, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Sep 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
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
*    Import :
*
      INTEGER                  X, Y                    ! Pixel wanted
*
*    Export :
*
      REAL                     BVAL                    ! Background value
*
*    Function definitions :
*
      REAL                     PSS_GET_BVAL_INT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Call internal routine
      BVAL = PSS_GET_BVAL_INT( BDS_DIMS(1), BDS_DIMS(2),
     :                   X, Y, %VAL(BG_DATA_PTR) )

      END

*+  PSS_GET_BVAL_INT - Get a value from the background model
      REAL FUNCTION PSS_GET_BVAL_INT( NX, NY, X, Y, BGND )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Jun 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                  NX, NY                  ! Bgnd dimensions
      INTEGER                  X, Y                    ! Pixel wanted
      REAL                     BGND(NX,NY)             ! Bgnd data
*-

      PSS_GET_BVAL_INT = BGND(X,Y)

      END

*+  PSS_GET_CONF - Get confidence levels
      SUBROUTINE PSS_GET_CONF( PARAM, MAXLEV, TEXT, LEVS, ACTLEV,
     :                                                   STATUS )
*
*    Description :
*
*     Gets a list of confidence levels. These may be entered as
*
*      a) Real number percentages, optionally followed by %
*      b) Real number followed by s*igma
*
*     Multiple levels can be specified : the separator can be either commas
*     or spaces
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     22 Feb 91 : Original (DJA)
*     30 Apr 93 : Let ! or -ve value denote no errors to be produced (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*
*    Import :
*
      CHARACTER*(*)              PARAM                ! Parameter name
      INTEGER                    MAXLEV               ! Size of LEVS array
*
*    Export :
*
      CHARACTER*(*)              TEXT                 ! The user response
      DOUBLE PRECISION           LEVS(MAXLEV)         ! Confidence levels
      INTEGER                    ACTLEV               ! Number of levels
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      DOUBLE PRECISION           S15ABF
      LOGICAL                    STR_ABBREV
*
*    Local constants :
*
      CHARACTER                  COMMA, TERM
         PARAMETER               ( COMMA=',', TERM = '$' )
*
*    Local variables :
*
      DOUBLE PRECISION           VAL                  ! Real value

      INTEGER                    BEG,I                ! Character pointers
      INTEGER                    ISTAT                ! Local status
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get string from user and capitalise
 10   CALL USI_GET0C( PARAM, TEXT, STATUS )

*    Trap the ! case
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        ACTLEV = -1
        LEVS(1) = -1.0
        GOTO 99
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      END IF
      CALL USI_CANCL( PARAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Insert marker at end of string
      TEXT = TEXT(:LEN(TEXT)-1)//'$'

*    Loop over string
      I = 1
      ACTLEV = 0
      ISTAT = SAI__OK
      DO WHILE ( ( ISTAT .EQ. SAI__OK ) .AND. (TEXT(I:I) .NE. TERM)
     :                                .AND. ( ACTLEV .LT. MAXLEV ) )

*      Skip white space
        CALL CHR_FIWS( TEXT, I, ISTAT )

*      Stop if terminator reached
        IF ( TEXT(I:I) .NE. TERM ) THEN

*        Locate next character after what we hope will be a number
          BEG = I
          CALL CHR_FIWE( TEXT, I, ISTAT )
          IF ( INDEX('Ss%',TEXT(I:I)) .NE. 0 ) I = I - 1

*        Try to get real number
          CALL CHR_CTOD( TEXT(BEG:I), VAL, ISTAT )
          IF ( ISTAT .NE. 0 ) THEN
            CALL MSG_PRNT( '! Error reading real number' )
            GOTO 10
          END IF
          I = I + 1

*        Look for -ve on first time through
          IF ( VAL .LT. 0.0 ) THEN
            IF ( ACTLEV .EQ. 0 ) THEN
              ACTLEV = -1
              LEVS(1) = -1.0
            ELSE
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Negative error confidence can only'/
     :        /' be used as 1st item to denote "no errors".', STATUS )
            END IF
            GOTO 99
          END IF

*        Skip white space
          CALL CHR_FIWS( TEXT, I, ISTAT )

*        Has "sigma" been specified
          IF ( INDEX('Ss',TEXT(I:I)) .NE. 0 ) THEN
            BEG = I
            CALL CHR_FIWE( TEXT, I, ISTAT )
            IF ( STR_ABBREV( TEXT(BEG:I), 'SIGMAS')  ) THEN
              ACTLEV = ACTLEV + 1
              LEVS(ACTLEV) = 100.0d0*(2.0D0*S15ABF(VAL,ISTAT) - 1.0D0)
            ELSE
              CALL MSG_PRNT( '! Error reading confidence levels' )
              GOTO 10
            END IF
            I = I + 1

*          Skip white space
            CALL CHR_FIWS( TEXT, I, ISTAT )

          ELSE

*          Skip optional % sign
            IF ( TEXT(I:I) .EQ. '%' ) I = I + 1

*          Store number as percentage confidence
            ACTLEV = ACTLEV + 1
            LEVS(ACTLEV) = VAL

          END IF

*        If comma then continue
          IF ( TEXT(I:I) .EQ. COMMA ) I = I + 1

          IF ( ISTAT .NE. SAI__OK ) THEN
            CALL MSG_PRNT( '! Error reading confidence levels' )
            GOTO 10
          END IF

        END IF

      END DO

*    Strip marker off end
      TEXT = TEXT(:LEN(TEXT)-1)

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_GET_CONF', STATUS )
      END IF

      END

*+  PSS_GET_MODE - Get source processing mode
      SUBROUTINE PSS_GET_MODE( MPAR, STATUS )
*
*    Description :
*
*     Gets the source processing mode and set up PSS common block logicals.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     27 Feb 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      CHARACTER*(*)            MPAR                ! Parameter to use
*
*    Functions :
*
      LOGICAL                  CHR_SIMLR           ! Case insensitive check
*
*    Local constants :
*
      INTEGER                  NMODE               ! Number of supported modes
        PARAMETER              ( NMODE = 6 )
*
*    Local variables :
*
      CHARACTER*10             MODE                ! Processing mode
      CHARACTER*6              MODES(NMODE)        ! Processing mode names

      INTEGER                  IMODE               ! Loop over modes
      INTEGER                  IMODES(NMODE)       ! Processing mode codes

      LOGICAL                  FOUND               ! Found the named mode?
*
*    Local data :
*
      DATA                     MODES/'SEARCH','PARAM','UPLIM','UPMAP',
     :                               'SENMAP','SIGERR'/
      DATA                     IMODES/PSS__M_SEARCH,PSS__M_PARAM,
     :                                PSS__M_UPLIM, PSS__M_UPMAP,
     :                                PSS__M_SENMAP,PSS__M_SIGVAR/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get processing mode
      CALL USI_GET0C( MPAR, MODE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Look up in list
        FOUND = .FALSE.
        IMODE = 1
        DO WHILE ( (IMODE.LE.NMODE) .AND. .NOT. FOUND )
          IF ( CHR_SIMLR(MODE,MODES(IMODE)) ) THEN
            FOUND = .TRUE.
            CP_MODE = IMODES(IMODE)
          ELSE
            IMODE = IMODE + 1
          END IF
        END DO

*      In list?
        IF ( FOUND ) THEN
          CP_SPOT = ((CP_MODE.EQ.PSS__M_UPLIM).OR.
     :                (CP_MODE.EQ.PSS__M_PARAM))
          CP_OPT  = ((CP_MODE.EQ.PSS__M_SEARCH).OR.
     :                (CP_MODE.EQ.PSS__M_PARAM).OR.
     :                (CP_MODE.EQ.PSS__M_SIGVAR))
        ELSE
          CALL MSG_SETC( 'MODE', MODE )
          CALL MSG_PRNT( 'Unrecognised processing mode /^MODE/' )
        END IF

      END IF

      END

*+  PSS_GET_SUBSET - Define the slice of the image to be used
      SUBROUTINE PSS_GET_SUBSET( STATUS )
*
*    Description :
*
*     The section of the image to be considered is user selectable in UPMAP
*     and SEARCH modes - the whole image is the default in the spot modes.
*     After the bounds of the image are known in image coordinates, these
*     values are also found in radians and pixels.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Jul 89 : Original (DJA)
*     18 Apr 93 : Better checking of user supplied slice (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
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
*    Functions definitions :
*
      INTEGER                  PIX
      REAL                     DAT, AXV
*
*    Local variables :
*
      REAL                     LO(2), HI(2)            ! Image bounds
      REAL                     RNGS(2,PSS__MXDIM)      ! User supplied ranges

      INTEGER                  IAX                     ! Loop over axes
      INTEGER                  J                       ! Loop over ranges
      INTEGER                  NRNGS(PSS__MXDIM)       ! # ranges per dimension

      LOGICAL                  OK                      ! Supplied values ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get box of interest
      DO IAX = 1, 2
        LO(IAX) = AXV(IAX,DAT(IAX,1))
        HI(IAX) = AXV(IAX,DAT(IAX,BDS_DIMS(IAX)))
      END DO

*    If spot mode accept default automatically
      IF ( CP_SPOT ) THEN

        CALL USI_DEF0C( 'SLICE', '*:*,*:*', STATUS )

      ELSE

*      Display ranges
        DO IAX = 1, 2
          CALL MSG_SETR( 'LO', LO(IAX) )
          CALL MSG_SETR( 'HI', HI(IAX) )
          CALL MSG_SETC( 'LAB', AX_LABEL(IAX) )
          CALL MSG_SETC( 'UNI', AX_UNITS(IAX) )
          CALL MSG_PRNT( '^LAB axis range is from ^LO to ^HI ^UNI' )
        END DO

*      Change prompt for upper limit mapping
        IF ( CP_MODE .EQ. PSS__M_UPMAP ) THEN
          CALL USI_PROMT( 'SLICE', 'Slice for upper limit mapping',
     :                                                     STATUS )
        ELSE IF ( CP_MODE .EQ. PSS__M_SENMAP ) THEN
          CALL USI_PROMT( 'SLICE', 'Slice for sensitivity mapping',
     :                                                     STATUS )
        END IF

      END IF

*    Get ranges
 20   CALL PRS_GETRANGES( 'SLICE', 2, 2, LO, HI, RNGS, NRNGS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Convert selected range to radians
      DO IAX = 1, 2

*      Check NRNGS
        IF ( NRNGS(IAX) .NE. 1 ) THEN
          CALL MSG_PRNT( '! Error in range specification' )
          STATUS = SAI__ERROR
          GOTO 99
        END IF

*      Check that coordinates are increasing or descreasing correctly
        OK = ( (RNGS(2,IAX)-RNGS(1,IAX))*SIGN(1.0,AX_DR(IAX))
     :                                                 .GT. 0.0 )
        IF ( .NOT. OK ) THEN
          CALL USI_CANCL( 'SLICE', STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'A', IAX )
            CALL MSG_PRNT( 'Axis ^A ranges are inverted - re-enter' )
            GOTO 20
          ELSE
            GOTO 99
          END IF
        END IF

*      Convert spatial ranges to radians and round to pixel centre. Check
*      that slice doesn't exceed bounds of image.
        IF ( IAX .LE. 2 ) THEN
          DO J = 1, 2
            RNGS(J,IAX) = RNGS(J,IAX)*AX_TOR(IAX)
            RNGS(J,IAX) = DAT(IAX,PIX(IAX,RNGS(J,IAX)))
            BDS_SPOF(J,IAX) = PIX(IAX,RNGS(J,IAX))
          END DO
          IF ( (BDS_SPOF(1,IAX).LT.1) .OR.
     :         (BDS_SPOF(1,IAX).GT.BDS_DIMS(IAX)) ) THEN
            IF ( IAX .EQ. 1 ) THEN
              CALL MSG_SETC( 'AX', 'X' )
            ELSE
              CALL MSG_SETC( 'AX', 'Y' )
            END IF
            CALL MSG_PRNT( 'Truncating ^AX axis slice at image bounds' )
            BDS_SPOF(1,IAX) = MAX(1,BDS_SPOF(1,IAX))
            BDS_SPOF(2,IAX) = MIN(BDS_DIMS(IAX),BDS_SPOF(2,IAX))
          END IF

*        Set the slice centre in radians
          BDS_SCEN(IAX) = DAT(IAX,BDS_SPOF(2,IAX)) -
     :                    DAT(IAX,BDS_SPOF(1,IAX))

        END IF

      END DO

*    Define extrema of image accessed by PSS
      DO IAX = 1, 2
        BDS_EXTREMA(1,IAX) = MAX( 1, BDS_SPOF(1,IAX)
     :                       - PSF_PPS(PSF_NIPOS) - 3 )
        BDS_EXTREMA(2,IAX) = MIN( BDS_DIMS(IAX), BDS_SPOF(2,IAX)
     :                       + PSF_PPS(PSF_NIPOS) + 3 )
      END DO

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_GET_SUBSET', STATUS )
      END IF

      END
