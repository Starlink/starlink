*+  PSS_DIAG_GETSEL - Get diagnostic mode selections
      SUBROUTINE PSS_DIAG_GETSEL( STATUS )
*
*    Description :
*
*    History :
*
*      3 Feb 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER  STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
*
*    Local variables :
*
      CHARACTER*20             DSEL                        ! Diag selection
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( '  Select from diagnostic options below :' )
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( '   A - Chi-squared vs. flux per source' )
        CALL MSG_PRNT( '   B - Chi-squared vs. flux and background '/
     :                 /'per source' )
        CALL MSG_PRNT( '   C - Significance vs. convolution radius '/
     :                 /'per source' )
        CALL MSG_PRNT( '   D - Output background rescale map' )
        CALL MSG_PRNT( '   E - P(flux) profile per source' )
        CALL MSG_PRNT( '   F - P(x,y,flux) cube per source' )
        CALL MSG_PRNT( '   G - Free flux fit, allow -ve if required' )
        CALL MSG_PRNT( '   K - Poisson probability per source in '/
     :                 /'output list' )
        CALL MSG_PRNT( '   L - Edit the source list' )
        CALL MSG_PRNT( '   M - Maximum no sources after 1st pass '/
     :                 /'filter' )
        CALL MSG_PRNT( '   P - Chi-squared vs. X and Y per source' )
        CALL MSG_PRNT( '   Q - Re-optimised flux vs. X and Y per'/
     :                 /' source' )
        CALL MSG_PRNT( '   R - Constrain allowed source box' )
        CALL MSG_PRNT( '   T - Chi-squared vs. X per source' )
        CALL MSG_PRNT( '   U - Re-optimised flux vs. X per source' )
        CALL MSG_PRNT( '   V - Validate input image and background' )
        CALL MSG_PRNT( ' ' )
        CALL MSG_PRNT( '  eg. Enter AB to select first two' )
        CALL MSG_PRNT( ' ' )
        CALL PAR_GET0C( 'DIAGOPT', DSEL, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        CALL CHR_UCASE( DSEL )
        DI_CHI_V_FLUX = (INDEX(DSEL,'A').NE.0)
        DI_CHI_V_F_B = (INDEX(DSEL,'B').NE.0)
        DI_SIG_V_CRAD = (INDEX(DSEL,'C').NE.0)
        DI_RESC_MAP = (INDEX(DSEL,'D').NE.0)
        DI_VALIDATE = (INDEX(DSEL,'V').NE.0)
        DI_P_S_PROFILE = (INDEX(DSEL,'E').NE.0)
        DI_P_XYS_CUBE = (INDEX(DSEL,'F').NE.0)
        DI_FREE_FIT = (INDEX(DSEL,'G').NE.0)
        DI_POISS_PROB = (INDEX(DSEL,'K').NE.0)
        DI_LIST_EDIT = (INDEX(DSEL,'L').NE.0)
        DI_MAX_FILTER = (INDEX(DSEL,'M').NE.0)
        DI_CHI_V_XY = (INDEX(DSEL,'P').NE.0)
        DI_FLUX_V_XY = (INDEX(DSEL,'Q').NE.0)
        DI_CHI_V_X = (INDEX(DSEL,'T').NE.0)
        DI_FLUX_V_X = (INDEX(DSEL,'U').NE.0)
        IF ( .NOT. ( DI_SIG_V_CRAD .OR.
     :               DI_VALIDATE .OR. DI_RESC_MAP .OR.
     :               DI_CHI_V_F_B .OR. DI_P_S_PROFILE .OR.
     :               DI_P_XYS_CUBE .OR. DI_FREE_FIT .OR.
     :               DI_POISS_PROB .OR. DI_LIST_EDIT .OR.
     :               DI_MAX_FILTER .OR. DI_CHI_V_XY .OR.
     :               DI_FLUX_V_XY .OR. DI_CHI_V_X .OR.
     :               DI_FLUX_V_X .OR.
     :               DI_CHI_V_FLUX ) ) THEN
          CALL MSG_PRNT( 'No selections made - only diagnostic '/
     :        /'parameters will be output.' )
        END IF
        DI_ON = .TRUE.
      END IF

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_DIAG_GETSEL', STATUS )
      END IF

      END
*+  PSS_DIAG_OPEN_RMAP - Open a rescaling map file
      SUBROUTINE PSS_DIAG_OPEN_RMAP( RMAP, STATUS )
*
*    Description :
*
*     Opens the background rescale factor map. This involves writing the
*     IMPORT compatible header.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     29 Jul 91 : Original (DJA)
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
      INTEGER STATUS
*
*    Export :
*
      INTEGER                  RMAP                    ! Unit to be written
*
*    Local constants :
*
      CHARACTER*11             NAME
        PARAMETER              ( NAME = 'RESCALE_MAP' )
*
*    Local variables :
*
      CHARACTER*80             TEXT                    ! Output buffer

      INTEGER                  AX                      ! Loop over axes
      INTEGER                  FSTAT                   ! I/O status
      INTEGER                  TLEN                    ! TEXT length
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Grab logical unit
      CALL FIO_GUNIT( RMAP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Open file
      OPEN( UNIT=RMAP, FILE=NAME, STATUS='NEW',
     :      CARRIAGECONTROL='LIST', IOSTAT=FSTAT )
      IF ( FSTAT .EQ. 0 ) THEN
        WRITE( RMAP, * )
     :     'TOPLEVEL TYPE "BINDS" TITLE "Background rescale factor"'
 10     FORMAT ( 1X, 5A )
        DO AX = 1, 2
          CALL MSG_SETC( 'LABEL', AX_LABEL(AX) )
          CALL MSG_SETC( 'UNITS', AX_UNITS(AX) )
          CALL MSG_MAKE( 'AXIS LABEL "^LABEL" UNITS "^UNITS"',
     :                                            TEXT, TLEN )
          WRITE( RMAP, * ) TEXT(:TLEN)
        END DO
        WRITE( RMAP, * ) 'DATA'

*      Write message
        CALL PSS_OP( 'INFO', '<DIAG> An IMPORT readable text file'/
     :               /' containing a map of background rescaling' )
        CALL PSS_OP( 'INFO', '       factors will be written to '/
     :                                    /'logical file '//NAME )

      ELSE
        CALL FIO_SERR( FSTAT, STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_DIAG_OPEN_RMAP', STATUS )
      END IF

      END
