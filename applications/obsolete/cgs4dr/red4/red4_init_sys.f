*+  RED4_INIT_SYS - Initialise the common block system values
      SUBROUTINE RED4_INIT_SYS( STATUS )
*    Invocation :
*     CALL RED4_INIT_SYS( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     29-Nov-1994: Original version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                        ! Global status
*    External functions :
      INTEGER CHR_LEN
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'             ! RED4 common block
      INCLUDE 'RED4_ENG.INC'                ! RED4 engineering common block
*    Local variables :
      CHARACTER*40 NODE                     ! Node name of machine
      CHARACTER*20 REL                      ! Release of op. system
      CHARACTER*20 VERSION                  ! Version of op. system
      CHARACTER*40 MACHINE                  ! Machine type
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise some engineering variables
      LOG_OPEN = .FALSE.

*   Get the type of operating system, set prefix and separator
      CALL PSX_UNAME( SYSNAME, NODE, REL, VERSION, MACHINE, STATUS )
      IF ( SYSNAME .EQ. 'VMS' ) THEN
        PREFIX = ' '
        SEPARATOR = ':'
      ELSE
        PREFIX = '$'
        SEPARATOR = '/'
      ENDIF

*    Get the translations of environmental variables
      CALL CHR_FILL( ' ', CGS4_HOME )
      CALL PSX_GETENV( 'HOME', CGS4_HOME, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_HOME = CGS4_HOME(1:CHR_LEN(CGS4_HOME)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_CT )
      CALL PSX_GETENV( 'CGS4_CT', CGS4_CT, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_CT = CGS4_CT(1:CHR_LEN(CGS4_CT)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_ENG )
      CALL PSX_GETENV( 'CGS4_ENG', CGS4_ENG, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_ENG = CGS4_ENG(1:CHR_LEN(CGS4_ENG)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_CONFIG )
      CALL PSX_GETENV( 'CGS4_CONFIG', CGS4_CONFIG, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_CONFIG = CGS4_CONFIG(1:CHR_LEN(CGS4_CONFIG)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_MASKS )
      CALL PSX_GETENV( 'CGS4_MASKS', CGS4_MASKS, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_MASKS = CGS4_MASKS(1:CHR_LEN(CGS4_MASKS)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_INDEX )
      CALL PSX_GETENV( 'CGS4_INDEX', CGS4_INDEX, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_INDEX = CGS4_INDEX(1:CHR_LEN(CGS4_INDEX)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_DATA )
      CALL PSX_GETENV( 'CGS4_DATA', CGS4_DATA, STATUS )
      IF ( SEPARATOR .EQ. '/' ) CGS4_DATA = CGS4_DATA(1:CHR_LEN(CGS4_DATA)) // SEPARATOR

      CALL CHR_FILL( ' ', CGS4_DATE )
      CALL PSX_GETENV( 'CGS4_DATE', CGS4_DATE, STATUS )
      IF ( CGS4_DATE(1:2) .EQ. '19' .OR. CGS4_DATE(1:2).EQ.'20' ) CGS4_DATE(1:2) = '  '
      CALL CHR_RMBLK( CGS4_DATE )

      CALL CHR_FILL( ' ', RGDIR )
      CALL PSX_GETENV( 'RGDIR', RGDIR, STATUS )
      IF ( SEPARATOR .EQ. '/' ) RGDIR = RGDIR(1:CHR_LEN(RGDIR)) // SEPARATOR

      CALL CHR_FILL( ' ', RODIR )
      CALL PSX_GETENV( 'RODIR', RODIR, STATUS )
      IF ( SEPARATOR .EQ. '/' ) RODIR = RODIR(1:CHR_LEN(RODIR)) // SEPARATOR

      CALL CHR_FILL( ' ', RIDIR )
      CALL PSX_GETENV( 'RIDIR', RIDIR, STATUS )
      IF ( SEPARATOR .EQ.  '/' ) RIDIR = RIDIR(1:CHR_LEN(RIDIR)) // SEPARATOR

      CALL CHR_FILL( ' ', ODIR )
      CALL PSX_GETENV( 'ODIR', ODIR, STATUS )
      IF ( SEPARATOR .EQ. '/' ) ODIR = ODIR(1:CHR_LEN(ODIR)) // SEPARATOR

      CALL CHR_FILL( ' ', IDIR )
      CALL PSX_GETENV( 'IDIR', IDIR, STATUS )
      IF ( SEPARATOR .EQ. '/' ) IDIR =IDIR(1:CHR_LEN(IDIR)) // SEPARATOR
      END
