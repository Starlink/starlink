*+  P4_INIT_SYS - Initialise the common block system values
      SUBROUTINE P4_INIT_SYS( STATUS )
*    Invocation :
*     CALL P4_INIT_SYS( STATUS )
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
      INCLUDE 'P4COM.INC'                   ! P4 common block
*    Local variables :
      CHARACTER*40 NODE                     ! Node name of machine
      CHARACTER*20 REL                      ! Release of op. system
      CHARACTER*20 VERSION                  ! Version of op. system
      CHARACTER*40 MACHINE                  ! Machine type
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

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
      CALL CHR_FILL( ' ', P4_HOME )
      CALL PSX_GETENV( 'HOME', P4_HOME, STATUS )
      IF ( SEPARATOR .EQ. '/' ) P4_HOME = P4_HOME(1:CHR_LEN(P4_HOME)) // SEPARATOR

      CALL CHR_FILL( ' ', P4_CT )
      CALL PSX_GETENV( 'P4_CT', P4_CT, STATUS )
      IF ( SEPARATOR .EQ. '/' ) P4_CT = P4_CT(1:CHR_LEN(P4_CT)) // SEPARATOR

      CALL CHR_FILL( ' ', P4_CONFIG )
      CALL PSX_GETENV( 'P4_CONFIG', P4_CONFIG, STATUS )
      IF ( SEPARATOR .EQ. '/' ) P4_CONFIG = P4_CONFIG(1:CHR_LEN(P4_CONFIG)) // SEPARATOR

      CALL CHR_FILL( ' ', P4_DATA )
      CALL PSX_GETENV( 'P4_DATA', P4_DATA, STATUS )
      IF ( SEPARATOR .EQ. '/' ) P4_DATA = P4_DATA(1:CHR_LEN(P4_DATA)) // SEPARATOR

      CALL CHR_FILL( ' ', P4_DATE )
      CALL PSX_GETENV( 'P4_DATE', P4_DATE, STATUS )
      IF ( P4_DATE(1:2) .EQ. '19' .OR. P4_DATE(1:2).EQ.'20' ) P4_DATE(1:2) = '  '
      CALL CHR_RMBLK( P4_DATE )

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
