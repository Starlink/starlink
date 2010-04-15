*+  P4_CHECK_INPUT - Expands an input filename
      SUBROUTINE P4_CHECK_INPUT( INPUT, STATUS )
*    Description :
*     This routine checks the input filename for a directory specification.
*    Invocation :
*     CALL P4_CHECK_INPUT( INPUT, STATUS )
*    Parameters :
*     INPUT  =  CHARACTER( UPDATE )
*       The input filename to be parsed.
*     STATUS =  INTEGER( UPDATE )
*       The returned globl ADAM status.
*    Bugs :
*     None known.
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     30-Nov-1994: Original Unix version.                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'P4COM.INC'
*    Import-Export :
      CHARACTER*(*) INPUT             ! The input filename
*    Status :
      INTEGER STATUS                  ! Global status
*    External references :
      INTEGER CHR_LEN                 ! Character length finding function
*    Local variables :
      CHARACTER*80 ENVAR              ! Environmental variable
      INTEGER SEPPOS                  ! File separator position
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check for a known directories and expand
      CALL CHR_RMBLK( INPUT )
      SEPPOS = INDEX( INPUT, SEPARATOR )
      IF ( INDEX( INPUT, '~' ) .GT. 0 ) THEN
        INPUT = P4_HOME(1:CHR_LEN(P4_HOME)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'HOME' ) .GT. 0 ) THEN
        INPUT = P4_HOME(1:CHR_LEN(P4_HOME)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'sys$login' ) .GT. 0 ) THEN
        INPUT = P4_HOME(1:CHR_LEN(P4_HOME)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'SYS$LOGIN' ) .GT. 0 ) THEN
        INPUT = P4_HOME(1:CHR_LEN(P4_HOME)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'p4_ct' ) .GT. 0 ) THEN
        INPUT = P4_CT(1:CHR_LEN(P4_CT)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'P4_CT' ) .GT. 0 ) THEN
        INPUT = P4_CT(1:CHR_LEN(P4_CT)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'p4_config' ) .GT. 0 ) THEN
        INPUT = P4_CONFIG(1:CHR_LEN(P4_CONFIG)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'P4_CONFIG' ) .GT. 0 ) THEN
        INPUT = P4_CONFIG(1:CHR_LEN(P4_CONFIG)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RODIR' ) .GT. 0 ) THEN
        INPUT = RODIR(1:CHR_LEN(RODIR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RGDIR' ) .GT. 0 ) THEN
        INPUT = RGDIR(1:CHR_LEN(RGDIR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RIDIR' ) .GT. 0 ) THEN
        INPUT = RIDIR(1:CHR_LEN(RIDIR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'IDIR' ) .GT. 0 ) THEN
        INPUT = IDIR(1:CHR_LEN(IDIR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'ODIR' ) .GT. 0 ) THEN
        INPUT = ODIR(1:CHR_LEN(ODIR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))

*   Check for certain CGS4-like files
      ELSE IF ( INPUT(1:8) .EQ. 'rg'//P4_DATE(1:6) ) THEN
        INPUT = RGDIR(1:CHR_LEN(RGDIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RG'//P4_DATE(1:6) ) THEN
        INPUT = RGDIR(1:CHR_LEN(RGDIR)) // 'rg' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'st'//P4_DATE(1:6) ) THEN
        INPUT = RGDIR(1:CHR_LEN(RGDIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ST'//P4_DATE(1:6) ) THEN
        INPUT = RGDIR(1:CHR_LEN(RGDIR)) // 'st' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ca'//P4_DATE(1:6) ) THEN
        INPUT = RODIR(1:CHR_LEN(RODIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'CA'//P4_DATE(1:6) ) THEN
        INPUT = RODIR(1:CHR_LEN(RODIR)) // 'ca' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ro'//P4_DATE(1:6) ) THEN
        INPUT = RODIR(1:CHR_LEN(RODIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RO'//P4_DATE(1:6) ) THEN
        INPUT = RODIR(1:CHR_LEN(RODIR)) // 'ro' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'o'//P4_DATE(1:6) ) THEN
        INPUT = ODIR(1:CHR_LEN(ODIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'O'//P4_DATE(1:6) ) THEN
        INPUT = ODIR(1:CHR_LEN(ODIR)) // 'o' // INPUT(2:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'i'//P4_DATE(1:6) ) THEN
        INPUT = IDIR(1:CHR_LEN(IDIR)) // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'I'//P4_DATE(1:6) ) THEN
        INPUT = IDIR(1:CHR_LEN(IDIR)) // 'i' // INPUT(2:CHR_LEN(INPUT))

*   Check for a starting '/' indicating a true path
      ELSE IF ( INPUT(1:1) .EQ. '/' ) THEN
         CONTINUE

*   Translate the environmental variable for VMS
      ELSE
        CALL CHR_FILL( ' ', ENVAR )
        IF ( SYSNAME .EQ. 'VMS' ) THEN
          IF ( SEPPOS .GT. 1 ) THEN
            ENVAR = INPUT( 1 : SEPPOS-1 )
            CALL CHR_UCASE( ENVAR )
            CALL PSX_GETENV( ENVAR(1:CHR_LEN(ENVAR)), ENVAR, STATUS )
            INPUT = ENVAR(1:CHR_LEN(ENVAR)) // INPUT(SEPPOS+1:CHR_LEN(INPUT))
          END IF

*   Translate the environmental variable for Unix
        ELSE
          IF ( SEPPOS .GT. 3 ) THEN
            ENVAR = INPUT( INDEX(INPUT,'$')+1 : SEPPOS-1 )
            CALL CHR_UCASE( ENVAR )
            CALL PSX_GETENV( ENVAR(1:CHR_LEN(ENVAR)), ENVAR, STATUS )
            INPUT = ENVAR(1:CHR_LEN(ENVAR)) // '/' // INPUT(SEPPOS+1:CHR_LEN(INPUT))
          END IF
        END IF
      END IF

*   Remove any new blank characters
      CALL CHR_RMBLK( INPUT )

*   In verbose mode, state the output file
      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'INPUT', INPUT )
        CALL MSG_OUT( ' ', 'P4_CHECK_INPUT: File is ^INPUT', STATUS )
      END IF

*   Exit subroutine
      END
