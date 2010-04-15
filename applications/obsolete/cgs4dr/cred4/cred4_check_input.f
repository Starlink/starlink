*+  CRED4_CHECK_INPUT - Expands an input filename
      SUBROUTINE CRED4_CHECK_INPUT( INPUT, STATUS )
*    Description :
*     This routine checks the input filename for a directory specification.
*    Invocation :
*     CALL CRED4_CHECK_INPUT( INPUT, STATUS )
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
      INCLUDE 'CRED4COM.INC'
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
      SEPPOS = CHR_LEN( INPUT )
      CALL CHR_FIND( INPUT, SEPARATOR, .FALSE., SEPPOS )
      IF ( INDEX( INPUT, '~' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'HOME' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'sys$login' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'SYS$LOGIN' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_ct' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_CT' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_CT' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_CT' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_eng' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_ENG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_ENG' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_ENG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_masks' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_MASKS' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_CT' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_MASKS' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_config' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_CONFIG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_CONFIG' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_CONFIG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_data' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_DATA' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_DATA' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'CGS4_DATA' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RODIR' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RGDIR' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RIDIR' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'RIDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'IDIR' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'IDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'ODIR' ) .GT. 0 ) THEN
        INPUT = PREFIX // 'ODIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))

*   Check for certain CGS4-like files
      ELSE IF ( INPUT(1:8) .EQ. 'rg'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RG'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // 'rg' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'st'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ST'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // 'st' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ca'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'CA'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RODIR' // SEPARATOR // 'ca' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ro'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RO'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'RODIR' // SEPARATOR // 'ro' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'o'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'ODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'O'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'ODIR' // SEPARATOR // 'o' // INPUT(2:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'i'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'IDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'I'//CGS4_DATE(1:6) ) THEN
        INPUT = PREFIX // 'IDIR' // SEPARATOR // 'i' // INPUT(2:CHR_LEN(INPUT))

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
        CALL MSG_OUT( ' ', 'CRED4_CHECK_INPUT: File is ^INPUT', STATUS )
      END IF

*   Exit subroutine
      END
