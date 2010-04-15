*+  RED4_CHECK_INPUT - Expands an input filename
      SUBROUTINE RED4_CHECK_INPUT( INPUT, STATUS )
*    Description :
*     This routine checks the input filename for a directory specification.
*    Invocation :
*     CALL RED4_CHECK_INPUT( INPUT, STATUS )
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
      INCLUDE 'RED4_COMMON.INC'
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
      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'INPUT', INPUT )
        CALL MSG_SETI( 'INLEN', LEN(INPUT) )
        CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input is ^INPUT of length ^INLEN', STATUS )
      ENDIF
      CALL CHR_RMBLK( INPUT )
      SEPPOS = CHR_LEN( INPUT )
      CALL CHR_FIND( INPUT, SEPARATOR, .FALSE., SEPPOS )
      IF ( INDEX( INPUT, '~' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in ~', STATUS )
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'HOME' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in HOME', STATUS )
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'sys$login' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in sys$login', STATUS )
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'SYS$LOGIN' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in SYS$LOGIN', STATUS )
        INPUT = PREFIX // 'HOME' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_ct' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in cgs4_ct', STATUS )
        INPUT = PREFIX // 'CGS4_CT' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_CT' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CGS4_CT', STATUS )
        INPUT = PREFIX // 'CGS4_CT' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_eng' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in cgs4_eng', STATUS )
        INPUT = PREFIX // 'CGS4_ENG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_ENG' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CGS4_ENG', STATUS )
        INPUT = PREFIX // 'CGS4_ENG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_masks' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in cgs4_masks', STATUS )
        INPUT = PREFIX // 'CGS4_MASKS' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_MASKS' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CGS4_MASKS', STATUS )
        INPUT = PREFIX // 'CGS4_MASKS' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_config' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in cgs4_config', STATUS )
        INPUT = PREFIX // 'CGS4_CONFIG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_CONFIG' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CGS4_CONFIG', STATUS )
        INPUT = PREFIX // 'CGS4_CONFIG' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'cgs4_data' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in cgs4_data', STATUS )
        INPUT = PREFIX // 'CGS4_DATA' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'CGS4_DATA' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CGS4_DATA', STATUS )
        INPUT = PREFIX // 'CGS4_DATA' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RODIR' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in RODIR', STATUS )
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RGDIR' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in RGDIR', STATUS )
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'RIDIR' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in RIDIR', STATUS )
        INPUT = PREFIX // 'RIDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'IDIR' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in IDIR', STATUS )
        INPUT = PREFIX // 'IDIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))
      ELSE IF ( INDEX( INPUT, 'ODIR' ) .GT. 0 ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in ODIR', STATUS )
        INPUT = PREFIX // 'ODIR' // SEPARATOR // INPUT(SEPPOS+1:CHR_LEN(INPUT))

*   Check for certain CGS4-like files
      ELSE IF ( INPUT(1:8) .EQ. 'rg'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in rg<DATE>', STATUS )
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RG'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in RG<DATE>', STATUS )
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // 'rg' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'st'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in st<DATE>', STATUS )
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ST'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in ST<DATE>', STATUS )
        INPUT = PREFIX // 'RGDIR' // SEPARATOR // 'st' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ca'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in ca<DATE>', STATUS )
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'CA'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in CA<DATE>', STATUS )
        INPUT = PREFIX // 'RODIR' // SEPARATOR // 'ca' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'ro'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in ro<DATE>', STATUS )
        INPUT = PREFIX // 'RODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:8) .EQ. 'RO'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in RO<DATE>', STATUS )
        INPUT = PREFIX // 'RODIR' // SEPARATOR // 'ro' // INPUT(3:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'o'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in o<DATE>', STATUS )
        INPUT = PREFIX // 'ODIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'O'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in O<DATE>', STATUS )
        INPUT = PREFIX // 'ODIR' // SEPARATOR // 'o' // INPUT(2:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'i'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in i<DATE>', STATUS )
        INPUT = PREFIX // 'IDIR' // SEPARATOR // INPUT(1:CHR_LEN(INPUT))
      ELSE IF ( INPUT(1:7) .EQ. 'I'//CGS4_DATE(1:6) ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in I<DATE>', STATUS )
        INPUT = PREFIX // 'IDIR' // SEPARATOR // 'i' // INPUT(2:CHR_LEN(INPUT))

*   Check for starting / indicating a true path
      ELSE IF ( INPUT(1:1) .EQ. '/' ) THEN
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in /<SOMEWHERE>', STATUS )
        CONTINUE

*   Translate the environmental variable for VMS
      ELSE
        IF (VERBOSE) CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Input was in <SOMEWHERE>', STATUS )
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
        CALL MSG_SETI( 'OUTLEN', CHR_LEN(INPUT) )
        CALL MSG_OUT( ' ', 'RED4_CHECK_INPUT: Output is ^INPUT of length ^OUTLEN', STATUS )
      END IF

*   Exit subroutine
      END
