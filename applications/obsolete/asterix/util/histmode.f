*+  HISTMODE - Set or or describe HISTORY mode for a dataset
      SUBROUTINE HISTMODE( STATUS )
*
*    Description :
*
*     Enables the HISTORY update mode to be changed or reported. A logical
*     toggle controls each mode - if none is specified, the current mode
*     is reported.
*
*    Environment parameters :
*
*     ILOC = UNIV(R)
*       Dataset whose update mode is to be changed or reported
*     DISABLED = LOGICAL(R)
*       Set update mode to DISABLED
*     QUIET = LOGICAL(R)
*       Set update mode to QUIET
*     NORMAL = LOGICAL(R)
*       Set update mode to NORMAL
*     VERBOSE = LOGICAL(R)
*       Set update mode to VERBOSE
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Jun 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(HIST_CMN)'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC 		! Input dataset
      CHARACTER*10           MODE               ! HISTORY mode

      LOGICAL                IPRIM              ! Input primitive?
      LOGICAL                OK                 ! History already present?
      LOGICAL                QUIET, NORMAL,     ! Mode toggles
     :                       DISABLED, VERBOSE
*
*    Version :
*
      CHARACTER*30       VERSION
        PARAMETER        (VERSION = 'HISTMODE Version 1.7-0')
*-

*    Initialise
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input
      CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, IPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    History already present?
      CALL HIST_OK( ILOC, OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'No HISTORY structure present in dataset -'/
     :                                            /' creating it' )
        CALL HIST_NEW( ILOC, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get logicals
      DISABLED = .FALSE.
      QUIET = .FALSE.
      NORMAL = .FALSE.
      VERBOSE = .FALSE.
      CALL PAR_GET0L( 'DISABLED', DISABLED, STATUS )
      IF ( DISABLED ) THEN
        MODE = 'DISABLED'
      ELSE
        CALL PAR_GET0L( 'QUIET', QUIET, STATUS )
        IF ( QUIET ) THEN
          MODE = 'QUIET'
        ELSE
          CALL PAR_GET0L( 'NORMAL', NORMAL, STATUS )
          IF ( NORMAL ) THEN
            MODE = 'NORMAL'
          ELSE
            CALL PAR_GET0L( 'VERBOSE', VERBOSE, STATUS )
            IF ( VERBOSE ) THEN
              MODE = 'VERBOSE'
            ELSE
              MODE = ' '
            END IF
          END IF
        END IF
      END IF

*    Set the update mode?
      IF ( MODE .GT. ' ' ) THEN

*      History not present?
        IF ( .NOT. OK ) THEN

*        Create it
          CALL HIST_NEW( ILOC, STATUS )

*      Present, but no flag (old ASTERIX datasets)
        ELSE IF ( .NOT. HTBL(REC).VERB_SET ) THEN

*        Create the object
          CALL DAT_NEW0C( HTBL(REC).HIST_LOC, 'UPDATE_MODE', 10,
     :                                                  STATUS )
          HTBL(REC).VERB_SET = .TRUE.

        END IF

*      Write the flag
        CALL CMP_PUT0C( HTBL(REC).HIST_LOC, 'UPDATE_MODE', MODE,
     :                                                  STATUS )

*    otherwise report it
      ELSE

*      History present?
        IF ( OK ) THEN

*        Flag set?
          IF ( HTBL(REC).VERB_SET ) THEN
            CALL CMP_GET0C( HTBL(REC).HIST_LOC, 'UPDATE_MODE', MODE,
     :                                                      STATUS )
            CALL MSG_SETC( 'MODE', MODE )
            CALL MSG_PRNT( '* Current HISTORY update mode is ^MODE *' )
          ELSE
            CALL MSG_PRNT( '* No HISTORY update mode is set - '/
     :                                  /'default is NORMAL *' )
          END IF

        ELSE
          CALL MSG_PRNT( '* No HISTORY present in dataset *' )
        END IF

      END IF

      CALL AST_CLOSE()


*    Tidy up
 99   CALL AST_ERR( STATUS )

      END
