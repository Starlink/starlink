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
*     INP = UNIV(R)
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
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     26 Mar 95 : V1.8-1 Use new data interface (DJA)
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
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*10           	MODE               	! HISTORY mode

      INTEGER			HCID			! History control
      INTEGER			IFID			! Dataset id

      LOGICAL                	OK                 ! History already present?
      LOGICAL                	QUIET, NORMAL,     ! Mode toggles
     :                          DISABLED, VERBOSE
*
*    Version :
*
      CHARACTER*30       VERSION
        PARAMETER        (VERSION = 'HISTMODE Version 1.8-1')
*-

*    Initialise
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input
      CALL USI_TASSOCI( 'INP', '*', 'UPDATE', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    History already present?
      CALL HSI_OK( IFID, OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'No HISTORY structure present in dataset -'/
     :                                            /' creating it' )
        CALL HSI_NEW( IFID, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get control object
      CALL HSI_GETCTR( IFID, HCID, STATUS )

*    Get logicals
      DISABLED = .FALSE.
      QUIET = .FALSE.
      NORMAL = .FALSE.
      VERBOSE = .FALSE.
      CALL USI_GET0L( 'DISABLED', DISABLED, STATUS )
      IF ( DISABLED ) THEN
        MODE = 'DISABLED'
      ELSE
        CALL USI_GET0L( 'QUIET', QUIET, STATUS )
        IF ( QUIET ) THEN
          MODE = 'QUIET'
        ELSE
          CALL USI_GET0L( 'NORMAL', NORMAL, STATUS )
          IF ( NORMAL ) THEN
            MODE = 'NORMAL'
          ELSE
            CALL USI_GET0L( 'VERBOSE', VERBOSE, STATUS )
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

*      Change the verbosity
        CALL ADI_CPUT0C( HCID, 'Verbosity', MODE, STATUS )

*      Update the control info
        CALL HSI_PUTCTR( IFID, HCID, STATUS )

*    otherwise report it
      ELSE

*      History present?
        IF ( OK ) THEN

*        Flag set?
          CALL ADI_CGET0C( HCID, 'Verbosity', MODE, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'MODE', MODE )
            CALL MSG_PRNT( '* Current HISTORY update mode is ^MODE *' )
          ELSE
            CALL ERR_ANNUL( STATUS )
            CALL MSG_PRNT( '* No HISTORY update mode is set - '/
     :                                  /'default is NORMAL *' )
          END IF

        ELSE
          CALL MSG_PRNT( '* No HISTORY present in dataset *' )
        END IF

      END IF

*    Release control object
      CALL ADI_ERASE( HCID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
