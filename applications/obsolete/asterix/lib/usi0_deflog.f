*+  USI0_DEFLOG - Define USI logging state
      SUBROUTINE USI0_DEFLOG( STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     21 May 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI0_PAR'
*
*    Global variables :
*
      INCLUDE 'USI_CMN'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Functions :
*
      LOGICAL			CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*132	        VSTR			! State variable value

      INTEGER			IC			! Character index
*-

*    New error context in case PSX_ calls fail
      CALL ERR_MARK

*    Default is no logging
      USI_LOGGING = .FALSE.

*    Get the state variable value
      CALL PSX_GETENV( 'AST_LOG_STATE', VSTR, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Variable something other than OFF?
        IF ( .NOT. CHR_SIMLR( VSTR(1:3), 'OFF' ) ) THEN

*        Get the mode
          IC = 1
          CALL CHR_FIWE( VSTR, IC, STATUS )
          USI_LOGMODE = VSTR(1:IC)

*        Compare modes
          IF ( CHR_SIMLR( USI_LOGMODE, 'ICL' ) ) THEN
            USI_LOGIMODE = USI__L_ICL

          ELSE IF ( CHR_SIMLR( USI_LOGMODE, 'CSH' ) ) THEN
            USI_LOGIMODE = USI__L_CSH

          ELSE IF ( CHR_SIMLR( USI_LOGMODE, 'CALL' ) ) THEN
            USI_LOGIMODE = USI__L_CALL

          ELSE
            CALL MSG_SETC( 'MODE', USI_LOGMODE )
            CALL MSG_OUT( ' ', 'Unrecognised logging mode ^MODE' )
            GOTO 99
          END IF

*        Get the filename
          IC = IC + 1
          CALL CHR_FIWS( VSTR, IC, STATUS )
          USI_LOGFILE = VSTR(IC:)

*        Open the file for append access
          CALL FIO_OPEN( USI_LOGFILE, 'APPEND', 'LIST', 0, USI_LOGFID,
     :                   STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN

*          Switch logging on
            USI_LOGGING = .TRUE.

          ELSE
            CALL ERR_REP( ' ', 'Unable to open command log file',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )

          END IF

        END IF

      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*    Abort point
 99   CONTINUE

*    Restore error context
      CALL ERR_RLSE

      END
