*-----------------------------------------------------------------------
*+  IKNSEL - Select Configuration

      SUBROUTINE IKNSEL ( DISPID, NCONF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDSEL.
*     The arguments are identical to those in IIDSEL.
*
*    Invocation :
*     CALL IKNSEL( DISPID, NCONF, STATUS )
*
*    Method :
*     Verify the input arguments and if the requested configuration
*     is not the current one then do a hard reset to obtain the new
*     configuration.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     March 1989
*     December 1990  Changed name from IIDSEL
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     Configuration number
      INTEGER NCONF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
*-

*   Verify the configuration number
      IF ( ( NCONF .LT. 0 ) .OR. ( NCONF .GE. CONNUM ) ) THEN
         STATUS = IDI__INCON
         GOTO 99
      ENDIF

*   If the given configuration number is not the current one then do a
*   hard reset of the terminal with the new configuration
*   This is an easy way out
      IF ( NCONF .NE. CONFIG ) THEN
         CONFIG = NCONF
         CALL IKNRST( DISPID, STATUS )
      ENDIF

  99  CONTINUE

      END

