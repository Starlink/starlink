*+  DTASK_ESETK - Set message associated with status into token
      SUBROUTINE DTASK_ESETK ( NAME, STATUS )
*    Description :
*     Set message associated with status into EMS token.
*     This is done via this routine to assist in portability.
*    Invocation :
*     CALL DTASK_ESETK ( NAME, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*           name of the token to be set
*     STATUS=INTEGER
*    Method :
*     Call ERR_FACER
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     AJC: A J Chipperfield (STARLINK)
*    History :
*     13-OCT-1992 (AJC):
*       Original version
*     30-SEP-1994 (AJC):
*       Use ERR_FACER not ERR_SYSER (Now common for Unix and VMS)
*      1-MAR-1995 (AJC):
*       Remove comment VMS version
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME
*    Status :
      INTEGER STATUS
*-

*   If status is OK, set 'OK'
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( NAME, 'OK' )

*   otherwise call ERR_FACER
      ELSE
         CALL ERR_FACER( NAME, STATUS )

      ENDIF

      END
