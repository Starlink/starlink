*+  TASK_KICK - signal another action to reschedule
      SUBROUTINE TASK_KICK ( NAME, LENGTH, VALUE, STATUS )
*    Description :
*     This routine should be called by one action in a task to cause
*     another action in that task to be rescheduled.
*    Invocation :
*     CALL TASK_KICK ( NAME, LENGTH, VALUE, STATUS )
*    Parameters :
*     NAME=CHARACTER (given)
*           name of action to be rescheduled
*     LENGTH=INTEGER (given)
*           number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*           a set of bytes to be passed to the main-line code
*     STATUS=INTEGER
*    Method :
*     Use MESSYS_KICK.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     23.05.1991: original (REVAD::BDK)
*     27.05.1991: remove LIB$SIGNAL (REVAD::BDK)
*     15.06.2001: use AMS (FAMS) _KICK not MESSYS_KICK (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) NAME     ! name of action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! a set of bytes to be passed to the
                             ! main-line code 
*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Inform the message system.
*
      CALL FAMS_KICK ( NAME, LENGTH, VALUE, STATUS )

      END
