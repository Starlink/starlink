*+  TASK_ADD_MESSINFO - add to list of active subsidiary actions for an action
      SUBROUTINE TASK_ADD_MESSINFO ( PATH, MESSID, STATUS )
*    Description :
*     Adds an entry to the list of active subsidiary actions for the current
*     action. The entry simply associates the action with the path and
*     message id corresponding to the action that it has just initiated
*     in a subsidiary task.
*    Invocation :
*     CALL TASK_ADD_MESSINFO ( PATH, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     STATUS=INTEGER
*    Method :
*     Check that there is a current action (action pointer should be in COMMON).
*     Search through the list until find a free entry (action pointer <= 0).
*     Copy current action pointer, path and message id to the entry.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS (REVAD::BDK)
*     04.10.1992:  add PAR_PAR (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_ERR'
      INCLUDE 'TASK_PAR'

*    Import :
      INTEGER PATH        ! the path identifying the subsidiary task
      INTEGER MESSID      ! the message id'ing the action in the subsidiary task

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have added entry to the list
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Check that there is a current action.
*
      IF ( CURACTPTR .LE. 0 ) THEN
         STATUS = TASK__NOCURACT
*
*    Search for a free entry in the list.
*
      ELSE
         I = 0
         DONE = .FALSE.
         DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
            I = I + 1
            IF ( MESACTPTR(I) .LE. 0 ) THEN
               MESACTPTR(I) = CURACTPTR
               MESPATH(I) = PATH
               MESMESSID(I) = MESSID
               DONE = .TRUE.
            ENDIF
         ENDDO
         IF ( .NOT. DONE ) THEN
            STATUS = TASK__NOMESROOM
         ENDIF
      ENDIF

      END
