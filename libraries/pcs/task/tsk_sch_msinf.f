*+  TASK_SEARCH_MESSINFO - search for entry from list of active subsid actions
      SUBROUTINE TASK_SEARCH_MESSINFO ( PATH, MESSID, ACTPTR, STATUS )
*    Description :
*     Searches for an entry corresponding to a specified path and message id in
*     the list of active subsidiary actions. If the entry is found, the action
*     pointer is returned. Otherwise, an action pointer of zero is returned
*     (but not a bad status).
*    Invocation :
*     CALL TASK_SEARCH_MESSINFO ( PATH, MESSID, ACTPTR, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     ACTPTR=INTEGER (returned)
*           The action pointer for the action that initiated the action in
*           the subsidiary task. Zero if the path and message are not found
*           in the list
*     STATUS=INTEGER
*    Method :
*     Search list for entry with matching path and message id.
*     If found, return action pointer.
*     If not found, return action pointer of zero.
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
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*    Import :
      INTEGER PATH        ! the path identifying the subsidiary task
      INTEGER MESSID      ! the message id'ing the action in the subsidiary task

*    Export :
      INTEGER ACTPTR      ! the action pointer for the initiating action

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have searchd entry to the list
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list until find matching entry or find a zero action
*    pointer (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      ACTPTR = 0
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer (end of list) exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            DONE = .TRUE.
*
*    If find a matching entry, return action pointer and exit from the loop.
*
         ELSE IF ( ( MESPATH(I) .EQ. PATH ) .AND.
     :    ( MESMESSID(I) .EQ. MESSID ) ) THEN
            ACTPTR = MESACTPTR(I)
            DONE = .TRUE.
         ENDIF
      ENDDO

      END
