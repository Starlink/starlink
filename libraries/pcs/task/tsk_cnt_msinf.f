*+  TASK_COUNT_MESSINFO - count entries in list of active subsid actions
      SUBROUTINE TASK_COUNT_MESSINFO ( ACTPTR, COUNT, STATUS )
*    Description :
*     Counts entries in the list of active actions in subsidiary tasks
*     corresponding to a given initiating action.
*    Invocation :
*     CALL TASK_COUNT_MESSINFO ( ACTPTR, COUNT, STATUS )
*    Parameters :
*     ACTPTR=INTEGER (given)
*           The action pointer for the action that initiated the actions in
*           the subsidiary task.
*     COUNT=INTEGER (given)
*           The number of actions initiated by this action that are still
*           active.
*     STATUS=INTEGER
*    Method :
*     Count list entries associated with this action.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     01.05.1989:  original (AAOEPP::WFL)
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
      INTEGER ACTPTR      ! the action pointer for the initiating action

*    Export :
      INTEGER COUNT       ! the number of active actions initiated by this one

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      INTEGER I           ! counter
      LOGICAL DONE        ! whether have finished searching the list
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list counting matching entries until hit end of list or
*    find a zero action pointer (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      COUNT = 0
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer (end of list) exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            DONE = .TRUE.
*
*    If find a matching entry, increment count.
*
         ELSE IF ( MESACTPTR(I) .EQ. ACTPTR ) THEN
            COUNT = COUNT + 1
         ENDIF
      ENDDO

      END
