*+  TASK_CLEAR_MESSINFO - clear list of active subsidiary actions for an action
      SUBROUTINE TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
*    Description :
*     Marks all entries in the list of active subsidiary actions for an action 
*     as being unused. Entries are added to the list when user code calls
*     TASK_ADD_MESSINFO to indicate that it has initiated an action in a
*     subsidiary (lower-level) task. Entries are removed from the list when
*     fixed d-task routines determine that such an action has completed. It
*     is assumed that this routine is only called when the list is expected
*     to be empty and warning messages are output if any entries are found.
*    Invocation :
*     CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
*    Parameters :
*     ACTPTR=INTEGER (given)
*           The pointer by which entries in the list are associated with
*           actions (normally the same as the action pointer that is used
*           in the DTASK_ routines but it doesn't have to be).
*     STATUS=INTEGER
*    Method :
*     Search list for entries with matching action pointers.
*     If found, warn and set action pointer to -1 (unused).
*     If this was last used entry, set topmost -1's to 0 (end of list).
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*     B.D.Kelly (REVAD::BDK)
*     A.J.Chipperfield (STARLINK)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files and set error status before 
*                  calling ERR routines (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     11.11.1992:  Use ERR_REP and ERR_FLUSH not ERR_OUT (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*     15.06.2001:  Use AMS (FAMS) _PLOOKUP not MESSYS_PLOOKUP (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*    Import :
      INTEGER ACTPTR                  ! action pointer for which entries
                                      ! are to be cleared 

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      INTEGER I                       ! counter
      LOGICAL DONE                    ! whether have finished going
                                      ! through the list 
      CHARACTER*(MESSYS__TNAME) TASK  ! task name used in error messages
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Cycle through the list until exhaust it or find a zero action pointer
*    (which indicates end of list).
*
      I = 0
      DONE = .FALSE.
      DO WHILE ( ( I .LT. TASK__MAXSUB ) .AND. ( .NOT. DONE ) )
         I = I + 1
*
*    If find a zero action pointer, set adjacent -1 (unused) pointers to
*    zero too, and exit from the loop.
*
         IF ( MESACTPTR(I) .EQ. 0 ) THEN
            I = I - 1
            DO WHILE ( ( I .GT. 0 ) .AND. ( MESACTPTR(I) .EQ. -1 ) )
               MESACTPTR(I) = 0
               I = I - 1
            ENDDO
            DONE = .TRUE.
*
*    If find a matching pointer, output warning message and clear it to
*    -1 (unused).
*
         ELSE IF ( MESACTPTR(I) .EQ. ACTPTR ) THEN
            TASK = 'unknown'
            CALL FAMS_PLOOKUP ( MESPATH(I), TASK, STATUS )
            STATUS = SAI__ERROR
            CALL ERR_REP ( ' ', 'Programming error: active '//
     :               'subsidiary action at action completion', STATUS )
            CALL MSG_SETC ( 'TASK', TASK )
            CALL MSG_SETI ( 'MESSID', MESMESSID(I) )
            CALL ERR_REP ( ' ', 'Task ^TASK, message id ^MESSID',
     :                                                         STATUS )
            CALL ERR_FLUSH ( STATUS )
            MESACTPTR(I) = -1
         ENDIF
      ENDDO

      END
