*+ P4.FOR - Top level routine for P4 plotting task
      SUBROUTINE P4( STATUS )
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      4-Aug-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ACT_ERR'
      INCLUDE 'PAR_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'
*    Local variables :
      CHARACTER*( PAR__SZNAM ) NAME   ! The name of action
      INTEGER CONTEXT                 ! The context
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check the context, which should be OBEY or CANCEL.
      CALL TASK_GET_CONTEXT( CONTEXT, STATUS )
      IF ( CONTEXT .EQ. OBEY ) THEN

*     Obey the named action
        CALL TASK_GET_NAME( NAME, STATUS )

*     Plot a colour bar next to an IMAGE plot
        IF ( NAME .EQ. 'BAR' ) THEN
           CALL P4_BAR( STATUS )

*     Clear the display within a given port
        ELSE IF ( NAME .EQ. 'CLEAR' ) THEN
           CALL P4_CLEAR( STATUS )

*     Close the noticeboard.
        ELSE IF ( NAME .EQ. 'CLOSE_NB' ) THEN
           CALL P4_CLOSE_NB( STATUS )

*     Close the viewport
        ELSE IF ( NAME .EQ. 'CLOSE_PORT' ) THEN
           CALL P4_CLOSE_PORT( STATUS )

*     Use the cursor within a port to obtain an X/Y position.
        ELSE IF ( NAME .EQ. 'CURSOR' ) THEN
           CALL P4_CURSOR( STATUS )

*     Use the cursor within a port to obtain an X/Y position and value
        ELSE IF ( NAME .EQ. 'CURSORVAL' ) THEN
           CALL P4_CURSORVAL( STATUS )

*     Display the data in some way
        ELSE IF ( NAME .EQ. 'DISPLAY' ) THEN
           CALL P4_DISPLAY( STATUS )

*     Identify the plot with username, date
        ELSE IF ( NAME .EQ. 'IDENTIFY' ) THEN
           CALL P4_IDENTIFY( STATUS )

*     List the common block values
        ELSE IF ( NAME .EQ. 'LIST_CB' ) THEN
           CALL P4_LIST_CB( STATUS )

*     List the noticeboard to screen
        ELSE IF ( NAME .EQ. 'LIST_NB' ) THEN
           CALL P4_LIST_NB( STATUS )

*     Load a colour table
        ELSE IF ( NAME .EQ. 'LUT' ) THEN
           CALL P4_LUT( STATUS )

*     Plot an overgraph
        ELSE IF ( NAME .EQ. 'OVERGRAPH' ) THEN
           CALL P4_DISPLAY_OVERGRAPH( STATUS )

*     Open a noticeboard
        ELSE IF ( NAME .EQ. 'OPEN_NB' ) THEN
           CALL P4_OPEN_NB( STATUS )

*     Reset the P4 task and annul errors
        ELSE IF ( NAME .EQ. 'RESET' ) THEN
           CALL P4_RESET( STATUS )

*     Restore the configuration from a file
        ELSE IF ( NAME .EQ. 'RESTORE' ) THEN
           CALL P4_RESTORE( STATUS )

*     Save the configuration to a file
        ELSE IF ( NAME .EQ. 'SAVE' ) THEN
           CALL P4_SAVE( STATUS )

*     Quick status check for P4 task
        ELSE IF ( NAME .EQ. 'STATUS' )  THEN
           CALL MSG_OUT( ' ', 'P4 Task  : Portable-CGS4DR P4 VPKG_VERS', STATUS )
           CALL MSG_OUT( ' ', 'P4 Task  : The uncached P4 task is OK', STATUS )

*     Verbose on
        ELSE IF ( NAME .EQ. 'VERBOSE' ) THEN
           VERBOSE = .TRUE.
           CALL MSG_OUT( ' ', 'P4: Verbose messages enabled in task', STATUS )

*     Verbose off
        ELSE IF ( NAME .EQ. 'NOVERBOSE' ) THEN
           VERBOSE = .FALSE.
           CALL MSG_OUT( ' ', 'P4: Verbose messages disabled in task', STATUS )

*     The action name is not recognised.
        ELSE
           STATUS = SAI__ERROR
           CALL MSG_SETC( 'NAME', NAME )
           CALL ERR_REP( ' ', 'P4: Invalid P4 action, ^NAME', STATUS )
        ENDIF

*   Cancel the action
      ELSE IF ( CONTEXT .EQ. CANCEL ) THEN
         CALL TASK_PUT_REQUEST( ACT__CANCEL, STATUS )

*   The context is not recognised
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'P4: Invalid P4 context', STATUS )
      ENDIF

      END
