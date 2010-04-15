*+  P4_DISPLAY - Display a dataset in a variety of ways
      SUBROUTINE P4_DISPLAY( STATUS )
*    Invocation :
*     CALL P4_DISPLAY( STATUS )
*    Authors :
*     P. N. Daly (JACH::PND)
*    History :
*     15-Aug-1994: Original Unix version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'P4COM.INC'                 ! P4 common block
*    Local variables :
      INTEGER PORT                        ! Port to plot in
      INTEGER NBS_STATUS                  ! Status from NBS call
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set flag to indicate task is busy
      NBS_STATUS = SAI__OK
      CALL NBS_PUT_VALUE( TASK_BUSY_ID, 0, VAL__NBI, .TRUE., NBS_STATUS )

*    Get the port number, read noticebaord, get dataset
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )
      CALL PAR_GET0C( 'DATA', DISPLAY_DATA( PORT ), STATUS )
      CALL CHR_UCASE( DISPLAY_PLANE(PORT) )

*    Reset mean and sigma
      MEAN( PORT )  = 0.0
      SIGMA( PORT ) = 0.0

*    Send a verbose message if required
      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'PLANE', DISPLAY_PLANE(PORT) )
        CALL MSG_SETC( 'DATA', DISPLAY_DATA(PORT) )
        CALL MSG_SETC( 'TYPE', DISPLAY_TYPE(PORT) )
        CALL MSG_SETI( 'PORT', PORT )
        CALL MSG_OUT( ' ', 'Displaying ^PLANE plane of ^DATA as a ^TYPE on port ^PORT', STATUS )
      ENDIF

*    Do the plot of the appropriate type
      IF ( DISPLAY_TYPE( PORT ) .EQ. 'IMAGE' ) THEN
        CALL P4_IMAGE( PORT, STATUS )
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'GRAPH' ) THEN
        CALL P4_GRAPH( PORT, STATUS )
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'OVERGRAPH' ) THEN
        CALL P4_GRAPH( PORT, STATUS )
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'HISTOGRAM' ) THEN
        CALL P4_HISTOGRAM( PORT, STATUS )
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'CONTOUR' ) THEN
        CALL P4_CONTOUR( PORT, STATUS )
      ELSE IF ( DISPLAY_TYPE( PORT ) .EQ. 'SURFACE' ) THEN
        CALL P4_SURFACE( PORT, STATUS )
      ENDIF

*    Update the noticeboard
      ISTART(PORT) = -1
      IEND(PORT)   = -1
      JSTART(PORT) = -1
      JEND(PORT)   = -1
      TITLE(PORT)  = 'A_U_T_O'
      CALL P4_WRITE_NB( PORT, STATUS )

*    Unset the task busy flag
      NBS_STATUS = SAI__OK
      CALL NBS_PUT_VALUE( TASK_BUSY_ID, 0, VAL__NBI, .FALSE., NBS_STATUS )

      END
