*+  P4_DISPLAY_OVERGRAPH - Display a dataset in a variety of ways
      SUBROUTINE P4_DISPLAY_OVERGRAPH( STATUS )
*    Invocation :
*     CALL P4_DISPLAY_OVERGRAPH( STATUS )
*    Authors :
*     P. N. Daly (JACH::PND)
*    History :
*     17-Jan-1994: Original Unix version (PND)
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
      INTEGER ACTBYTES, NBS_STATUS
      CHARACTER*( NBS_FLEN ) OLD_TYPE     ! Previous display type
      CHARACTER*( NBS_FLEN ) OLD_CUT      ! Previous cut direction
      CHARACTER*( NBS_FLEN ) OLD_COL      ! Previous overcolour
      LOGICAL OLD_ERASE                   ! Old value of pre_erase_plot
      REAL OLD_SSTART, OLD_SEND           ! Old values of slice start and end
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set flag to indicate task is busy
      NBS_STATUS = SAI__OK
      CALL NBS_PUT_VALUE( TASK_BUSY_ID, 0, VAL__NBI, .TRUE., NBS_STATUS )

*    Get the port number, read noticeboard and save old values
      CALL PAR_GET0I( 'PORT', PORT, STATUS )
      CALL P4_READ_NB( PORT, STATUS )
      OLD_TYPE = DISPLAY_TYPE( PORT )
      OLD_SSTART = SLICE_START( PORT )
      OLD_SEND = SLICE_END( PORT )
      OLD_CUT = CUT_DIRECTION( PORT )
      OLD_COL = OVERCOLOUR( PORT )

*    Get new values
      CALL PAR_GET0C( 'DATA', DISPLAY_DATA( PORT ), STATUS )
      CALL CHR_UCASE( DISPLAY_PLANE(PORT) )
      CALL PAR_GET0C( 'CUT', CUT_DIRECTION(PORT), STATUS )
      CALL CHR_UCASE( CUT_DIRECTION(PORT) )
      CALL PAR_GET0C( 'COLOUR', OVERCOLOUR(PORT), STATUS )
      CALL CHR_UCASE( OVERCOLOUR(PORT) )
      CALL PAR_GET0R( 'SPOS', SLICE_START(PORT), STATUS )
      CALL PAR_GET0R( 'EPOS', SLICE_END(PORT), STATUS )

*    Reset some values
      DISPLAY_TYPE( PORT ) = 'OVERGRAPH'
      CALL NBS_GET_VALUE( PRE_ERASE_PLOT_ID(PORT), 0, VAL__NBI, OLD_ERASE, ACTBYTES, STATUS )
      CALL NBS_PUT_VALUE( PRE_ERASE_PLOT_ID(PORT), 0, VAL__NBI, .FALSE., STATUS )

*    Send a verbose message if required
      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'PLANE', DISPLAY_PLANE(PORT) )
        CALL MSG_SETC( 'DATA', DISPLAY_DATA(PORT) )
        CALL MSG_SETC( 'TYPE', DISPLAY_TYPE(PORT) )
        CALL MSG_SETC( 'OCOL', OVERCOLOUR(PORT) )
        CALL MSG_SETI( 'PORT', PORT )
        CALL MSG_OUT( ' ', 'Displaying ^PLANE plane of ^DATA as a ^TYPE on port ^PORT in ^OCOL', STATUS )
        CALL MSG_SETC( 'CUT', CUT_DIRECTION(PORT) )
        CALL MSG_SETR( 'SPOS', SLICE_START(PORT) )
        CALL MSG_SETR( 'EPOS', SLICE_END(PORT) )
        CALL MSG_OUT( ' ', 'Taking cut in ^CUT direction between ^SPOS and ^EPOS', STATUS )
      ENDIF

*    Reset values, plot then update noticeboard
      MEAN( PORT )  = 0.0
      SIGMA( PORT ) = 0.0
      CALL P4_GRAPH( PORT, STATUS )
      ISTART(PORT) = -1
      IEND(PORT)   = -1
      JSTART(PORT) = -1
      JEND(PORT)   = -1
      TITLE(PORT)  = 'A_U_T_O'
      DISPLAY_TYPE( PORT ) = OLD_TYPE
      SLICE_START( PORT ) = OLD_SSTART
      SLICE_END( PORT ) = OLD_SEND
      CUT_DIRECTION( PORT ) = OLD_CUT
      OVERCOLOUR( PORT ) = OLD_COL
      CALL NBS_PUT_VALUE( PRE_ERASE_PLOT_ID(PORT), 0, VAL__NBI, OLD_ERASE, STATUS )
      CALL P4_WRITE_NB( PORT, STATUS )

*    Unset task busy flag
      NBS_STATUS = SAI__OK
      CALL NBS_PUT_VALUE( TASK_BUSY_ID, 0, VAL__NBI, .FALSE., NBS_STATUS )

      END
