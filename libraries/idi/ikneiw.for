*-----------------------------------------------------------------------
*+  IKNEIW - Execute interaction and wait for Ikon

      SUBROUTINE IKNEIW ( DISPID, TRIGS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIIEIW.
*     The arguments are identical to those in IIIEIW.
*
*    Invocation :
*     CALL IKNEIW( DISPID, TRIGS, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     This is a complex routine that serves all possible interactions.
*     It first sets up the Ikon mouse ( GID ) for smooth operation.
*     It then sees if any of the interactions require special treatment,
*     such as application specific code or ROIs.
*     The main loop is then entered which basically polls the mouse
*     and performs interactions until an exit trigger is fired.
*     In more detail the loop does the following:
*        Poll the mouse and inquire the GID position.
*        Establish if any buttons ( triggers ) have been pressed.
*        Loop through the interactions one by one.
*           Serve the interactions that use a locator.
*           Serve the interactions that use a trigger.
*        See if any of the exit triggers has been fired.
*     If no exit trigger has been fired and if there is no call for
*     application specific code then return to interaction loop.
*     Tidy up if an exit trigger has been fired.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     April 1990     Abort with error if GID won't clear
*     May 1990       Reset GID position if only rotating LUT
*     March 1991     For application specific code stay in routine
*                    if interactor has not changed
*     April 1991     Added interaction flags and corrected application
*                    specific code
*     June 1991      Test for exit triggers before application specific
*                    enabled triggers at end of main loop
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

*    Export :
*     Trigger status array
      LOGICAL TRIGS( * )

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMINT)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local Constants :
*     Number of tries to clear out type-ahead button presses
      INTEGER MAXCLR
      PARAMETER ( MAXCLR = 100 )

*     Values for the interactor types
      INTEGER VLOCAT, VREVAL, VIEVAL, VLEVAL, VCEVAL, VTRIGG
      PARAMETER ( VLOCAT = 0 )
      PARAMETER ( VREVAL = 1 )
      PARAMETER ( VIEVAL = 2 )
      PARAMETER ( VLEVAL = 3 )
      PARAMETER ( VCEVAL = 4 )
      PARAMETER ( VTRIGG = 5 )

*     Values for object types
      INTEGER VNOEFF, VCURS, VITT, VVLUT, VROI, VMEM, VDISP
      PARAMETER ( VNOEFF = 0 )
      PARAMETER ( VCURS = 1 )
      PARAMETER ( VITT = 2 )
      PARAMETER ( VVLUT = 3 )
      PARAMETER ( VROI = 4 )
      PARAMETER ( VMEM = 5 )
      PARAMETER ( VDISP = 6 )

*     Values for interactive operation
      INTEGER VEXAPP, VMOVE, VROTE, VIZOOM, VDZOOM, VNZOOM, VBLINK,
     :        VMODI
      PARAMETER ( VEXAPP = 0 )
      PARAMETER ( VMOVE = 1 )
      PARAMETER ( VROTE = 2 )
      PARAMETER ( VIZOOM = 3 )
      PARAMETER ( VDZOOM = 4 )
      PARAMETER ( VNZOOM = 5 )
      PARAMETER ( VBLINK = 6 )
      PARAMETER ( VMODI = 7 )

*    Local variables :
      LOGICAL FIRED, LBLINK, LDOCLR, LIMIT, LINTOP, LLORE,
     :        LMDROI, LMVCUR, LMVROI, LPASCR, LTRIG, LVLUT, LPRSET,
     :        LUPDAT, REPEAT, STATE( 3 )

      INTEGER * 2 PRESS, WORDS( 40 ), XINC, XYIN( 2 ), XYLAST( 2 ),
     :            XYPOS( 2 ), YINC

      INTEGER BUTTON( 0 : 2 )
      INTEGER DXDIFF, I, INTNUM, J, K, L, LUTLEN, LUTNUM, M, NUMWOR,
     :        ROIMOD, ROINUM, RSIZEX, RSIZEY, TEMP( 2 ), XDIFF, XD2, XL,
     :        XP, XPOS, XYANC( 2 ), XYMOD( 2 ), YDIFF, YD2, YPOS, ZOOMF

      REAL VLUT( 3, MAXCOL )

*    Local data :
      DATA BUTTON / 16384, 8192, 4096 /
      DATA LDOCLR / .TRUE. /
      DATA XYLAST / 0, 0 /
      SAVE BUTTON, LDOCLR, XYLAST
*-

*   Recover the characteristics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Clear the trigger status array, and the state array.
*   The state array determines if the button press will be obeyed.
*   It is only reset when a button is released which ensures that
*   holding down a button does not result in multiple button presses.
      DO 10 J = 1, CNTRIG
         TRIGS( J ) = .FALSE.
         STATE( J ) = .TRUE.
  10  CONTINUE

*   Release the GID from its clamp by setting the xy size
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 28 = '1C'X = Graphic input device size XY
      WORDS( 1 ) = 99
      WORDS( 2 ) = 28
      WORDS( 3 ) = CTWDIY( 0 )
      WORDS( 4 ) = CTWDIX( 0 )

*   Read the current position to use as the zero point for movements
*   but do not update the position inside an applications specific loop
*   Ikon command 165 = 'A5'X = Return current position
      WORDS( 5 ) = 165
      NUMWOR = 5
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, XYPOS, STATUS )
      IF ( LDOCLR ) THEN
         XYLAST( 1 ) = XYPOS( 1 )
         XYLAST( 2 ) = XYPOS( 2 )
      ENDIF

*   Remember the current position on entry
      XYIN( 1 ) = XYPOS( 1 )
      XYIN( 2 ) = XYPOS( 2 )

*   Clear out any residual button presses from the mouse
*   First set and reset the GID position which clears the menu status
*   Only do this if the flag LDOCLR is TRUE. This is to stop this
*   section being repeated for application specific code interactions
      IF ( LDOCLR ) THEN

*   Read the GID position
*   Ikon command 105 = '69'X = Read register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
         WORDS( 1 ) = 105
         WORDS( 2 ) = 29
         NUMWOR = 2
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 2
         CALL IKNIBW( DISPID, NUMWOR, XYPOS, STATUS )

*   Increment the position, send it, then send it again.
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
         WORDS( 1 ) = 99
         WORDS( 2 ) = 29
         WORDS( 3 ) = XYPOS( 1 ) + 1
         WORDS( 4 ) = XYPOS( 2 ) + 1
         WORDS( 5 ) = 99
         WORDS( 6 ) = 29
         WORDS( 7 ) = XYPOS( 1 )
         WORDS( 8 ) = XYPOS( 2 )
         NUMWOR = 8
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Then check that the menu status is OK
*   Only do this a limited number of times ( MAXCLR ) since the user
*   may be holding down the mouse button
         I = 0
  20     CONTINUE

*   Ikon command 94 = '5E'X = Return menu status
         WORDS( 1 ) = 94
         NUMWOR = 1
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         CALL IKNOUT( STATUS )
         NUMWOR = 1
         CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
         I = I + 1

*   Abort with error if the buttons aren't clear after MAXCLR tries
         IF ( I .GE. MAXCLR ) THEN
            STATUS = IDI__IOERR
            GOTO 99
         ENDIF

*   Repeat if the buttons are not clear
         IF ( PRESS .NE. 0 ) GOTO 20
      ENDIF

*   Find out what sort of interactions have been requested
      LBLINK = .FALSE.
      LINTOP = .FALSE.
      LLORE = .FALSE.
      LMDROI = .FALSE.
      LMVCUR = .FALSE.
      LMVROI = .FALSE.
      LPASCR = .FALSE.
      LTRIG = .FALSE.
      LVLUT = .FALSE.
      LPRSET = .TRUE.
      DO 30 J = 0, CINTN - 1

*   See if any of the interactions are for application specific code
         IF ( CINTOP( J ) .EQ. VEXAPP ) THEN
            LINTOP = .TRUE.

*   Set the button clearing flag to false
            LDOCLR = .FALSE.

*   Set the locator update flag if this is the first call to IKNEIW
            IF ( CINTFL( J ) .EQ. 1 ) THEN
               CINTFL( J ) = 0
            ENDIF
         ENDIF

*   Find out if any interactions use the locator or evaluators
         IF ( ( CINTTY( J ) .GE. VLOCAT ) .AND.
     :        ( CINTTY( J ) .LE. VCEVAL ) ) THEN
            LLORE = .TRUE.
         ENDIF

*   Find out if ant interactions use the triggers
         IF ( CINTTY( J ) .EQ. VTRIGG ) THEN
            LTRIG = .TRUE.
         ENDIF

*   See if there is an interaction for the look-up table
         IF ( COBJTY( J ) .EQ. VVLUT ) THEN
            LVLUT = .TRUE.
            INTNUM = J
         ENDIF

*   See if there is an interaction to blink
         IF ( CINTOP( J ) .EQ. VBLINK ) THEN
            LBLINK = .TRUE.
         ENDIF

*   See if there is an interaction to modify the ROI
         IF ( ( COBJTY( J ) .EQ. VROI ) .AND.
     :        ( CINTOP( J ) .EQ. VMODI ) ) THEN
            LMDROI = .TRUE.
            ROINUM = COBJID( J )
         ENDIF

*   See if there is an interaction to move the ROI
         IF ( ( COBJTY( J ) .EQ. VROI ) .AND.
     :        ( CINTOP( J ) .EQ. VMOVE ) ) THEN
            LMVROI = .TRUE.
            ROINUM = COBJID( J )
         ENDIF

*   See if there is an interaction to move a cursor
         IF ( ( COBJTY( J ) .EQ. VCURS ) .AND.
     :        ( CINTOP( J ) .EQ. VMOVE ) ) THEN
            LMVCUR = .TRUE.
         ENDIF

*   See if there is an interaction to pan or scroll the memory or display
         IF ( ( CINTOP( J ) .EQ. VMOVE ) .AND.
     :        ( ( COBJTY( J ) .EQ. VMEM ) .OR.
     :          ( COBJTY( J ) .EQ. VDISP ) ) ) THEN
            LPASCR = .TRUE.
         ENDIF

*   If there is an interaction to move or modify an object then
*   do not let the GID position be reset
         IF ( ( CINTOP( J ) .EQ. VMOVE ) .OR.
     :        ( CINTOP( J ) .EQ. VMODI ) ) THEN
            LPRSET = .FALSE.
         ENDIF

  30  CONTINUE

*   The LUPDAT flag is used to check that the state of the
*   interactor has changed when using application specific code
      LUPDAT = .FALSE.

*   Establish the conditions for leaving the routine if there is
*   an interaction for application specific code
      IF ( LINTOP ) THEN

*   If there is a locator or evaluator then leave the routine
         IF ( LLORE ) THEN
            REPEAT = .FALSE.
            LUPDAT = .TRUE.

         ELSE
            REPEAT = .TRUE.
         ENDIF

*   If there is no application specific code then stay in the routine
      ELSE
         LTRIG = .FALSE.
         REPEAT = .TRUE.
      ENDIF

*   If the GID is to be reset then set the GID to the middle of the range
      IF ( LPRSET ) THEN
         XYLAST( 1 ) = CTWDIX( 0 ) / 2
         XYLAST( 2 ) = CTWDIY( 0 ) / 2
      ENDIF

*   If there is to be panning then set the GID position to the middle
*   of its range allowing for any previous pans
*   Do not do this if there is a cursor or ROI interaction
      IF ( LPASCR .AND. ( .NOT. LMVCUR ) .AND. ( .NOT. LMVROI ) ) THEN
         XYLAST( 1 ) = CMEMX( 0 ) + CTWDIX( 0 ) / 2
         XYLAST( 2 ) = CMEMY( 0 ) + CTWDIY( 0 ) / 2
      ENDIF

*   For application specific code don't want to reset the GID position
*   inside the main loop
      IF ( LUPDAT ) THEN
         LPRSET = .FALSE.
      ENDIF

*   Update the locator position
      CLOCXY( 1 ) = XYLAST( 1 )
      CLOCXY( 2 ) = XYLAST( 2 )

*   Set the GID position to correspond to the current position
*   Send this twice in case the GID is moving during the first send
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
      WORDS( 1 ) = 99
      WORDS( 2 ) = 29
      WORDS( 3 ) = XYLAST( 2 )
      WORDS( 4 ) = XYLAST( 1 )
      WORDS( 5 ) = 99
      WORDS( 6 ) = 29
      WORDS( 7 ) = XYLAST( 2 )
      WORDS( 8 ) = XYLAST( 1 )
      WORDS( 9 ) = 164
      WORDS( 10 ) = XYLAST( 1 )
      WORDS( 11 ) = XYLAST( 2 )
      NUMWOR = 11
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   If the ROI is to be modified then set up the rubber band box
      IF ( LMDROI ) THEN

*   Set the anchor position at top-right corner
         XYANC( 1 ) = CROIXH( ROINUM )
         XYANC( 2 ) = CROIYH( ROINUM )
         XYMOD( 1 ) = CROIXL( ROINUM )
         XYMOD( 2 ) = CROIYL( ROINUM )
         ROIMOD = 0

*   If the ROI is visible then set up the rubber band box
         IF ( CROIVI( ROINUM ) .NE. 0 ) THEN

*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
            WORDS( 1 ) = 124
            WORDS( 2 ) = CROIBI( ROINUM )
            WORDS( 3 ) = 125
            WORDS( 4 ) = CROIBI( ROINUM )

*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 86 = '56'X = Rubber band anchor 1
            WORDS( 5 ) = 99
            WORDS( 6 ) = 86
            WORDS( 7 ) = XYANC( 2 )
            WORDS( 8 ) = XYANC( 1 )

*   Set the GID at the other corner
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
            WORDS( 9 ) = 99
            WORDS( 10 ) = 29
            WORDS( 11 ) = XYMOD( 2 )
            WORDS( 12 ) = XYMOD( 1 )

*   Enable the GID with rubber band from anchor 1
*   Ikon command 96 = '63'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
            WORDS( 13 ) = 96
            WORDS( 14 ) = 24
            WORDS( 15 ) = 128 + 16 + 4

*   Set the GID position again
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon register 29 = '1D'X = GID position XY
            WORDS( 16 ) = 99
            WORDS( 17 ) = 29
            WORDS( 18 ) = XYMOD( 2 )
            WORDS( 19 ) = XYMOD( 1 )

            NUMWOR = 19
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         ENDIF
      ENDIF

*   If the ROI is to be moved then set up the rubber band box
      IF ( LMVROI ) THEN

*   If the ROI is visible then set up the rubber band box
         IF ( CROIVI( ROINUM ) .NE. 0 ) THEN

*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
            WORDS( 1 ) = 124
            WORDS( 2 ) = CROIBI( ROINUM )
            WORDS( 3 ) = 125
            WORDS( 4 ) = CROIBI( ROINUM )

*   Set up fixed sized rectangle specified by the copy source area
*   Ikon command 164 = 'A4'X = Move to
            WORDS( 5 ) = 164
            WORDS( 6 ) = CROIXL( ROINUM ) - CMEMX( 0 )
            WORDS( 7 ) = CROIYL( ROINUM ) - CMEMY( 0 )
*   Ikon command 48 = '30'X = Mark copy source area
            WORDS( 8 ) = 48
            WORDS( 9 ) = CROIXH( ROINUM ) - CMEMX( 0 )
            WORDS( 10 ) = CROIYH( ROINUM ) - CMEMY( 0 )

*   Set the GID at the bottom left corner of the box
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
            WORDS( 11 ) = 99
            WORDS( 12 ) = 29
            WORDS( 13 ) = CROIYL( ROINUM ) - CMEMY( 0 )
            WORDS( 14 ) = CROIXL( ROINUM ) - CMEMX( 0 )

*   Enable the GID with a fixed rectangle
*   Ikon command 96 = '63'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
            WORDS( 15 ) = 96
            WORDS( 16 ) = 24
            WORDS( 17 ) = 128 + 32

*   Set the GID position again
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon register 29 = '1D'X = GID position XY
            WORDS( 18 ) = 99
            WORDS( 19 ) = 29
            WORDS( 20 ) = CROIYL( ROINUM )
            WORDS( 21 ) = CROIXL( ROINUM )
            NUMWOR = 21
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         ENDIF
      ENDIF

*   If there is a LUT interaction. This is to prevent having to read it
*   in every time during the interaction loop.
      LUTLEN = 2 ** CLUTDE
      IF ( LVLUT ) THEN
         LUTNUM = COBJID( INTNUM )
         CALL IKNRLT( DISPID, LUTNUM, 0, LUTLEN, VLUT, STATUS )
      ENDIF

*   If there is no interaction for application specific code then
*   stay in this routine until an exit trigger is fired

*   Loop back to here if repeat is true
  40  CONTINUE

*   Poll the mouse by requesting a return menu status
*   Ikon command 94 = '5E'X = Return menu status
      WORDS( 1 ) = 94
      NUMWOR = 1
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 1
      CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )

*   Read the current position
*   Ikon command 165 = 'A5'X = Return current position
      WORDS( 1 ) = 165
      NUMWOR = 1
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, XYPOS, STATUS )

*   Calculate the difference from the last position
      XDIFF = XYPOS( 1 ) - XYLAST( 1 )
      YDIFF = XYPOS( 2 ) - XYLAST( 2 )

*   See if the locator has moved since the routine began
      IF ( ( XYPOS( 1 ) .NE. XYIN( 1 ) ) .OR.
     :     ( XYPOS( 2 ) .NE. XYIN( 2 ) ) ) THEN
         LUPDAT = .FALSE.
      ENDIF

*   Reset the current position and GID position if the flag is set
*   Ikon command 164 = 'A4'X = Move to
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
      IF ( LPRSET ) THEN
         WORDS( 1 ) = 164
         WORDS( 2 ) = XYLAST( 1 )
         WORDS( 3 ) = XYLAST( 2 )
         WORDS( 4 ) = 99
         WORDS( 5 ) = 29
         WORDS( 6 ) = XYLAST( 2 )
         WORDS( 7 ) = XYLAST( 1 )
         NUMWOR = 7
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Otherwise make the previous position the current position
      ELSE
         XYLAST( 1 ) = XYPOS( 1 )
         XYLAST( 2 ) = XYPOS( 2 )
      ENDIF

*   To stop the Ikon mouse overrunning the GID limits have to reset
*   the GID position if it has reached one of the limits
      LIMIT = .FALSE.
      IF ( XYPOS( 1 ) .LE. 0 ) THEN
         XINC = +1
         YINC = 0
         LIMIT = .TRUE.
      ELSEIF ( XYPOS( 1 ) .GE. CTWDIX( 0 ) - 1 ) THEN
         XINC = -1
         YINC = 0
         LIMIT = .TRUE.
      ELSEIF ( XYPOS( 2 ) .LE. 0 ) THEN
         XINC = 0
         YINC = +1
         LIMIT = .TRUE.
      ELSEIF ( XYPOS( 2 ) .GE. CTWDIY( 0 ) - 1 ) THEN
         XINC = 0
         YINC = -1
         LIMIT = .TRUE.
      ENDIF
      IF ( LIMIT ) THEN
         WORDS( 1 ) = 99
         WORDS( 2 ) = 29
         WORDS( 3 ) = XYPOS( 2 ) + YINC
         WORDS( 4 ) = XYPOS( 1 ) + XINC
         WORDS( 5 ) = 99
         WORDS( 6 ) = 29
         WORDS( 7 ) = XYPOS( 2 )
         WORDS( 8 ) = XYPOS( 1 )
         NUMWOR = 8
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      ENDIF

*   See if any buttons have been pressed
      DO 50 K = 1, CNTRIG
         IF ( IAND( PRESS, BUTTON( K - 1 ) ) .NE. 0 ) THEN
            TRIGS( K ) = .TRUE.
         ELSE
            TRIGS( K ) = .FALSE.
            STATE( K ) = .TRUE.
         ENDIF
  50  CONTINUE

*   Loop throught the interactions
      DO 80 J = 0, CINTN - 1

*   Remember the button number for this interaction
         K = CINTID( J ) + 1

*   If the interaction is for no visible effect then skip the next bit
         IF ( COBJTY( J ) .EQ. VNOEFF ) THEN
            GOTO 70
         ENDIF

*   See if any interactions use a locator
         IF ( CINTTY( J ) .EQ. VLOCAT ) THEN

*   Cursor
*   Note : If the cursor and memory scroll are both active then the
*   cursor will move twice as fast as the memory because the memory
*   scroll does not change the current position, but the current
*   position, and therefore the cursor, is changed by the GID.
            IF ( ( COBJTY( J ) .EQ. VCURS ) .AND.
     :           ( CINTOP( J ) .EQ. VMOVE ) ) THEN

*   Remember this cursor position
               CURX( COBJID( J ) ) = XYPOS( 1 ) + CMEMX( 0 )
               CURY( COBJID( J ) ) = XYPOS( 2 ) + CMEMY( 0 )

*   Memory or display
*   Note : If the cursor and memory scroll are both active then the
*   cursor will move twice as fast as the memory because the memory
*   scroll does not change the current position, but the current
*   position, and therefore the cursor, is changed by the GID.
            ELSEIF ( ( ( COBJTY( J ) .EQ. VMEM ) .OR.
     :                 ( COBJTY( J ) .EQ. VDISP ) ) .AND.
     :               ( CINTOP( J ) .EQ. VMOVE ) ) THEN

*   Set frame buffer to write
*   Ikon command 125 = '7D'X = Set frame buffer to write
               WORDS( 1 ) = 125
               WORDS( 2 ) = COBJID( J )

*   Set memory scroll
*   Ikon command 219 = 'DB'X = Pan /scroll current screen
*   The memory scroll can only occur in sterps of 16, so recalculate
*   xdiff in steps of 16
               XP = XYPOS( 1 )
               XL = XP - XDIFF
               DXDIFF = ( ( XP + SIGN( 8, XP ) ) / 16 ) * 16 -
     :                  ( ( XL + SIGN( 8, XL ) ) / 16 ) * 16
               XPOS = CSCROX( COBJID( J ) ) + DXDIFF
               YPOS = CSCROY( COBJID( J ) ) + YDIFF
               WORDS( 3 ) = 219
               WORDS( 4 ) = -XPOS
               WORDS( 5 ) = YPOS + CSCROF( 2 )
               NUMWOR = 5

*   Remember this memory position
*   Save this for all memories as they all scroll together
               DO 60 L = 0, CNMEM - 1
                  CSCROX( L ) = XPOS
                  CSCROY( L ) = YPOS
                  CMEMX( L ) = CMEMX( L ) + DXDIFF
                  CMEMY( L ) = CMEMY( L ) + YDIFF
  60           CONTINUE

*   Send these commands
               CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
               CALL IKNOUT( STATUS )

*   Move region of interest
            ELSEIF ( ( COBJTY( J ) .EQ. VROI ) .AND.
     :               ( CINTOP( J ) .EQ. VMOVE ) ) THEN

*   Calculate the size of the ROI
               RSIZEX = CROIXH( COBJID( J ) ) - CROIXL( COBJID( J ) )
               RSIZEY = CROIYH( COBJID( J ) ) - CROIYL( COBJID( J ) )

*   Remember this ROI position
               CROIXL( COBJID( J ) ) = XYPOS( 1 ) + CMEMX( 0 )
               CROIXH( COBJID( J ) ) = CROIXL( COBJID( J ) ) + RSIZEX
               CROIYL( COBJID( J ) ) = XYPOS( 2 ) + CMEMY( 0 )
               CROIYH( COBJID( J ) ) = CROIYL( COBJID( J ) ) + RSIZEY

*   Modify region of interest
*   One corner is anchored and the other moves with the GID
            ELSEIF ( ( COBJTY( J ) .EQ. VROI ) .AND.
     :               ( CINTOP( J ) .EQ. VMODI ) ) THEN
               XYMOD( 1 ) = XYPOS( 1 )
               XYMOD( 2 ) = XYPOS( 2 )

*   Rotate the look-up table
*   Do not include colour 0 in the rotation
            ELSEIF ( ( COBJTY( J ) .EQ. VVLUT ) .AND.
     :               ( CINTOP( J ) .EQ. VROTE ) ) THEN
               CALL IDLROT( VLUT, 1, LUTLEN, XDIFF, STATUS )
               CALL IKNWLT( DISPID, LUTNUM, 0, LUTLEN, VLUT, STATUS )
            ENDIF

*   See if it is a trigger for any other action
         ELSEIF ( CINTTY( J ) .EQ. VTRIGG ) THEN

*   Move object
            IF ( ( CINTOP( J ) .EQ. VMOVE ) .AND.
     :           STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               STATUS = IDI__NOTIM

*   Rotate object
            ELSEIF ( ( CINTOP( J ) .EQ. VROTE ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               STATUS = IDI__NOTIM

*   Increase object zoom
            ELSEIF ( ( CINTOP( J ) .EQ. VIZOOM ) .AND.
     :               ( ( COBJTY( J ) .EQ. VMEM ) .OR.
     :                 ( COBJTY( J ) .EQ. VDISP ) ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               ZOOMF = MIN( CMEMZ( 0 ) + 1, CZOOMR( 1 ) )
               CALL IKNWZM( DISPID, COBJID( J ), 1, ZOOMF, STATUS )

*   Decrease object zoom
            ELSEIF ( ( CINTOP( J ) .EQ. VDZOOM ) .AND.
     :               ( ( COBJTY( J ) .EQ. VMEM ) .OR.
     :                 ( COBJTY( J ) .EQ. VDISP ) ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               ZOOMF = MAX( CMEMZ( 0 ) - 1, CZOOMR( 0 ) )
               CALL IKNWZM( DISPID, COBJID( J ), 1, ZOOMF, STATUS )

*   Set object zoom to normal
            ELSEIF ( ( CINTOP( J ) .EQ. VNZOOM ) .AND.
     :               ( ( COBJTY( J ) .EQ. VMEM ) .OR.
     :                 ( COBJTY( J ) .EQ. VDISP ) ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               ZOOMF = 0
               CALL IKNWZM( DISPID, COBJID( J ), 1, ZOOMF, STATUS )

*   Blink object
            ELSEIF ( ( CINTOP( J ) .EQ. VBLINK ) .AND.
     :               ( COBJTY( J ) .EQ. VMEM ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.
               WORDS( 1 ) = 92
               IF ( COBJID( J ) .EQ. 0 ) THEN
                  WORDS( 2 ) = 3
               ELSEIF ( COBJID( J ) .EQ. 1 ) THEN
                  WORDS( 2 ) = 0
               ELSE
                  WORDS( 2 ) = 1
               ENDIF
               NUMWOR = 2
               CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
               CALL IKNOUT( STATUS )

*   Modify object ( ROI )
            ELSEIF ( ( CINTOP( J ) .EQ. VMODI ) .AND.
     :               ( COBJTY( J ) .EQ. VROI ) .AND.
     :               STATE( K ) .AND. TRIGS( K ) ) THEN
               STATE( K ) = .FALSE.

*   Switch the active corner to the opposite corner
               ROIMOD = 0
               IF ( ( XYMOD( 1 ) .LE. XYANC( 1 ) ) .AND.
     :              ( XYMOD( 2 ) .LE. XYANC( 2 ) ) ) THEN
                  ROIMOD = 1
               ELSEIF ( ( XYMOD( 1 ) .GT. XYANC( 1 ) ) .AND.
     :                  ( XYMOD( 2 ) .LT. XYANC( 2 ) ) ) THEN
                  ROIMOD = 2
               ELSEIF ( ( XYMOD( 1 ) .LT. XYANC( 1 ) ) .AND.
     :                  ( XYMOD( 2 ) .GT. XYANC( 2 ) ) ) THEN
                  ROIMOD = 3
               ENDIF

*   Anchor top-right hand corner
               IF ( ROIMOD .EQ. 0 ) THEN
                  TEMP( 1 ) = MIN( XYANC( 1 ), XYMOD( 1 ) )
                  TEMP( 2 ) = MIN( XYANC( 2 ), XYMOD( 2 ) )
                  XYANC( 1 ) = MAX( XYANC( 1 ), XYMOD( 1 ) )
                  XYANC( 2 ) = MAX( XYANC( 2 ), XYMOD( 2 ) )
                  XYMOD( 1 ) = TEMP( 1 )
                  XYMOD( 2 ) = TEMP( 2 )

*   Anchor bottom-left hand corner
               ELSEIF ( ROIMOD .EQ. 1 ) THEN
                  TEMP( 1 ) = MIN( XYANC( 1 ), XYMOD( 1 ) )
                  TEMP( 2 ) = MIN( XYANC( 2 ), XYMOD( 2 ) )
                  XYMOD( 1 ) = MAX( XYANC( 1 ), XYMOD( 1 ) )
                  XYMOD( 2 ) = MAX( XYANC( 2 ), XYMOD( 2 ) )
                  XYANC( 1 ) = TEMP( 1 )
                  XYANC( 2 ) = TEMP( 2 )

*   Anchor bottom-right hand corner
               ELSEIF ( ROIMOD .EQ. 2 ) THEN
                  TEMP( 1 ) = MAX( XYANC( 1 ), XYMOD( 1 ) )
                  TEMP( 2 ) = MIN( XYANC( 2 ), XYMOD( 2 ) )
                  XYMOD( 1 ) = MIN( XYANC( 1 ), XYMOD( 1 ) )
                  XYMOD( 2 ) = MAX( XYANC( 2 ), XYMOD( 2 ) )
                  XYANC( 1 ) = TEMP( 1 )
                  XYANC( 2 ) = TEMP( 2 )

*   Anchor top-left hand corner
               ELSEIF ( ROIMOD .EQ. 3 ) THEN
                  TEMP( 1 ) = MIN( XYANC( 1 ), XYMOD( 1 ) )
                  TEMP( 2 ) = MAX( XYANC( 2 ), XYMOD( 2 ) )
                  XYMOD( 1 ) = MAX( XYANC( 1 ), XYMOD( 1 ) )
                  XYMOD( 2 ) = MIN( XYANC( 2 ), XYMOD( 2 ) )
                  XYANC( 1 ) = TEMP( 1 )
                  XYANC( 2 ) = TEMP( 2 )
               ENDIF

*   If the ROI is visible then set up the rubber band box
               IF ( CROIVI( ROINUM ) .NE. 0 ) THEN

*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
                  WORDS( 1 ) = 124
                  WORDS( 2 ) = CROIBI( ROINUM )
                  WORDS( 3 ) = 125
                  WORDS( 4 ) = CROIBI( ROINUM )

*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 86 = '56'X = Rubber band anchor 1
                  WORDS( 5 ) = 99
                  WORDS( 6 ) = 86
                  WORDS( 7 ) = XYANC( 2 )
                  WORDS( 8 ) = XYANC( 1 )

*   Set the GID at the other corner
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
                  WORDS( 9 ) = 99
                  WORDS( 10 ) = 29
                  WORDS( 11 ) = XYMOD( 2 )
                  WORDS( 12 ) = XYMOD( 1 )

*   Enable the GID with rubber band from anchor 1
*   Ikon command 96 = '63'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
                  WORDS( 13 ) = 96
                  WORDS( 14 ) = 24
                  WORDS( 15 ) = 128 + 16 + 4

*   Set the GID position again
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon register 29 = '1D'X = GID position XY
                  WORDS( 16 ) = 99
                  WORDS( 17 ) = 29
                  WORDS( 18 ) = XYMOD( 2 )
                  WORDS( 19 ) = XYMOD( 1 )

                  NUMWOR = 19
                  CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
               ENDIF
            ENDIF

         ENDIF

*   Jump to the next interaction
  70     CONTINUE

  80  CONTINUE

*   See if any exit triggers have been fired
      FIRED = .FALSE.
      DO 100 K = 1, CNTRIG

*   Update repeat status if there is application specific code
         IF ( REPEAT .AND. LTRIG .AND.
     :        TRIGS( K ) .AND. STATE( K ) ) THEN
            STATE( K ) = .FALSE.
            REPEAT =.FALSE.
         ENDIF

*   Loop through the interactions
         DO 90 J = 0, CINTN - 1

*   See if a button press corresponds to an exit trigger
            IF ( ( CEXTRN( J ) .EQ. K - 1 ) .AND.
     :           TRIGS( K ) .AND. STATE( K ) ) THEN
               STATE( K ) = .FALSE.
               REPEAT = .FALSE.
               LUPDAT = .FALSE.
               FIRED = .TRUE.
            ENDIF

*   Exit application specific code if enabled trigger is fired
            IF ( TRIGS( K ) .AND. STATE( K ) .AND.
     :           ( CINTTY( J ) .EQ. VTRIGG ) .AND.
     :           ( CINTID( J ) .EQ. K - 1 ) ) THEN
               LUPDAT = .FALSE.
               STATE( K ) = .FALSE.
            ENDIF

*   Reset the clear flag if the interaction is application specific code
            IF ( FIRED .AND. ( CINTOP( J ) .EQ. VEXAPP ) ) THEN
               LDOCLR = .TRUE.
            ENDIF

  90     CONTINUE
 100  CONTINUE

*   If no exit trigger has been fired then return to loop
      IF ( REPEAT .OR. LUPDAT ) THEN
         GOTO 40
      ENDIF

*   Clear out any residual button presses before leaving the routine
*   Wait until all buttons are released otherwise Ikon can get
*   in a mess if it is closed down with a button held.
 110  CONTINUE
*   Ikon command 94 = '5E'X = Return menu status
      WORDS( 1 ) = 94
      NUMWOR = 1
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 1
      CALL IKNIBW( DISPID, NUMWOR, PRESS, STATUS )
*   Repeat if the buttons are not clear
      IF ( PRESS .NE. 0 ) GOTO 110

*   Clamp the GID if an exit trigger has been fired
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 28 = '1C'X = Graphic input device size XY
      IF ( FIRED ) THEN
         WORDS( 1 ) = 99
         WORDS( 2 ) = 28
         WORDS( 3 ) = 0
         WORDS( 4 ) = 0
         NUMWOR = 4
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Reset the screen to normal if there has been blinking
*   Ikon command 92 = '5C'X = Set frame grab control latch
         IF ( LBLINK ) THEN
            WORDS( 1 ) = 92
            WORDS( 2 ) = 1
            NUMWOR = 2
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         ENDIF

*   If an ROI has been modified then 
         IF ( LMDROI ) THEN

*   Fix the corrdinates of the modified box
            CROIXL( ROINUM ) = MIN( XYANC( 1 ), XYMOD( 1 ) )
            CROIYL( ROINUM ) = MIN( XYANC( 2 ), XYMOD( 2 ) )
            CROIXH( ROINUM ) = MAX( XYANC( 1 ), XYMOD( 1 ) )
            CROIYH( ROINUM ) = MAX( XYANC( 2 ), XYMOD( 2 ) )

*   If it is visible set it back to a fixed rectagle
            IF ( CROIVI( ROINUM .NE. 0 ) ) THEN

*   Set up fixed sized rectangle specified by the copy source area
*   Ikon command 164 = 'A4'X = Move to
               WORDS( 1 ) = 164
               WORDS( 2 ) = CROIXL( ROINUM ) - CMEMX( 0 )
               WORDS( 3 ) = CROIYL( ROINUM ) - CMEMY( 0 )
*   Ikon command 48 = '30'X = Mark copy source area
               WORDS( 4 ) = 48
               WORDS( 5 ) = CROIXH( ROINUM ) - CMEMX( 0 )
               WORDS( 6 ) = CROIYH( ROINUM ) - CMEMY( 0 )

*   Set the GID at the bottom left corner of the box
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 29 = '1D'X = Graphic input device position XY
               WORDS( 7 ) = 99
               WORDS( 8 ) = 29
               WORDS( 9 ) = CROIYL( ROINUM ) - CMEMY( 0 )
               WORDS( 10 ) = CROIXL( ROINUM ) - CMEMX( 0 )

*   Enable the GID with a fixed rectangle
*   Ikon command 96 = '63'X = Set register - 8 bit
*   Ikon address 24 = '18'X = Graphic input device mode
               WORDS( 11 ) = 96
               WORDS( 12 ) = 24
               WORDS( 13 ) = 128 + 32

*   Set the GID position again
*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon register 29 = '1D'X = GID position XY
               WORDS( 14 ) = 99
               WORDS( 15 ) = 29
               WORDS( 16 ) = CROIYL( ROINUM )
               WORDS( 17 ) = CROIXL( ROINUM )
               NUMWOR = 17
               CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            ENDIF
         ENDIF

*   Send these commands
         CALL IKNOUT( STATUS )

      ENDIF

  99  CONTINUE

      END

