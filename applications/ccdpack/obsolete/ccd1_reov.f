      SUBROUTINE CCD1_REOV( ID, MEMID, DSIZE, SPAN, THICK,
     :                      LBND1, UBND1, LBND2, UBND2, SCALE,
     :                      MAXPOS, X1, Y1, X2, Y2, NRET, STATUS )
*+
*  Name:
*     CCD1_REOV

*  Purpose:
*     Reads positions from the overlap region of two images displayed
*     using IDI.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_REOV( ID, MEMID, DSIZE, FRAC, THICK, LBND1, UBND1,
*                     LBND2, UBND2, SCALE, MAXPOS, X1, Y1, X2, Y2, NRET,
*                     STATUS )

*  Description:
*     This routine returns array coordinate positions which are
*     determined from within an overlap region of two images displayed
*     using IDI (and which have been manipulated by a routine such as
*     CCD1_DISP). The dimensions of the original image arrays are
*     related to the transfer windows sizes ( LBND1,UBND1 and
*     LBND2,UBND2 ) by the scale factor SCALE.
*
*     A description of the function of this routine
*        " Select image features   [ select | | exit section ]",
*     is drawn into the region above the DSIZE region. It
*     this is to be seen a suitable amount of space must be reserved on
*     the display.

*  Arguments:
*     ID = INTEGER (Given)
*        The IDI display identifier
*     MEMID = INTEGER (Given)
*        The IDI memory to display in.
*     DSIZE( 2 ) = INTEGER (Given)
*        Size of the scratch region of the display. The full size
*        image pair are drawn into this region. This should exclude
*        the palette region.
*     SPAN = INTEGER (Given)
*        The number of pixels along a span of the crosses drawn as
*        markers.
*     THICK = INTEGER (Given)
*        The thickness in device pixels of a cross
*     LBND1( 2 ) = INTEGER (Given)
*        The lower bounds of the transfer window of the first image.
*     UBND1( 2 ) = INTEGER (Given)
*        The upper bounds of the transfer window of the first image.
*     LBND2( 2 ) = INTEGER (Given)
*        The lower bounds of the transfer window of the second image.
*     UBND2( 2 ) = INTEGER (Given)
*        The upper bounds of the transfer window of the second image.
*     SCALE = DOUBLE PRECISION (Given)
*        Scale factor which maps display pixels to the original array
*        pixels.
*     MAXPOS = INTEGER (Given)
*        The maximum number of positions which may be returned.
*     X1( MAXPOS ) = DOUBLE PRECISION (Returned)
*        The X positions of the features identified within image one
*        (LBND1,UBND1). These are returned as coordinates in the
*        original array dimensions (scaled by SCALE).
*     Y1( MAXPOS ) = DOUBLE PRECISION (Returned)
*        The Y positions of the features identified within image one
*        (LBND1,UBND1). These are returned as coordinates in the
*        original array dimensions (scaled by SCALE).
*     X2( MAXPOS ) = DOUBLE PRECISION (Returned)
*        The X positions of the features identified within image two
*        (LBND2,UBND2). These are returned as coordinates in the
*        original array dimensions (scaled by SCALE).
*     Y2( MAXPOS ) = DOUBLE PRECISION (Returned)
*        The Y positions of the features identified within image two
*        (LBND2,UBND2). These are returned as coordinates in the
*        original array dimensions (scaled by SCALE).
*     NRET = INTEGER (Returned) 
*        The number of positions actually returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes
*     - an IDI device must be opened before calling this routine.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_ERR'          ! IDI errors etc.
      INCLUDE 'IDI_PAR'          ! IDI parameterisations

*  Arguments Given:
      INTEGER ID
      INTEGER MEMID
      INTEGER DSIZE( 2 )
      INTEGER SPAN
      INTEGER THICK
      INTEGER LBND1( 2 )
      INTEGER UBND1( 2 )
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )
      DOUBLE PRECISION SCALE
      INTEGER MAXPOS

*  Arguments Returned:
      DOUBLE PRECISION X1( MAXPOS )
      DOUBLE PRECISION Y1( MAXPOS )
      DOUBLE PRECISION X2( MAXPOS )
      DOUBLE PRECISION Y2( MAXPOS )
      INTEGER NRET

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) MESS    ! Output message string
      INTEGER EXTRN              ! Exit trigger number
      INTEGER INTID              ! Interactor identifier/number
      INTEGER INTOP              ! Interactive operation
      INTEGER INTTY              ! Interactor type
      INTEGER ISTAT              ! IDI status
      INTEGER LBND( 2 )          ! Lower bounds of overlap region
      INTEGER MESLEN             ! Length of message string
      INTEGER NARR               ! Dummy
      INTEGER NTRIGS( 1 )        ! Number of triggers available on this device
      INTEGER NVAL               ! Number of values returned
      INTEGER OBJID              ! Object identifier
      INTEGER OBJTY              ! Object type
      INTEGER OUTMID             ! Output memory no.
      INTEGER TRIGS( IDI__MAXTR ) ! Trigger values on return
      INTEGER UBND( 2 )          ! Upper bounds of overlap region
      INTEGER XC                 ! Cursor X position
      INTEGER XO                 ! Initial scroll offset (not changed)
      INTEGER XPOS( 5 )          ! Overlap regions X positions
      INTEGER XTEXT              ! Position of descriptive text
      INTEGER YC                 ! Cursor Y position
      INTEGER YO                 ! Initial scroll offset (not changed)
      INTEGER YPOS( 5 )          ! Overlap regions Y positions
      INTEGER YTEXT              ! Position of descriptive text
      INTEGER ZOOM               ! Current zoom factor
      INTEGER ZRANGE( 2 )        ! Range of zoom values
      LOGICAL KEYOK              ! Use of keyboard is allowed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set IDI status.
      ISTAT = IDI__OK

*  Check the configuration of the device. If more than 3 triggers are
*  available assume these mean that the keyboard is available. Always
*  limit exit trigger number to be less than equal to the maximum no.
      KEYOK = .FALSE.
      NARR = 1
      CALL IIDQCI( ID, INTRIG, NARR, NTRIGS, NVAL, ISTAT )
      IF ( NTRIGS( 1 ) .GT. 3 ) THEN

*  The keyboard is available.
         KEYOK = .TRUE.
      END IF

*  Set the exit trigger number.
      EXTRN = NTRIGS( 1 ) - 1

*  Set the overlap region.
      LBND( 1 ) = MAX( LBND1( 1 ), LBND2( 1 ) )
      LBND( 2 ) = MAX( LBND1( 2 ), LBND2( 2 ) )
      UBND( 1 ) = MIN( UBND1( 1 ), UBND2( 1 ) )
      UBND( 2 ) = MIN( UBND1( 2 ), UBND2( 2 ) )

*  And draw an outline around the region in which positions may be selected.
      XPOS( 1 ) = LBND( 1 ) 
      XPOS( 2 ) = UBND( 1 ) 
      XPOS( 3 ) = UBND( 1 ) 
      XPOS( 4 ) = LBND( 1 ) 
      XPOS( 5 ) = LBND( 1 )

      YPOS( 1 ) = LBND( 2 ) 
      YPOS( 2 ) = LBND( 2 ) 
      YPOS( 3 ) = UBND( 2 ) 
      YPOS( 4 ) = UBND( 2 ) 
      YPOS( 5 ) = LBND( 2 )
      CALL IIGPLY( ID, MEMID, XPOS, YPOS, 5, 4, 1, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set up interactions to move the cursor to allow positions to 
*  be read.
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Inform the user of their options.
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to control the cursor'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )
      
*  Set up an interaction to read a position using the left-hand button.
*  Use application specific code to perform this function.
      INTTY = 5
      INTID = 0
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to select an image feature in overlap'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to use the centre button. This is actual unused
*  but we want to send out a message to the user if they accidentally
*  press it. Use application specific code to perform this function.
      INTTY = 5
      INTID = 1
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  use an interaction of the right-hand button to exit.
      INTTY = 5
      INTID = 2
      OBJTY = 0
      OBJID = 0
      INTOP = 0
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99
      MESS( MESLEN + 1: ) = ' to exit this section'
      CALL MSG_SETC( 'MESS', MESS )
      CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  If available set up two interactions to zoom in and out.
      IF ( KEYOK ) THEN

*  Inform the user additional operations are available.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '  The following options are also '//
     :   'available on this device:', STATUS )

*  Find out the range of zoom factors.
         NARR = 2
         CALL IIDQCI( ID, IZOOMR, NARR, ZRANGE, NVAL, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Get the current zoom.
         CALL IIZRSZ( ID, MEMID, XO, YO, ZOOM, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Set up an interaction to zoom in the display using the I key.
*  Use application specific code.
         INTTY = 5
         INTID = 22
         OBJTY = 5
         OBJID = MEMID
         INTOP = 3
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to zoom in'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to zoom out the display using the O key.
         INTTY = 5
         INTID = 28
         OBJTY = 5
         OBJID = MEMID
         INTOP = 4
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to zoom out'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to scroll the memory using the arrow keys.
         INTTY = 0
         INTID = 1
         OBJTY = 5
         OBJID = MEMID
         INTOP = 1
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to scroll the display'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to unzoom and unscroll the memory.
         INTTY = 5
         INTID = 16
         OBJTY = 5
         OBJID = MEMID
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to cancel zoom and scroll'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )

*  Set up an interaction to abort the program using the q key.
         INTTY = 5
         INTID = 30
         OBJTY = 5
         OBJID = MEMID
         INTOP = 0
         CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :                ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIIQID( ID, INTTY, INTID, MESS, MESLEN, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         MESS( MESLEN + 1: ) = ' to quit the application'
         CALL MSG_SETC( 'MESS', MESS )
         CALL MSG_OUT( ' ', '    ^MESS', STATUS )
      END IF

*   Display the cursor by setting its visibility to .TRUE.
      CALL IICRCP( ID, MEMID, 0, XC, YC, OUTMID, ISTAT )
      CALL IICWCP( ID, MEMID, 0, XC, YC, ISTAT )
      CALL IICSCV( ID, 0, 1, ISTAT )
      IF ( STATUS .NE. IDI__OK ) GO TO 99

*-----------------------------------------------------------------------
*  Main loop
*-----------------------------------------------------------------------
*
*  Set the number of output positions.
      NRET = 0 

*  Describe the function of this routine by writting text to the
*  display.
      XTEXT = MAX( 1, INT( DSIZE( 1 ) * 0.025 ) )
      YTEXT = DSIZE( 2 )
      CALL IIGTXT( ID, MEMID,
     : ' Select image features   [ select feature |  | exit section ]'
     :            ,XTEXT, YTEXT, 0, 0, 1, 0, ISTAT)

  10  CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*   Execute the interactions.
      CALL IIIEIW( ID, TRIGS, ISTAT )
      IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  If left-hand trigger has been pressed then read the new position.
      IF ( TRIGS( 1 ) .NE. 0 ) THEN

*  Read the cursor position relative to the memory origin.
         CALL IICRCP( ID, MEMID, 0, XC, YC, OUTMID, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99

*  Is the cursor in the allowed overlap region?
         IF ( XC .GE. LBND( 1 ) .AND. XC .LE. UBND( 1 ) .AND.
     :        YC .GE. LBND( 2 ) .AND. YC .LE. UBND( 2 ) ) THEN 

*  Yes it is. Determine the positions with respect to the two transfer
*  windows and scale the values to array coordinates.
           NRET = NRET + 1
           IF ( NRET .LE. MAXPOS ) THEN 
              X1( NRET ) = ( XC - LBND1( 1 ) ) * SCALE
              Y1( NRET ) = ( YC - LBND1( 2 ) ) * SCALE
              X2( NRET ) = ( XC - LBND2( 1 ) ) * SCALE
              Y2( NRET ) = ( YC - LBND2( 2 ) ) * SCALE
           ELSE
              CALL MSG_SETI( 'MAXPOS', MAXPOS )
              CALL MSG_OUT( ' ', '   Warning - maximum number '//
     : ' (^MAXPOS) of positions exceeded. This position not used.', 
     : STATUS )
           END IF

*  And mark the position.
            CALL CCD1_DRAWI( ID, MEMID, XC, YC, SPAN, THICK, 4, ISTAT )
            IF ( ISTAT .NE. IDI__OK ) GO TO 99
         END IF
         GO TO 10 
      ELSE IF ( TRIGS( 17 ) .NE. 0 ) THEN 

*  C keyboard keypress - unzoom and unscroll the display memory.
         CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         CALL IIZWSC( ID, MEMID, 1, 0, 0, ISTAT )
         IF ( ISTAT .NE. IDI__OK ) GO TO 99
         GO TO 10

      ELSE IF ( TRIGS( 3 ) .NE. 0 ) THEN 

*  Trap exit trigger - and do so.

      ELSE IF ( TRIGS( 31 ) .NE. 0 ) THEN

*  Call to abort the routine.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ABORT', '  Abort selected', STATUS )
      ELSE IF ( TRIGS( 2 ) .NE. 0 ) THEN 

*  Press of unused trigger.
         CALL MSG_BELL( STATUS )
         CALL MSG_OUT( ' ', '  Trigger not in use', STATUS )
         GO TO 10
      ELSE

*  Do nothing just return for next position.
         GO TO 10 
      END IF

*

*  Exit on error label.
 99   CONTINUE

*  If an IDI error has occurred. Report it set STATUS and exit.
      IF ( ISTAT .NE. IDI__OK ) THEN
         STATUS = ISTAT
         CALL IIDERR( ISTAT, MESS, MESLEN )
         CALL MSG_SETC( 'MESS', MESS( : MESLEN ) )
         CALL ERR_REP( ' ', '^MESS', STATUS )
      END IF

*  Unzoom the display if it is zoomed.
      IF ( KEYOK .AND. ISTAT .EQ. IDI__OK ) THEN
         CALL IIZRSZ( ID, MEMID, XO, YO, ZOOM, ISTAT )
         IF ( ZOOM .NE. 0 ) CALL IIZWZM( ID, MEMID, 1, 0, ISTAT )
         IF ( XO .NE. 0 .OR. YO .NE. 0 ) THEN
            CALL IIZWSC( ID, MEMID, 1, 0, 0, ISTAT )
         END IF
      END IF

*  Erase the descriptive text.
      CALL IIGTXT( ID, MEMID,
     : ' Select image features   [ select feature |  | exit section ]',
     :              XTEXT, YTEXT, 0, 0, 0, 0, ISTAT)

*  Make the cursor invisible and stop all interactive input.
      CALL IICSCV( ID, 0, 0, ISTAT )
      CALL IIISTI( ID, ISTAT )
      END 
* $Id$
