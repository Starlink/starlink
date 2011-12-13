      SUBROUTINE IDI_TEST ( STATUS )
*+
*  Name:
*     IDI_TEST
*  Purpose:
*     TEST IDI
*  Language:
*     FORTRAN
*  Type of Module:
*     ADAM A-task
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Copyright:
*     Copyright (C) 1989, 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     NE: Nick Eaton  (Durham University)
*  History:
*     May 1989 (NE):
*        Original version
*     March 1991 (NE):
*        Update to use NDFs and work with X-windows interface
*-
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'CNF_PAR'          ! CNF_PVAL
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'IDI_PAR'          ! IDI global constants
*  Status:
      INTEGER STATUS             ! Global status
*  Local variables:
      CHARACTER * 64 TEXT
      INTEGER TRIGS( IDI__MAXTR )
      INTEGER DEPTH, DIRECN, DSIZE(2), EXTRN, ID, INDF, INTID, INTOP,
     :        INTTY, IPIN, LBND(2), LTEXT, MEMID, NARR, NCHAR, NDATA,
     :        NDIM, NEL, NUMCUR, NVAL, NX, NY, OBJID, OBJTY, OUTMID,
     :        PACK, UBND(2), XC, XOFF, XSTART, YC, YOFF, YSTART
*.
*   Check inherited global status.
      IF ( STATUS .NE. IDI__OK ) GOTO 99

*   Get the picture to plot. Should be 2-dimensional.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )
      CALL NDF_MAP( INDF, 'DATA', '_INTEGER', 'READ', IPIN, NEL, STATUS)
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
      IF ( ( STATUS .NE. IDI__OK ) .OR. ( NDIM .GT. 2 ) ) GOTO 99

*   Calculate image size
      NX = UBND( 1 ) - LBND( 1 ) + 1
      NY = UBND( 2 ) - LBND( 2 ) + 1
      NDATA = NX * NY

*   Open IDI for a device obtained through the parameter system
*   Force a reset by using 'WRITE' mode
      CALL IDI_ASSOC( 'DEVICE', 'WRITE', ID, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 99

*   Inquire the physical size of the display using Query Capabilities
*   Integer code 12 ( ISIZED ) returns the screen size in pixels
      NARR = 2
      CALL IIDQCI( ID, ISIZED, NARR, DSIZE, NVAL, STATUS )

*   Check the image size against the display size
      IF ( ( NX .GT. DSIZE( 1 ) ) .OR. ( NY .GT. DSIZE( 2 ) ) ) GOTO 99

*   Inquire the depth of the first (default) memory using Query Capabilities
*   Integer code 22 ( IMEMDE ) returns the memory depth
      NARR = 1
      CALL IIDQCI( ID, IMEMDE, NARR, DEPTH, NVAL, STATUS )

*   Set up the transfer window to be the same size as the input image
*   and to be centered in the middle of the screen; XOFF, YOFF define
*   the position of the lower left corner of the transfer window. Plot
*   the image in the base memory ( 0 ) and loading from bottom to top ( 0 )
      MEMID = 0
      DIRECN = 0
      XOFF = MAX( 0, ( DSIZE( 1 ) - NX ) / 2 )
      YOFF = MAX( 0, ( DSIZE( 2 ) - NY ) / 2 )
      CALL IIMSTW( ID, MEMID, DIRECN, NX, NY, DEPTH, XOFF, YOFF,
     :             STATUS )

*   Write the image to the screen starting at position 0, 0 in the
*   transfer window. The data is taken from the lowest byte of each
*   integer word in the data array ( defined by PACK = 1, DEPTH = 8 ).
      PACK = 1
      XSTART = 0
      YSTART = 0
      CALL IIMWMY( ID, MEMID, %VAL( CNF_PVAL(IPIN) ), NDATA, DEPTH,PACK,
     :             XSTART, YSTART, STATUS )

*   Abort if an error has occured
      IF ( STATUS .NE. IDI__OK ) THEN

*   Obtain a meaningful IDI error message
         CALL IIDERR( STATUS, TEXT, NCHAR )

*   Output this message
         CALL ERR_REP( 'IDI_WMY', TEXT, STATUS )
         GOTO 99
      ENDIF

*   Display the memory by setting its visibility to .TRUE.
      CALL IIMSMV( ID, MEMID, 1, 1, STATUS )

*   Set up interactions to move the cursor and zoom the memory
*   Set up the mouse ( interactor type = 0, interactor id = 0 ) to
*   control the cursor ( object type = 1, object id = 0 ) by moving
*   it ( interactive operation = 1 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      TEXT( LTEXT + 1 : ) = ' to control cursor'
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Set up the left hand button ( interactor type = 5, interactor id = 0 )
*   to control the memory ( object type = 5, object id = 0 ) by increasing
*   the zoom ( interactive operation = 3 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 5
      INTID = 0
      OBJTY = 5
      OBJID = MEMID
      INTOP = 3
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      TEXT( LTEXT + 1 : ) = ' to increase zoom'
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Set up the centre button ( interactor type = 5, interactor id = 1 )
*   to control the memory ( object type = 5, object id = 0 ) by decreasing
*   the zoom ( interactive operation = 4 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 5
      INTID = 1
      OBJTY = 5
      OBJID = MEMID
      INTOP = 4
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      TEXT( LTEXT + 1 : ) = ' to decrease zoom'
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Inquire the exit trigger operation and instruct the user
      CALL IIIQID( ID, INTTY, EXTRN, TEXT, LTEXT, STATUS )
      TEXT( LTEXT + 1 : ) = ' to exit'
      CALL MSG_OUT( ' ', TEXT, STATUS )

*   Move the cursor to the middle of the screen. A memory id = -1 sets
*   the cursor position relative to the screen origin.
      XC = DSIZE( 1 ) / 2
      YC = DSIZE( 2 ) / 2
      NUMCUR = 0
      CALL IICINC( ID, -1, NUMCUR, 1, 2, XC, YC, STATUS )

*   Display the cursor by setting its visibility to .TRUE.
      CALL IICSCV( ID, NUMCUR, 1, STATUS )

*   Execute the interactions.
      CALL IIIEIW( ID, TRIGS, STATUS )

*   Read the cursor position relative to the memory origin.
      CALL IICRCP( ID, MEMID, NUMCUR, XC, YC, OUTMID, STATUS )

*   Output the cursor position.
      CALL MSG_SETI( 'XC', XC )
      CALL MSG_SETI( 'YC', YC )
      CALL MSG_OUT( ' ', 'Cursor position is ^XC , ^YC', STATUS )

*   Undisplay the cursor by setting its visibility to .FALSE.
      CALL IICSCV( ID, NUMCUR, 0, STATUS )

  99  CONTINUE

*   Close down IDI using the parameter system
      CALL IDI_CANCL( 'DEVICE', STATUS )

*   Close the data file
      CALL NDF_END( STATUS )

      END

