************************************************************************

      SUBROUTINE IICINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC, YC,
     :                   STATUS )

*+
*  Name:
*     IICINC
*
*  Purpose:
*     Initialize Cursor
*
*  Invocation:
*     CALL IICINC( DISPID, MEMID, NUMCUR, SHAPE, COLOR, XC, YC, STATUS )
*
*  Description:
*     The cursor position is given in memory coordinates in the
*     specified memory unless the memory ideintifier is -1, in which
*     case the cursor position is in screen coordinates. If cursor
*     position is outside the specified memory of off the screen the
*     cursor position is not changed.
*
*     The following cursor shapes are defined:
*     0 - implementation dependent
*     1 - cross hair ( full screen )
*     2 - cross
*     3 - open cross
*     4 - square
*     5 - diamond
*     6 - circle
*     7 - diagonal arrow
*     Devices with programmable cursors may use cursor shape codes >7 to
*     access special cursor shapes.
*
*     The cursor colour is defined as for Polyline and Plot Text in a
*     graphics memory except that a value of 0 is implementation
*     dependent. If the specified cursor shape or cursor colour is not
*     available then the shape and colour will be implementation
*     dependent.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     SHAPE = INTEGER (Given)
*        Cursor shape
*     COLOR = INTEGER (Given)
*        Cursor colour
*     XC = INTEGER (Given)
*        X cursor position
*     YC = INTEGER (Given)
*        Y cursor position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NUMCUR
      INTEGER SHAPE
      INTEGER COLOR
      INTEGER XC
      INTEGER YC

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNINC( DEVID( DISPID ), MEMID, NUMCUR, SHAPE, COLOR,
     :                XC, YC, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWINC( DEVID( DISPID ), MEMID, NUMCUR, SHAPE, COLOR,
     :                XC, YC, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IICRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID,
     :                   STATUS )

*+
*  Name:
*     IICRCP
*
*  Purpose:
*     Read Cursor Position
*
*  Invocation:
*     CALL IICRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID, STATUS )
*
*  Description:
*     The returned cursor position is relative to the input memory's
*     origin unless the input memory identifier is -1, in which case the
*     position is relative to the display origin and the output memory
*     identifier identifies the memory to which the cursor is currently
*     pointing. Otherwise the output memory identifier is equal to the
*     input memory identifier.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INMID = INTEGER (Given)
*        Input memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     XC = INTEGER (Returned)
*        X position
*     YC = INTEGER (Returned)
*        Y position
*     OUTMID = INTEGER (Returned)
*        Output memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INMID
      INTEGER NUMCUR

*  Arguments Returned:
      INTEGER XC
      INTEGER YC
      INTEGER OUTMID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRCP( DEVID( DISPID ), INMID, NUMCUR, XC, YC, OUTMID,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRCP( DEVID( DISPID ), INMID, NUMCUR, XC, YC, OUTMID,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IICSCV( DISPID, NUMCUR, LVIS, STATUS )

*+
*  Name:
*     IICSCV
*
*  Purpose:
*     Set Cursor Visibility
*
*  Invocation:
*     CALL IICSCV( DISPID, NUMCUR, LVIS, STATUS )
*
*  Description:
*     Select whether a cursor position is indicated by some visible mark
*     on the display. If visibility is 'true' then the cursor becomes
*     visible.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NUMCUR
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSCV( DEVID( DISPID ), NUMCUR, LVIS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSCV( DEVID( DISPID ), NUMCUR, LVIS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IICWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )

*+
*  Name:
*     IICWCP
*
*  Purpose:
*     Write Cursor Position
*
*  Invocation:
*     CALL IICWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )
*
*  Description:
*     The given cursor position is relative to the specified memory's
*     origin unless the memory identifier is -1, in which case the
*     position is relative to the display origin. A display may have a
*     number of cursors, each of which can be moved independently. The
*     position of any cursor ( selected by the cursor number ) can be
*     written at any time, regardless of whether or not the cursor is
*     visible.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NUMCUR = INTEGER (Given)
*        Cursor number
*     XC = INTEGER (Given)
*        X position
*     YC = INTEGER (Given)
*        Y position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NUMCUR
      INTEGER XC
      INTEGER YC

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWCP( DEVID( DISPID ), MEMID, NUMCUR, XC, YC, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWCP( DEVID( DISPID ), MEMID, NUMCUR, XC, YC, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID,
     :                   STATUS )

*+
*  Name:
*     IIDAMY
*
*  Purpose:
*     Allocate Memory
*
*  Invocation:
*     CALL IIDAMY( DISPID, XSIZE, YSIZE, MEMDEP, MEMTYP, MEMID, STATUS )
*
*  Description:
*     Allocate a new memory of the requested type. The new memory
*     identifier is returned by the interface, and is used subsequently
*     to reference the newly allocated memories. The memory depth is
*     given in bits, and the memory type is the same as is defined for
*     Query Defined Configuration. An error is returned if the memory
*     request cannot be satisfied.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XSIZE = INTEGER (Given)
*        Requested X size
*     YSIZE = INTEGER (Given)
*        Requested Y size
*     MEMDEP = INTEGER (Given)
*        Requested memory depth
*     MEMTYP = INTEGER (Given)
*        Requested memory type
*     MEMID = INTEGER (Returned)
*        New memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER MEMDEP
      INTEGER MEMTYP

*  Arguments Returned:
      INTEGER MEMID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNAMY( DEVID( DISPID ), XSIZE, YSIZE, MEMDEP, MEMTYP,
     :                MEMID, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWAMY( DEVID( DISPID ), XSIZE, YSIZE, MEMDEP, MEMTYP,
     :                MEMID, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDCLO( DISPID, STATUS )

*+
*  Name:
*     IIDCLO
*
*  Purpose:
*     Close Display
*
*  Invocation:
*     CALL IIDCLO( DISPID, STATUS )
*
*  Description:
*     Close a device after updating it and releasing all resources. Once
*     a display has been closed if cannot be referred to again unless it
*     is re-opened with a call to Open Display.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Reformatted and added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   Close the device whatever the status
*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNCLO( DEVID( DISPID ), STATUS )
         DTYPE( DISPID ) = 0
         DISPID = 0

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWCLO( DEVID( DISPID ), STATUS )
         DTYPE( DISPID ) = 0
         DISPID = 0

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDENC( DISPID, STATUS )

*+
*  Name:
*     IIDENC
*
*  Purpose:
*     Enable configuration
*
*  Invocation:
*     CALL IIDENC( DISPID, STATUS )
*
*  Description:
*     Mark the beginning of a sequence of configuration operations. It
*     must be used only if additional memory configurations are required
*     and a "negotiation" must be carried out instead.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNENC( DEVID( DISPID ), STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWENC( DEVID( DISPID ), STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDERR( STATUS, MESSAG, MESLEN )

*+
*  Name:
*     IIDERR
*
*  Purpose:
*     Get Error
*
*  Invocation:
*     CALL IIDERR( STATUS, MESSAG, MESLEN )
*
*  Description:
*     Convert an integer error code returned by an IDI routine into a
*     character string that contains the text of the error message.
*     If the error code is not defined an empty string is returned.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.
*     MESSAG = CHARACTER * ( * ) (Returned)
*        Error message.
*     MESLEN = INTEGER (Returned)
*        Length of error message.
*
*  Algorithm:
*     Try each driver in turn to see if it understands the error
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     January 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER STATUS

*  Arguments Returned:
      CHARACTER * ( * ) MESSAG
      INTEGER MESLEN
*.

*   Try calling each driver in turn.
*   See if the status value is an Ikon error
      CALL IKNERR( STATUS, MESSAG, MESLEN )

*   If no error message was found then see if it is an X-windows error
      IF ( MESLEN .EQ. 0 ) THEN
         CALL IXWERR( STATUS, MESSAG, MESLEN )
      ENDIF

      END

************************************************************************

      SUBROUTINE IIDIAG( DISPID, OUTID, STATUS )

*+
*  Name:
*     IIDIAG
*
*  Purpose:
*     Diagnostic Routine
*
*  Invocation:
*     CALL IIDIAG( DISPID, OUTID, STATUS )
*
*  Description:
*     Write implementation specific diagnostic data to the selected
*     output device. The meaning of the output identifier is
*     implementation dependent; it could be, for example a FORTRAN unit
*     number. Output identifier 0 is reserved for ASCII text output to
*     the alphanumeric memory of the image display device.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     OUTID = INTEGER (Given)
*        Output identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER OUTID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNIAG( DEVID( DISPID ), OUTID, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWIAG( DEVID( DISPID ), OUTID, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDOPN( DEVNAM, DISPID, STATUS )

*+
*  Name:
*     IIDOPN
*
*  Purpose:
*     Open Display
*
*  Invocation:
*     CALL IIDOPN( DEVNAM, DISPID, STATUS )
*
*  Description:
*     Allocate a display ( or part of a display if one display is being
*     treated as more than one logical IDI device ) and initialise the
*     state of the IDI software to mirror the state of the display. If
*     possible the state of the display at a preceding call to Close
*     Display is recovered, otherwise the display is reset to an
*     appropriate default state.
*
*     The device name identifies the physical device, the display type
*     and the default configuration for that device. The display
*     identifier is used to refer to the device in subsequent calls to
*     IDI routines. Application programs should not manipulate this
*     identifier in any way apart from passing it to another routine.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DEVNAM = CHARACTER * ( * ) (Given)
*        Device name.
*     DISPID = INTEGER (Returned)
*        Display identifier.
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Use GNS to translate the device name then call the device
*     specific routines.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     July 1989 (NE):
*        Use GNS for device names
*     December 1990 (NE):
*        Reformatted routine and added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      CHARACTER * ( * ) DEVNAM

*  Arguments Returned:
      INTEGER DISPID

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER * 10 GNSNAM, GNSTYP

      INTEGER J, LENAME, TEMPID

*   Flag to indicate if the arrays are to be initialised
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
*.

*   Reset status
      STATUS = IDI__OK

*   If this is the first call to this routine then reset the
*   top-level display identifiers
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         DO J = 1, NUMIDS
            DTYPE( J ) = 0
         ENDDO
      ENDIF

*   Assign a top-level display identifier.
*   Look for an empty slot in the array
      DISPID = 0
      DO J = NUMIDS, 1, -1
         IF ( DTYPE( J ) .EQ. 0 ) THEN
            DISPID = J
         ENDIF
      ENDDO

*   If there were no empty slots then indicate an error
      IF ( DISPID .EQ. 0 ) THEN
         STATUS = IDI__COOVF
         GOTO 99
      ENDIF

*   Use GNS to translate the given device name into something IDI
*   will recognise
      CALL GNS_TNI( DEVNAM, GNSTYP, GNSNAM, STATUS )

*   Check that no errors have occured
      IF ( STATUS .EQ. IDI__OK ) THEN

*   If it is an IKON name then open that device
         IF ( GNSTYP( 1:2 ) .EQ. 'IK' ) THEN
            CALL IKNOPN( GNSNAM, TEMPID, STATUS )
            DTYPE( DISPID ) = D_IKON
            DEVID( DISPID ) = TEMPID

*   If it is an X-windows name then open that device
*   Pass the length of the name string as an argument
         ELSEIF ( GNSTYP( 1:2 ) .EQ. 'XW' ) THEN
            LENAME = LEN( GNSNAM )
            CALL IXWOPN( GNSNAM, LENAME, TEMPID, STATUS )
            DTYPE( DISPID ) = D_XWIN
            DEVID( DISPID ) = TEMPID

*   Else it does not recognise the device
         ELSE
            STATUS = IDI__DEVNM
         ENDIF

      ELSE
         STATUS = IDI__DEVNM
      ENDIF

*   If an error has occured then clear the top-level identifier
      IF ( STATUS .NE. IDI__OK ) THEN
         DTYPE( DISPID ) = 0
         DISPID = 0
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IIDQCI
*
*  Purpose:
*     Query Capabilities Integer
*
*  Invocation:
*     CALL IIDQCI( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*  Description:
*     Detemine the device capabilities that are not configuration
*     dependent and that describe the physical device ( or image display
*     station that is being used. The allowed values of capability,
*     which is a symbolic integer, are listed in appendix A of the
*     specification document.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CAPID = INTEGER (Given)
*        Capability
*     NARR = INTEGER (Given)
*        Size of output array
*     OUTARR( NARR ) = INTEGER (Returned)
*        Output array
*     NOUT = INTEGER (Returned)
*        Number of values returned
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*     April 1991 (NE):
*        Initialise the number of return values
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CAPID
      INTEGER NARR

*  Arguments Returned:
      INTEGER OUTARR( NARR )
      INTEGER NOUT

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Initialise the number of return values
      NOUT = 0

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNQCI( DEVID( DISPID ), CAPID, NARR, OUTARR, NOUT,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWQCI( DEVID( DISPID ), CAPID, NARR, OUTARR, NOUT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )

*+
*  Name:
*     IIDQCR
*
*  Purpose:
*     Query Capabilities Real
*
*  Invocation:
*     CALL IIDQCR( DISPID, CAPID, NARR, OUTARR, NOUT, STATUS )
*
*  Description:
*     Determine the device capabilities that are not configuration
*     dependent and that describe the physical device ( or image display
*     station ) that is being used. The allowed values of capability,
*     which is a symbolic integer, are listed in appendix A of the
*     specification document.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CAPID = INTEGER (Given)
*        Capability
*     NARR = INTEGER (Given)
*        Size of output array
*     OUTARR( NARR ) = REAL (Returned)
*        Output array
*     NOUT = INTEGER (Returned)
*        Number of values returned
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*     April 1991 (NE):
*        Initialise the number of return values
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CAPID
      INTEGER NARR

*  Arguments Returned:
      REAL OUTARR( NARR )
      INTEGER NOUT

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Initialise the number of return values
      NOUT = 0

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNQCR( DEVID( DISPID ), CAPID, NARR, OUTARR, NOUT,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWQCR( DEVID( DISPID ), CAPID, NARR, OUTARR, NOUT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON,
     :                   MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP,
     :                   NMEM, STATUS )

*+
*  Name:
*     IIDQDC
*
*  Purpose:
*     Query Defined Configuration
*
*  Invocation:
*     CALL IIDQDC( DISPID, NCONF, MEMTYP, NMEMAX, MODCON, MEMID,
*    :             MEMSIX, MEMSIY, MEMDEP, ITTDEP, NMEM, STATUS )
*
*  Description:
*     The possible configurations available on a device are numbered 0
*     through n-1. For each configuration the configuration mode and
*     a list of each of the three types of memories ( image, text and
*     graphics ) can be obtained. A seperate call is required for each
*     type of memory with the memory type parameter set to the
*     appropriate value. The allowed values for memory type are:
*     1 - image memory
*     2 - text memory
*     4 - graphics memory.
*     The memory type parameter is effectively a bit mask. Memories
*     which must serve a dual purpose are indicated by summing the
*     codes representing each function ( so that, for example, a single
*     memory which is to be used for both image and graphics would have
*     an integer code of 5 ).
*
*     The routine returns a list of memory identifiers and, for each
*     memory, its size, depth, depth of its associated ITTs, and the
*     total number of memories of the specified type. If the number of
*     available memories is greater the the value of the maximum number
*     of memories parameter, only maximum number of memories memory
*     identifiers will be returned. Note that in any one configuration
*     no two memories have the same identifier even if they are of
*     different types.
*
*     Configuration mode indicates the mode in which the device is set
*     up ( monochrome, pseudo-colour or true-colour [R,G,B] ). This mode
*     reflects the internal state of the binding of the various LUTs to
*     video output paths. The returned values are:
*     0 - monochrome mode
*     1 - pseudo-colour mode
*     2 - true-colour [R,G,B] mode
*     It is possible to use the Select Display Path routine to change
*     the LUT bindings ( and thus the display mode ) after a
*     configuration has been selected if the hardware is capable of
*     changing colour modes without a total reset or reconfiguration.
*
*     A call to Query Defined Configuration with configuration number
*     set to -1 will return information about the currently slected
*     configuration ( as selected by Open Display or by Select
*     Configuration ).
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     MEMTYP = INTEGER (Given)
*        Memory type
*     NMEMAX = INTEGER (Given)
*        Maximum number of memories
*     MODCON = INTEGER (Returned)
*        Configuration mode
*     MEMID( * ) = INTEGER (Returned)
*        List of memory identifiers
*     MEMSIX( * ) = INTEGER (Returned)
*        Memory sizes in x
*     MEMSIY( * ) = INTEGER (Returned)
*        Memory sizes in y
*     MEMDEP( * ) = INTEGER (Returned)
*        Memory depths
*     ITTDEP( * ) = INTEGER (Returned)
*        Memory ITT depths
*     NMEM = INTEGER (Returned)
*        Number of memories
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF
      INTEGER MEMTYP
      INTEGER NMEMAX

*  Arguments Returned:
      INTEGER MODCON
      INTEGER MEMID( * )
      INTEGER MEMSIX( * )
      INTEGER MEMSIY( * )
      INTEGER MEMDEP( * )
      INTEGER ITTDEP( * )
      INTEGER NMEM

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNQDC( DEVID( DISPID ), NCONF, MEMTYP, NMEMAX, MODCON,
     :                MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP, NMEM,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWQDC( DEVID( DISPID ), NCONF, MEMTYP, NMEMAX, MODCON,
     :                MEMID, MEMSIX, MEMSIY, MEMDEP, ITTDEP, NMEM,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
     :                   NITT, NCURS, STATUS )

*+
*  Name:
*     IIDQDV
*
*  Purpose:
*     Query Device Characteristics
*
*  Invocation:
*     CALL IIDQDV( DISPID, NCONF, XSIZE, YSIZE, DEPTH, NVLUT,
*    :                   NITT, NCURS, STATUS )
*
*  Description:
*     Inquire device parameters that are not configuration dependent and
*     that describe the basic characteristics of the physical device
*     being used.
*
*     Display size refers to the largest area of memory that can be
*     displayed on the screen, not the size of the pixel memories of
*     the device. Display depth is the number of different intensity
*     levels that the device is capable of displaying, expressed as the
*     number of bits in the digital to analogue converters.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Returned)
*        Number of available configurations
*     XSIZE = INTEGER (Returned)
*        Display size in x ( pixels )
*     YSIZE = INTEGER (Returned)
*        Display size in y ( pixels )
*     DEPTH = INTEGER (Returned)
*        Display depth ( number of bits in DAC's )
*     NVLUT = INTEGER (Returned)
*        Number of VLUT's in device
*     NITT = INTEGER (Returned)
*        Number of ITT's per image memory
*     NCURS = INTEGER (Returned)
*        Number of cursors
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER NCONF
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER DEPTH
      INTEGER NVLUT
      INTEGER NITT
      INTEGER NCURS

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNQDV( DEVID( DISPID ), NCONF, XSIZE, YSIZE, DEPTH,
     :                NVLUT, NITT, NCURS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWQDV( DEVID( DISPID ), NCONF, XSIZE, YSIZE, DEPTH,
     :                NVLUT, NITT, NCURS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDRLC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IIDRLC
*
*  Purpose:
*     Release Configuration
*
*  Invocation:
*     CALL IIDRLC( DISPID, NCONF, STATUS )
*
*  Description:
*     Return the physical memory allocated to the specified
*     configuration number to the pool of unused memory available for
*     the creation of new memories. The configuration number and its
*     associated memory identifiers, defined with Allocate Memory,
*     become invalid but may be reused when creating new memories in a
*     subsequent negotiation for a new configuration.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRLC( DEVID( DISPID ), NCONF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRLC( DEVID( DISPID ), NCONF, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDRST( DISPID, STATUS )

*+
*  Name:
*     IIDRST
*
*  Purpose:
*     Reset Display
*
*  Invocation:
*     CALL IIDRST( DISPID, STATUS )
*
*  Description:
*     Reset the display hardware to a "known" initial state. All
*     memories are cleared, the LUTs and ITTs are loaded with linear
*     tables and the device is left in the configuration last selected
*     ( either by a call to Select Configuration or by Open Display ).
*
*     A reset may need to be performed after a change of device
*     configuration for those devices which require a "hard reset" to
*     alter operating modes.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   Reset the display
*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRST( DEVID( DISPID ), STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRST( DEVID( DISPID ), STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )

*+
*  Name:
*     IIDSDP
*
*  Purpose:
*     Select Display Path
*
*  Invocation:
*     CALL IIDSDP( DISPID, MEMID, NMEM, LUTLIS, ITTLIS, STATUS )
*
*  Description:
*     Select the display path of the visible memory through the LUTs and
*     ITTs. Each video output path has three sections numbered 1 ( R ),
*     2 ( G ) and 4 ( B ). The values in list of LUT flags specify the
*     path to which each memory is to be bound. For pseudo-colour
*     displays each of the image memories is bound to all three DACs, so
*     a flag value of 7 ( R + G + B ) is used for all memories. For true
*     colour displays each of three memories is seperately bound to a
*     DAC, thus the flags will be 1, 2 and 4 for each of the memories in
*     a set which are to contain, respectively, the R, G and B
*     components of an image. A negative value in the list of LUT flags
*     retains the current LUT setting for that memory.
*
*     ITT flag specifies whether the ITT selected for each memory should
*     be bypassed ( as if it were loaded with a linear table ) or not.
*     The flag can take the following values:
*     >0 - Use ITT
*     =0 - Bypass ITT
*     <0 - Retain current ITT setting.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     LUTLIS( * ) = INTEGER (Given)
*        List of VLUT flags
*     ITTLIS( * ) = INTEGER (Given)
*        List of ITT flags
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER LUTLIS( * )
      INTEGER ITTLIS( * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSDP( DEVID( DISPID ), MEMID, NMEM, LUTLIS, ITTLIS,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSDP( DEVID( DISPID ), MEMID, NMEM, LUTLIS, ITTLIS,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDSEL( DISPID, NCONF, STATUS )

*+
*  Name:
*     IIDSEL
*
*  Purpose:
*     Select Configuration
*
*  Invocation:
*     CALL IIDSEL( DISPID, NCONF, STATUS )
*
*  Description:
*     The predefined configuration identified by configuration number
*     is selected. This routine also sets up a default transfer window
*     equal to the smaller of the maximum memory size or the maximum
*     transfer window size.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Given)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     March 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSEL( DEVID( DISPID ), NCONF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSEL( DEVID( DISPID ), NCONF, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, IMAGE, STATUS )

*+
*  Name:
*     IIDSNP
*
*  Purpose:
*     Create Snapshot
*
*  Invocation:
*     CALL IIDSNP( DISPID, CMODE, NPIX, XSTART, YSTART, DEPTH,
*    :             PACK, IMAGE, STATUS )
*
*  Description:
*     Obtain a snapshot of the picture that appears on the screen, not a
*     copy of all the data that has been loaded into the display. This
*     returns a pixel representation of either a monochrome image or
*     each of the R, G and B components for composition into a form
*     suitable for use with a hardcopy device. Data is read out starting
*     at ( x, y ) in row order, packed as indicated by data depth and
*     packing factor, and utilizing the currently established transfer
*     window. When data position in x or data position in y is non-zero
*     care must be taken to be sure that the transfer window has been
*     set properly. If the image is RGB, then each image is returned
*     seperately. The values for colour mode are:
*     0 - monochrome / pseudocolour
*     1 - red
*     2 - blue
*     3 - green
*     The remaining arguments are identical to those of Read Memory.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     CMODE = INTEGER (Given)
*        Colour mode
*     NPIX = INTEGER (Given)
*        Number of pixels
*     XSTART = INTEGER (Given)
*        Data position in X
*     YSTART = INTEGER (Given)
*        Data position in Y
*     DEPTH = INTEGER (Given)
*        Data depth ( bits / pixel )
*     PACK = INTEGER (Given)
*        Packing factor
*     IMAGE( * ) = INTEGER (Returned)
*        Image data
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER CMODE
      INTEGER NPIX
      INTEGER XSTART
      INTEGER YSTART
      INTEGER DEPTH
      INTEGER PACK

*  Arguments Returned:
      INTEGER IMAGE( * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSNP( DEVID( DISPID ), CMODE, NPIX, XSTART, YSTART,
     :                DEPTH, PACK, IMAGE, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSNP( DEVID( DISPID ), CMODE, NPIX, XSTART, YSTART,
     :                DEPTH, PACK, IMAGE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                   YSPLIT, STATUS )

*+
*  Name:
*     IIDSSS
*
*  Purpose:
*     Set Split Screen
*
*  Invocation:
*     CALL IIDSSS( DISPID, MEMID, XOFF, YOFF, SPLIT, XSPLIT,
*    :             YSPLIT, STATUS )
*
*  Description:
*     Split the screen into up to four segments ( numbered 1 - 4 ). The
*     form of the split is controlled by split flag as follows:
*     0 - four segments
*     1 - two segments side by side
*     2 - two segments one above the other
*     The index of the memory identifier array determines the screen
*     segment bound to each memory ID; e.g. the second memory ID is
*     bound to screen segment 2, the location of which is determined by
*     split flag.
*
*     Split x location and split y location specify the ( x, y )
*     position of the intersection of the split screen segments. Split y
*     location is irrelevant for split flag = 1, and split x location is
*     irrelevant jfor split flag = 2.
*
*     The x and y offsets determinr the positioning of the image memory
*     with respect to the lower left corner of the corresponding split
*     screen segment. A split flag of -1 is used to reset the display
*     with no split and with the first memory visible.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        Memory identifiers
*     XOFF( * ) = INTEGER (Given)
*        X offsets
*     YOFF( * ) = INTEGER (Given)
*        Y offsets
*     SPLIT = INTEGER (Given)
*        Split flag
*     XSPLIT = INTEGER (Given)
*        Split X location
*     YSPLIT = INTEGER (Given)
*        Split Y location
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER XOFF( * )
      INTEGER YOFF( * )
      INTEGER SPLIT
      INTEGER XSPLIT
      INTEGER YSPLIT

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSSS( DEVID( DISPID ), MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                YSPLIT, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSSS( DEVID( DISPID ), MEMID, XOFF, YOFF, SPLIT, XSPLIT,
     :                YSPLIT, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDSTC( DISPID, NCONF, STATUS )

*+
*  Name:
*     IIDSTC
*
*  Purpose:
*     Stop Configuration
*
*  Invocation:
*     CALL IIDSTC( DISPID, NCONF, STATUS )
*
*  Description:
*     When this is called the display driver software will allocate
*     physical memory to the memories created since the last call to
*     Enable Configuration. By postponing the allocation of physical
*     memory until all the memories required by the application have
*     been defined, the driver software can allocate the available
*     memory in an optimum way. The configuration identifier returned is
*     then used in a call to Select Configuration in order to enable the
*     use of the newly defined memories.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NCONF = INTEGER (Returned)
*        Configuration number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER NCONF

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSTC( DEVID( DISPID ), NCONF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSTC( DEVID( DISPID ), NCONF, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIDUPD( DISPID, STATUS )

*+
*  Name:
*     IIDUPD
*
*  Purpose:
*     Update Display
*
*  Invocation:
*     CALL IIDUPD( DISPID, STATUS )
*
*  Description:
*     Any buffered output is sent to the display. This function is
*     called implicitly by any routine that reads data from the display
*     and by Enable Interaction.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        {argument_description}
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     November 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   Update the display
*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNUPD( DEVID( DISPID ), STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWUPD( DEVID( DISPID ), STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIEGEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IIEGEP
*
*  Purpose:
*     Get Escape Parameter
*
*  Invocation:
*     CALL IIEGEP( PARAM, SLEN, STRING, STATUS )
*
*  Description:
*     Query an implementation specific parameter.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name
*     SLEN = INTEGER (Given)
*        Length of output buffer
*     STRING = CHARACTER * ( * ) (Returned)
*        Buffer for string parameter
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Call the device specific routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER SLEN

*  Arguments Returned:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   See if the Ikon driver knows about the parameter
      CALL IKNGEP( PARAM, SLEN, STRING, STATUS )

*   See if the X-window driver knows about the parameter
      IF ( STATUS .NE. IDI__OK ) THEN
         STATUS = IDI__OK
         CALL IXWGEP( PARAM, SLEN, STRING, STATUS )
      ENDIF

      END

************************************************************************

      SUBROUTINE IIEPEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IIEPEP
*
*  Purpose:
*     Put Escape Parameter
*
*  Invocation:
*     CALL IIEPEP( PARAM, SLEN, STRING, STATUS )
*
*  Description:
*     Set an implementation specific parameter.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name
*     SLEN = INTEGER (Given)
*        Length of input buffer
*     STRING = CHARACTER * ( * ) (Given)
*        Buffer for string parameter
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Call the device specific routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER SLEN
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   See if the Ikon driver knows about the parameter
      CALL IKNPEP( PARAM, SLEN, STRING, STATUS )

*   See if the X-window driver knows about the parameter
      CALL IXWPEP( PARAM, SLEN, STRING, STATUS )

      END

************************************************************************

      SUBROUTINE IIGPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                   STATUS )

*+
*  Name:
*     IIGPLY
*
*  Purpose:
*     Polyline
*
*  Invocation:
*     CALL IIGPLY( DISPID, MEMID, X, Y, NXY, COLOR, LSTYLE, STATUS )
*
*  Description:
*     Connect the specified points with straight lines. The value of
*     colour determines the colour of the line ( in conjunction with the
*     associated ITT and LUT ) in a way that depends on the type of
*     memory being written to. For image memories colour is the pixel
*     value written into the memory to draw the lines. For graphics
*     memories the value of colour selects the colour according to the
*     following table:
*     0 - background
*     1 - black
*     2 - white
*     3 - red
*     4 - green
*     5 - blue
*     6 - yellow
*     7 - magenta
*     8 - cyan
*     These colours will be displayed unless the LUT associated with the
*     memory has been altered by the applications program. The effect of
*     setting the value of colour >8 is implementation dependent. If the
*     specified colour is not available white is used.
*
*     Line style is an integer in the range 1 to 4. The linestyles are
*     the same as those defined by GKS:
*     1 - solid line
*     2 - dashed line
*     3 - dotted line
*     4 - dash-dotted line
*     If the specified line style is not available a solid line is used.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     X( * ) = INTEGER (Given)
*        List of X positions
*     Y( * ) = INTEGER (Given)
*        List of Y positions
*     NXY = INTEGER (Given)
*        Number of ( x,y ) positions
*     COLOR = INTEGER (Given)
*        Colour
*     LSTYLE = INTEGER (Given)
*        Line style
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     April 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER X( * )
      INTEGER Y( * )
      INTEGER NXY
      INTEGER COLOR
      INTEGER LSTYLE

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNPLY( DEVID( DISPID ), MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWPLY( DEVID( DISPID ), MEMID, X, Y, NXY, COLOR, LSTYLE,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIGTXT( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH,
     :                   TANGLE, COLOR, TSIZE, STATUS )

*+
*  Name:
*     IIGTXT
*
*  Purpose:
*     Plot text
*
*  Invocation:
*     CALL IIGTXT( DISPID, MEMID, TEXT, XPOS, YPOS, TPATH,
*    :             TANGLE, COLOR, TSIZE, STATUS )
*
*  Description:
*     This is intended for plotting hardware text but devices without
*     hardware text provide a software font ( probably a raster font ).
*     The text is displayed with the nearest height available.
*
*     The x and y positions refer to the bottom left corner of the first
*     character. Allowed values of text path are:
*     0 - text runs from left to right
*     1 - text runs from bottom to top
*     2 - text runs from right to left
*     3 - text runs from top to bottom
*     The support of text paths other than from left to right is
*     optional. Text orientation is specified in degrees of rotation in
*     a clockwise direction; support of orientations other than 0 is
*     optional.
*
*     The value of colour is interpreted as the text mode if the memory
*     identifier refers to a text memory; otherwise it is the same as
*     defined for Polyline. When interpreted as a text mode, the
*     following values are defined but the displayed values may be
*     implementation dependent:
*     0 - normal text
*     1 - inverse video
*     2 - blinking text
*     3 - solid background
*     4 - solid background, inverse video
*     5 - solid background, inverse video, blinking.
*
*     Text size can assume the following values:
*     0 - normal text size
*     1 - large text size
*     2 - very large text size
*     3 - small text size
*     The support of text sizes other than the defualt is optional, and
*     if other sizes are available they will be either those supported
*     in the hardware or text sizes that are chosen by the implementor
*     to give a pleasing balance to text output on the screen.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     TEXT = CHARACTER * ( * ) (Given)
*        Text
*     XPOS = INTEGER (Given)
*        X postion
*     YPOS = INTEGER (Given)
*        Y position
*     TPATH = INTEGER (Given)
*        Text path
*     TANGLE = INTEGER (Given)
*        Text orientation
*     COLOR = INTEGER (Given)
*        Pixel colour value
*     TSIZE = INTEGER (Given)
*        Text size
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     April 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      CHARACTER * ( * ) TEXT
      INTEGER XPOS
      INTEGER YPOS
      INTEGER TPATH
      INTEGER TANGLE
      INTEGER COLOR
      INTEGER TSIZE

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER LENTXT
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNTXT( DEVID( DISPID ), MEMID, TEXT, XPOS, YPOS, TPATH,
     :                TANGLE, COLOR, TSIZE, STATUS )

*   If the device is an X-window call the appropriate routine
*   Pass the length of the text string as an argument
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         LENTXT = LEN( TEXT )
         CALL IXWTXT( DEVID( DISPID ), MEMID, TEXT, LENTXT, XPOS, YPOS,
     :                TPATH, TANGLE, COLOR, TSIZE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIEIW( DISPID, TRIGS, STATUS )

*+
*  Name:
*     IIIEIW
*
*  Purpose:
*     Execute interaction and wait
*
*  Invocation:
*     CALL IIIEIW( DISPID, TRIGS, STATUS )
*
*  Description:
*     This routine returns only when an enabled trigger has been fired,
*     or if an interactive operation of type 0 ( application specific
*     interaction ) has been requested. In this latter case the
*     application program is allowed to execute some application
*     specific code, to re-enter the execute interaction routine, and
*     loop until the exit trigger is fired. The actual value of some
*     event caused by a user interaction, e.g. moving a joystick or
*     entering an input string, must be read by calling the appropriate
*     input function.
*
*     The trigger status array is an array of logicals indicating which
*     of the triggers on the device have been fired ( value set to
*     'true', or non-zero ). The length of this array is known from the
*     device capabilities table, which is accessed via the routine Query
*     Capabilities Integer. The trigger status array is cleared ( all
*     values set to 'false', or zero ) on entry to the routine.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     TRIGS( * ) = LOGICAL (Returned)
*        Trigger status array
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      LOGICAL TRIGS( * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNEIW( DEVID( DISPID ), TRIGS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWEIW( DEVID( DISPID ), TRIGS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
     :                   EXTRN, STATUS )

*+
*  Name:
*     IIIENI
*
*  Purpose:
*     Enable interaction
*
*  Invocation:
*     CALL IIIENI( DISPID, INTTY, INTID, OBJTY, OBJID, INTOP,
*    :             EXTRN, STATUS )
*
*  Description:
*     The interactor type selects the different operator interactive
*     devices that are to be used by the user to interact with the
*     object. The following interactor types are defined:
*     0 - locator
*     1 - real evaluator
*     2 - integer evaluator
*     3 - logical evaluator ( switch )
*     4 - character evaluator
*     5 - trigger
*     Implementation and device specific types may also be provided in
*     addition to the basic set. Interactor number selects which actual
*     unit is to be used, since there may be more than one unit of the
*     same type available on a device.
*
*     Object type defines the functional unit of the image display
*     system which is to be modified by the interaction. The following
*     types are defined:
*     0 - no visible effect
*     1 - cursor
*     2 - ITT ( intensity transformation table )
*     3 - LUT ( look-up table )
*     4 - ROI ( region of interest )
*     5 - memory
*     6 - entire display
*     The object identifier specifies the number of the selected object.
*     The exact meaning depends on the object type; for exmaple, the
*     object identifier is the cursor number if object type = 1.
*
*     The interacive operation specifies the action the selected object
*     is to be performed on. The following operation types are defined:
*     0 - execute application-specific code
*     1 - move object, e.g. memory or cursor
*     2 - rotate objecy, e.g. colour table
*     3 - increase object zoom
*     4 - reduce object zoom
*     5 - set object zoom to normal
*     6 - blink object, e.g. memory sets
*     7 - modify object, e.g. ROI
*
*     Exit trigger is the number of the trigger used to signal the end
*     of the interactive operation.
*
*     The behaviour of this routine is determined by the capabilities of
*     the device: on devices capable of handling the interaction without
*     the involvement of the host computer, it starts the interaction on
*     the device and returns. On less sophisticated devices it simply
*     stores the relevant data and exits. In the latter case the
*     interaction only begins when Execute Interaction and Wait is
*     called.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INTTY = INTEGER (Given)
*        Interactor type
*     INTID = INTEGER (Given)
*        Interactor identifier
*     OBJTY = INTEGER (Given)
*        Object type
*     OBJID = INTEGER (Given)
*        Object identifier
*     INTOP = INTEGER (Given)
*        Interactive operation
*     EXTRN = INTEGER (Given)
*        Exit trigger number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INTTY
      INTEGER INTID
      INTEGER OBJTY
      INTEGER OBJID
      INTEGER INTOP
      INTEGER EXTRN

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNENI( DEVID( DISPID ), INTTY, INTID, OBJTY, OBJID,
     :                INTOP, EXTRN, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWENI( DEVID( DISPID ), INTTY, INTID, OBJTY, OBJID,
     :                INTOP, EXTRN, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIGIE( DISPID, NEVAL, IVALUE, STATUS )

*+
*  Name:
*     IIIGIE
*
*  Purpose:
*     Get Integer Evaluator
*
*  Invocation:
*     CALL IIIGIE( DISPID, NEVAL, IVALUE, STATUS )
*
*  Description:
*     Return the input integer value that was entered on the input
*     device specified by evaluator number.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     IVALUE = INTEGER (Returned)
*        Input integer value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      INTEGER IVALUE

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNGIE( DEVID( DISPID ), NEVAL, IVALUE, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWGIE( DEVID( DISPID ), NEVAL, IVALUE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIGLD( DISPID, LOCNUM, DX, DY, STATUS )

*+
*  Name:
*     IIIGLD
*
*  Purpose:
*     Get Locator Displacement
*
*  Invocation:
*     CALL IIIGLD( DISPID, LOCNUM, DX, DY, STATUS )
*
*  Description:
*     Return the displacement fo the locator indicated by locator
*     number. Locators such as joysticks or track balls, return the
*     ( x, y ) displacement from their previous position, rather than an
*     offset from some origin.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LOCNUM = INTEGER (Given)
*        Locator number
*     DX = INTEGER (Returned)
*        X displacement
*     DY = INTEGER (Returned)
*        Y displacement
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LOCNUM

*  Arguments Returned:
      INTEGER DX
      INTEGER DY

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNGLD( DEVID( DISPID ), LOCNUM, DX, DY, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWGLD( DEVID( DISPID ), LOCNUM, DX, DY, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIGLE( DISPID, NEVAL, LVALUE, STATUS )

*+
*  Name:
*     IIIGLE
*
*  Purpose:
*     Get Logical Evaluator
*
*  Invocation:
*     CALL IIIGLE( DISPID, NEVAL, LVALUE, STATUS )
*
*  Description:
*     Return the input logical value that was entered on the input
*     device specified by evaluator number.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     LVALUE = LOGICAL (Returned)
*        Input logical value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      LOGICAL LVALUE

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNGLE( DEVID( DISPID ), NEVAL, LVALUE, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWGLE( DEVID( DISPID ), NEVAL, LVALUE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIGRE( DISPID, NEVAL, RVALUE, STATUS )

*+
*  Name:
*     IIIGRE
*
*  Purpose:
*     Get Real Evaluator
*
*  Invocation:
*     CALL IIIGRE( DISPID, NEVAL, RVALUE, STATUS )
*
*  Description:
*     Return the input real value that was entered on the input device
*     specified by evaluator number.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     RVALUE = REAL (Returned)
*        Input real value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      REAL RVALUE

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNGRE( DEVID( DISPID ), NEVAL, RVALUE, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWGRE( DEVID( DISPID ), NEVAL, RVALUE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIGSE( DISPID, NEVAL, STRING, SLEN, STATUS )

*+
*  Name:
*     IIIGSE
*
*  Purpose:
*     Get String Evaluator
*
*  Invocation:
*     CALL IIIGSE( DISPID, NEVAL, STRING, SLEN, STATUS )
*
*  Description:
*     Return the input string that was entered by the user on the input
*     device specified by evaluator number ( different evaluator numbers
*     might correspond, for example, to a device with multiple
*     keyboards ). The maximum allowed length of the input string is 80
*     characters. The output argument length of returned string contains
*     the actual length of the string.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     NEVAL = INTEGER (Given)
*        Evaluator number
*     STRING = CHARACTER * ( * ) (Returned)
*        Input string
*     SLEN = INTEGER (Returned)
*        Length of returned string
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER NEVAL

*  Arguments Returned:
      CHARACTER * ( * ) STRING
      INTEGER SLEN

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNGSE( DEVID( DISPID ), NEVAL, STRING, SLEN, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWGSE( DEVID( DISPID ), NEVAL, STRING, SLEN, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIIQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )

*+
*  Name:
*     IIIQID
*
*  Purpose:
*     Query Interactor Description
*
*  Invocation:
*     CALL IIIQID( DISPID, INTTY, INTID, MESSAG, MESLEN, STATUS )
*
*  Description:
*     This returns a string of text that contains a description of an
*     interactive device. It allows suitable help messages to be
*     displayed on a device independent way. For example, strings such
*     as "move track ball", "position mouse", "push centre button", and
*     so on, could be used to describe the nature of the interactive
*     device so that such prompt messages would not need to be hard
*     coded in the application.
*
*     The maximum allowed length of the interactive device description
*     string is 80 characters. The output argument length of description
*     string contains the significant length of the string, i.e. with
*     all trailing blanks removed. The allowed values for interactor
*     type are the same as for Enable Interaction.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INTTY = INTEGER (Given)
*        Interactor type
*     INTID = INTEGER (Given)
*        Interactor number
*     MESSAG = CHARACTER * ( * ) (Returned)
*        Interactive device description
*     MESLEN = INTEGER (Returned)
*        Length of description string
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     January 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INTTY
      INTEGER INTID

*  Arguments Returned:
      CHARACTER * ( * ) MESSAG
      INTEGER MESLEN

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNQID ( DEVID( DISPID ), INTTY, INTID, MESSAG, MESLEN,
     :                 STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWQID ( DEVID( DISPID ), INTTY, INTID, MESSAG, MESLEN,
     :                 STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIISTI( DISPID, STATUS )

*+
*  Name:
*     IIISTI
*
*  Purpose:
*     Stop Interactive Input
*
*  Invocation:
*     CALL IIISTI( DISPID, STATUS )
*
*  Description:
*     Cancel all object/interactor bindings and terminate any
*     interactions that are in progress.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSTI( DEVID( DISPID ), STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSTI( DEVID( DISPID ), STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IILRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IILRIT
*
*  Purpose:
*     Read Intensity Transformation Table
*
*  Invocation:
*     CALL IILRIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
*
*  Description:
*     Parameter definitions are identical to those for Write Intensite
*     Transformation Table.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     ITT( 3, NENT ) = REAL (Returned)
*        Intensity transformation table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ITTNUM
      INTEGER START
      INTEGER NENT

*  Arguments Returned:
      REAL ITT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRIT( DEVID( DISPID ), MEMID, ITTNUM, START, NENT, ITT,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRIT( DEVID( DISPID ), MEMID, ITTNUM, START, NENT, ITT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IILRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IILRLT
*
*  Purpose:
*     Read Video Look Up Table
*
*  Invocation:
*     CALL IILRLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
*
*  Description:
*     Parameters are identical to those for Write Video Look-up Table.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LUTNUM = INTEGER (Given)
*        VLUT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     VLUT( 3, NENT ) = REAL (Returned)
*        Video look-up table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     March 1990 (NE):
*        Use assumed size array for VLUT
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LUTNUM
      INTEGER START
      INTEGER NENT

*  Arguments Returned:
      REAL VLUT( 3, * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRLT( DEVID( DISPID ), LUTNUM, START, NENT, VLUT,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRLT( DEVID( DISPID ), LUTNUM, START, NENT, VLUT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IILSBV( DISPID, MEMID, LVIS, STATUS )

*+
*  Name:
*     IILSBV
*
*  Purpose:
*     Set Intensity Bar Visibility
*
*  Invocation:
*     CALL IILSBV( DISPID, MEMID, LVIS, STATUS )
*
*  Description:
*     Setting the intensity bar visibility to 'true' causes an intensity
*     bar to be displayed corresponding to the ITT and LUT selected for
*     the given memory identifier. Making the intensity bar visible may
*     have the effect of making other data invisible depending on the
*     capabilities of the device.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSBV( DEVID( DISPID ), MEMID, LVIS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSBV( DEVID( DISPID ), MEMID, LVIS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IILWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT,
     :                   STATUS )

*+
*  Name:
*     IILWIT
*
*  Purpose:
*     Write Intensity Transformation Table
*
*  Invocation:
*     CALL IILWIT( DISPID, MEMID, ITTNUM, START, NENT, ITT, STATUS )
*
*  Description:
*     The intensity transformation table is a list of floating point
*     numbers in the range 0.0 to 1.0. ITT number is an integer in the
*     range 0 to n-1, where n is fixed for any particular display type.
*     A value of -1 specifies the ITT currently bound to the selected
*     memory ( either selected by default or bound to the memory
*     identifier by a call to Select Memory Look-up Table ).
*
*     Table entries are indexed starting with 0, and start position
*     indicates the first element of a block of entries that are to be
*     written. The value of start position must therefore be in the
*     range 0 to m-1, where m is the number of entries in the table. The
*     number of entries must be in the range 1 to m - start position.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     ITT( 3, NENT ) = REAL (Given)
*        Intensity transformation table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ITTNUM
      INTEGER START
      INTEGER NENT
      REAL ITT( 3, NENT )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWIT( DEVID( DISPID ), MEMID, ITTNUM, START, NENT, ITT,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWIT( DEVID( DISPID ), MEMID, ITTNUM, START, NENT, ITT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IILWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )

*+
*  Name:
*     IILWLT
*
*  Purpose:
*     Write Video Look Up Table
*
*  Invocation:
*     CALL IILWLT( DISPID, LUTNUM, START, NENT, VLUT, STATUS )
*
*  Description:
*     LUT number is an integer in the range 0 to n-1, where n is fixed
*     for any particular display type. A value of -1 specifies the LUT
*     currently bound to the selected memory. The look-up table is a
*     list of R, G and B intensities in the range 0.0 to 1.0. The table
*     data is stored in a 3 x m floating point array.
*
*     Table entries are indexed starting with 0, and start position
*     indicates the first element of a block of entries that are to be
*     written. The value of start position must therefore be in the
*     range 0 to m-1, where m is the number of entries in the table.
*     The number of entries must be in the range 1 to m - start
*     position.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     LUTNUM = INTEGER (Given)
*        VLUT number
*     START = INTEGER (Given)
*        Start position
*     NENT = INTEGER (Given)
*        Number of entries
*     VLUT( 3, NENT ) = REAL (Given)
*        Video look-up table
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     March 1990 (NE):
*        Use assumed size array for VLUT
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER LUTNUM
      INTEGER START
      INTEGER NENT
      REAL VLUT( 3, * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routines
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWLT( DEVID( DISPID ), LUTNUM, START, NENT, VLUT,
     :                STATUS )

*   If the device is an X-window call the appropriate routines
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWLT( DEVID( DISPID ), LUTNUM, START, NENT, VLUT,
     :                STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )

*+
*  Name:
*     IIMBLM
*
*  Purpose:
*     Blink Memories
*
*  Invocation:
*     CALL IIMBLM( DISPID, MEMID, NMEM, BLINKS, STATUS )
*
*  Description:
*     Set up the blink period for the identified memories.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     BLINKS( * ) = REAL (Given)
*        Blink periods ( in seconds )
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      REAL BLINKS( * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNBLM( DEVID( DISPID ), MEMID, NMEM, BLINKS, STATUS )

*   If the device is an Ikon call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWBLM( DEVID( DISPID ), MEMID, NMEM, BLINKS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMCMY( DISPID, MEMID, NMEM, BACK, STATUS )

*+
*  Name:
*     IIMCMY
*
*  Purpose:
*     Clear Memory
*
*  Invocation:
*     CALL IIMCMY( DISPID, MEMID, NMEM, BACK, STATUS )
*
*  Description:
*     Erase the entire contents of a memory. If supported by the
*     hardware the memory may be filled with a value other than zero
*     specified by background value. If the hardware does not support
*     this function the background value may be ignored.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     BACK = INTEGER (Given)
*        Background value
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER BACK

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNCMY( DEVID( DISPID ), MEMID, NMEM, BACK, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWCMY( DEVID( DISPID ), MEMID, NMEM, BACK, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )

*+
*  Name:
*     IIMEBM
*
*  Purpose:
*     Define External Bitmap
*
*  Invocation:
*     CALL IIMEBM( DISPID, BMDSCR, BMTYPE, XSIZE, YSIZE, STATUS )
*
*  Description:
*     This routine does not appear in the IDI specification.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*     It appears in the X-windows driver supplied by Trieste.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     BMDSCR = CHARACTER * ( * ) (Given)
*        Bitmap descriptor
*     BMTYPE = CHARACTER (Given)
*        Bitmap type
*     XSIZE = INTEGER (Given and Returned)
*        Bitmap X size
*     YSIZE = INTEGER (Given and Returned)
*        Bitmap Y size
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1990 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      CHARACTER * ( * ) BMDSCR
      CHARACTER BMTYPE

*  Arguments Given and Returned:
      INTEGER XSIZE
      INTEGER YSIZE

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER LDSCR
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNEBM( DEVID( DISPID ), BMDSCR, BMTYPE, XSIZE, YSIZE,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
*   Pass the length of the string as an argument
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         LDSCR = LEN( BMDSCR )
         CALL IXWEBM( DEVID( DISPID ), BMDSCR, LDSCR, BMTYPE, XSIZE,
     :                YSIZE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
     :                   PACK, ITTON, IMAGE, STATUS )

*+
*  Name:
*     IIMRMY
*
*  Purpose:
*     Read Memory
*
*  Invocation:
*     CALL IIMRMY( DISPID, MEMID, NPIX, XSTART, YSTART, DEPTH,
*    :             PACK, ITTON, IMAGE, STATUS )
*
*  Description:
*     Read image data from a memory. All the arguments have the same
*     meaning as those in Write Memory except for ITT flag. When ITT
*     flag is set to 'true' the values read are transformed by the
*     intensity transformation selected for the memory. When ITT flag is
*     'false' the values are not transformed. Note that this function
*     like Write Memory, can be used to read either image, graphics or
*     text memories where the hardware allows it.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     NPIX = INTEGER (Given)
*        Number of pixels
*     XSTART = INTEGER (Given)
*        Start position in X
*     YSTART = INTEGER (Given)
*        Start position in Y
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     PACK = INTEGER (Given)
*        Packing factor. Number of pixels per longword
*     ITTON = LOGICAL (Given)
*        ITT flag
*     IMAGE( * ) = INTEGER (Returned)
*        Image data
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER NPIX
      INTEGER XSTART
      INTEGER YSTART
      INTEGER DEPTH
      INTEGER PACK
      INTEGER ITTON

*  Arguments Returned:
      INTEGER IMAGE( * )

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRMY( DEVID( DISPID ), MEMID, NPIX, XSTART, YSTART,
     :                DEPTH, PACK, ITTON, IMAGE, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRMY( DEVID( DISPID ), MEMID, NPIX, XSTART, YSTART,
     :                DEPTH, PACK, ITTON, IMAGE, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )

*+
*  Name:
*     IIMSLT
*
*  Purpose:
*     Select Memory Look up Tables
*
*  Invocation:
*     CALL IIMSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )
*
*  Description:
*     Selects the intensity transformation table and look-up table to be
*     used when displaying the contents of the memory. If LUT identifier
*     or ITT number is set to -1 the current LUT/ITT selection is
*     retained.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     LUTNUM = INTEGER (Given)
*        VLUT identifier
*     ITTNUM = INTEGER (Given)
*        ITT number
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     June 1989 (NE):
*        Original version.
*     December 1990 *NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER LUTNUM
      INTEGER ITTNUM

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSLT( DEVID( DISPID ), MEMID, LUTNUM, ITTNUM, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSLT( DEVID( DISPID ), MEMID, LUTNUM, ITTNUM, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMSMV( DISPID, MEMID, NMEM, LVIS, STATUS )

*+
*  Name:
*     IIMSMV
*
*  Purpose:
*     Set Memory Visibility
*
*  Invocation:
*     CALL IIMSMV( DISPID, MEMID, NMEM, LVIS, STATUS )
*
*  Description:
*     Setting a memory visibility to 'true' causes it to be displayed
*     using the currently selected ITT and LUT. Making a memory visible
*     may have the effect of making other memories invisible depending
*     on the capabilities of the device. It is an error to make a memory
*     visible before it has been associated with a ITT and a LUT. If the
*     specified visibilities cannot be realized, an error is returned;
*     the visibility of memories not in the list is implementation
*     dependent.
*
*     See the IDI specification for a description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      LOGICAL LVIS

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSMV( DEVID( DISPID ), MEMID, NMEM, LVIS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSMV( DEVID( DISPID ), MEMID, NMEM, LVIS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
     :                   XOFF, YOFF, STATUS )

*+
*  Name:
*     IIMSTW
*
*  Purpose:
*     Set Transfer Window
*
*  Invocation:
*     CALL IIMSTW( DISPID, MEMID, DIRECN, XSIZE, YSIZE, DEPTH,
*    :             XOFF, YOFF, STATUS )
*
*  Description:
*     The default transfer window is established in the Open Display
*     call and is reset automatically by a call to Select Configuration.
*     The transfer window need only be redefined if the I/O window
*     within an image plane is to be changed. Note that a given device
*     may permit I/O transfers to a small section of the memory at a
*     time. Set Transfer Window must be used to move the I/O window
*     around in the writable image memory.
*
*     The load direction argument specifies whether images should be
*     loaded bottom up or top down. The allowed values of load direction
*     are:
*     0 - load from bottom of screen to top
*     1 - load from top of screen to bottom
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     DIRECN = INTEGER (Given)
*        Direction of load. 0 = up. 1 = down
*     XSIZE = INTEGER (Given)
*        Size of window in X
*     YSIZE = INTEGER (Given)
*        Size of window in Y
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     XOFF = INTEGER (Given)
*        Start position of window in X
*     YOFF = INTEGER (Given)
*        Start position of window in Y
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     June 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER DIRECN
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER DEPTH
      INTEGER XOFF
      INTEGER YOFF

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSTW( DEVID( DISPID ), MEMID, DIRECN, XSIZE, YSIZE,
     :                DEPTH, XOFF, YOFF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSTW( DEVID( DISPID ), MEMID, DIRECN, XSIZE, YSIZE,
     :                DEPTH, XOFF, YOFF, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIMWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                   XSTART, YSTART, STATUS )

*+
*  Name:
*     IIMWMY
*
*  Purpose:
*     Write Memory
*
*  Invocation:
*     CALL IIMWMY( DISPID, MEMID, IMAGE, NPIX, DEPTH, PACK,
*    :                   XSTART, YSTART, STATUS )
*
*  Description:
*     Load image data into a memory. Data transfers may be of any size;
*     a pixel at a time, a line at a time, or a full image in one call.
*     Data is assumed to be stored in memory as a single contiguous
*     array and is loaded into memory in the order received. This
*     function can be used to write any type of memory provided that the
*     hardware supports the writing of image data into the specified
*     memory.
*
*     The window boundaries that control how the data wraps from one
*     pixel row to the next are controlled by Set Transfer Window;
*     default settings are automatically defined with calls to Open
*     Display or Select Configuration. If seperate writes are used to
*     load parts of an image the x and y data positions must be updated
*     to indicate where each write is to start.
*
*     Packing factor describes the number of data items packed into a
*     single integer word. For example for a system with 32 bit integers
*     there could be one 32 bit integer, two 16 bit integers or four 8
*     bit integers packed into each word. Data depth refers to the
*     number of bits of data per pixel. If data depth is not equal to
*     the depth of the memory being written to each pixel value will
*     either be truncated at the most significant end or extended with
*     zeroes to fit the memory depth. For example if 8 bit pixels are
*     stored one per integer word, packing factor would be 1 and data
*     depth would be 8. If two 16 bit pixels are packed into a 32 bit
*     integer word, packing factor would be 2 and data depth would be
*     16.
*
*     The use of data depth which does not match the memory depth may
*     result in a considerable overhead for some models of display. In
*     general only a limited number of data depths will be supported.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     IMAGE( * ) = INTEGER (Given)
*        Image data
*     NPIX = INTEGER (Given)
*        Number of pixels
*     DEPTH = INTEGER (Given)
*        Data depth. Bits per pixel
*     PACK = INTEGER (Given)
*        Packing factor. Number of pixels per longword
*     XSTART = INTEGER (Given)
*        Start position in X
*     YSTART = INTEGER (Given)
*        Start position in Y
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER IMAGE( * )
      INTEGER NPIX
      INTEGER DEPTH
      INTEGER PACK
      INTEGER XSTART
      INTEGER YSTART

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWMY( DEVID( DISPID ), MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                XSTART, YSTART, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWMY( DEVID( DISPID ), MEMID, IMAGE, NPIX, DEPTH, PACK,
     :                XSTART, YSTART, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIRINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
     :                   ROIID, STATUS )

*+
*  Name:
*     IIRINR
*
*  Purpose:
*     Initialize Rectangular Region of Interest
*
*  Invocation:
*     CALL IIRINR( DISPID, MEMID, ROICOL, XMIN, YMIN, XMAX, YMAX,
*    :             ROIID, STATUS )
*
*  Description:
*     The ROI positions and colour are as specified in Initialize
*     Cursor. Note that the interface assigns an ROI identifier, which
*     is used by the other ROI routines to reference the ROI.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ROICOL = INTEGER (Given)
*        ROI marker colour
*     XMIN = INTEGER (Given)
*        Minimum X position
*     YMIN = INTEGER (Given)
*        Minimum Y position
*     XMAX = INTEGER (Given)
*        Maximum X position
*     YMAX = INTEGER (Given)
*        Maximum Y position
*     ROIID = INTEGER (Returned)
*        ROI identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     March 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ROICOL
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX

*  Arguments Returned:
      INTEGER ROIID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNINR( DEVID( DISPID ), MEMID, ROICOL, XMIN, YMIN,
     :                XMAX, YMAX, ROIID, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWINR( DEVID( DISPID ), MEMID, ROICOL, XMIN, YMIN,
     :                XMAX, YMAX, ROIID, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIRRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                   OUTMID, STATUS )

*+
*  Name:
*     IIRRRI
*
*  Purpose:
*     Read Rectangular Region of Interest
*
*  Invocation:
*     CALL IIRRRI( DISPID, INMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :             OUTMID, STATUS )
*
*  Description:
*     The input and output memory identifiers are as specified in Read
*     Cursor Position.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     INMID = INTEGER (Given)
*        Input memory identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     XMIN = INTEGER (Returned)
*        Minimum X position
*     YMIN = INTEGER (Returned)
*        Minimum Y position
*     XMAX = INTEGER (Returned)
*        Maximum X position
*     YMAX = INTEGER (Returned)
*        Maximum Y position
*     OUTMID = INTEGER (Returned)
*        Output memory identifier
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     March 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'IDI_ERR'

*  Global variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER INMID
      INTEGER ROIID

*  Arguments Returned:
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX
      INTEGER OUTMID

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRRI( DEVID( DISPID ), INMID, ROIID, XMIN, YMIN,
     :                XMAX, YMAX, OUTMID, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRRI( DEVID( DISPID ), INMID, ROIID, XMIN, YMIN,
     :                XMAX, YMAX, OUTMID, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIRSRV( DISPID, ROIID, LVIS, STATUS )

*+
*  Name:
*     IIRSRV
*
*  Purpose:
*     Set Visibility Rectangular Region of Interest
*
*  Invocation:
*     CALL IIRSRV( DISPID, ROIID, LVIS, STATUS )
*
*  Description:
*     Set the visibility of the ROI markers on the screen. Parameters
*     are identical to those for Set Cursor Visibility.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     LVIS = LOGICAL (Given)
*        Visibility
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     March 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER ROIID
      INTEGER LVIS

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNSRV( DEVID( DISPID ), ROIID, LVIS, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWSRV( DEVID( DISPID ), ROIID, LVIS, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIRWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
     :                   STATUS )

*+
*  Name:
*     IIRWRI
*
*  Purpose:
*     Write Rectangular Region of Interest
*
*  Invocation:
*     CALL IIRWRI( DISPID, MEMID, ROIID, XMIN, YMIN, XMAX, YMAX,
*    :             STATUS )
*
*  Description:
*     Parameters are identical to those for Write Cursor Position.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     ROIID = INTEGER (Given)
*        ROI identifier
*     XMIN = INTEGER (Given)
*        Minimum X position
*     YMIN = INTEGER (Given)
*        Minimum Y position
*     XMAX = INTEGER (Given)
*        Maximum X position
*     YMAX = INTEGER (Given)
*        Maximum Y position
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     March 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID
      INTEGER ROIID
      INTEGER XMIN
      INTEGER YMIN
      INTEGER XMAX
      INTEGER YMAX

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWRI( DEVID( DISPID ), MEMID, ROIID, XMIN, YMIN,
     :                XMAX, YMAX, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWRI( DEVID( DISPID ), MEMID, ROIID, XMIN, YMIN,
     :                XMAX, YMAX, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIZRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IIZRSZ
*
*  Purpose:
*     Read Memory Scroll and Zoom
*
*  Invocation:
*     CALL IIZRSZ( DISPID, MEMID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     Inquire the current position and zoom factor for a memory. The
*     values returned are teh actual values realized by the hardware
*     which may be different from the values selected by the
*     applications program using Write Memory Scroll and Write Memory
*     Zoom.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     XOFF = INTEGER (Returned)
*        X offset
*     YOFF = INTEGER (Returned)
*        Y offset
*     ZOOMF = INTEGER (Returned)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID

*  Arguments Returned:
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER XZOOM
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRSZ( DEVID( DISPID ), MEMID, XOFF, YOFF, ZOOMF,
     :                STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRSZ( DEVID( DISPID ), MEMID, XOFF, YOFF, XZOOM,
     :                STATUS )

*   The X-windows implementation uses ZOOM = 1 to imply no zoom
         ZOOMF = SIGN( ABS( XZOOM ) - 1, XZOOM )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIZRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IIZRZP
*
*  Purpose:
*     Read Display Zoom and Pan
*
*  Invocation:
*     CALL IIZRZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     Return the current zoom and pan settings for the whole display.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XOFF = INTEGER (Returned)
*        X offset
*     YOFF = INTEGER (Returned)
*        Y offset
*     ZOOMF = INTEGER (Returned)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Call device specific routines
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID

*  Arguments Returned:
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER XZOOM
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNRZP( DEVID( DISPID ), XOFF, YOFF, ZOOMF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWRZP( DEVID( DISPID ), XOFF, YOFF, XZOOM, STATUS )

*   The X-windows implementation uses ZOOM = 1 to imply no zoom
         ZOOMf = SIGN( ABS( XZOOM ) - 1, XZOOM )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIZWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )

*+
*  Name:
*     IIZWSC
*
*  Purpose:
*     Write Memory Scroll
*
*  Invocation:
*     CALL IIZWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )
*
*  Description:
*     By default memories are displayed with pixel ( 0, 0 ) at the
*     bottom left corner of the screen. Write Memory Scroll allows one
*     or more memories to be offset by an integral number of pixels. The
*     offset is relative to the default position in screen coordinates:
*     positive x and y values shift the memory upwards and to the right.
*     The memory is set to the nearest position that the hardware can
*     achieve.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     XOFF = INTEGER (Given)
*        X offset
*     YOFF = INTEGER (Given)
*        Y offset
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1989 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER XOFF
      INTEGER YOFF

*  Status:
      INTEGER STATUS
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWSC( DEVID( DISPID ), MEMID, NMEM, XOFF, YOFF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN
         CALL IXWWSC( DEVID( DISPID ), MEMID, NMEM, XOFF, YOFF, STATUS )

*   Else flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIZWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )

*+
*  Name:
*     IIZWZM
*
*  Purpose:
*     Write Memory Zoom
*
*  Invocation:
*     CALL IIZWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )
*
*  Description:
*     By default memories are displayed with a one-to-one mapping
*     between memory coordinates and screen coordinates. Write Memory
*     Zoom allows the memory to be scaled by integer factors. Positive
*     values of zoom factor cause memory pixels to be replicated on the
*     screen; negative values cause only every n'th pixel to be
*     displayed. Zooming leaves the position on the screen of the centre
*     of the memory unchanged. However, if one or more cursors are
*     visible, the position of the cursor having the lowest numbered
*     cursor identifier remains unchanged. If a region of interest is
*     visible, the centre of the region of interest remains unchanged.
*     Cursors have priority over regions of interest. The memory zoom
*     and position is set to the nearest values that the hardware can
*     achieve.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID( * ) = INTEGER (Given)
*        List of memory identifiers
*     NMEM = INTEGER (Given)
*        Number of memory identifiers
*     ZOOMF = INTEGER (Given)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     December 1988 (NE):
*        Original version.
*     December 1990 (NE):
*        Added X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER MEMID( * )
      INTEGER NMEM
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER XZOOM
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWZM( DEVID( DISPID ), MEMID, NMEM, ZOOMF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN

*   The X-windows implementation uses ZOOM = 1 to imply no zoom
         XZOOM = SIGN( ABS( ZOOMF ) + 1, ZOOMF )
         CALL IXWWZM( DEVID( DISPID ), MEMID, NMEM, XZOOM, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IIZWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*+
*  Name:
*     IIZWZP
*
*  Purpose:
*     Write Display Zoom and Pan
*
*  Invocation:
*     CALL IIZWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*  Description:
*     This function affects the entire display and not just the
*     individual memories. When the display is zoomed only part of the
*     display will be visible. The x and y offsets control which area
*     of the display will be visible by specifying by how many pixels
*     the display should be offset from the centre of the screen. The
*     offset is relative to the default position in screen coordinates:
*     positive x and y values shift the memory upwards and to the right.
*
*     See the IDI specification for a full description of the routine.
*     Terrett et al., 1988,
*     Astron.Astrophys.Suppl.Ser., vol 76, pp 263-304.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     XOFF = INTEGER (Given)
*        X offset
*     YOFF = INTEGER (Given)
*        Y offset
*     ZOOMF = INTEGER (Given)
*        Zoom factor
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Algorithm:
*     Verify the display identifier and call the device specific
*     routines
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     May 1990 (NE):
*        Original version.
*     December 1990 (NE):
*        Add X-windows interface
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Global Variables:
      INCLUDE 'idi_did'

*  Arguments Given:
      INTEGER DISPID
      INTEGER XOFF
      INTEGER YOFF
      INTEGER ZOOMF

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER XZOOM
*.

*   Reset status
      STATUS = IDI__OK

*   Verify display identifier
      IF ( ( DISPID .LT. 1 ) .OR. ( DISPID .GT. NUMIDS ) ) THEN
         STATUS = IDI__INDID
         GOTO 99
      ENDIF

*   If the device is an Ikon call the appropriate routine
      IF ( DTYPE( DISPID ) .EQ. D_IKON ) THEN
         CALL IKNWZP( DEVID( DISPID ), XOFF, YOFF, ZOOMF, STATUS )

*   If the device is an X-window call the appropriate routine
      ELSEIF ( DTYPE( DISPID ) .EQ. D_XWIN ) THEN

*   The X-windows implementation uses ZOOM = 1 to imply no zoom
         XZOOM = SIGN( ABS( ZOOMF ) + 1, ZOOMF )
         CALL IXWWZP( DEVID( DISPID ), XOFF, YOFF, XZOOM, STATUS )

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INDID
      ENDIF

  99  CONTINUE

      END

************************************************************************

      SUBROUTINE IDI_CLRFG ( IFLAG )

*+
*  Name:
*     IDI_CLRFG
*
*  Purpose:
*     Set clear flag
*
*  Invocation:
*     CALL IDI_CLRFG( IFLAG )
*
*  Description:
*     This routine sets the clear flag. The clear flag controls the
*     source of the device context. If the flag = 0 ( the default )
*     then the context is obtained from teh workstation state file.
*     If the flag = 1 then the context is obtained from the device.
*     This value also suppresses the writing out of the context at
*     closedown.
*
*  Arguments:
*     IFLAG = INTEGER (Given)
*        Clear flag
*
*  Algorithm:
*     Store the value in a common block.
*
*  Authors:
*     NE : Nick Eaton  (Durham University)
*
*  History:
*     November 1989 (NE):
*        Original version
*     February 1992 (NE):
*        Unix version is a dummy routine
*-
*  Type Definitions :
      IMPLICIT NONE

*  Arguments given :
      INTEGER IFLAG
*.

      END

