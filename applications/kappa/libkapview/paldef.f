      SUBROUTINE PALDEF( STATUS )
*+
*  Name:
*     PALDEF

*  Purpose:
*     Loads the default palette to a colour table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PALDEF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application loads the standard palette of colours to fill
*     the portion of the current image display's colour table which is
*     reserved for the palette.  The palette comprises 16 colours and
*     is intended to provide coloured annotations, borders, axes,
*     graphs etc. that are unaffected by changes to the lookup table
*     used for images.
*
*     Pen 0 (the background colour) and pen 1 (the foreground colour) are
*     set to the default values for the specified graphics device. Thus
*     they may be white on black for an X window, but black on white for
*     a printer. The other colours in the standard palette are:
*
*     - 2: Red
*     - 3: Green
*     - 4: Blue
*     - 5: Yellow
*     - 6: Magenta
*     - 7: Cyan
*     - 8 to 15: Black

*  Usage:
*     paldef [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.  [Current image-display device]

*  Examples:
*     paldef
*        This loads the standard palette into the reserved portion of
*        the colour table of the current image display.
*     paldef xwindows
*        This loads the standard palette into the reserved portion of
*        the colour table of the xwindows device.

*  Notes:
*     - The effects of this command will only be immediately apparent
*     when run on X windows which have 256 colours (or other similar
*     pseudocolour devices). On other devices (for instance, X windows
*     with more than 256 colours) the effects will only become apparent
*     when subsequent graphics applications are run.

*  Related Applications:
*     KAPPA: PALENTRY, PALREAD, PALSAVE.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998-1999, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 19 (MJC):
*        Original version.
*     1994 January 24 (MJC):
*        Allowed the device to be a MATRIX_PRINTER.
*     30-OCT-1998 (DSB):
*        Modified to save current palette in the adam directory so that
*        subsequent PGPLOT applications can read it back in again.
*     22-JUL-1999 (TDCA):
*        Modified to use PGPLOT.
*     30-SEP-1999 (DSB):
*        Tidied up.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colour-table management constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER IPIC1              ! Current picture identifier
      INTEGER UP                 ! Highest available colour index.
      REAL PALETT( NPRICL, 0:CTM__RSVPN - 1 ) ! Reserved palette colours

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open up workstation in update mode as only some colours
*  are to be changed.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC1, STATUS )

*  Check whether chosen device is an 'image display'.  It must have
*  a suitable minimum number of colour indices, and will not reset
*  when opened.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,MATRIX_PRINTER', 'RESET',
     :                8 + CTM__RSVPN, UP, STATUS )

*  Create the pre-defined palette colours. There are only CTM__RSVPN
*  standard colour indices that form a palette.
      CALL KPS1_CLPAL( CTM__RSVPN, 1, PALETT, STATUS )

*  Install the palette into image-display colour table.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO  I = 0, CTM__RSVPN - 1, 1
            CALL PGSCR( I, PALETT( 1, I ), PALETT( 2, I ),
     :                  PALETT( 3, I ) )
         END DO
      END IF

*  Save the palette in the adam directory so that it can be read back
*  again by subsequent applications (PGPLOT resets the colour palette
*  when it opens a device, so the palette then needs to be re-instated).
*  The first two pens ( 0 and 1, the background and foreground colours)
*  are not saved, but reset to their default (unspecified) values. This
*  means (for instance), that foreground text will be white on black
*  on an xwindow, but black on white on a printer.
      CALL KPG1_PLSAV( 2, 0, .TRUE., STATUS )

*  Close the graphics system.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

* If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PALDEF_ERR', 'PALDEF: Unable to load the '//
     :                 'standard palette colours.', STATUS )
      END IF

      END
