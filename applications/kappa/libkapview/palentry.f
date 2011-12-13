      SUBROUTINE PALENTRY( STATUS )
*+
*  Name:
*     PALENTRY

*  Purpose:
*     Enters a colour into an image display's palette.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PALENTRY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application obtains a colour and enters it into the palette
*     portion of the current image display's colour table.  The palette
*     comprises up to 16 colours and is intended to provide coloured
*     annotations, borders, axes, graphs etc. that are unaffected by
*     changes to the lookup table used for images.
*
*     A colour is specified either by the giving the red, green, blue
*     intensities; or named colours.

*  Usage:
*     palentry palnum colour [device]

*  ADAM Parameters:
*     COLOUR() = LITERAL (Read)
*        A colour to be added to the palette at the entry given by
*        parameter PALNUM.  It is one of the following options.
*
*          o  A named colour from the standard colour set, which may
*          be abbreviated.  If the abbreviated name is ambiguous the
*          first match (in alphabetical order) is selected.  The case
*          of the name is ignored.  Some examples are "Pink", "Yellow",
*          "Aquamarine", and "Orchid".
*
*          o  Normalised red, green, and blue intensities separated by
*          commas or spaces.  Each value must lie in the range 0.0--1.0.
*          For example, "0.7,0.7,1.0" would give a pale blue.
*
*          o  An HTML colour code such as #ff002d.
*
*     DEVICE = DEVICE (Read)
*        Name of the image display to be used.
*        [Current image-display device]
*     PALNUM = _INTEGER (Read)
*        The number of the palette entry whose colour is to be
*        modified.  PALNUM must lie in the range zero to the minimum
*        of 15 or the number of colour indices minus one.  The
*        suggested default is 1.

*  Examples:
*     palentry 5 gold
*        This makes palette entry number 5 have the colour gold in the
*        reserved portion of the colour table of the current image
*        display.
*     palentry 12 [1.0,1.0,0.3] xwindows
*        This makes the xwindows device's palette entry number 12 have
*        a pale-yellow colour.

*  Notes:
*     - The effects of this command will only be immediately apparent
*     when run on X windows which have 256 colours (or other similar
*     pseudocolour devices).  On other devices (for instance, X windows
*     with more than 256 colours) the effects will only become apparent
*     when subsequent graphics applications are run.

*  Related Applications:
*     KAPPA: PALDEF, PALREAD, PALSAVE.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1998-1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 December 1 (MJC):
*        Made to work with WINDOW_OVERLAY devices.
*     30-OCT-1998 (DSB):
*        Modified to save current palette in the adam directory so that
*        subsequent PGPLOT applications can read it back in again.
*     22-JUL-1999 (TDCA):
*        Modified to use PGPLOT.
*     1-OCT-1999 (DSB):
*        Tidied up.  Use KPG1_PGOPN instead of AGP_ASSOC.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
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
      INTEGER PALNUM             ! Palette entry number to have its
                                 ! colour changed
      INTEGER IPIC               ! AGI identifier for current picture
      INTEGER UP                 ! Highest available colour index
      REAL RGBINT( NPRICL )      ! RGB intensities of the chosen colour
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open up PGPLOT in update mode as only some colours are to be changed.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Check whether chosen device is an 'image display'.  It must have
*  a suitable minimum number of colour indices, and will not reset
*  when opened.
      CALL KPG1_PQVID( 'DEVICE', 'IMAGE_DISPLAY,IMAGE_OVERLAY,'/
     :                /'WINDOW,WINDOW_OVERLAY,MATRIX_PRINTER',
     :                'COLOUR', 2, UP, STATUS )

*  The suggested default will usually have a grey colour.
      CALL PAR_GDR0I( 'PALNUM', 1, 0, MIN( CTM__RSVPN, UP ) - 1,
     :                .FALSE., PALNUM, STATUS )

*  Get one colour for the palette number.
      CALL KPG1_GPCOL( 'COLOUR', RGBINT, STATUS )

*  Install the palette into image-display colour table.
      IF ( STATUS .EQ. SAI__OK ) CALL PGSCR( PALNUM, RGBINT( 1 ),
     :                                       RGBINT( 2 ), RGBINT( 3 ) )

*  Save the modified palette entry in the parameter-file directory so
*  that it can be read back again by subsequent applications (PGPLOT
*  resets the colour palette when it opens a device, so the palette then
*  needs to be re-instated).  Other elements in the saved palette are
*  left unchanged.
      CALL KPG1_PLSAV( PALNUM, PALNUM, .FALSE., STATUS )

*  Shut down the graphics system.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PALENTRY_ERR', 'PALENTRY: Unable to enter '/
     :                 /'a colour into the palette.', STATUS )
      END IF

      END
