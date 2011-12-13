      SUBROUTINE KPS1_AGNCV( X1, Y1, XW, YW, STATUS )
*+
*  Name:
*     KPS1_AGNCV

*  Purpose:
*     Displays text telling the user what the latest value is for the
*     cursor position in ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNCV( X1, Y1, XW, YW, STATUS )

*  Description:
*     The routine displays the latest value for the cursor position.
*     When an image is being displayed output is in the form of pixel
*     co-ordinates.

*  Arguments:
*     X1 = REAL (Given)
*        X pixel co-ordinate of the left-hand edge of the image.
*     Y1 = REAL (Given)
*        Y pixel co-ordinate of the bottom edge of the image.
*     XW = REAL (Given)
*        X pixel co-ordinate.
*     YW = REAL (Given)
*        Y pixel co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993-1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-Mar-1993 (GJP)
*        Original version
*     5-DEC-1994 (DSB)
*        Tidied up.  Name changed from ARDG1_CURVD to KPS1_AGNCV.
*        Layout of displayed text modified.
*     1995 March 15 (MJC):
*        Shortened long lines, corrected typo's, and prologue
*        indentation.  Removed the "data" co-ordinates which were
*        `relative' to the picture origin to avoid confusion with
*        genuine data or axis co-ordinates.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL XW                    ! X pixel co-ordinate
      REAL X1                    ! X pixel co-ordinate of the image
                                 ! edge
      REAL YW                    ! Y pixel co-ordinate
      REAL Y1                    ! Y pixel co-ordinate of the image
                                 ! bottom

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'KPS1_AGNCV_MSG1', 'Cursor position...', STATUS )

*  Put the pixel co-ordinates into message tokens.
      CALL MSG_SETR( 'XVALW', XW )
      CALL MSG_SETR( 'YVALW', YW )

*  Display the current X and Y values.
      CALL MSG_OUT( 'KPS1_AGNCV_MSG3', '   Pixel co-ordinates: '/
     :              /'(^XVALW, ^YVALW)', STATUS )
      CALL MSG_BLANK( STATUS )

*  The following call achieves graphics/text synchronisation.
      CALL MSG_SYNC( STATUS )

      END
