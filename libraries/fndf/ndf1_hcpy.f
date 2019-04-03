      SUBROUTINE NDF1_HCPY( NLINES, HIST, TEXT, STATUS )
*+
*  Name:
*     NDF1_HCPY

*  Purpose:
*     Copy lines of history text.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HCPY( NLINES, HIST, TEXT, STATUS )

*  Description:
*     The routine copies lines of history text from one character array
*     to another. Character assignment is done using the normal Fortran
*     rules (truncating or padding with blanks, as appropriate).

*  Arguments:
*     NLINES = INTEGER (Given)
*        Number of lines of text to copy.
*     HIST( NLINES ) = CHARACTER * ( * ) (Returned)
*        Array of output history lines to receive text.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        Array of input text lines to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The conventionl argument order is not followed by this routine in
*     order to simplify the passing of mapped character arrays for the
*     HIST argument.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     7-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( * )

*  Arguments Returned:
      CHARACTER * ( * ) HIST( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy each text line to the history array.
      DO 1 I = 1, NLINES
         HIST( I ) = TEXT( I )
 1    CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HCPY',
     :                                            STATUS )

      END
