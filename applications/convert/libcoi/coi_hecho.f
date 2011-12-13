      SUBROUTINE COI_HECHO( NLINES, TEXT, STATUS )
*+
*  Name:
*     COI_HECHO

*  Purpose:
*     Writes history text to the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_HECHO( NLINES, TEXT, STATUS )

*  Description:
*     This routine appends the history text associated with an
*     NDF to the current IRAF OIF header.  It is not called directly
*     (by COI_WHISR), but is passed as an external argument to routine
*     NDF_HOUT.  (It is an equivalent to NDF_HECHO.  See SUN/33 for
*     more details.)

*  Arguments:
*     NLINES = INTEGER (Given)
*        Number of lines of history text.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        The history text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The argument list should not be changed.

*  Prior Requirements:
*     The NDF and the IRAF file must already be open.  The other
*     headers should have been written.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 25 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'COI_CMN'          ! Common block for passing required
                                 ! additional arguments
*        FIMDES = INTEGER (Read)
*           The Fortran logical unit number of the IRAF file.

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) HEADER  ! Output header card
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Append the lines of history text to the IRAF headers.
      DO I = 1, NLINES
         HEADER = 'HISTORY   '//TEXT( I )
         CALL ADLINE( FIMDES, HEADER )
      END DO

      END
