      SUBROUTINE CVG_HECHO( NLINES, TEXT, STATUS )
*+
*  Name:
*     CVG_HECHO

*  Purpose:
*     Writes history text to the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_HECHO( NLINES, TEXT, STATUS )

*  Description:
*     This routine appends the history text associated with an
*     NDF to the current FITS header.  It is not called directly (by
*     CVG_WHISR), but is passed as an external argument to routine
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
*     -  There is no error checking of the FITSIO status.

*  Prior Requirements:
*     The NDF and the FITS file must already be open.  The current
*     HDU in the FITS file should be the primary and the other headers
*     should have been written.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 January 13 (MJC):
*        Original version.
*     19-NOV-2013 (DSB):
*        Moved from CONVERT to CVG.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Global Variables:
      INCLUDE 'CVG_CMN'          ! Common block for passing required
                                 ! additional arguemnts
*        FFUNIT = INTEGER (Read)
*           The Fortran logical unit number of the FITS file.

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = CVG__FITSOK

*  Write the text.
      DO I = 1, NLINES
         CALL FTPHIS( FFUNIT, TEXT( I ), FSTAT )
      END DO

      END
