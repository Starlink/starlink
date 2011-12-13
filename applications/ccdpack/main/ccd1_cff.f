
      SUBROUTINE CCD1_CFF( IMAGE, NCOLS, NLINES, STATUS )
*+
*  Name:
*     CCD1_CFF

*  Purpose:
*     Creates a ramped image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CFF( IMAGE, NCOLS, NLINES, STATUS )

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOLS
      INTEGER NLINES

*  Arguments Given and Returned:
      REAL IMAGE( NCOLS, NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL INTER
      INTEGER I, J

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      INTER  = 1.0 / ( NLINES - 1 )
      DO  J  =  1, NLINES
         DO  I  =  1, NCOLS
            IMAGE( I, J ) = 0.5 +  ( INTER *  ( I - 1 ) )
         END DO
      END DO

      END
* $Id$
