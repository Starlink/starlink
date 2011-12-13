      SUBROUTINE KPG1_PLGET( CI1, CI2, ARRAY, STATUS )
*+
*  Name:
*     KPG1_PLGET

*  Purpose:
*     Gets the colour palette for the currently open graphics device from
*     a supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLGET( CI1, CI2, ARRAY, STATUS )

*  Description:
*     This routine gets the colour palette from a supplied array and
*     loads it into the colour table of the currently open graphics device.
*
*  Arguments:
*     CI1 = INTEGER (Given)
*        The lowest colour index to change.
*     CI2 = INTEGER (Given)
*        The highest colour index to change.
*     ARRAY( 3, 0 : CI2 ) = REAL (Given)
*        The array containing the colour palette to load.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A graphics device must previously have been opened using PGPLOT.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1998 (DSB):
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
      INTEGER CI1
      INTEGER CI2
      REAL ARRAY( 3, 0 : CI2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Colour index

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each colour index.
      DO I = CI1, CI2

*  Ignore this entry if any of the values are negative.
         IF( ARRAY( 1, I ) .GE. 0.0 .AND.
     :       ARRAY( 2, I ) .GE. 0.0 .AND.
     :       ARRAY( 3, I ) .GE. 0.0 ) THEN

*  Store the new pen representation.
            CALL PGSCR( I, ARRAY( 1, I ), ARRAY( 2, I ), ARRAY( 3, I ) )

         END IF

      END DO

      END
