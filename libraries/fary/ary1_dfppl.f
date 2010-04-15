      SUBROUTINE ARY1_DFPPL( LOCP, NAME, LOC, STATUS )
*+
*  Name:
*     ARY1_DFPPL

*  Purpose:
*     Create a primitive array with an entry in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DFPPL( LOCP, NAME, LOC< STATUS )

*  Description:
*     The routine creates an placeholder object for a deferred
*     primitive array.

*  Arguments:
*     LOCP = CHARACTER * ( * ) (Given)
*        Locator for the parent object in which the primitive array is to
*        be created.
*     NAME = CHARACTER * ( * ) (Given)
*        The name for the primitive array.
*     LOC = CHARACTER * ( * ) (Returned)
*        A locator to the new placeholder object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     18-JUL-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER LOCP*(*)
      CHARACTER NAME*(*)

*  Arguments Returned:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Does component already exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the component does not already exist.
      CALL DAT_THERE( LOCP, NAME, THERE, STATUS )
      IF( .NOT. THERE ) THEN

*  Create an ARRAY structure in the given parent object, and get a
*  locator to it.
         CALL DAT_NEW( LOCP, NAME, 'ARRAY', 0, 0, STATUS )
         CALL DAT_FIND( LOCP, NAME, LOC, STATUS )

*  Put a VARIANT component in here indicating that the ARRAY is a deferred
*  primitive array.
         CALL DAT_NEW0C( LOC, 'VARIANT', 9, STATUS )
         CALL CMP_PUT0C( LOC, 'VARIANT', 'PRIMITIVE', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DFPPL', STATUS )

      END
