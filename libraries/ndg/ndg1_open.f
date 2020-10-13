      SUBROUTINE NDG1_OPEN( NAME, PLACE, STATUS )
*+
*  Name:
*     NDG1_OPEN

*  Purpose:
*     Get an NDF place holder for a new NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_OPEN( NAME, PLACE, STATUS )

*  Description:
*     This routine returns an NDF place holder for a new NDF, ensuring that
*     any HDS container file or structures required to created the named
*     NDF exist. If they do not exist, they are created.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The full HDS path to the NDF to be created.
*     PLACE = INTEGER (Returned)
*        The place holder.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1999 (DSB):
*        Original version.
*     13-OCT-2020 (DSB):
*        Only annull the error if it indicates that one or more of the
*        required parent structures do not exit. This avoids loosing
*        other useful error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'DAT_ERR'          ! HDS error constants.

*  Arguments Given:
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER PLACE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! Dummy argument

*.

*  Initialise
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context
      CALL ERR_BEGIN( STATUS )

*  Attempt to get a place holder for the named NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME, 'WRITE', 'NEW', INDF, PLACE,
     :               STATUS )

*  If an error was reported indicating that the named NDF cannot be
*  created because one or more of its parents do not exist, annul the
*  error, ensure that the structure required to contain the NDF exists,
*  and try to get the place holder again.
      IF( STATUS .EQ. DAT__FILNF .OR.   ! Container file not found
     :    STATUS .EQ. DAT__NAMIN ) THEN ! One of more parent objects not found
         CALL ERR_ANNUL( STATUS )
         CALL NDG1_CRPTH( NAME, STATUS )
         CALL NDF_OPEN( DAT__ROOT, NAME, 'WRITE', 'NEW', INDF, PLACE,
     :                  STATUS )
      END IF

*  End the error context
      CALL ERR_END( STATUS )

      END
