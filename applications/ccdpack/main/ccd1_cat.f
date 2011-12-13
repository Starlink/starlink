      SUBROUTINE CCD1_CAT( FILE, STATUS )
*+
*  Name:
*     CCD1_CAT

*  Purpose:
*     Lists the contents of a file to the user.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_CAT( FILE, STATUS )

*  Description:
*     This routine outputs the contents of a text file using the
*     message system to deliver it.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the file to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Message system constants
      INCLUDE 'FIO_ERR'         ! FIO errors codes

*  Arguments Given:
      CHARACTER * ( * ) FILE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG) LINE ! Line buffer for reads
      INTEGER FD                ! File descriptor

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Now open the file.
      CALL FIO_OPEN( FILE, 'READ', 'LIST', 0, FD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Write out its name.
         CALL MSG_SETC( 'FILE', FILE )
         CALL MSG_OUT( ' ', 'Contents of file: ^FILE', STATUS )
         CALL MSG_BLANK( STATUS )

*  Now loop until file is all read.
 1       CONTINUE
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL FIO_READF( FD, LINE, STATUS )
            CALL MSG_OUT( ' ', LINE, STATUS )
            GO TO 1
         END IF
         IF ( STATUS .EQ. FIO__EOF ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL FIO_CLOSE( FD, STATUS )
      END IF
      END
