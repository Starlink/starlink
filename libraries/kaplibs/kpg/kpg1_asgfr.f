      SUBROUTINE KPG1_ASGFR( STATUS )
*+
*  Name:
*     KPG1_ASGFR

*  Purpose:
*     Reads a line of an AST Object description from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGFR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine reads a line of an AST Object description from a GRP
*     group, removing the first character if it is a "#" or a "!". It then
*     returns the line of text to the AST library using AST_PUTLINE. It is
*     intended to be used as a source function with AST_CHANNEL.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'GRP_ERR'          ! GRP error constants

*  Global Constants:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTGRP = INTEGER (Read)
*           GRP identifier for the group.
*        ASTLN = INTEGER (Read and Write)
*           The index of the previous element read from the group.

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER LINE*(GRP__SZNAM)! Text to be written to the group
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Increment the index of the last element read.
      ASTLN = ASTLN + 1

*  Get the next line of text from the group.
      LINE = ' '
      CALL GRP_GET( ASTGRP, ASTLN, 1, LINE, STATUS )

*  If there are no more elements to be read, annul the error and return
*  a negative line length.
      IF( STATUS .EQ. GRP__OUTBN ) THEN
         CALL ERR_ANNUL( STATUS )
         ASTLN = ASTLN - 1
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  Otherwise...
      ELSE

*  If the first character is a "!" or a "#" remove it.
         IF( LINE( 1 : 1 ) .EQ. '#' .OR. LINE( 1 : 1 ) .EQ. '!' ) THEN
            LINE( 1 : 1 ) = ' '
            CALL CHR_LDBLK( LINE )
         END IF

*  Return it to the AST library.
         CALL AST_PUTLINE( LINE, CHR_LEN( LINE ), STATUS )

      END IF

      END
