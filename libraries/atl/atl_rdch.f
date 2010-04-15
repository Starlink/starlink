      SUBROUTINE ATL_RDCH( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL_RDCH

*  Purpose:
*     Read an AST Object from a GRP group using a Channel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDCH( IGRP, IAST, STATUS )

*  Description:
*     Read an AST Object from a GRP group using a Channel.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group holding the text.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER IGRPC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATLSRC/ IGRPC, NEXT, SIZE

*  External References:
      EXTERNAL ATL_SRC1

*  Local Variables:
      INTEGER CHAN
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Store the group identifer in common so that the source function can
*  get at it.
      IGRPC = IGRP

*  Initialise the next group element to be read.
      NEXT = 1

*  Store the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Create a Channel through which to read the Objects stored in the
*  group.
      CHAN = AST_CHANNEL( ATL_SRC1, AST_NULL, ' ', STATUS )

*  Attempt to read an object from the current channel.
      IAST = AST_READ( CHAN, STATUS )
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ATL_RDCH_ERR1', 'No AST Object could be '//
     :                 'read from the supplied file.', STATUS )
      END IF

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END


      SUBROUTINE ATL_SRC1( STATUS )
*+
*  Name:
*     ATL_SRC1

*  Purpose:
*     A source function for use with a standard AST Channel.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'GRP_PAR'

*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER IGRPC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATLSRC/ IGRPC, NEXT, SIZE

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER BUF*(GRP__SZNAM)

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no more lements in the group, return a length of -1.
      IF( NEXT .GT. SIZE ) THEN
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  Otherwise, get the element from the group, store it in the channel,
*  and increment the index of the next element to be read from the group.
      ELSE
         CALL GRP_GET( IGRPC, NEXT, 1, BUF, STATUS )
         CALL AST_PUTLINE( BUF, CHR_LEN( BUF ), STATUS )
         NEXT = NEXT + 1
      END IF

      END
