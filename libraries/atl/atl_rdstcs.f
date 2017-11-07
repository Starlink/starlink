      SUBROUTINE ATL_RDSTCS( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL_RDSTCS

*  Purpose:
*     Read an AST Object from a GRP group using an StcsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDSTCS( IGRP, IAST, STATUS )

*  Description:
*     Read an AST Object from a GRP group using an StcsChan.The StcsChan
*     can be configured using a set of attribute settings specified in
*     the environment variable ATOOLS_CHATT_IN.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group holding the text.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-JUN-2009 (DSB):
*        Original version.
*     7-NOV-2017 (DSB):
*        Allow Channel attributes to be set using environment variable
*        ATOOLS_CHATT_IN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants

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
      CHARACTER ATTRS*500
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

*  Create an StcsChan through which to read the Objects stored in the
*  group.
      CHAN = AST_STCSCHAN( ATL_SRC1, AST_NULL, 'ReportLevel=2',
     :                     STATUS )

*  See if any attributes should be set in the channel.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PSX_GETENV( 'ATOOLS_CHATT_IN', ATTRS, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL AST_SET( CHAN, ATTRS, STATUS )
            IF( STATUS .EQ. AST__BADAT ) CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Attempt to read an object from the StcsChan.
      IAST = AST_READ( CHAN, STATUS )
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ATL_RDSTCS_ERR1', 'No AST Object could be '//
     :                 'read from the supplied file.', STATUS )
      END IF

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
