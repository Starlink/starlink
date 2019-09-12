      SUBROUTINE ATL_RDMOC( IGRP, IAST, STATUS )
*+
*  Name:
*     ATL_RDMOC

*  Purpose:
*     Read an AST Object from a GRP group using an MocChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDMOC( IGRP, IAST, STATUS )

*  Description:
*     Read an AST Object from a GRP group using an MocChan.The MocChan
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
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     9-MAY-2019 (DSB):
*        Original version.
*     12-SEP_2019 (DSB):
*        Change to use a source function that preserves the end-of-line
*        white space, which is significant in the MOC string serialisation.
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
      EXTERNAL ATL_SRC3

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

*  Create an MocChan through which to read the Objects stored in the
*  group.
      CHAN = AST_MOCCHAN( ATL_SRC3, AST_NULL, 'ReportLevel=2',
     :                    STATUS )

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

*  Attempt to read an object from the MocChan.
      IAST = AST_READ( CHAN, STATUS )
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ATL_RDMOC_ERR1', 'No AST Object could be '//
     :                 'read from the supplied file.', STATUS )
      END IF

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END



      SUBROUTINE ATL_SRC3( STATUS )
*+
*  Name:
*     ATL_SRC3

*  Purpose:
*     A source function for use with a MocChan.

*  Description:
*     White space, including white space at the ends of lines, is
*     significant in the MOC string serialisation. This function is like
*     ATL_SRC1 except that it adds a space to the end of each line
*     supplied to AST. This space is equivalent to the carriage return/
*     line-feed character marking the end of each line of text, but which
*     will have been removed by GRP.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
      INTEGER BLEN

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no more elements in the group, return a length of -1.
      IF( NEXT .GT. SIZE ) THEN
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  Otherwise, get the element from the group.
      ELSE
         CALL GRP_GET( IGRPC, NEXT, 1, BUF, STATUS )

*  Get its used length.
         BLEN = CHR_LEN( BUF )

*  If possible, increase this length by one to include a trailing space
*  (Fortran strings are space padded). This space acts as a separator
*  between the last value on this line and the first value on the next line.
*  The carriage return/line-feed at the end of the line is supposed to
*  serve this purpose, but GRP will have removed it.
         IF( BLEN .LT. LEN( BUF ) ) BLEN = BLEN + 1

*  Store the line in the channel, and increment the index of the next
*  element to be read from the group.
         CALL AST_PUTLINE( BUF, BLEN, STATUS )
         NEXT = NEXT + 1
      END IF

      END
