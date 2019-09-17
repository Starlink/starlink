      SUBROUTINE ATL_RDFCH( VFS, IAST, STATUS )
*+
*  Name:
*     ATL_RDFCH

*  Purpose:
*     Read an AST Object from a VFS using a FitsChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDFCH( VFS, IAST, STATUS )

*  Description:
*     Read an AST Object from a VFS (see atl2.c) using a FitsChan. The
*     FitsChan can be configured using a set of attribute settings specified
*     in the environment variable ATOOLS_CHATT_IN.

*  Arguments:
*     VFS = INTEGER (Given)
*        An identifier for the VFS holding the text, such as created by
*        routine ATL_RDVFS.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001, 2003 Central Laboratory of the Research
*     Copyright (C) 2019 East Asian Observatory
*     Councils. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     4-FEB-2003 (DSB):
*        Fixed bug which caused last line in FITS header to be ignored.
*     7-NOV-2017 (DSB):
*        Allow Channel attributes to be set using environment variable
*        ATOOLS_CHATT_IN.
*     16-SEP-2019 (DSB):
*        Changed to use a VFS as input rather than a GRP group. This
*        allows it to read long lines (such as used sometimes to store
*        MOCs).
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
      INTEGER VFS

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER VFSC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATLSRC/ VFSC, NEXT, SIZE

*  External References:
      EXTERNAL ATL_SRC2

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

*  Store the VFS identifer in common so that the source function can
*  get at it.
      VFSC = VFS

*  Initialise the next VFS element to be read.
      NEXT = 1

*  Store the size of the VFS.
      CALL ATL2_GTSIZ( VFS, SIZE, STATUS )

*  Create a FitsChan through which to read the Objects stored in the
*  VFS.
      CHAN = AST_FITSCHAN( ATL_SRC2, AST_NULL, ' ', STATUS )

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




      INTEGER FUNCTION ATL_SRC2( BUF, STATUS )
*+
*  Name:
*     ATL_SRC2

*  Purpose:
*     A source function for use with an AST FitsChan.

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

*  Arguments:
      CHARACTER BUF*80
      INTEGER STATUS

*  Global Variables.
      INTEGER VFSC
      INTEGER NEXT
      INTEGER SIZE
      COMMON /ATLSRC/ VFSC, NEXT, SIZE

* Local Variables:
      LOGICAL TRUNC

*  Initialise things to indicate "no more headers".
      BUF = ' '
      ATL_SRC2 = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If any elements remain to be read, get the next element from the VFS,
*  and increment the index of the next element to be read from the VFS.
*  No chance of truncation since all FITS headers are exactly 80 characters
*  long.
      IF( NEXT .LE. SIZE ) THEN
         CALL ATL2_GET( VFSC, NEXT, 1, BUF, TRUNC, STATUS )
         NEXT = NEXT + 1
         ATL_SRC2 = 1
      END IF

      END
