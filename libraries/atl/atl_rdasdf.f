      SUBROUTINE ATL_RDASDF( VFS, IAST, STATUS )
*+
*  Name:
*     ATL_RDASDF

*  Purpose:
*     Read an AST Object from a VFS using a YamlChan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_RDASDF( VFS, IAST, STATUS )

*  Description:
*     Read an AST Object from a VFS (see atl2.c) using a YamlChan with
*     ASDF encoding. The YamlChan can be configured using a set of attribute
*     settings specified in the environment variable ATOOLS_CHATT_IN.

*  Arguments:
*     VFS = INTEGER (Given)
*        The VFS holding the text, such as returned by ATL_RDVFS.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
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
*     15-SEP-2020 (DSB):
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
      LOGICAL READCH
      LOGICAL BEGIN
      INTEGER MXLEN
      INTEGER IPLINE
      COMMON /ATLSRC/ VFSC, NEXT, SIZE, READCH, BEGIN, MXLEN, IPLINE

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

*  Store the VFS in common so that the source function can get at it.
      VFSC = VFS

*  Initialise the next VFS element to be read.
      NEXT = 1

*  Store the size of the VFS.
      CALL ATL2_GTSIZ( VFS, SIZE, STATUS )

*  Get the maximum length of a line in the VFS and allocate a buffer to
*  store one line.
      CALL ATL2_GTMXL( VFS, MXLEN, STATUS )
      CALL PSX_CALLOC( MXLEN, '_BYTE', IPLINE, STATUS )

*  Create a YamlChan through which to read the Objects stored in the
*  VFS.
      CHAN = AST_YAMLCHAN( ATL_SRC1, AST_NULL, 'YamlEncoding=ASDF',
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

*  Attempt to read an object from the YamlChan.
      IAST = AST_READ( CHAN, STATUS )
      IF( IAST .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ATL_RDASDF_ERR1', 'No AST Object could be '//
     :                 'read from the supplied file.', STATUS )
      END IF

*  Free line buffer.
      CALL PSX_FREE( IPLINE, STATUS )

*  Export the returned Object from the current AST context so that it is
*  not annulled by the following call to AST_END. If an error has occurred,
*  the Object will not be exported, and so will be annulled by AST_END.
      CALL AST_EXPORT( IAST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
