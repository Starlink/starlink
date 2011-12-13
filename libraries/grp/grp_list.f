      SUBROUTINE GRP_LIST( PARAM, INDXLO, INDXHI, COMNT, IGRP, STATUS )
*+
*  Name:
*     GRP_LIST

*  Purpose:
*     Write names to a text file specified by the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_LIST( PARAM, INDXLO, INDXHI, COMNT, IGRP, STATUS )

*  Description:
*     A text file is created with a name obtained from the environment
*     using the supplied parameter. The supplied comment is written to
*     the file as the first record (so long as it is not blank), using
*     the groups current comment character (see routine GRP_SETCC) .
*     All the names stored within the specified group section are then
*     written to the file, one name per record. If the group is case
*     insensitive (as set up by a call to routine GRP_SETCS) then the
*     names are written out in upper case, otherwise they are written
*     out as supplied.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to be used to get the name of the text file to
*        be created.
*     INDXLO = INTEGER (Given)
*        The low index limit of the group section. If both INDXLO and
*        INDXHI are zero, then the entire group is used.
*     INDXHI = INTEGER (Given)
*        The high index limit of the group section.
*     COMNT = CHARACTER * ( * ) (Given)
*        A comment line to form the first record in the file. The text
*        is prefixed with the group's current comment character before
*        being written to the file.
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER INDXLO
      INTEGER INDXHI
      CHARACTER COMNT*(*)
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OPEN               ! True if a text file has been
                                 ! successfully opened.
      INTEGER UNIT               ! IO unit on which the file is opened.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of a file from the environment, and open it.
      CALL GRP1_GTFIL( PARAM, 'WRITE', .TRUE., UNIT, OPEN, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If a file was opened, add the comment line.
      IF( OPEN ) THEN

*  Call GRP1_LISTU to write out the names to the opened fortran unit.
         CALL GRP1_LISTU( UNIT, INDXLO, INDXHI, COMNT, IGRP, STATUS )

*  Close the file.
         CLOSE( UNIT )

      END IF

*  If an error occurred, give a context message.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_LIST_ERR1',
     :                 'GRP_LIST: Unable to produce a list file '//
     :                 'containing the names in a group', STATUS )
      END IF

      END
