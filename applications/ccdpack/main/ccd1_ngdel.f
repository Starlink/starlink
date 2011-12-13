      SUBROUTINE CCD1_NGDEL( GRP, INDEX, VERB, STATUS )
*+
*  Name:
*     CCD1_NGDEL

*  Purpose:
*     Delete an NDF given its position in an NDG group.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NGDEL( GRP, INDEX, VERB, STATUS )

*  Description:
*     This routine will delete an NDF structure from disk, given its
*     position within an NDG group.  If its NDF identifier is already
*     available to the calling code, the NDF_DELET can be used, but
*     if the NDF is not currently open, this will erase it without
*     the calling code having to reopen it.
*
*     This routine checks to see whether it is in native (NDF) format
*     first, and also whether it represents a whole NDF or just a slice.
*     If it is a whole native NDF, then it opens it and calls NDF_DELET.
*     If not, then opening it would have a high overhead and would also
*     be useless, since it would be creating a new temporary NDF on disk
*     only to delete it; in this case no action is taken.

*  Arguments:
*     GRP = INTEGER (Given)
*        The NDG identifier for the NDG group referencing the NDFs.
*        Note this must be a proper NDG group and not just a GRP group
*        containing the NDF names, since NDG_GTSUP is called on its
*        elements.
*     INDEX = INTEGER (Given)
*        The index of the element in GRP to be erased.
*     VERB = LOGICAL (Given)
*        If true, a message is logged through the CCDPACK logging system
*        for each NDF which is actually deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUL-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      INTEGER GRP
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM) FIELDS( 6 ) ! NDG supplementary information
      INTEGER INDF               ! NDF identifier
      LOGICAL VERB               ! Log message about deleted files?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDG supplementary data for this NDF.
      CALL NDG_GTSUP( GRP, INDEX, FIELDS, STATUS )

*  Check whether it is in native format.
      IF ( FIELDS( 3 ) .EQ. '.sdf' ) THEN

*  Check whether it just represents a slice of NDF.
         IF ( FIELDS( 1 ) .EQ. ' ' ) THEN

*  If it is an NDF, get the NDF identifier and erase it.
            CALL NDG_NDFAS( GRP, INDEX, 'UPDATE', INDF, STATUS )
            CALL NDF_DELET( INDF, STATUS )

*  Log if so requested.
            IF ( VERB ) THEN
               CALL MSG_SETC( 'NDF', FIELDS( 6 ) )
               CALL CCD1_MSG( ' ', '   NDF ^NDF deleted', STATUS )
            END IF
         END IF

*  If it is not an NDF, take no action.
      END IF

      END
* $Id$
