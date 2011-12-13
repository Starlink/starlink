      SUBROUTINE CTG_GTSUP( IGRP, I, FIELDS, STATUS )
*+
*  Name:
*     CTG_GTSUP

*  Purpose:
*     Get supplemental information for a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_GTSUP( IGRP, I, FIELDS, STATUS )

*  Description:
*     Returns the supplemental information associated with a given entry
*     in a CTG group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The CTG group as returned by CTG_ASSOC, etc. This should be the last
*        group in a GRP owner-slave chain.
*     I = INTEGER (Given)
*        The index of the required entry.
*     FIELDS( 5 ) = CHARACTER * ( * ) (Returned)
*        The supplemental information associated with the entry specified
*        by I. Each element of the returned array contains the following:
*
*           1 - FITS extension specification (e.g. "{3}") if any
*           2 - File type
*           3 - Base file name
*           4 - Directory path
*           5 - Full catalogue specification
*
*        This information is obtained from a set of groups associated with
*        the supplied group IGRP by means of a chain of GRP "owner-slave"
*        relationships. If any of these groups do not exist, the correponding
*        elements of the above array are returned blank. Note, Element 5,
*        the full catalogue specification, is obtained directly from the
*        supplied group IGRP.
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
*     13-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER I

*  Arguments Returned:
      CHARACTER FIELDS( 5 )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPS              ! Group holding FITS extension fields
      INTEGER IGRPT              ! Group holding file type fields
*.

*  Initialise the returned fields.
      FIELDS( 1 ) = ' '
      FIELDS( 2 ) = ' '
      FIELDS( 3 ) = ' '
      FIELDS( 4 ) = ' '
      FIELDS( 5 ) = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the full file spec from the supplied group.
      CALL GRP_GET( IGRP, I, 1, FIELDS( 5 ), STATUS )

*  Get the owner of the supplied group.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      IF( IGRPD .EQ. GRP__NOID ) THEN

*  Get the directory path from this group.
         CALL GRP_GET( IGRPD, I, 1, FIELDS( 4 ), STATUS )

*  Get the owner of this group.
         CALL GRP_OWN( IGRPD, IGRPB, STATUS )
         IF( IGRPB .EQ. GRP__NOID ) THEN

*  Get the base file name from this group.
            CALL GRP_GET( IGRPB, I, 1, FIELDS( 3 ), STATUS )

*  Get the owner of this group.
            CALL GRP_OWN( IGRPB, IGRPT, STATUS )
            IF( IGRPT .EQ. GRP__NOID ) THEN

*  Get the file type from this group.
               CALL GRP_GET( IGRPT, I, 1, FIELDS( 2 ), STATUS )

*  Get the owner of this group.
               CALL GRP_OWN( IGRPT, IGRPS, STATUS )
               IF( IGRPS .EQ. GRP__NOID ) THEN

*  Get the FITS extension from this group.
                  CALL GRP_GET( IGRPS, I, 1, FIELDS( 1 ), STATUS )

               END IF
            END IF
         END IF
      END IF

      END
