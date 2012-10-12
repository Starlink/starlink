      SUBROUTINE NDG_GTSUP( IGRP, I, FIELDS, STATUS )
*+
*  Name:
*     NDG_GTSUP

*  Purpose:
*     Get supplemental information for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_GTSUP( IGRP, I, FIELDS, STATUS )

*  Description:
*     Returns the supplemental information associated with a given entry
*     in an NDG group.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The NDG group as returned by NDG_ASSOC, etc.
*     I = INTEGER (Given)
*        The index of the required entry.
*     FIELDS( 6 ) = CHARACTER * ( * ) (Returned)
*        The supplemental information associated with the entry specified
*        by I. Each element of the returned array contains the following:
*
*           1 - NDF slice specification (if any)
*           2 - HDS path (if any)
*           3 - File type
*           4 - Base file name
*           5 - Directory path
*           6 - Full NDF specification
*
*        If the supplied group is the last group in a GRP owner-slave chain,
*        then this information is obtained from these groups. If any of these
*        groups do not exist, the corresponding elements of the above array
*        are returned holding values formed by parsing the full file
*        specifications in the supplied group. Note, Element 6, the full NDF
*        specification, is obtained directly from the supplied group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     6-SEP-1999 (DSB):
*        Original version.
*     22-FEB-2001 (DSB):
*        Changed ".EQ. GRP__NOID" to ".NE. GRP__NOID" through out.
*     12-OCT-2012 (DSB):
*        If the supplied group has no associated groups holding
*        supplemental information (e.g. if the group was created by
*        NDG_CREXP), then return field values formed by parsing the
*        full file spec.
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
      CHARACTER FIELDS( 6 )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPH              ! Group holding HDS path fields
      INTEGER IGRPS              ! Group holding NDF slice fields
      INTEGER IGRPT              ! Group holding file type fields
*.

*  Initialise the returned fields.
      FIELDS( 1 ) = ' '
      FIELDS( 2 ) = ' '
      FIELDS( 3 ) = ' '
      FIELDS( 4 ) = ' '
      FIELDS( 5 ) = ' '
      FIELDS( 6 ) = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the full file spec from the supplied group.
      CALL GRP_GET( IGRP, I, 1, FIELDS( 6 ), STATUS )

*  Parse the full file spec to get defaults for any missing fields.
      CALL NDG1_FSPLIT( FIELDS( 6 ), FIELDS( 5 ), FIELDS( 4 ),
     :                  FIELDS( 3 ), FIELDS( 2 ), FIELDS( 1 ), STATUS )

*  Get the owner of the supplied group.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      IF( IGRPD .NE. GRP__NOID ) THEN

*  Get the directory path from this group.
         CALL GRP_GET( IGRPD, I, 1, FIELDS( 5 ), STATUS )

*  Get the owner of this group.
         CALL GRP_OWN( IGRPD, IGRPB, STATUS )
         IF( IGRPB .NE. GRP__NOID ) THEN

*  Get the base file name from this group.
            CALL GRP_GET( IGRPB, I, 1, FIELDS( 4 ), STATUS )

*  Get the owner of this group.
            CALL GRP_OWN( IGRPB, IGRPT, STATUS )
            IF( IGRPT .NE. GRP__NOID ) THEN

*  Get the file type from this group.
               CALL GRP_GET( IGRPT, I, 1, FIELDS( 3 ), STATUS )

*  Get the owner of this group.
               CALL GRP_OWN( IGRPT, IGRPH, STATUS )
               IF( IGRPH .NE. GRP__NOID ) THEN

*  Get the HDS path from this group.
                  CALL GRP_GET( IGRPH, I, 1, FIELDS( 2 ), STATUS )

*  Get the owner of this group.
                  CALL GRP_OWN( IGRPH, IGRPS, STATUS )
                  IF( IGRPS .NE. GRP__NOID ) THEN

*  Get the NDF section from this group.
                     CALL GRP_GET( IGRPS, I, 1, FIELDS( 1 ), STATUS )

                  END IF
               END IF
            END IF
         END IF
      END IF

      END
