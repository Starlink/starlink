      SUBROUTINE NDG_COPY( IGRP1, INDXLO, INDXHI, REJECT, IGRP2,
     :                     STATUS )
*+
*  Name:
*     NDG_COPY

*  Purpose:
*     Copy a section of an existing NDG group to a new group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_COPY( IGRP, INDXLO, INDXHI, REJECT, IGRP2, STATUS )

*  Description:
*     This routine extends the functionality of GRP_COPY when copying
*     elements from a group created by NDG. Such groups have
*     "supplemental information" associated with them that holds further
*     information about each NDF in the group. This function ensures that
*     the returned group also has such supplemental information.
*
*     NDG's supplemental information is stored in a chain of "slave
*     groups" that are attached to each other using the facilities of
*     GRP (e.g. see GRP_SOWN). The supplied group is the lowest level
*     "slave" in this chain.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the input group.
*     INDXLO = INTEGER (Given)
*        The lowest index to reject or to copy.
*     INDXHI = INTEGER (Given)
*        The highest index to reject or to copy.
*     REJECT = LOGICAL ( Given)
*        If reject is .TRUE., then names in the given range are
*        rejected.  Otherwise, names in the given range are copied.
*     IGRP2 = INTEGER (Returned)
*        A GRP identifier for the created group. Returned equal to
*        GRP__NOID if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP1
      INTEGER INDXLO
      INTEGER INDXHI
      LOGICAL REJECT

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRPB              ! Input base name fields group
      INTEGER IGRPB2             ! Output base name fields group
      INTEGER IGRPD              ! Input directory fields group
      INTEGER IGRPD2             ! Output directory fields group
      INTEGER IGRPH              ! Input HDS path fields group
      INTEGER IGRPH2             ! Output HDS path fields group
      INTEGER IGRPS              ! Input NDF slice fields group
      INTEGER IGRPS2             ! Output NDF slice fields group
      INTEGER IGRPT              ! Input file type fields group
      INTEGER IGRPT2             ! Output file type fields group
*.

*  Initialise the returned identifier.
      IGRP2 = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The supplied group holds the full file spec for each NDF. Copy the
*  required elements into a new group. This is the group that is returned
*  to the caller.
      CALL GRP_COPY( IGRP1, INDXLO, INDXHI, REJECT, IGRP2, STATUS )

*  Get the owner of the supplied group.
      CALL GRP_OWN( IGRP1, IGRPD, STATUS )
      IF( IGRPD .NE. GRP__NOID ) THEN

*  Copy the directory paths from this group into a new group.
         CALL GRP_COPY( IGRPD, INDXLO, INDXHI, REJECT, IGRPD2, STATUS )

*  Establish the returned group as the slave of the new group.
         CALL GRP_SOWN( IGRP2, IGRPD2, STATUS )

*  Get the owner of the input directory path group.
         CALL GRP_OWN( IGRPD, IGRPB, STATUS )
         IF( IGRPB .NE. GRP__NOID ) THEN

*  Copy the base file name from this group into a new group.
            CALL GRP_COPY( IGRPB, INDXLO, INDXHI, REJECT, IGRPB2,
     :                     STATUS )

*  Establish the directory paths group as the slave of the new group.
            CALL GRP_SOWN( IGRPD2, IGRPB2, STATUS )

*  Get the owner of the input base file name group.
            CALL GRP_OWN( IGRPB, IGRPT, STATUS )
            IF( IGRPT .NE. GRP__NOID ) THEN

*  Copy the file type from this group into a new group.
               CALL GRP_COPY( IGRPT, INDXLO, INDXHI, REJECT, IGRPT2,
     :                        STATUS )

*  Establish the file base name group as the slave of the new group.
               CALL GRP_SOWN( IGRPB2, IGRPT2, STATUS )

*  Get the owner of the input file type group.
               CALL GRP_OWN( IGRPT, IGRPH, STATUS )
               IF( IGRPH .NE. GRP__NOID ) THEN

*  Copy the HDS path from this group into a new group.
                  CALL GRP_COPY( IGRPH, INDXLO, INDXHI, REJECT, IGRPH2,
     :                           STATUS )

*  Establish the file type group as the slave of the new group.
                  CALL GRP_SOWN( IGRPT2, IGRPH2, STATUS )

*  Get the owner of the input HDS path group.
                  CALL GRP_OWN( IGRPH, IGRPS, STATUS )
                  IF( IGRPS .NE. GRP__NOID ) THEN

*  Copy the NDF section from this group into a new group.
                     CALL GRP_COPY( IGRPS, INDXLO, INDXHI, REJECT,
     :                              IGRPS2, STATUS )

*  Establish the HDS path group as the slave of the new group.
                     CALL GRP_SOWN( IGRPH2, IGRPS2, STATUS )

                  END IF
               END IF
            END IF
         END IF
      END IF

      END
