      SUBROUTINE CTG_PTSUP( IGRP, I, FIELDS, STATUS )
*+
*  Name:
*     CTG_PTSUP

*  Purpose:
*     Store suplemental information for an catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_PTSUP( IGRP, I, FIELDS, STATUS )

*  Description:
*     Stores the supplied items of supplemental information for a given
*     entry in a CTG group. The GRP groups needed to store this
*     supplemental information are created if they do not already exist,
*     and associated with the supplied group by means of a chain of GRP
*     "owner-slave" relationships. They will be deleted automaticaly when
*     the supplied group is deleted using GRP_DELET.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The CTG group as returned by CTG_ASSOC, etc. This should be the last
*        group in a GRP owner-slave chain.
*     I = INTEGER (Given)
*        The index of the required entry.
*     FIELDS( 5 ) = CHARACTER * ( * ) (Given)
*        The supplemental information to be stored with the entry specified
*        by I. Each element of the supplied array should contain the
*        following:
*           1 - FITS extension (e.g. "{3}") if any
*           2 - File type
*           3 - Base file name
*           4 - Directory path
*           5 - Full catalogue specification
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
      CHARACTER FIELDS( 5 )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRPB              ! Group holding base name fields
      INTEGER IGRPD              ! Group holding directory fields
      INTEGER IGRPS              ! Group holding FITS extension fields
      INTEGER IGRPT              ! Group holding file type fields
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The supplied group should be owned by a group holding the directory
*  specifications for each file. Check to see if the group exists. If
*  not, create it and establish it as the owner of the supplied group.
      CALL GRP_OWN( IGRP, IGRPD, STATUS )
      IF( IGRPD .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Directory', IGRPD, STATUS )
         CALL GRP_SOWN( IGRP, IGRPD, STATUS )
      END IF

*  The directories group is owned by a group holding the file base names.
      CALL GRP_OWN( IGRPD, IGRPB, STATUS )
      IF( IGRPB .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Base name', IGRPB, STATUS )
         CALL GRP_SOWN( IGRPD, IGRPB, STATUS )
      END IF

*  The base names group is owned by a group holding the file types.
      CALL GRP_OWN( IGRPB, IGRPT, STATUS )
      IF( IGRPT .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'File type', IGRPT, STATUS )
         CALL GRP_SOWN( IGRPB, IGRPT, STATUS )
      END IF

*  The file types group is owned by a group holding the FITS extensions.
      CALL GRP_OWN( IGRPT, IGRPS, STATUS )
      IF( IGRPS .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'FITS extension', IGRPS, STATUS )
         CALL GRP_SOWN( IGRPT, IGRPS, STATUS )
      END IF

*  Store the full file spec in the supplied group.
      CALL GRP_PUT( IGRP, 1, FIELDS( 5 ), I, STATUS )

*  Store the directory path from this group.
      CALL GRP_PUT( IGRPD, 1, FIELDS( 4 ), I, STATUS )

*  Store the base file name from this group.
      CALL GRP_PUT( IGRPB, 1, FIELDS( 3 ), I, STATUS )

*  Store the file type from this group.
      CALL GRP_PUT( IGRPT, 1, FIELDS( 2 ), I, STATUS )

*  Store the FITS extension from this group.
      CALL GRP_PUT( IGRPS, 1, FIELDS( 1 ), I, STATUS )

      END
