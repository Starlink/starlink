      SUBROUTINE LPG_CRPL1( PARAM, PLACE, NAME, STATUS )
*+
*  Name:
*     LPG_CRPL1

*  Purpose:
*     Create a single new NDF placeholder using a specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_CRPL1( PARAM, PLACE, NAME, STATUS )

*  Description:
*     This routine is equivalent to NDF_CREPL except that it allows the
*     NDF to be specified using a GRP group expression (for instance, its
*     name may be given within a text file, etc.). The first NDF in the
*     group expression is returned. Any other names in the group
*     expression are ignored. Any modification elements in the supplied
*     group expression will be treated literally.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     PLACE = INTEGER (Returned)
*        NDF placeholder.
*     NAME = CHARACTER * ( * ) (Returned)
*        The full file specification for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-NOV-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER PLACE
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP               ! Group holding supplied NDF names
      INTEGER JUNK               ! Unused NDF identifier
      INTEGER SIZE               ! No. of supplied NDF names
      LOGICAL FLAG               ! User wants to supply more NDFs?
*.

*  Set an initial value for the PLACE argument.
      PLACE = NDF__NOPL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group of NDFs from the environment using the supplied parameter.
*  There is no need to loop if a group expression is given which is
*  terminated by a flag character since we only want one NDF.
*  Modification elements are ignored (i.e. treated as a literal file name).
      IGRP = GRP__NOID
      CALL NDG_CREAT( PARAM, GRP__NOID, IGRP, SIZE, FLAG, STATUS )

*  Get the name of the first NDF in the group.
      CALL GRP_GET( IGRP, 1, 1, NAME, STATUS )

*  Get an NDF placeholder for the first NDF in the group.
      CALL NDG_NDFPL( IGRP, 1, PLACE, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, annul the placeholder by calling an NDF routine
*  that uses placeholder.
      IF( STATUS .NE. SAI__OK ) CALL NDF_COPY( NDF__NOID, PLACE, JUNK,
     :                                         STATUS )

      END
