      SUBROUTINE CTG_CREA1( PARAM, CI, NAME, STATUS )
*+
*  Name:
*     CTG_CREA1

*  Purpose:
*     Create a single new catalogue using a specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG_CREA1( PARAM, CI, NAME, STATUS )

*  Description:
*     This routine is equivalent to CAT_CREAT except that it allows the
*     catalogue to be specified using a GRP group expression (for
*     instance, its name may be given within a text file, etc), and it
*     also ensures that any existing catalogue with the same name is
*     deleted before the new one is created (so long as no FITS extension
*     number is included in the catalogue specification). The first
*     catalogue in the group expression is returned. Any other names in
*     the group expression are ignored. Any modification elements in the
*     supplied group expression will be treated literally.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     CI = INTEGER (Returned)
*        Catalogue identifier.
*     NAME = CHARACTER * ( * ) (Returned)
*        The file specification for the catalogue.
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

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'CAT_PAR'          ! catalogue constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER CI
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP               ! Group holding supplied catalogue names
      INTEGER SIZE               ! No. of supplied catalogue names
      LOGICAL FLAG               ! User wants to supply more catalogues?
*.

*  Set an initial value for the CI argument.
      CI = CAT__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group of catalogues from the environment using the supplied parameter.
*  There is no need to loop if a group expression is given which is
*  terminated by a flag character since we only want one catalogue.
*  Modification elements are ignored (i.e. treated as a literal file name).
      IGRP = GRP__NOID
      CALL CTG_CREAT( PARAM, GRP__NOID, IGRP, SIZE, FLAG, STATUS )

*  Get the name of the first NDF in the group.
      CALL GRP_GET( IGRP, 1, 1, NAME, STATUS )

*  Get a CAT identifier for the first catalogue in the group.
      CALL CTG_CATCR( IGRP, 1, CI, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, annul the catalogue.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL CAT_TRLSE( CI, STATUS )
         CALL ERR_END( STATUS )
         CI = CAT__NOID
      END IF

      END
