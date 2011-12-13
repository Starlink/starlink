      SUBROUTINE NDG_CREA1( PARAM, FTYPE, NDIM, LBND, UBND, INDF,
     :                      NAME, STATUS )
*+
*  Name:
*     NDG_CREA1

*  Purpose:
*     Create a single new simple NDF using a specified parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_CREA1( PARAM, FTYPE, NDIM, LBND, UBND, INDF, NAME,
*                     STATUS )

*  Description:
*     This routine is equivalent to NDF_CREAT except that it allows the
*     NDF to be specified using a GRP group expression (for instance, its
*     name may be given within a text file, etc). The first NDF in the
*     group expression is returned. Any other names in the group
*     expression are ignored. Any modification elements in the supplied
*     group expression will be treated literally.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     FTYPE = CHARACTER * ( * ) (Given)
*        Full data type of the NDF's DATA component (e.g. '_DOUBLE' or
*        'COMPLEX_REAL').
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower pixel-index bounds of the NDF.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds of the NDF.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     NAME = CHARACTER * ( * ) (Returned)
*        The full file specification for the NDF.
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
*     24-AUG-1999 (DSB):
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
      CHARACTER FTYPE*(*)
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER INDF
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGRP               ! Group holding supplied NDF names
      INTEGER SIZE               ! No. of supplied NDF names
      LOGICAL FLAG               ! User wants to supply more NDFs?
*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

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

*  Get an NDF identifier for the first NDF in the group.
      CALL NDG_NDFCR( IGRP, 1, FTYPE, NDIM, LBND, UBND, INDF, STATUS )

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, annul the NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_ANNUL( INDF, STATUS )

      END
