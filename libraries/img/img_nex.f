      SUBROUTINE IMG_NEX( PARAM, N, STATUS )
*+
* Name:
*    IMG_NEX

*  Purpose:
*    Returns the number of extensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_NEX( PARAM, N, STATUS )

*  Description:
*     This routine determines the number of extensions in an NDF.  The
*     number returned can be used as an upper limit when indexing using
*     IMG_REXN.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     N = INTEGER (Returned)
*        The number of extensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ parameters

*  Global variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER N

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER SLOT               ! Parameter slot number
      LOGICAL WASNEW             ! True if parameter is new
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter name and obtain its slot number.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the number of known extensions from NDF_.
         CALL NDF_XNUMB( PCB_INDF( SLOT ), N, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed in attempt to access the extension, add information to any
*  error messages.
            CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
            CALL ERR_REP( 'IMG_NEX_ERR', 'Failed to determine the ' //
     :           'number of extensions in the NDF ^NDF.', STATUS )
         END IF
      END IF
      END
* $Id$
