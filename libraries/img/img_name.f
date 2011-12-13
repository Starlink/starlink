      SUBROUTINE IMG_NAME( PARAM, VALUE, STATUS )
*+
*  Name:
*     IMG_NAME

*  Purpose:
*     Return the image name.

*  Language:
*     Starlink Fortran-77.

*  Invocation:
*     CALL IMG_NAME( PARAM, VALUE, STATUS )

*  Description:
*     This subroutine returns the name of the input image as a character
*     string.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     VALUE = CHARACTER * ( * ) (Returned)
*        The name of the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The character string VALUE should be made large enough to
*       contain the full name of the image.
*
*     - This subroutine will directly access an image if an association
*       has not already been made. Note that it will be opened for
*       read-only access and you will not be able to write or delete any
*       header items. If you need to be able to do this then access the
*       image first, either with one of the IMG\_MOD[n][x] subroutines (if
*       you intend to process the image data), or the HDR\_MOD subroutine.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1996 (PDRAPER):
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
      INCLUDE 'NDF_PAR'          ! NDF_ parameters
      INCLUDE 'IMG_PCB'          ! IMG_ parameter control block
      INCLUDE 'IMG_ERR'          ! IMG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER SLOT              ! Parameter slot number
      INTEGER STRLEN            ! Length of name string
      LOGICAL WASNEW            ! NDF has never been accesed
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter and its slot number.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency reasons.
         IF ( WASNEW ) CALL IMG1_ASSOC( VPAR, 'READ', SLOT, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Clear the name string.
            VALUE = ' '

*  And load the value from the message system and NDF.
           CALL NDF_MSG( 'NAME', PCB_INDF( SLOT ) )
           CALL MSG_LOAD( 'NAME', '^NAME', VALUE, STRLEN, STATUS )
         END IF
      ELSE

*  Not a valid parameter name.
         STATUS = IMG__PARIN
         CALL ERR_REP( 'IMG_NAME_NOPAR',
     :        'No valid parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END

*  $Id$
