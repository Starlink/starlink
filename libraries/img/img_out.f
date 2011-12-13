      SUBROUTINE IMG_OUT( PARAM1, PARAM2, IP, STATUS )
*+
*  Name:
*     IMG_OUT

*  Purpose:
*     Creates an output image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_OUT( PARAM1, PARAM2, IP, STATUS )

*  Description:
*     This routine creates a new output image by duplicating an input
*     image.  A pointer is returned to the output image data, mapped as
*     floating point (REAL) values.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        Parameter name for the input image (case insensitive).
*     PARAM2 = CHARACTER * ( * ) (Given)
*        Parameter name for the new output image (case insensitive).
*     IP = INTEGER (Returned)
*        Pointer to the mapped output data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Access to multiple output images can also be provided by this
*     routine. Multiple parameter names are specified by supplying a
*     comma separated list of names (i.e. 'OUT1,OUT2'). A pointer to the
*     data of each image is then returned (in this case the IP argument
*     must be passed as an array of size at least the number of
*     parameter names). The advantage of using this method is that
*     multiple copies of an input image can be made using a single
*     invocation of this routine. Multiple input image names are not
*     allowed.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     18-FEB-1992 (RFWS):
*        Original version.
*     18-AUG-1994 (PDRAPER):
*        Extended to use multiple parameter names for the output NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM1
      CHARACTER * ( * ) PARAM2

*  Arguments Returned:
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_OK
      LOGICAL IMG1_OK            ! Test if error status is OK

*.

*  Set an initial null value for the first IP argument.
      IP( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Propagate the input NDF to the output(s).
      CALL IMG1_PRNDF( PARAM1, PARAM2, '_REAL', IP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( .NOT. IMG1_OK( STATUS ) ) THEN
         IF ( INDEX( PARAM2, ',' ) .NE. 0 ) THEN
            CALL ERR_REP( 'IMG_OUT_ERR',
     :           'IMG_OUT: Error creating output images by ' //
     :           'duplicating an input image.', STATUS )
         ELSE
            CALL ERR_REP( 'IMG_OUT_ERR',
     :           'IMG_OUT: Error creating an output image by ' //
     :           'duplicating an input image.', STATUS )
         END IF
      END IF

      END
* $Id$
