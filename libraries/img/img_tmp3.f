      SUBROUTINE IMG_TMP3( PARAM, NX, NY, NZ, IP, STATUS )
*+
*  Name:
*     IMG_TMP3

*  Purpose:
*     Creates a temporary 3-dimensional image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_TMP3( PARAM, NX, NY, NZ, IP, STATUS )

*  Description:
*     This routine creates a temporary 3-dimensional image for use as
*     workspace and returns a pointer to its data, mapped as floating
*     point (REAL) values. The image will be deleted automatically
*     when it is later released (e.g. by calling IMG_FREE).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     NX = INTEGER (Given)
*        Size of image first dimension (in pixels).
*     NY = INTEGER (Given)
*        Size of image second dimension (in pixels).
*     NZ = INTEGER (Given)
*        Size of image third dimension (in pixels).
*     IP = INTEGER (Returned)
*        Pointer to the image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Access to more than one piece of temporary image data is
*     possible using multiple parameter names. These are specified by
*     supplying a comma separated list of names
*     (i.e. 'WORK1,WORK2,WORK3'). A pointer to the data of each image
*     is then returned (in this case the IP argument should be passed
*     as an array of size at least the number of parameter names).

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
*     21-FEB-1992 (RFWS):
*        Original version.
*     18-AUG-1994 (PDRAPER):
*        Extended to use multiple parameter names.
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
      CHARACTER * ( * ) PARAM
      INTEGER NX
      INTEGER NY
      INTEGER NZ

*  Arguments Returned:
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Number of NDF dimensions
      PARAMETER ( NDIM = 3 )

*  Local Variables:
      INTEGER DIM( NDIM )        ! NDF dimension sizes

*.

*  Set an initial null value for the first returned pointer.
      IP( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the NDF dimensions.
      DIM( 1 ) = NX
      DIM( 2 ) = NY
      DIM( 3 ) = NZ

*  Create temporary NDF(s).
      CALL IMG1_TPNDF( PARAM, '_REAL', NDIM, DIM, IP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( INDEX( PARAM, ',' ) .NE. 0 ) THEN
             CALL ERR_REP( 'IMG_TMP3_ERR',
     :           'IMG_TMP3: Error creating temporary ' //
     :           '3-dimensional images.', STATUS )
        ELSE
            CALL ERR_REP( 'IMG_TMP3_ERR',
     :           'IMG_TMP3: Error creating a temporary ' //
     :           '3-dimensional image.', STATUS )
         END IF
      END IF

      END
* $Id$
