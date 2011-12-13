      SUBROUTINE IMG_MOD3( PARAM, NX, NY, NZ, IP, STATUS )
*+
*  Name:
*     IMG_MOD3

*  Purpose:
*     Accesses a 3-dimensional image for modification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_MOD3( PARAM, NX, NY, NZ, IP, STATUS )

*  Description:
*     This routine provides access to a 3-dimensional input image. It
*     returns the size of the image and a pointer to its data, mapped
*     as floating point (REAL) values. Existing values in the input
*     image may be modified.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     NX = INTEGER (Returned)
*        Size of the first dimension of the image (in pixels).
*     NY = INTEGER (Returned)
*        Size of the second dimension of the image (in pixels).
*     NZ = INTEGER (Returned)
*        Size of the third dimension of the image (in pixels).
*     IP = INTEGER (Returned)
*        Pointer to the image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Access to multiple image data can also be provided by this
*     routine. Multiple parameter names are specified by supplying a
*     comma separated list of names (i.e. 'DATA1,DATA2,DATA3'). A
*     pointer to the data of each image is then returned (in this case
*     the IP argument should be passed as an array of size at least the
*     number of parameter names). The advantage of obtaining a
*     sequence of images in this manner is that the images are
*     guaranteed to have the same shape (NX, NY and NZ) and the same
*     type (REAL).

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     29-NOV-1995 (PDRAPER):
*        Original version.
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

*  Arguments Returned:
      INTEGER NX
      INTEGER NY
      INTEGER NZ
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_OK
      LOGICAL IMG1_OK            ! Test if error status is OK

*  Local Constants:
      INTEGER MXDIM              ! Maximum number of NDF dimensions
      PARAMETER ( MXDIM = 3 )

*  Local Variables:
      INTEGER DIM( MXDIM )       ! NDF dimension sizes

*.

*  Set initial null values for the first returned pointer and the
*  dimension sizes.
      IP( 1 ) = IMG__NOPTR
      NX = 1
      NY = 1
      NZ = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain access to the NDF.
      CALL IMG1_GTNDF( PARAM, '_REAL', .FALSE., MXDIM, DIM, IP, STATUS )

*  Return its dimension sizes (these are the same for all images).
      NX = DIM( 1 )
      NY = DIM( 2 )
      NZ = DIM( 3 )

*  If an error occurred, then report a contextual message.
      IF ( .NOT. IMG1_OK( STATUS ) ) THEN
         IF ( INDEX( PARAM, ',' ) .NE. 0 ) THEN
            CALL ERR_REP( 'IMG_MOD3_ERR',
     :           'IMG_MOD3: Error obtaining update access to ' //
     :           '3-dimensional input images.', STATUS )
         ELSE
            CALL ERR_REP( 'IMG_MOD3_ERR',
     :           'IMG_MOD3: Error obtaining update access to a ' //
     :           '3-dimensional input image.', STATUS )
         END IF
      END IF

      END
* $Id$
