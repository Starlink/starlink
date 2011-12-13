      SUBROUTINE IMG_MOD( PARAM, NX, NY, IP, STATUS )
*+
*  Name:
*     IMG_MOD

*  Purpose:
*     Accesses an image for modification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_MOD( PARAM, NX, NY, IP, STATUS )

*  Description:
*     This routine provides access to a 2-dimensional input image. It
*     returns the size of the image and pointer to its data, mapped
*     as floating point (REAL) values. Existing values in the image
*     may be modified.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name. (case insensitive).
*     NX = INTEGER (Returned)
*        Size of first dimension of the image (in pixels).
*     NY = INTEGER (Returned)
*        Size of second dimension of the image (in pixels).
*     IP = INTEGER (Returned)
*        Pointer to image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Access to multiple image data can also be provided by this
*     routine. Multiple parameter names are specified by supplying a
*     comma separated list of names (i.e. 'DATA,BIAS,FLAT'). A pointer
*     to the data of each image is then returned (in this case the IP
*     argument should be passed as an array of size at least the number
*     of parameter names). The advantage of obtaining a sequence of
*     images in this manner is that the images are guaranteed to have
*     the same shape (NX and NY) and the same type (REAL).
*
*     - The message which complains when "bad" (undefined) pixels are
*     present in the input data can be stopped by following each
*     parameter name by the character "!", i.e. 'DATA!,BIAS!,FLAT' will
*     inhibit checking the images associated with parameters 'DATA' and
*     'BIAS', but will check the image 'FLAT'. "Bad" pixels have the
*     symbolic value 'VAL__BADR' which is defined in the include file
*     'PRM_PAR'.

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
*        Original version. Based on IMG_IN.
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
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_OK
      LOGICAL IMG1_OK            ! Test if error status is OK

*  Local Constants:
      INTEGER MXDIM              ! Maximum number of NDF dimensions
      PARAMETER ( MXDIM = 2 )

*  Local Variables:
      INTEGER DIM( MXDIM )       ! NDF dimension sizes

*.

*  Set initial null values for the first pointer and the dimension
*  sizes.
      IP( 1 ) = IMG__NOPTR
      NX = 1
      NY = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain access to the NDF.
      CALL IMG1_GTNDF( PARAM, '_REAL', .FALSE., MXDIM, DIM, IP, STATUS )

*  Return its dimension sizes (these should all be the same for multiple
*  NDFs).
      NX = DIM( 1 )
      NY = DIM( 2 )

*  If an error occurred, then report a contextual message.
      IF ( .NOT. IMG1_OK( STATUS ) ) THEN
         IF ( INDEX ( PARAM, ',' ) .NE. 0 ) THEN
            CALL ERR_REP( 'IMG_MODS_ERR', 'IMG_MOD: Error obtaining'//
     :                    ' update access to input images.', STATUS )
         ELSE
            CALL ERR_REP( 'IMG_MOD_ERR', 'IMG_MOD: Error obtaining'//
     :                    ' update access to an input image.', STATUS )
         END IF
      END IF

      END
* $Id$
