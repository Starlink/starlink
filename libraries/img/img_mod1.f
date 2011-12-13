      SUBROUTINE IMG_MOD1( PARAM, NX, IP, STATUS )
*+
*  Name:
*     IMG_MOD1

*  Purpose:
*     Accesses a 1-dimensional image for modification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_MOD1( PARAM, NX, IP, STATUS )

*  Description:
*     This routine provides access to a 1-dimensional input image. It
*     returns the size of the image and a pointer to its data, mapped
*     as floating point (REAL) values. Any existing values in the
*     input image may be modified.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     NX = INTEGER (Returned)
*        Size of the image (in pixels).
*     IP = INTEGER (Returned)
*        Pointer to the image data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Access to multiple image data can also be provided by this
*     routine. Multiple parameter names are specified by supplying a
*     comma separated list of names (i.e. 'SPEC,ARC'). A pointer to the
*     data of each image is then returned (in this case the IP argument
*     should be passed as an array of size at least the number of
*     parameter names). The advantage of obtaining a sequence of
*     images in this manner is that the images are guaranteed to have
*     the same size (in pixels) and the same type (REAL).

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
*        Original version. Based on IMG_IN1.
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
      INTEGER IP( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_OK
      LOGICAL IMG1_OK            ! Test if error status is OK

*  Local Constants:
      INTEGER MXDIM              ! Maximum number of NDF dimensions
      PARAMETER ( MXDIM = 1 )

*  Local Variables:
      INTEGER DIM( MXDIM )       ! NDF dimension sizes

*.

*  Set initial null values for the returned pointer and dimension
*  size.
      IP( 1 ) = IMG__NOPTR
      NX = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain access to the NDF.
      CALL IMG1_GTNDF( PARAM, '_REAL', .FALSE., MXDIM, DIM, IP, STATUS )

*  Return its dimension size (this is the same for all images).
      NX = DIM( 1 )

*  If an error occurred, then report a contextual message.
      IF ( .NOT. IMG1_OK( STATUS ) ) THEN
         IF ( INDEX( PARAM, ',' ) .NE. 0 ) THEN
            CALL ERR_REP( 'IMG_MOD1S_ERR',
     :           'IMG_MOD1: Error obtaining update access to '//
     :           '1-dimensional input images.', STATUS )
         ELSE
            CALL ERR_REP( 'IMG_MOD1_ERR',
     :           'IMG_IN1: Error obtaining update access to a ' //
     :           '1-dimensional input image.', STATUS )
         END IF
      END IF
      END
* $Id$
