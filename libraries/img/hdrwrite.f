      SUBROUTINE HDRWRITE( ISTAT )
*+
* Name:
*    HDRWRITE

*  Purpose:
*     Writes a new header item.

*  Description:
*     This routine is part of the IMG example suite. It accesses an
*     existing image and creates a copy as an output image. It then
*     writes a new header item to the output image.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     PWD: Peter Draper (Starlink)

*  History:
*     03-JUN-1998 (PWD):
*         Original Version

*  Notes:
*     This could be simplified by just modifying the input image rather
*     than creating a copy. To do this just comment out the IMG_IN and
*     IMG_OUT lines. You might also want to change the HDR_OUT line to
*     access the image via parameter 'IN'.
*
*     The PAR routines are described in SUN/114.

*-

*  Local Variables:
      CHARACTER * ( 8 ) ITEM
      CHARACTER * ( 30 ) COMMEN, VALUE
*.

*  Access the input image.
      CALL IMG_IN( 'IN', NX, NY, IPIN, ISTAT )

*  Copy this to an output image.
      CALL IMG_OUT( 'IN', 'OUT', IPOUT, ISTAT )

*  Now get the name of the header item.
      CALL PAR_GET0C( 'ITEM', ITEM, ISTAT )

*  And its value.
      CALL PAR_GET0C( 'VALUE', VALUE, ISTAT )

*  And a comment for it.
      CALL PAR_GET0C( 'COMMENT', COMMEN, ISTAT )

*  Now write it.
      CALL HDR_OUT( 'OUT', ' ', ITEM, COMMEN, VALUE, ISTAT )

*  Free all the images.
      CALL IMG_FREE( '*', ISTAT )

      END
* $Id$
