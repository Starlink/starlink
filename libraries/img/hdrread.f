      SUBROUTINE HDRREAD( ISTAT )
*+
* Name:
*    HDRREAD

*  Purpose:
*     Reports the value of a header item.

*  Description:
*     This routine is part of the IMG example suite. It accesses an
*     existing image and reads the value of a named item. The value is
*     then reported.

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
*     - The headers accessed by this routine are FITS items.
*
*     - The output is written using MSG routines rather than
*       print statements. MSG is documented in SUN/104.
*
*     - The PAR routines are described in SUN/114.

*-

*  Local Variables:
      CHARACTER * ( 8 ) ITEM
      CHARACTER * ( 40 ) VALUE
*.

*  Get the name of the FITS item we are to read.
      CALL PAR_GET0C( 'ITEM', ITEM, ISTAT )

*  See if it exists (this call also accesses the image).
      CALL HDR_NUMB( 'IN', ' ', ITEM, N, ISTAT )
      IF ( N .GT. 0 ) THEN

*  Try to read the value.
         CALL HDR_IN( 'IN', ' ', ITEM, 1, VALUE, ISTAT )

*  And write it out.
         CALL MSG_SETC( 'ITEM', ITEM )
         CALL MSG_SETC( 'VALUE', VALUE )
         CALL MSG_OUT( ' ', 'The header item ''^ITEM'' has a ' //
     :        'value of ^VALUE.', ISTAT )
      ELSE

*  Item doesn't exist so make a report to this effect.
         CALL MSG_SETC( 'ITEM', ITEM )
         CALL MSG_OUT( ' ', 'The header item ''^ITEM'' doesn''t exist.',
     :                 ISTAT )
      END IF

*  Free the input image.
      CALL IMG_FREE( 'IN', ISTAT )
      END
* $Id$
