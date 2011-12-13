      SUBROUTINE ADD( ISTAT )
*+
* Name:
*    ADD

*  Purpose:
*     Adds a constant value to all the elements of an image.

*  Description:
*     This is a demonstration routine for IMG. It creates a copy of an
*     existing image and then adds a specified constant to all the
*     image elements.

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     03-JUN-1998 (PWD):
*         Original Version
*     16-AUG-2004 (TIMJ):
*         Use CNF_PVAL

*  Notes:
*     This routine could also be implemented to just modify the input
*     image, rather than creating a new copy.
*
*     The PAR routines are described in SUN/114.

*-
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL

*  Access an existing image.
      CALL IMG_IN( 'IN', NX, NY, IPIN, ISTAT )

*  Copy this to an output image.
      CALL IMG_OUT( 'IN', 'OUT', IPOUT, ISTAT )

*  Get the value to add.
      CALL PAR_GET0R( 'CONSTANT', VALUE, ISTAT )

*  And do the work.
      CALL DOADD( %VAL( CNF_PVAL( IPOUT ) ), NX, NY, VALUE, ISTAT )

*  Free the input and output images.
      CALL IMG_FREE( '*', ISTAT )
      END

      SUBROUTINE DOADD( IMAGE, NX, NY, VALUE, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Loop over all elements of the image adding VALUE.
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            IMAGE( I, J ) = IMAGE( I, J ) + VALUE
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
