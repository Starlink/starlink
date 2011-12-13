      SUBROUTINE FLAT( ISTAT )
*+
* Name:
*    FLAT

*  Purpose:
*     Creates a false flatfield.

*  Description:
*     This is a demonstration routine for IMG. It creates a new image
*     and fills it with ones.

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

*-
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL
*.

*  Create a new image.
      CALL IMG_NEW( 'OUT', 416, 578, IP, ISTAT )

*  Set all its elements to the value 1.0.
      CALL DOFILL( %VAL( CNF_PVAL( IP ) ), 416, 578, ISTAT )

*  Free the new image.
      CALL IMG_FREE( 'OUT', ISTAT )

      END

      SUBROUTINE DOFILL( IMAGE, NX, NY, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Loop over all elements of the image setting them to 1.0.
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            IMAGE( I, J ) = 1.0
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
