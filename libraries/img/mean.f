      SUBROUTINE MEAN( ISTAT )
*+
* Name:
*    MEAN

*  Purpose:
*     Calculates and reports the mean value of an image.

*  Description:
*     This is a demonstration routine for IMG. It accesses an existing
*     image and calculates the mean value which it then writes to the
*     terminal.

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


*  Access an input image.
      CALL IMG_IN( 'IN', NX, NY, IP, ISTAT )

*  Derive the mean and write it out.
      CALL DOSTAT( %VAL( CNF_PVAL( IP ) ), NX, NY, ISTAT )

*  Free the input image.
      CALL IMG_FREE( 'IN', ISTAT )
      END

      SUBROUTINE DOSTAT( IMAGE, NX, NY, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Initialise the sum and loop over all elements of the image.
      SUM = 0.0
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            SUM = SUM + IMAGE( I, J )
 2       CONTINUE
 1    CONTINUE

*  Write out the mean value.
      WRITE( *, * ) 'Mean = ', SUM / REAL( NX * NY )

      END
* $Id$
