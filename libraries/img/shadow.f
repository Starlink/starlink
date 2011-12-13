      SUBROUTINE SHADOW( ISTAT )
*+
* Name:
*    SHADOW

*  Purpose:
*    Produces a false shadowing effect in an image.

*  Description:
*     This routine is a demonstration module for IMG. It accesses an
*     input image, an output image and a temporary image as
*     workspace. The output image contains the difference between the
*     input image and a slightly shifted version.

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
*     This can be done without the requirement for workspace (with a
*     little extra effort), but then it wouldn't be an example of how
*     to get some.
*
*     The PAR routines are described in SUN/114.

*-
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL

*  Local Variables:
      INTEGER XSHIFT, YSHIFT
*.

*  Access the input image.
      CALL IMG_IN( 'IN', NX, NY, IPIN, ISTAT )

*  Copy this to an output image.
      CALL IMG_OUT( 'IN', 'OUT', IPOUT, ISTAT )

*  Get a temporary image as workspace.
      CALL IMG_TMP( 'TEMP', NX, NY, IPTEMP, ISTAT )

*  Find out how far to shift the image.
      CALL PAR_GDR0I( 'XSHIFT', 3, -NX + 1, NX, .TRUE., XSHIFT, ISTAT )
      CALL PAR_GDR0I( 'YSHIFT', 3, -NY + 1, NY, .TRUE., YSHIFT, ISTAT )

*  Shift the input data and place result in the temporary image.
      CALL DOSHFT( %VAL( CNF_PVAL( IPIN ) ), NX, NY, XSHIFT, YSHIFT,
     :             %VAL( CNF_PVAL( IPTEMP ) ), ISTAT )

*  Take the difference between the input data and the shifted data
*  putting the result in the output image.
      CALL DODIFF( %VAL( CNF_PVAL( IPIN ) ), %VAL( CNF_PVAL( IPTEMP ) ),
     :             NX, NY, %VAL( CNF_PVAL( IPOUT ) ),
     :             ISTAT )

*  Free all the images (this deletes the temporary image).
      CALL IMG_FREE( '*', ISTAT )

*  If an error occurred add the routine name.
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SHADOW_ERR',
     :      'SHADOW: failed to produce output image.', ISTAT )
      END IF
      END

      SUBROUTINE DOSHFT( ARRIN, NX, NY, XSHIFT, YSHIFT, ARROUT, ISTAT )

*+
*  Name:
*    DOSHFT

*  Purpose:
*     Shifts a data array writing result into another array.

*-
      INCLUDE 'SAE_PAR'
      INTEGER XSHIFT, YSHIFT
      REAL ARRIN( NX, NY )
      REAL ARROUT( NX, NY )

*  Local Variables:
      INTEGER XHIGH, XLOW, YHIGH, YLOW
*.

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Determine the bounds of the overlap regions.
      XLOW = MAX( 1, XSHIFT + 1 )
      YLOW = MAX( 1, YSHIFT + 1 )
      XHIGH = MIN( NX, NX + XSHIFT )
      YHIGH = MIN( NY, NY + YSHIFT )

*  Loop over all the output array initialising it to 0.0
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            ARROUT( I, J ) = 0.0
 2       CONTINUE
 1    CONTINUE

*  Now copy the data into the overlap region.
      JJ = YLOW - YSHIFT - 1
      ISTART = XLOW - XSHIFT - 1
      DO 3 J = YLOW, YHIGH
         JJ = JJ + 1
         II = ISTART
         DO 4 I = XLOW, XHIGH
            II = II + 1
            ARROUT( II, JJ ) = ARRIN( I, J )
 4       CONTINUE
 3    CONTINUE
      END

      SUBROUTINE DODIFF( ARR1, ARR2, NX, NY, ARROUT, ISTAT )

*+
* Name:
*    DODIFF

*  Purpose:
*     Forms of difference of two arrays, except when ARR2 is zero.

*-
      INCLUDE 'SAE_PAR'
      REAL ARR1( NX, NY )
      REAL ARR2( NX, NY )
      REAL ARROUT( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Form the difference.
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            IF ( ARR2( I, J ) .NE. 0.0 ) THEN
               ARROUT( I, J ) = ARR1( I, J ) - ARR2( I, J )
            ELSE
               ARROUT( I, J ) = 0.0
            END IF
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
