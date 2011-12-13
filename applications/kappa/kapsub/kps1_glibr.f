      SUBROUTINE KPS1_GLIBR( LBND1, LBND2, UBND1, UBND2, DIN, IPPIX,
     :                       NPOS, STATUS )
*+
*  Name:
*     KPS1_GLIBR

*  Purpose:
*     Get pixel positions to be de-glitched from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GLIBR( LBND1, LBND2, UBND1, UBND2, DIN, IPPIX, NPOS, STATUS )

*  Description:
*     This routine returns an array holding the pixel co-ordinates at the
*     centre of all the bad pixels in the supplied array.

*  Arguments:
*     LBND1 = INTEGER (Given)
*        Lower pixel index on axis 1.
*     LBND2 = INTEGER (Given)
*        Lower pixel index on axis 2.
*     UBND1 = INTEGER (Given)
*        Upper pixel index on axis 1.
*     UBND2 = INTEGER (Given)
*        Upper pixel index on axis 2.
*     DIN( LBND1:UBND1, LBND2:UBND2 ) = REAL (Given)
*        The input data array.
*     IPPIX = INTEGER (Returned)
*        A pointer to a double precision array with dimensions (NPOS,2),
*        containing the pixel co-ordinates at the centre of every bad pixel.
*        This should be freed with PSX_FREE when no longer needed.
*     NPOS = INTEGER (Returned)
*        The number of returned positions. Returned equal to zero if
*        there are no bad pixels in the array,
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Copyright:
*     Copyright (C) 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-MAR-2000 (DSB):
*        Initial version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE parameters
      INCLUDE 'PRM_PAR'        ! VAL__ constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Arguments Given:
      INTEGER LBND1
      INTEGER LBND2
      INTEGER UBND1
      INTEGER UBND2
      REAL DIN( LBND1:UBND1, LBND2:UBND2 )

*  Arguments Returned:
      INTEGER IPPIX
      INTEGER NPOS

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I,J
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Count the number of bad pixels.
      NPOS = 0
      DO J = LBND2, UBND2
         DO I = LBND1, UBND1
            IF( DIN( I, J ) .EQ. VAL__BADR ) NPOS = NPOS + 1
         END DO
      END DO

*  If there are no bad pixels, return a pointer value of zero.
      IF( NPOS .EQ. 0 ) THEN
         IPPIX = 0

*  Otherwise, allocate the memory.
      ELSE
         CALL PSX_CALLOC( NPOS*2, '_DOUBLE', IPPIX, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Store the pixel positions in the memory.
            CALL KPS1_GLIDR( LBND1, LBND2, UBND1, UBND2, DIN,
     :                       NPOS, %VAL( CNF_PVAL( IPPIX ) ), STATUS )

         END IF

      END IF

      END
