      SUBROUTINE KPS1_PRFLT( AXIS, NP, NDIM, POS, MAP, STATUS )
*+
*  Name:
*     KPS1_PRFLT

*  Purpose:
*     Create an AST LutMap for a specific axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PRFLT( AXIS, NP, NDIM, POS, MAP, STATUS )

*  Description:
*     This routine creates an AST LutMap which transforms the 1-D
*     co-ordinate in the input Frame into the co-ordinate on a given axis
*     of a given output Frame. It also fills the supplied 1-D LUT with
*     the corresponding 1-D GRID co-ordinates. It is used by PROFILE.

*  Arguments:
*     AXIS = INTEGER (Given)
*        The index of the axis within the output Frame for which the LutMap
*        is required.
*     NP = INTEGER (Given)
*        The number of positions to be described in the look-up-table.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the supplied POS array.
*     POS( NP, NDIM ) = DOUBLE PRECISION (Given and Returned)
*        A set of look-up-tables, one for each of the NDIM output axes, each
*        with NP entries. On exit, the row specified by AXIS is set holding
*        values 1.0, 2.0, 3.0, ... DBLE( NP ).
*     MAP = INTEGER (Returned)
*        An AST pointer to the returned LutMap for the required axis.
*     STATUS = INTEGER (Given)
*        Global status value.

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
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER AXIS
      INTEGER NP
      INTEGER NDIM

*  Arguments Given and Returned:
      DOUBLE PRECISION POS( NP, NDIM )

*  Arguments Returned:
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      DOUBLE PRECISION VAL0      ! First LUT value
      LOGICAL EQUAL              ! All LUT values equal?
*.

*  Initialise.
      MAP = AST__NULL

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the LUT values are all equal.
      EQUAL = .TRUE.
      VAL0 = POS( 1, AXIS )

      DO I = 2, NP
         IF( POS( I, AXIS ) .NE. VAL0 ) THEN
            EQUAL = .FALSE.
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  If they are all equal return a PermMap with one input and one output
*  which performs the required mapping.
      IF( EQUAL ) THEN
         MAP = AST_PERMMAP( 1, 0, 1, -1, VAL0, ' ', STATUS )

*  If the LUT values are not all equal, create the LutMap.
      ELSE
         MAP = AST_LUTMAP( NP, POS( 1, AXIS ), 1.0D0, 1.0D0, ' ',
     :                     STATUS )
      END IF

*  Return an array holding GRID co-ordinate within the LUT.
      DO I = 1, NP
         POS( I, AXIS ) = DBLE( I )
      END DO

      END
