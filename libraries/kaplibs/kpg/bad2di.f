      SUBROUTINE BAD2DI( DIM1, DIM2, LBND, UBND, ARRAY, STATUS )
*+
*  Name:
*     BAD2DI
 
*  Purpose:
*     Substitutes the bad value in a defined sub-section of a 2-d array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL BAD2DI( DIM1, DIM2, LBND, UBND, ARRAY, STATUS )
 
*  Description:
*     {routine_description}
 
*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the array to be edited.
*     DIM2 = INTEGER (Given)
*        The second dimension of the array to be edited.
*     LBND( 2 ) = INTEGER (Given)
*        Lower bounds of the region to be made bad.
*     UBND( 2 ) = INTEGER (Given)
*        Upper bounds of the region to be made bad.
*     ARRAY( DIM1, DIM2 ) = _INTEGER (Given and Returned)
*        The array to be edited.
*     STATUS = INTEGER (Given)
*        The global status.
 
*  Algorithm:
*     -  Check that the bounds are reasonable, if not report an error
*        and return.
*     -  Substitute bad pixels into the sub-region.
 
*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1990 April 7 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value constants
 
*  Arguments Given:
      INTEGER
     :  DIM1,
     :  DIM2,
     :  LBND( 2 ),
     :  UBND( 2 )
 
*  Arguments Given and Returned:
      INTEGER
     :  ARRAY( DIM1, DIM2 )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER
     :  I, J                     ! Loop counters
 
*.
 
*    Check inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      IF ( LBND( 1 ) .LT. 1 .OR. UBND( 1 ) .GT. DIM1 .OR.
     :     LBND( 1 ) .GT. UBND( 1 ) .OR. LBND( 2 ) .GT. UBND( 2 ) .OR.
     :     LBND( 2 ) .LT. 1 .OR. UBND( 2 ) .GT. DIM2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BAD2D_BNDS',
     :     'BAD2D: Bounds error---probable programming error.', STATUS )
 
      ELSE
 
*       Loop for all pixels in the sub-array selected.
 
         DO  J = LBND( 2 ), UBND( 2 )
            DO  I = LBND( 1 ), UBND( 1 )
 
*              Substitute the bad value.
 
                ARRAY( I, J ) = VAL__BADI
            END DO
         END DO
      END IF
 
      END
