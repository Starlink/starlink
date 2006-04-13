      SUBROUTINE ARY1_EQBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2,
     :                       EQUAL, STATUS )
*+
*  Name:
*     ARY1_EQBND

*  Purpose:
*     Test if two arrays have equal bounds.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_EQBND( NDIM1, LBND1, UBND1, NDIM2, LBND2, UBND2, EQUAL,
*     STATUS )

*  Description:
*     The routine tests if the lower and upper bounds of two arrays are
*     equal. If the arrays being compared have different
*     dimensionalities, then the bounds information for the one with
*     lower dimensionality is padded out with 1's before making the
*     comparison. The bounds information supplied is not checked for
*     validity.

*  Arguments:
*     NDIM1 = INTEGER (Given)
*        Number of dimensions for first array.
*     LBND1( NDIM1 ) = INTEGER (Given)
*        Lower bounds information for first array.
*     UBND1( NDIM1 ) = INTEGER (Given)
*        Upper bounds information for first array.
*     NDIM2 = INTEGER (Given)
*        Number of dimensions for second array.
*     LBND2( NDIM2 ) = INTEGER (Given)
*        Lower bounds information for second array.
*     UBND2( NDIM2 ) = INTEGER (Given)
*        Upper bounds information for second array.
*     EQUAL = LOGICAL (Returned)
*        Whether the array bounds are equal.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise EQUAL to .TRUE..
*     -  Test each relevant dimension in turn, returning with EQUAL set
*     .FALSE. if a comparison between the array bounds for that
*     dimension indicates inequality.
*     -  For each comparison, use the array bound supplied, unless the
*     array has fewer dimensions than that being tested. In the latter
*     case, substitute 1's for both array bounds.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-JUN-1989  (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER NDIM1
      INTEGER LBND1( NDIM1 )
      INTEGER UBND1( NDIM1 )
      INTEGER NDIM2
      INTEGER LBND2( NDIM2 )
      INTEGER UBND2( NDIM2 )

*  Arguments Returned:
      LOGICAL EQUAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER L1                 ! Lower bound of first array
      INTEGER L2                 ! Lower bound of second array
      INTEGER U1                 ! Upper bound of first array
      INTEGER U2                 ! Upper bound of second array

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      EQUAL = .TRUE.

*  Loop to test each relevant dimension.
      DO 1 I = 1, MAX( NDIM1, NDIM2 )

*  Obtain values for bounds of first array.
         IF ( I .LE. NDIM1 ) THEN
            L1 = LBND1( I )
            U1 = UBND1( I )
         ELSE
            L1 = 1
            U1 = 1
         END IF

*  Obtain values for bounds of second array.
         IF ( I .LE. NDIM2 ) THEN
            L2 = LBND2( I )
            U2 = UBND2( I )
         ELSE
            L2 = 1
            U2 = 1
         END IF

*  Test bounds for equality, quitting the loop with EQUAL set to
*  .FALSE. if necessary.
         IF ( ( L1 .NE. L2 ) .OR. ( U1 .NE. U2 ) ) THEN
            EQUAL = .FALSE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_EQBND', STATUS )

      END
