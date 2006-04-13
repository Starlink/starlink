      PROGRAM ARY_TEST
*+
*  Name:
*     ARY_TEST

*  Purpose:
*     Test installation of ARY.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program should be run after building and installing ARY in
*     order to test for correct installation. Note that this is not an
*     exhaustive test of ARY, but only of its installation.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     16-JAN-1992 (RFWS):
*        Original version, derived from the equivalent HDS routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Top-level HDS locator
      INTEGER DIM( 2 )           ! Array dimensions
      INTEGER EL                 ! Number of mapped elements
      INTEGER IARY               ! Array identifier
      INTEGER ISUM               ! Sum of array elements
      INTEGER PLACE              ! Array placeholder
      INTEGER PNTR               ! Pointer to mapped array

*  Local Data:
      DATA DIM / 10, 20 /

*.

*  Initialise the global status.
      STATUS = SAI__OK

*  Initialise HDS and create a new container file.
      CALL HDS_START( STATUS )
      CALL HDS_NEW( 'ary_test', 'ARY_TEST', 'NDF', 0, DIM, LOC, STATUS )

*  Create an array inside it.
      CALL ARY_PLACE( LOC, 'ARRAY', PLACE, STATUS )
      CALL ARY_NEWP( '_INTEGER', 2, DIM, PLACE, IARY, STATUS )

*  Map the array.
      CALL ARY_MAP( IARY, '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Initialise the array.
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  Clean up and close the file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_CLOSE( LOC, STATUS )

*  Re-open the file.
      CALL HDS_OPEN( 'ary_test', 'UPDATE', LOC, STATUS )

*  Find and map the array.
      CALL ARY_FIND( LOC, 'ARRAY', IARY, STATUS )
      CALL ARY_MAP( IARY, '_INTEGER', 'READ', PNTR, EL, STATUS )

*  Sum the data elements.
      CALL SUM( EL, %VAL( CNF_PVAL( PNTR ) ), ISUM, STATUS )

*  Clean up and erase the container file.
      CALL ARY_ANNUL( IARY, STATUS )
      CALL HDS_ERASE( LOC, STATUS )

*  Close down HDS.
      CALL HDS_STOP( STATUS )

*  Check if the test ran OK. If so, then report success.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( ISUM .EQ. 20100 ) ) THEN
         WRITE( *, * ) '   ARY installation test succeeded.'

*  Otherwise, report an error.
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'ARY_TEST_ERR',
     :   'ARY_TEST: ARY installation test failed.', STATUS )
      END IF

      END

      SUBROUTINE SETUP( EL, ARRAY, STATUS )
*+
*  Name:
*     SETUP

*  Purpose:
*     Initialise an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SETUP( EL, ARRAY, STATUS )

*  Description:
*     Set each element of a 1-dimensional array equal to its element
*     number.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements.
*     ARRAY( EL ) = REAL (Returned)
*        Array to be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL

*  Arguments Returned:
      REAL ARRAY( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the array.
      DO 1 I = 1, EL
         ARRAY( I ) = REAL( I )
 1    CONTINUE

      END

      SUBROUTINE SUM( EL, ARRAY, ISUM, STATUS )
*+
*  Name:
*     SUM

*  Purpose:
*     Sum the elements of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUM( EL, ARRAY, ISUM, STATUS )

*  Description:
*     Return the sum of the elements of a 1-dimensional array.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements.
*     ARRAY( EL ) = INTEGER (Given)
*        Array whose elements are to be summed.
*     ISUM = INTEGER (Returned)
*        Sum of array elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1991 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER ARRAY( * )

*  Arguments Returned:
      INTEGER ISUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      ISUM = 0

*  Sum the array elements.
      DO 1 I = 1, EL
         ISUM = ISUM + ARRAY( I )
 1    CONTINUE

      END
