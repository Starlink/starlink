      PROGRAM NDF_TEST
*+
*  Name:
*     NDF_TEST (Fortran version)

*  Purpose:
*     Test installation of NDF From Fortran.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program should be run after building and installing NDF in
*     order to test for correct installation. Note that this is not an
*     exhaustive test of NDF, but only of its installation.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  History:
*     16-JAN-1992 (RFWS):
*        Original version, derived from the equivalent HDS routine.
*     7-SEP-1993 (RFWS):
*        Remove calls to HDS_START and HDS_STOP (no longer needed) and
*        use DAT_ANNUL instead of HDS_CLOSE.
*     8-SEP-1993 (RFWS):
*        Changed to use NDF_OPEN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 2 )           ! NDF dimensions
      INTEGER EL                 ! Number of mapped elements
      INTEGER INDF               ! NDF identifier
      INTEGER ISUM               ! Sum of array elements
      INTEGER PLACE              ! NDF placeholder
      INTEGER PNTR               ! Pointer to mapped array

*  External References:
      INCLUDE 'NDF_FUNC'         ! NDF statement functions
                                 ! (test existence)

*  Local Data:
      DATA DIM / 10, 20 /

*.

*  Initialise the global status.
      STATUS = SAI__OK

*  Create a new file containing an NDF.
      CALL NDF_OPEN( DAT__ROOT, 'ndf_test', 'WRITE', 'NEW', INDF, PLACE,
     :               STATUS )
      CALL NDF_NEWP( '_INTEGER', 2, DIM, PLACE, INDF, STATUS )

*  Map the NDF's data array.
      CALL NDF_MAP( INDF, 'Data', '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Initialise the array.
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  Clean up.
      CALL NDF_ANNUL( INDF, STATUS )

*  Re-open the NDF.
      CALL NDF_OPEN( DAT__ROOT, 'ndf_test', 'UPDATE', 'OLD', INDF,
     :               PLACE, STATUS )

*  Map its data array.
      CALL NDF_MAP( INDF, 'Data', '_INTEGER', 'READ', PNTR, EL, STATUS )

*  Sum the data elements.
      CALL SUM( EL, %VAL( CNF_PVAL( PNTR ) ), ISUM, STATUS )

*  Clean up, deleting the NDF.
      CALL NDF_DELET( INDF, STATUS )

*  Check if the test ran OK. If so, then report success.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( ISUM .EQ. 20100 ) ) THEN
         WRITE( *, * ) '*********************************************'
         WRITE( *, * ) '*                                           *'
         WRITE( *, * ) '*  NDF Fortran installation test succeeded  *'
         WRITE( *, * ) '*                                           *'
         WRITE( *, * ) '*********************************************'

*  Otherwise, report an error.
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'NDF_TEST_ERR',
     :   'NDF_TEST_F: NDF Fortran installation test failed.', STATUS )
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
