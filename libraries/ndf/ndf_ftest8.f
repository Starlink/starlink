      PROGRAM NDF_TEST8
*+
*  Name:
*     NDF_TEST8 (Fortran version)

*  Purpose:
*     Test installation of NDF from Fortran using INTEGER*8 dimensions.

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
*     DSB: David S Berry (EAO)

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory

*  History:
*     18-SEP-2019 (DSB):
*        Original version, derived from ndf_ftest.f
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
      INTEGER*8 DIM( 2 )         ! NDF dimensions
      INTEGER*8 EL               ! Number of mapped elements
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
      CALL NDF_NEWP8( '_INTEGER', 2, DIM, PLACE, INDF, STATUS )

*  Map the NDF's data array.
      CALL NDF_MAP8( INDF, 'Data', '_REAL', 'WRITE', PNTR, EL, STATUS )

*  Initialise the array.
      CALL SETUP( EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  Clean up.
      CALL NDF_ANNUL( INDF, STATUS )

*  Re-open the NDF.
      CALL NDF_OPEN( DAT__ROOT, 'ndf_test', 'UPDATE', 'OLD', INDF,
     :               PLACE, STATUS )

*  Map its data array.
      CALL NDF_MAP8( INDF, 'Data', '_INTEGER', 'READ', PNTR, EL,
     :               STATUS )

*  Sum the data elements.
      CALL SUM( EL, %VAL( CNF_PVAL( PNTR ) ), ISUM, STATUS )

*  Clean up, deleting the NDF.
      CALL NDF_DELET( INDF, STATUS )

*  Check if the test ran OK. If so, then report success.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( ISUM .EQ. 20100 ) ) THEN
         WRITE( *, * ) '*********************************************'
         WRITE( *, * ) '*                                           *'
         WRITE( *, * ) '*  NDF Fortran (INTEGER*8) test succeeded   *'
         WRITE( *, * ) '*                                           *'
         WRITE( *, * ) '*********************************************'

*  Otherwise, report an error.
      ELSE
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'NDF_TEST_ERR',
     :   'NDF_TEST_F: NDF Fortran (INTEGER*8) test failed.', STATUS )
      END IF

      END

      SUBROUTINE SETUP( EL, ARRAY, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER*8 EL

*  Arguments Returned:
      REAL ARRAY( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the array.
      DO 1 I = 1, EL
         ARRAY( I ) = REAL( I )
 1    CONTINUE

      END

      SUBROUTINE SUM( EL, ARRAY, ISUM, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER*8 EL
      INTEGER ARRAY( * )

*  Arguments Returned:
      INTEGER ISUM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop counter

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
