      SUBROUTINE ARY_VERFY( IARY, STATUS )
*+
*  Name:
*     ARY_VERFY

*  Purpose:
*     Verify that an array's data structure is correctly constructed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_VERFY( IARY, STATUS )

*  Description:
*     The routine checks that the data structure containing an array is
*     correctly constructed and that the array's pixel values are
*     defined. It also checks for the presence of any "rogue"
*     components in the data structure. If an anomaly is found, then an
*     error results. Otherwise, the routine returns without further
*     action.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the array identifier.
*     -  Obtain an index to the data object entry in the DCB and verify
*     the object's structure.
*     -  Ensure that state information is available in the DCB.
*     -  If the array's data values are undefined, then report an error.
*     -  If any error occurred, then report context information.

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
*     1-SEP-1989 (RFWS):
*        Original version.
*     18-SEP-1989 (RFWS):
*        Made minor improvements to error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Data object state.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.


*  Arguments Given:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB and verify the
*  object's structure.
         IDCB = ACB_IDCB( IACB )
         CALL ARY1_DVFY( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Ensure that state information is available in the DCB.
            CALL ARY1_DSTA( IDCB, STATUS )

*  If the object's data values are undefined, then report an error.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( .NOT. DCB_STA( IDCB ) ) THEN
                  STATUS = ARY__UNDEF
                  CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                  CALL ERR_REP( 'ARY_VERFY_UDEF',
     :            'The array ^ARRAY is in an undefined state.', STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_VERFY_ERR',
     :   'ARY_VERFY: Error verifying array data structure.', STATUS )
         CALL ARY1_TRACE( 'ARY_VERFY', STATUS )
      END IF

      END
