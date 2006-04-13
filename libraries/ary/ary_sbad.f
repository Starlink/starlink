      SUBROUTINE ARY_SBAD( BAD, IARY, STATUS )
*+
*  Name:
*     ARY_SBAD

*  Purpose:
*     Set the bad-pixel flag for an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SBAD( BAD, IARY, STATUS )

*  Description:
*     The routine sets the value of the bad-pixel flag for an array. A
*     call to this routine with BAD set to .TRUE. declares that the
*     specified array may contain bad pixel values for which checks
*     must be made by algorithms which subsequently processes its
*     values.  A call with BAD set to .FALSE. declares that there are
*     definitely no bad values present and that subsequent checks for
*     such values may be omitted.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Bad-pixel flag value to be set.
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array is mapped for access when this routine is called,
*     then the bad-pixel flag will be associated with the mapped
*     values. This information will only be transferred to the actual
*     data object when the array is unmapped (but only if it was mapped
*     for UPDATE or WRITE access). The value transferred may be
*     modified if conversion errors occur during the unmapping process.

*  Algorithm:
*     -  Import the array identifier.
*     -  Check that WRITE access to the array is permitted.
*     -  If the array is currently mapped for access, then set the bad
*     pixel flag value in the Mapping Control Block.
*     -  Otherwise, set the bad pixel flag value for the array's Access
*     Control Block entry.
*     -  If an error occurred, then report context information.

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
*     11-JUL-1989 (RFWS):
*        Original version.
*     15-SEP-1989 (RFWS):
*        Changed to call ARY1_CHACC to check that WRITE access is
*        available.
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

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to associated MCB entry.

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_BAD( ARY__MXMCB ) = LOGICAL (Write)
*           Whether there may be "bad" values in the mapping transfer
*           region (if it exists).
*        MCB_PBAD( ARY__MXMCB ) = LOGICAL (Write)
*           Whether there may be "bad" values in the padding region (if
*           it exists) which surrounds the mapping transfer region.

*  Arguments Given:
      LOGICAL BAD
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to ACB entry
      INTEGER IMCB               ! Index to MCB entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Check that WRITE access to the array is permitted.
      CALL ARY1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the index to the MCB entry for the array. If this is non-zero,
*  then the array is currently mapped, so the bad pixel flag value being
*  set refers to the mapped data.
         IMCB = ACB_IMCB( IACB )
         IF ( IMCB .GT. 0 ) THEN

*  Enter the bad pixel flag value into the MCB. This information will
*  then be transferred to the corresponding ACB entry when the data are
*  unmapped.
            MCB_BAD( IMCB ) = BAD
            MCB_PBAD( IMCB ) = BAD

*  If the array is not mapped, then set the bad pixel flag for the ACB
*  entry directly.
         ELSE
            CALL ARY1_SBD( BAD, IACB, STATUS )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SBAD_ERR',
     :   'ARY_SBAD: Error setting the bad-pixel flag value for an ' //
     :   'array.', STATUS )
         CALL ARY1_TRACE( 'ARY_SBAD', STATUS )
      END IF

      END
