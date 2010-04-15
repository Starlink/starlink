      SUBROUTINE ARY_IMPRT( LOC, IARY, STATUS )
*+
*  Name:
*     ARY_IMPRT

*  Purpose:
*     Import an array into the ARY_ system from HDS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_IMPRT( LOC, IARY, STATUS )

*  Description:
*     The routine imports an array into the ARY_ system from HDS and
*     issues an identifier for it. The array may then be manipulated by
*     the ARY_ routines.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to an array structure.
*     IARY = INTEGER (Returned)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The locator supplied as input to this routine may later be
*     annulled without affecting the subsequent behaviour of the ARY_
*     system.
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The ARY__NOID
*     constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Set an initial value for the IARY argument before checking the
*     inherited status.
*     -  Import the array structure into the ACB.
*     -  Export an identifier for the new array.
*     -  If an error occurred, then reset the IARY value and report
*     context information.

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
*     28-JUL-1989 (RFWS):
*        Original version.
*     14-SEP-1989 (RFWS):
*        Changed to call ARY1_IMP rather than ARY1_DIMP.
*     9-OCT-1989 (RFWS):
*        Minor change to algorithm description to reflect previous
*        change to routine.
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

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Set an initial value for the IARY argument.
      IARY = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array structure into the ACB.
      CALL ARY1_IMP( LOC, IACB, STATUS )

*  Export an identifier for the new array.
      CALL ARY1_EXPID( IACB, IARY, STATUS )

*  If an error occurred, then reset the IARY value and report context
*  information.
      IF ( STATUS .NE. SAI__OK ) THEN
         IARY = ARY__NOID
         CALL ERR_REP( 'ARY_IMPRT_ERR',
     :   'ARY_IMPRT: Error importing an array structure from HDS.',
     :   STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY_IMPRT', STATUS )

      END
