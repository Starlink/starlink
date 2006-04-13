      SUBROUTINE ARY_FIND( LOC, NAME, IARY, STATUS )
*+
*  Name:
*     ARY_FIND

*  Purpose:
*     Find an array in an HDS structure and import it into the ARY_
*     system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_FIND( LOC, NAME, IARY, STATUS )

*  Description:
*     The routine finds a named array within an HDS structure, imports
*     it into the ARY_ system and issues an identifier for it. The
*     imported array may then be manipulated by the ARY_ routines.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to the enclosing HDS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the HDS structure component to be imported.
*     IARY = INTEGER (Returned)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
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
*        Changed to call ARY1_IMP rather than ARY1_DIMP and added
*        missing call to ARY1_EXPID.
*     15-SEP-1989 (RFWS):
*        Added check that a standard component name has been supplied.
*     15-SEP-1989 (RFWS):
*        Fixed bug; wrong locator was being passed to ARY1_IMP.
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

*  Arguments Given:
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCARY ! Locator to array structure
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Set an initial value for the IARY argument.
      IARY = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that a standard data component name has been supplied.
      CALL ARY1_CHSCN( NAME, STATUS )

*  Locate the named HDS array structure.
      LOCARY = ARY__NOLOC
      CALL DAT_FIND( LOC, NAME, LOCARY, STATUS )

*  Import the array structure into the ACB.
      CALL ARY1_IMP( LOCARY, IACB, STATUS )

*  Export an array identifier.
      CALL ARY1_EXPID( IACB, IARY, STATUS )

*  Annul the locator to the array structure.
      CALL DAT_ANNUL( LOCARY, STATUS )
      LOCARY = ARY__NOLOC
       
*  If an error occurred, then reset the IARY argument and report
*  context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         IARY = ARY__NOID
         CALL ERR_REP( 'ARY_FIND_ERR',
     :   'ARY_FIND: Error finding an array in an HDS structure.',
     :   STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY_FIND', STATUS )

      END
