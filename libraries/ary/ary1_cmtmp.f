      SUBROUTINE ARY1_CMTMP( TYPE, NDIM, DIM, LOC, PNTR, STATUS )
*+
*  Name:
*     ARY1_CMTMP

*  Purpose:
*     Create and map a temporary workspace array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CMTMP( TYPE, NDIM, DIM, LOC, PNTR, STATUS )

*  Description:
*     The routine creates a temporary HDS object of the type and shape
*     specified and maps it for use as workspace. A pointer to the
*     workspace is returned.  The type specified must be a primitive
*     numeric data type, otherwise an error will be reported.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        An HDS primitive data type string specifying the type of object
*        to be created (case insensitive).
*     NDIM = INTEGER (Given)
*        Number of object dimensions.
*     DIM( * ) = INTEGER (Given)
*        Object dimensions.
*     LOC = CHARACTER * ( * ) (Returned)
*        HDS locator to the temporary object.
*     PNTR = INTEGER (Returned)
*        Pointer to the mapped array. The array is not initialised by
*        this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check if the data type specified is a primitive numeric type
*     and report an error if it is not.
*     -  Create the temporary object.
*     -  Map it for 'WRITE' access.

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
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) LOC
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL NUMER              ! Whether data type is numeric

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the data type specified is numeric and report an error if
*  it is not.
      CALL ARY1_INTYP( TYPE, NUMER, STATUS )
      IF ( .NOT. NUMER ) THEN
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_CMTMP' )
         CALL MSG_SETC( 'BADTYPE', TYPE )
         CALL ERR_REP( 'ARY1_CMTMP_TYPE',
     :   'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :   'of ''^BADTYPE'' (internal programming error).', STATUS )

*  Create the temporary object and map it as workspace.
      ELSE
         CALL ARY1_TEMP( TYPE, NDIM, DIM, LOC, STATUS )
         CALL DAT_MAP( LOC, TYPE, 'WRITE', NDIM, DIM, PNTR, STATUS )
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CMTMP', STATUS )

      END
