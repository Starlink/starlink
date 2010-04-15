      SUBROUTINE ARY1_STP( TYPE, CMPLX, IACB, STATUS )
*+
*  Name:
*     ARY1_STP

*  Purpose:
*     Set a new data type for an array identified by its ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_STP( TYPE, CMPLX, IACB, STATUS )

*  Description:
*     The routine changes the data type of an array identified by its
*     entry in the ACB. If the array's state is "defined", then the
*     data it contains undergo type conversion, otherwise no conversion
*     is necessary. Conversions from non-complex to complex data types
*     (and vice versa) are handled.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The new numeric data type for the array; a primitive numeric
*        HDS data type string (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether the new data type is to be complex.
*     IACB = INTEGER (Given)
*        Index to the array entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine has no effect if the array is not a base array.

*  Side Effects:
*     -  The array's bad pixel flag may be set if data conversion
*     errors occur.

*  Algorithm:
*     -  If the array is not a base array, then check that it is not
*     mapped and report an error if it is. Otherwise, do nothing.
*     -  If the array is a base array, then check that no part of it is
*     mapped and report an error if it is.
*     -  Change the data type of the data object.
*     -  If data conversion errors occurred, then set the bad pixel flag
*     value to .TRUE..

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     8-AUG-1989 (RFWS):
*        Original version.
*     7-MAR-1990 (RFWS):
*        Minor improvements to error messages and comments.
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
*           Data object locator.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read)
*           Number of current read mappings to the data object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read)
*           Number of current write mappings to the data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a cut (i.e. section).
*        ACB_IDCB( ARY__MXACB) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in the MCB.

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL DCE                ! Data conversion error occurred?
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If an array section has been provided, then check to ensure that it
*  is not mapped and report an error if it is. Otherwise do nothing, as
*  the data type of a section cannot be changed except through the base
*  array.
      IF ( ACB_CUT( IACB ) ) THEN
         IF ( ACB_IMCB( IACB ) .GT. 0 ) THEN
            STATUS = ARY__ISMAP
            IDCB = ACB_IDCB( IACB )
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_STP_MAP',
     :      'The array ^ARRAY is mapped for access through the ' //
     :      'specified identifier (possible programming error).',
     :      STATUS )
         END IF

*  If the array is a base array, then see if any part of it is mapped
*  for access. Report an error if it is.
      ELSE
         IDCB = ACB_IDCB( IACB )
         IF ( ( DCB_NREAD( IDCB ) .NE. 0 ) .OR.
     :        ( DCB_NWRIT( IDCB ) .NE. 0 ) ) THEN
            STATUS = ARY__ISMAP
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_STP_BMAP',
     :      'The base array ''^ARRAY'' is mapped for access, ' //
     :      'perhaps through another identifier (possible' //
     :      'programming error).', STATUS )

*  Convert the data object to its new type.
         ELSE
            CALL ARY1_DSTP( TYPE, CMPLX, IDCB, DCE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If data conversion errors occurred, then set the bad pixel flag for
*  the array.
               IF ( DCE ) THEN
                  CALL ARY1_SBD( .TRUE., IACB, STATUS )
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_STP', STATUS )

      END
