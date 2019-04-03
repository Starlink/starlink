      SUBROUTINE NDF1_VSBD( BAD, IACB, STATUS )
*+
*  Name:
*     NDF1_VSBD

*  Purpose:
*     Set the bad pixel flag for the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VSBD( BAD, IACB, STATUS )

*  Description:
*     The routine sets a value for the logical bad pixel flag of an
*     NDF's variance component. The NDF is identified by its ACB entry.

*  Arguments:
*     BAD = LOGICAL (Given)
*        The value to be set.
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  If the variance component is mapped for access, then set the
*     ACB bad pixel flag for the mapped values and note it has been
*     modified.
*     -  Otherwise, ensure that variance information is available in the
*     DCB and ACB.
*     -  See if the ARY_ system identifier for the variance array is
*     valid. If not, then the array does not exist.
*     -  If the array exists, then set its bad pixel flag.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1989 (RFWS):
*        Original version.
*     21-MAR-1990 (RFWS):
*        Changed handling of the bad pixel flag for mapped values to
*        note when it is modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_VID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the variance array.
*        ACB_VMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the variance component is mapped for access.
*        ACB_VMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for mapped variance values.
*        ACB_VMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      LOGICAL BAD
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether the variance array exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the variance array is mapped for access, then set the ACB bad
*  pixel flag for the mapped values and note it has been modified.
      IF ( ACB_VMAP( IACB ) ) THEN
         ACB_VMBAD( IACB ) = BAD
         ACB_VMBMD( IACB ) = .TRUE.

*  Otherwise, ensure that variance information is available in the DCB
*  and ACB.
      ELSE
         CALL NDF1_VIMP( IACB, STATUS )

*  See if the ARY_ system identifier for the variance array is valid.
*  If not, then the array does not exist.
         CALL ARY_VALID( ACB_VID( IACB ), THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it exists, then set its bad pixel flag (if it does not exist,
*  then the variance component is undefined, so its bad pixel flag
*  cannot be changed and remains at .TRUE.).
            IF ( THERE ) THEN
               CALL ARY_SBAD( BAD, ACB_VID( IACB ), STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VSBD', STATUS )

      END
