      SUBROUTINE NDF_SAME( INDF1, INDF2, SAME, ISECT, STATUS )
*+
*  Name:
*     NDF_SAME

*  Purpose:
*     Enquire if two NDFs are part of the same base NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SAME( INDF1, INDF2, SAME, ISECT, STATUS )

*  Description:
*     The routine determines whether two NDF identifiers refer to parts
*     of the same base NDF. If so, it also determines whether their
*     transfer windows intersect.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the first NDF (or NDF section).
*     INDF2 = INTEGER (Given)
*        Identifier for the second NDF (or NDF section).
*     SAME = LOGICAL (Returned)
*        Whether the identifiers refer to parts of the same base NDF.
*     ISECT = LOGICAL (Returned)
*        Whether their transfer windows intersect.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the transfer windows of the two NDFs (or NDF sections)
*     intersect, then (i) they both refer to the same base NDF, and
*     (ii) altering values in an array component of one of the NDFs can
*     result in the values in the corresponding component of the other
*     NDF changing in consequence. Thus, the array components of the
*     two NDFs are not mutually independent.

*  Algorithm:
*     -  Import the two NDF identifiers.
*     -  Use the ARY_ system to determine whether the data arrays of the
*     two NDFs refer to the same base array.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-NOV-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

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
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2

*  Arguments Returned:
      LOGICAL SAME
      LOGICAL ISECT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! 1st NDF index in the ACB
      INTEGER IACB2              ! 2nd NDF index in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the two NDF identifiers.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      CALL NDF1_IMPID( INDF2, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Use the ARY_ system to see if the data arrays of the two NDFs refer
*  to the same base array.
         CALL ARY_SAME( ACB_DID( IACB1 ), ACB_DID( IACB2 ), SAME, ISECT,
     :                  STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SAME_ERR',
     :   'NDF_SAME: Error enquiring if two NDFs are part of the ' //
     :   'same base NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_SAME', STATUS )
      END IF

      END
