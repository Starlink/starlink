      SUBROUTINE NDF1_QFRM( IACB, FORM, STATUS )
*+
*  Name:
*     NDF1_QFRM

*  Purpose:
*     Determine the storage form of the quality component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QFRM( IACB, FORM, STATUS )

*  Description:
*     The routine returns the storage form of the quality component of
*     an NDF identified by its index in the ACB. The storage form is
*     returned as an upper case character string.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry identifying the NDF.
*     FORM = CHARACTER * ( * ) (Returned)
*        Storage form.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that quality information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  See if the ARY_ system identifier for the quality array is
*     valid.
*     -  If so, then enquire the array's storage form.
*     -  Otherwise, return the default value stored in the DCB.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     29-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VFRM routine.
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
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.
*        DCB_QFRM( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
*           The default form of array used to store data in the NDF's
*           quality component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      CHARACTER * ( * ) FORM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Whether ARY_ system ID is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that quality information is available in the DCB and ACB.
      CALL NDF1_QIMP( IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  See if the ARY_ system identifier for the quality array is valid.
      CALL ARY_VALID( DCB_QID( IDCB ), VALID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then enquire the storage form of the array.
         IF ( VALID ) THEN
            CALL ARY_FORM( DCB_QID( IDCB ), FORM, STATUS )

*  Otherwise, use the default value stored in the DCB.
         ELSE
            CALL NDF1_CCPY( DCB_QFRM( IDCB ), FORM, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QFRM', STATUS )

      END
