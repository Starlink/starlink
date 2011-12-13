      SUBROUTINE NDF1_VFTP( IACB, FTYPE, STATUS )
*+
*  Name:
*     NDF1_VFTP

*  Purpose:
*     Determine the full data type of the variance component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VFTP( IACB, FTYPE, STATUS )

*  Description:
*     The routine returns the full data type of the variance component
*     of an NDF identified by its index in the ACB. The data type is
*     returned as an upper case character string.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry identifying the NDF.
*     FTYPE = CHARACTER * ( * ) (Returned)
*        Full data type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that variance information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  See if the ARY_ system identifier for the variance array is
*     valid.
*     -  If so, then enquire the array's full data type.
*     -  Otherwise, return a result based on the default attributes
*     stored in the DCB.

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
*     20-NOV-1989 (RFWS):
*        Original version.
*     8-DEC-1989 (RFWS):
*        Installed the NDF1_VIMP routine.
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
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_VCPX( NDF__MXDCB ) = LOGICAL (Read)
*           Whether the NDF's variance component holds complex data by
*           default.
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        DCB_VTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Default numeric data type of the NDF's variance component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      CHARACTER * ( * ) FTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL VALID              ! Whether ARY_ system ID is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that variance information is available in the DCB and ACB.
      CALL NDF1_VIMP( IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  See if the ARY_ system identifier for the variance array is valid.
      CALL ARY_VALID( DCB_VID( IDCB ), VALID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then enquire the full data type of the array.
         IF ( VALID ) THEN
            CALL ARY_FTYPE( DCB_VID( IDCB ), FTYPE, STATUS )

*  Otherwise, use the default attributes stored in the DCB to return an
*  appropriate full data type string.
         ELSE
            IF ( DCB_VCPX( IDCB ) ) THEN
               CALL NDF1_CCPY( 'COMPLEX' // DCB_VTYP( IDCB ), FTYPE,
     :                         STATUS )
            ELSE
               CALL NDF1_CCPY( DCB_VTYP( IDCB ), FTYPE, STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VFTP', STATUS )

      END
