      SUBROUTINE NDF1_VSTYP( IACB, TYPE, STATUS )
*+
*  Name:
*     NDF1_VSTYP

*  Purpose:
*     Determine the numeric data type of the scaled variance component
*     of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VSTYP( IACB, TYPE, STATUS )

*  Description:
*     The routine returns the numeric data type of the scaled variance
*     component of an NDF identified by its index in the ACB. The data
*     type is returned as an upper case character string. The returned
*     type describes the values stored in the array, before they are
*     unscaled using the associated scale and zero values. Use NDF1_VTYP
*     if you need the data type of the array after it has been unscaled.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry identifying the NDF.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric data type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.

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
*        DCB_VID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's variance array.
*        DCB_VSTYP( NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP ) (Read)
*           Default numeric data type of the NDF's variance component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

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

*  If so, then enquire the data type of the array.
         IF ( VALID ) THEN
            CALL ARY_SCTYP( DCB_VID( IDCB ), TYPE, STATUS )

*  Otherwise, use the default value stored in the DCB.
         ELSE
            CALL NDF1_CCPY( DCB_VTYP( IDCB ), TYPE, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VSTYP', STATUS )

      END
