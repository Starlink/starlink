      SUBROUTINE ARY_TYPE( IARY, TYPE, STATUS )
*+
*  Name:
*     ARY_TYPE

*  Purpose:
*     Obtain the numeric type of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_TYPE( IARY, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of an array as an upper-case
*     character string (e.g. '_REAL').

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric type of the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The symbolic constant ARY__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type
*     of an array. This constant is defined in the include file
*     ARY_PAR.

*  Algorithm:
*     -  Import the array identifier.
*     -  Obtain the Data Control Block index for the data object.
*     -  Ensure that type information is available for the data object.
*     -  Copy the numeric type string to the output argument.
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
*     14-JUN-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_TYP( ACB_MXDCB ) = CHARACTER * ( DAT__SZTYP ) (Read)
*           Numeric type string for data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER IACB               ! Index to ACB entry
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the DCB index for the data object.
         IDCB = ACB_IDCB( IACB )

*  Ensure that type information is available.
         CALL ARY1_DTYP( IDCB, STATUS )

*  Copy the numeric type string to the output argument.
         CALL ARY1_CCPY( DCB_TYP( IDCB ), TYPE, STATUS )
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_TYPE_ERR',
     :   'ARY_TYPE: Error obtaining the numeric type of an array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_TYPE', STATUS )
      END IF

      END
