      SUBROUTINE ARY_SCTYP( IARY, TYPE, STATUS )
*+
*  Name:
*     ARY_SCTYP

*  Purpose:
*     Obtain the numeric type of a scaled array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SCTYP( IARY, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of a scaled array as an
*     upper-case character string (e.g. '_REAL'). The returned type
*     describes the values stored in the array, before they are unscaled
*     using the associated scale and zero values. Use ARY_TYPE if you
*     need the data type of the array after it has been unscaled.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     TYPE = CHARACTER * ( * ) (Returned)
*        Numeric type of the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array is not stored in SCALED form, then this routine
*     returns the same type as the ARY_TYPE routine.
*     -  The symbolic constant ARY__SZTYP may be used for declaring the
*     length of a character variable which is to hold the numeric type
*     of an array. This constant is defined in the include file
*     ARY_PAR.

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
         CALL ERR_REP( 'ARY_SCTYP_ERR', 'ARY_SCTYP: Error obtaining '//
     :                 'the numeric type of a scaled array.', STATUS )
         CALL ARY1_TRACE( 'ARY_SCTYP', STATUS )
      END IF

      END
