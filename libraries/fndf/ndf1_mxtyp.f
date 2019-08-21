      SUBROUTINE NDF1_MXTYP( ITYPE1, ITYPE2, ITYPE, STATUS )
*+
*  Name:
*     NDF1_MXTYP

*  Purpose:
*     Maximise two numeric data type codes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MXTYP( ITYPE1, ITYPE2, ITYPE, STATUS )

*  Description:
*     The routine compares two integer codes representing numeric data
*     types and "maximises" them. It returns a new type code
*     representing the numeric data type with the lowest precision such
*     that data stored using either of the input data types can be
*     converted to the new type without loss of precision.

*  Arguments:
*     ITYPE1 = INTEGER (Given)
*        First numeric type code.
*     ITYPE2 = INTEGER (Given)
*        Second numeric type code.
*     ITYPE = INTEGER (Returned)
*        The "maximised" type code.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The type codes which this routine uses are symbolic constants
*     with names of the form NDF__TYPx (where x indicates the numeric
*     type). These constants are defined in the include file NDF_CONST.
*     -  The implementation of this routine depends on the collating
*     sequence for numeric data types established by the definitions of
*     these type codes.

*  Algorithm:
*     -  Find the numerical maximum of the two input type codes,
*     constraining it to lie within range.
*     -  Loop to increase this value.
*     -  Test each value to see if conversion from the input data types
*     involves loss of precision. Check for errors.
*     -  If no loss of precision is involved, then set the result.

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
*     16-JAN-1990 (RFWS):
*        Original version.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file and restored standard
*        prologue delimiting lines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Arguments Given:
      INTEGER ITYPE1
      INTEGER ITYPE2

*  Arguments Returned:
      INTEGER ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for refining the result
      INTEGER ISTART             ! First guess at the result
      LOGICAL OK1                ! Loss of precision for type 1?
      LOGICAL OK2                ! Loss of precision for type 2?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the numerical maximum value of the two input type codes,
*  constraining it to lie within range.
      ISTART = MIN( MAX( NDF__TYPUB, ITYPE1, ITYPE2 ), NDF__MXTYP )

*  Loop to increase this value until conversion from both input data
*  types does not lose precision.
      DO 1 I = ISTART, NDF__MXTYP

*  Test both conversions to see if precision is lost.
         CALL NDF1_QITYP( ITYPE1, I, OK1, STATUS )
         CALL NDF1_QITYP( ITYPE2, I, OK2, STATUS )

*  Check for errors.
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 2

*  If no precision is lost, then set the result.
         ELSE IF ( OK1 .AND. OK2 ) THEN
            ITYPE = I
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MXTYP', STATUS )

      END
