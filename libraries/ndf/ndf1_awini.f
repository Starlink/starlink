      SUBROUTINE NDF1_AWINI( TYPE, LBND, UBND, DATA, PNTR, STATUS )
*+
*  Name:
*     NDF1_AWINI

*  Purpose:
*     Initialise an axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWINI( TYPE, LBND, UBND, DATA, PNTR, STATUS )

*  Description:
*     The routine initialises an axis width array of any numeric type.
*     The values assigned are calculated from an associated axis data
*     array (giving the positions of the pixel centres) by forming
*     differences between the centre positions of neighbouring pixels.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type of the axis width array; a primitive numeric type
*        string (case insensitive).
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     DATA( LBND : UBND ) = DOUBLE PRECISION (Given)
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived.
*     PNTR = INTEGER (Given)
*        Pointer to the axis width array, whose size should be equal to
*        UBND - LBND + 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Make a copy of the numeric type string and check it is not too
*     long.
*     -  If OK, convert it to upper case.
*     -  Test the type string against each valid value in turn, calling
*     the appropriate routine to initialise the array.
*     -  Note if the type string was not recognised.
*     -  If the type string was invalid, then report an error.

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
*     18-OCT-1990 (RFWS):
*        Original version.
*     9-NOV-1990 (RFWS):
*        Changed name and altered to perform initialisation instead of
*        extrapolation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER LBND
      INTEGER UBND
      DOUBLE PRECISION DATA( LBND : UBND )
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) UTYPE ! Upper case type string
      LOGICAL TYPOK              ! Whether type string is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a copy of the numeric type string and check it is not too long.
      UTYPE = TYPE
      TYPOK = UTYPE .EQ. TYPE

*  If OK, convert it to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( UTYPE )

*  Test the type string against each valid value in turn, calling the
*  appropriate routine to initialise the array.

*  ...byte.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL NDF1_AWIB( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...unsigned byte.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL NDF1_AWIUB( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                       STATUS )

*  ...double precision.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL NDF1_AWID( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...integer.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL NDF1_AWII( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...real.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL NDF1_AWIR( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...word.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL NDF1_AWIW( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...unsigned word.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL NDF1_AWIUW( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                       STATUS )

*  ...64-bit integer.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL NDF1_AWIK( LBND, UBND, DATA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  Note if the type string was not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the type string was invalid, then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_AWINI' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_AWINI_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ''^BADTYPE'' (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWINI', STATUS )

      END
