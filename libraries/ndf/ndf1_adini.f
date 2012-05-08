      SUBROUTINE NDF1_ADINI( TYPE, LBNDA, UBNDA, PNTR, STATUS )
*+
*  Name:
*     NDF1_ADINI

*  Purpose:
*     Initialise an axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ADINI( TYPE, LBNDA, UBNDA, PNTR, STATUS )

*  Description:
*     The routine assigns initial values to an axis data array of any
*     numeric type. The values assigned are chosen so as to define the
*     default axis coordinate system, in which, for each axis, the
*     pixel with index (I) has a central coordinate of (I-0.5).

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type of the axis data array to which initial values
*        are to be assigned (an HDS primitive numeric data type string,
*        case insensitive).
*     LBNDA = INTEGER (Given)
*        Index of the first pixel on the axis.
*     UBNDA = INTEGER (Given)
*        Index of the last pixel on the axis.
*     PNTR = INTEGER (Given)
*        Pointer to the 1-dimensional axis data array, whose size should
*        be equal to UBNDA - LBNDA + 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     LBNDA-0.5, LBNDA+0.5, LBNDA+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values LBNDA,
*     LBNDA+1, LBNDA+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned
*     cannot be represented using the array's numeric type, then an
*     error will be reported and STATUS set.

*  Algorithm:
*     -  Make a copy of the numeric type string and check it is not too
*     long.
*     -  If OK, convert it to upper case.
*     -  Test the type string against each valid value in turn, calling
*     the appropriate routine to assign initial values to the array.
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
*     13-JUN-1990 (RFWS):
*        Original version.
*     8-OCT-1990 (RFWS):
*        Corrected prolgue information about the values assigned.
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
      INTEGER LBNDA
      INTEGER UBNDA
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
*  appropriate routine to assign values to the array.

*  ...byte.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL NDF1_ADIB( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...unsigned byte.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL NDF1_ADIUB( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                       STATUS )

*  ...double precision.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL NDF1_ADID( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...integer.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL NDF1_ADII( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...real.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL NDF1_ADIR( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...word.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL NDF1_ADIW( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                      STATUS )

*  ...unsigned word.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL NDF1_ADIUW( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
     :                       STATUS )

*  ...64-bit integer.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL NDF1_ADIK( LBNDA, UBNDA, %VAL( CNF_PVAL( PNTR ) ),
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
            CALL MSG_SETC( 'ROUTINE', 'NDF1_ADINI' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_ADINI_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ''^BADTYPE'' (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ADINI', STATUS )

      END
