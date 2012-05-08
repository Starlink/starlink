      SUBROUTINE NDF1_S2V( BAD, TYPE, EL, PNTR, DCE, STATUS )
*+
*  Name:
*     NDF1_S2V

*  Purpose:
*     Convert standard deviation values to variances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_S2V( BAD, TYPE, EL, ARRAY, DCE, STATUS )

*  Description:
*     The routine converts a vectorised array of standard deviation
*     values into variances by squaring them. It will check for "bad"
*     values in the array if required. If a negative standard deviation
*     is found, then STATUS is set to NDF__NGSTD, an error is reported
*     and a bad value is assigned to the affected array element -
*     however, the routine continues to process the entire array. The
*     array to be processed is passed by pointer.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether it is necessary to check for bad values.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric data type of the array; a primitive numeric HDS
*        data type string (case insensitive).
*     EL = INTEGER (Given)
*        Number of array elements to process.
*     PNTR = INTEGER (Given)
*        Pointer to the array to be processed. On input, an array of
*        standard deviation values is supplied. On output, they are
*        replaced by the variance values. The pointer is not changed.
*     DCE = LOGICAL (Returned)
*        Whether a data conversion error occurred resulting in new bad
*        values being produced. This may result either from numerical
*        overflow (which the routine handles) or from replacement of
*        illegal negative standard deviations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Make a copy of the TYPE argument and check that it is not
*     truncated.
*     -  If OK, convert the value to upper case.
*     -  Compare the upper case type string with each permitted value in
*     turn, calling the appropriate routine to process the array.
*     -  Note if the data type string is not recognised.
*     -  If the TYPE argument has an invalid value, then report an
*     error.

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
*     14-NOV-1990 (RFWS):
*        Added an extra status check.
*     {enter_changes_here}

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
      LOGICAL BAD
      CHARACTER * ( * ) TYPE
      INTEGER EL
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) UTYPE ! Upper case data type string
      LOGICAL TYPOK              ! Whether data type string is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the data type to the UTYPE variable and check it is not
*  truncated.
      UTYPE = TYPE
      TYPOK = UTYPE .EQ. TYPE

*  If OK, then convert to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( UTYPE )

*  Compare the upper case data type string with each permitted value in
*  turn, calling the appropriate routine to process the array.

*  ...Byte data.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL NDF1_S2VB( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                      STATUS )

*  ...Unsigned byte data.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL NDF1_S2VUB( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                       STATUS )

*  ...Double precision data.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL NDF1_S2VD( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                      STATUS )

*  ...Integer data.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL NDF1_S2VI( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                      STATUS )

*  ...Real data.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL NDF1_S2VR( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                      STATUS )

*  ...Word data.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL NDF1_S2VW( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                      STATUS )

*  ...Unsigned word data.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL NDF1_S2VUW( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                       STATUS )

*  ...64-bit integer data.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL NDF1_S2VK( BAD, EL, %VAL( CNF_PVAL( PNTR ) ), DCE,
     :                       STATUS )

*  Note if the data type string was not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the TYPE argument has an invalid value, then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_S2V' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_S2V_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ''^BADTYPE'' (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_S2V', STATUS )

      END
