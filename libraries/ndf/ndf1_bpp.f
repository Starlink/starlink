      SUBROUTINE NDF1_BPP( TYPE, EL, PNTR, BAD, STATUS )
*+
*  Name:
*     NDF1_BPP

*  Purpose:
*     Determine if bad pixels are present in a vectorised array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_BPP( TYPE, EL, PNTR, BAD, STATUS )

*  Description:
*     The routine examines the values in a vectorised array of any
*     numeric type and returns a logical result BAD indicating whether
*     any element of the array contains the appropriate "bad" pixel
*     value VAL__BADx (where x corresponds with the array's data type).
*     The vectorised array to be examined is passed by pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the vectorised array; an HDS primitive
*        numeric data type string (case insensitive).
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     PNTR = INTEGER (Given)
*        Pointer to the array.
*     BAD = LOGICAL (Returned)
*        Whether any array element had the value VAL__BADx.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Make a copy of the TYPE argument and check it is not truncated.
*     -  Convert to upper case.
*     -  Compare the data type with each permitted value in turn and
*     call the appropriate routine to check the array for bad pixels.
*     -  If the TYPE argument was not valid, then report an error.

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
*     18-DEC-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     14-NOV-1990 (RFWS):
*        Added an extra status check.
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
      INTEGER EL
      INTEGER PNTR

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) UTYPE ! Upper case version of TYPE
      LOGICAL TYPOK              ! Whether the TYPE argument is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a copy of the TYPE argument and check it is not truncated.
      UTYPE = TYPE
      TYPOK = UTYPE .EQ. TYPE

*  If OK, then convert to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( UTYPE )

*  Compare the data type with each permitted value in turn, calling the
*  appropriate routine to examine the array for bad pixels.

*  ...byte.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL NDF1_BPPB( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...unsigned byte.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL NDF1_BPPUB( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...double precision.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL NDF1_BPPD( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...integer.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL NDF1_BPPI( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...real.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL NDF1_BPPR( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...word.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL NDF1_BPPW( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...unsigned word.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL NDF1_BPPUW( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  ...64-bit integer.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL NDF1_BPPK( EL, %VAL( CNF_PVAL( PNTR ) ), BAD, STATUS )

*  Note if the data type string was not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the TYPE argument was not valid, then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_BPP' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_BPP_BAD',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ''^BADTYPE'' (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_BPP', STATUS )

      END
