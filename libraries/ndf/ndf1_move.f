      SUBROUTINE NDF1_MOVE( TYPE, N, PNTR1, PNTR2, STATUS )
*+
*  Name:
*     NDF1_MOVE

*  Purpose:
*     Move a vectorised array to a new location.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MOVE( TYPE, N, PNTR1, PNTR2, STATUS )

*  Description:
*     The routine moves a vectorised array of any numeric data type from
*     one location to another. The source and destination arrays are
*     passed by pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric data type of the array to be moved; an HDS primitive
*        numeric type string (case insensitive).
*     N = INTEGER (Given)
*        Number of array elements to be moved.
*     PNTR1 = INTEGER (Given)
*        Pointer to the source array.
*     PNTR2 = INTEGER (Given)
*        Pointer to the destination array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Make a copy of the data type string and check it is not
*     truncated.
*     -  If OK, convert the string to upper case.
*     -  Compare the data type with each valid value in turn and call
*     the appropriate routine to copy the array.
*     -  If the data type string was not recognised, then note this
*     fact.
*     -  If a bad data type string was supplied then report an error.

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
*     3-APR-1990 (RFWS):
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
      CHARACTER * ( * ) TYPE
      INTEGER N
      INTEGER PNTR1
      INTEGER PNTR2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) UTYPE ! Upper case data type string
      INTEGER IERR               ! Dummy variable
      INTEGER NERR               ! Dummy variable
      LOGICAL TYPOK              ! Whether data type string is valid

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a copy of the data type string and check it is not truncated.
      UTYPE = TYPE
      TYPOK = UTYPE .EQ. TYPE

*  If OK, convert the string to upper case.
      IF ( TYPOK ) THEN
         CALL CHR_UCASE( UTYPE )

*  Compare the data type with each valid value in turn and call the
*  appropriate routine to copy the array.

*  ...byte data.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  ...unsigned byte data.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                       %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                       STATUS )

*  ...double precision data.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  ...integer data.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  ...real data.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  ...word data.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  ...unsigned word data.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                       %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                       STATUS )

*  ...64-bit integer data.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL VEC_KTOK( .FALSE., N, %VAL( CNF_PVAL( PNTR1 ) ),
     :                     %VAL( CNF_PVAL( PNTR2 ) ), IERR, NERR,
     :                     STATUS )

*  If the data type string was not recognised, then note this fact.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If a bad data type string was supplied then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_MOVE' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_MOVE_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ^BADTYPE (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MOVE', STATUS )

      END
