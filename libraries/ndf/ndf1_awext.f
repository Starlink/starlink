      SUBROUTINE NDF1_AWEXT( TYPE, UPPER, PIX0, WIDTH, LBND, UBND,
     :                       PNTR, STATUS )
*+
*  Name:
*     NDF1_AWEXT

*  Purpose:
*     Assign extrapolated values to an axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_AWEXT( TYPE, UPPER, PIX0, WIDTH, LBND, UBND, PNTR,
*                      STATUS )

*  Description:
*     The routine assigns extrapolated values to an axis width array of
*     any numeric type. It is intended for assigning values to those
*     axis width array elements which are not present in an actual NDF
*     data structure, but which are encountered when accessing the axis
*     component of a section which is a super-set of the NDF.  The
*     extrapolated value assigned is a constant, which is supplied as
*     an argument.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type of the axis width array; a primitive numeric type
*        string (case insensitive).
*     UPPER = LOGICAL (Given)
*        If a .TRUE. value is given for this argument, then
*        extrapolation will be performed towards higher array index
*        values. Otherwise extrapolation will be towards lower array
*        index values.
*     PIX0 = INTEGER (Given)
*        The index of the first "unknown" pixel to be assigned a value.
*        If UPPER is .TRUE., this will be the index of the pixel
*        following the last one whose value is known. If UPPER is
*        .FALSE., it will be the index of the pixel before the first
*        one whose value is known.
*     WIDTH = DOUBLE PRECISION (Given)
*        The extrapolated width value to be assigned.
*     LBND = INTEGER (Given)
*        The lower bound of the axis width array.
*     UBND = INTEGER (Given)
*        The upper bound of the axis width array.
*     PNTR = INTEGER (Given)
*        Pointer to the axis width array to be extrapolated, whose size
*        should be equal to UBND - LBND + 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If overflow occurs because the extrapolated value cannot be
*     represented using the array's numeric type, then an error will be
*     reported and STATUS set.

*  Algorithm:
*     -  Make a copy of the numeric type string and check it is not too
*     long.
*     -  If OK, convert it to upper case.
*     -  Test the type string against each valid value in turn, calling
*     the appropriate routine to extrapolate the array.
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
*     9-NOV-1990 (RFWS):
*        Original version.
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
      LOGICAL UPPER
      INTEGER PIX0
      DOUBLE PRECISION WIDTH
      INTEGER LBND
      INTEGER UBND
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
*  appropriate routine to extrapolate the array.

*  ...byte.
         IF ( UTYPE .EQ. '_BYTE' ) THEN
            CALL NDF1_AWEB( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...unsigned byte.
         ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
            CALL NDF1_AWEUB( UPPER, PIX0, WIDTH, LBND, UBND,
     :                       %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...double precision.
         ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
            CALL NDF1_AWED( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...integer.
         ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
            CALL NDF1_AWEI( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...real.
         ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
            CALL NDF1_AWER( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...word.
         ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
            CALL NDF1_AWEW( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...unsigned word.
         ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
            CALL NDF1_AWEUW( UPPER, PIX0, WIDTH, LBND, UBND,
     :                       %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  ...64-bit integer.
         ELSE IF ( UTYPE .EQ. '_INT64' ) THEN
            CALL NDF1_AWEK( UPPER, PIX0, WIDTH, LBND, UBND,
     :                      %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  Note if the type string was not recognised.
         ELSE
            TYPOK = .FALSE.
         END IF
      END IF

*  If the type string was invalid, then report an error.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( .NOT. TYPOK ) THEN
            STATUS = NDF__FATIN
            CALL MSG_SETC( 'ROUTINE', 'NDF1_AWEXT' )
            CALL MSG_SETC( 'BADTYPE', TYPE )
            CALL ERR_REP( 'NDF1_AWEXT_TYPE',
     :      'Routine ^ROUTINE called with an invalid TYPE argument ' //
     :      'of ''^BADTYPE'' (internal programming error).', STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_AWEXT', STATUS )

      END
