      SUBROUTINE NDF1_PSTYP( TYPE, ITYPE, STATUS )
*+
*  Name:
*     NDF1_PSTYP

*  Purpose:
*     Parse a numeric data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSTYP( TYPE, ITYPE, STATUS )

*  Description:
*     The routine parses a string representing a numeric data type and
*     returns an integer code representing the data type identified. An
*     error is reported if an invalid data type string is supplied.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type string to be parsed.
*     ITYPE = INTEGER (Returned)
*        Type code identifying the data type.  These integers have
*        symbolic names NDF__TYPx (where "x" identifies the data type)
*        and are defined in the NDF_CONST include file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Compare the type string with each permitted value in turn,
*     obtaining the appropriate integer value for each data type.
*     -  If the data type was not recognised, then report an error.

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
*     20-NOV-1989 (RFWS):
*        Original version.
*     12-JAN-1990 (RFWS):
*        Corrected bug in character string subscript initialisation.
*        Also inserted missing escape character in error message token.
*     16-JAN-1990 (RFWS):
*        Substantial simplification to process only a single data type
*        string, rather than a comma-separated list of them. Renamed
*        from NDF1_PSTPL to NDF1_PSTYP.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the type string with each permitted value in turn, obtaining
*  the appropriate integer value for each data type.

*  ...byte data.
      IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         ITYPE = NDF__TYPB

*  ...unsigned byte data.
      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         ITYPE = NDF__TYPUB

*  ...double precision data.
      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         ITYPE = NDF__TYPD

*  ...integer data.
      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         ITYPE = NDF__TYPI

*  ...real data.
      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         ITYPE = NDF__TYPR

*  ...word data.
      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         ITYPE = NDF__TYPW

*  ...unsigned word data.
      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         ITYPE = NDF__TYPUW

*  ...64-bit integer data.
      ELSE IF ( CHR_SIMLR( TYPE, '_INT64' ) ) THEN
         ITYPE = NDF__TYPK

*  If the data type was not recognised, then report an error.
      ELSE
         STATUS = NDF__TYPIN
         CALL MSG_SETC( 'BADTYPE', TYPE )
         CALL ERR_REP( 'NDF1_PSTYP_BAD',
     :   'Invalid numeric type ''^BADTYPE'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSTYP', STATUS )

      END
