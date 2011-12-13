      SUBROUTINE COF_FD2HT( DATCOD, TYPE, STATUS )
*+
*  Name:
*     COF_FD2HT

*  Purpose:
*     Converts FITSIO binary-table datacode into an HDS data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_FD2HT( DATCOD, TYPE, STATUS )

*  Description:
*     This converts a FITSIO datacode string into its equivalent HDS
*     primitive data type.

*  Arguments:
*     DATCOD = INTEGER (Given)
*        The FITSIO datacode from a FITS binary-table header whose
*        equivalent HDS primitive data type is to be found.  Thus for
*        example, 42 would generate _REAL, and 21 makes _WORD.  See the
*        notes for a full list.  Generally, the meaning need not be
*        known, as one merely passes the value returned by FTGTCL into
*        this routine.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS primitive data type corresponding to the datacode.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The supported datacode type codes and their equivalent HDS types
*     are: 1, _UBYTE; 11, _UBYTE; 14, _LOGICAL; 16, _CHAR; 21, _WORD;
*     41, _INTEGER; 42, _REAL; and 82, _DOUBLE.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 21 (MJC):
*        Original version.
*     1998 January 22 (MJC):
*        Made 82 _DOUBLE (used to be 81).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER DATCOD

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply test for each value in turn, and assign the appropriate
*  HDS primitive data type.
      IF ( DATCOD .EQ. 1 .OR. DATCOD .EQ. 11 ) THEN
         TYPE = '_UBYTE'

      ELSE IF ( DATCOD .EQ. 21 ) THEN
         TYPE = '_WORD'

      ELSE IF ( DATCOD .EQ. 41 ) THEN
         TYPE = '_INTEGER'

      ELSE IF ( DATCOD .EQ. 42 ) THEN
         TYPE = '_REAL'

      ELSE IF ( DATCOD .EQ. 82 ) THEN
         TYPE = '_DOUBLE'

      ELSE IF ( DATCOD .EQ. 14 ) THEN
         TYPE = '_LOGICAL'

      ELSE IF ( DATCOD .EQ. 16 ) THEN
         TYPE = '_CHAR'

*  Report that there is no equivalent HDS primitive type to the FITSIO
*  datacode (i.e. complex and double complex).
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'DC', DATCOD )
         CALL ERR_REP( 'COF_FD2HT_TYPERR',
     :     'The FITSIO datacode ^DC does not have an HDS counterpart.',
     :     STATUS )
      END IF

      END
