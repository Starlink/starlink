      SUBROUTINE COF_BP2HT( BITPIX, FMTCNV, TYPE, STATUS )
*+
*  Name:
*     COF_BP2HT

*  Purpose:
*     Converts FITS BITPIX into an HDS primitive data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_BP2HT( BITPIX, FMTCNV, TYPE, STATUS )

*  Description:
*     This converts a FITS type specified through a BITPIX value
*     into its equivalent HDS primitive data type.

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The BITPIX code from a FITS header.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS primitive data type corresponding to the BITPIX.
*     FMTCNV = LOGICAL (Given)
*        Return a floating point data type?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The supported BITPIX type codes and their equivalent HDS types
*     are: 8, _UBYTE; 16, _WORD; 32, _INTEGER; -32, _REAL; and
*     -64, _DOUBLE.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996, 1999 Central Laboratory of the Research
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
*     DSB: David S. Berry (DSB)
*     {enter_new_authors_here}

*  History:
*     1996 January 21 (MJC):
*        Original version.
*     8-JAN-1999 (DSB):
*        Added FMTCNV argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX
      LOGICAL FMTCNV

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a floating point data type is required, use _DOUBLE for 32 bit
*  integers, and _REAL for all other integer types.
      IF( FMTCNV ) THEN
         IF ( BITPIX .EQ. 8 ) THEN
            TYPE = '_REAL'

         ELSE IF ( BITPIX .EQ. 16 ) THEN
            TYPE = '_REAL'

         ELSE IF ( BITPIX .EQ. 32 ) THEN
            TYPE = '_DOUBLE'

         ELSE IF ( BITPIX .EQ. -32 ) THEN
            TYPE = '_REAL'

         ELSE IF ( BITPIX .EQ. -64 ) THEN
            TYPE = '_DOUBLE'

*  Report that there is no equivalent HDS primitive type to the BITPIX.
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'BP', BITPIX )
            CALL ERR_REP( 'COF_BP2HT_TYPERR',
     :        'The BITPIX code ^BP does not have an HDS counterpart.',
     :        STATUS )
         END IF

*  If both integer or floating point types are acceptable...
      ELSE

*  Simply test for each value in turn, and assign the appropriate
*  HDS primitive data type.
         IF ( BITPIX .EQ. 8 ) THEN
            TYPE = '_UBYTE'

         ELSE IF ( BITPIX .EQ. 16 ) THEN
            TYPE = '_WORD'

         ELSE IF ( BITPIX .EQ. 32 ) THEN
            TYPE = '_INTEGER'

         ELSE IF ( BITPIX .EQ. -32 ) THEN
            TYPE = '_REAL'

         ELSE IF ( BITPIX .EQ. -64 ) THEN
            TYPE = '_DOUBLE'

*  Report that there is no equivalent HDS primitive type to the BITPIX.
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'BP', BITPIX )
            CALL ERR_REP( 'COF_BP2HT_TYPERR',
     :        'The BITPIX code ^BP does not have an HDS counterpart.',
     :        STATUS )
         END IF

      END IF

      END
