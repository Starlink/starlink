      SUBROUTINE COF_BN2HT( TFORM, TYPE, STATUS )
*+
*  Name:
*     COF_BN2HT

*  Purpose:
*     Converts FITS binary-table TFORM into an HDS primitive data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_BN2HT( TFORM, TYPE, STATUS )

*  Description:
*     This converts a TFORM string from a FITS binary-table header,
*     into its equivalent HDS primitive data type.

*  Arguments:
*     TFORM = CHARACTER * ( * ) (Given)
*        The TFORM code from a FITS binary-table header.  The GSFC
*        extension for characters is supported.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS primitive data type corresponding to the TFORM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The supported TFORM type codes and their equivalent HDS types are:
*     B, _UBYTE; I, _WORD; J, _INTEGER; E or F, _REAL; D, _DOUBLE; and
*     A[n], _CHAR[*n].
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996-1997 Central Laboratory of the Research
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
*     1997 November 12 (MJC):
*        Added _LOGICAL.  (This was originally done in 1996, but
*        appears not to have made it into V1.0.)
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) TFORM

*  Arguments Returned:
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Variables:
      INTEGER CPOS               ! Position of the type code in TFORM
      INTEGER LENGTH             ! Number of characters in supplied
      INTEGER NC                 ! Number of characters in supplied
                                 ! TFORM
      CHARACTER * ( 1 ) TCODE    ! Type code
      INTEGER TPOS               ! Character position in the type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Skip over the leading repeat-count numeral(s) and spaces.
      CPOS = 1
      CALL CHR_SKCHR( '0123456789 ', TFORM, .TRUE., CPOS )

*  Extract the type character and convert to uppercase.
      TCODE = TFORM( CPOS:CPOS )
      CALL CHR_UCASE( TCODE )

*  Check for each case.
      IF ( TCODE .EQ. 'B' ) THEN
         TYPE = '_UBYTE'

      ELSE IF ( TCODE .EQ. 'I' ) THEN
         TYPE = '_WORD'

      ELSE IF ( TCODE .EQ. 'J' ) THEN
         TYPE = '_INTEGER'

      ELSE IF ( TCODE .EQ. 'E' .OR. TCODE .EQ. 'F' ) THEN
         TYPE = '_REAL'

      ELSE IF ( TCODE .EQ. 'D' ) THEN
         TYPE = '_DOUBLE'

      ELSE IF ( TCODE .EQ. 'L' ) THEN
         TYPE = '_LOGICAL'

      ELSE IF ( TCODE .EQ. 'A' ) THEN
         TYPE = '_CHAR'

*  There may be a string length following.
         NC = CHR_LEN( TFORM )
         IF ( NC .GT. CPOS ) THEN

*  Assume that it comprises one or more integers.  If the conversion
*  works, just append this to the returned HDS type, otherwise annul the
*  error, and leave the HDS type unchanged.  Use a new error context.
            CALL ERR_MARK
            CALL CHR_CTOI( TFORM( CPOS+1:NC ), LENGTH, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               TPOS = 5
               CALL CHR_APPND( '*', TYPE, TPOS )
               CALL CHR_APPND( TFORM( CPOS+1:NC ), TYPE, TPOS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

            CALL ERR_RLSE

         ELSE IF ( CPOS .GT. 1 ) THEN

*  Assume that it comprises one or more integers.  If the conversion
*  works, just append this to the returned HDS type, otherwise annul the
*  error, and leave the HDS type unchanged.  Use a new error context.
            CALL ERR_MARK
            CALL CHR_CTOI( TFORM( 1:CPOS-1 ), LENGTH, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               TPOS = 5
               CALL CHR_APPND( '*', TYPE, TPOS )
               CALL CHR_APPND( TFORM( 1:CPOS-1 ), TYPE, TPOS )
            ELSE
               CALL ERR_ANNUL( STATUS )
            END IF

            CALL ERR_RLSE
         END IF

*  Report that there is no equivalent HDS type to the bit, and complex
*  types of a binary table.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TC', TCODE )
         CALL ERR_REP( 'COF_BN2HT_TYPERR',
     :     'The TFORM code ^TC does not have an HDS counterpart.',
     :     STATUS )
      END IF

      END
