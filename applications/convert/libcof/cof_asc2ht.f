      SUBROUTINE COF_ASC2HT( TFORM, TYPE, STATUS )
*+
*  Name:
*     COF_ASC2HT

*  Purpose:
*     Converts FITS ASCII-table TFORM into an HDS primitive data type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_ASC2HT( TFORM, TYPE, STATUS )

*  Description:
*     This converts a TFORM string from a FITS ASCII-table header,
*     into its equivalent HDS primitive data type.

*  Arguments:
*     TFORM = CHARACTER * ( * ) (Given)
*        The TFORM code from a FITS ASCII-table header.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The HDS primitive data type corresponding to the TFORM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The supported TFORM type codes and their equivalent HDS types are:
*     Iw, _INTEGER; Kw, _INT64; Ew.d or Fw.d, _REAL; Dw.d, _DOUBLE;
*     and A[w], _CHAR[*w].
*
*     Formerly routine COF_BN2HT was used for this purpose but that gave
*     HDS type _WORD for TFORM I and so was wrong if the data contained
*     integers larger than 16 bits.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     AJC: Alan J Chipperfield (Starlink, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*      1-SEP-2000 (AJC):
*        Original version.
*     2012 April 30 (MJC):
*        Add 64-bit integer.
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

*  Extract the type character and convert to uppercase.
      TCODE = TFORM( 1:1 )
      CALL CHR_UCASE( TCODE )

*  Check for each case.
      IF ( TCODE .EQ. 'I' ) THEN
         TYPE = '_INTEGER'

      ELSE IF ( TCODE .EQ. 'K' ) THEN
         TYPE = '_INT64'

      ELSE IF ( TCODE .EQ. 'E' .OR. TCODE .EQ. 'F' ) THEN
         TYPE = '_REAL'

      ELSE IF ( TCODE .EQ. 'D' ) THEN
         TYPE = '_DOUBLE'

      ELSE IF ( TCODE .EQ. 'A' ) THEN
         TYPE = '_CHAR'

*  There may be a string length following.
         CPOS = 1
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

         END IF

*  Report that there is no equivalent HDS type to the bit, and complex
*  types of a binary table.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TC', TCODE )
         CALL ERR_REP( 'COF_ASC2HT_TYPERR',
     :     'The TFORM code ^TC does not have an HDS counterpart.',
     :     STATUS )
      END IF

      END
