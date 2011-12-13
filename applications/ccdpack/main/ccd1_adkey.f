       SUBROUTINE CCD1_ADKEY( KEYWRD, VALUE, USEPRO, PROTEC, BUFFER,
     :                        IAT, STATUS )
*+
*  Name:
*     CCD1_ADKEY

*  Purpose:
*     Adds a keyword and value to a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ADKEY( KEYWRD, VALUE, PROTEC, BUFFER, IAT, STATUS )

*  Description:
*     This routine adds a keyword and value combination to a string.
*     The final results is a
*
*          KEYWORD=VALUE
*
*     arrangement the = sign is added by this routine and should not be
*     included in either of the given strings. If the USEPRO argument is
*     set to true then the value is surrounded with the PROTEC strings
*     (say for stopping interpretation by the C-shell).

*  Arguments:
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value of the keyword parameter.
*     USEPRO = LOGICAL (Given)
*        If true then the value is protected by enclosing in PROTECs.
*     PROTEC( 2 ) = CHARACTER * ( * ) (Given)
*        Used if USEPRO is true. These characters "quote" the value
*        string (ie. KEYWORD=PROTEC(1)//VALUE//PROTEC(2) is used_.
*     BUFFER = CHARACTER * ( * ) (Given and Returned)
*        The character buffer into which the keyword value combination
*        is to be written. Character prior to the IAT position are
*        unchanged on exit.
*     IAT = INTEGER (Given and Returned)
*        The position after which the keyword/value combination are to
*        be written. On exit this is set to the last character
*        position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     9-JUN-1993 (PDRAPER):
*        Original version.
*     23-SEP-1993 (PDRAPER):
*        Added PROTEC characters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) KEYWRD
      CHARACTER * ( * ) VALUE
      LOGICAL USEPRO
      CHARACTER * ( * ) PROTEC( 2 )

*  Arguments Given and Returned:
      CHARACTER * ( * ) BUFFER
      INTEGER IAT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN           ! Length of string excluding trailing blnaks
      INTEGER CHR_LEN

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Add the keywrd to the string.
      IAT = IAT - 1
      CALL CHR_PUTC( KEYWRD( : CHR_LEN( KEYWRD ) ), BUFFER, IAT )

*  And the equals sign.
      CALL CHR_PUTC( '=', BUFFER, IAT )

*  Add the first quote if its required.
      IF ( USEPRO ) THEN
         CALL CHR_APPND( PROTEC( 1 ), BUFFER, IAT )
      END IF

*  Now the value.
      CALL CHR_APPND( VALUE( : CHR_LEN( VALUE ) ), BUFFER, IAT )

*  and finally the last quote.
      IF ( USEPRO ) THEN
         CALL CHR_APPND( PROTEC( 2 ), BUFFER, IAT )
      END IF
      END
* $Id$
