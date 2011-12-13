      SUBROUTINE TRN_STOKD( TOKEN, VALUE, TEXT, NSUBS, STATUS )
*+
*  Name:
*     TRN_STOKD

*  Purpose:
*     Substitute token with double precision value.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_STOKD( TOKEN, VALUE, TEXT, NSUBS, STATUS )

*  Description:
*     The routine searches the TEXT string provided and replaces every
*     occurrence of the TOKEN sub-string with the double precision
*     value supplied, formatted as a decimal character string. To be
*     replaced, token sub-strings must be delimited by non-alphanumeric
*     characters. If they are delimited by angle brackets thus:
*     <TOKEN>, then the brackets are regarded as part of the token and
*     are also replaced. Token names must begin with an alphabetic
*     character and continue with alphanumeric characters only
*     (including underscore). They may be of any length.
*
*     The routine returns the substituted text and a count of the
*     number of token values substituted (NSUBS). If the VALUE supplied
*     is negative, its formatted value will be enclosed in parentheses.
*     This routine is not sensitive to case.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The token string.
*     VALUE = DOUBLE PRECISION (Given)
*        The value to be substituted.
*     TEXT = CHARACTER * ( * ) (Given & Returned)
*        The text to be processed.
*     NSUBS = INTEGER (Returned)
*        The number of token substitutions made.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Call CHR_DTOC to format the VALUE as a character string.
*     - Add a 'D' exponent (if necessary replacing an 'E') to indicate
*     that this is a formatted double precision value.
*     - Call TRN_STOK to make the token substitution(s), enclosing the
*     value to be substituted in parentheses if it is negative.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988 (RFWS):
*        Original version.
*     22-JUL-1992 (RFWS):
*        Fixed bug; CHR_DTOC called with a status argument which
*        doesn't exist.
*     31-MAR-1995 (RFWS):
*        Fixed problem due to change in CHR to writing 'D' exponents
*        instead of the traditional 'E'.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      CHARACTER * ( * ) TOKEN    ! Token string
      DOUBLE PRECISION VALUE     ! Value to be substituted

*  Arguments Given and Returned:
      CHARACTER * ( * ) TEXT     ! Text to be processed

*  Arguments Returned:
      INTEGER NSUBS              ! Number of token substitutions made

*  Status:
      INTEGER STATUS             ! Error status

*  Local Variables:
      CHARACTER * ( VAL__SZD ) CHVAL ! Character formatted value
      INTEGER IE                 ! Position of exponent character 'E'
      INTEGER NCH                ! Number of characters in value

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Format the value as a character string.
      CALL CHR_DTOC( VALUE, CHVAL, NCH )

*  See if the formatted double precision value contains an exponent
*  character 'E'. If so, then replace it with a 'D' to distinguish it
*  from a formatted real value.
      IE = INDEX( CHVAL( : NCH ), 'E' )
      IF ( IE .NE. 0 ) CHVAL( IE : IE ) = 'D'

*  If there is no exponent character, then add 'D0' at the end of the
*  string, to indicate that this is a double precision value.
      IF ( INDEX( CHVAL( : NCH ), 'D' ) .EQ. 0 ) THEN
         NCH = NCH + 2
         CHVAL( NCH - 1 : NCH ) = 'D0'
      END IF

*  If the value is positive or zero, substitute the token with the
*  formatted character string.
      IF ( VALUE .GE. 0.0D0 ) THEN
         CALL TRN_STOK( TOKEN, CHVAL( : NCH ), TEXT, NSUBS, STATUS )

*  If the value is negative, enclose it in parentheses before making
*  the substitution.
      ELSE
         CALL TRN_STOK( TOKEN, '(' // CHVAL( : NCH ) // ')', TEXT,
     :                  NSUBS, STATUS )
      END IF

      END
