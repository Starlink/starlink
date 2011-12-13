      SUBROUTINE TRN_STOKI( TOKEN, VALUE, TEXT, NSUBS, STATUS )
*+
*  Name:
*     TRN_STOKI

*  Purpose:
*     Substitute token with integer value.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_STOKI( TOKEN, VALUE, TEXT, NSUBS, STATUS )

*  Description:
*     The routine searches the TEXT string provided and replaces every
*     occurrence of the TOKEN sub-string with the integer value
*     supplied, formatted as a decimal character string. To be
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
*     VALUE = INTEGER (Given)
*        The value to be substituted.
*     TEXT = CHARACTER * ( * ) (Given & Returned)
*        The text to be processed.
*     NSUBS = INTEGER (Returned)
*        The number of token substitutions made.
*     STATUS = INTEGER (Given & Returned)
*        Inherited error status.

*  Algorithm:
*     - Call CHR_ITOC to format the VALUE as a character string.
*     - Call TRN_STOK to make the token substitution(s), enclosing the
*     value to be substituted in parentheses if it is negative.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*        Fixed bug; CHR_DTOI called with a status argument which
*        doesn't exist.
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
      INTEGER VALUE              ! Value to be substituted

*  Arguments Given and Returned:
      CHARACTER * ( * ) TEXT     ! Text to be processed

*  Arguments Returned:
      INTEGER NSUBS              ! Number of token substitutions made

*  Status:
      INTEGER STATUS             ! Error status

*  Local Variables:
      CHARACTER * ( VAL__SZI ) CHVAL ! Character formatted value
      INTEGER NCH                ! Number of characters in value

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Format the value as a character string.
      CALL CHR_ITOC( VALUE, CHVAL, NCH )

*  If the value is positive or zero, substitute the token with the
*  formatted character string.
      IF ( VALUE .GE. 0 ) THEN
         CALL TRN_STOK( TOKEN, CHVAL( : NCH ), TEXT, NSUBS, STATUS )

*  If the value is negative, enclose it in parentheses before making
*  the substitution.
      ELSE
         CALL TRN_STOK( TOKEN, '(' // CHVAL( : NCH ) // ')', TEXT,
     :                  NSUBS, STATUS )
      END IF

*  Exit routine.
      END
