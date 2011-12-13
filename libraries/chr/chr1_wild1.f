      LOGICAL FUNCTION CHR1_WILD1( SLEN, MLEN, WLEN )
*+
*  Name:
*     CHR1_WILD1

*  Purpose:
*     Validate input variables for CHR_WILD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR1_WILD1( SLEN, MLEN, WLEN )

*  Description:
*     Ensure that the match pattern string (MATCH) is long enough, at
*     least as long as the candidate string (STRING).
*     Ensure that if the candidate string (STRING) is of finite length,
*     the wild card pattern (WILDS) is also of finite length.
*

*  Arguments:
*     MLEN = INTEGER (Given)
*            Declared length of MATCH
*     SLEN = INTEGER (Given)
*            Declared length of STRING
*     WLEN = INTEGER (Given)
*            Declared length of WILDS

*  Returned Value:
*     CHR1_WILD1 = LOGICAL
*        Whether the input variables to CHR_WILD are valid.

*  Algorithm:

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC: A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (PCTR):
*        Original version.
*     8-OCT-1991 (PCTR):
*        Final (working) version with changes prompted by P.T. Wallace.
*     8-MAR-1993 (PCTR):
*        Cure bug which leads to a WILDN chracter being present
*        at the beginning of the WILDS string.
*     27-SEP-1993 (ACC):
*        Modularise: this routine created.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER MLEN               ! Declared length of MATCH
      INTEGER SLEN               ! Declared length of STRING
      INTEGER WLEN               ! Declared length of WILDS

*  Arguments Returned:
*     None

*  Local Constants:
*     None

*  Local Variables:
*     None

*.


*  Initialise the returned value, CHR1_WILD1.
      CHR1_WILD1 = .TRUE.

      IF ( SLEN .GT. MLEN ) THEN

*     The match pattern string is too short to contain the complete
*     match pattern (i.e. the length of the candidate string), so return.
         CHR1_WILD1 = .FALSE.

      ELSE IF ( ( SLEN .GT. 0 ) .AND. ( WLEN .EQ. 0 ) ) THEN

*     The wild-card pattern has zero length and the candidate string does
*     not: no match is possible.
         CHR1_WILD1 = .FALSE.

      END IF

      END
