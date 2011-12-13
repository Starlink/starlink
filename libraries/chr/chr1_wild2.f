      SUBROUTINE CHR1_WILD2( WILDS, WLEN, NWILDA, NWILDN, FIRSTN,
     :                       LASTN )
*+
*  Name:
*     CHR1_WILD2

*  Purpose:
*     Find the number of wild cards in the wild card pattern (WILDS).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR1_WILD2( WILDS, WLEN, NWILDA, NWILDN, FIRSTN, LASTN )

*  Description:
*     Search the wild card pattern (WILDS) for the number of wild card
*     characters.
*     The wild-cards used are:
*
*        % a single character wild-card;
*        * an arbitrary length string wild-card, including zero length.
*
*     There is also a literal escape character '\' for use when the
*     characters '*' and '%' are to be interpreted literally within
*     the wild-card pattern.

*  Arguments:
*     WILDS = CHARACTER * ( * ) (Given)
*        The wild-card pattern to be used in the match.
*     WLEN = INTEGER (Given)
*       The declared length of WILDS.
*     NWILDA = INTEGER  (Returned)
*        The number of single character wild characters (WILDA) in
*        the wild card pattern (WILDS).
*     NWILDN = INTEGER (Returned)
*        The number of arbitrary length string wild characters (WILDN)
*        in the wild card pattern (WILDS).
*     FIRSTN = INTEGER (Returned)
*        The index of the left-hand arbitrary length string wild
*        character (WILDN).
*     LASTN = INTEGER (Returned)
*        The index of the right-hand arbitrary length string wild
*        character (WILDN).

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
*     28-SEP-1993 (ACC):
*        Subroutine created during modlarisation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) WILDS
      INTEGER WLEN               ! Declared length of WILDS

*  Arguments Returned:
      INTEGER NWILDA             ! Number of WILDA wild characters in WILDS
      INTEGER NWILDN             ! Number of WILDN wild characters in WILDS
      INTEGER FIRSTN             ! Index of the left-hand WILDN character
      INTEGER LASTN              ! Index of the right-hand WILDN character

*  Local Constants include file:
      INCLUDE 'CHR_SYS'

*  Local Variables:
      INTEGER ICHW               ! Character loop index for WILDS

*.

*  Find the number of wild-card characters in WILDS.

      NWILDA = 0
      NWILDN = 0
      FIRSTN = 0
      LASTN = 0

      ICHW = 1

*     DO WHILE loop.
 10      CONTINUE
         IF ( ICHW .LE. WLEN ) THEN

*        Check for escaped characters and increment the wild character
*        when appropriate.
            IF ( WILDS( ICHW : ICHW ) .EQ. ESCAPE ) THEN
               ICHW = ICHW + 1
            ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDA ) THEN
               NWILDA = NWILDA + 1
            ELSE IF ( WILDS( ICHW : ICHW ) .EQ. WILDN ) THEN
               NWILDN = NWILDN + 1
               IF ( NWILDN .EQ. 1 ) FIRSTN = ICHW
               LASTN = ICHW
            END IF

*        Increment the character pointer.
            ICHW = ICHW + 1
         GO TO 10
         END IF

      END
