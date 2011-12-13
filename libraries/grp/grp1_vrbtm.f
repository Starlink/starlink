      SUBROUTINE GRP1_VRBTM( SLOT, GEXP, VERB, GRPEXP, STATUS )
*+
*  Name:
*     GRP1_VRBTM

*  Purpose:
*     Escape any control characters within verbatim sections of the
*     supplied group expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_VRBTM( SLOT, GEXP, VERB, GRPEXP, STATUS )

*  Description:
*     This subroutine searches the supplied group expression for the
*     strings "<!!" (which marks the start of a verbatim section) and
*     "!!>" (which marks the end of a verbatim section). A modified form
*     of the supplied group expression is created in which:
*
*     1) any control characters located within a verbatim section are
*     preceeded by an escape character. If no escape character is defined
*     for the group, then the control characters will NOT be escaped.
*
*     2) the srings marking the start or end of a verbatim section are
*     removed.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group.
*     GEXP = CHARACTER * ( * ) (Given)
*        The group expression to be checked.
*     VERB = LOGICAL (Given and Returned)
*        Indicates whether or not we are currently within a verbatim
*        section.
*     GRPEXP = CHARACTER * ( * ) (Returned)
*        The modified group expression.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-AUG-2001 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants
      INCLUDE 'GRP_CONST'        ! GRP private constants

*  Arguments Given:
      INTEGER SLOT
      CHARACTER GEXP*(*)

*  Arguments Given and Returned:
      LOGICAL VERB
      CHARACTER GRPEXP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CC*1             ! A control character
      CHARACTER CCS*(GRP__NCHAR) ! A list of all defined control characters
      CHARACTER ESCC*1           ! The escape character
      INTEGER I                  ! Index of next input character to read
      INTEGER IMAX               ! Length of input string variable
      INTEGER J                  ! Index of next output character to write
      INTEGER JMAX               ! Length of output string variable
      INTEGER NC                 ! No. of currently used control characters
      LOGICAL CCOK               ! Is the control character defined?
      LOGICAL ESCOK              ! Is the escape character defined?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the group's current escape character.
      CALL GRP1_CONC( SLOT, GRP__PESCC, ESCC, ESCOK, STATUS )

*  Form a string holding all the currently used control characters.
      NC = 0
      DO I = 1, GRP__NCHAR
         CALL GRP1_CONC( SLOT, I, CC, CCOK, STATUS )
         IF( CCOK ) THEN
            NC = NC + 1
            CCS( NC : NC ) = CC
         END IF
      END DO

*  Process the supplied string
      GRPEXP = ' '
      I = 1
      J = 1
      IMAX = LEN( GEXP )
      JMAX = LEN( GRPEXP )
      DO WHILE( I .LE. IMAX .AND. J .LE. JMAX )

*  If the remaining text starts with the string "<!!", jump over it, and
*  indicate that we are now in a verbatim section.
         IF( GEXP( I : MIN( I + 2, IMAX ) ) .EQ. '<!!' ) THEN
            I = I + 3
            VERB = .TRUE.

*  If the remaining text starts with the string "!!>", jump over it, and
*  indicate that we are now not in a verbatim section.
         ELSE IF( GEXP( I : MIN( I + 2, IMAX ) ) .EQ. '!!>' ) THEN
            I = I + 3
            VERB = .FALSE.

*  If an escape character is defined, and we are in a verbatim section,
*  check the current character against the list of control characters.
         ELSE IF( ESCOK .AND. VERB ) THEN

*  If found, insert an escape character in the output string.
            IF( INDEX( CCS( : NC ), GEXP( I : I ) ) .GT. 0 ) THEN
               GRPEXP( J : J ) = ESCC
               IF( J .LT. JMAX ) J = J + 1
            END IF

* Copy the input character.
            GRPEXP( J : J ) = GEXP( I : I )
            I = I + 1
            J = J + 1

*  If we are not escaping control characters for any reason, just copy the
*  current character to the returned group expression.
         ELSE
            GRPEXP( J : J ) = GEXP( I : I )
            I = I + 1
            J = J + 1
         END IF

      END DO

      END
