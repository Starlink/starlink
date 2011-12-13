      SUBROUTINE GRP1_RMESC( SLOT, FIRST, LAST, STATUS )
*+
*  Name:
*     GRP1_RMESC

*  Purpose:
*     Remove escape characters from a set of names.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_RMESC( SLOT, FIRST, LAST, STATUS )

*  Description:
*     This subroutine removes escape characters from the specified range
*     of names in the supplied group. Escape characters are only
*     removed if they preceed an active control character.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot index for the group.
*     FIRST = INTEGER (Given)
*        The index of the first name to be checked.
*     LAST = INTEGER (Given)
*        The index of the last name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     27-AUG-1999 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      INTEGER SLOT
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CCHARS*( GRP__NCHAR )! Control characters
      CHARACTER C                ! Current character
      CHARACTER ELEM*( GRP__SZNAM )! Current name
      CHARACTER ESC              ! The escape character
      INTEGER EDEP               ! Indirection depth of the element
      INTEGER EIFILE             ! Index of the indirection file name
      INTEGER EMODGP             ! Identifier for basis group
      INTEGER EMODIN             ! Index into the group given by EMODGP
      INTEGER IESC               ! Absolute index of next escape character
      INTEGER II                 ! Name index
      INTEGER NCC                ! No. of active control characters
      INTEGER P                  ! Character index
      INTEGER Q                  ! Character index
      INTEGER START              ! Index of start of remaining text
      LOGICAL MOD                ! Was element modified?
      LOGICAL OK                 ! Is control character defined?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the escape control character.
      CALL GRP1_CONC( SLOT, GRP__PESCC, ESC, OK, STATUS )

*  Only proceed if the escape character is defined.
      IF( OK ) THEN

*  Get a string holding all the active control characters (except NULL).
         NCC = 0
         DO II = 1, GRP__NCHAR
            IF( II .NE. GRP__PNULC ) THEN
               CALL GRP1_CONC( SLOT, II, C, OK, STATUS )
               IF( OK ) THEN
                  NCC = NCC + 1
                  CCHARS( NCC : NCC ) = C
               END IF
            END IF
         END DO

*  Loop round all required names.
         DO II = FIRST, LAST

*  Get the current element.
            CALL GRP1_GTELM( SLOT, II, ELEM, EDEP, EIFILE, EMODGP,
     :                       EMODIN, STATUS )

*  Loop round finding each escape character in the current name.
            IESC = INDEX( ELEM, ESC )
            START = 1
            MOD = .FALSE.
            DO WHILE( IESC .NE. 0 )

*  Convert the index of the escape character from a value relative to
*  START, to an absolute value.
               IESC = IESC + START - 1

*  If this is the last character in the name, the following character
*  cannot be a control character and so leave the loop, retaining the
*  escape character in the name.
               IF( IESC .GE. GRP__SZNAM ) THEN
                  IESC = 0

*  Otherwise, see if the following character is an active control
*  character.
               ELSE
                  P = IESC + 1
                  C = ELEM( P : P )

*  If this character is contained within the list of active control
*  characters, remove the escape character from the name by shuffling
*  all the remaining characters down one place.
                  IF( INDEX( CCHARS( : NCC ), C ) .NE. 0 ) THEN

                     Q = IESC
                     DO P = IESC + 1, GRP__SZNAM
                        C = ELEM( P : P )
                        ELEM( Q : Q ) = C
                        Q = Q + 1
                     END DO

                     ELEM( GRP__SZNAM : GRP__SZNAM ) = ' '
                     MOD = .TRUE.

*  Correct the index of the character following the escape character.
                     P = IESC

                  END IF

*  Find the next escape character following the escaped control character
*  (which may itself be an escape character).
                  START = P + 1
                  IESC = INDEX( ELEM( START : ), ESC )

               END IF

            END DO

*  Save the modified name if anything changed.
            IF( MOD ) THEN
               CALL GRP1_PTELM( SLOT, II, ELEM, EDEP, EIFILE, EMODGP,
     :                          EMODIN, STATUS )
            END IF

         END DO

      END IF

      END
