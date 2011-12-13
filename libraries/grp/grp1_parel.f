      SUBROUTINE GRP1_PAREL( SLOT, TEXT, FIRST, P1, P2, K1, K2, S1, S2,
     :                       T1, T2, NEXT, STATUS )
*+
*  Name:
*     GRP1_PAREL

*  Purpose:
*     Parse an element expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_PAREL( SLOT, TEXT, FIRST, P1, P2, K1, K2, S1, S2, T1,
*                      T2, NEXT, STATUS )

*  Description:
*     The end of the first element in the section of the supplied text
*     starting at index FIRST is located. The element is split into
*     four section;
*
*     1) The prefix, which is any string occuring before the outer-most
*     OPEN_KERNEL character.
*
*     2)The kernel, which is the string occuring between the first
*     OPEN_KERNEL character and the last CLOSE_KERNEL character (or the
*     whole string prior to the first SEPARATOR character if no
*     OPEN_KERNEL or CLOSE_KERNEL characters are found). An error is
*     reported if OPEN_KERNEL and CLOSE_KERNEL characters are not
*     correctly nested. This kernel may contain other kernels nested
*     within it.
*
*     3) The suffix, which is any string occuring after the outer-most
*     CLOSE_KERNEL character, but before the first SEPARATOR character.
*
*     4) The substitution string, which starts with the first SEPARATOR
*     character after the suffix and ends with the third SEPARATOR
*     character after the suffix. An error is reported if an incorrect
*     number of SEPARATOR characters are found.
*
*     The indices of the first and last characters in each of these
*     sections is returned. If any section is blank, or null (i.e. of
*     zero length), then the index to the last character is returned
*     smaller than the index to the first character.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the group whose control characters are be
*        used when parsing the group expression.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text of the group expression or element to be parsed.
*     FIRST = INTEGER (Given)
*        The index (within TEXT) of the first character in the element
*        to be parsed.
*     P1 = INTEGER (Returned)
*        The index of the first character in the prefix.
*     P2 = INTEGER (Returned)
*        The index of the last character in the prefix. Returned less
*        than P1 if there is no prefix or if the prefix is blank.
*     K1 = INTEGER (Returned)
*        The index of the first character in the outer-most kernel.
*     K2 = INTEGER (Returned)
*        The index of the last character in the outer-most kernel.
*        Returned less than K1 if the kernel is blank.
*     S1 = INTEGER (Returned)
*        The index of the first character in the suffix.
*     S2 = INTEGER (Returned)
*        The index of the last character in the suffix. Returned less
*        than S1 if there is no suffix or if the suffix is blank.
*     T1 = INTEGER (Returned)
*        The index of the first character in the substitution string
*        (i.e. the index of the first SEPARATOR character).
*     T2 = INTEGER (Returned)
*        The index of the last character in the substitution string
*        (i.e. the index of the third SEPARATOR character).  Returned
*        less than T1 if there is no substitition string.
*     NEXT = INTEGER (Returned)
*        The index of the DELIMITER character which marks the end of
*        the element. Returned equal to zero if no DELIMITER character
*        was found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     26-JAN-1994 (DSB):
*        Original version.
*     27-AUG-1999 (DSB):
*        Added control character escape facility.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP_ private constants
      INCLUDE 'GRP_ERR'          ! GRP_ error constants

*  Arguments Given:
      INTEGER SLOT
      CHARACTER TEXT*(*)
      INTEGER FIRST

*  Arguments Returned:
      INTEGER P1
      INTEGER P2
      INTEGER K1
      INTEGER K2
      INTEGER S1
      INTEGER S2
      INTEGER T1
      INTEGER T2
      INTEGER NEXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.
      LOGICAL GRP1_CHKCC         ! See if a character is a control character

*  Local Variables:
      CHARACTER CH               ! Current character
      CHARACTER CLNCC            ! Close nest control character
      CHARACTER DELCC            ! Delimiter control character
      CHARACTER ESCCC            ! The escape character
      CHARACTER KCLCC            ! Close kernel control character
      CHARACTER KOPCC            ! Open kernel control character
      CHARACTER OPNCC            ! Open nest control character
      CHARACTER SEPCC            ! Separator control character

      INTEGER I                  ! Index of current character
      INTEGER L                  ! Position of last non-blank character
      INTEGER LAST               ! Position of last non-blank character
      INTEGER N                  ! Position of next non-blank character
      INTEGER NESTK              ! Kernel nesting level
      INTEGER NESTN              ! OPEN_NEST nesting level
      INTEGER NSEPCC             ! No. of separators found so far

      LOGICAL CLN                ! Is current character a "close nest"?
      LOGICAL CLNOK              ! Close nest control character defined?
      LOGICAL DEL                ! Is current character a delimiter?
      LOGICAL DELOK              ! Delimiter control character defined?
      LOGICAL ESCOK              ! Is the escape character defined?
      LOGICAL KCL                ! Is current character a "close kernel"
      LOGICAL KCLOK              ! Close kernel control character defined?
      LOGICAL KOK                ! Can a kernel expression be started?
      LOGICAL KOP                ! Is current character an "open kernel"
      LOGICAL KOPOK              ! Open kernel control character defined?
      LOGICAL OPN                ! Is current character an "open nest"?
      LOGICAL OPNOK              ! Open nest control character defined?
      LOGICAL SEP                ! Is current character a separator?
      LOGICAL SEPOK              ! Separator control character defined?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the supplied starting position is less than 1.
      IF( FIRST .LE. 0 ) THEN
         STATUS = GRP__INTER
         CALL MSG_SETI( 'FIRST', FIRST )
         CALL ERR_REP( 'GRP1_PAREL_ERR1', 'GRP_PAREL: Invalid start'//
     :                 'position (^FIRST) supplied (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Get the required syntax characters for the group being expanded.
      CALL GRP1_CONC( SLOT, GRP__PDELC, DELCC, DELOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PMSPC, SEPCC, SEPOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__POPNC, OPNCC, OPNOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PCLNC, CLNCC, CLNOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__POPKC, KOPCC, KOPOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PCLKC, KCLCC, KCLOK, STATUS )
      CALL GRP1_CONC( SLOT, GRP__PESCC, ESCCC, ESCOK, STATUS )

*  Initialise the returned pointers to indicate that all strings are
*  blank.
      P1 = FIRST
      P2 = 0

      K1 = FIRST
      K2 = 0

      S1 = FIRST
      S2 = 0

      T1 = FIRST
      T2 = 0

*  Initialise the position of the next element delimiter to zero, to
*  indicate that only one element was found in the supplied text.
      NEXT = 0

*  Store the used length of the supplied character string.
      LAST = CHR_LEN( TEXT )

*  Report an error if the supplied starting position is beyond the end
*  of the string.
      IF( FIRST .GT. LAST ) THEN
         STATUS = GRP__INTER
         CALL MSG_SETC( 'TEXT', TEXT )
         CALL MSG_SETI( 'FIRST', FIRST )
         CALL MSG_SETI( 'LAST', LAST )
         CALL ERR_REP( 'GRP1_PAREL_ERR2', 'GRP_PAREL: Supplied start'//
     :                 'position (^FIRST) is beyond end (^LAST) of '//
     :                 'string ''^TEXT'' (programming error).', STATUS )
         GO TO 999
      END IF

*  If the text to be examined is blank, no further processing is needed.
      IF( TEXT( FIRST : LAST ) .NE. ' ' ) THEN

*  Initialise the kernel to be the whole string.
         K2 = LAST

*  Initialise the nesting levels associated with kernel delimiters and
*  OPEN_NEST and CLOSE_NEST delimiters.
         NESTK = 0
         NESTN = 0

*  Set a flag to indicate that new kernel expressions may be started.
         KOK = .TRUE.

*  Initialise the number of substitution characters found so far.
         NSEPCC = 0

*  Look at each significant character in the string in turn.
         DO I = FIRST, LAST
            CH = TEXT ( I : I )

*  Classify the character.
            OPN = GRP1_CHKCC( TEXT, I, OPNCC, ESCCC, ESCOK ) .AND. OPNOK
            CLN = GRP1_CHKCC( TEXT, I, CLNCC, ESCCC, ESCOK ) .AND. CLNOK
            KOP = GRP1_CHKCC( TEXT, I, KOPCC, ESCCC, ESCOK ) .AND. KOPOK
            KCL = GRP1_CHKCC( TEXT, I, KCLCC, ESCCC, ESCOK ) .AND. KCLOK
            SEP = GRP1_CHKCC( TEXT, I, SEPCC, ESCCC, ESCOK ) .AND. SEPOK
            DEL = GRP1_CHKCC( TEXT, I, DELCC, ESCCC, ESCOK ) .AND. DELOK

*  If the current character is an OPEN_NEST delimiter, increment
*  the associated nesting level.
            IF( OPN ) THEN
               NESTN = NESTN + 1

*  If the current character is a CLOSE_NEST delimiter, decrement the
*  associated nesting level. Report an error and abort if the nesting
*  level goes negative.
            ELSE IF( CLN ) THEN
               NESTN = NESTN - 1
               IF( NESTN .LT. 0 ) THEN
                  STATUS = GRP__INVEL
                  CALL MSG_SETC( 'TEXT', TEXT )
                  CALL MSG_SETC( 'OP', OPNCC )
                  CALL MSG_SETC( 'CL', CLNCC )
                  CALL ERR_REP( 'GRP1_PAREL_ERR3', 'GRP1_PAREL: '//
     :                          'Un-matched delimiters ''^OP^CL'' '//
     :                          'found in ''^TEXT''.', STATUS )
                  GO TO 999
               END IF

            END IF

*  Characters occuring inside an expression delimited by OPEN_NEST and
*  CLOSE_NEST characters are ignored, so skip the further checks if the
*  OPEN_NEST nesting level is not zero.
            IF( NESTN .EQ. 0 ) then

*  If the current character is an opening kernel delimiter, increment
*  the kernel nesting level.
               IF( KOP ) THEN
                  NESTK = NESTK + 1

*  Kernel expressions may only be started if they are separated by an
*  element delimiter from any previous kernel expression. Report an
*  error an abort if this is not the case.
                  IF( .NOT. KOK ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'TEXT', TEXT )
                     CALL ERR_REP( 'GRP1_PAREL_ERR4', 'GRP1_PAREL: '//
     :                             'Two or more adjacent kernels '//
     :                             'found in ''^TEXT''.', STATUS )
                     GO TO 999
                  END IF

*  Report an error and abort if a kernel delimiter is found within a
*  substitution string.
                  IF( NSEPCC .NE. 0 ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'TEXT', TEXT )
                     CALL MSG_SETC( 'SEP', SEPCC )
                     CALL ERR_REP( 'GRP1_PAREL_ERR5', 'GRP1_PAREL: '//
     :                             'Mis-placed separator character '//
     :                             '''^SEP'' found in ''^TEXT''.',
     :                             STATUS )
                     GO TO 999
                  END IF

*  If the current kernel nesting level is one, return the position of
*  the last character in the prefix, and the position of the first
*  character in the outer-most kernel.
                  IF( NESTK .EQ. 1 ) THEN
                     P2 = I - 1
                     K1 = I + 1
                  END IF

*  If the current character is a closing kernel delimiter, decrement
*  the kernel nesting level. Set a flag to indicate that new kernel
*  expressions may not be started until after the next element
*  delimiter. Report an error and abort if the kernel nesting level
*  goes negative.
               ELSE IF( KCL ) THEN
                  NESTK = NESTK - 1
                  KOK = .FALSE.
                  IF( NESTK .LT. 0 ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'TEXT', TEXT )
                     CALL MSG_SETC( 'OP', KOPCC )
                     CALL MSG_SETC( 'CL', KCLCC )
                     CALL ERR_REP( 'GRP1_PAREL_ERR6', 'GRP1_PAREL: Un'//
     :                             '-matched delimiters ''^OP^CL'' '//
     :                             'found in ''^TEXT''.', STATUS )
                     GO TO 999
                  END IF

*  Report an error and abort if a kernel delimiter is found within a
*  substitution string.
                  IF( NSEPCC .NE. 0 ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'TEXT', TEXT )
                     CALL MSG_SETC( 'SEP', SEPCC )
                     CALL ERR_REP( 'GRP1_PAREL_ERR7', 'GRP1_PAREL: '//
     :                             'Mis-placed separator character '//
     :                             '''^SEP'' found in ''^TEXT''.',
     :                             STATUS )
                     GO TO 999
                  END IF

*  If the current kernel nesting level is zero, return the position of
*  the first character in the suffix, and the position of the last
*  character in the outer-most kernel.
                  IF( NESTK .EQ. 0 ) THEN
                     S1 = I + 1
                     K2 = I - 1
                  END IF

*  If the current character is a substitution string separator,
*  increment the number of substitution characters found since the
*  last reset.
               ELSE IF( SEP ) THEN
                  NSEPCC = NSEPCC + 1

*  If the current nesting level is zero, return the position of the
*  first separator, and the position of the last character in the
*  suffix or kernel (depending on whther or not a suffix has been
*  started).
                  IF( NESTK .EQ. 0 .AND. NSEPCC .EQ. 1 ) THEN
                     T1 = I
                     IF( S1 .GT. FIRST .AND. S2 .EQ. 0 ) THEN
                        S2 = I - 1
                     ELSE
                        K2 = I - 1
                     END IF
                  END IF

*  If all three separators required for a valid substitution string
*  have been found, reset the separator count and return the position
*  of the end of the substitution string if the current kernel nesting
*  level is zero. Report an error if the next non-blank character is
*  neither an element delimiter nor a closing kernel delimiter.
                  IF( NSEPCC .EQ. 3 ) THEN
                     NSEPCC = 0
                     IF( NESTK .EQ. 0 ) T2 = I

                     IF( I .LT. LAST ) THEN
                        CALL CHR_FANDL( TEXT( I + 1 : LAST ), N, L )
                        IF( N .LE. L ) THEN
                           N = N + I
                           IF(
     :          .NOT. ( GRP1_CHKCC( TEXT, N, DELCC, ESCCC, ESCOK ) .AND.
     :                  DELOK ) .AND.
     :          .NOT. ( GRP1_CHKCC( TEXT, N, KCLCC, ESCCC, ESCOK ) .AND.
     :                  KCLOK ) ) THEN
                              STATUS = GRP__INVEL
                              CALL MSG_SETC( 'TEXT', TEXT )
                              CALL ERR_REP( 'GRP1_PAREL_ERR8',
     :                                 'GRP1_PAREL: Text following a '//
     :                                 'valid substitution string '//
     :                                 'found in ''^TEXT''.', STATUS )
                              GO TO 999
                           END IF
                        END IF
                     END IF

                  END IF

*  If the current character is an element delimiter, indicate that a
*  new kernel expression may now be started. If the current kernel
*  nesting level is zero, indicate that more elements remain to be
*  checked, and leave the loop.
               ELSE IF( DEL ) THEN
                  KOK = .TRUE.

*  Report an error and abort if a element delimiter is found within a
*  substitution string.
                  IF( NSEPCC .NE. 0 ) THEN
                     STATUS = GRP__INVEL
                     CALL MSG_SETC( 'TEXT', TEXT )
                     CALL MSG_SETC( 'SEP', SEPCC )
                     CALL ERR_REP( 'GRP1_PAREL_ERR9', 'GRP1_PAREL: '//
     :                             'Mis-placed separator character '//
     :                             '''^SEP'' found in ''^TEXT''.',
     :                             STATUS )
                     GO TO 999
                  END IF

*  If the current kernel nesting level is zero, return the position of
*  the end of the kernel or of the suffix (depending on whether or not
*  a suffix has been started, and whether or not a substitution string
*  has been found). Also return the index of the delimiter, and leave
*  the loop.
                  IF( NESTK .EQ. 0 ) THEN

                     IF( S1 .GT. FIRST .AND. S2 .EQ. 0 ) THEN
                        S2 = I - 1
                     ELSE IF( T2 .EQ. 0 ) THEN
                        K2 = I - 1
                     END IF

                     NEXT = I
                     GO TO 999

                  END IF

               END IF

            END IF

         END DO

*  If a suffix has been started but not finished, the end of the suffix
*  is the last character.
         IF( S1 .GT. FIRST .AND. S2 .EQ. 0 ) S2 = LAST

*  If a substitution string has been started but not finished, report
*  an error and abort
         IF( NSEPCC .NE. 0 ) THEN
            STATUS = GRP__INVEL
            CALL MSG_SETC( 'TEXT', TEXT )
            CALL MSG_SETC( 'OP', OPNCC )
            CALL MSG_SETC( 'CL', CLNCC )
            CALL ERR_REP( 'GRP1_PAREL_ERR10', 'GRP1_PAREL: '//
     :                    'Incomplete substitution string found in '//
     :                    '''^TEXT''.', STATUS )
            GO TO 999
         END IF

*  Report an error and abort if the final kernel nesting level is not
*  zero.
         IF( NESTK .NE. 0 ) THEN
            STATUS = GRP__INVEL
            CALL MSG_SETC( 'TEXT', TEXT )
            CALL MSG_SETC( 'OP', KOPCC )
            CALL MSG_SETC( 'CL', KCLCC )
            CALL ERR_REP( 'GRP1_PAREL_ERR11', 'GRP1_PAREL: '//
     :                    'Un-matched delimiters ''^OP^CL'' '//
     :                    'found in ''^TEXT''.', STATUS )
            GO TO 999
         END IF

*  Report an error and abort if the final OPEN_NEST nesting level is not
*  zero.
         IF( NESTN .NE. 0 ) THEN
            STATUS = GRP__INVEL
            CALL MSG_SETC( 'TEXT', TEXT )
            CALL MSG_SETC( 'OP', OPNCC )
            CALL MSG_SETC( 'CL', CLNCC )
            CALL ERR_REP( 'GRP1_PAREL_ERR12', 'GRP1_PAREL: '//
     :                    'Un-matched delimiters ''^OP^CL'' '//
     :                    'found in ''^TEXT''.', STATUS )
            GO TO 999
         END IF

      END IF

*  Jump to here if an error occurs or if more than one element is
*  found in the supplied text.
 999  CONTINUE

*  If any of the strings are blank, return the end index less than the
*  start index.
         IF( P2 .GE. P1 ) THEN
            IF( TEXT( P1 : P2 ) .EQ. ' ' ) THEN
               P1 = 0
               P2 = -1
            END IF
         END IF

         IF( K2 .GE. K1 ) THEN
            IF( TEXT( K1 : K2 ) .EQ. ' ' ) THEN
               K1 = 0
               K2 = -1
            END IF
         END IF

         IF( S2 .GE. S1 ) THEN
            IF( TEXT( S1 : S2 ) .EQ. ' ' ) THEN
               S1 = 0
               S2 = -1
            END IF
         END IF

         IF( T2 .GE. T1 ) THEN
            IF( TEXT( T1 : T2 ) .EQ. ' ' ) THEN
               T1 = 0
               T2 = -1
            END IF
         END IF

      END
