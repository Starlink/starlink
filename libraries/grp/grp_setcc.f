      SUBROUTINE GRP_SETCC( IGRP, CCLIST, CC, STATUS )
*+
*  Name:
*     GRP_SETCC

*  Purpose:
*     Sets requested control characters for the specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SETCC( IGRP, CCLIST, CC, STATUS )

*  Description:
*     Each group has associated with it several "control characters"
*     which are the characters used to indicate various items of syntax
*     within a group expression. These characters are listed and
*     described in the "Notes" section below. The control characters are
*     given default values when the group is created, but can be changed
*     at any time by calling this routine.
*
*     Checks for particular control characters may be suppressed by
*     assigning the NULL character to them. The NULL character is
*     itself a control character which may be assigned a value using
*     this routine. Some control characters form pairs, and an error is
*     reported if only one member of a pair is assigned the NULL value.
*     These pairs are OPEN_NEST and CLOSE_NEST, and OPEN_KERNEL and
*     CLOSE_KERNEL.
*
*     If a blank value for argument CCLIST is supplied, then the default
*     control characters described in the "Notes" section are
*     re-established.
*
*     An error is reported if any two control characters are the same.
*     The exception to this is that any number of control characters
*     may have the same value as the NULL control character. If any
*     error occurs, the control characters are left unaltered.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group to which the control characters
*        refer.
*     CCLIST = CHARACTER * ( * ) (Given)
*        A comma separated list of names specifying which control
*        character are to be altered. Unambiguous abbreviations may be
*        used. A blank value causes all control characters to be reset
*        to the default values.
*     CC = CHARACTER * ( * ) (Given)
*        A list of the new control characters, in the same order as the
*        corresponding names in CCLIST. Note, if CCLIST contains N
*        names, then the first N characters are used out of the string
*        specified by CC. If the total length (including any trailing
*        blanks) of CC is less than N, then an error is reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The following names are used to refer to the individual control
*     characters:
*     -  INDIRECTION: (Default "^") The name given to the character used
*     to indicate that an element of a group expression is the name of a
*     text file from which further elements should be read.
*     -  COMMENT: (Default "#") The name given to the character used to
*     introduce comments in group expressions.
*     -  DELIMITER: (Default ",") The name given to the character used
*     to delimit elements within group expressions. Note, delimiters
*     within group expressions are ignored if they occur within matched
*     nesting characters (see OPEN_NEST and CLOSE_NEST below).
*     -  NAME_TOKEN: (Default "*") The name given to the character used
*     as a token for input names in a modification element.
*     -  SEPARATOR: (Default "|") The name given to the character used
*     to separate the substitution strings within a modification
*     element.
*     -  OPEN_NEST: (Default "(") The name given to the character used
*     to open a "nest" within a group expression. Any delimiter
*     characters occurring within matched nesting characters are ignored.
*     -  CLOSE_NEST: (Default ")") The name given to the character used
*     to close a "nest" within a group expression.
*     -  FLAG: (Default "-") The name given to a character which can
*     be appended to the end of a group expression in order to
*     "flag" that expression. The interpretation of this flag is left
*     up to the application.
*     - OPEN_KERNEL: (Default "{") The name given to the character used
*     to open a "kernel" within a group expression.
*     - CLOSE_KERNEL: (Default "}") The name given to the character used
*     to close a "kernel" within a group expression.
*     -  NULL: (Default "%") The name given to the character which can
*     be assigned to other control characters to suppress checks for
*     those control characters. If this is changed, any other characters
*     currently set to the null character are also changed to the new NULL
*     character.
*     - ESCAPE: (Default to the NULL character) The name given to the
*     character which can be used to escape control characters within
*     a group expression.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     18-AUG-1992 (DSB):
*        Original version
*     26-JAN-1994 (DSB):
*        OPEN_KERNEL and CLOSE_KERNEL added.
*     27-AUG-1999 (DSB):
*        Added ESCAPE to prologue. Update any CCs which are set to the
*        null character if the null character is changed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CHARS( GRP__MAXG ) = CHARACTER*(GRP__NCHAR) (Read and Write)
*           The control characters used to define the syntax of group
*           expressions. A set of characters stored in a single string
*           for each group.
*        CMN_CCNAM( GRP__NCHAR ) = CHARACTER (Read)
*           The names used to identify the control characters stored in
*           CMN_CHARS, in the same order as the characters in CMN_CHARS.

*  Arguments Given:
      INTEGER IGRP
      CHARACTER CCLIST*(*)
      CHARACTER CC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Returns used length of a string.

      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.


*  Local Variables:
      INTEGER CCLEN              ! Total length of CC.
      CHARACTER CHARS*(GRP__NCHAR)! Local copy of groups control
                                 ! characters.
      LOGICAL CLNULL             ! True if CLOSE_NEST is set to NULL.
      INTEGER END                ! Position of last non-blank character
                                 ! in the current word.
      INTEGER I                  ! Offset into string holding all
                                 ! control characters.
      INTEGER J                  ! Offset into string holding all
                                 ! control characters.
      LOGICAL MORE               ! True if more words remain to be
                                 ! processed in CCLIST.
      INTEGER NCC                ! Index within CC of the next control
                                 ! character to be used.
      INTEGER NMATCH             ! No. of matches between defined
                                 ! control character names and the
                                 ! current word.
      CHARACTER NULL0*1          ! The original NULL control character.
      CHARACTER NULLCC*1         ! The NULL control character.
      LOGICAL OPNULL             ! True if OPEN_NEST is set to NULL.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
      INTEGER START              ! Position of the first non-blank
                                 ! character in the current word.
      CHARACTER TESTCC*1         ! The control character being tested.
      INTEGER ULEN               ! Used length of CCLIST.
      INTEGER WLEN               ! Used length of the current word.
      CHARACTER WORD*(GRP__SZWRD)! Current word.
      INTEGER WRDBEG             ! Start of current word.
      INTEGER WRDEND             ! End of current word.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If a blank value is supplied for CCLIST, re-establish the default
*  control characters.
      IF( CCLIST .EQ. ' ' ) THEN
         CMN_CHARS( SLOT ) = GRP__DEFCC

*  Otherwise, take a local copy of the current default control
*  characters.
      ELSE
         CHARS = CMN_CHARS( SLOT )

*  Store the original value of the NULL control character in a local
*  variable.
         NULL0 = CHARS( GRP__PNULC : GRP__PNULC )

*  Find the used length of the control character list.
         ULEN = CHR_LEN( CCLIST )

*  Find the total length of the string holding the new control
*  characters.
         CCLEN = LEN( CC )

*  Initialise the begining of the next word to the first character.
         WRDBEG = 1

*  Initialise the index (within CC) of the next control character to be
*  set.
         NCC = 1

*  Loop round each word in the list of control characters names.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Find the end of the next word. First look for a comma.
            WRDEND = INDEX( CCLIST( WRDBEG : ), ',' ) + WRDBEG - 2

*  If no comma remains, use the end of the string.
            IF( WRDEND .EQ. WRDBEG - 2 ) WRDEND = ULEN

*  Find the next word, and store its used length.
            CALL CHR_FANDL( CCLIST( WRDBEG : WRDEND), START, END )
            WLEN = END - START + 1

*  If a word was found, extract it and convert it to upper case so long
*  as another control character has been supplied.
            IF( END .GT. 0 ) THEN
               IF( NCC .LE. CCLEN ) THEN
                  WORD = CCLIST( WRDBEG + START - 1: WRDBEG + END - 1 )
                  CALL CHR_UCASE( WORD )

*  Store the corresponding control character. The names associated with
*  each control character are held in common array CMN_CCNAM.  They
*  are stored at offsets within CHARS corresponding to their indices
*  within CMN_CCNAM.  Keep a count of the number of matches found
*  between the word and the names of the control characters.
                  NMATCH = 0
                  DO I = 1, GRP__NCHAR
                     IF( INDEX( CMN_CCNAM( I ), WORD( :WLEN ) )
     :                                                     .EQ. 1 ) THEN
                        CHARS( I : I ) = CC( NCC : NCC )
                        NMATCH = NMATCH + 1

                     END IF
                  END DO

*  If no matches were found, report an error.
                  IF( NMATCH .EQ. 0 ) THEN
                     STATUS = GRP__BADCC
                     CALL MSG_SETC( 'W', WORD )
                     CALL ERR_REP( 'GRP_SETCC_ERR1',
     :                'GRP_SETCC: Unknown control character name, ^W.',
     :                          STATUS )

*  If more than one match was found, report an error.
                  ELSE IF( NMATCH .GT. 1 ) THEN
                     STATUS = GRP__BADCC
                     CALL MSG_SETC( 'W', WORD )
                     CALL ERR_REP( 'GRP_SETCC_ERR2',
     :               'GRP_SETCC: Ambiguous control character name, ^W.',
     :                          STATUS )

*  If exactly one match was found, increment the number of control
*  characters used so far, and set the start of the next word.
                  ELSE
                     NCC = NCC + 1
                     WRDBEG = WRDEND + 2

*  If the start of the next word is beyond the end of CCLIST, indicate
*  that no more words remain.
                     IF( WRDBEG .GT. ULEN ) MORE = .FALSE.

                  END IF

*  If there are insufficient control characters supplied in CC, report
*  an error.
               ELSE
                  STATUS = GRP__SHORT
                  CALL MSG_SETI( 'L', CCLEN )
                  CALL ERR_REP( 'GRP_SETCC_ERR3',
     :   'GRP_SETCC: Length of supplied argument CC (^L) is too small.',
     :                       STATUS )
               END IF

*  If no more words were found in CCLIST, indicate that the job is
*  complete.
            ELSE
               MORE = .FALSE.

            END IF

         END DO

         IF( STATUS. EQ. SAI__OK ) THEN

*  Store the value of the NULL control character in a local variable for
*  fast access.
            NULLCC = CHARS( GRP__PNULC : GRP__PNULC )

*  If the NULL character has changed, update any other characters which
*  were previously set to the null character so that they are equal to
*  the new NULL character. Do not update them if they have explicitly
*  been assigned a new value.
            IF( NULLCC .NE. NULL0 ) THEN
               DO I = 1, GRP__NCHAR - 1
                  TESTCC = CHARS( I : I )
                  IF( CMN_CHARS( SLOT )( I : I ) .EQ. TESTCC .AND.
     :                TESTCC .EQ. NULL0 ) CHARS( I : I ) = NULLCC
               END DO
            END IF

*  Check that no two control characters are the same. If so, report an
*  error. Note, any control character may have the same value as the
*  NULL control character.
            DO I = 1, GRP__NCHAR - 1
               TESTCC = CHARS( I : I )

*  Only bother checking this character any further if it is not equal to
*  the NULL control character.
               IF( TESTCC .NE. NULLCC ) THEN

*  Compare it against every other remaining control character.
                  DO J = I + 1, GRP__NCHAR

*  If they are equal, report an error.
                     IF( TESTCC .EQ. CHARS( J : J ) ) THEN
                        STATUS = GRP__BADCC
                        CALL MSG_SETC( 'C', TESTCC )
                        CALL ERR_REP( 'GRP_SETCC_ERR4',
     :'GRP_SETCC: "^C" used for more than one control character',
     :                          STATUS )
                        GO TO 999
                     END IF

                  END DO

               END IF

            END DO

*  Check that if one of OPEN_NEST and CLOSE_NEST has been assigned a
*  NULL value, then the other one has also been assigned a NULL value.
            OPNULL = CHARS( GRP__POPNC : GRP__POPNC ) .EQ. NULLCC
            CLNULL = CHARS( GRP__PCLNC : GRP__PCLNC ) .EQ. NULLCC

            IF( OPNULL .AND. .NOT. CLNULL ) THEN
               STATUS = GRP__BADCC
               CALL ERR_REP( 'GRP_SETCC_ERR5',
     :       'GRP_SETCC: Control character OPEN_NEST is set null but '//
     :       'CLOSE_NEST is not.', STATUS )
               GO TO 999

            ELSE IF( CLNULL .AND. .NOT. OPNULL ) THEN
               STATUS = GRP__BADCC
               CALL ERR_REP( 'GRP_SETCC_ERR6',
     :      'GRP_SETCC: Control character CLOSE_NEST is set null but '//
     :      'OPEN_NEST is not.', STATUS )
               GO TO 999

            END IF

*  Check that if one of OPEN_KERNEL and CLOSE_KERNEL has been assigned
*  a NULL value, then the other one has also been assigned a NULL
*  value.
            OPNULL = CHARS( GRP__POPKC : GRP__POPKC ) .EQ. NULLCC
            CLNULL = CHARS( GRP__PCLKC : GRP__PCLKC ) .EQ. NULLCC

            IF( OPNULL .AND. .NOT. CLNULL ) THEN
               STATUS = GRP__BADCC
               CALL ERR_REP( 'GRP_SETCC_ERR7',
     :       'GRP_SETCC: Control character OPEN_KERNEL is set null '//
     :       'but CLOSE_KERNEL is not.', STATUS )
               GO TO 999

            ELSE IF( CLNULL .AND. .NOT. OPNULL ) THEN
               STATUS = GRP__BADCC
               CALL ERR_REP( 'GRP_SETCC_ERR8',
     :      'GRP_SETCC: Control character CLOSE_KERNEL is set null '//
     :      'but OPEN_KERNEL is not.', STATUS )
               GO TO 999

            END IF

*  Store the new control characters in common.
            CMN_CHARS( SLOT ) = CHARS

         END IF

      END IF

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'CL', CCLIST )
         CALL ERR_REP( 'GRP_SETCC_ERR9',
     :  'GRP_SETCC: Unable to set control characters; ^CL.',
     :                 STATUS )
      END IF

      END
