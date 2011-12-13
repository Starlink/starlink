      SUBROUTINE GRP_GETCC( IGRP, CCLIST, CC, STATUS )
*+
*  Name:
*     GRP_GETCC

*  Purpose:
*     Returns requested control characters for the specified group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GETCC( IGRP, CCLIST, CC, STATUS )

*  Description:
*     Each group has associated with it several "control characters"
*     which are the characters used to indicate various items of syntax
*     within a group expression. These control characters can be
*     changed at any time by calling GRP_SETCC. This routine returns
*     the current values of a list of these control character.  The
*     individual characters are described in GRP__SETCC.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group for which the control characters
*        are required.
*     CCLIST = CHARACTER * ( * ) (Given)
*        A comma separated list of control character names to be
*        returned. See routine GRP_SETCC for a description of these
*        names.
*     CC = CHARACTER * ( * ) (Returned)
*        A character variable to receive the requested list of control
*        characters. The control characters are stored at adjacent
*        indices within this character variable, starting at index 1.
*        The characters are stored in the same order that they are
*        specified in CCLIST. An error is reported if the character
*        variable is not long enough to receive all the requested
*        control characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     {enter_changes_here}

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
*        CMN_CCNAM( GRP__NCHAR ) = CHARACTER (Read)
*           The names used to identify the control characters stored in
*           CMN_CHARS, in the same order as the characters in CMN_CHARS.

*  Arguments Given:
      INTEGER IGRP
      CHARACTER CCLIST*(*)

*  Arguments Returned:
      CHARACTER CC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      INTEGER CCLEN              ! Total length of CC.
      INTEGER END                ! Position of last non-blank character
                                 ! in the current word.
      INTEGER I                  ! Loop count.
      LOGICAL MORE               ! True if more words remain to be
                                 ! processed in CCLIST.
      INTEGER NCC                ! Index within CC of the next control
                                 ! character to be returned.
      INTEGER NMATCH             ! No. of matches between defined
                                 ! control character names and the
                                 ! current word.
      LOGICAL OK                 ! .TRUE. if the control character is
                                 ! not equal to the NULL character.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
      INTEGER START              ! Position of the first non-blank
                                 ! character in the current word.
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

*  Find the used length of the control character list.
      ULEN = CHR_LEN( CCLIST )

*  Find the total length of the returned string.
      CCLEN = LEN( CC )

*  Initialise the begining of the next word to the first character.
      WRDBEG = 1

*  Initialise the index (within CC) of the next control character to be
*  returned.
      NCC = 1

*  Loop round each name in the list of control characters.
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
*  as there is room for another control character in the returned
*  string.
         IF( END .GT. 0 ) THEN
            IF( NCC .LE. CCLEN ) THEN
               WORD = CCLIST( WRDBEG + START - 1: WRDBEG + END - 1 )
               CALL CHR_UCASE( WORD )

*  Add the corresponding control character to the returned string. The
*  names associated with each control character are held in the common
*  array CMN_CCNAM. Keep a count of the number of matches found between
*  the word and the names of the control characters.
               NMATCH = 0
               DO I = 1, GRP__NCHAR
                  IF( INDEX( CMN_CCNAM( I ), WORD( :WLEN ) )
     :                                                     .EQ. 1 ) THEN
                     CALL GRP1_CONC( SLOT, I, CC( NCC : NCC ), OK,
     :                               STATUS )

*  Do not return the default internal escape character to the user.
                     IF( NCC .EQ. GRP__PESCC .AND. OK .AND.
     :                   CC( NCC : NCC ) .EQ. CHAR( GRP__DFESC ) ) THEN
                        CALL GRP1_CONC( SLOT, GRP__PNULC,
     :                                  CC( NCC : NCC ), OK, STATUS )
                        OK = .FALSE.
                     END IF

                     NMATCH = NMATCH + 1
                  END IF
               END DO

*  If no matches were found, report an error.
               IF( NMATCH .EQ. 0 ) THEN
                  STATUS = GRP__BADCC
                  CALL MSG_SETC( 'W', WORD )
                  CALL ERR_REP( 'GRP_GETCC_ERR1',
     :                'GRP_GETCC: Unknown control character name, ^W.',
     :                          STATUS )

*  If more than one match was found, report an error.
               ELSE IF( NMATCH .GT. 1 ) THEN
                  STATUS = GRP__BADCC
                  CALL MSG_SETC( 'W', WORD )
                  CALL ERR_REP( 'GRP_GETCC_ERR2',
     :               'GRP_GETCC: Ambiguous control character name, ^W.',
     :                          STATUS )

*  If exactly one match was found, increment the number of control
*  characters found so far, and set the start of the next word.
               ELSE
                  NCC = NCC + 1
                  WRDBEG = WRDEND + 2

*  If the start of the next word is beyond the end of CCLIST, indicate
*  that no more words remain.
                  IF( WRDBEG .GT. ULEN ) MORE = .FALSE.

               END IF

*  If there is no room for any more control characters in CC, report an
*  error.
            ELSE
               STATUS = GRP__SHORT
               CALL MSG_SETI( 'L', CCLEN )
               CALL ERR_REP( 'GRP_GETCC_ERR3',
     :   'GRP_GETCC: Length of supplied argument CC (^L) is too small.',
     :                       STATUS )
            END IF

*  If no more words were found in CCLIST, indicate that the job is
*  complete.
         ELSE
            MORE = .FALSE.

         END IF

      END DO

*  If an error occurred, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'CL', CCLIST )
         CALL ERR_REP( 'GRP_GETCC_ERR4',
     :  'GRP_GETCC: Unable to return list of control characters; ^CL.',
     :                 STATUS )
      END IF

      END
