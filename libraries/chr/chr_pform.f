      SUBROUTINE CHR_PFORM( MXPAR, PARRAY, JUSTFY, IPOSN, STRING )
*+
*  Name:
*     CHR_PFORM

*  Purpose:
*     Reformat a paragraph to a new width.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_PFORM( MXPAR, PARRAY, JUSTFY, IPOSN, STRING )

*  Description:
*     This subroutine is called repeatedly to reformat the given
*     paragraph to a new width (given by the declared length of the
*     returned character variable). The output may be optionally
*     justified to the right margin (i.e. the end of the returned
*     character variable). This routine should be called repeatedly
*     to generate successive returned lines from the given paragraph
*     array. Initially, the context argument IPOSN should be set to zero;
*     it will be updated after each call, ready to generate the next
*     output line. A value of zero is returned for IPOSN when there are
*     no more lines to return. Any unprintable characters (e.g. tabs)
*     are treated as if they were spaces for the purpose of generating
*     line-breaks.

*  Arguments:
*     MXPAR = INTEGER (Given)
*        The maximum length of the given paragraph array, PARRAY.
*     PARRAY( MXPAR ) = CHARACTER * ( * ) (Given)
*        The character array which contains the paragraph text to be
*        reformatted, one line per array element. Leading blanks are
*        ignored. A line-break is interpreted as the start of a new
*        word.
*     JUSTFY = LOGICAL (Given)
*        The right justification flag: if this is given as .TRUE.,
*        the text is returned right justified; otherwise the text is
*        returned with a ragged right margin.
*     IPOSN = INTEGER (Given and Returned)
*        On entry, this argument specifies the character position in
*        PARRAY from which to start generating the next returned line.
*        It is given as the number of characters from the start of the
*        first character in the first element in PARRAY. If a value less
*        than 1 is used, then 1 will be used.
*
*        On exit, this argument is set to one more than the character
*        offset of the start of PARRAY of the last non-blank character
*        which appears in the returned line STRING (i.e. the position
*        at which the generation of the next output line should start).
*        If STRING is blank because there are no more characters to
*        process, then IPOSN is returned set to zero.
*     STRING = CHARACTER * ( * ) (Returned)
*        The returned line of text in the paragraph, left justified. The
*        length of this argument defines the maximum length of the
*        returned paragraph line.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     ACC:  A.C. Charles (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-APR-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     12-AUG-2004 (TIMJ):
*        Initialise variables that were generating warnings
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER MXPAR

      CHARACTER * ( * ) PARRAY( MXPAR )

      LOGICAL JUSTFY

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Arguments Returned:
      CHARACTER * ( * ) STRING

*  Local Variables:
      LOGICAL ISEND              ! Whether at the end of the output line
      LOGICAL ISGAP              ! Whether in an inter-word gap
      LOGICAL ISPACE             ! Whether a redundant space

      INTEGER ICH                ! Character loop index
      INTEGER ICHP               ! Character loop index for STRING
      INTEGER ICSTRT             ! Character index for STRING start
      INTEGER ICWEND             ! Character index for last word end
      INTEGER ILN                ! Line loop index
      INTEGER ILNP               ! Line loop index for STRING
      INTEGER ILSTRT             ! Line index for STRING start
      INTEGER ILWEND             ! Line index for last word end
      INTEGER IPLEN              ! Declared length of a line in PARRAY
      INTEGER NGAP               ! Number of gaps in line of paragraph
      INTEGER NPC                ! PARRAY character pointer
      INTEGER NPCH               ! No. characters copied into STRING
      INTEGER NPCHLW             ! NPCH to the end of the last word
      INTEGER NPL                ! PARRAY line pointer
      INTEGER OPLEN              ! Declared length of the output line

      CHARACTER * 1 CVALUE       ! Element of PARRAY

*.

*  Initialise variables that might possibly be used
*  without being intialised otherwise
      ICH = 0
      ICWEND = 0
      ILNP = 0
      ILWEND = 0
      IPLEN = 0
      NPC = 0
      NPCHLW = 0
      NPL = 0
      OPLEN = 0

*  Check the length of the given paragraph array.
      IF ( MXPAR .GT. 0 ) THEN

*     Get the declared lengths of the input and output character
*     variables.
         IPLEN = LEN( PARRAY( 1 ) )
         OPLEN = LEN( STRING )

*     If the starting position does not lie beyond the end of the input
*     paragraph, then there is potentially some output.
         IF ( IPOSN .LE. IPLEN*MXPAR ) THEN

*        If the starting position is before the beginning of the string,
*        then advance it to the first character position.
            IF ( IPOSN .LT. 1 ) THEN
               IPOSN = 1
            END IF

*        Get the current line and character pointers.
            NPC = MOD( IPOSN-1, IPLEN ) + 1
            NPL = ( IPOSN-1 ) / IPLEN + 1

*        Search for the beginning of the next word: i.e. the next
*        non-blank character in PARRAY.
            ICSTRT = NPC
            ILSTRT = NPL

            DO 20 ILN = ILSTRT, MXPAR

               DO 10 ICH = ICSTRT, IPLEN

*              Get the next character from the paragraph array.
                  CVALUE = PARRAY( ILN )( ICH : ICH )

*              Clean the character: i.e. convert any unprintable
*              characters to blanks.
                  CALL CHR_CLEAN( CVALUE )

*              If the character is not a blank, then exit the loop.
                  IF ( CVALUE .NE. ' ' ) THEN
                     GO TO 30
                  END IF
 10            CONTINUE

*           Update ICSTRT to begin all lines but the first (i.e. ILSTRT)
*           at the first character.
               ICSTRT = 1
 20         CONTINUE
 30         CONTINUE

*        Set the initial paragraph loop indices to point to the
*        beginning of the first word.
            ICSTRT = ICH
            ILSTRT = ILN

*        Initialise the character and word gap counters.
            NPCH = 0
            NPCHLW = 1
            NGAP = 0
            ILNP = ILSTRT

*        Initialise the word gap and output flags.
            ISGAP = .FALSE.
            ISEND = .FALSE.
            ISPACE = .FALSE.

*        Loop to perform the load of the output paragraph line.
*        DO WHILE loop.
 40         CONTINUE
            IF ( ( .NOT. ISEND ) .AND. ( ILNP .LE. MXPAR ) ) THEN

               DO 50 ICHP = ICSTRT, IPLEN

*              Get the next character from PARRAY and convert any
*              unprintable characters to blanks.
                  CVALUE = PARRAY( ILNP )( ICHP : ICHP )
                  CALL CHR_CLEAN( CVALUE )

*              If the current character is a blank, then check if the
*              previous character was also blank: if so, discard it;
*              else mark the end of the last word.
                  IF ( CVALUE .EQ. ' ' ) THEN
                     IF ( .NOT. ISGAP ) THEN

*                    The end of a word, so update the character and gap
*                    counters.
                        NPCH = NPCH + 1
                        NGAP = NGAP + 1
                        ISGAP = .TRUE.
                     ELSE

*                    Redundant blank space.
                        ISPACE = .TRUE.
                     END IF
                  ELSE

*                 Part of a word, so update the character pointer.
                     NPCH = NPCH + 1
                     ISGAP = .FALSE.
                     ISPACE = .FALSE.
                  END IF

*              Check for redundant spaces.
                  IF ( .NOT. ISPACE ) THEN

*                 Check if the character marks the end of a word.
                     IF ( ISGAP ) THEN

*                    Update the pointers to the end of the last word.
                        ICWEND = ICHP - 1
                        ILWEND = ILNP
                        NPCHLW = NPCH
                     END IF

*                 Check the number of characters copied to STRING.
                     IF ( NPCH .LE. OPLEN ) THEN

*                    Add the character to STRING.
                        STRING( NPCH : NPCH ) = CVALUE
                     ELSE

*                    The end of the output line has been reached, so check
*                    if there are any word-breaks in STRING.
                        IF ( NGAP .GT. 0 ) THEN

*                       There are word-breaksin the string, so update
*                       the values of NPL, NPC to point to beyond the end
*                       of the last word.
                           NPC = ICWEND + 1
                           NPL = ILWEND
                        ELSE

*                       There has been no word-break in STRING, so update
*                       NPL, NPC to point to the next character.
                           NPC = ICHP
                           NPL = ILNP
                           NPCHLW = OPLEN
                        END IF

*                    Abort the loop.
                        ISEND = .TRUE.
                        GO TO 60
                     END IF
                  END IF
 50            CONTINUE
 60            CONTINUE

*           Check if the end of the output line has been reached.
               IF ( .NOT. ISEND ) THEN

*              The end of the current input line has been reached, reset
*              ICSTRT to the beginning of a line.
                  ICSTRT = 1

*              Treat the line break as the start of a new word.
                  CVALUE = ' '

                  IF ( .NOT. ISGAP ) THEN
                     NPCH = NPCH + 1
                     NGAP = NGAP + 1
                     ISGAP = .TRUE.
                  ELSE
                     ISPACE = .TRUE.
                  END IF

*              Check for redundant spaces.
                  IF ( .NOT. ISPACE ) THEN

*                 Check if the character marks the end of a word.
                     IF ( ISGAP ) THEN

*                    Update the pointers to the end of the last word.
                        ICWEND = ICHP - 1
                        ILWEND = ILNP
                        NPCHLW = NPCH
                     END IF

*                 Check the number of characters copied to STRING.
                     IF ( NPCH .LE. OPLEN ) THEN

*                    Add the character to STRING.
                        STRING( NPCH : NPCH ) = CVALUE
                     ELSE

*                    Update the values of NPL, NPC to point to the end
*                    of the last word.
                        NPC = ICWEND + 1
                        NPL = ILWEND
                     END IF
                  END IF

*              Increment the line index.
                  ILNP = ILNP + 1
               END IF
            GO TO 40
            END IF
         END IF
      END IF

*  Finish off line.
      IF ( NPCHLW .LT. OPLEN ) STRING( NPCHLW : ) = ' '

*  If the line is not the last line in the paragraph, right-justify it
*  and return updated value of IPOSN; else reset IPOSN.
      IF ( ILNP .LE. MXPAR ) THEN
         IF ( JUSTFY ) CALL CHR_RJUST( STRING )
         IPOSN = ( NPL-1 ) * IPLEN + NPC
      ELSE
         IPOSN = 0
      END IF

      END
