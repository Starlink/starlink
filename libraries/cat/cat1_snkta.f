      SUBROUTINE CAT1_SNKTA (STATUS)
*+
*  Name:
*     CAT1_SNKTA
*  Purpose:
*     Write AST data as text to a catalogue.
*  Language:
*     Starlink Fortran 77
*  Invocation:
*     CALL CAT1_SNKTA (STATUS)
*  Description:
*     This is a service routine to be provided as a "sink" routine for
*     the AST_CHANNEL function. It takes data in the form of text (in
*     response to writing an AST object to a Channel) and delivers it
*     to a catalogue for storage.
*
*     This routine has only a STATUS argument, so it communicates with
*     other CAT routines via global variables stored in the CAT1_AST_CMN
*     common block.
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get a line of AST information.
*     If ok then
*       If the line is not blank then
*         Copy the AST line to a local work buffer.
*         If the copied string is not blank then
*           Remove leading blanks.
*           Find the length of the remaining string.
*           Determine the number of text lines that the AST line must
*           be split up into.
*           For every text line
*             Compute the start and stop points in the AST line.
*             Assemble the text line.
*             Write the text line to the catalogue.
*           end for
*         end if
*       end if
*     end if

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
*     DSB: David S. Berry (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/10/99 (ACD): Original version (from KPG1_SNKTA).
*     3/11/99  (ACD): Re-written to access the catalogue directly, rather
*       than via GRP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! External CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_AST_CMN'     ! CAT - AST common block.
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
*  Local Variables:
      CHARACTER
     :  ASTLN*(AST__SZLNE),  ! Current line of AST information.
     :  WORK*(AST__SZLNE),   ! Local copy of current AST line.
     :  TEXT*(CAT__SZTXL)    ! Current line of textual information.
      INTEGER
     :  LASTLN,   ! Length of ASTLN (excl. trail. blanks).
     :  LWORK,    !   "    "  WORK  ( "  .   "  .   "   ).
     :  LTEXT,    !   "    "  TEXT  ( "  .   "  .   "   ).
     :  REM,      ! Remaining characters whilst finding no. of lines.
     :  NTXLNE,   ! Number of text line corresponding to the AST line.
     :  CTXLNE,   ! Current text line.
     :  START,    ! Start position of current text line in AST line.
     :  STOP      ! Stop     "     "     "     "    "   "   "   "  .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to get a line of AST information.  Proceed if all is
*       ok and the line is not blank.  Note that returned string
*       ASTLN is not necessarily padded with spaces beyond its last valid
*       character, LASTLN.

         CALL AST_GETLINE (ASTLN, LASTLN, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (LASTLN .GT. 0) THEN

*
*             Copy the AST line to a local work buffer.

               WORK = ' '
               WORK = ASTLN(1 : LASTLN)

*
*             Check that the string copied is not blank.

               IF (WORK .NE. ' ') THEN

*
*                Remove any leading blanks.  This contraction removes
*                the indentation, which is intended for human readers,
*                and may reduce the number of continuation lines required.

                  CALL CHR_LDBLK (WORK)

*
*                Find the length of the remaining line.

                  LWORK = CHR_LEN(WORK)

*
*                Determine the number of text lines that the AST line must
*                be split up into.

                  NTXLNE = LWORK / TLNSZ__AST
                  REM = MOD(LWORK, TLNSZ__AST)
                  IF (REM .GT. 0) THEN
                     NTXLNE = NTXLNE + 1
                  END IF

*
*                Write each text line.  The first line corresponds to a new
*                AST line, the rest are continuation lines.  For new lines
*                a space immediately follows the AST signature.  For
*                continuations a '+' immediately follows the signature.

                  DO CTXLNE = 1, NTXLNE

*
*                   Compute the start and stop positions of the current
*                   text line in the AST line.

                     START = 1 + (TLNSZ__AST * (CTXLNE - 1 ) )
                     STOP = START + TLNSZ__AST - 1
                     STOP = MIN(STOP, LWORK)

*
*                   Assemble the text line.

                     TEXT = ' '
                     LTEXT = 0

                     CALL CHR_PUTC (CAT__ASTSG, TEXT, LTEXT)

                     IF (CTXLNE .EQ. 1) THEN
                        CALL CHR_PUTC (' ', TEXT, LTEXT)
                     ELSE
                        CALL CHR_PUTC ('+', TEXT, LTEXT)
                     END IF

                     CALL CHR_PUTC (WORK(START : STOP), TEXT, LTEXT)

*
*                   Write the text line to the catalogue.

                     CALL CAT_PUTXT (CI__AST, 'COMMENT',
     :                 TEXT(1 : LTEXT), STATUS)

                  END DO

               END IF
            END IF
         END IF

      END IF

      END
