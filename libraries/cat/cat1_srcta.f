      SUBROUTINE CAT1_SRCTA (STATUS)
*+
*  Name:
*     CAT1_SRCTA
*  Purpose:
*     Read AST data as text from a catalogue.
*  Language:
*     Starlink Fortran 77
*  Invocation:
*     CALL CAT1_SRCTA (STATUS)
*  Description:
*     This is a service routine to be provided as a "source" routine
*     for the AST_CHANNEL function. It reads data from a catalogue
*     (in response to reading from an AST Channel) and delivers it to
*     the AST library for interpretation.
*
*     This routine has only a STATUS argument, so it communicates with
*     other CAT routines via global variables stored in the CAT1_AST_CMN
*     common block.
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while (more)
*       Attempt to read a line of textual information.
*       If ok and not finished then
*         If the textual information is of class AST then
*           Set the 'found AST frame-set' flag.
*           Strip off the AST signature.
*           If the text corresponds to a new AST line then
*             If the AST line buffer is not empty then
*               Send the line to the AST library.
*               Initialise the AST line.
*             end if
*             Add the text line to the AST line.
*             Set the termination flag (because an AST line has been
*             completed).
*           else (the text line is an AST continuation line)
*             If there is sufficient space in the AST line then
*               Append the text line to the AST line.
*             else
*               Set the status.
*               Report an error.
*             end if
*           else (bad AST line)
*             Set the status.
*             Report an error.
*           end if
*         end if
*       else (failure or finished reading textual information)
*         Set the termination flag.
*         If finished then
*           If the AST line buffer is not empty then
*             Send the line to the AST library.
*           end if
*         end if
*       end if
*     end do

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (Starlink)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     8/10/99 (ACD): Original version (from KPG1_SRCTA).
*     2/11/99 (ACD): Re-written to access the catalogue directly, rather
*       than via GRP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing.
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'CAT_PAR'          ! External CAT constants.
      INCLUDE 'CAT_ERR'          ! CAT error codes.
      INCLUDE 'CAT1_PAR'         ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_AST_CMN'     ! CAT - AST common block.
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
*  Local Constants:
*     <...>
*  Local Variables:
      LOGICAL
     :  MORE,    ! Flag; more lines of catalogue textual info. to process?
     :  FINISH   ! Flag; has the last line of textual info. been processed?
      CHARACTER
     :  CLASS*10,            ! Class of current line of text.
     :  TEXT*(CAT__SZTXL),   ! Current line of text.
     :  WTEXT*(CAT__SZTXL)   ! Working copy of TEXT.
      INTEGER
     :  LASTSG,  ! Length of CAT__ASTSG (AST signature).
     :  SIGPOS,  ! Position of AST signature in the current text line.
     :  LWTEXT   ! Length of WTEXT (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Determine the length of the AST signature.

         LASTSG = CHR_LEN(CAT__ASTSG)

*
*       Read lines of textual information from the catalogue.  Reading
*       continues until either a new line of AST information is
*       encountered or all the textual information is read.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to read a line of textual information and proceed if
*          ok and all the textual information has not been read.

            CALL CAT_GETXT (CI__AST, FINISH, CLASS, TEXT, STATUS)
C           print3000, finish, class, text(1 : 50)
C3000       format(1x, 'read:', l3, 1x, a10, 1x, a50)

            IF (STATUS .EQ. SAI__OK  .AND.  .NOT. FINISH) THEN

*
*             Check whether the class is 'AST'.

               IF (CLASS .EQ. 'AST') THEN

*
*                Set the 'found AST frame-set' flag.

                  FND__AST = .TRUE.

*
*                A line of AST information has been found.  Remove
*                all characters at the start of the line up to and
*                including the identifying 'AST signature' at the start
*                of the line, to leave the genuine AST details.
*
*                Note that because the line is of class AST it must
*                include an 'AST signature'.

                  SIGPOS = INDEX(TEXT, CAT__ASTSG)

                  WTEXT = ' '
                  WTEXT = TEXT(SIGPOS+LASTSG : CAT__SZTXL)

*
*                Check whether the text line corresponds to a new AST
*                line or is a continuation of the previous AST line.

                  IF (WTEXT(1 : 1) .EQ. ' ') THEN

*
*                   A new AST line.  First, if the existing AST line
*                   buffer is not empty then send it to the AST library
*                   for interpretation.  Then re-initialise the AST line.

                     IF (LLINE__AST .GT. 0) THEN
C                       print3001, lline__ast, line__ast(1 : 75)
C3001                   format(1x, 'line sent to AST,  characters = ',
C    :                    i4 / 1x, a75)

                        CALL AST_PUTLINE (LINE__AST, LLINE__AST, STATUS)

                        LINE__AST = ' '
                        LLINE__AST = 0
                     END IF

*
*                   Add the text line to the AST line.

                     IF (WTEXT .NE. ' ') THEN
                        LWTEXT = CHR_LEN(WTEXT)

                        CALL CHR_PUTC (WTEXT(1 : LWTEXT), LINE__AST,
     :                    LLINE__AST)
                     END IF

*
*                   Set the termination flag (because an AST line has been
*                   processed).

                     MORE = .FALSE.

                  ELSE IF (WTEXT(1 : 1) .EQ. '+') THEN
C                    print3002
C3002                format(1x, 'continuation line encountered.',
C    :                 '******************************')

*
*                   The text line is an AST continuation line.  Append
*                   it to the AST line buffer if there is space; otherwise
*                   set the status and report an error.

                     WTEXT(1 : 1) = ' '
                     CALL CHR_LDBLK (WTEXT)

                     IF (WTEXT .NE. ' ') THEN
                        LWTEXT = CHR_LEN(TEXT)

                        IF (LWTEXT .LE. AST__SZLNE + 1 - LLINE__AST)
     :                    THEN
                           CALL CHR_PUTC (WTEXT(1 : LWTEXT),
     :                       LINE__AST, LLINE__AST)

                        ELSE
                           STATUS = CAT__INVAS

                           CALL MSG_SETI ('SZLNE', AST__SZLNE)
                           CALL ERR_REP ('CAT1_SRCTA_CON',
     :                       'Too many input AST continuation '/
     :                       /'lines; internal buffer length of '/
     :                       /'^SZLNE characters exceeded.',
     :                       STATUS)

                        END IF
                     END IF

                  ELSE

*
*                   A bad AST line has been encountered; one which is
*                   neither a new line nor a continuatuin line.  Set the
*                   status and report an error.

                     STATUS = CAT__INVAS

                     IF (WTEXT .NE. ' ') THEN
                        LWTEXT = CHR_LEN(WTEXT)
                        LWTEXT = MIN(LWTEXT, 60)
                     ELSE
                        LWTEXT = 1
                     END IF

                     CALL MSG_SETC ('WTEXT', WTEXT(1 : LWTEXT) )
                     CALL ERR_REP ('CAT1_SRCTA_BAD',
     :                 'Bad AST line: ^WTEXT', STATUS)

                  END IF
               END IF

            ELSE

*
*             Failed to read a line of textual information (either
*             because input is complete or an error occurred); set the
*             termination flag.

               MORE = .FALSE.

*
*             If all the textual information has been read and the
*             AST line is not empty then send the final AST line to
*             the AST library for interpretation.
*
*             If all the textual information was read without finding
*             an AST frame-set then the line sent to AST is set to
*             blank, so that AST can recognise that input has terminated.

               IF (FINISH) THEN

                  IF (.NOT. FND__AST) THEN
                     LINE__AST = ' '
                     LLINE__AST = 0
                  END IF

                  IF (LLINE__AST .GT. 0) THEN
c                     print3003, lline__ast, line__ast(1 : 75)
c3003                 format(1x, 'final line sent to AST,  ',
c    :                  'characters = ', i4 / 1x, a75)
                     CALL AST_PUTLINE (LINE__AST, LLINE__AST, STATUS)
                  END IF
               END IF
            END IF

         END DO

      END IF

      END
