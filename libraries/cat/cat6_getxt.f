      SUBROUTINE CAT6_GETXT (CI, FINISH, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT6_GETXT
*  Purpose:
*     Get the next line of textual information from a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GETXT (CI; FINISH, CLASS, TEXT; STATUS)
*  Description:
*     Get the next line of textual information from a tab-separated table.
*  Arguments:
*     CI  =  INTEGER (Given)
*         Catalogue identifier.
*     FINISH   =  LOGICAL (Returned)
*         Flag indicating whether a line of text was obtained, coded as
*         follows:
*         .FALSE. -  a line was obtained ok; input of of textual
*                    information continues.
*         .TRUE.  -  all the textual information has already been
*                    returned; input of textual information has
*                    terminated.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  A set of values are
*         permitted for each type of catalogue back-end (or file
*         format), and these sets are different for different back-ends.
*         Note that this argument is returned rather than given; an
*         application cannot prescribe to CAT what class of textual
*         information is required.
*     TEXT   =  CHARACTER*(*) (Returned)
*         A single line of textual information.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the tab-separator character.
*     Determine the common block array element for the catalogue.
*     Get the Fortran unit number for accessing the description file.
*     Get the current header record.
*     Get the total number of header records.
*     If the current header record is less than the total number of
*     records then
*       Attempt to read a record from the table.
*       If ok then
*         Increment the number of the current record.
*         Obtain the class and text of the record, as follows:-
*         If the current record is the first record then
*           If the total number of records is more than two then
*             Set the class to 'NOTE'.
*             Copy the value.
*           else
*             Set the class to 'COLUMNS'
*             Replace tab characters with spaces.
*             Copy the text.
*           end if
*         else the current record is the penumtimate header record then
*           Set the class to 'COLUMNS'
*           Replace tab characters with spaces.
*           Copy the text.
*         else if the current record is the last header record then
*           Set the class to 'BEGINTABLE'
*           Replace tab characters with spaces.
*           Copy the text.
*         else
*           If the line is blank then
*             Set the class to 'NOTE'
*             Set the text to blank.
*           else
*             Split the line into words
*             If the first word starts with '#' then
*               Set the class to 'NOTE'
*             else if the first word ends in ':' then
*               Set the class to 'PARAMETER'
*             else
*               Set the class to 'COMMENT'
*             end if
*             Copy the text.
*           end if
*         end if
*       else (attempted read failed)
*         Set the finished flag to 'finished'.
*         Set the class to blank.
*         Set the text to blank.
*       end if
*     else (last header record already read)
*       Set the finished flag to 'finished'.
*       Set the class to blank.
*       Set the text to blank.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     17/6/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Arguments Returned:
      LOGICAL
     :  FINISH
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS           ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
       INTEGER MXWORD          ! Maximum permitted number of words in
       PARAMETER (MXWORD = 2)  ! a line.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  TSUNIT,  ! Fortan unit number for accessing tab-separated table.
     :  LSTAT,   ! Local Fortran I/O status.
     :  HDCUR,   ! Current header record.
     :  HDREC,   ! Total number of header records.
     :  BUFLEN,  ! Length of BUFFER (excl. trail. blanks).
     :  LOOP,    ! Loop index.
     :  DCSTAT,  ! Status splitting current line into words.
     :  NWORD,   ! Number of words in current line.
     :  START(MXWORD),  ! Start positions of words in current line.
     :  STOP(MXWORD)    ! Stop      "     "    "   "     "     "  .
      CHARACTER
     :  TAB*1,                 ! Tab-separator character.
     :  BUFFER*(CAT6__SZDRC),  ! Buffer for current record.
     :  WORDS(MXWORD)*20       ! Words found in the current record.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Set the separator character (a tab).

         TAB = CHAR(CAT6__TABI)

*
*       Determine the common block array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Get the Fortran unit number for accessing the table, the number
*       of header records that it contains and the current header
*       record.

         TSUNIT = TSUNT__CAT6(CIELM)
         HDREC = HDREC__CAT6(CIELM)
         HDCUR = HDCUR__CAT6(CIELM)

*
*       Check that all the header records have not been previously
*       read.

         IF (HDCUR .LT. HDREC) THEN

*
*          Attempt to read a record from the table and proceed if ok.

            READ(TSUNIT, 2000, IOSTAT=LSTAT) BUFFER
 2000       FORMAT(A)
            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Increment the current record number.

               HDCUR = HDCUR + 1
               HDCUR__CAT6(CIELM) = HDCUR

*
*             Determine the class and obtain the text from the record.
*             The details are somewhat complicated.
*
*             Start by handling the special case of the first record,
*             which is probably a title, but might be the list of column
*             names (the total number of header records distinguishes
*             between these two cases).

               IF (HDCUR .EQ. 1) THEN
                  IF (HDREC .GT. 2) THEN
                     CLASS = 'NOTE'
                     TEXT = BUFFER
                  ELSE
                     CLASS = 'COLUMNS'

                     IF (BUFFER .NE. ' ') THEN
                        BUFLEN = CHR_LEN(BUFFER)

                        DO LOOP = 1, BUFLEN
                           IF (BUFFER(LOOP : LOOP) .EQ. TAB) THEN
                              BUFFER(LOOP : LOOP) = ' '
                           END IF
                        END DO

                        TEXT = BUFFER
                     ELSE
                        TEXT = ' '
                     END IF
                  END IF

*
*             Case of the penultimate header record, which should
*             contain the list of column names.  Copy the names,
*             replacing the tabs with spaces.

               ELSE IF (HDCUR .EQ. HDREC - 1) THEN
                  CLASS = 'COLUMNS'

                  IF (BUFFER .NE. ' ') THEN
                     BUFLEN = CHR_LEN(BUFFER)

                     DO LOOP = 1, BUFLEN
                        IF (BUFFER(LOOP : LOOP) .EQ. TAB) THEN
                           BUFFER(LOOP : LOOP) = ' '
                        END IF
                     END DO

                     TEXT = BUFFER
                  ELSE
                     TEXT = ' '
                  END IF

*
*             Case of the last header record, which is the line of
*             dashes and tabs ending the description and immediately
*             preceding the table.

               ELSE IF (HDCUR .EQ. HDREC) THEN
                  CLASS = 'BEGINTABLE'

                  IF (BUFFER .NE. ' ') THEN
                     BUFLEN = CHR_LEN(BUFFER)

                     DO LOOP = 1, BUFLEN
                        IF (BUFFER(LOOP : LOOP) .EQ. TAB) THEN
                           BUFFER(LOOP : LOOP) = ' '
                        END IF
                     END DO

                     TEXT = BUFFER
                  ELSE
                     TEXT = ' '
                  END IF

               ELSE

*
*                Check whether the line is completely blank.  If so then
*                it is a 'NOTE'.

                  IF (BUFFER .EQ. ' ') THEN
                     CLASS = 'NOTE'
                     TEXT = ' '

                  ELSE

*
*                   Split the line into words and determine whether it
*                   is a 'NOTE', 'PARAMETER' or 'COMMENT':
*
*                   + the first word of a 'NOTE' starts with a '#',
*                   + the first word of a 'PARAMETER' ends with ':',
*                   + anything else is a 'COMMENT'.
*
*                   In all three cases the text is simply copied.

                     CALL CHR_DCWRD (BUFFER, MXWORD, NWORD, START, STOP,
     :                 WORDS, DCSTAT)

                     IF (WORDS(1)(1 : 1) .EQ. '#') THEN
                        CLASS = 'NOTE'
                     ELSE IF (BUFFER(STOP(1) : STOP(1)) .EQ. ':') THEN
                        CLASS = 'PARAMETER'
                     ELSE
                        CLASS = 'COMMENT'
                     END IF

                     TEXT = BUFFER

                  END IF
               END IF

            ELSE

*
*             The attempted read failed; tidy up.

               FINISH = .TRUE.
               CLASS = ' '
               TEXT = ' '

            END IF

         ELSE

*
*          The last record has already been read.  Set the 'finished'
*          flag and tidy up.

            FINISH = .TRUE.
            CLASS = ' '
            TEXT = ' '

         END IF

      END IF

      END
