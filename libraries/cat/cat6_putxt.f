      SUBROUTINE CAT6_PUTXT (CI, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT6_PUTXT
*  Purpose:
*     Put a line of textual information to a tab-separated table (TST).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_PUTXT (CI, CLASS, TEXT; STATUS)
*  Description:
*     Put a line of textual information to a tab-separated table (TST).
*
*     Lines are checked to ensure that they do not constitute TST
*     parameters.  For parameters the first word ends in a colon (':').
*     Any such lines are preceded by a '#', which turns them into a
*     TST note rather than a parameter.  This stratagem is adopted to
*     prevent the inadvertent duplication of parameters when TST
*     catalogues are copied.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  The classes permitted for
*         putting text are:
*         COMMENT  -  intended for general comments,
*         HISTORY  -  intended for history information.
*     TEXT   =  CHARACTER*(*) (Given)
*        A single line of textual information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the class is 'HISTORY' or 'COMMENT' then
*       Get the array element for the catalogue.
*       Get the Fortran unit number for the table.
*       Check whether the line is a parameter.
*       if it is not a parameter then
*         Attempt to write the line to the tab-separated table.
*       else
*         Attempt to write the line to the tab-separated table,
*         preceding it by a '#'.
*       end if
*       Report any error.
*     else
*       Set the status.
*       Report error: attempt to put an line of text with an illegal
*       class.
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
*     17/6/99 (ACD): Original version (from CAT5_GETXT).
*     18/6/99 (ACD): Added checks to avoid the inadvertent duplication
*        of parameters.
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
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS    ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
       INTEGER MXWORD          ! Maximum permitted number of words in
       PARAMETER (MXWORD = 2)  ! a line.
*  Local Variables:
      INTEGER
     :  CIELM,    ! Element for the catalogue in the common block arrays.
     :  TSUNIT,   ! Fortran unit number for the tab-separated table.
     :  LSTAT,    ! Fortran I/O status.
     :  LCLASS,   ! Length of CLASS  (excl. trail. blanks).
     :  ERRLEN,   !   "    "  ERRTXT ( "  .   "  .   "   ).
     :  DCSTAT,   ! Status splitting current line into words.
     :  NWORD,    ! Number of words in current line.
     :  START(MXWORD),     ! Start positions of words in current line.
     :  STOP(MXWORD)       ! Stop      "
      CHARACTER
     :  WORDS(MXWORD)*20,  ! Words found in the current line.
     :  ERRTXT*75 ! Error message text.
      LOGICAL
     :  ISPARM    ! Flag; is the line a parameter?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the class for the line of text is one of the
*       permitted classes.

         IF (CLASS .EQ. 'COMMENT'  .OR.  CLASS .EQ. 'HISTORY') THEN

*
*          Get the array element for the catalogue and the Fortran
*          unit number for the tab-separated table.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

            TSUNIT = TSUNT__CAT6(CIELM)


*
*          Check whether the line is a parameter.

            IF (TEXT .NE. ' ') THEN
               CALL CHR_DCWRD (TEXT, MXWORD, NWORD, START, STOP, WORDS,
     :           DCSTAT)

               IF (TEXT(STOP(1) : STOP(1)) .EQ. ':') THEN
                  ISPARM = .TRUE.
               ELSE
                  ISPARM = .FALSE.
               END IF

            ELSE
               ISPARM = .FALSE.
            END IF

*
*          Write the line, either preceded by a '#' or not, depending
*          on whether it is a parameter.

            IF (.NOT. ISPARM) THEN
               WRITE(TSUNIT, 2000, IOSTAT=LSTAT) TEXT
 2000          FORMAT(A)
            ELSE
               WRITE(TSUNIT, 2001, IOSTAT=LSTAT) TEXT
 2001          FORMAT('# ', A)
            END IF

            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .NE. CAT__OK) THEN
               CALL CAT1_ERREP ('CAT6_PUTXT_ERR', 'Error writing '/
     :           /'line of text to tab-separated table.', STATUS)
            END IF

         ELSE

*
*          The given class does not correspond to one of the permitted
*          classes.  Set the status and report an error.

            STATUS = CAT__ERROR

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Text class supplied, ', ERRTXT, ERRLEN)

            IF (CLASS .NE. ' ') THEN
               LCLASS = CHR_LEN(CLASS)
               CALL CHR_PUTC (CLASS(1 : LCLASS), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC (', is illegal.  Line of text not written.',
     :        ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT6_PUTXT_ICL', ERRTXT(1 : ERRLEN),
     :        STATUS)

         END IF

      END IF

      END
