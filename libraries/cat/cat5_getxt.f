      SUBROUTINE CAT5_GETXT (CI, FINISH, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT5_GETXT
*  Purpose:
*     Get the next line of textual information from a small text list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_GETXT (CI; FINISH, CLASS, TEXT; STATUS)
*  Description:
*     Get the next line of textual information from a small text list.
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
*     Determine the common block array element for the catalogue.
*     Get the Fortran unit number for accessing the description file.
*     If the last element of the description has not already been
*     found then
*       Attempt to read a record from the description file.
*       If ok then
*         Copy the record read to the return string.
*         Set the return flag to 'not finished'.
*         Attempt to determine the class of the line of text (and if
*         the class is 'BEGINTABLE' then set the 'last element of
*         the description found' flag).
*       else
*         Set the return flag to finished.
*         Set the return string to blank.
*         Set the return class to blank.
*         If the status is end-of-file then
*           Reset the status to ok.
*         else
*           Report an error.
*         end if
*       end if
*     else
*       Set the return flag to finished.
*       Set the return string to blank.
*       Set the return class to blank.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     26/7/96  (ACD): Original version.
*     10/12/96 (ACD): Added reading 'KAPPA format' STLs.
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
      INCLUDE 'CAT5_STL_CMN'      ! STL back-end common block.
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
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  DFUNIT,  ! Fortan unit number for accessing FITS table.
     :  LSTAT    ! Local Fortran I/O status.
      CHARACTER
     :  BUFFER*(CAT1__SZDRC)  ! Buffer for record read.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the common block array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Get the Fortran unit number for accessing the description file.

         DFUNIT = STUNT__CAT5(CIELM)

*
*       Check whether the last element of the description has already been
*       found then

         IF (.NOT. FNDSC__CAT5(CIELM) ) THEN

*
*          Attempt to read a record from the description file and
*          proceed if ok.

            READ(DFUNIT, '(A)', IOSTAT=LSTAT) BUFFER
            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Copy the record read to the return string.

               TEXT = BUFFER

*
*             Set the return flag to 'not finished'.

               FINISH = .FALSE.

*
*             Attempt to determine the class of the line of text (and if
*             the class is 'BEGINTABLE' then set the 'last element of
*             the description found' flag).

               IF (BUFFER .NE. ' ') THEN
                  CALL CHR_LDBLK (BUFFER)
                  CALL CHR_UCASE (BUFFER)

                  IF (BUFFER(1 : 1) .EQ. 'P'  .OR.
     :                BUFFER(1 : 2) .EQ. '#P') THEN
                     CLASS = 'PARAMETER'
                  ELSE IF (BUFFER(1 : 1) .EQ. 'C'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#C') THEN
                     CLASS = 'COLUMN'
                  ELSE IF (BUFFER(1 : 1) .EQ. 'D'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#D') THEN
                     CLASS = 'DIRECTIVE'
                  ELSE IF (BUFFER(1 : 1) .EQ. ':'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#:') THEN
                     CLASS = 'CONTINUATION'
                  ELSE IF (BUFFER(1 : 1) .EQ. '!'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#!') THEN
                     CLASS = 'NOTE'
                  ELSE IF (BUFFER(1 : 1) .EQ. 'T'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#T') THEN
                     CLASS = 'COMMENT'
                  ELSE IF (BUFFER(1 : 1) .EQ. 'B'  .OR.
     :                     BUFFER(1 : 2) .EQ. '#B') THEN
                     CLASS = 'BEGINTABLE'
                     FNDSC__CAT5(CIELM) = .TRUE.
                  ELSE
                     CLASS = 'UNKNOWN'
                  END IF
               ELSE
                  CLASS = 'NOTE'
               END IF

            ELSE

*
*             The attempt to read a line failed.  Set the return values.
*             If the status was end-of-file then reset it, otherwise
*             report an error.

               FINISH = .TRUE.
               TEXT = ' '
               CLASS = ' '

               IF (STATUS .EQ. CAT__EOF) THEN
                  STATUS = CAT__OK
               ELSE
                  CALL CAT1_ERREP ('CAT5_GETXT_ERR', 'Error reading '/
     :              /'a line of text from the description file.',
     :              STATUS)
               END IF
            END IF

         ELSE
*
*          The 'finished' flag is set (that is, the previous line was
*          a 'BEGINTABLE').  Set the return values.

            FINISH = .TRUE.
            TEXT = ' '
            CLASS = ' '

         END IF

      END IF

      END
