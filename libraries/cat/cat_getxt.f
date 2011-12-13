      SUBROUTINE CAT_GETXT (CI, FINISH, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT_GETXT
*  Purpose:
*     Get the next line of textual information from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_GETXT (CI; FINISH, CLASS, TEXT; STATUS)
*  Description:
*     Get the next line of textual information from a catalogue.
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
*     Determine the type of the identifier.
*     If the identifier is for a catalogue then
*       Determine the back-end type.
*       Get the next line of text.
*       If the string is a ling of AST information then
*         Set the class to AST.
*       end if
*     else
*       Set the status: invalid identifier.
*     end if
*     Report any error.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/9/94  (ACD): Original version.
*     11/10/99 (ACD): Added trapping of AST information.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
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
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  IDTYPE,  ! Type of identifier CI.
     :  BCKTYP,  ! Back-end type.
     :  ASTPOS   ! Position of the AST object signature.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier and proceed if it
*       corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*          Determine the back-end type.

            CALL CAT1_CIELM (CI, CIELM, STATUS)
            BCKTYP = BKTYP__CAT1(CIELM)

*
*          Get the line of text.

            CALL CAT0_GETXT (BCKTYP, CI, FINISH, CLASS, TEXT, STATUS)

*
*          Check whether the line includes the AST signature and
*          if so then set the class to 'AST'.

            ASTPOS = INDEX (TEXT, CAT__ASTSG)

            IF (ASTPOS .GT. 0) THEN
               CLASS = 'AST'
            END IF

         ELSE

*
*          The given identifier does not correspond to a catalogue; set
*          the return status to 'invalid identifier'.

            STATUS = CAT__INVID

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_GETXT_ERR', 'CAT_GETXT: error '/
     :        /'getting line of text from catalogue.', STATUS)
         END IF

      END IF

      END
