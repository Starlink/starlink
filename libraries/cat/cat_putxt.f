      SUBROUTINE CAT_PUTXT (CI, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT_PUTXT
*  Purpose:
*     Put a line of textual information to a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_PUTXT (CI, CLASS, TEXT; STATUS)
*  Description:
*     Put a line of textual information to a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  The standard classes
*         'COMMENT' and 'HISTORY' are available for all back-ends.
*         Individual back-ends may support additional classes.
*     TEXT   =  CHARACTER*(*) (Given)
*        A single line of textual information.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier is for a catalogue then
*       Determine the access mode of the catalogue.
*       If the access mode is 'WRITE' then
*         Determine the back-end type.
*         Put the next line of text.
*       else
*         Set the status: cannot write to a read-only catalogue.
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
*     15/9/94 (ACD): Original version.
*     28/9/94 (ACD): First stable version.
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
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  IDTYPE,  ! Type of identifier CI.
     :  MODE,    ! Code for access mode: READ, WRITE etc.
     :  BCKTYP   ! Back-end type.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier and proceed if it
*       corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*          Determine the access mode of the catalogue and proceed if
*          it can be written to.

            CALL CAT1_CIELM (CI, CIELM, STATUS)
            MODE = MODE__CAT1(CIELM)

            IF (MODE .EQ. CAT1__MDWRT) THEN

*
*             Determine the back-end type.

               BCKTYP = BKTYP__CAT1(CIELM)

*
*             Put the line of text.

               CALL CAT0_PUTXT (BCKTYP, CI, CLASS, TEXT, STATUS)

            ELSE

*
*             The catalogue access mode is not 'WRITE'.  Set the
*             return status to indicate an attempt to write to a
*             read-only catalogue.

               STATUS = CAT__INVWT

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
            CALL CAT1_ERREP ('CAT_PUTXT_ERR', 'CAT_PUTXT: error '/
     :        /'putting line of text to catalogue.', STATUS)
         END IF

      END IF

      END
