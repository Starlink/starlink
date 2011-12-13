      SUBROUTINE CAT_SZTXT (CI, ACCESS, LINESZ, STATUS)
*+
*  Name:
*     CAT_SZTXT
*  Purpose:
*     Return the maximum permitted size of a line of text for a cat.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_SZTXT (CI, ACCESS; LINESZ; STATUS)
*  Description:
*     Return the maximum permitted size of a line of text for a
*     catalogue.  The maximum permitted size is defined by the back-end
*     type.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ACCESS  =  CHARACTER*(*) (Given)
*        Mode in which the text is to be accessed; 'READ' or 'WRITE'.
*     LINESZ  =  INTEGER (Returned)
*        Maximum permitted size of a line of textual information for
*        the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier is for a catalogue then
*       If the access mode is 'READ' or 'WRITE' then
*         Determine the back-end type.
*         Determine the maximum permitted line size.
*       else
*         Set the status: invalid access mode.
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
*     28/9/94  (ACD): First stable version.
*     30/7/96  (ACD): Added back-end type for small text lists.
*     22/1/97  (ACD): Fixed a spelling mistake in the prologue.
*     16/6/99  (ACD): Added back-end type for tab-separated table.
*     12/10/99 (ACD): Changed the maximum size for writing for a TST.
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
     :  ACCESS*(*)
*  Arguments Returned:
      INTEGER
     :  LINESZ
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  IDTYPE,  ! Type of identifier CI.
     :  BCKTYP   ! Back-end type.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the type of the identifier and proceed if it
*       corresponds to a catalogue.

         CALL CAT_TIDTP (CI, IDTYPE, STATUS)

         IF (IDTYPE .EQ. CAT__CITYP) THEN

*
*          Check that the access mode is either 'READ' or 'WRITE'.

            IF (ACCESS .EQ. 'READ'  .OR.  ACCESS .EQ. 'WRITE') THEN

*
*             Determine the back-end type.

               CALL CAT1_CIELM (CI, CIELM, STATUS)
               BCKTYP = BKTYP__CAT1(CIELM)

*
*             Determine the maximum permitted line size.  Note that
*             for some back-ends the mode will vary depending on
*             whether the access is 'READ' or 'WRITE'.

               IF (BCKTYP .EQ. CAT1__BKADC) THEN
                  IF (ACCESS .EQ. 'READ') THEN
                     LINESZ = 132
                  ELSE
                     LINESZ = 1
                  END IF

               ELSE IF (BCKTYP .EQ. CAT1__BKFIT) THEN
                  IF (ACCESS .EQ. 'READ') THEN
                     LINESZ = 80
                  ELSE
                     LINESZ = 72
                  END IF

               ELSE IF (BCKTYP .EQ. CAT1__BKCHI) THEN
                  LINESZ = 1

               ELSE IF (BCKTYP .EQ. CAT1__BKSTL) THEN
                  IF (ACCESS .EQ. 'READ') THEN
                     LINESZ = CAT1__SZDRC
                  ELSE
                     LINESZ = 78
                  END IF

               ELSE IF (BCKTYP .EQ. CAT1__BKTST) THEN
                  LINESZ = 80

               ELSE
                  LINESZ = 1
                  STATUS = CAT__INVBK

               END IF

            ELSE
               STATUS = CAT__INVAC

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
            CALL CAT1_ERREP ('CAT_SZTXT_ERR', 'CAT_SZTXT: error '/
     :        /'inquiring text line length for catalogue.', STATUS)
         END IF

      END IF

      END
