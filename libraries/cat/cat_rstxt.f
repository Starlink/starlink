      SUBROUTINE CAT_RSTXT (CI, STATUS)
*+
*  Name:
*     CAT_RSTXT
*  Purpose:
*     Reset the access to the textual information in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_RSTXT (CI; STATUS)
*  Description:
*     Reset the access to the textual information in a catalogue.
*     A subsequent attempt access a line of textual information will
*     return the first line of textual information.
*  Arguments:
*     CI  =  INTEGER (Given)
*         Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the type of the identifier.
*     If the identifier is for a catalogue then
*       Determine the back-end type.
*       Reset access to the textual information.
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
*          Determine the back-end type.

            CALL CAT1_CIELM (CI, CIELM, STATUS)
            BCKTYP = BKTYP__CAT1(CIELM)

*
*          Reset access to the textual information.

            CALL CAT0_RSTXT (BCKTYP, CI, STATUS)

         ELSE

*
*          The given identifier does not correspond to a catalogue; set
*          the return status to 'invalid identifier'.

            STATUS = CAT__INVID

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_RSTXT_ERR', 'CAT_RSTXT: error '/
     :        /'resetting text access to catalogue.', STATUS)
         END IF

      END IF

      END
