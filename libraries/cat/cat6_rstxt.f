      SUBROUTINE CAT6_RSTXT (CI, STATUS)
*+
*  Name:
*     CAT6_RSTXT
*  Purpose:
*     Reset the access to the textual information in a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_RSTXT (CI; STATUS)
*  Description:
*     Reset the access to the textual information in a tab-separated table.
*     A subsequent attempt access a line of textual information will
*     return the first line of textual information.
*
*     This effect is achieved by rewinding the tab-separated table file.
*  Arguments:
*     CI  =  INTEGER (Given)
*         Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the common block array element for the catalogue.
*     Obtain the Fortran unit number for the description file.
*     Attempt to rewind the description file.
*     If ok then
*       Set the current record to zero.
*     else
*       Report an error.
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
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  CIELM,  ! Element for the catalogue in the common block arrays.
     :  TSUNIT, ! Fortran unit number for the tab-separated table.
     :  LSTAT   ! Fortan I/O status from closing the files.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the common block array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Determine the Fortran unit number for the description file.

         TSUNIT = TSUNT__CAT6(CIELM)

*
*       Attempt to rewind the description file.

         REWIND(TSUNIT, IOSTAT=LSTAT)
         CALL CAT1_IOERR (LSTAT, STATUS)

*
*       If the rewind succeeded then set the current record number to
*       zero (the start of the file), otherwise report an error.

         IF (STATUS .EQ. CAT__OK) THEN
            HDCUR__CAT6(CIELM) = 0
         ELSE
            CALL CAT1_ERREP ('CAT6_RSTXT_ERR', 'Error rewinding '/
     :        /'the tab-separated table.', STATUS)
         END IF

      END IF

      END
