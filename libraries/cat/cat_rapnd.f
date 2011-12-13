      SUBROUTINE CAT_RAPND (CI, STATUS)
*+
*  Name:
*     CAT_RAPND
*  Purpose:
*     Append the current row buffer to the end of the catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_RAPND (CI; STATUS)
*  Description:
*     Append the current row buffer as a new row at the end of the
*     catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Note that the actual writing to the catalogue files is actually
*     handled by the back-end specific parts of the PUT routines.  All
*     that is required here is to increment the current row number and
*     the total number of rows.
*
*     Obtain the common block array element for the catalogue.
*     Obtain the access mode of the catalogue.
*     If the catalogue was opened for 'WRITE' then
*       Increment the current row number.
*       Increment the total number of rows.
*     else
*       Set the status.
*     end if
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
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
*     3/5/93  (ACD): Prologue only.
*     13/8/93 (ACD): Original version.
*     24/1/94 (ACD): Modified error reporting.
*     4/4/01  (ACD): Moved a note describing how the routine is implemented
*       from the 'Description' section of the prologue to the more
*       appropriate 'Algorithm'.
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
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CIELM,   ! Common block array element for the catalogue.
     :  MODE     ! Access mode for the catalogue.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Obtain the common block array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Obtain the access mode of the catalogue.

         MODE = MODE__CAT1(CIELM)

*
*       Check that the catalogue was opened for 'WRITE'.

         IF (MODE .EQ. CAT1__MDWRT) THEN

*
*          Increment the current row number and the the total number of
*          rows.

            CROW__CAT1(CIELM) = CROW__CAT1(CIELM) + 1
            NROW__CAT1(CIELM) = NROW__CAT1(CIELM) + 1

         ELSE
            STATUS = CAT__INVWT

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_RAPND_ERR', 'CAT_RAPND: error'/
     :        /'appending current row buffer to catalogue.', STATUS)
         END IF

      END IF

      END
