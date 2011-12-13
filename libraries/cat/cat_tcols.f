      SUBROUTINE CAT_TCOLS (CI, COLFLG, NUMCOL, STATUS)
*+
*  Name:
*     CAT_TCOLS
*  Purpose:
*     Get the number of columns in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_TCOLS (CI, COLFLG; NUMCOL; STATUS)
*  Description:
*     Get the number of columns in a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     COLFLG  =  INTEGER (Given)
*        Flag indicating the type of columns of which the number is
*        to be obtained, coded as follows:
*        CAT__GVIRT - virtual columns only,
*        CAT__GPHYS - physical columns only,
*        CAT__GALL  - all columns (virtual and physical).
*     NUMCOL  =  INTEGER (Returned)
*        Number of columns in the catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to find the array element corresponding to the catalogue.
*     If ok then
*       Extract the details of the columns of the catalogue from the
*       catalogue arrays.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93  (ACD): Prologue only.
*     17/7/93 (ACD): First implementation.
*     24/1/94 (ACD): Modified error reporting.
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
     :  CI,
     :  COLFLG
*  Arguments Returned:
      INTEGER
     :  NUMCOL
*  Status:
      INTEGER STATUS   ! Global status
*  Local Variables:
      INTEGER
     :  CIELM     ! Array element corresponding to the catalogue.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to find the array element corresponding to the
*       catalogue and proceed if ok.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Extract the details of the columns of the catalogue from the
*          catalogue arrays.

            IF (COLFLG .EQ. CAT__GVIRT) THEN
               NUMCOL = NVCOL__CAT1(CIELM)

            ELSE IF (COLFLG .EQ. CAT__GPHYS) THEN
               NUMCOL = NPCOL__CAT1(CIELM)

            ELSE IF (COLFLG .EQ. CAT__GALL) THEN
               NUMCOL = NVCOL__CAT1(CIELM) + NPCOL__CAT1(CIELM)

            ELSE
               NUMCOL = 0
               STATUS = CAT__INVGN

            END IF

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            CALL CAT1_ERREP ('CAT_TCOLS_ERR', 'CAT_TCOLS: error'/
     :        /'getting the number of columns in the catalogue.',
     :        STATUS)
         END IF

      END IF

      END
