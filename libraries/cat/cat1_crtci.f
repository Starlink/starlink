      SUBROUTINE CAT1_CRTCI (CI, STATUS)
*+
*  Name:
*     CAT1_CRTCI
*  Purpose:
*     Attempt to create a new catalogue identifier.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRTCI (CI; STATUS)
*  Description:
*     Attempt to create a new catalogue identifier.
*  Arguments:
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to create the identifier.
*     If ok then
*       Attempt to get an array element for a new identifier.
*       If ok then
*         Initialise the details for the catalogue.
*       else
*         Set the identifier to null.
*         Set the return status.
*         Report an error.
*       end if
*     else
*       Set the identifier to null.
*       Set the return status.
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     4/7/93  (ACD): Original version.
*     23/1/94 (ACD): Modified error reporting.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     7/3/95  (ACD): Modified to set the catalogue element array.
*     11/4/95 (ACD): Changed the name of the null identifier.
*     28/5/98 (ACD): Allow for re-use of released identifiers.
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
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP,    ! Loop index.
     :  CIELM    ! Element in catalogues arrays for the catalogue.
      LOGICAL
     :  FOUND,   ! Flag: has a free identifier been found?
     :  MORE     ! Flag; continue hunting for an identifier?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to create the identifier.

         CALL CAT1_CRTID (CAT__CITYP, CAT__PRNUL, CI, STATUS)
         IF (STATUS .EQ. CAT__OK) THEN

*
*          Attempt to get an array element for a new identifier.
*          Elements which have been previously released are reused.

            FOUND = .FALSE.

            IF (NCATS__CAT1 .LT. CAT__MXCAT) THEN
               NCATS__CAT1 = NCATS__CAT1 + 1
               CIELM = NCATS__CAT1
               FOUND = .TRUE.

            ELSE
               LOOP = 0
               MORE = .TRUE.

               DO WHILE (MORE)
                  LOOP = LOOP + 1

                  IF (CIDS__CAT1(LOOP) .EQ. CAT__NOID) THEN
                     CIELM = LOOP
                     FOUND = .TRUE.
                     MORE = .FALSE.
                  END IF

                  IF (LOOP .GE. NCATS__CAT1) THEN
                     MORE = .FALSE.
                  END IF
               END DO

            END IF

*
*          Proceed if there is space for a new catalogue.

            IF (FOUND) THEN

*
*             Initialise the details for the catalogue.

               CIDS__CAT1(CIELM) = CI
               NROW__CAT1(CIELM) = 0
               NPCOL__CAT1(CIELM) = 0
               NVCOL__CAT1(CIELM) = 0
               NIND__CAT1(CIELM) = 0
               NPAR__CAT1(CIELM) = 0
               NSEL__CAT1(CIELM) = 0

*
*             Set the catalogue array element in the identifiers arrays.

               IDCEL__CAT1(CI) = CIELM
            ELSE
               CI = CAT__NOID
               STATUS = CAT__MAXOP

               CALL CAT1_ERREP ('CAT1_CRTCI_MOP', 'Failed to create '/
     :           /'a new catalogue identifier.', STATUS)
            END IF

         ELSE
            CI = CAT__NOID
            STATUS = CAT__MAXID

            CALL CAT1_ERREP ('CAT1_CRTCI_MID', 'Failed to create a '/
     :        /'new catalogue identifier.', STATUS)
         END IF

      END IF

      END
