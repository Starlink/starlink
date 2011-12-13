      SUBROUTINE CAT3_CLOSE (CI, STATUS)
*+
*  Name:
*     CAT3_CLOSE
*  Purpose:
*     Close a catalogue held as a FITS binary table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_CLOSE (CI; STATUS)
*  Description:
*     Close a catalogue held as a FITS binary table.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the array element for the catalogue.
*     If ok then
*       Determine the Fortran unit number to access the FITS table.
*       If the state of the table is 'NEW' then
*         If creation of the catalogue is finished then
*           Write the number of rows as the appropriate 'NAXIS' keyword.
*         end if
*       end if
*       If creation of the catalogue is finished then
*         Close the catalogue file.
*       else
*         Delete the catalogue file.
*       end if
*       Release the FITS unit number.
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
*     6/8/93  (ACD): Original version.
*     23/1/94 (ACD): Modified error reporting.
*     19/9/99 (ACD): Tidied up closing incomplete catalogues, ie.
*        ones to which no rows had been writen.
*     8/2/00  (ACD): Added a call to FTFIOU to explicitly release the
*        FITS unit number after closing or deleting the catalogue.  This
*        addition appears to be required by CFITSIO.
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
      INCLUDE 'CAT3_FIT_CMN'      ! FITS common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Constants:
      INTEGER FITOK   ! FITSIO Success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  CIELM,  ! Element for the catalogue in the common block arrays.
     :  FITUNT, ! Fortran unit no. for access the FITS file.
     :  FITSTT, ! FITSIO running status.
     :  ROWS    ! Number of rows in the catalogue.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         FITSTT = FITOK

*
*       Determine the array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)
c        print2000, ci, cielm, status
c2000    format(1x, 'CAT3_CLOSE, ci, cielm, status: ', i5, i5, i10 )
         IF (STATUS .EQ. CAT__OK) THEN

*
*          Determine the Fortran unit number to access the FITS file.

            FITUNT = FUNT__CAT3(CIELM)
c           print2001, fitunt
c2001       format(1x, 'CAT3_CLOSE, fitunt: ', i5 )

*
*          Check whether the state of the catalogue is 'NEW', and if so
*          then update the number of rows.

c           print2002, state__cat1(cielm), nrow__cat1(cielm)
c2002       format(1x, 'CAT3_CLOSE, ',
c    :        'state__cat1(cielm), nrow__cat1(cielm): ', i5, 3x, i10 )

            IF (STATE__CAT1(CIELM) .EQ. CAT1__STNEW) THEN

*
*             Check whether creation of the catalogue is finished.

               IF (FINSH__CAT1(CIELM)) THEN
                  ROWS = NROW__CAT1(CIELM)

c                 print2004, rows
c2004             format(1x, 'CAT3_CLOSE, (before FTMKYJ), rows: ', i10)

                  CALL FTMKYJ (FITUNT, 'NAXIS2', ROWS, '&', FITSTT)

c                 print2005, fitstt
c2005             format(1x, 'CAT3_CLOSE, (after FTMKYJ), fitstt: ', i10)

                  IF (FITSTT .NE. FITOK) THEN
                     STATUS = CAT__ERROR
                     CALL CAT3_FITER ('CAT3_CLOSE_ROW', 'Failed to '/
     :                 /'write the number of rows to the FITS file',
     :                 FITSTT, STATUS)
                  END IF

               END IF
            END IF

*
*          Close the file.

c           print2006, fitunt
c2006       format(1x, 'CAT3_CLOSE, (before FTCLOS), fitunt: ', i5 )

            IF (FINSH__CAT1(CIELM)) THEN
               CALL FTCLOS (FITUNT, FITSTT)
            ELSE
               CALL FTDELT (FITUNT, FITSTT)
            END IF

c           print1000, 'FTCLOS', fitstt
c1000       format(1X, 'after ', A, 3x, 'fitstt: ', I10)

*
*          Release the FITS unit number.

            CALL FTFIOU (FITUNT, FITSTT)

            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_CLOSE_CSE', 'Failed to close '/
     :           /'the FITS file correctly', FITSTT, STATUS)
            END IF

         END IF

c        print2003
c2003    format(1x, 'CAT3_CLOSE - finished.')

      END IF

      END
