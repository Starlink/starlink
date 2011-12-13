      SUBROUTINE CAT6_CLOSE (CI, STATUS)
*+
*  Name:
*     CAT6_CLOSE
*  Purpose:
*     Close a catalogue held as a tab-separated table (TST).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_CLOSE (CI; STATUS)
*  Description:
*     Close a catalogue held as a tab-separated table (TST).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to determine the common block array element for the
*     catalogue.
*     If ok then
*       Obtain the Fortran unit number for the TST file.
*       If the catalogue is being created then
*         If creation of the catalogue is finished then
*           Set the total number of rows.
*           Obtain the Fortran unit number for the TST file.
*           Write a blank line to the file.
*           Write the parameters.
*           Write a blank line.
*           Identify any TST special columns and write their details.
*           Write the column details, names and `end of description' line.
*           Write the table of values.
*         end if
*       end if
*       If creation of the catalogue is finished then
*         Close the catalogue file.
*       else
*         Delete the catalogue file.
*       end if
*       Release the workspace associated with all the catalogue columns.
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
*     15/6/99 (ACD): Original version (from CAT5_CLOSE).
*     19/9/99 (ACD): First stable version.
*     11/7/00 (ACD): Modified to write some column details as well as
*        the column names.
*     14/7/00 (ACD): Added proper identification and handling of TST
*        special columns.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT6_TST_CMN'      ! TST common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS              ! Global status
*  Local Variables:
      INTEGER
     :  CIELM,  ! Element for the catalogue in the common block arrays.
     :  TSUNIT, ! Fortran unit number for the catalogue file.
     :  LSTAT,  ! Fortan I/O status from closing the files.
     :  LOOP    ! Loop index.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to determine the common block array element for the
*       catalogue and proceed if ok

         CALL CAT1_CIELM (CI, CIELM, STATUS)
         IF (STATUS .EQ. CAT__OK) THEN

*
*          Obtain the Fortran unit number for the STL file.

            TSUNIT = TSUNT__CAT6(CIELM)

*
*          Check whether the catalogue is being created.

            IF (STATE__CAT1(CIELM) .EQ. CAT1__STNEW) THEN

*
*             Check whether creation of the catalogue is finished.

               IF (FINSH__CAT1(CIELM)) THEN

*
*                Set the total number of rows.

C                 print3000, nrow__cat1(cielm), crow__cat1(cielm)
C3000             format(1x, 'nrow__cat1(cielm), crow__cat1(cielm): ',
C    :              i5, i5)

                  NROW__CAT1(CIELM) = CROW__CAT1(CIELM) - 1

C                 print3000, nrow__cat1(cielm), crow__cat1(cielm)

*
*                Write a blank line to the STL file.  This blank line
*                separates the parameters from preceding items.

                  WRITE(TSUNIT, 2000, IOSTAT=LSTAT)
 2000             FORMAT(1X)
                  CALL CAT1_IOERR (LSTAT, STATUS)

*
*                Write the parameters.

                  CALL CAT6_WRPAR (CI, TSUNIT, STATUS)

*
*                Write another blank line.

                  WRITE(TSUNIT, 2000, IOSTAT=LSTAT)
                  CALL CAT1_IOERR (LSTAT, STATUS)

*
*                Identify any TST special columns (object name, Right
*                Ascension and Declination) and write their details
*                to the catalogue file.

                  CALL CAT6_SPCOL (CI, TSUNIT, STATUS)

*
*                Write the column details, names and `end of description'
*                line.

                  CALL CAT6_WRCOL (CI, TSUNIT, STATUS)

*
*                Write the table of values.

                  CALL CAT6_WRTBL (CI, NROW__CAT1(CIELM), TSUNIT,
     :              STATUS)

               END IF
            END IF

*
*          If creation of the catalogue is complete then close the
*          catalogue file.  Otherwise delete it.

            IF (FINSH__CAT1(CIELM)) THEN
               CLOSE(UNIT=TSUNIT, IOSTAT=LSTAT)
            ELSE
               CLOSE(UNIT=TSUNIT, STATUS='DELETE', IOSTAT=LSTAT)
            END IF

            IF (STATUS .EQ. CAT__OK) THEN
               CALL CAT1_IOERR (LSTAT, STATUS)
            END IF

*
*          Release the workspace associated with all the catalogue
*          columns.

            DO LOOP = 1, NIDS__CAT1
               IF (IDTYP__CAT1(LOOP) .EQ. CAT__FITYP  .OR.
     :             IDTYP__CAT1(LOOP) .EQ. CAT__FETYP) THEN
                  IF (IDPRN__CAT1(LOOP) .EQ. CI) THEN
                     CALL CAT1_FREAR (FPTR__CAT6(LOOP), STATUS)
                     CALL CAT1_FREAR (FPTRN__CAT6(LOOP), STATUS)
                  END IF
               END IF
            END DO

         END IF

      END IF

      END
