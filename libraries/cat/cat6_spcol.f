      SUBROUTINE CAT6_SPCOL (CI, TSUNIT, STATUS)
*+
*  Name:
*     CAT6_SPCOL
*  Purpose:
*     Set details of the TST special columns.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_SPCOL (CI, TSUNIT; STATUS)
*  Description:
*     Set details of the TST special columns.
*
*     Any columns with the names ID, RA and DEC are identified and
*     their sequence numbers written as the TST special parameters
*     id_col, ra_col and dec_col respectively.
*
*     Also the units of columns RA and DEC are reset to the display
*     format required by the TST format (respectively sexagesimal
*     hours and degrees).
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     TSUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the tab-separated table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the default ID, RA and DEC column numbers.
*     While there are more columns.
*       Increment the number of columns.
*       Attempt to obtain an identifier for the next column.
*       If ok and not the null identifier then
*         Get the column name.
*         Convert the column name to upper case.
*         If the column name is 'ID' then
*           Set the ID column number to the current column number.
*         else if column name is 'RA' then
*           Set the RA column number to the current column number.
*           Set the units for the RA column.
*         else if column name is 'DEC' then
*           Set the DEC column number to the current column number.
*           Set the units for the DEC column.
*         end if
*       else
*         Set the termination flag.
*         If the status is not ok then
*           Report error.
*         end if
*       end if
*     end do
*     Write the TST special parameters defining the ID, RA and DEC
*     columns.
*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
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
*     14/7/00 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  TSUNIT
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Variables:
      INTEGER
     :  IDNUM,   ! Sequence number for identifier      column.
     :  RANUM,   !    "       "     "  Right Ascension   "   .
     :  DECNUM,  !    "       "     "  Declination       "   .
     :  CURCOL,  ! Number of current column.
     :  FI,      ! Identifier for the current column.
     :  LSTAT    ! Fortran I/O status.
      LOGICAL
     :  MORE     ! Flag; more columsn to process?
      CHARACTER
     :  FNAME*(CAT__SZCMP)   ! Name of the current column.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the default sequence numbers for the identifier,
*       Right Ascension and Declination columns (called ID, RA and
*       DEC respectively).
*
*       Note that for a CAT catalogue the default has to be that the
*       catalogue does not contain these columns.  A value of zero
*       indicates the colomns are absent.

         IDNUM = 0
         RANUM = 0
         DECNUM = 0

*
*       Examine all the columns.

         CURCOL = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Increment the number of columns and attempt to obtain an
*          identifier for the next column.  Then proceed if all is ok
*          and the identifier is not the null identifier.

            CURCOL = CURCOL + 1
            CALL CAT_TNDNT (CI, CAT__FITYP, CURCOL, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Get the column name and convert it to upper case.

               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               CALL CHR_UCASE (FNAME)

*
*             Check for the various special column names.
*
*             In all cases the column number is recorded.  For the
*             Right Ascension and Declination the units are reset to
*             a format which is part of the TST format.  Note that
*             several decimal places of seconds of arc or time are
*             specified to preserve accuracy.
*
*             ... first identifier, ID.

               IF (FNAME .EQ. 'ID') THEN
                  IDNUM = CURCOL

*             ... Right Ascension, RA.

               ELSE IF (FNAME .EQ. 'RA') THEN
                  RANUM = CURCOL

                  CALL CAT_TATTC (FI, 'UNITS', 'RADIANS{HMS.3}',
     :              STATUS)

*             ... Declination, DEC.

               ELSE IF (FNAME .EQ. 'DEC') THEN
                  DECNUM = CURCOL

                  CALL CAT_TATTC (FI, 'UNITS', 'RADIANS{DMS.2}',
     :              STATUS)

               END IF

            ELSE

*
*             Failed to get a column identifier.  Set the termination flag
*             and report any error.

               MORE = .FALSE.

               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT6_SPCOL_ERR', 'Failed to '/
     :              /'get column identifier.', STATUS)
               END IF
            END IF
         END DO

*
*       Write the TST special parameters defining the identifier,
*       Right Ascension and Declination special columns.  The values
*       of these parameters are the corresponding column numbers.  But
*       note that the numbers stored in the TST are one less than the
*       CAT numbers (the former start counting at 0, the latter at 1).
*       The value used to indicate that no column is present (-1 and
*       0 respectively) differs by the same amount.

         IDNUM = IDNUM - 1
         RANUM = RANUM - 1
         DECNUM = DECNUM - 1

         WRITE(TSUNIT, 2000, IOSTAT=LSTAT) IDNUM, RANUM, DECNUM
 2000    FORMAT('id_col: ', I4 / 'ra_col: ', I4 / 'dec_col: ', I4 / )
         CALL CAT1_IOERR (LSTAT, STATUS)

      END IF

      END
