      SUBROUTINE CAT6_GTCLD (CI, ROWS, MAXCOL, NUMCOL, FIA, FDTYPA,
     :  FCSIZA, FPTRA, FPTRNA, STATUS)
*+
*  Name:
*     CAT6_GTCLD
*  Purpose:
*     Obtain data types and workspace for the columns.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_GTCLD (CI, ROWS, MAXCOL; NUMCOL, FIA, FDTYPA, FCSIZA,
*       FPTRA, FPTRNA; STATUS)
*  Description:
*     Obtain data types and workspace for the columns.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the small text list.
*     MAXCOL  =  INTEGER (Given)
*        Maximum perimitted number of columns.
*     NUMCOL  =  INTEGER (Returned)
*        Total number of columns in the catalogue (treating vector
*        elements as separate columns).
*     FIA(MAXCOL)  =  INTEGER (Returned)
*        Column identifiers.
*     FDTYPA(MAXCOL)  =  INTEGER (Returned)
*        Data types of the columns.
*     FCSIZA(MAXCOL)  =  INTEGER (Returned)
*        Size of character columns.
*     FPTRA(MAXCOL)  =  INTEGER (Returned)
*        Pointer to array to hold the column.
*     FPTRNA(MAXCOL)  =  INTEGER (Returned)
*        Pointer to array to hold the null value flags for the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of columns.
*     While there are more columns to be obtained
*       Attempt to obtain an identifier
*       If ok then
*         If there is space for another column then
*           Increment the number of columns.
*           Return the column identifier.
*           Obtain the data type.
*           Map the work space for the array of values and the array
*           of null-value flags.
*         else
*           Set the termination flag.
*           Set the status.
*           Report error: arrays full.
*         end if
*       else
*         Set the termination flag.
*         If an error ocurred then
*           Report the error.
*         end if
*       end if
*     end do
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
*     4/6/99  (ACD): Original version.
*     15/6/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ROWS,
     :  MAXCOL
*  Arguments Returned:
      INTEGER
     :  NUMCOL,
     :  FIA(MAXCOL),
     :  FDTYPA(MAXCOL),
     :  FCSIZA(MAXCOL),
     :  FPTRA(MAXCOL),
     :  FPTRNA(MAXCOL)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  MORE     ! Flag; are there more columns to process
      INTEGER
     :  LTYPE,   ! Length of TYPE  (excl. trail. blanks).
     :  NXTCOL,  ! Number of the next column.
     :  FI,      ! Current column identifier.
     :  FDTYPE,  ! Data type attribute for the current column.
     :  FCSIZE   ! Character size attribute for the current column.
      CHARACTER
     :  TYPE*(CAT__SZTYP)   ! Character repn. of current data type.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the number of columns.

         NUMCOL = 0

*
*       Attempt to obtain details for all the columns.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to obtain an identifier and proceed if all is ok.

            NXTCOL = NUMCOL + 1
            CALL CAT_TNDNT (CI, CAT__FITYP, NXTCOL, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Check whether there is space for another column.

               IF (NUMCOL .LT. MAXCOL) THEN
                  NUMCOL = NUMCOL + 1

*
*                Return the column identifier.

                  FIA(NUMCOL) = FI

*
*                Obtain the data type and character size of the column.

                  CALL CAT_TIQAI (FI, 'DTYPE', FDTYPE, STATUS)
                  CALL CAT_TIQAI (FI, 'CSIZE', FCSIZE, STATUS)

                  FDTYPA(NUMCOL) = FDTYPE
                  FCSIZA(NUMCOL) = FCSIZE

*
*                Attempt to map the work space for the array of values and
*                the corresponding array of null-value flags.


                  TYPE = ' '
                  LTYPE = 0

                  CALL CAT_TYFMT (FDTYPE, FCSIZE, TYPE, LTYPE, STATUS)

                  CALL CAT1_CRTAR (ROWS, TYPE(1 : LTYPE),
     :              FPTRA(NUMCOL), STATUS)

                  CALL CAT1_CRTAR (ROWS, '_LOGICAL', FPTRNA(NUMCOL),
     :              STATUS)

               ELSE

*
*                The arrays are full: set the termination flag,
*                set the status and report an error.

                  MORE = .FALSE.
                  STATUS = CAT__MAXID

                  CALL CAT1_ERREP ('CAT6_GTCLD_MXID', 'Maximum number '/
     :              /'of columns exceeded.', STATUS)
               END IF
            ELSE

*
*             Failed to obtain a column identifier.  Set the termination
*             flag.  If an error ocurred then report it.

               MORE = .FALSE.

               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT6_GTCLD_MXID', 'Failed to get '/
     :              /'column identifier.', STATUS)
               END IF
            END IF
         END DO

      END IF

      END
