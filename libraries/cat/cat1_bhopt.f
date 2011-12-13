      SUBROUTINE CAT1_BHOPB (CI, FI, ROWS, ORDER, XVALB, L, STATUS)
*+
*  Name:
*     CAT1_BHOPB
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPB (CI, FI, ROWS, ORDER, XVALB; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALB  and
*                                X[L-1] .LT. XVALB
*
*     For a descending column:   X[L]   .LE. XVALB  and
*                                X[L-1] .GT. XVALB
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALB  =  BYTE (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
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
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALB.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPEB, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPB_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPC (CI, FI, ROWS, ORDER, XVALC, L, STATUS)
*+
*  Name:
*     CAT1_BHOPC
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPC (CI, FI, ROWS, ORDER, XVALC; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALC  and
*                                X[L-1] .LT. XVALC
*
*     For a descending column:   X[L]   .LE. XVALC  and
*                                X[L-1] .GT. XVALC
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALC  =  CHARACTER*(*) (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALC.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPEC, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPC_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPD (CI, FI, ROWS, ORDER, XVALD, L, STATUS)
*+
*  Name:
*     CAT1_BHOPD
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPD (CI, FI, ROWS, ORDER, XVALD; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALD  and
*                                X[L-1] .LT. XVALD
*
*     For a descending column:   X[L]   .LE. XVALD  and
*                                X[L-1] .GT. XVALD
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALD  =  DOUBLE PRECISION (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALD.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPED, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPD_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPI (CI, FI, ROWS, ORDER, XVALI, L, STATUS)
*+
*  Name:
*     CAT1_BHOPI
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPI (CI, FI, ROWS, ORDER, XVALI; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALI  and
*                                X[L-1] .LT. XVALI
*
*     For a descending column:   X[L]   .LE. XVALI  and
*                                X[L-1] .GT. XVALI
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALI  =  INTEGER (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALI.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPEI, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPI_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPL (CI, FI, ROWS, ORDER, XVALL, L, STATUS)
*+
*  Name:
*     CAT1_BHOPL
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPL (CI, FI, ROWS, ORDER, XVALL; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALL  and
*                                X[L-1] .LT. XVALL
*
*     For a descending column:   X[L]   .LE. XVALL  and
*                                X[L-1] .GT. XVALL
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALL  =  LOGICAL (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALL.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPEL, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPL_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPR (CI, FI, ROWS, ORDER, XVALR, L, STATUS)
*+
*  Name:
*     CAT1_BHOPR
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPR (CI, FI, ROWS, ORDER, XVALR; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALR  and
*                                X[L-1] .LT. XVALR
*
*     For a descending column:   X[L]   .LE. XVALR  and
*                                X[L-1] .GT. XVALR
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALR  =  REAL (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALR.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPER, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPR_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAT1_BHOPW (CI, FI, ROWS, ORDER, XVALW, L, STATUS)
*+
*  Name:
*     CAT1_BHOPW
*  Purpose:
*     Locate row number corresponding to a value by binary chop.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_BHOPW (CI, FI, ROWS, ORDER, XVALW; L; STATUS)
*  Description:
*     Perform a binary chop to locate the row in a catalogue for
*     which the field of a column corresponds to a given value.  The
*     column must be sorted into ascending or descending order.  The
*     corresponding row, L, of column X is defined such that:
*
*     For an ascending column:   X[L]   .GE. XVALW  and
*                                X[L-1] .LT. XVALW
*
*     For a descending column:   X[L]   .LE. XVALW  and
*                                X[L-1] .GT. XVALW
*
*     where XVAL is the given (target) value.
*
*     For a column in ascending order:
*       if the target value is .LE. the first row then a row number
*       of zero is returned,
*
*       if the target value is .GT. than the last row then a row
*       number of the number of rows + 1 is returned.
*
*     Note that the routine only works for numeric columns and values.
*     For CHARACTER and LOGICAL columns and values it will compile but
*     usually will not produce sensible results.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     FI  =  INTEGER (Given)
*        Column identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     ORDER  =  INTEGER (Given)
*        Flag indicating the ordering of the column.  It must adopt
*        either of the following two values:
*        CAT__ASCND   -  for an ascending column,
*        CAT__DSCND   -  for an descending column.
*        All other values will cause the routine to exit with an error.
*     XVALW  =  INTEGER*2 (Given)
*        Target value for which the corresponding row is to be located.
*     L  =  INTEGER (Returned)
*        Row number corresponding to XVAL.  See the comments above for
*        the value returned if the target value is outside the range
*        of the column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Convert the value to DOUBLE PRECISION.
*     If the column is in ascending order then
*       If the first element is .LT. the target value then
*         If the last element is .GE. the target value then
*           (the target value lies in the range of the column)
*           Set the current row to (number of rows / 2).
*           Set the jump (to the next row) to the number of rows.
*           Do while (row corresponding to target no found)
*             If the current row corresponds to the target then
*               Set the termination flag.
*               Set the returned row number.
*             else
*               Compute the new jump (half the existing jump).
*               Force the jump to be at least one.
*               If the current row .GE. the target value then
*                 new row = current row - jump
*               else
*                 new row = current row + jump
*               end if
*             end if
*           end do
*         else (last element .LT. target)
*           Set the returned row number to number of rows + 1.
*         end if
*       else (first element .GE. than target) then
*         Set the returned row number to 0.
*       end if
*     else if the column is in descending order then
*       .
*       . As for an ascending column, but with the tests for
*       . inqualities reversed.
*       .
*     else (the column order is invalid).
*       Set the returned row to 0.
*       Set the error status.
*       Report an error.
*     end if
*  Implementation Deficiencies:
*     Does not handle null values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     15/8/94  (ACD): Original version.
*     8/9/94   (ACD): First stable version.
*     13/12/96 (ACD): Removed non-standard use of the relational
*       operators in the LOGICAL instantiation of the routine (which
*       had been revealed by the port to Linux).
*     26/5/97  (ACD): Fixed a bug which could cause the routine to loop
*       indefinitely if the target value lay between the first and
*       second rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FI,
     :  ROWS,
     :  ORDER

*
*   Note that in the instantiation of the routine for a given data type
*   only one of the following variables is an argument and all the
*   others are local variables.

      BYTE             XVALUB
      BYTE             XVALB
      INTEGER*2        XVALUW
      INTEGER*2        XVALW
      INTEGER          XVALI
      REAL             XVALR
      DOUBLE PRECISION XVALD
      LOGICAL          XVALL
      CHARACTER        XVALC*(CAT__SZVAL)
*  Arguments Returned:
      INTEGER
     :  L
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      LOGICAL
     :  FOUND,      ! Flag; found the row corresponding to the target?
     :  NULFLG      ! Null value flag for the current row.
      INTEGER
     :  CURROW,     ! Current row number.
     :  PRVROW,     ! Previous row number.
     :  JUMP,       ! Number of rows to jump.
     :  ERRPOS      ! Current position in ERRTXT (excl. trail. blanks).
      CHARACTER
     :  ERRTXT*75   ! Error message.
      DOUBLE PRECISION
     :  DXVAL,      ! DOUBLE PRECISION copy of the target value, VALW.
     :  XCUR,       ! Value read for the current row.
     :  XPREV       ! Value read for the previous row.

*
*    Dummy variables for arguments to the type conversion routine.

      BYTE             DUMUB
      BYTE             DUMB
      INTEGER*2        DUMUW
      INTEGER*2        DUMW
      INTEGER          DUMI
      REAL             DUMR
      LOGICAL          DUML
      CHARACTER        DUMC*(CAT__SZVAL)

      LOGICAL
     :  CONVOK      ! Flag; did value convert to DOUBLE PRECISION ok?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Convert the value to DOUBLE PRECISION.

         CALL CAT1_TCNVT (CAT__TYPEW, XVALUB, XVALB, XVALUW, XVALW,
     :     XVALI, XVALR, XVALD, XVALL, XVALC,   CAT__TYPED, DUMUB,
     :     DUMB, DUMUW, DUMW, DUMI, DUMR, DXVAL, DUML, DUMC,  CONVOK,
     :     STATUS)

*
*       Check for an ascending column.

         IF (ORDER .EQ. CAT__ASCND) THEN

*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .LT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .GE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .GE. DXVAL  .AND.  XPREV .LT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .GE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

*
*       Check for a descending column.

         ELSE IF (ORDER .EQ. CAT__DSCND) THEN


*
*          Check that the target value lies in the range of the column.

            CALL CAT_RGET (CI, 1, STATUS)
            CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

            IF (XCUR .GT. DXVAL) THEN
               CALL CAT_RGET (CI, ROWS, STATUS)
               CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

               IF (XCUR .LE. DXVAL) THEN

*
*                The target value does lie within the range of the
*                column.  Initialise for the binary chop.  Note that the
*                minimum permitted row is two because the previous row
*                must also be read.

                  CURROW = ROWS / 2
                  CURROW = MAX(CURROW, 2)
                  JUMP = ROWS
                  FOUND = .FALSE.

*
*                Perform a binary chop until the required row is found.

                  DO WHILE (.NOT. FOUND)

*
*                   Get values for the current and previous rows.

                     CALL CAT_RGET (CI, CURROW, STATUS)
                     CALL CAT_EGT0D (FI, XCUR, NULFLG, STATUS)

                     PRVROW = CURROW - 1

                     CALL CAT_RGET (CI, PRVROW, STATUS)
                     CALL CAT_EGT0D (FI, XPREV, NULFLG, STATUS)

*
*                   Check whether the required row has been found.

                     IF (XCUR .LE. DXVAL  .AND.  XPREV .GT. DXVAL) THEN
                        FOUND = .TRUE.
                        L = CURROW

                     ELSE

*
*                      The required row has not been found.  Jump to
*                      to the next row to be tested.  Note that the
*                      jump is forced to be at least one row.

                        JUMP = JUMP / 2
                        JUMP = MAX(JUMP, 1)

                        IF (XCUR .LE. DXVAL) THEN
                           CURROW = CURROW - JUMP
                        ELSE
                           CURROW = CURROW + JUMP
                        END IF

*
*                      Force the new row number to be in the range of
*                      the catalogue.  Note that the minimum permitted
*                      row is two because the previous row must also be
*                      read.

                        CURROW = MAX(CURROW, 2)
                        CURROW = MIN(CURROW, ROWS)
                     END IF
                  END DO
               ELSE

*
*                The value is beyond the last row of the catalogue.

                  L = ROWS + 1
               END IF
            ELSE

*
*             The value is before the first row of the catalogue.

               L = 0
            END IF

         ELSE

*
*          The column is neither ascending nor descending.  Set the
*          return value, set the error status and report an error.

            L = 0
            STATUS = CAT__INVSR

            CALL CHR_PUTC ('Column has an illegal order for a binary '/
     :        /'chop (code: ', ERRTXT, ERRPOS)
            CALL CHR_PUTI (ORDER, ERRTXT, ERRPOS)
            CALL CHR_PUTC (').', ERRTXT, ERRPOS)

            CALL CAT1_ERREP ('CAT1_CHOPW_ERR', ERRTXT(1 : ERRPOS),
     :        STATUS)
         END IF

      END IF

      END
