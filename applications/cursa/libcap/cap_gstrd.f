      SUBROUTINE CAP_GSTRD (SI, ROWS, NUMCOL, COLID, COLTYP, COLPTR,
     :  COLNNR, STATUS)
*+
*  Name:
*     CAP_GSTRD
*  Purpose:
*     Read values for a set of columns into corresponding arrays.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSTRD (SI, ROWS, NUMCOL, COLID, COLTYP, COLPTR,
*       COLNNR; STATUS)
*  Description:
*     Read values for a set of columns into corresponding arrays.
*     The values are read as DOUBLE PRECISION numbers.
*  Arguments:
*     SI  =  INTEGER (Given)
*        Identifier for the required selection or catalogue.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the required selection or catalogue.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns.
*     COLID(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns.
*     COLTYP(NUMCOL)  =  INTEGER (Given)
*        Integer codes for the data types of the columns.
*     COLPTR(NUMCOL)  =  INTEGER (Given)
*        Pointers to the DOUBLE PRECISION arrays to hold values read.
*        (Though the pointers are not themselves altered the contents
*        array elements are, of course.)
*     COLNNR(NUMCOL)  =  INTEGER (Returned)
*        Number of non-null rows read for each column.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of non-null rows for each column to zero.
*     For every row in the selection
*       Read in the next row.
*       For every column
*         If the column is numeric then
*           Attempt to get a DOUBLE PRECISION value for the field.
*           If the value is not null then
*             Increment the number of non-null rows for this column.
*             Add the value to the array for this column.
*           end if
*         else
*           Attempt to get a CHARACTER value for the field.
*           If the value is not null then
*             Increment the number of non-null rows for this column.
*           end if
*         end if
*       end do
*     end do
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     28/11/96 (ACD): Original version.
*     2/12/96  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Arguments Given:
      INTEGER
     :  SI,
     :  ROWS,
     :  NUMCOL,
     :  COLID(NUMCOL),
     :  COLTYP(NUMCOL),
     :  COLPTR(NUMCOL)
*  Arguments Returned:
      INTEGER
     :  COLNNR(NUMCOL)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  ROW,       ! Current row.
     :  CURCOL     ! Current column.
      DOUBLE PRECISION
     :  DVALUE     ! DOUBLE PRECISION value for the current field.
      CHARACTER
     :  CVALUE*(CAT__SZVAL) ! CHARACTER value for the current field.
      LOGICAL
     :  NULFLG     ! Null value flag for the current field.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the number of non-null rows for each column to zero.

         DO CURCOL = 1, NUMCOL
            COLNNR(CURCOL) = 0
         END DO

*
*       Examine every row in the selection.

         DO ROW = 1, ROWS
            CALL CAT_RGET (SI, ROW, STATUS)

*
*          Examine the data type of every column.  If it is numeric then
*          attempt to get a value for the current field; otherwise
*          simply check whether it is null.

            DO CURCOL = 1, NUMCOL
               IF (COLTYP(CURCOL) .NE. CAT__TYPEL  .AND.
     :             COLTYP(CURCOL) .NE. CAT__TYPEC) THEN
                  CALL CAT_EGT0D (COLID(CURCOL), DVALUE, NULFLG, STATUS)

                  IF (.NOT. NULFLG) THEN
                     COLNNR(CURCOL) = COLNNR(CURCOL) + 1
                     CALL CAP_STAED (ROWS, COLNNR(CURCOL), DVALUE,
     :                 %VAL(CNF_PVAL(COLPTR(CURCOL))), STATUS)
                  END IF

               ELSE
                  CALL CAT_EGT0C (COLID(CURCOL), CVALUE, NULFLG, STATUS)

                  IF (.NOT. NULFLG) THEN
                     COLNNR(CURCOL) = COLNNR(CURCOL) + 1
                  END IF
               END IF
            END DO
         END DO

      END IF

      END
