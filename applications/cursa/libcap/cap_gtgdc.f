      SUBROUTINE CAP_GTGDC (CI, ROWS, NCOL, FID, COLPTR, PTS, NULLS,
     :  STATUS)
*+
*  Name:
*     CAP_GTGDC
*  Purpose:
*     Read in columns to be assembled into a grid or histogram.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GTGDC (CI, ROWS, NCOL, FID, COLPTR; PTS, NULLS; STATUS)
*  Description:
*     Read in columns to be assembled into a grid or histogram.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     NCOL  =  INTEGER (Given)
*        Number of columns to be read.
*     FID(NCOL)  =  INTEGER (Given)
*        Identifiers for the columns to be read.
*     COLPTR(NCOL)  =  INTEGER (Given)
*        Points to the arrays to hold the values read.
*     PTS  =  INTEGER (Returned)
*        Number of points read.
*     NULLS  =  INTEGER (Returned)
*        Number of rows rejected because of null values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every row
*       Move to this row.
*       For every column
*         Attempt to get the value.
*         If ok and not null then
*           Copy the value to the array.
*         else
*           Set the bad row flag.
*         end if
*       end for
*       If the bad row flag is set then
*         Increment the number of rows rejected because of null values.
*         Decrement the number of points read.
*       end if
*     end for
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version.
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
     :  CI,
     :  ROWS,
     :  NCOL,
     :  FID(NCOL),
     :  COLPTR(NCOL)
*  Arguments Returned:
      INTEGER
     :  PTS,
     :  NULLS
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  ROW,     ! Current row.
     :  CURCOL   ! Current column.
      REAL
     :  CURVAL   ! Value read for the current field.
      LOGICAL
     :  BADROW,  ! Flag; any nulls encountered for the current row?
     :  NULFLG   ! Flag; is the current field null?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Read all the rows in the catalogue.

         PTS = 0
         NULLS = 0

         DO ROW =1, ROWS

*
*          Move to the next row.

            PTS = PTS + 1
            CALL CAT_RGET (CI, ROW, STATUS)

*
*          Attempt to get a value for each column.  If ok then store
*          it in the return array.  Otherwise set the bad row flag.

            BADROW = .FALSE.

            DO CURCOL = 1, NCOL
               CALL CAT_EGT0R (FID(CURCOL), CURVAL, NULFLG, STATUS)

               IF (STATUS .EQ. SAI__OK  .AND.  .NOT. NULFLG) THEN
                  CALL CAP_STAER (ROWS, PTS, CURVAL,
     :              %VAL(CNF_PVAL(COLPTR(CURCOL))), STATUS)
               ELSE
                  BADROW = .TRUE.
               END IF
            END DO

*
*          If a null value was encountered in the row, or there was
*          some other mishap, then increment the number of nulls and
*          decrement the number of points.

            IF (BADROW) THEN
               NULLS = NULLS + 1
               PTS = PTS - 1
            END IF
         END DO

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_GTGDC_ERR', 'Failure reading '/
     :        /'columns of values.', STATUS)
         END IF

      END IF

      END
