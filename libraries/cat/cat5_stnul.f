      SUBROUTINE CAT5_STNUL (NULVAL, ROWS, NULCOL, STATUS)
*+
*  Name:
*     CAT5_STNUL
*  Purpose:
*     Set a column of null value flags to a single value.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_STNUL (NULVAL, ROWS; NULCOL; STATUS)
*  Description:
*     Set a column of null value flags to a single value.
*  Arguments:
*     NULVAL  =  LOGICAL (Given)
*        Value to which the column is to be set.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the column.
*     NULCOL(ROWS)  =  LOGICAL (Returned)
*        Column of null value flags.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set every row in the column to the required value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/7/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      LOGICAL
     :  NULVAL
      INTEGER
     :  ROWS
*  Arguments Returned:
      LOGICAL
     :  NULCOL(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP                     ! Loop index.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         DO LOOP = 1, ROWS
            NULCOL(LOOP) = NULVAL
         END DO

      END IF

      END
