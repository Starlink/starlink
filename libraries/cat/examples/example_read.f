      SUBROUTINE EXAMPLE_READ (STATUS)
*+
*  Name:
*     EXAMPLE_READ
*  Purpose:
*     Example program to demonstrate reading a catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL EXAMPLE_READ (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Example program to demonstrate reading a catalogue.
*  Algorithm:
*     Attempt to open the catalogue.
*     Obtain the number of rows in the catalogue.
*     Obtain identifiers for the columns.
*     List the columns.
*     Release the identifier for the catalogue.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     7/10/93  (ACD): Original version.
*     28/1/94  (ACD): Re-written as a proper ADAM application.
*     21/2/94  (ACD): Removed unused variable.
*     23/11/94 (ACD): Changed the output from Fortran unit 17 to the
*        default output stream.
*     19/4/95  (ACD): Tidied up the output statements.
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  CI,       ! Catalogue identifier.
     :  GI1,      ! Identifier to the first  column.
     :  GI2,      !     "      "   "  second   "   .
     :  GI3       !     "      "   "  third    "   .
      INTEGER
     :  LOOP,     ! Loop index.
     :  ROWS,     ! Number of rows in the catalogue.
     :  VAL1      ! Value obtained for first (integer) column.
      REAL
     :  VAL2      ! Value obtained for second (real) column.
      CHARACTER
     :  VAL3*15   ! Value obtained for third (character) column.
      LOGICAL
     :  NUL1,     ! Null value flag for the first  column.
     :  NUL2,     !  "     "    "    "   "  second   "   .
     :  NUL3      !  "     "    "    "   "  third    "   .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to open the catalogue.

         CALL CAT_ASSOC ('CNAME', 'READ', CI, STATUS)

*
*       Obtain the number of rows in the catalogue.

         CALL CAT_TROWS (CI, ROWS, STATUS)

*
*       Obtain identifiers for the columns.

         CALL CAT_TIDNT (CI, 'COLI', GI1, STATUS)
         CALL CAT_TIDNT (CI, 'COLR', GI2, STATUS)
         CALL CAT_TIDNT (CI, 'COLC', GI3, STATUS)

*
*       List the columns.

         WRITE(*, 2000)
 2000    FORMAT(1X, 'LOOP',   T11, 'COLI',   T23, 'COLR',
     :     T38, 'COLC',   T61, 'STATUS' / )

         DO LOOP = 1, ROWS
            CALL CAT_RGET (CI, LOOP, STATUS)

            CALL CAT_EGT0I (GI1, VAL1, NUL1, STATUS)
            IF (NUL1) VAL1 = 0

            CALL CAT_EGT0R (GI2, VAL2, NUL2, STATUS)
            IF (NUL2) VAL2 = 0.0E0

            CALL CAT_EGT0C (GI3, VAL3, NUL3, STATUS)
            IF (NUL3) VAL3 = ' '

            WRITE(*, 2001) LOOP, VAL1, NUL1, VAL2, NUL2, VAL3, NUL3,
     :        STATUS
 2001       FORMAT(1X, I4,   3X, I6,  L2,   3X, 1PE12.3, L2,
     :        3X, A15, L2,   1X, I12  )
         END DO

*
*       Release the catalogue identifier.

         CALL CAT_TRLSE (CI, STATUS)

      END IF

      END
