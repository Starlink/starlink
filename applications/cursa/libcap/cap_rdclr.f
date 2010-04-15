      SUBROUTINE CAP_RDCLR (CI, ID, ROWS, PTS, VAL, NUMNUL, STATUS)
*+
*  Name:
*     CAP_RDCLR
*  Purpose:
*     Read an expression from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RDSCT (CI, XID, ROWS, PTS, VAL; NUMNUL; STATUS)
*  Description:
*     Read an expression from a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ID  =  INTEGER (Given)
*        Identifier for the expression.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     PTS  =  INTEGER (Given)
*        Number of points returned.
*     VAL(ROWS)  =  REAL (Returned)
*        Values for the points.
*     NUMNUL  =  INTEGER (Returned)
*        Number of rows encountered for which the value was null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every row
*       Read the row.
*       Attempt to get the value.
*       If the value is not null then
*         Increment the number of points.
*         Copy the value to the return array.
*       else
*         Increment the number of null rows.
*       end if
*     end for
*     If the status is ok then
*       If no points were obtained then
*         Set the status.
*         Report an error.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/9/99 (ACD): Original version (from CAP_RDSCT).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ID,
     :  ROWS
*  Arguments Returned:
      INTEGER
     :  PTS,
     :  NUMNUL
      REAL
     :  VAL(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      REAL
     :  CUR      ! Value for the current row.
      LOGICAL
     :  NULL     ! Null flag for the current value.
      INTEGER
     :  ROW      ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Read all the rows.  Points are accepted only if the value is
*       not null.

         PTS = 0
         NUMNUL = 0

         DO ROW = 1, ROWS
            CALL CAT_RGET (CI, ROW, STATUS)

            CALL CAT_EGT0R (ID, CUR, NULL, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (.NOT. NULL) THEN
                  PTS = PTS + 1

                  VAL(PTS) = CUR

               ELSE
                  NUMNUL = NUMNUL + 1

               END IF
            END IF
         END DO

*
*       If the status is ok but all the points were null then set
*       the status and report an error.

         IF (STATUS .EQ. SAI__OK) THEN
            IF (PTS .LE. 0) THEN
               STATUS = SAI__OK

               CALL ERR_REP ('CAP_RDCLR_ERR', 'All the rows read '/
     :           /'contained null values.', STATUS)
            END IF
         END IF

      END IF

      END
