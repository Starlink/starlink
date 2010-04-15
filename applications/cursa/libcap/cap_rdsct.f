      SUBROUTINE CAP_RDSCT (CI, XID, YID, ROWS, PTS, XVAL, YVAL,
     :  NUMNUL, STATUS)
*+
*  Name:
*     CAP_RDSCT
*  Purpose:
*     Read two expressions from a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RDSCT (CI, XID, YID, ROWS, PTS, XVAL, YVAL,
*       NUMNUL; STATUS)
*  Description:
*     Read two expressions from a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     XID  =  INTEGER (Given)
*        Identifier for the X expression.
*     YID  =  INTEGER (Given)
*        Identifier for the Y expression.
*     ROWS  =  INTEGER (Given)
*        Number of rows in the catalogue.
*     PTS  =  INTEGER (Given)
*        Number of points returned.
*     XVAL(ROWS)  =  REAL (Returned)
*        X values for the points.
*     YVAL(ROWS)  =  REAL (Returned)
*        Y values for the points.
*     NUMNUL  =  INTEGER (Returned)
*        Number of rows encountered for which either X or Y was null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every row
*       Read the row.
*       Attempt to get the X value.
*       Attempt to get the Y value.
*       If neither value is null then
*         Increment the number of points.
*         Copy the values to the return arrays.
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
*     9/7/98 (ACD): Original version.
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
     :  XID,
     :  YID,
     :  ROWS
*  Arguments Returned:
      INTEGER
     :  PTS,
     :  NUMNUL
      REAL
     :  XVAL(ROWS),
     :  YVAL(ROWS)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      REAL
     :  XCUR,    ! X value for the current row.
     :  YCUR     ! Y   "    "   "     "     " .
      LOGICAL
     :  XNULL,   ! Null flag for the current X value.
     :  YNULL    !  "    "    "   "     "    Y   "  .
      INTEGER
     :  ROW      ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Read all the rows.  Points are accepted only if neither
*       the X nor the Y value is null.

         PTS = 0
         NUMNUL = 0

         DO ROW = 1, ROWS
            CALL CAT_RGET (CI, ROW, STATUS)

            CALL CAT_EGT0R (XID, XCUR, XNULL, STATUS)
            CALL CAT_EGT0R (YID, YCUR, YNULL, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (.NOT. XNULL  .AND.  .NOT. YNULL) THEN
                  PTS = PTS + 1

                  XVAL(PTS) = XCUR
                  YVAL(PTS) = YCUR

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

               CALL ERR_REP ('CAP_RDSCT_ERR', 'All the rows read '/
     :           /'contained null values.', STATUS)
            END IF
         END IF

      END IF

      END
