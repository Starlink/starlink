      SUBROUTINE CAP_RDPLY (CIPOLY, XCRNRI, YCRNRI, CORNER, XCORNR,
     :  YCORNR, STATUS)
*+
*  Name:
*     CAP_RDPLY
*  Purpose:
*     Read in the polygon corners (or vertices).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RDPLY (CIPOLY, XCRNRI, YCRNRI, CORNER, XCORNR, YCORNR,
*       STATUS)
*  Description:
*     Read in the polygon corners (or vertices).  Note that the
*     coordinates of the corners should not contain null values and any
*     null values which are encountered are considered to constitute
*     an error.
*  Arguments:
*     CIPOLY  =  INTEGER (Given)
*        Identifier to the polygon catalogue.
*     XCRNRI  =  INTEGER (Given)
*        Identifier for the X coordinate column in the polygon catalogue.
*     YCRNRI  =  INTEGER (Given)
*        Identifier for the Y coordinate column in the polygon catalogue.
*     CORNER  =  INTEGER (Given)
*        Number of corners (or vertices) in the polygon catalogue.
*     XCORNR(CORNER)  =  REAL (Returned)
*        Work array for the X coordinates of the polygon corners.
*     YCORNR(CORNER)  =  REAL (Returned)
*        Work array for the Y coordinates of the polygon corners.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each row
*       Attempt to read values for the row.
*       If ok then
*         If neither of the coordinates is null then
*           Copy the values to the return array.
*         else
*           Report a message: null values have been encountered.
*         end if
*       end if
*     end for
*     If the status is ok then
*       If any null values were found
*         Set the status.
*         Report an error.
*       end if
*     else
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/6/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  CIPOLY,
     :  XCRNRI,
     :  YCRNRI,
     :  CORNER
*  Arguments Returned:
      REAL
     :  XCORNR(CORNER),
     :  YCORNR(CORNER)
*  Status:
      INTEGER STATUS            ! Global status.
*  Local Variables:
      INTEGER
     :  CURCOR   ! Current corner in the polygon.
      LOGICAL
     :  NULFLX,  ! Flag; null value read for X coordinate?
     :  NULFLY,  !  "  ;  "     "    "    "  Y     "     ?
     :  NULFND   ! Flag; any null values found?
      REAL
     :  XCUR,    ! X coordinate read for current polygon corner.
     :  YCUR     ! Y     "       "    "     "       "      "   .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to read in the polygon corners.  If any null values are
*       found a message is reported and a flag is set, but the error
*       status is not set at this stage (so that the entire polygon
*       catalogue can be checked for null coordinates).

         NULFND = .FALSE.

         DO CURCOR = 1, CORNER
            XCORNR(CURCOR) = 0.0E0
            YCORNR(CURCOR) = 0.0E0

            CALL CAT_RGET (CIPOLY, CURCOR, STATUS)

            CALL CAT_EGT0R (XCRNRI, XCUR, NULFLX, STATUS)
            CALL CAT_EGT0R (YCRNRI, YCUR, NULFLY, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               IF (.NOT. NULFLX  .AND.  .NOT. NULFLY) THEN
                  XCORNR(CURCOR) = XCUR
                  YCORNR(CURCOR) = YCUR

               ELSE
                  NULFND = .TRUE.

                  CALL MSG_SETI ('CURCOR', CURCOR)
                  CALL MSG_OUT (' ', 'Null value encountered in row '/
     :              /'^CURCOR of the polygon catalogue.', STATUS)
               END IF
            END IF
         END DO

C        do curcor = 1, corner
C           print2000, curcor, xcornr(curcor), ycornr(curcor)
C2000       format(1x, 'input corner, x, y: ', i5, 1pe15.3, 1pe15.3)
C        end do

*
*       Report any error, or if any null values were found the set the
*       status and report an error.

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NULFND) THEN
               STATUS = SAI__ERROR

               CALL ERR_REP ('CAP_RDPLY_NULL', 'CAP_RDPLY: failure '/
     :           /'because null values encountered in polygon '/
     :           /'catalogue.', STATUS)
            END IF
         ELSE
            CALL ERR_REP ('CAP_RDPLY_ERR', 'CAP_RDPLY: failure '/
     :           /'reading polygon catalogue.', STATUS)
         END IF

      END IF

      END
