      SUBROUTINE CAP_CLCNT (N, X, Y, X0, Y0, XP, YP, NCROSS, STATUS)
*+
*  Name:
*     CAP_CLCNT
*  Purpose:
*     No. of times a line segment crosses a polygon boundary.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CLCNT (N, X, Y, X0, Y0, XP, YP; NCROSS; STATUS)
*  Description:
*     Takes arrays X and Y of N coordinates which contain the vertices
*     of a polygon and calculates how many times, NCROSS, the line
*     segment from (X0,Y0) to (XP,YP) crosses the region boundary.
*  Arguments:
*     N  =  INTEGER (Given)
*           Number of vertices (corners) of the polygon.
*     X(N)  =  REAL (Given)
*           X coords. of the polygon vertices.
*     Y(N)  =  REAL (Given)
*           Y coords. of the polygon vertices.
*     X0  =  REAL (Given)
*           X coord. of one end of the line segment.
*     Y0  =  REAL (Given)
*           Y coord. of one end of the line segment.
*     XP  =  REAL (Given)
*           X coord. of the other end of the line segment.
*     YP  =  REAL (Given)
*           Y coord. of the other end of the line segment.
*     NCROSS  =  INTEGER (Returned)
*           Number of times the line segment crosses the polygon
*           boundary.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the number of crossings to zero.
*     for all corneres except the first do
*       if coords. of corner not the same as those of the previous
*       corner then
*         Check if the line segment from this corner to the previous
*         corner intersects the input line segment.
*         If it does intersect then
*           increment the no. of crossings
*         end if
*       end if
*     end do
*     if the coords. of the last corner are not equal to those of the first
*     then
*       check if the line segment joining the last and first corners
*       crosses the input segment
*       if it does cross then
*         increment the no. of crossings.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ASOC1: D.R.K.Brownrigg  (Edinburgh)
*     ACD:   A C Davenhall    (Edinburgh)
*  History:
*     16/7/82 (ASOC1): Original version.
*     28/2/84   (ACD): Modified to "SSE" style.
*     16/6/86   (ACD): Converted from Haggis to SCAR.
*     17/6/96   (ACD): Converted from SCAR to CURSA.
*     24/10/96  (ACD): First stable CURSA version.  Included a fix
*        to prevent testing REAL numbers for equality.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     : N      ! Number of vertices (corners) of polygon input.
      REAL
     : X(N),  ! X coords. of polygon vertices.
     : Y(N),  ! Y   "   . "     "       "    .
     : X0,    ! X coord. of one end of the line segment.
     : Y0,    ! Y   "  . "   "   "  "   "   "      "   .
     : XP,    ! X coord. of the other end of the line segment.
     : YP     ! Y   "  . "   "    "    "  "   "   "      "   .
*  Arguments Returned:
      INTEGER
     :  NCROSS ! No. of times the line crosses the polygon boundary.
*  Status:
      INTEGER STATUS             ! Global status
*  Local Constants:
      REAL MINVAL      ! Minimum permitted difference from zero.
      PARAMETER (MINVAL = 1.0E-8)
*  Local Variables:
      INTEGER
     :  I,     ! Number of the corner currently being examined.
     :  INT    ! Flag showing whether line crosses a given polgon side.
      REAL
     :  DXI1I, ! Difference: X(I-1) - X(I)
     :  DYI1I, !     "     : Y(I-1) - Y(I)
     :  DXN1,  !     "     : X(N) - X(1)
     :  DYN1   !     "     : Y(N) - Y(1)
*.

*
*    INT (returned by CAP_CLINT);
*      = 1  -  The 2 input segments cross.
*      = 0  -  The 2 input segments do not cross.

      IF (STATUS .EQ. SAI__OK) THEN

         NCROSS = 0

         DO I = 2, N

            DXI1I = X(I-1) - X(I)
            DYI1I = Y(I-1) - Y(I)

            IF (ABS(DXI1I) .GE. MINVAL .OR. ABS(DYI1I) .GE. MINVAL)
     :        THEN
               CALL CAP_CLINT (X(I-1), Y(I-1), X(I), Y(I), X0, Y0,
     :           XP, YP, INT, STATUS)
               IF (INT .EQ. 1) THEN
                  NCROSS = NCROSS + 1
               END IF
            END IF
         END DO

         DXN1 = x(N) - x(1)
         DYN1 = Y(N) - Y(1)

         IF (ABS(DXN1) .GE. MINVAL .OR. ABS(DYN1) .GE. MINVAL)
     :     THEN
            CALL CAP_CLINT (X(N), Y(N), X(1), Y(1), X0, Y0, XP, YP,
     :        INT, STATUS)
            IF (INT .EQ. 1) THEN
               NCROSS = NCROSS + 1
            END IF
         END IF

      END IF

      END
