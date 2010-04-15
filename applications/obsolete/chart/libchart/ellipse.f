       SUBROUTINE ELLIPSE(A,B,PA,X,Y,N, STATUS )
C
C
C         SUBROUTINE ELLIPSE
C
C
C         This  routine  generates  a  set  of  (x,y)  co-ordinates   which
C         represent  an  ellipse  at  any angle. The output is suitable for
C         plotting by any polyline routine.
C
C         A           REAL  IN    This is the size of the longest  diameter
C                                 of the ellipse.
C
C         B           REAL  IN    This is the size of the shortest diameter
C                                 of the ellipse.
C
C         PA          REAL  IN    This is the position angle of  the  major
C                                 axis in degrees.
C
C         X           REAL  OUT   This is the array (of dimension N)  which
C                                 contains the x co-ordinates whi have been
C                                 generated.
C
C         Y           REAL  OUT   This is the array (of dimension N)  which
C                                 contains the y co-ordinates whi have been
C                                 generated.
C
C         N           INT   IN    This defines the number of points  to  be
C                                 generated.

*     STATUS = INTEGER (Given and Returned)
*        The global status.
C
C
*   History:

C         M J Currie               RGO-Starlink                   24-MAY-82
C         Modified 20-Jul-84 by J.V.Carey
C         Modified  7-Feb-85 by B.M.Harris
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     3-AUG-2004 (TIMJ):
*       Remove non-portable COSD,SIND,ATAND
C
C--------------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      REAL PI
      PARAMETER ( PI = 3.141592654 )

      INTEGER I,N
*  Status:
      INTEGER STATUS             ! Global status

      REAL A,AA,AABB,ANG1,ANG2,ANGR1,ANGR2,ANGR3,ANGR4,B,BB,PA,PA1
      REAL STEP,TH,X(N),Y(N)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*    Deal with ellipses
*

      IF (B.EQ.0.0) B = A

      AA = A/2.0
      BB = B/2.0

      IF (A.GT.0.0) THEN

         PA1  = (PA + 90.0) * PI / 180.0

         IF (AA.GT.0.0) THEN
*     Step in degrees
            STEP = 360.0/REAL(N-1)
            X(1) = AA*COS(PA1)
            Y(1) = AA*SIN(PA1)

            DO I = 2,N
*     Converted to radians
               TH   = STEP*REAL(I-1) * PI / 180.0
               X(I) = AA*COS(PA1)*COS(TH) - BB*SIN(PA1)*SIN(TH)
               Y(I) = AA*SIN(PA1)*COS(TH) + BB*COS(PA1)*SIN(TH)
            ENDDO

         ENDIF
*
*   For rectangles, calculate 4 corners
*
      ELSE
         AA = -AA
         BB = -BB
*
*       Calculate half the length of each axis and half the diagonal
*
         AABB = SQRT(AA*AA + BB*BB)
*
*       Calculate the angle from zero to the line joining each corner to
*       the centre point, adding the angle through which the user wishes
*       to rotate the major axis.
*
         ANG1 = ATAN(AA/BB)
         ANG2 = PI - ANG1

         ANGR1 = PA + ANG1
         ANGR2 = PA + ANG2
         ANGR3 = PI + ANGR1
         ANGR4 = PI + ANGR2
*
*       Now calculate the x,y values for the four corner points.
*
         X(1) =   AABB*COS(ANGR1)
         Y(1) =   AABB*SIN(ANGR1)
         X(2) =   AABB*COS(ANGR2)
         Y(2) =   AABB*SIN(ANGR2)
         X(3) =   AABB*COS(ANGR3)
         Y(3) =   AABB*SIN(ANGR3)
         X(4) =   AABB*COS(ANGR4)
         Y(4) =   AABB*SIN(ANGR4)
      ENDIF

      END
