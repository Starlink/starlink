      SUBROUTINE MSC_POSUB(N, X, Y, MAXS, MAXL, LMIN, LMAX, SMIN, SMAX)

*+
*
*   Name:
*      SUBROUTINE MSC_POSUB
*
*   Description:
*      Given a set of points defining a polygon, give the pixel subset of
*      an image contained by this shape.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER N              ! number of vertices

      REAL*8 X(N)              ! x-vertices
      REAL*8 Y(N)              ! y-vertices

      INTEGER MAXS           ! maximum number of samples per line
      INTEGER MAXL           ! maximum number of image lines

*   Export:
      INTEGER LMIN           ! minimum image line contained
      INTEGER LMAX           ! maximum image line contained
      INTEGER SMIN(MAXL)     ! minimum image sample per line
      INTEGER SMAX(MAXL)     ! maximum image sample per line

*   Local variables:
      REAL*8 DX                ! side offset
      REAL*8 DXDY              ! side gradient
      REAL*8 X1                ! side start x-value
      REAL*8 X2                ! side end x-value
      REAL*8 Y1                ! side start y-value
      REAL*8 Y2                ! side end y-value

      INTEGER I              ! loop index
      INTEGER IL             ! int il
      INTEGER IL1            ! line start
      INTEGER IL2            ! line end
      INTEGER IS             ! sample index
      INTEGER IS1            ! sample start
      INTEGER IS2            ! sample end

*   Initialise
      DO IL = 1, MAXL
         SMIN(IL) = MAXS + 1
         SMAX(IL) = 0
      END DO

*   For each side of the polygon
      X2 = X(N)
      Y2 = Y(N)

      DO 200 I = 1, N

*      Create line
         X1 = X2
         Y1 = Y2
         X2 = X(I)
         Y2 = Y(I)

*      Linear interpolation
         IL1 = NINT(REAL(Y1))
         IL2 = NINT(REAL(Y2))
         IF (IL2.LT.IL1) CALL MSC_ISWAP(IL1, IL2)

*      Constrain line range
         IL1 = MAX(1, IL1)
         IL2 = MIN(MAXL, IL2)

         IF (IL2.GE.IL1) THEN

            IF (Y2.EQ.Y1) THEN

               IS1 = NINT(REAL(X1))
               IS2 = NINT(REAL(X2))
               IF (IS2.GT.IS1) CALL MSC_ISWAP(IS1, IS2)

*            Constrain sample range
               IS1 = MAX(1, IS1)
               IS2 = MIN(MAXS, IS2)
               SMIN(IL1) = MIN(SMIN(IL1), IS1)
               SMAX(IL1) = MAX(SMAX(IL1), IS2)

            ELSE

*            Create straight line representation
               DXDY = (X2 - X1) / (Y2 - Y1)
               DX = X1 - Y1 * DXDY

               DO IL = IL1, IL2
                  IS = NINT(REAL(DXDY) * REAL(IL) + REAL(DX))
                  SMIN(IL) = MIN(SMIN(IL), IS)
                  SMAX(IL) = MAX(SMAX(IL), IS)
               END DO
            END IF
         END IF

 200  CONTINUE

*   Initialise LMIN, LMAX
      LMIN = 1
      LMAX = MAXL

*   Update lmin to exclude empty lines
      IL1 = LMIN

      DO 300 IL = IL1, LMAX

         IF (SMIN(IL).LE.SMAX(IL)) GO TO 400
         LMIN = IL + 1

 300  CONTINUE

*   Update lmax to exclude empty lines
 400  CONTINUE

      IF (LMIN.LE.LMAX) THEN

         IL2 = LMAX

         DO 450 IL = IL2, LMIN, -1

            IF (SMIN(IL).LE.SMAX(IL)) GO TO 500
            LMAX = IL - 1

 450     CONTINUE

      END IF

 500  CONTINUE

      END
