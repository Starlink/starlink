C+
      REAL FUNCTION FIG_CLOSEST (DATA,NX,NY,IX,IY,FLAG)
C
C     F I G _ C L O S E S T
C
C     Returns the value of the closest pixel in DATA to IX,IY
C     that has not been flagged as invalid.  The algorithm spirals
C     out looking for good pixels, and eventually gives up when
C     the spiral has got ridiculously large.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The image data
C     (>) NX      (Integer) The first dimension of DATA
C     (>) NY      (Integer) The second dimension of DATA
C     (>) IX      (Integer) The position of the target pixel in X
C     (>) IY      (Integer) The position of the target pixel in Y
C     (>) FLAG    (Real) The value usedto flag invalid pixels
C
C     Returns -
C
C     FIG_CLOSEST (Real) The value of the closest pixel (zero if
C                 the routine gives up).
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C     This routine is based on an original routine by John Tonry
C
C                                          KS / CIT 29th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX,IY
      REAL    DATA(NX,NY),FLAG
C
C     Local variables
C
      INTEGER I,J,K,L,IXV,IYV
      REAL VALUE
C
C     First check the origin pixel
C
      IF ((IX.LE.0).OR.(IY.LE.0).OR.(IX.GT.NX).OR.(IY.GT.NY)) THEN
         VALUE=0.
         GO TO 100
      END IF
      VALUE=DATA(IX,IY)
      IF (VALUE.EQ.FLAG) THEN
C
C        Scan the other pixels nearby
C
         DO J=1,10
            DO I=0,8*J-1
               K=MOD(I,J+J)-J
               L=I/(J+J)+1
               GO TO (1,2,3,4),L
    1          CONTINUE
                 IXV=IX+K
                 IYV=IY+J
                 GO TO 5
    2          CONTINUE
                 IXV=IX+J
                 IYV=IY-K
                 GO TO 5
    3          CONTINUE
                 IXV=IX-K
                 IYV=IY-J
                 GO TO 5
    4          CONTINUE
                 IXV=IX-J
                 IYV=IY+K
    5          CONTINUE
               IF ((IXV.LE.NX).AND.(IXV.GE.1)
     :             .AND.(IYV.LE.NY).AND.(IYV.GE.1)) THEN
                  VALUE=DATA(IXV,IYV)
                  IF (VALUE.NE.FLAG) THEN
                     GO TO 100
                  END IF
               END IF
            END DO
         END DO
         VALUE=0.
      END IF
C
  100 CONTINUE
      FIG_CLOSEST=VALUE
C
      END
