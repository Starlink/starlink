C+
      REAL FUNCTION FIG_CLOSESTQ (DATA,QUAL,NX,NY,IX,IY)
C
C     F I G _ C L O S E S T Q
C
C     Returns the value of the closest pixel in DATA to IX,IY
C     that is not indicated as invalid by the corresponding quality
C     array.  The algorithm spirals out looking for good pixels, and
C     eventually gives up when the spiral has got ridiculously large.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The image data
C     (>) QUAL    (Byte array QUAL(NX,NY)) Quality of the image data
C     (>) NX      (Integer) The first dimension of DATA
C     (>) NY      (Integer) The second dimension of DATA
C     (>) IX      (Integer) The position of the target pixel in X
C     (>) IY      (Integer) The position of the target pixel in Y
C
C     Returns -
C
C     FIG_CLOSESTQ (Real) The value of the closest pixel (zero if
C                  the routine gives up).
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C     This routine is based on an original routine by John Tonry
C     Adapted 1998 from FIG_CLOSEST.
C
C                                          MBT / IoA 30th Jul 1998
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX,IY
      REAL    DATA(NX,NY)
      BYTE    QUAL(NX,NY)
C
C     Local variables
C
      INTEGER I,J,K,L,IXV,IYV
      REAL VALUE
      BYTE GOOD
C
C     Constants
C
      PARAMETER (GOOD=0)             ! Value of quality array for valid data
C

C
C     First check the origin pixel
C
      IF ((IX.LE.0).OR.(IY.LE.0).OR.(IX.GT.NX).OR.(IY.GT.NY)) THEN
         VALUE=0.
         GO TO 100
      END IF
      VALUE=DATA(IX,IY)
      IF (QUAL(IX,IY).NE.GOOD) THEN
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
                  IF (QUAL(IXV,IYV).NE.GOOD) THEN
                     GO TO 100
                  END IF
               END IF
            END DO
         END DO
         VALUE=0.
      END IF
C
  100 CONTINUE
      FIG_CLOSESTQ=VALUE
C
      END
