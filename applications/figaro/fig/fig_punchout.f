C+
      SUBROUTINE FIG_PUNCHOUT (DATA,NX,NY,IX1,IY1,IX2,IY2,FLAG)
C
C     F I G _ P U N C H O U T
C
C     Flags an area of an image as invalid, by setting all the
C     pixels to a pre-defined flag value.
C
C     Parameters -  (">" input, "<" output)
C
C     (<) DATA    (Real array DATA(NX,NY)) The image data
C     (>) NX      (Integer) The first dimension of DATA
C     (>) NY      (Integer) The second dimension of DATA
C     (>) IX1     (Integer) The first X value for the zapped area
C     (>) IY1     (Integer) The   "   Y   "    "   "    "     "
C     (>) IX2     (Integer) The last  X   "    "   "    "     "
C     (>) IY2     (Integer) The   "   Y   "    "   "    "     "
C     (>) FLAG    (Real) The value to be used to flag invalid pixels
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                            KS / CIT 22nd Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX1,IY1,IX2,IY2
      REAL    DATA(NX,NY),FLAG
C
C     Local variables
C
      INTEGER IX,IY
C
C     Zap the area
C
      DO IY=MAX(1,IY1),MIN(NY,IY2)
         DO IX=MAX(1,IX1),MIN(NX,IX2)
            DATA(IX,IY)=FLAG
         END DO
      END DO
C
      END
