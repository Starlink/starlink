C+
      SUBROUTINE FIG_FIXCOLS (DATA,NX,NY,LX1,LX2,LY1,LY2,NCOL,
     :                                      ICOLS,NCOEFF,FLAG)
C
C     F I G _ F I X C O L S
C
C     Fixes a series of bad columns, by interpolating over them.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) DATA      (Real array DATA(NX,NY)) The image data.
C     (>) NX        (Integer) The first dimension of DATA
C     (>) NY        (Integer) The second dimension of DATA.
C     (>) LX1       (Integer) Pixels outside a subset of DATA given by
C     (>) LX2       (Integer) the limits LY1,LY2 (in the 2nd dimension)
C     (>) LY1       (Integer) and LX1,LX2 (in the first dimension) will
C     (>) LY2       (Integer) not be changed by this routine.
C     (>) NCOL      (Integer) The number of columns to be fixed.
C     (>) ICOLS     (Integer array ICOLS(NCOL)) The columns to be fixed
C                   - ie their x-positions.
C     (>) NCOEFF    (Integer) The number of coefficients to be used
C                   in the interpolation.  1=>NCOEFF=>8.
C     (>) FLAG      (Real) Value used to flag invalid pixels
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_HORIZONTAL (FIG_ package) Fix a single element by a horiz. fit.
C
C                                                 KS / CIT 1st March 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,LX1,LX2,LY1,LY2,NCOL,ICOLS(NCOL),NCOEFF
      REAL    DATA(NX,NY),FLAG
C
C     Workspace dimensions
C
      INTEGER MAXPT
      PARAMETER (MAXPT=35)
C
C     Local variables
C
      INTEGER COL,IC,IY,LIMX1,LIMX2,STATUS
      REAL X(MAXPT),Z(MAXPT)
C
C     Loop through columns
C
      DO IC=1,NCOL
         COL=ICOLS(IC)
C
C        Ignore column if outside X  bounds
C
         IF ((COL.GE.LX1).AND.(COL.LE.LX2)) THEN
C
C           Set the interpolation limits and fix the column
C
            LIMX2=COL+2*NCOEFF
            LIMX1=COL-2*NCOEFF
            DO IY=LY1,LY2
               CALL FIG_HORIZONTAL(DATA,NX,NY,IY,COL,COL,LIMX1,
     :                             LIMX2,NCOEFF,FLAG,MAXPT,X,Z,STATUS)
            END DO
         END IF
      END DO
C
      END
