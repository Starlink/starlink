C+
      SUBROUTINE FIG_FIXROWSQ (DATA,QUAL,NX,NY,LX1,LX2,LY1,LY2,NROW,
     :                                      IROWS,NCOEFF)
C
C     F I G _ F I X R O W S Q
C
C     Fixes a series of bad rows, by interpolating over them.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) DATA      (Real array DATA(NX,NY)) The image data.
C     (!) QUAL      (Byte array QUAL(NX,NY)) Quality array for DATA.
C     (>) NX        (Integer) The first dimension of DATA
C     (>) NY        (Integer) The second dimension of DATA.
C     (>) LX1       (Integer) Pixels outside a subset of DATA given by
C     (>) LX2       (Integer) the limits LY1,LY2 (in the 2nd dimension)
C     (>) LY1       (Integer) and LX1,LX2 (in the first dimension) will
C     (>) LY2       (Integer) not be changed by this routine.
C     (>) NROW      (Integer) The number of rows to be fixed.
C     (>) IROWS     (Integer array IROWS(NROW)) The rows to be fixed
C                   - ie their y-positions.
C     (>) NCOEFF    (Integer) The number of coefficients to be used
C                   in the interpolation.  1=>NCOEFF=>8.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_VERTICALQ (FIG_ package) Fix a single element by a vertical fit.
C
C     Adapted 1998 from FIG_FIXROWS.
C
C                                          MBT / IoA 30th Jul 1998
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,LX1,LX2,LY1,LY2,NROW,IROWS(NROW),NCOEFF
      REAL    DATA(NX,NY)
      BYTE    QUAL(NX,NY)
C
C     Workspace dimensions
C
      INTEGER MAXPT
      PARAMETER (MAXPT=35)
C
C     Local variables
C
      INTEGER ROW,IC,IX,LIMY1,LIMY2,STATUS
      REAL Y(MAXPT),Z(MAXPT)
C
C     Loop through rows
C
      DO IC=1,NROW
         ROW=IROWS(IC)
C
C        Ignore row if outside Y bounds
C
         IF ((ROW.GE.LY1).AND.(ROW.LE.LY2)) THEN
C
C           Set the interpolation limits and fix the row
C
            LIMY2=ROW+2*NCOEFF
            LIMY1=ROW-2*NCOEFF
            DO IX=LX1,LX2
               CALL FIG_VERTICALQ(DATA,QUAL,NX,NY,IX,ROW,ROW,LIMY1,
     :                             LIMY2,NCOEFF,MAXPT,Y,Z,STATUS)
            END DO
         END IF
      END DO
C
      END
