C+
      SUBROUTINE FIG_HORIZONTAL (DATA,NX,NY,IY,IX1,IX2,LIMX1,LIMX2,
     :                               NCOEFF,FLAG,MAXPT,X,Z,STATUS)
C
C     F I G _ H O R I Z O N T A L
C
C     Routine to interpolate across a feature in a single horizontal
C     cut through an image.  Data within a range on either side of
C     the feature are fitted and the pixels delimiting the feature are
C     replaced by the interpolated values.  Note that the operation
C     of this routine is limited to that single horizontal cut, so to
C     fix a long vertical feature this routine has to be called
C     repeatedly.
C
C     Parameters - (">" input, "<" output, "W" workspace, "!" modified)
C
C     (!) DATA     (Real array DATA(NX,NY)) The image data.
C     (>) NX       (Integer) The first (horizontal) dimension of DATA.
C     (>) NY       (Integer) The second (vertical) dimension of DATA.
C     (>) IY       (Integer) The y-coordinate of the horizontal cut.
C     (>) IX1      (Integer) The lower x-value of the range of data
C                  to be fixed.
C     (>) IX2      (Integer) The higher x-value of the range of data
C                  to be fixed.  If IX1=IX2 only one pixel is modified.
C     (>) LIMX1    (Integer) The lower bound of the range of data to be
C                  used for the fit.
C     (>) LIMX2    (Integer) The upper bound of the range of data to be
C                  used for the fit.  The fit is therefore normally to the
C                  pixels in the ranges LIMX1 to I1-1 and I2+1 to LIMX2
C                  (inclusive).  The range may be extended a little if too
C                  many of the pixels in this range are flagged as invalid.
C     (>) NCOEFF   (Integer) The number of coefficients for the fit.
C                  Must be 8 or less.
C     (>) FLAG     (Real) Value used to indicate invalid pixels.
C     (>) MAXPT    (Integer) Maximum number of points that can be used
C                  for the fit - ie the dimension of X and Z.
C     (W) X        (Real array X(MAXPT)) Workspace.
C     (W) Z        (Real array Z(MAXPT)) Workspace.
C     (<) STATUS   (Integer) Return status code. 0 => OK, 1 => some
C                  error - ie no points to fit.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     JTY_FITLPOLY     (JT package) Fits Legendre polynomials to data
C     JTY_LPOLY        ( "   "    ) Evaluates Legendre polynomials
C
C     This routine is based on an original routine by John Tonry.
C
C                                            KS / CIT 24th Feb 1984
C     Modified:
C
C     6th  Oct 1992  HME / UoE, Starlink.  LPOLY and FITLPOLY are
C                    JTY_... now.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IY,IX1,IX2,LIMX1,LIMX2,NCOEFF,MAXPT,STATUS
      REAL    DATA(NX,NY),FLAG,X(MAXPT),Z(MAXPT)
C
C     Functions
C
      REAL JTY_LPOLY
C
C     Local variables
C
      INTEGER IPT,NC,NPT
      REAL SCALE(2)
      DOUBLE PRECISION COEFFS(8)
C
C     Fill array of points to be fitted.  First to the right,
C
      NPT=0
      IPT=IX2+1
      DO WHILE ((IPT.LT.(IX2+3*(LIMX2-IX2))).AND.(NPT.LT.(LIMX2-IX2))
     :                                              .AND.(IPT.LE.NX))
         IF (DATA(IPT,IY).NE.FLAG) THEN
            NPT=NPT+1
            X(NPT)=IPT
            Z(NPT)=DATA(IPT,IY)
         END IF
         IPT=IPT+1
      END DO
      IF (NPT.GT.0) THEN
         SCALE(2)=X(NPT)
      ELSE
         SCALE(2)=IX2
      END IF
C
C     and then to the left
C
      IPT=IX1-1
      DO WHILE ((IPT.GT.(IX1-3*(IX1-LIMX1))).AND.
     :         (NPT.LT.(LIMX2-IX2+IX1-LIMX1)).AND.(IPT.GE.1))
         IF (DATA(IPT,IY).NE.FLAG) THEN
            NPT=NPT+1
            X(NPT)=IPT
            Z(NPT)=DATA(IPT,IY)
         END IF
         IPT=IPT-1
      END DO
C
C     Fit data
C
      IF (NPT.LE.1) THEN
         STATUS=1
      ELSE
         SCALE(1)=X(NPT)
         IF (SCALE(1).EQ.SCALE(2)) THEN
            SCALE(1)=X(1)
            SCALE(2)=X(NPT)
         END IF
         NC=MAX(1,MIN(8,NCOEFF))
         CALL JTY_FITLPOLY(NPT,X,Z,SCALE,NC,COEFFS)
C
C        Fix pixels with interpolated data
C
         DO IPT=IX1,IX2
            DATA(IPT,IY)=JTY_LPOLY(FLOAT(IPT),SCALE,NC,COEFFS)
         END DO
         STATUS=0
      END IF
C
      END
