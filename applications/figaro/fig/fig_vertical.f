C+
      SUBROUTINE FIG_VERTICAL (DATA,NX,NY,IX,IY1,IY2,LIMY1,LIMY2,
     :                               NCOEFF,FLAG,MAXPT,Y,Z,STATUS)
C
C     F I G _ V E R T I C A L
C
C     Routine to interpolate across a feature in a single vertical
C     cut through an image.  Data within a range on either side of
C     the feature are fitted and the pixels delimiting the feature are
C     replaced by the interpolated values.  Note that the operation
C     of this routine is limited to that single vertical cut, so to
C     fix a long horizontal feature this routine has to be called
C     repeatedly.
C
C     Parameters - (">" input, "<" output, "W" workspace, "!" modified)
C
C     (!) DATA     (Real array DATA(NX,NY)) The image data.
C     (>) NX       (Integer) The first (horizontal) dimension of DATA.
C     (>) NY       (Integer) The second (vertical) dimension of DATA.
C     (>) IX       (Integer) The x-coordinate of the vertical cut.
C     (>) IY1      (Integer) The lower y-value of the range of data
C                  to be fixed.
C     (>) IY2      (Integer) The higher y-value of the range of data
C                  to be fixed.  If IY1=IY2 only one pixel is modified.
C     (>) LIMY1    (Integer) The lower bound of the range of data to be
C                  used for the fit.
C     (>) LIMY2    (Integer) The upper bound of the range of data to be
C                  used for the fit.  The fit is therefore normally to the
C                  pixels in the ranges LIMY1 to I1-1 and I2+1 to LIMY2
C                  (inclusive).  The range may be extended a little if too
C                  many of the pixels in this range are flagged as invalid.
C     (>) NCOEFF   (Integer) The number of coefficients for the fit.
C                  Must be 8 or less.
C     (>) FLAG     (Real) Value used to indicate invalid pixels.
C     (>) MAXPT    (Integer) Maximum number of points that can be used
C                  for the fit - ie the dimension of Y and Z.
C     (W) Y        (Real array X(MAXPT)) Workspace.
C     (W) Z        (Real array Y(MAXPT)) Workspace.
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
C                                            KS / CIT 23rd Feb 1984
C     Modified:
C
C     6th  Oct 1992  HME / UoE, Starlink.  LPOLY and FITLPOLY are
C                    JTY_... now.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX,IY1,IY2,LIMY1,LIMY2,NCOEFF,MAXPT,STATUS
      REAL    DATA(NX,NY),FLAG,Y(MAXPT),Z(MAXPT)
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
C     Fill array of points to be fitted.  First above the line,
C
      NPT=0
      IPT=IY2+1
      DO WHILE ((IPT.LT.(IY2+3*(LIMY2-IY2))).AND.(NPT.LT.(LIMY2-IY2))
     :                                              .AND.(IPT.LE.NY))
         IF (DATA(IX,IPT).NE.FLAG) THEN
            NPT=NPT+1
            Y(NPT)=IPT
            Z(NPT)=DATA(IX,IPT)
         END IF
         IPT=IPT+1
      END DO
      IF (NPT.GT.0) THEN
         SCALE(2)=Y(NPT)
      ELSE
         SCALE(2)=IY2
      END IF
C
C     and then below
C
      IPT=IY1-1
      DO WHILE ((IPT.GT.(IY1-3*(IY1-LIMY1))).AND.
     :         (NPT.LT.(LIMY2-IY2+IY1-LIMY1)).AND.(IPT.GE.1))
         IF (DATA(IX,IPT).NE.FLAG) THEN
            NPT=NPT+1
            Y(NPT)=IPT
            Z(NPT)=DATA(IX,IPT)
         END IF
         IPT=IPT-1
      END DO
C
C     Fit data
C
      IF (NPT.LE.1) THEN
         STATUS=1
      ELSE
         SCALE(1)=Y(NPT)
         IF (SCALE(1).EQ.SCALE(2)) THEN
            SCALE(1)=Y(1)
            SCALE(2)=Y(NPT)
         END IF
         NC=MAX(1,MIN(8,NCOEFF))
         CALL JTY_FITLPOLY(NPT,Y,Z,SCALE,NC,COEFFS)

C
C        Fix pixels with interpolated data
C
         DO IPT=IY1,IY2
            DATA(IX,IPT)=JTY_LPOLY(FLOAT(IPT),SCALE,NC,COEFFS)
         END DO
         STATUS=0
      END IF
C
      END
