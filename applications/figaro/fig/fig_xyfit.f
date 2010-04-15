C+
      SUBROUTINE FIG_XYFIT(X,Y,NN,COEFFS,M)
C
C     F I G _ X Y F I T
C
C     Performs a least squares fit of order M to the NN data
C     points in X and Y, returning a power series polynomial.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) X      (Real array X(NN)) The X values of the points to
C                be fitted.
C     (>) Y      (Real array Y(NN)) The Y values of the points to
C                be fitted.
C     (>) NN     (Integer) The number of points.  This routine is
C                limited by its internal storage to 100 points.
C     (<) COEFFS (Double precision COEFFS(M+1)) The coefficients of the
C                power series polynomial fitted to the points.  The
C                constant term is returned in COEFFS(M+1).
C     (>) M      (Integer) The order of the polynomial to be fitted.
C
C     Common variables used -  None
C
C     History - This routine has a dubious pedigree, originating in a
C     routine used in the IPCS Interdata software and lifted from one
C     of the appendices in the IBM Fortran IV manual!  It was then used
C     in Spica, with modifications made by all sorts of people.  At
C     Caltech it was re-written for Figaro to use the IMSL math library,
C     and now this current version, from AAO, uses a combination of NAG
C     routines and a coefficient decoder wirtten by John Lucey (GEN_
C     CHB2NO).  In all this, the calling sequence has remained unchanged.
C     The original IBM example needed no extra workspace, so none is
C     passed to the routine.  All further modifications have required
C     temporary workspace; hence the limitation on the number of points
C     that can be handled.  The use of PAR_WRUSER for output messages
C     makes this routine somewhat FIGARO-specific.
C
C                                          KS / AAO 29th March 1985.
C     Modified:
C
C     29th Sep 1986  KS / AAO.  Comments modified.  Description of
C                    COEFFS had constant as first term, not last.
C     22nd Sep 1992  HME / UoE, Starlink.  Changed name from XYFIT to
C                    FIG_XYFIT.
C     18th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NN, M
      REAL X(NN),Y(NN)
      DOUBLE PRECISION COEFFS(M+1)
C
C     Maximum number of points
C
      INTEGER MAXPTS
      PARAMETER (MAXPTS=100)
      INTEGER MAXDEG
      PARAMETER (MAXDEG=10)
C
C     Local variables
C
      INTEGER I, IFAIL, IFAIL2, NDEG, STATUS
      DOUBLE PRECISION EPS, A2(3*MAXPTS+3*(MAXDEG+1))
      DOUBLE PRECISION WW(MAXPTS), XX(MAXPTS), YY(MAXPTS), RR(MAXPTS)
C
C     Initially, set all coefficients zero.
C
      DO I=1,M+1
         COEFFS(I)=0D0
      END DO
C
C     Bail out if too many points, or too high a degree
C
      IF (NN.GT.MAXPTS) THEN
         CALL PAR_WRUSER('Too many points for least-squares fit.',
     :                                                       STATUS)
      ELSE IF (M.GT.MAXDEG) THEN
         CALL PAR_WRUSER('Too high a degree of fit requested.',STATUS)
      ELSE
C
C        Fill up double precision X, Y and weight arrays.
C
         DO I=1,NN
            XX(I)=X(I)
            YY(I)=Y(I)
            WW(I)=1.0D0
         END DO
C
C        Perform least squares fit
C
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NN,XX,YY,WW,M,NDEG,EPS,RR,IFAIL,A2,IFAIL2)
         IF (NDEG.NE.M .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0) THEN
            CALL PAR_WRUSER('Error in PDA_DPOLFT',STATUS)
         ELSE
C
C           Convert coefficients to power-series form in the desired
C           order.
C
            CALL PDA_DPCOEF(-M,0D0,COEFFS,A2,IFAIL2)
            IF (IFAIL2.NE.0) THEN
               CALL PAR_WRUSER('Error in PDA_DPCOEF',STATUS)
            END IF
         END IF
      END IF
C
      END
