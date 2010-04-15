C+
      SUBROUTINE ECH_ARCFIT(X,Y,W,NN,
     :                      PTRS,WORK1,WW,XX,YY,
     :                      COEFFS,M)
C
C     E C H _ A R C F I T
C
C     Performs a weighted least squares fit of order M to the NN data
C     points in X and Y, returning a power series polynomial.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) X      (Real array X(NN)) The X values of the points to
C                be fitted.
C     (>) Y      (Real array Y(NN)) The Y values of the points to
C                be fitted.
C     (>) W      (Real array W(NN)) The weights to be used for the
C                points.  Set all weights to 1.0 for normal equal
C                weighting. For historical reasons weights should be
C                given as 1/uncertainty, as was recommended for the NAG
C                routine E02ADF. Now PDA_DPOLFT is used and needs 1/variance
C                for weights, but this is handled internally by this
C                routine.
C     (>) NN     (Integer) The number of points.  This routine is
C                limited by its internal storage to 100 points.
C     (W) PTRS   (Integer array PTRS(NN))  Workspace array.
C     (W) WORK1  (Double precision array WORK1(4*NN+33))  Workspace.
C     (W) WW     (Double precision array WW(NN)) Workspace array.
C     (W) XX     (Double precision array XX(NN)) Workspace array.
C     (W) YY     (Double precision array YY(NN)) Workspace array.
C     (<) COEFFS (Double precision COEFFS(M+1)) The coefficients of the
C                power series polynomial fitted to the points.  The
C                constant term is returned in COEFFS(M+1).
C     (>) M      (Integer) The order of the polynomial to be fitted.
C
C     Common variables used -  None
C
C     History - This routine is a new & generalized version of the Figaro
C        routine WXYFIT, which was in turn a slight modification to the
C        routine XYFIT, having the additional argument giving the weights
C        of the points. XYFIT had a dubious pedigree, originating in a
C        routine used in the IPCS Interdata software and lifted from one
C        of the appendices in the IBM Fortran IV manual!  It was then used
C        in Spica, with modifications made by all sorts of people.  At
C        Caltech it was re-written for Figaro to use the IMSL math library,
C        and now this current version, from AAO, uses a combination of NAG
C        routines and a coefficient decoder wirtten by John Lucey (GEN_
C        CHB2NO).  In all this from the beginning through to WXYFIT, the
C        calling sequence remained unchanged:  the original IBM example
C        needed no extra workspace, so none was passed to versions up to
C        and including WXYFIT.  Hence these routines needed to arbitrarily
C        limit the number of points that could be handled.  The present
C        routine, ECH_ARCFIT, gets around this number of pixel limitation
C        by getting its workspace from the calling program, where the number
C        of points ought to be known and so workspace can be dynamically
C        allocated and passed to the routine.  The use of PAR_WRUSER for
C        output messages also makes WXYFIT and its decendent ECH_ARCFIT
C        somewhat FIGARO-specific.
C
C                    -- most recently WXYFIT:  KS / AAO 5th Sept 1985
C             ... converted into ECH_ARCFIT:  JKM / ESO 21. Nov. 1987
C
C     Modified:
C
C     21 Nov 1987  JKM / ESO.  As explained in the History section
C                  above, workspace arrays were created to do away
C                  with an unnecessary limitation on the number of
C                  points that could be fit.  Comments changed to
C                  reflect this new arrangement; program renamed
C                  from WXYFIT to ECH_ARCFIT and appended to the
C                  Figaro command ECHARC, version 1.0 (auto line
C                  identification for almost all orders).
C     18 Apr 1995  HME / UoE, Starlink.  No longer use NAG, workspace is
C                  longer now and measured by NN _and_ maximum degree of
C                  polynomial.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NN, M
      REAL X(NN),Y(NN),W(NN)
      DOUBLE PRECISION COEFFS(M+1)
C
C     Workspace
C
      INTEGER PTRS(NN)
      DOUBLE PRECISION WORK1(4*NN+33)
      DOUBLE PRECISION WW(NN), XX(NN), YY(NN)
C
C     Local variables
C
      INTEGER I, IFAIL, IFAIL2, NDEG, STATUS
      DOUBLE PRECISION EPS
C
C     Initially, set all coefficients zero.
C
      DO I=1,M+1
         COEFFS(I)=0.
      END DO
C
C     Bail out if too high a degree of fit was requested.
C
      IF (M.GT.10) THEN
         CALL PAR_WRUSER('Too high a degree of fit requested.',STATUS)
      ELSE
C
C        Fill up double precision X, Y and weight arrays.
C
         DO I=1,NN
            XX(I)=X(I)
            YY(I)=Y(I)
            WW(I)=W(I)*W(I)
            IF (WW(I).LE.0D0) WW(I)=W(I)
         END DO
C
C        Perform least squares fit
C
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NN,XX,YY,WW,M,NDEG,EPS,WORK1,
     :      IFAIL,WORK1(NN+1),IFAIL2)
         IF (NDEG.NE.M .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0) THEN
            CALL PAR_WRUSER('Error in PDA_DPOLFT',STATUS)
         ELSE
C
C           Convert coefficients to power series form
C
            IFAIL2=0
            CALL PDA_DPCOEF(M,0D0,COEFFS,WORK1(NN+1),IFAIL2)
            IF (IFAIL2.NE.0) THEN
               CALL PAR_WRUSER('Error in PDA_DPCOEF',STATUS)
            END IF
            CALL GEN_REVR8(COEFFS,M+1,1,.TRUE.,COEFFS)
         END IF
      END IF
C
      RETURN
      END
