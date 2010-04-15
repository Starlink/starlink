C+
      SUBROUTINE ECH_ARFILL(OUTPUT,NX,NORDERS,F1,F2,FITS,
     :                          NFITS,NC,MLDATA,WTS,ORDER)
C
C     E C H _ A R F I L L
C
C     This subroutine fills the OUTPUT array with wavelength values by
C     interpolating/extrapolating between/beyond the fitted orders.
C
C     Parameters -  (">" input, "<" output, "M" modified, "W" workspace)
C
C     (<) OUTPUT  (Double Prec Array OUTPUT(NX,NORDERS))  The output
C                 wavelength image originally empty, to be filled with
C                 interpolated+extrapolated wavelengths by ECH_ARFILL.
C     (>) NX      (Integer)  The number of columns in the OUTPUT image.
C     (>) NORDERS (Integer)  The number of rows in the OUTPUT image.
C     (>) OFITS   (Real Array OFITS(NOFITS))  The order numbers which
C                 where are already filled with wavelengths on input.
C     (>) NOFITS  (Integer)  The number of orders filled on input.
C     (W) MLDATA  (Real Array MLDATA(NOFITS))  A Workspace array for
C                 the order number * lambda product, which will be fit
C                 in order to calculate other lambda's and fill OUTPUT.
C     (W) WTS     (Real Array WTS(NOFITS)) The weights to be applied to
C                 the MLDATA points; this array is set to 1.0
C                 everywhere.
C     (>) F1      (Integer)  The first order number in OUTPUT.
C     (>) F2      (Integer)  The last order number in OUTPUT.
C
C     Subroutines / Functions used:
C
C        FIG_WXYFIT - (FIGARO) -- fits a polynomial to a set of points
C                 with adjustable weighting factors.
C
C        GEN_EPOLYD - (GEN_pckg) -- returns the double precision evalua-
C                 tion of a polynomial at a given x point.
C
C                                             -- JKM / ESO 18 Nov. 1987
C+
      IMPLICIT NONE
C
C     Functions
C
      DOUBLE PRECISION GEN_EPOLYD
C
C     Parameters
C
      INTEGER NX,NORDERS,F1,F2
      INTEGER FITS(NORDERS),NC
      DOUBLE PRECISION OUTPUT(NX,NORDERS)
C
C     Workspace
C
      INTEGER NFITS
      REAL MLDATA(NFITS),WTS(NFITS),ORDER(NFITS)
C
C     Local Variables
C
      INTEGER NMAX
      PARAMETER (NMAX=11)
      INTEGER DY,I,J,K,N,NF,NEWNC,JORD
C     REAL XPLOT(100),YPLOT(100),TOP,BOT
      DOUBLE PRECISION COEFFS(NMAX)
C
C     Determine direction of fit
C
      IF (F1.LT.F2) THEN
         DY=1
      ELSE
         DY=-1
      END IF
C
C     Initialize WTS array
C
      DO K=1,NFITS,1
         WTS(K)=1.00
      END DO
C
C     Fill OUTPUT(I,J) for each column
C
      DO I=1,NX,1
C
C        Compute M*LAMBDA data to be fit vs. fitted order numbers
C
         NF=0
         DO J=1,NORDERS,1
            IF (FITS(J).GT.0) THEN
               NF=NF+1
               ORDER(NF)=FLOAT(F1+DY*(J-1))
               MLDATA(NF)=ORDER(NF)*OUTPUT(I,J)
            END IF
         END DO
C        IF (NF.NE.NFITS) CALL PAR_WRUSER(
C    :      ' *** ECH_ARFILL unable to find all fitted orders',
C    :                                                   NSTAT)
C
C        Peform fit to this column
C
         DO N=1,NMAX,1
            COEFFS(N)=0.0D00
         END DO
         IF (NFITS.LT.NC) THEN
C
C           Not enough orders for NC-th coefficient fit
C
            NEWNC=NFITS
         ELSE
            NEWNC=NC
         END IF
C
C        Check NEWNC (=NC at most) against upper limit NMAX
C
         IF (NEWNC.GT.NMAX) THEN
C
C           Not enough coefficients for a NC fit as requested
C
            NEWNC=NMAX
C
         END IF
C
         CALL FIG_WXYFIT(ORDER,MLDATA,WTS,NFITS,COEFFS,NEWNC-1)
C
C***> MONITOR
C
C         IF (I.EQ.400) THEN
C            CALL PGBEGIN(7,'PGPLOT',1,1)
C            CALL GKD_INIT('PGPLOT')
C            TOP=MLDATA(NFITS)+0.2*(MLDATA(NFITS)-MLDATA(1))
C            BOT=MLDATA(1)    -0.2*(MLDATA(NFITS)-MLDATA(1))
C            CALL PGENV(FLOAT(F1),FLOAT(F2),BOT,TOP,0,0)
C            CALL PGPOINT(NFITS,ORDER,MLDATA,3)
C            DO J=1,NORDERS,1
C               XPLOT(J)=FLOAT(F1+(J-1)*DY)
C               YPLOT(J)=GEN_EPOLYD(DBLE(XPLOT(J)),COEFFS,NEWNC)
C            END DO
C            CALL PGLINE(NORDERS,XPLOT,YPLOT)
C            CALL GKD_CLOSE
C            CALL PGEND
C         END IF
C***^
C        Now fill this column of OUTPUT with the fit results
C
         DO J=1,NORDERS,1
            IF (FITS(J).EQ.0) THEN
               JORD=F1+(J-1)*DY
               OUTPUT(I,J)=GEN_EPOLYD(DBLE(JORD),COEFFS,NEWNC)
               OUTPUT(I,J)=OUTPUT(I,J)/DBLE(JORD)
C
            END IF
         END DO
C
C        Proceed on to next column of data
C
      END DO
C
      RETURN
      END
