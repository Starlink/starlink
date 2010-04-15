      SUBROUTINE FOURI (X,Y,NPTS,NNEG,NMAX,SPX,
     +                    FREQ,RE,IM,PS)
C
C  Evaluates the real and imaginary components of the Fourier transform
C of a real function.
C
C  Program evaluates all areas using a simple trapezium method.
C
      IMPLICIT NONE
      INTEGER I,J,NMAX,NNEG,NPTS,JMAX
      DOUBLE PRECISION X(-NNEG:NMAX), Y(-NNEG:NMAX)
      DOUBLE PRECISION EVEN(-20000:20000),ODD(-20000:20000)
      DOUBLE PRECISION FREQ(0:*), RE(0:*), IM(0:*), PS(0:*)
      DOUBLE PRECISION SPX, NCF, PI, TPI, PERIOD, FRES
      PARAMETER (PI=3.1415926536D+00, TPI=2.0D+00*PI)
C
C  Obtain odd and even parts of the function.
C  If inverse of X value lies outside range of data, then obtain meaningful
C Y value from an adjacent cycle. This is valid because the Fourier Theorem
C assumes the data repeats cyclicly ad infinitum: our datastream is just
C one period's sample of the underlying cyclic function.
C
      PERIOD = X(NMAX) - X(-NNEG)
      DO 110, I=-NNEG,NMAX
        IF (-I.LT.-NNEG) THEN
          EVEN(I) = (Y(I)+Y(NNEG+NMAX-I))*0.5D+00
          ODD(I)  = (Y(I)-Y(NNEG+NMAX-I))*0.5D+00
        ELSEIF (-I.GT.NMAX) THEN
          EVEN(I) = (Y(I)+Y(-(NNEG+NMAX)-I))*0.5D+00
          ODD(I)  = (Y(I)-Y(-(NNEG+NMAX)-I))*0.5D+00
        ELSE
          EVEN(I) = (Y(I)+Y(-I))*0.5D+00
          ODD(I)  = (Y(I)-Y(-I))*0.5D+00
        ENDIF
  110 CONTINUE
C
C  Evaluate real and imaginary parts of transform at frequencies separated
C by lowest required frequency increments out to Nyquist Frequency.
C
      FRES = 1.0D+00/PERIOD
      NCF = 1.0D+00/SPX * 0.50D+00
      JMAX = INT(NCF/FRES)
      DO 200, J=0,JMAX
        FREQ(J) = FRES * DBLE(J)
        RE(J) = 0.0D+00
        IM(J) = 0.0D+00
        DO 300, I=-NNEG,NMAX
          RE(J) = RE(J) + EVEN(I) * DCOS(TPI*X(I)*FREQ(J))
          IM(J) = IM(J) + ODD(I) * DSIN(TPI*X(I)*FREQ(J))
  300   CONTINUE
        RE(J) = RE(J)/DBLE(NPTS)
        IM(J) = IM(J)/DBLE(NPTS)
        PS(J) = DSQRT( RE(J)**2 + IM(J)**2 )
  200 CONTINUE
      NPTS = JMAX + 1
      RETURN
      END
