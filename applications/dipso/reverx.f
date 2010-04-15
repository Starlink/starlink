      SUBROUTINE REVERX (FREQ, RE, IM, NPTS, SHIFT, X, Y)
C
C  Evaluates the even and odd components of a real function from the
C the real and imaginary components of its Fourier transform.
C
      IMPLICIT NONE
C  imports:-
      DOUBLE PRECISION FREQ(0:*)             ! frequencies of transform
      DOUBLE PRECISION RE(0:*), IM(0:*)      ! real and imag. components of tr.
      DOUBLE PRECISION SHIFT    ! shift required to restore data to right cycle
C  import/exports:-
      INTEGER NPTS           ! no. points in transform -> no. points in data
C  exports:-
      DOUBLE PRECISION X(*), Y(*)                  ! recovered data
C  local:-
      DOUBLE PRECISION FRES, NCF          ! frequncy spacing, highest frequency
      DOUBLE PRECISION EVEN, ODD   ! components of recovered data
      DOUBLE PRECISION SPX, PERIOD       ! data spacing, period covered by data
      DOUBLE PRECISION PI, TPI                 ! pi, 2pi
      PARAMETER (PI=3.1415926536D+00, TPI=2.0D+00*PI)
      INTEGER I, J                           ! DO-loop increment variables
      INTEGER JMAX                           ! temporary DO-loop delimiter
C
      NCF = FREQ(NPTS-1)
      FRES = (NCF-FREQ(0))/(DBLE(NPTS-1))
      SPX = 0.5D+00/NCF
      PERIOD = 1.0D+00/FRES
C
C  Evaluate even and odd parts of recovered function at X values ranging over
C one period starting from first X value in interpolated data.
C
      JMAX = INT(PERIOD/SPX) + 1
      X(1) = SHIFT
      DO 200, J = 1, JMAX
        X(J) = X(1) + SPX * DBLE(J-1)
        EVEN = 0.0D+00
        ODD = 0.0D+00
        DO 100, I = 0, (NPTS-1)
          EVEN = EVEN + RE(I) * DCOS(TPI*X(J)*FREQ(I))
          ODD =  ODD  + IM(I) * DSIN(TPI*X(J)*FREQ(I))
  100   CONTINUE
        Y(J) = 2.0D+00*(EVEN + ODD) - RE(0)
  200 CONTINUE
      NPTS = JMAX
      RETURN
      END
