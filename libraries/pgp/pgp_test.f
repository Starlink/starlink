      PROGRAM PGTEST
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: draw a histogram.
C-----------------------------------------------------------------------

      INTEGER  I, ISEED, PGBEG
      REAL     DATA(1000), X(620), Y(620)
      REAL     PGRNRM
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      IF (PGBEG(0,'?',1,1).NE.1)  GOTO 999
C
C Call RNGAUS to obtain 1000 samples from a normal distribution.
C
      ISEED = 5778731
      DO 10 I=1,1000
          DATA(I) = PGRNRM(ISEED)
   10 CONTINUE
C
C Draw a histogram of these values.
C
      CALL PGHIST(1000,DATA,-3.1,3.1,31,0)
      CALL PGLAB('Variate', ' ',
     $             'PGPLOT Example 2 - Gaussian distribution')
C
C Superimpose the theoretical distribution.
C
      DO 20 I=1,620
          X(I) = -3.1 + 0.01*(I-1)
          Y(I) = 0.2*1000./SQRT(2.*3.14159265)*EXP(-0.5*X(I)*X(I))
   20 CONTINUE
      CALL PGLINE(620,X,Y)
C
C Don't forget to call PGEND!
C
      CALL PGEND
C
999   CONTINUE
      END

      REAL FUNCTION PGRNRM (ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a normally distributed deviate with zero mean and unit
C variance. The routine uses the Box-Muller transformation of uniform
C deviates. For a more efficient implementation of this algorithm,
C see Press et al., Numerical Recipes, Sec. 7.2.
C
C Arguments:
C  ISEED  (in/out) : seed used for PGRAND random-number generator.
C
C Subroutines required:
C  PGRAND -- return a uniform random deviate between 0 and 1.
C
C History:
C  1995 Dec 12 - TJP.
C-----------------------------------------------------------------------
      REAL R, X, Y, PGRAND
C
 10   X = 2.0*PGRAND(ISEED) - 1.0
      Y = 2.0*PGRAND(ISEED) - 1.0
      R = X**2 + Y**2
      IF (R.GE.1.0) GOTO 10
      PGRNRM = X*SQRT(-2.0*LOG(R)/R)
C-----------------------------------------------------------------------
      END

      REAL FUNCTION PGRAND(ISEED)
      INTEGER ISEED
C-----------------------------------------------------------------------
C Returns a uniform random deviate between 0.0 and 1.0.
C
C NOTE: this is not a good random-number generator; it is only
C intended for exercising the PGPLOT routines.
C
C Based on: Park and Miller's "Minimal Standard" random number
C   generator (Comm. ACM, 31, 1192, 1988)
C
C Arguments:
C  ISEED  (in/out) : seed.
C-----------------------------------------------------------------------
      INTEGER   IM, IA, IQ, IR
      PARAMETER (IM=2147483647)
      PARAMETER (IA=16807, IQ=127773, IR= 2836)
      REAL      AM
      PARAMETER (AM=128.0/IM)
      INTEGER   K
C-
      K = ISEED/IQ
      ISEED = IA*(ISEED-K*IQ) - IR*K
      IF (ISEED.LT.0) ISEED = ISEED+IM
      PGRAND = AM*(ISEED/128)
      RETURN
      END
