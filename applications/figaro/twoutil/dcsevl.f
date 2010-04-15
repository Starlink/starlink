      DOUBLE PRECISION FUNCTION DCSEVL(X,A,N)
C***BEGIN PROLOGUE  DCSEVL
C***DATE WRITTEN   770401   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  CHEBYSHEV,FNLIB,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Evaluate the double precision N-term Chebyshev series A
C            at X.
C***DESCRIPTION
C
C Evaluate the N-term Chebyshev series A at X.  Adapted from
C R. Broucke, Algorithm 446, C.A.C.M., 16, 254 (1973).
C W. Fullerton, C-3, Los Alamos Scientific Laboratory.
C
C       Input Arguments --
C X    double precision value at which the series is to be evaluated.
C A    double precision array of N terms of a Chebyshev series.  In
C      evaluating A, only half of the first coefficient is summed.
C N    number of terms in array A.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ERR_REP
C***END PROLOGUE  DCSEVL
       DOUBLE PRECISION A(N),X,TWOX,B0,B1,B2
       include 'SAE_PAR'

C***FIRST EXECUTABLE STATEMENT  DCSEVL
       IF(N.LT.1)CALL ERR_REP(' ','DCSEVL  NUMBER OF TERMS LE 0',
     1 SAI__ERROR)
       IF(N.GT.1000)CALL ERR_REP(' ','DCSEVL  NUMBER OF TERMS GT 1000',
     1 SAI__ERROR)
       IF ((X.LT.-1.D0) .OR. (X.GT.1.D0)) CALL ERR_REP(' ', 'DCSEVL
     1 X OUTSIDE (-1,+1)', SAI__ERROR)
C
       TWOX = 2.0D0*X
       B1 = 0.D0
       B0=0.D0
       DO 10 I=1,N
         B2=B1
         B1=B0
         NI = N - I + 1
         B0 = TWOX*B1 - B2 + A(NI)
 10    CONTINUE
C
       DCSEVL = 0.5D0 * (B0-B2)
C
       RETURN
      END
