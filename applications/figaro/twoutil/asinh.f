      FUNCTION ASINH(X)
* ======================================================================
* NIST Guide to Available Math Software.
* Source for module ASINH from package CMLIB.
* Retrieved from CAMSUN on Mon Feb 24 12:17:20 1997.
* ======================================================================

C***BEGIN PROLOGUE  ASINH
C***DATE WRITTEN   770401   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C4C
C***KEYWORDS  ARC HYPERBOLIC SINE,ASINH,ELEMENTARY FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the arc hyperbolic Sine.
C***DESCRIPTION
C
C ASINH(X) computes the arc hyperbolic sine of X.
C
C Series for ASNH       on the interval  0.          to  1.00000D+00
C                                        with weighted error   2.19E-17
C                                         log weighted error  16.66
C                               significant figures required  15.60
C                                    decimal places required  17.31
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL,INITS,R1MACH
C***END PROLOGUE  ASINH
      DIMENSION ASNHCS(20)
      DATA ALN2 /0.6931471805 5994530942E0/
      DATA ASNHCS( 1) /   -.1282003991 1738186E0 /
      DATA ASNHCS( 2) /   -.0588117611 89951768E0 /
      DATA ASNHCS( 3) /    .0047274654 32212481E0 /
      DATA ASNHCS( 4) /   -.0004938363 16265361E0 /
      DATA ASNHCS( 5) /    .0000585062 07058557E0 /
      DATA ASNHCS( 6) /   -.0000074669 98328931E0 /
      DATA ASNHCS( 7) /    .0000010011 69358355E0 /
      DATA ASNHCS( 8) /   -.0000001390 35438587E0 /
      DATA ASNHCS( 9) /    .0000000198 23169483E0 /
      DATA ASNHCS(10) /   -.0000000028 84746841E0 /
      DATA ASNHCS(11) /    .0000000004 26729654E0 /
      DATA ASNHCS(12) /   -.0000000000 63976084E0 /
      DATA ASNHCS(13) /    .0000000000 09699168E0 /
      DATA ASNHCS(14) /   -.0000000000 01484427E0 /
      DATA ASNHCS(15) /    .0000000000 00229037E0 /
      DATA ASNHCS(16) /   -.0000000000 00035588E0 /
      DATA ASNHCS(17) /    .0000000000 00005563E0 /
      DATA ASNHCS(18) /   -.0000000000 00000874E0 /
      DATA ASNHCS(19) /    .0000000000 00000138E0 /
      DATA ASNHCS(20) /   -.0000000000 00000021E0 /
      DATA NTERMS, XMAX, SQEPS /0, 0., 0./
C***FIRST EXECUTABLE STATEMENT  ASINH
      IF (NTERMS.NE.0) GO TO 10
      NTERMS = INITS (ASNHCS, 20, 0.1*R1MACH(3))
      SQEPS = SQRT (R1MACH(3))
      XMAX = 1.0/SQEPS
C
 10   Y = ABS(X)
      IF (Y.GT.1.0) GO TO 20
C
      ASINH = X
      IF (Y.GT.SQEPS) ASINH = X*(1.0 + CSEVL (2.*X*X-1., ASNHCS,NTERMS))
      RETURN
C
 20   IF (Y.LT.XMAX) ASINH = ALOG (Y + SQRT(Y**2+1.))
      IF (Y.GE.XMAX) ASINH = ALN2 + ALOG(Y)
      ASINH = SIGN (ASINH, X)
C
      RETURN
      END

***********************************************************************
*******               FUNCTIONS REQUIRED ABOVE                 ********
***********************************************************************



      FUNCTION CSEVL(X,CS,N)
***BEGIN PROLOGUE  CSEVL
C***DATE WRITTEN   770401   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  CHEBYSHEV,FNLIB,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Evaluate the N-term Chebyshev series CS at X.
C***DESCRIPTION
C
C Evaluate the N-term Chebyshev series CS at X.  Adapted from
C R. Broucke, Algorithm 446, C.A.C.M., 16, 254 (1973). Also see Fox
C and Parker, Chebyshev Polynomials in Numerical Analysis, Oxford Press,
C page 56.
C
C       Input Arguments --
C X    value at which the series is to be evaluated.
C CS   array of N terms of a Chebyshev series.  In eval-
C      uating CS, only half the first coefficient is summed.
C N    number of terms in array CS.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  CSEVL
C
       DIMENSION CS(1)
C***FIRST EXECUTABLE STATEMENT  CSEVL
       IF(N.LT.1) CALL XERROR( 'CSEVL   NUMBER OF TERMS LE 0', 28, 2,2)
       IF(N.GT.1000) CALL XERROR ( 'CSEVL   NUMBER OF TERMS GT 1000',
     1   31,3,2)
       IF (X.LT. -1.0 .OR. X.GT. 1.0) CALL XERROR( 'CSEVL   X OUTSIDE (-
     11,+1)', 25, 1, 1)
C
       B1=0.
       B0=0.
       TWOX=2.*X
       DO 10 I=1,N
       B2=B1
       B1=B0
       NI=N+1-I
       B0=TWOX*B1-B2+CS(NI)
 10    CONTINUE
C
       CSEVL = 0.5 * (B0-B2)
C
       RETURN
      END


      FUNCTION INITS(OS,NOS,ETA)
C***BEGIN PROLOGUE  INITS
C***DATE WRITTEN   770401   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  INITIALIZE,ORTHOGONAL SERIES,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Initializes an orthogonal series so that it defines the
C            number of terms to carry in the series to meet a specified
C            error.
C***DESCRIPTION
C
C Initialize the orthogonal series so that INITS is the number of terms
C needed to insure the error is no larger than ETA.  Ordinarily, ETA
C will be chosen to be one-tenth machine precision.
C
C             Input Arguments --
C OS     array of NOS coefficients in an orthogonal series.
C NOS    number of coefficients in OS.
C ETA    requested accuracy of series.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  INITS
      DIMENSION OS(NOS)
C***FIRST EXECUTABLE STATEMENT  INITS
      IF (NOS.LT.1) CALL XERROR ( 'INITS   NUMBER OF COEFFICIENTS LT 1',
     1 35, 2, 2)
C
      ERR = 0.
      DO 10 II=1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(OS(I))
        IF (ERR.GT.ETA) GO TO 20
 10   CONTINUE
C
 20   IF (I.EQ.NOS) CALL XERROR ( 'INITS   ETA MAY BE TOO SMALL', 28,
     1  1, 2)
      INITS = I
C
      RETURN
      END
