      FUNCTION INITDS(DOS,NOS,ETA)
C***BEGIN PROLOGUE  INITDS
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  CHEBYSHEV,DOUBLE PRECISION,INITIALIZE,
C             ORTHOGONAL POLYNOMIAL,SERIES,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Initializes the d.p. properly normalized orthogonal
C            polynomial series to determine the number of terms needed
C            for specific accuracy.
C***DESCRIPTION
C
C Initialize the double precision orthogonal series DOS so that INITDS
C is the number of terms needed to insure the error is no larger than
C ETA.  Ordinarily ETA will be chosen to be one-tenth machine precision
C
C             Input Arguments --
C DOS    dble prec array of NOS coefficients in an orthogonal series.
C NOS    number of coefficients in DOS.
C ETA    requested accuracy of series.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ERR_REP
C***END PROLOGUE  INITDS
C

      DOUBLE PRECISION DOS(NOS)
      include 'SAE_PAR'
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS.LT.1) CALL ERR_REP(' ','INITDS  NUMBER OF COEFFs LT 1',
     1 SAI__ERROR)
C
      ERR = 0.
      DO 10 II=1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(SNGL(DOS(I)))
        IF (ERR.GT.ETA) GO TO 20
 10   CONTINUE
C
 20   IF (I.EQ.NOS) CALL ERR_REP(' ','INITDS  ETA MAY BE TOO SMALL',
     1  SAI__ERROR)
      INITDS = I
C
      RETURN
      END




