*DECK PDA_DCSEVL
      DOUBLE PRECISION FUNCTION PDA_DCSEVL (X, CS, N, STATUS)
C***BEGIN PROLOGUE  PDA_DCSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (CSEVL-S, PDA_DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X        value at which the series is to be evaluated.
C  CS       array of N terms of a Chebyshev series.  In evaluating
C           CS, only half the first coefficient is summed.
C  N        number of terms in array CS.
C  STATUS   Returned error status.
C           The status must be zero on entry. This
C           routine does not check the status on entry.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  PDA_D1MACH, PDA_XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to PDA_XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   950404  Implement status.  (HME)
C***END PROLOGUE  PDA_DCSEVL
      INTEGER STATUS
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X, PDA_D1MACH
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  PDA_DCSEVL
      IF (FIRST) ONEPL = 1.0D0 + PDA_D1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL PDA_XERMSG ('SLATEC', 'PDA_DCSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2, STATUS)
      IF (N .GT. 1000) CALL PDA_XERMSG ('SLATEC', 'PDA_DCSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2, STATUS)
      IF (ABS(X) .GT. ONEPL) CALL PDA_XERMSG ('SLATEC', 'PDA_DCSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1, STATUS)
C
      B1 = 0.0D0
      B0 = 0.0D0
      TWOX = 2.0D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      PDA_DCSEVL = 0.5D0*(B0-B2)
C
      RETURN
      END
