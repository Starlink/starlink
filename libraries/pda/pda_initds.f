*DECK PDA_INITDS
      FUNCTION PDA_INITDS (OS, NOS, ETA, STATUS)
C***BEGIN PROLOGUE  PDA_INITDS
C***PURPOSE  Determine the number of terms needed in an orthogonal
C            polynomial series so that it meets a specified accuracy.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (INITS-S, PDA_INITDS-D)
C***KEYWORDS  CHEBYSHEV, FNLIB, INITIALIZE, ORTHOGONAL POLYNOMIAL,
C             ORTHOGONAL SERIES, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Initialize the orthogonal series, represented by the array OS, so
C  that PDA_INITDS is the number of terms needed to insure the error is no
C  larger than ETA.  Ordinarily, ETA will be chosen to be one-tenth
C  machine precision.
C
C             Input Arguments --
C   OS     double precision array of NOS coefficients in an orthogonal
C          series.
C   NOS    number of coefficients in OS.
C   ETA    single precision scalar containing requested accuracy of
C          series.
C   STATUS Returned error status.
C          The status must be zero on entry. This
C          routine does not check the status on entry.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  PDA_XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891115  Modified error message.  (WRB)
C   891115  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to PDA_XERMSG.  (THJ)
C   950404  Implement status.  (HME)
C***END PROLOGUE  PDA_INITDS
      INTEGER STATUS
      DOUBLE PRECISION OS(*)
C***FIRST EXECUTABLE STATEMENT  PDA_INITDS
      IF (NOS .LT. 1) CALL PDA_XERMSG ('SLATEC', 'PDA_INITDS',
     +   'Number of coefficients is less than 1', 2, 1, STATUS)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL PDA_XERMSG ('SLATEC', 'PDA_INITDS',
     +   'Chebyshev series too short for specified accuracy',
     +   1, 1, STATUS)
      PDA_INITDS = I
C
      RETURN
      END
