*DECK PDA_DPCOEF
      SUBROUTINE PDA_DPCOEF (L, C, TC, A, STATUS)
C***BEGIN PROLOGUE  PDA_DPCOEF
C***PURPOSE  Convert the PDA_DPOLFT coefficients to Taylor series form.
C***LIBRARY   SLATEC
C***CATEGORY  K1A1A2
C***TYPE      DOUBLE PRECISION (PCOEF-S, PDA_DPCOEF-D)
C***KEYWORDS  CURVE FITTING, DATA FITTING, LEAST SQUARES, POLYNOMIAL FIT
C***AUTHOR  Shampine, L. F., (SNLA)
C           Davenport, S. M., (SNLA)
C***DESCRIPTION
C
C     Abstract
C
C     PDA_DPOLFT  computes the least squares polynomial fit of degree  L  as
C     a sum of orthogonal polynomials.  PDA_DPCOEF  changes this fit to its
C     Taylor expansion about any point  C , i.e. writes the polynomial
C     as a sum of powers of (X-C).  Taking  C=0.  gives the polynomial
C     in powers of X, but a suitable non-zero  C  often leads to
C     polynomials which are better scaled and more accurately evaluated.
C
C     The parameters for  PDA_DPCOEF  are
C
C     INPUT -- All TYPE REAL variables are DOUBLE PRECISION
C         L -      Indicates the degree of polynomial to be changed to
C                  its Taylor expansion.  To obtain the Taylor
C                  coefficients in reverse order, input  L  as the
C                  negative of the degree desired.  The absolute value
C                  of L  must be less than or equal to NDEG, the highest
C                  degree polynomial fitted by  PDA_DPOLFT .
C         C -      The point about which the Taylor expansion is to be
C                  made.
C         A -      Work and output array containing values from last
C                  call to  PDA_DPOLFT .
C
C     OUTPUT -- All TYPE REAL variables are DOUBLE PRECISION
C         TC -     Vector containing the first LL+1 Taylor coefficients
C                  where LL=ABS(L).  If  L.GT.0 , the coefficients are
C                  in the usual Taylor series order, i.e.
C                    P(X) = TC(1) + TC(2)*(X-C) + ... + TC(N+1)*(X-C)**N
C                  If L .LT. 0, the coefficients are in reverse order,
C                  i.e.
C                    P(X) = TC(1)*(X-C)**N + ... + TC(N)*(X-C) + TC(N+1)
C         STATUS - Returned error status.
C                  The status must be zero on entry. This
C                  routine does not check the status on entry.
C
C***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
C                 Curve fitting by polynomials in one variable, Report
C                 SLA-74-0270, Sandia Laboratories, June 1974.
C***ROUTINES CALLED  PDA_DP1VLU
C***REVISION HISTORY  (YYMMDD)
C   740601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891006  Cosmetic changes to prologue.  (WRB)
C   891006  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C   950404  Implement status.  (HME)
C   950517  Return immediately if PDA_DP1VLU returns a status.  (HME)
C***END PROLOGUE  PDA_DPCOEF
C
      INTEGER STATUS
      INTEGER I,L,LL,LLP1,LLP2,NEW,NR
      DOUBLE PRECISION A(*),C,FAC,SAVE,TC(*)
C***FIRST EXECUTABLE STATEMENT  PDA_DPCOEF
      LL = ABS(L)
      LLP1 = LL + 1
      CALL PDA_DP1VLU (LL,LL,C,TC(1),TC(2),A,STATUS)
      IF (STATUS .NE. 0) RETURN
      IF (LL .LT. 2) GO TO 2
      FAC = 1.0D0
      DO 1 I = 3,LLP1
        FAC = FAC*(I-1)
 1      TC(I) = TC(I)/FAC
 2    IF (L .GE. 0) GO TO 4
      NR = LLP1/2
      LLP2 = LL + 2
      DO 3 I = 1,NR
        SAVE = TC(I)
        NEW = LLP2 - I
        TC(I) = TC(NEW)
 3      TC(NEW) = SAVE
 4    RETURN
      END
