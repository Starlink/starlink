*+
* Description:
*   Calling functions for Asterix PDA replacements.
*   Designed to be as close to NAG calls as possible
*   and wrapper to insulate the user from the actual
*   algorithm called.

* Authors:
*   RB: Richard Beard (ROSAT, University of Birmingham)

* History:
*   19 Jun 1997: RB
*     Original version
*-

*+
* Description:
*   Exponential integral E1(x)
*   Replacement for S13AAF
*-
      DOUBLE PRECISION FUNCTION PDA_EXPINT(X, IFAIL)

      DOUBLE PRECISION		EONE
      EXTERNAL			EONE

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .LE. 0.0D0) THEN
        IFAIL = 1
        PDA_EXPINT = 0.0D0
      ELSE
        IFAIL = 0
        PDA_EXPINT = EONE(X)
      END IF

      END

*+
* Description:
*  Gamma function G(x)
*   Replacement for S14AAF
*-
      DOUBLE PRECISION FUNCTION PDA_GAMMA(X, IFAIL)

      DOUBLE PRECISION		DGAMMA
      EXTERNAL			DGAMMA

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .GT. 171.624D0) THEN
        IFAIL = 1
        PDA_GAMMA = 0.0D0
      ELSE IF (X .EQ. DINT(X) .AND. X .LT. 0.0D0) THEN
        IFAIL = 4
        PDA_GAMMA = 0.0D0
      ELSE
        IFAIL = 0
        PDA_GAMMA = DGAMMA(X)
      END IF

      END

*+
* Description:
*   Logarithm of Gamma function ln G(x)
*   Replacement for S14ABF
*-
      DOUBLE PRECISION FUNCTION PDA_LNGAM(X, IFAIL)

      DOUBLE PRECISION		DLGAMA
      EXTERNAL			DLGAMA

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .LE. 0.0D0) THEN
        IFAIL = 1
        PDA_LNGAM = 0.0D0
      ELSE IF (X .GT. 2.55D+305) THEN
        IFAIL = 2
        PDA_LNGAM = 0.0D0
      ELSE
        IFAIL = 0
        PDA_LNGAM = DLGAMA(X)
      END IF

      END

*+
* Description:
*   Complementary error function erfc(x)
*   Replacement for S15ADF
*-
      DOUBLE PRECISION FUNCTION PDA_ERFC(X, IFAIL)

      DOUBLE PRECISION		DERFC
      EXTERNAL			DERFC

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      PDA_ERFC = DERFC(X)

      END

*+
* Description:
*   Error function erf(x)
*   Replacement for S15AEF
*-
      DOUBLE PRECISION FUNCTION PDA_ERF(X, IFAIL)

      DOUBLE PRECISION		DERF
      EXTERNAL			DERF

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      PDA_ERF = DERF(X)

      END

*+
* Description:
*   Dawson's integral F(x)
*   Replacement for S15AFF
*-
      DOUBLE PRECISION FUNCTION PDA_DAWINT(X, IFAIL)

      DOUBLE PRECISION		DAW
      EXTERNAL			DAW

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      PDA_DAWINT = DAW(X)

      END

*+
* Description:
*   Bessel function Y0(x)
*   Replacement for S17ACF
*-
      DOUBLE PRECISION FUNCTION PDA_BESY0(X, IFAIL)

      DOUBLE PRECISION		BESY0
      EXTERNAL			BESY0

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .GT. 1.07D+09) THEN
        IFAIL = 1
        PDA_BESY0 = 0.0D0
      ELSE IF (X .LE. 0.0D0) THEN
        IFAIL = 2
        PDA_BESY0 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_BESY0 = BESY0(X)
      END IF

      END

*+
* Description:
*   Bessel function Y1(x)
*   Replacement for S17ADF
*-
      DOUBLE PRECISION FUNCTION PDA_BESY1(X, IFAIL)

      DOUBLE PRECISION		BESY1
      EXTERNAL			BESY1

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .GT. 1.07D+09) THEN
        IFAIL = 1
        PDA_BESY1 = 0.0D0
      ELSE IF (X .LE. 0.0D0) THEN
        IFAIL = 2
        PDA_BESY1 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_BESY1 = BESY1(X)
      END IF

      END

*+
* Description:
*   Bessel function J0(x)
*   Replacement for S17AEF
*-
      DOUBLE PRECISION FUNCTION PDA_BESJ0(X, IFAIL)

      DOUBLE PRECISION		BESJ0
      EXTERNAL			BESJ0

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (ABS(X) .GT. 1.07D+09) THEN
        IFAIL = 1
        PDA_BESJ0 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_BESJ0 = BESJ0(X)
      END IF

      END

*+
* Description:
*   Bessel function J1(x)
*   Replacement for S17AFF
*-
      DOUBLE PRECISION FUNCTION PDA_BESJ1(X, IFAIL)

      DOUBLE PRECISION		BESJ1
      EXTERNAL			BESJ1

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (ABS(X) .GT. 1.07D+09) THEN
        IFAIL = 1
        PDA_BESJ1 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_BESJ1 = BESJ1(X)
      END IF

      END

*+
* Description:
*   Modified Bessel function K0(x)
*   Replacement for S18ACF
*-
      DOUBLE PRECISION FUNCTION PDA_MBESK0(X, IFAIL)

      DOUBLE PRECISION		BESK0
      EXTERNAL			BESK0

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .LE. 0.0D0) THEN
        IFAIL = 1
        PDA_MBESK0 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_MBESK0 = BESK0(X)
      END IF

      END

*+
* Description:
*   Modified Bessel function K1(x)
*   Replacement for S18ADF
*-
      DOUBLE PRECISION FUNCTION PDA_MBESK1(X, IFAIL)

      DOUBLE PRECISION		BESK1
      EXTERNAL			BESK1

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .LE. 0.0D0) THEN
        IFAIL = 1
        PDA_MBESK1 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_MBESK1 = BESK1(X)
      END IF

      END

*+
* Description:
*   Modified Bessel function I0(x)
*   Replacement for S18AEF
*-
      DOUBLE PRECISION FUNCTION PDA_MBESI0(X, IFAIL)

      DOUBLE PRECISION		BESI0
      EXTERNAL			BESI0

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .GT. 713.986D0) THEN
        IFAIL = 1
        PDA_MBESI0 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_MBESI0 = BESI0(X)
      END IF

      END

*+
* Description:
*   Modified Bessel function I1(x)
*   Replacement for S18AFF
*-
      DOUBLE PRECISION FUNCTION PDA_MBESI1(X, IFAIL)

      DOUBLE PRECISION		BESI1
      EXTERNAL			BESI1

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IF (X .GT. 713.987D0) THEN
        IFAIL = 1
        PDA_MBESI1 = 0.0D0
      ELSE
        IFAIL = 0
        PDA_MBESI1 = BESI1(X)
      END IF

      END

*+
* Description:
*   Cumulative Normal distribution function P(x)
*   Replacement for S15ABF
*-
      DOUBLE PRECISION FUNCTION PDA_CNDFPX(X, IFAIL)

      DOUBLE PRECISION		PDA_ERFC

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      PDA_CNDFPX = 0.5D0 * PDA_ERFC(-X / SQRT(2.0D0), IFAIL)

      END

*+
* Description:
*   Complement of cumulative Normal distribution function Q(x)
*   Replacement for S15ACF
*-
      DOUBLE PRECISION FUNCTION PDA_CCNDFQ(X, IFAIL)

      DOUBLE PRECISION		PDA_ERFC

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      PDA_CCNDFQ = 0.5D0 * PDA_ERFC(X / SQRT(2.0D0), IFAIL)

      END

*+
* Description:
*   Value of Psi(x) - ln(x)
*   Replacement for S14ACF
*-
      DOUBLE PRECISION FUNCTION PDA_PSIXML(X, IFAIL)

      DOUBLE PRECISION		PSI
      EXTERNAL			PSI

      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      IF (X .LE. 0.0D0) THEN
        IFAIL = 1
        PDA_PSIXML = 0.0D0
      ELSE
        PDA_PSIXML = PSI(X) - LOG(x)
      END IF

      END

*+
* Description:
*   Probabilities from standrad Normal distribution function
*   Replacement for G01EAF
*-
      DOUBLE PRECISION FUNCTION PDA_NORMAL(TAIL, X, IFAIL)

      DOUBLE PRECISION		PDA_ERFC, PDA_ERF

      CHARACTER*(*)		TAIL
      DOUBLE PRECISION		X
      INTEGER			IFAIL

      IFAIL = 0
      IF (TAIL(1:1) .EQ. 'L' .OR. TAIL(1:1) .EQ. 'l') THEN
        PDA_NORMAL = 0.5D0 * PDA_ERFC(-X / SQRT(2.0D0), IFAIL)
      ELSE IF (TAIL(1:1) .EQ. 'U' .OR. TAIL(1:1) .EQ. 'u') THEN
        PDA_NORMAL = 0.5D0 * PDA_ERFC(X / SQRT(2.0D0), IFAIL)
      ELSE IF (TAIL(1:1) .EQ. 'S' .OR. TAIL(1:1) .EQ. 's') THEN
        PDA_NORMAL = PDA_ERFC(ABS(X) / SQRT(2.0D0), IFAIL)
      ELSE IF (TAIL(1:1) .EQ. 'C' .OR. TAIL(1:1) .EQ. 'c') THEN
        PDA_NORMAL = PDA_ERF(ABS(X) / SQRT(2.0D0), IFAIL)
      ELSE
        IFAIL = 1
        PDA_NORMAL = 0.0D0
      END IF

      END

*+
* Description:
*   Probabilities from standrad Normal distribution function
*   Replacement for G01FAF
*-
      DOUBLE PRECISION FUNCTION PDA_NORDEV(TAIL, P, IFAIL)

      DOUBLE PRECISION		PDA_NORMAL

      CHARACTER*(*)		TAIL
      DOUBLE PRECISION		P
      INTEGER			IFAIL

      DOUBLE PRECISION		MIN_X, MAX_X
      DOUBLE PRECISION		X_EPSILON, X1, P1
      PARAMETER			(X_EPSILON = 1.0D-10)

      IFAIL = 0
      MIN_X = -99.9D0
      MAX_X = 99.9D0
      X1 = 0.0D0

      IF (TAIL(1:1) .NE. 'L' .AND. TAIL(1:1) .NE. 'l' .AND.
     :    TAIL(1:1) .NE. 'U' .AND. TAIL(1:1) .NE. 'u' .AND.
     :    TAIL(1:1) .NE. 'S' .AND. TAIL(1:1) .NE. 's' .AND.
     :    TAIL(1:1) .NE. 'C' .AND. TAIL(1:1) .NE. 'c' ) THEN
        IFAIL = 1
        PDA_NORDEV = 0.0D0
      ELSE IF (P .LE. 0.0D0 .OR. P .GE. 1.0D0) THEN
        IFAIL = 2
        PDA_NORDEV = 0.0D0
      ELSE
        DO WHILE (MAX_X - MIN_X .GT. X_EPSILON .AND. IFAIL .EQ. 0)
          P1 = PDA_NORMAL(TAIL, X1, IFAIL)
          IF (P1 .GT. P) THEN
            IF (TAIL(1:1) .EQ. 'U' .OR. TAIL(1:1) .EQ. 'u') THEN
              MIN_X = X1
            ELSE
              MAX_X = X1
            END IF
          ELSE
            IF (TAIL(1:1) .EQ. 'U' .OR. TAIL(1:1) .EQ. 'u') THEN
              MAX_X = X1
            ELSE
              MIN_X = X1
            END IF
          END IF
          X1 = (MIN_X + MAX_X) / 2.0D0
        END DO
        IF (TAIL(1:1) .EQ. 'S' .OR. TAIL(1:1) .EQ. 's') THEN
          PDA_NORDEV = ABS(X1)
        ELSE
	  PDA_NORDEV = X1
        END IF
      END IF

      END

*+
* Description:
*   Set up reference vector for a Poisson distribution
*   Replacement for G05ECF
*-
      SUBROUTINE PDA_POIREF(T, R, NR, IFAIL)

      DOUBLE PRECISION		T, R(*)
      INTEGER			NR, IFAIL

      IFAIL = 1

      END

*+
* Description:
*   Find zero of continuous function
*   Replacement for C05ADF
*-
      SUBROUTINE PDA_FUNC0(A, B, EPS, ETA, F, X, IFAIL)

      DOUBLE PRECISION		A, B, EPS, ETA, F, X
      INTEGER			IFAIL
      EXTERNAL			F

      DOUBLE PRECISION		ZEROIN
      EXTERNAL			ZEROIN

      IFAIL = 0

      IF (EPS .LE. 0.0D0 .OR. A .EQ. B .OR. F(A)*F(B) .GT. 0.0D0) THEN
        IFAIL = 1
      ELSE IF (EPS .LE. 0.0D0 .OR. 1.0D0+EPS .EQ. 1.0D0) THEN
        IFAIL = 2
      ELSE
        X = ZEROIN(A, B, F, ETA, EPS)
      END IF

      END

*+
* Description:
*   Lower, upper and point probabilities of a Poisson distribution
*   Replacement for G01BKF
*-
      SUBROUTINE PDA_POIPRB(L, K, PLEK, PGTK, PEQK, IFAIL)

      DOUBLE PRECISION		L, PLEK, PGTK, PEQK
      INTEGER			K, IFAIL

      EXTERNAL			PDA_GAMMA
      DOUBLE PRECISION		PDA_GAMMA

      INTEGER			I, J, KMAX

      IFAIL = 0
      PLEK = 0.0D0
      PGTK = 0.0D0
      PEQK = 0.0D0

*   The maximum value for K, above which over/underflow will start to occur.
*   The limit of 171 is for PDA_GAMMA returing zero.
      KMAX = INT(63.57 + 477.5 * REAL(L)**-0.3597)

      IF (K .GE. KMAX .OR. K .GE. 171) THEN
        PLEK = 1.0D0
      ELSE IF (L .LE. 0.0D0) THEN
        IFAIL = 1
      ELSE IF (K .LT. 0) THEN
        IFAIL = 2
      ELSE IF (L .GT. 1.0D6) THEN
        IFAIL = 3
      ELSE
        J = K
        PEQK = EXP(-L) * (L ** J) / PDA_GAMMA(DBLE(J+1), IFAIL)
        DO I = 0, K
          J = I
          PLEK = PLEK +
     :           (EXP(-L) * (L ** J) / PDA_GAMMA(DBLE(J+1), IFAIL))
        END DO
        PGTK = 1.0D0 - PLEK
      END IF

      END
