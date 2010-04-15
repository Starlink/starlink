      REAL FUNCTION PDA_RNGAM( A )
*+
*  Name:
*     PDA_RNGAM

*  Purpose:
*     Returns pseudo-random numbers from a gamma distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PDA_RNGAM( A )

*  Description:
*     This is a simple random-number generator providing deviates in the
*     from a gamma distribution, with a period of 2**26, and to 6 or
*     7 digits accuracy.  It is based upon Ahrens, Dieter & Grube's
*     TOMS599 routines.  A value of zero is returned if the argument of
*     the gamma function is not positive.

*  Arguments:
*     A = REAL (Given)
*        The argument (mean) of the gamma function.

*  Returned Value:
*     PDA_RNGAM = REAL
*        The pseudo-random deviate. A value of zero is returned if the
*        argument of the gamma function is not positive.

*  Prior Requirements:
*     The initial seed MUST be set using routine PDA_RNSED (equivalent
*     to NAG's G05CBF).  If it has not, there is no guarantee that
*     sensible values will be returned from this function.

*  Algorithm:
*     A multiplicative congruential generator of the form
*
*        R := MOD( R * FACTOR, 1 )
*
*     The factor is the integer of the form 8 * K + 5 (K positive
*     integer) nearest to to  2**26 * ( sqrt(5) - 1 )/2 (the so called
*     `golden section').  The initial seed is defined by PDA_RNSED.
*     The gamma-distribution variates are found using the
*     algorithms given in the references below.

*  References:
*     Ahrens, J.H., & Dieter, U. 1982, "Generating gamma variates
*       by a modified rejection technique", Comm. ACM 25(1), pp.47--54.
*       (For A >= 1.0, algorithm GD)
*     Ahrens, J.H., & Dieter, U. 1974, "Computer Methods for sampling
*       gamma, Poisson and binomial distributions", Computing. 12),
*       pp.223--246.  (For 0.0 < A < 1.0, adapted algorithm GS)

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 22 (MJC)
*        Original version.
*     1997 February 26 (DSB)
*        Starlink global status argument removed.
*     1997 February 27 (MJC)
*        Added SAVE statement.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL A

*  External References:
      EXTERNAL PDA_RAND
      REAL PDA_RAND              ! Pseudo-random number in range [0,1]

      EXTERNAL PDA_RNEXP
      REAL PDA_RNEXP             ! Pseudo-random number negative
                                 ! exponential

      EXTERNAL PDA_RNNOR
      REAL PDA_RNNOR             ! Pseudo-random number Gaussian (0,1)

*  Local Constants:
      INTEGER NPROB              ! Number of probabilities
      PARAMETER ( NPROB = 35 )

*  Local Variables:
      REAL A1, A2, A3            ! { Coefficients A(K) for Q = Q0 +
      REAL A4, A5, A6, A7        ! { (T*T/2)*SUM(A(K)*V**K)
      REAL AA                    ! Previous A preset to 0. A'
      REAL AAA                   ! Previous A preset to 0. A"
      REAL B                     ! Work variable(?)
      REAL C                     ! Work variable(?)
      REAL D                     ! Work variable(?)
      REAL E                     ! Standard ewxponential deviate
      REAL E1, E2, E3, E4, E5    ! Coefficients for sum exp(Q) =
                                 ! E(k)*Q**k
      REAL P                     ! Work variable(?)
      REAL Q                     ! Quotient
      REAL Q0                    ! Polynomial sum
      REAL Q1, Q2, Q3, Q4        ! { Coefficients for Q0 = sum
      REAL Q5, Q6, Q7            ! { Q0 = Q(k)*A**(-k)
      REAL R                     ! Inverse A
      REAL S                     ! Standard deviation
      REAL S2                    ! Variance
      REAL SI                    ! Constant depending on A(?)
      REAL SQRT32                ! Square root of 32
      REAL T                     ! Standard normal deviate
      REAL U                     ! Random value in range [0,1]
      REAL V                     ! Ratio of difference to probability
      REAL W                     ! Sum of Q polynomial
      REAL X                     ! Work variable

*  Local Data:
      DATA A1, A2, A3 / 0.3333333, -0.2500030, 0.2000062 /
      DATA A4, A5, A6 / -0.1662921, 0.1423657, -0.1367177 /
      DATA A7 / 0.1233795 /

      DATA AA/0.0/, AAA/0.0/, SQRT32/5.656854/

      DATA E1, E2, E3, E4 / 1.0, 0.4999897, 0.1668290, 0.0407753 /
      DATA E5/ 0.0102930 /

      DATA Q1, Q2, Q3 / 0.04166669, 0.02083148, 0.00801191 /
      DATA Q4, Q5, Q6 / 0.00144121, -0.00007388, 0.00024511 /
      DATA Q7 / 0.00024240 /

      SAVE AA, AAA, B, C, D, S, S2, SI

*.

*  Assign a dummy value first.
      PDA_RNGAM = 0.0

*  Return the dummy value if the supplied mean is not positive.
      IF ( A .LE. 0.0 ) RETURN

      IF ( A .NE. AA ) THEN
         IF ( A .LT. 1.0 ) THEN

*  Alternate method for parameter A below 1 (0.3678794=exp(-1)).
            AA = 0.0
            B = 1.0 + 0.3678794 * A
   20       CONTINUE
            P = B * PDA_RAND( 0.0 )
            IF ( P .GE. 1.0 ) THEN
               PDA_RNGAM = -ALOG( ( B - P ) / A )
               IF ( PDA_RNEXP( 0.0 ) .GE. ( 1.0 - A ) *
     :              ALOG( PDA_RNGAM ) ) GO TO 999
               GO TO 20
            ELSE
               PDA_RNGAM = EXP( ALOG( P ) / A )
               IF ( PDA_RNEXP( 0.0 ) .LT. PDA_RNGAM ) GO TO 20
               GO TO 999
            END IF
         ELSE

*  Step 1: recalculations of S2, S, D if A has changed.
            AA = A
            S2 = A - 0.5
            S = SQRT( S2 )
            D = SQRT32 - 12.0 * S
         END IF
      END IF

*  Step 2: T is standard normal deviate, X = (S,1/2) - normal deviate.
*  immediate acceptance (I)
      T = PDA_RNNOR( 0.0, 1.0 )
      X = S + 0.5 * T
      PDA_RNGAM = X * X
      IF ( T .GE. 0.0 ) GO TO 999

*  Step 3: U= 0,1 -uniform sample.  Squeeze acceptance (s).
      U = PDA_RAND( 0.0 )
      IF ( D * U .LE. T * T * T ) GO TO 999

*  Step 4: recalculations of Q0, B, SI, and C if necessary.
      IF ( A .NE. AAA ) THEN
         AAA = A
         R = 1.0 / A
         Q0 = ( ( ( ( ( ( Q7 * R + Q6 ) * R + Q5 ) * R + Q4 ) * R + Q3 )
     :         * R + Q2 ) * R + Q1 ) * R

*  Approximation depending on size of parameter A the constants in the
*  expressions for B, SI, and C were established by numerical
*  experiments.

*  Case 1: A =< 3.686.
         IF ( A .LE. 3.686 ) THEN

            B = 0.463 + S - 0.178 * S2
            SI = 1.235
            C = 0.195 / S - 0.079 + 0.016 * S

*  Case 2:  3.686 < A =< 13.022.
         ELSE IF ( A .LE. 13.022 ) THEN

            B = 1.654 + 0.0076 * S2
            SI = 1.68 / S + 0.275
            C = 0.062 / S + 0.024

*  Case 3: A > 13.022.
         ELSE
            B = 1.77
            SI = 0.75
            C = 0.1515 / S
         END IF
      END IF

*  Step 5: perform no quotient test if X is not positive.
      IF ( X .GT. 0.0 ) THEN

*  Step 6: calculation of V and the quotient Q.
         V = T / ( S + S )
         IF ( ABS( V ) .LE. 0.25 ) THEN
            Q = Q0 + 0.5 * T * T * ( ( ( ( ( (A7 * V + A6) * V + A5)
     :          * V + A4 ) * V + A3 ) * V + A2 ) * V + A1 ) * V
         ELSE
            Q = Q0 - S * T + 0.25 * T * T + ( 2 * S2 ) * ALOG( 1.0 + V )
         END IF

*  Step 7: quotient acceptance (Q)
         IF ( ALOG( 1.0 - U ) .LE. Q ) GO TO 999

      END IF

*  Step 8:  E=standard exponential deviate,  U = 0,1 -uniform deviate
*  T =( B, SI)-double exponential (Laplace) sample.
  100 CONTINUE
      E = PDA_RNEXP( 0.0 )
      U = PDA_RAND( 0.0 )
      U = U + U - 1.0
      T = B + SIGN( SI * E, U )

*  Step 9: Rejection if T < TAU(1) = -0.71874483771719
      IF ( T .LT. (-0.7187449) ) GO TO 100

*  Step 10: calculation of V and quotient q.
      V = T / ( S + S )
      IF ( ABS( V ) .LE. 0.25 ) THEN
         Q = Q0 + 0.5 * T * T * ( ( ( ( ( ( A7 * V + A6 ) *  V + A5 )
     :       * V + A4 ) * V + A3 ) * V + A2 ) * V + A1 ) * V
      ELSE
         Q = Q0 - S * T + 0.25 * T * T + ( S2 + S2 ) * ALOG( 1.0 + V )
      END IF

*  Step 11: test hat acceptance (H) (if Q is not positive go to step 8)
      IF ( Q .LE. 0.0 ) GO TO 100
      IF ( Q .LE. 0.5 ) THEN
         W = ( ( ( ( E5 * Q + E4 ) * Q + E3 ) * Q + E2 ) * Q + E1 ) * Q
      ELSE
         W = EXP( Q ) - 1.0
      END IF

*  If T is rejected, sample again at step 8.
      IF ( C * ABS( U ) .GT. W * EXP( E - 0.5 * T * T ) ) GO TO 100
      X = S + 0.5 * T
      PDA_RNGAM = X * X

 999  CONTINUE

      END
