      INTEGER FUNCTION PDA_RNPOI( MEAN )
*+
*  Name:
*     PDA_RNPOI

*  Purpose:
*     Returns pseudo-random numbers from a Poisson distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PDA_RNPOI( MEAN )

*  Description:
*     This is a simple random-number generator providing deviates in the
*     from a Poisson distribution, with a period of 2**26, and to 6 or
*     7 digits accuracy.  It is based upon Ahrens, Dieter & Grube's
*     TOMS599 routines.

*  Arguments:
*     MEAN = REAL (Given)
*        The mean value of the Poisson distribution.

*  Returned Value:
*     PDA_RNPOI = INTEGER
*        The pseudo-random deviate. A value of -1 is returned if the
*        supplied mean is not positive.

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
*     The Poisson-distribution deviates are found using the algorithm
*     given in the reference below.

*  References:
*     Ahrens, J.H., & Dieter, U. 1973, "Computer Generation of Poisson
*       Deviates from modified Normal distributions", ACM Trans. Math.
*       Software, 8(2), pp.163--179.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 21 (MJC)
*        Original version.
*     1997 February 26 (DSB)
*        Remove "      INCLUDE 'SAE_PAR'".
*        Remove STATUS argument. "SAVE" statement added since Linux
*        really does clobber previous values.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL MEAN

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
      REAL A0, A1, A2, A3        ! Coefficients for step F
      REAL A4, A5, A6, A7        ! Coefficients for step F
      REAL B1, B2                ! { Hermite approximations to discrete
      REAL C, C0, C1, C2, C3     ! { Normal probabilities
      REAL D                     !
      REAL DEL                   !
      INTEGER DEVIAT             ! Deviate
      REAL DIFMUK                ! Difference between mean and discrete
                                 ! Normal probability
      REAL E                     ! Random exponential deviate
      REAL FACT( 10 )            ! Factorials
      REAL FK                    ! Discrete Normal probability
      REAL FX                    !
      REAL FY                    !
      REAL G                     ! Random normal deviate
      INTEGER J                  ! Lower index to search cumulative
                                 ! probabilities
      INTEGER K                  ! Loop counter
      INTEGER KFLAG              ! Status flag?
      INTEGER L                  ! Deviate where Poisson probabilities
                                 ! exceed the discrete normal probs and
                                 ! upper index to search cumulative
                                 ! probabilities
      INTEGER M                  ! Positive mean
      REAL MU                    ! Mean
      REAL MUPREV                ! Previous mean
      REAL MUOLD                 ! The mean at last execution of step
                                 ! P or B
      REAL OMEGA                 ! SQRT(1/2signma*pi)
      REAL P                     !
      REAL P0                    !
      REAL PP( NPROB )           ! Cumulative Poisson probabilities
      REAL PX                    !
      REAL PY                    !
      REAL Q                     !
      REAL S                     ! Standard deviation
      REAL T                     ! Sample from Laplace `hat'/ number of
                                 ! standard deviations
      REAL U                     ! Random value in range [0,1]
      REAL V                     ! Ratio of difference to probability
      REAL X                     !
      REAL XX                    ! X squared

* Ensure the following variables retain their values between calls.
      SAVE MUPREV, MUOLD, M, L, P, Q, P0, S, D

*  Local Data:
      DATA A0, A1, A2 / -0.5, 0.3333333, -0.2500068 /
      DATA A3, A4, A5 / 0.2000118, -0.1661269, 0.1421878 /
      DATA A6, A7 / -0.1384794, 0.1250060 /
      DATA FACT / 1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0, 5040.0,
     :            40320.0, 362880.0 /
      DATA MUPREV, MUOLD / 0.0, 0.0 /

*.

*  Assign a dummy value first.
      PDA_RNPOI = -1

*  Return the dummy value if the supplied mean is not positive.
      IF ( MEAN .LE. 0.0 ) RETURN

*  Separation of cases A and B.
      MU = MEAN
      IF ( MU .NE. MUPREV ) THEN
         IF ( MU .LT. 10.0 ) THEN

*  C A S E  B.
*  ===========
*
*  Start new table and calculate P0 if necessary.
            MUPREV = 0.0
            IF ( MU .NE. MUOLD ) THEN
               MUOLD = MU
               M = MAX( 1, IFIX( MU ) )
               L = 0
               P = EXP( -MU )
               Q = P
               P0 = P
            END IF
            GO TO 300
         ELSE

*  C A S E  A.
*  ===========
*
*  Recalculation of S, D, L if MU has changed.
            MUPREV = MU
            S = SQRT( MU )
            D = 6.0 * MU * MU

*  The Poisson probabilities PK exceed the discrete normal
*  probabilities FK whenever K >= M(MU).  L=IFIX(MU-1.1484) is an upper
*  bound to M(MU) for all MU >= 10.
            L = IFIX( MU-1.1484 )
         END IF
      END IF

*  STEP N.
*  =======
*
*  Normal sample: use PDA_RNNOR for standard Normal deviate.
      G = MU + S * PDA_RNNOR( 0.0, 1.0 )
      IF ( G .GE. 0.0 ) THEN
         DEVIAT = IFIX( G )

*   STEP I.
*   =======
*
*   Immediate acceptance if PDA_RNPOI is large enough.
         IF ( DEVIAT .GE. L ) GO TO 999

*   STEP S.
*   =======
*
*   Squeeze acceptance---PDA_RAND for (0,1)-sample U.
         FK = REAL( DEVIAT )
         DIFMUK = MU - FK
         U = PDA_RAND( 0.0 )
         IF ( D * U .GE. DIFMUK * DIFMUK * DIFMUK ) GO TO 999
      END IF

*   STEP P.
*   =======
*
*   Preparations for steps Q and H (recalculating parameters if
*   necessary).  The constants whose numerical values are given are
*   0.3989423 = (2*pi)**(-0.5),  0.0416667 = 1./24., and
*   0.1428571 = 1./7.  The quantities B1, B2, C3, C2, C1, C0 are for
*   the Hermite approximations to the discrete Normal probabilities FK.
*   c = 0.1069/MU guarantees majorization by the 'hat'-function.
      IF ( MU .NE. MUOLD ) THEN
         MUOLD = MU
         OMEGA = 0.3989423 / S
         B1 = 0.04166667 / MU
         B2 = 0.3 * B1 * B1
         C3 = 0.1428571 * B1 * B2
         C2 = B2 - 15.0 * C3
         C1 = B1 - 6.0 * B2 + 45.0 * C3
         C0 = 1. - B1 + 3.0 * B2 - 15.0 * C3
         C = 0.1069 / MU
      END IF

      IF ( G .GE. 0.0 ) THEN

*  'Subroutine' F is called (KFLAG = 0 for correct return).
         KFLAG = 0
         GO TO 200
      END IF

*   STEP E.
*   =======

*   Exponential sample: use PDA_RNEXP for standard exponential deviate
*   E and sample T from the Laplace 'hat' (if T <= -0.6744 then
*   PK < FK for all MU >= 10.).  Loop until T is large enough
  100 CONTINUE
      E = PDA_RNEXP( 0.0 )
      U = PDA_RAND( 0.0 )
      U = U + U - 1.0
      T = 1.8 + SIGN( E, U )
      IF ( T .LE. (-0.6744) ) GO TO 100

      DEVIAT = IFIX( MU + S * T )
      FK = REAL( DEVIAT )
      DIFMUK = MU - FK

*  'Subroutine' F is called (KFLAG=1 for correct return).
      KFLAG = 1

*   STEP F.
*   =======
*
*   'Subroutine' F.  Calculation of PX, PY, FX, FY.  When the Poisson
*   deviate is < 10 it uses factorials from table FACT.
C
 200  CONTINUE
      IF ( DEVIAT .GE. 10 ) THEN

*   Poission deviate >= 10 uses a polynomial approximation evaluated
*   using Horner's method.  "A0-A7 for accuracy when advisable."  Note
*   the constants mean the following: 0.08333333 = 1./12. and 0.3989423
*   = (2*PI)**(-0.5).
         DEL = 0.08333333 / FK
         DEL = DEL - 4.8 * DEL * DEL * DEL
         V = DIFMUK / FK
         IF ( ABS( V ) .LE .0.25 ) THEN
            PX = (((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V+A0) *
     :           FK * V * V - DEL
         ELSE
            PX = FK * ALOG( 1.0 + V ) - DIFMUK - DEL
         END IF

         PY = 0.3989423 / SQRT( FK )

      ELSE
         PX = -MU
         PY = MU**DEVIAT / FACT( DEVIAT + 1 )

      END IF

      X = ( 0.5 - DIFMUK ) / S
      XX = X * X
      FX = -0.5 * XX
      FY = OMEGA * ( ( ( C3 * XX + C2) * XX + C1 ) * XX + C0 )
      IF ( KFLAG .LE. 0 ) THEN

*  STEP Q.
*  =======

*  Quotient acceptance (rare case).
         IF ( FY - U * FY .LE. PY * EXP( PX - FX ) ) GOTO 999
         GO TO 100
      ELSE

*  STEP H.
*  =======
*
*  Hat acceptance (E is repeated on rejection).
         IF ( C * ABS( U ) .GT. PY * EXP( PX + E ) -
     :        FY * EXP( FX + E ) ) GO TO 100
         GOTO 999
      END IF

*  STEP U.
*  =======
*
*  Uniform sample for inversion method.
 300  CONTINUE
      U = PDA_RAND( 0.0 )
      DEVIAT = 0
      IF ( U .LE. P0 ) GOTO 999

*  STEP T.
*  =======
*
*  Table comparison until the end PP(L) of the PP-table of cumulative
*  Poisson probabilities (0.458 = PP( 9 ) for MU = 10).
      IF ( L .NE. 0 ) THEN
         J = 1
         IF ( U .GT. 0.458 ) J = MIN( L, M )
         DO 350 K = J, L
            IF ( U .LE. PP( K ) ) GO TO 600
 350     CONTINUE
         IF ( L .EQ. NPROB ) GO TO 300
      END IF

*  STEP C.
*  =======
*
*  Creation of new Poisson probabilities P and their cumulatives
*  Q = PP(K).
      L = L + 1
      DO 400 K = L, NPROB
         P = P * MU / REAL( K )
         Q = Q + P
         PP( K ) = Q
         IF ( U .LE. Q ) GO TO 500
 400  CONTINUE
      L = NPROB
      GO TO 300

 500  CONTINUE
      L = K

 600  CONTINUE
      DEVIAT = K

 999  CONTINUE
      PDA_RNPOI = DEVIAT

      END
