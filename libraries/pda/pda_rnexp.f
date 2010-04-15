      REAL FUNCTION PDA_RNEXP( X )
*+
*  Name:
*     PDA_RNEXP

*  Purpose:
*     Returns pseudo-random numbers from an exponential distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PDA_RNEXP( X )

*  Description:
*     This is a simple random-number generator providing deviates in the
*     from an exponential distribution, with a period of 2**26, and to
*     6 or 7 digits accuracy.  It is based upon Ahrens, Dieter &
*     Grube's TOMS599 routines.

*  Arguments:
*     X = REAL (Given)
*        This is a dummy variable required by the Fortran standard.

*  Returned Value:
*     PDA_RNEXP = INTEGER
*        The pseudo-random deviate.

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
*     The exponential-distribution deviates are found using the "SA"
*     algorithm given in the reference below.

*  References:
*     Ahrens, J.H., & Dieter, U. 1972, "Computer Methods for sampling
*     from the exponential and Normal distributions", Comm. ACM 15(10),
*     pp.873--882.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 21 (MJC)
*        Original version.
*     1997 February 26 (DSB)
*        Remove "      INCLUDE 'SAE_PAR'".
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL X

*  External References:
      EXTERNAL PDA_RAND
      REAL PDA_RAND              ! Pseudo-random number in range [0,1]

*  Local Variables:
      REAL A                     ! Work variable
      INTEGER I                  ! Q index counter
      REAL Q( 8 )                ! Sum of (ALOG(2.0)**K)/K, K=1,8
      REAL U                     ! Random value in range [0,1]
      REAL UMIN                  ! Minimum deviate
      REAL USTAR                 ! Reference deviate

*  Local Data:
*        The highest N (here 8) is determined by Q(N) = 1.0 within
*        standard precision.
      DATA Q/ 0.6931472, 0.9333737, 0.9888778, 0.9984959, 0.9998293,
     :        0.9999833, 0.9999986, 0.9999999 /

*.

*  Obtain a random number in the range [0,1].
      A = 0.0
      U = PDA_RAND( A )
      UMIN = 0.0

*  Start a `DO WHILE' loop until the multiples of the deviate exceed 1.
  100 CONTINUE
      U = U + U
      IF ( U .LE. 1.0 ) THEN
         A = A + Q( 1 )
         GO TO 100
      END IF

*  Obtain the difference above 1.
      U = U - 1.0

*  If the deviate lies after the first Q...
      IF ( U .GT. Q( 1 ) ) THEN

*  Initialise the index deviate limit, and minimum deviate.
         I = 1
         USTAR = PDA_RAND( 0.0 )
         UMIN = USTAR

*  Start of `DO WHILE' loop to find the Q value greater than U.
*  Store the mimimum deviate found during the search.
  120    CONTINUE
         USTAR = PDA_RAND( 0.0 )
         IF ( USTAR .LT. UMIN ) UMIN = USTAR
         I = I + 1
         IF ( U .GT. Q( I ) ) GO TO 120
      END IF

*  Form the result.
      IF ( U .LT. Q( 1 ) ) THEN
         PDA_RNEXP = A + U
      ELSE
         PDA_RNEXP = A + UMIN * Q( 1 )
      END IF

      END
