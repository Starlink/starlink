      REAL FUNCTION PDA_RNNOR( MEAN, SIGMA )
*+
*  Name:
*     PDA_RNNOR

*  Purpose:
*     Returns pseudo-random numbers from a Gaussian distribution.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PDA_RNNOR( MEAN, SIGMA )

*  Description:
*     This is a simple random-number generator providing deviates in the
*     from a Gaussian distribution, with a period of 2**26, and to 6 or
*     7 digits accuracy.  It is based upon Ahrens, Dieter & Grube's
*     TOMS599 routines.

*  Arguments:
*     MEAN = REAL (Given)
*        The mean value of the Gaussian distribution.
*     SIGMA = REAL (Given)
*        The standard deviation of the Gaussian distribution.

*  Returned Value:
*     PDA_RNNOR = REAL
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
*     The Normal-distribution deviates are found using the "FL" (M=5)
*     algorithm given in the reference below.

*  References:
*     Ahrens, J.H., & Dieter, U. 1973, "Extensions of Forsythe's
*       Method for Random Sampling from the Normal distribution",
*       Math. Computing, 27(124), pp.927--937.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 20 (MJC)
*        Original version based on Grant Privett's partially tidied
*        version of SNORM.
*     1997 February 26 (DSB)
*        Remove "      INCLUDE 'SAE_PAR'"
*     23-NOV-2005 (DSB)
*        Guard against indexing the D array out of bounds.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL MEAN
      REAL SIGMA

*  External References:
      EXTERNAL PDA_RAND
      REAL PDA_RAND              ! Pseudo-random number in range [0,1]

*  Local Constants:
      INTEGER NCON               ! Number of constants
      PARAMETER ( NCON = 31 )

*  Local Variables:
      DOUBLE PRECISION A( NCON + 1 )
      DOUBLE PRECISION D( NCON )
      DOUBLE PRECISION H( NCON )
      INTEGER I                  ! Distribution bin number
      DOUBLE PRECISION T( NCON )
      REAL U                     ! Random value in range [0,1]
      REAL S,USTAR,W,TT,AA,Y     ! ?

*  Local Data:
*     Distribution data.
      DATA A/ 0.0, 0.3917609E-1, 0.7841241E-1, 0.1177699, 0.1573107,
     :        0.1970991, 0.2372021, 0.2776904, 0.3186394, 0.3601299,
     :        0.4022501, 0.4450965, 0.4887764, 0.5334097, 0.5791322,
     :        0.6260990, 0.6744898, 0.7245144, 0.7764218, 0.8305109,
     :        0.8871466, 0.9467818, 1.009990,  1.077516, 1.150349,
     :        1.229859, 1.318011, 1.417797, 1.534121, 1.675940,
     :        1.862732, 2.153875 /

      DATA D/ 5  *  0.0, 0.2636843, 0.2425085, 0.2255674, 0.2116342,
     :        0.1999243, 0.1899108, 0.1812252, 0.1736014, 0.1668419,
     :        0.1607967, 0.1553497, 0.1504094, 0.1459026, 0.1417700,
     :        0.1379632, 0.1344418, 0.1311722, 0.1281260, 0.1252791,
     :        0.1226109, 0.1201036, 0.1177417, 0.1155119, 0.1134023,
     :        0.1114027, 0.1095039 /

      DATA H/ 0.3920617E-1, 0.3932705E-1, 0.3950999E-1, 0.3975703E-1,
     :        0.4007093E-1, 0.4045533E-1, 0.4091481E-1, 0.4145507E-1,
     :        0.4208311E-1, 0.4280748E-1, 0.4363863E-1, 0.4458932E-1,
     :        0.4567523E-1, 0.4691571E-1, 0.4833487E-1, 0.4996298E-1,
     :        0.5183859E-1, 0.5401138E-1, 0.5654656E-1, 0.5953130E-1,
     :        0.6308489E-1, 0.6737503E-1, 0.7264544E-1, 0.7926471E-1,
     :        0.8781922E-1, 0.9930398E-1, 0.1155599, 0.1404344,
     :        0.1836142, 0.2790016, 0.7010474 /

      DATA T/ 0.7673828E-3, 0.2306870E-2, 0.3860618E-2, 0.5438454E-2,
     :        0.7050699E-2, 0.8708396E-2, 0.1042357E-1, 0.1220953E-1,
     :        0.1408125E-1, 0.1605579E-1, 0.1815290E-1, 0.2039573E-1,
     :        0.2281177E-1, 0.2543407E-1, 0.2830296E-1, 0.3146822E-1,
     :        0.3499233E-1, 0.3895483E-1, 0.4345878E-1, 0.4864035E-1,
     :        0.5468334E-1, 0.6184222E-1, 0.7047983E-1, 0.8113195E-1,
     :        0.9462444E-1, 0.1123001, 0.1364980, 0.1716886, 0.2276241,
     :        0.3304980, 0.5847031 /

*.

*  Start of a "DO WHILE" loop
 10   CONTINUE

*  Obtain a deviate in the range 0 to 1.
      U = PDA_RAND( 0.0 )

*  Use half the range but maintain the [0,1] scaling.
      IF ( U .GE. 0.5 ) THEN
         S = 1.0
      ELSE
         S = 0.0
      END IF

      U = U + U - S

*  Try again if U is zero (a value of zero would produce an infinite loop
*  below).
      IF( U .EQ. 0.0 ) GO TO 10

*  Find to which distribution element that corresponds.
      U = 32.0 * U
      I = INT( U )
      IF ( I .EQ. 0 ) THEN

*  Start at the tail.
*  ==================
         I = 6
         AA = A( 32 )

*  Start of a 'DO WHILE' loop (forming cumulative distribution?)
 50      CONTINUE
         U = U + U
         IF ( U .LT. 1.0 .AND. I .LE. NCON ) THEN
            AA = AA + D( I )
            I = I + 1
            GO TO 50

*  Centre to zero.
         ELSE
            U = U - 1.0
            GO TO 200
         END IF

*  Start in the centre.
*  ====================
      ELSE
         USTAR = U - REAL( I )
         AA = A( I )

*  Start of 'DO WHILE' loop.
 100     CONTINUE
         IF ( USTAR .LE. T( I ) ) THEN

*  Centre continued.  Get another random number.
            U = PDA_RAND( 0.0 )
            W = U * ( A( I+1 ) - AA )
            TT = ( 0.5 * W + AA ) * W

*  Start of 'DO WHILE' loop.
 150        CONTINUE
            IF ( USTAR .LE. TT ) THEN
               U = PDA_RAND( 0.0 )
               IF ( USTAR .GE. U ) THEN

*  Get another random number and return to the start of the inner
*  'DO WHILE' loop.
                  TT = U
                  USTAR = PDA_RAND( 0.0 )
                  GO TO 150
               ELSE

*  Get another random number and return to the start of the outer
*  'DO WHILE' loop.
                  USTAR = PDA_RAND( 0.0 )
                  GO TO 100
               END IF
            END IF

         ELSE
            W = ( USTAR - T( I ) ) * H( I )

         END IF
      END IF

      GO TO 300

 200  CONTINUE
      W = U * D( I )
      TT = ( 0.5 * W + AA ) * W

 250  CONTINUE
      USTAR = PDA_RAND( 0.0 )
      IF ( USTAR .GT. TT ) GO TO 300

      U = PDA_RAND( 0.0 )
      IF ( USTAR .GE. U ) THEN
         TT = U
         GO TO 250

      ELSE
         U = PDA_RAND( 0.0 )
         GO TO 200

      END IF

*  Exit (both cases).
  300 CONTINUE

      Y = AA + W
      IF ( S .EQ. 1.0 ) THEN
         PDA_RNNOR = MEAN - Y * SIGMA
      ELSE
         PDA_RNNOR = MEAN + Y * SIGMA
      END IF

      END
