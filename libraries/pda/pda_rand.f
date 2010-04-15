      REAL FUNCTION PDA_RAND( X )
*+
*  Name:
*     PDA_RAND

*  Purpose:
*     Returns pseudo-random numbers in the range 0 to 1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PDA_RAND( X )

*  Description:
*     This is a simple random number generator providing deviates in the
*     range 0 to 1, with period of 2**26, and to 6 or 7 digits
*     accuracy.  It is based upon Ahrens, Dieter & Grube's TOMS599
*     routines.  Note that there is no STATUS argument for efficiency.

*  Arguments:
*     X = REAL (Given)
*        This is a dummy variable required by the Fortran standard.

*  Returned Value:
*     PDA_RAND = REAL
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

*  References:
*     Ahrens, J.H., Dieter, U. & Grube, A., 1970, "Pseudo-random
*       numbers: a new proposal for the choice of multiplicators",
*       Computing, 6, pp.121--138.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 20 (MJC)
*        Original version.
*     1997 February 26 (DSB)
*        Removed "      INCLUDE 'SAE_PAR'".
*        Explicitly included common block PDA_SEEDY.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      DOUBLE PRECISION R         ! The current deviate.
      COMMON /PDA_SEEDY/  R

*  Arguments Given:
      REAL X

*  Local Variables:
      DOUBLE PRECISION FACTOR    ! Multiplier in congruential generator

      DATA FACTOR /41475557.0D0/

*.

*   Evaluate the random number, using the last value as a seed for the
*   next.
      R = MOD( R * FACTOR, 1.0D0 )
      PDA_RAND = SNGL( R )

      END
