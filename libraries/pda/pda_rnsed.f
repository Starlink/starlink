      SUBROUTINE PDA_RNSED( SEED )
*+
*  Name:
*     PDA_RNSED

*  Purpose:
*     Sets the seed for the PDA random-number generators.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_RNSED( SEED )

*  Description:
*     This sets the initial seed for the simple random-number generator
*     based upon Ahrens, Dieter & Grube's TOMS599 routines.  The seed
*     should be of the form 4*K+1, where K is a positive integer, and
*     less than 2**28.  When it is not, the nearest valid seed is used,
*     but if this is negative, the seed becomes 2001.

*  Arguments:
*     SEED = INTEGER (Given)
*        The random-number seed.

*  Algorithm:
*     A multiplicative congruential generator of the form
*
*        R := R * FACTOR (MOD 1)
*
*     is used.  In this routine, the R is initialised to be
*
*        R := SEED / 2**28
*
*     where SEED must be of the form SEED = 4 * K + 1 (K positive
*     integer) giving 2**28 significant bits.

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
*        Removed "      INCLUDE 'SAE_PAR'" and STATUS argument.
*        Explicitly include common block PDA_SEEDY.
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
      INTEGER SEED               ! Seed value

*  Local Variables:
      INTEGER ROOT               ! A valid version of the supplied seed
      DOUBLE PRECISION TWO28     ! Initial scale factor (2**28), i.e.
                                 ! 28 significant bits for deviates

*  Local Data:
      DATA TWO28 /268435456.0D0/

*.

*   Check that the seed satisfies the criterion.  If not, use the
*   nearest suitable seed, or a fixed seed if that's negative.
      ROOT = ( ( SEED - 1 )/ 4 ) * 4 + 1
      IF ( ROOT .LT. 0 .OR. ROOT .GT. TWO28 ) ROOT = 2001

*   Scale the integer seed value.
      R = DBLE( ROOT )/ TWO28

      END
