      REAL FUNCTION RANORM( A, B, STATUS )
*+
*  Name:
*     RANNORM

*  Purpose:
*     To return a pseudo-random real number taken from a normal
*     ( Gaussian ) distribution, with mean A and standard deviation B.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = RANORM( A, B, STATUS )

*  Description:
*     The routine calls the PDA_DRNOR pseudo random number generator
*     with a random seed for each time the program starts.

*  Arguments:
*     A = REAL (Given)
*        The mean of the normal distribution.
*     B = REAL (Given)
*        The standard deviation of the distribution.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     RANORM = REAL
*        An random number from the normal distribution, with mean A and
*        standard deviation B.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     3-SEP-1996 (PDRAPER):
*        Changed to use non-NAG routine PDA_DRNOR (not really a PDA
*        routine. Need to replace with the correct version when released).
*     23-OCT-2002 (PDRAPER):
*        Added change to use a different seed for each initialization.
*     15-JUL-2004 (TIMJ)
*        Use renamed PDA_DRNOR from PDA library
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL A
      REAL B

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL PDA_DRANN
      DOUBLE PRECISION PDA_DRANN ! The pseudo random normal number
                                 ! generator

*  Local variables:
      INTEGER SEED              ! The seed value

*  Local data:
      DATA SEED / 0 /
      SAVE SEED
*.

*  Set the generator seed if not already done.
      IF ( SEED .EQ. 0  ) THEN
*         CALL PSX_TIME( SEED, STATUS )
*  Use PID rather than some tick measurement as ticks are seconds, which
*  isn't always enough resolution to get a unique values, machines are
*  getting fast... May not work for non-UNIX.
         CALL PSX_GETPID( SEED, STATUS )
         CALL PDA_DRANS( SEED )
      END IF

*  And generate the random number. Note this has a mean of zero and a
*  standard deviation of 1.
      RANORM = REAL ( PDA_DRANN() ) * B + A

      END
* $Id$
