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
*     RESULT = RANNORM( A, B, STATUS )

*  Description:
*     The routine calls the NAG library function G05DDF to return the
*     pseudo random number

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
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     3-SEP-1996 (PDRAPER):
*        Changed to use non-NAG routine PDA_DRNOR (not really a PDA
*        routine. Need to replace with the correct version when released).
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
      EXTERNAL PDA_DRNOR
      DOUBLE PRECISION PDA_DRNOR ! The pseudo random normal number
                                 ! generator

*  Local variables:
      LOGICAL SEEDED            ! Whether generator seeded or not


*  Local data:
      DATA SEEDED /.FALSE./
      SAVE SEEDED
*.

*  Set the generator seed if not already done.
      IF ( .NOT. SEEDED ) THEN 
         CALL PDA_DSTART( 1024 )
         SEEDED = .TRUE. 
      END IF

*  And generate the random number. Note this has a mean of zero and a
*  standard deviation of 1.
      RANORM = REAL ( PDA_DRNOR() ) * B + A

      END
* $Id$
