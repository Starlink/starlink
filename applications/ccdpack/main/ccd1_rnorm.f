      REAL FUNCTION CCD1_RNORM( A, B, STATUS )
*+
*  Name:
*     CCD1_RNORM

*  Purpose:
*     To return a pseudo-random real number taken from a normal
*     ( Gaussian ) distribution, with mean A and standard deviation B.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = RANNORM( A, B, STATUS )

*  Description:
*     The routine calls the PDA library function PDA_DRNOR to return the
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
*     19-SEP-1996 (PDRAPER):
*        Now uses PDA routine instead of NAG routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL A
      REAL B

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PDA_DRNOR
      DOUBLE PRECISION PDA_DRNOR ! Pseudo random normal number generator
      EXTERNAL PDA_DSTART
      DOUBLE PRECISION PDA_DSTART ! Seed for generator

*  Local Variables:
      DOUBLE PRECISION AA        ! buffer for A
      DOUBLE PRECISION BB        ! buffer for B
      INTEGER ISEED              ! seed value for generator
      LOGICAL FIRST              ! whether generator is seeded
      DATA ISEED /32767/
      DATA FIRST /.TRUE./
      SAVE FIRST
*.

      IF ( FIRST ) THEN
         AA = PDA_DSTART(ISEED)
         FIRST = .FALSE.
      END IF
      AA = DBLE( A )
      BB = DBLE( B )
      CCD1_RNORM = REAL( PDA_DRNOR() ) * BB + AA

      END
* $Id$
