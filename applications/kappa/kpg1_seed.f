      REAL FUNCTION KPG1_SEED( STATUS )
*+
*  Name:
*     KPG1_SEED

*  Purpose:
*     Obtain a semi-random seed for random-number generation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_SEED( STATUS )

*  Description:
*     This function uses the computer time from an arbitrary date to
*     generate a non-repeatable seed.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     KPG1_SEED = REAL
*        The non-repeatable seed for use in SLA_RANDOM.  Note that
*        it is not necessarily in the range 0 to 1.

*  [optional_function_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 March 17 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NORM               ! Normalisation factor to modulate
                                 ! the time ticks
      PARAMETER ( NORM = 86400 )

*  Local Variables:
      INTEGER NTICKS             ! Number of computer clock ticks

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the computer time since an arbitrary epoch.
      CALL PSX_TIME( NTICKS, STATUS )

*  Generate the seed as an odd number (of seconds since midnight).
      KPG1_SEED = REAL( MOD( NTICKS, NORM ) * 2 + 1 )

      END
