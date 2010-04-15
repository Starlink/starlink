      SUBROUTINE DSA2_PIXIN( PIXIND, STATUS )
*+
*  Name:
*     DSA2_PIXIN

*  Purpose:
*     Determines which type of default axis co-ordinates are required.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA2_PIXIN( PIXIND, STATUS )

*  Description:
*     This routine searches for the presence of the environment
*     variable FIGARO_AXES.  If it is present, this indicates that
*     Figaro-style axis centres should be used; these are the pixel
*     indices (i.e. start at the lower bound and increment by 1 per
*     pixel).  If FIGARO_AXES is absent or a bad status returned when
*     finding its translation, then the Starlink standard default
*     co-ordinates (SSN/22) are used; these are the pixel indices less
*     0.5.  A bad status is annulled.

*  Arguments:
*     PIXIND = LOGICAL (Returned)
*        If .TRUE., FIGARO_AXES is defined, and hence pixel indices
*        should be used for the default axis centres.  If .FALSE.,
*        FIGARO_AXES is not defined, and so the default axis centres
*        should adhere to Starlink standard.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 July 9 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      LOGICAL PIXIND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 132 ) TRANS  ! The value of FIGARO_AXES environment
                                 ! variable

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the default value to the Starlink standard.
      PIXIND = .FALSE.

*  Start a new error context.
      CALL ERR_MARK

*  Attempt to obtain the environment variable FIGARO_AXES.
      CALL PSX_GETENV( 'FIGARO_AXES', TRANS, STATUS )

*  Any bad status is regarded as a failure.
      IF ( STATUS .NE. SAI__OK ) THEN

*  Annul the error, as it merely tells us that the environment variable
*  is undefined.
         CALL ERR_ANNUL( STATUS )

*  The only case where we should change the default is when nothing went
*  wrong and the translation is not null.
      ELSE IF ( TRANS .NE. ' ' ) THEN
         PIXIND = .TRUE.

      END IF

      END
