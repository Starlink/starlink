      SUBROUTINE IRA_ACTIV( ACTIVE )
*+
*  Name:
*     IRA_ACTIV

*  Purpose:
*     Determine if the IRA astrometry package is currently active.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_ACTIV( ACTIVE )

*  Description:
*     This routine returns a true value if IRA is currently active
*     (i.e. if IRA_INIT has been called), and a false value otherwise.

*  Arguments:
*     ACTIVE = LOGICAL (Returned)
*        The current status of the IRA package.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-APR-1993 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE_ constants.
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_STATE = CHARACTER (Read)
*           Set to the value of symbolic constant IRA__GOING when
*           IRA is initialised.

*  External References:
      EXTERNAL IRA1_INIT         ! Initialise IRA common blocks.

*  Arguments Returned:
      LOGICAL ACTIVE

*.

      IF( ACM_STATE .EQ. IRA__GOING ) THEN
         ACTIVE = .TRUE.
      ELSE
         ACTIVE = .FALSE.
      END IF

      END
