      SUBROUTINE PON_EXIT()
*+
*  Name:
*     PON_EXIT

*  Purpose:
*     Checks that PONGO is closed before exiting program.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL PON_EXIT()

*  Description:
*     This routine is designed to be called as part of the close down
*     of the PONGO monolith (probably by a function invoked via the
*     atexit mechanism). It checks that ENDPLOT has been called and if
*     not it issues a warning and tries to close down AGI and PGPLOT
*     and exits. It should really be used just as a reminder since
*     I suspect that AGI isn't working by the time this gets called
*     (presumably HDS is closed).

*  Arguments:
*     None.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1996 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants.

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP         ! PGPLOT device is open

*  Local Variables:
      INTEGER STATUS            ! Local status
*.

*  If the PGPLOT device is open then complain, close things down and
*  return. Note use WRITE(*,*) as messages do seem to appear at this
*  point, hope AGI calls still work, the AGP one seems to not return,
*  hence it has been commented out. PGEND also does not return at this
*  point, so leave device open and hope for the best.
      CALL ERR_MARK
      STATUS = SAI__OK
      IF ( PON_DEVOP( .FALSE., STATUS ) ) THEN
         CALL AGI_END( -1, STATUS )
         CALL AGI_CANCL( 'DEVICE', STATUS )
C         CALL AGP_DEACT( STATUS )

*  Take care as some systems (IRAF/CL) take tight control of this channel.
         WRITE(*,*,ERR=99)
     :'Warning PONGO is exiting before running ENDPLOT.'
         WRITE(*,*,ERR=99)
     :'An attempt to salvage your AGI database has been made.'
 99      CONTINUE
      END IF
      CALL ERR_RLSE
      END
