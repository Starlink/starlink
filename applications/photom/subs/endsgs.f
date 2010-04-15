************************************************************************

      SUBROUTINE ENDSGS

*+
*  Name :
*     ENDSGS
*
*  Purpose :
*     Close down SGS
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL ENDSGS
*
*  Description :
*     Close down SGS
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Local Variables :
      INTEGER ISTAT
*.

* Initialise ISTAT
      ISTAT = 0

* Close down SGS
      CALL AGS_DEACT( ISTAT )

* Close down AGI
      CALL AGI_END( -1, ISTAT )
      CALL AGI_CANCL( 'DEVICE', ISTAT )

      END

* $Id$
