      SUBROUTINE JCMT_CORRLST( NTOT, LST, STATUS )
*+
*{subroutine_prologue}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'        ! astronomical constants

*  Arguments Given:
      INTEGER NTOT

*  Arguments Given and Returned:
      DOUBLE PRECISION LST( NTOT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ADD       ! additive value to LST

      DOUBLE PRECISION LASTLST   ! LAST LST
      INTEGER I


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  quick exit if no correction needed (assume that maps never last
*  longer than a day)
      IF ( LST(NTOT) .GT. LST(1) ) RETURN
      ADD=0
      LASTLST=-1
      DO I = 1, NTOT
         IF( LASTLST .GT. LST(I) ) ADD=ADD+24
         LASTLST=LST(I)
         LST(I)=LST(I)+ADD
      END DO

      END
