      SUBROUTINE LORAD( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE LORAD

*  Description:
*     Define radial grid for LORES.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LORAD( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Aperture index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The LBLS radial grid is based on RL and RSAMP.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IAPER   ! Aperture index.

*  Status:
      INTEGER STATUS  ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set EXTP undefined.
      NOEXTP = .TRUE.

*   Automatic slit mode.
      IF ( AUSLIT ) THEN
         RL( 2 ) = 10.6
         RL( 2 ) = NINT( REAL( RL( 2 ) / RSAMP ) ) * RSAMP
         RL( 1 ) = -RL( 2 )
      END IF

*   Object slit radii (concessions for TRAK steals).
      ROBJ( 1 ) = RL( 1 )
      ROBJ( 2 ) = RL( 2 )
      NBKG = 0

*   OK now.
      NOEXTP = .FALSE.

      END
