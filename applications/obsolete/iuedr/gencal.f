      SUBROUTINE GENCAL
*+
*  Name:
*     SUBROUTINE GENCAL

*  Description:
*     The FLX arrays are assumed created previously.
*     Normally they ARE NET, but some order overlap corrections
*     may have changed this.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GENCAL

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     13-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     06-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  External References:
      INTEGER DQ_AND     ! 32 bit integer AND operation

*  Global Variables:
      INCLUDE 'CMWAV'
      INCLUDE 'CMFLX'
      INCLUDE 'CMNET'
      INCLUDE 'CMCAL'

*  Local Variables:
      INTEGER I          ! loop index
*.

      IF ( .NOT.NONET .AND. .NOT.NOCAL ) THEN
         DO I = 1, NWAV
            IF ( QCAL( I ) .NE. 0 ) THEN
               SFLX( I ) = 0.0
               DFLX( I ) = 0.0
               QFLX( I ) = 1

            ELSE IF ( DQ_AND( QFLX( I ), 1 ) .EQ. 0 ) THEN
               SFLX( I ) = SFLX( I ) / SCAL( I )
               DFLX( I ) = DFLX( I ) / SCAL( I )

            ELSE
               SFLX( I ) = 0.0
               DFLX( I ) = 0.0
               QFLX( I ) = 1
            END IF
         END DO
         NOFLX = .FALSE.
      END IF

      END
