      SUBROUTINE CALGEN( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE CALGEN

*  Description:
*     The Net is used as a starting attempt at Flx.
*     The Air wavelengths are craeted SEPERATELY from the vacuum
*     ones.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CALGEN( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Aperture index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     07-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMWAV'
      INCLUDE 'CMNET'
      INCLUDE 'CMCAL'
      INCLUDE 'CMFLX'

*  Arguments Given:
      INTEGER IAPER      ! Aperture index.

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      REAL*8 SCALE       ! Local scaling factor.

      INTEGER I          ! Loop index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Transfer Net to Flx.
      NOFLX = .TRUE.
      NOCAL = .TRUE.

      IF ( NONET ) THEN
         CALL ERROUT( 'Error: no net spectrum\\', STATUS )

      ELSE
         CALL STR_MOVE( 'Net\\', 40, NLABEL )
         CALL STR_MOVE( 'Flux\\', 40, FLABEL )

         IF ( PHOT ) THEN
            CALL STR_MOVE( '(FN)\\', 40, NUNITS )

            IF ( TSECS( IAPER ) .GT. 0.0 ) THEN
               SCALE = FSCALE( IAPER ) / TSECS( IAPER )
               CALL STR_MOVE( '(FN/s)\\', 40, FUNITS )

            ELSE
               SCALE = FSCALE( IAPER )
               CALL STR_MOVE( '(FN)\\', 40, FUNITS )
            END IF

         ELSE
            SCALE = 1.0
            CALL STR_MOVE( '(DN)\\', 40, NUNITS )
            CALL STR_MOVE( '(DN)\\', 40, FUNITS )
         END IF

         CALL STR_MOVE( 'Wavelength\\', 40, WLABEL )
         CALL STR_MOVE( '(A)\\', 40, WUNITS )

         DO I = 1, NWAV
            SFLX( I ) = SNET( I )
            DFLX( I ) = DNET( I )
            QFLX( I ) = QNET( I )
            IF ( SCALE .GT. 0.0 ) THEN
               SCAL( I ) = 1.0 / SCALE
               QCAL( I ) = 0

            ELSE
               QCAL( I ) = 1
            END IF
            WAVAIR( I ) = WAV( I )
         END DO

         NOCAL = .FALSE.

*      Air wavelengths.
         CALL IUE_TAIR( NWAV, WAVAIR )
      END IF

      END
