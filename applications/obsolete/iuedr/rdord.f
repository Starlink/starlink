      SUBROUTINE RDORD( NEWORD, CALIB, STATUS )
*+
*  Name:
*     SUBROUTINE RDORD
*
*  Description:
*     The NEWORD order is looked for in the list, and if found
*     it is made current.
*     In the event of the new order not existing, no harm is done.
*     The CALIB routine is used calibrate the spectrum if it is
*     read anew.
*
*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          07-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     10-OCT-94     IUEDR Vn. 3.1-6

*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Import:
      INTEGER NEWORD     ! required order

*  Export:
      INTEGER STATUS     ! status return

*  Global variables:
      INCLUDE 'CMSPEC'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMNET'
      INCLUDE 'CMWAV'

*  External references:
      EXTERNAL CALIB

*  Local variables:
      INTEGER I          ! wavelength index
      INTEGER IORDER     ! order index
*.

      CALL FNORD( NEWORD, IORDER )
      IF ( IORDER .LE. 0 ) THEN
         CALL ERROUT( 'Error: order not found\\', STATUS )

      ELSE IF ( NEWORD .NE. ORDER ) THEN
         CALL CNORD

         NWAV = NWAVS( IORDER )

         DO I = 1, NWAV
            WAV( I ) = WAVS( I, IORDER )
            SNET( I ) = SNETS( I, IORDER )
            DNET( I ) = 0.0
            CALL DQ_UTOI( QNETS( I, IORDER ), QNET( I ) )
         END DO

         NONET = .FALSE.
         ORDER = NEWORD

         CALL CALIB( STATUS )
         IF ( STATUS .NE. SAI__OK )
     :       CALL ERROUT( 'Error: spectrum calibration failed\\',
     :                    STATUS )
      END IF

      END
