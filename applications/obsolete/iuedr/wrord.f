      SUBROUTINE WRORD( STATUS )

*+
*  Name:
*     SUBROUTINE WRORD

*  Purpose:
*     Save current spectrum/order for later use.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRORD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          07-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2.  Starlink Styled.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*   Type Definitions:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Status:
      INTEGER STATUS     ! status return

*   Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMNET'
      INCLUDE 'CMWAV'

*   Local Variables:
      INTEGER I          ! wavelength index
      INTEGER IORDER     ! order index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Find existing order, or allocate a new one.
      CALL FNORD( ORDER, IORDER )
      IF ( IORDER .LE. 0 ) THEN
         IF ( NORDER .GE. 100 ) THEN
            CALL ERROUT( 'Error: too many orders\\', STATUS )
            RETURN

         ELSE
            NOSPEC = .FALSE.
            NORDER = NORDER + 1
            IORDER = NORDER
            ORDERS( IORDER ) = ORDER
         END IF
      END IF

*   Copy current order into store
      NWAVS( IORDER ) = NWAV
      WAV1S( IORDER ) = WAV( 1 )
      WAV2S( IORDER ) = WAV( NWAV )

      DO I = 1, NWAV
         WAVS( I, IORDER ) = WAV( I )
         SNETS( I, IORDER ) = SNET( I )
         CALL DQ_ITOU( QNET( I ), QNETS( I, IORDER ) )
      END DO

      SPCHAN = .TRUE.

      END
