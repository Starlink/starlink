      SUBROUTINE PREXT( STATUS )
*+
*  Name:
*     SUBROUTINE PREXT

*  Purpose:
*     Print extraction parameters for a particular order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The extraction parameters specific to the extraction of a
*     particular order are printed.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS        ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMWAV'
      INCLUDE 'CMDISH'

*  Local Variables:
      INTEGER IBKG          ! loop index
      INTEGER M             ! order

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extraction slits.
      CALL LINE_WCONT( '%p Channels:\\' )
      CALL LINE_WRITF( '  Object (%.1f,\\', ROBJ( 1 ) )
      CALL LINE_WRITF( '%.1f),\\', ROBJ( 2 ) )
      CALL LINE_WCONT( '  Backgrounds \\' )

      DO IBKG = 1, NBKG
         CALL LINE_WRITF( '(%.1f,\\', RBKG( 1, IBKG ) )
         CALL LINE_WRITF( '%.1f)\\', RBKG( 2, IBKG ) )
         IF ( IBKG .EQ. NBKG ) THEN
            CALL LINE_WCONT( '.\\' )

         ELSE
            CALL LINE_WCONT( ',\\' )
         END IF
      END DO
      CALL PRTBUF( STATUS )

*  Wavelength grid.
      CALL LINE_WRITF( '%p Wavelength grid (%.3f,\\', WAV1 )
      CALL LINE_WRITF( '%.3f,\\', REAL( NWAV - 1 ) * DWAV + WAV1 )
      CALL LINE_WRITF( '%.3f)\\', DWAV )

      IF ( CUTWV ) THEN
         CALL FNCUT( CORD, M )

      ELSE
         M = 0
      END IF

      IF ( M .LE. 0 ) THEN
         CALL LINE_WCONT( ' based on faceplate.\\' )

      ELSE
         CALL LINE_WCONT( ' based on cutoff limits.\\' )
      END IF

      CALL PRTBUF( STATUS )

*  Template source
      IF ( CENTM ) THEN
         CALL FNTEM( CORD, M )

      ELSE
         M = 0
      END IF

      IF ( M .GT. 0 ) THEN
         CALL LINE_WCONT(
     :        '%p Initial template based on pre-existing shape.\\'
     :               )

      ELSE
         CALL LINE_WCONT(
     :      '%p Initial template based on dispersion constants.\\'
     :             )

      END IF

      END
