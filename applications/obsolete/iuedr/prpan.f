      SUBROUTINE PRPAN( STATUS )

*+
*  Name:
*     SUBROUTINE PRPAN

*  Purpose:
*     Print LBLS extraction parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRPAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     This does NOT check NOEXTP, since it only prints the subset
*     of parameters that are independent of specific APERTURE/ORDER.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          04-NOV-88     IUEDR Vn. 2.0
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

*  Local Variables:
      REAL*8 CORD           ! order central wavelength

      INTEGER M

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   R-grid.
      CALL LINE_WCONT( '%p Radial grid \\' )
      CALL LINE_WRITF( '(%.3f\\', RL( 1 ) )
      CALL LINE_WRITF( ',%.3f\\', RL( 2 ) )
      CALL LINE_WRITF( ',%.3f)\\', RSAMP )
      IF ( AUSLIT ) THEN
         CALL LINE_WCONT( ' based on context.\\' )

      ELSE
         CALL LINE_WCONT( ' based on parameters.\\' )
      END IF
      CALL PRTBUF( STATUS )

*   Sample rate.
      CALL LINE_WRITF( '%p Wavelength sample width %.2f pixels\\',
     :                 GSAMP )
      CALL LINE_WRITF( ' (%.2f that of IUESIPS#1).\\', GSAMP / 1.414 )
      CALL PRTBUF( STATUS )

*   Wavelength grid.
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

*   Template source.
      IF ( CENTM ) THEN
         CALL FNTEM( CORD, M )

      ELSE
         M = 0
      END IF

      IF ( M .GT. 0 ) THEN
         CALL LINE_WCONT( '%p Template based on pre-existing shape.\\' )

      ELSE
         CALL LINE_WCONT( '%p Template based on dispersion constants.\\'
     :                    )
      END IF

      END
