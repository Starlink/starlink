      SUBROUTINE PRRIP( STATUS )
*+
*  Name:
*     SUBROUTINE PRRIP

*  Purpose:
*     Print ripple calibration.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRRIP( STATUS )

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
*     Paul Rees          05-OCT-88     IUEDR Vn. 2.0
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

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMRIP'
      INCLUDE 'CMCUT'

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Local Variables:
      INTEGER I             ! loop index
      INTEGER IORD          ! loop index
      INTEGER K             ! accumulator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Avoid non-HIRES and when undefined.
      IF ( .NOT.STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         GO TO 999

      ELSE IF ( NORIP ) THEN
         GO TO 999
      END IF

*  Banner
      CALL LINE_WCONT( '%p%2w Ripple Calibration:\\' )
      CALL PRTBUF( STATUS )

*  Global properties.
      CALL LINE_WRITS( '%p%4w Type ''%s'',\\', RIPTP )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITS( '%p%4w Source ''%s''.\\', RIPID )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITF( '%p%4w Global K(M)=(%g\\', RIPM( 1 ) )

      IF ( NRIPM .GT. 1 ) THEN
         DO I = 2, NRIPM
            CALL LINE_WRITF( ',%g\\', RIPM( I ) )
         END DO
      END IF

      CALL LINE_WCONT( ').\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITF( '%p%4w Global X-scaling %.3f,\\', RIPALF )
      CALL LINE_WRITF( ' X-limits (%.3f,\\', XRLIM( 1 ) )
      CALL LINE_WRITF( '%.3f).\\', XRLIM( 2 ) )
      CALL PRTBUF( STATUS )

*  Peculiar ripple parameters.
      IF ( NRIPO .GT. 0 ) THEN
         CALL LINE_WCONT( '%p%4w Individual Ripple Calibrations:\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%8w M%9wK%4wAlpha%2w Polynomial\\' )
         CALL PRTBUF( STATUS )
         DO IORD = 1, NRIPO
            IF ( RIPOS( IORD ) .GT. 0 ) THEN
               CALL LINE_WRITI( '%p%6w %3i\\', RIPOS( IORD ) )
               CALL LINE_WRITF( '%2w%8.1f\\', RIPKS( IORD ) )
               CALL LINE_WRITF( '%2w%7.3f\\', RIPAS( IORD ) )
               K = 1
               DO I = 1, NRIPCS( IORD )
                  CALL LINE_WRITF( '%2w%12.5e\\', RIPCS( I, IORD ) )
                  IF ( K.EQ.3 .AND. I.LT.NRIPCS( IORD ) ) THEN
                     CALL PRTBUF( STATUS )
                     CALL LINE_WCONT( '%p%27w \\' )
                     K = 0
                  END IF
                  K = K + 1
               END DO
               CALL PRTBUF( STATUS )
            END IF
         END DO

         CALL LINE_WCONT( '%p%4w end.\\' )
         CALL PRTBUF( STATUS )
      END IF

*  End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

*  Wavelength cut-off data.
      IF ( .NOT.NOCUT .AND. NCUT.GT.0 ) THEN
         CALL LINE_WCONT( '%p%2w Individual Wavelength Limits:\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p \\' )
         DO I = 1, 3
            CALL LINE_WCONT( '%6wM%4wMIN%4wMAX\\' )
         END DO
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p \\' )
         K = 1
         DO I = 1, NCUT
            IF ( CUTORD( I ) .GT. 0 ) THEN
               CALL LINE_WRITI( '%4w%3i\\', CUTORD( I ) )
               CALL LINE_WRITF( '%1w%6.1f\\', CUTW1( I ) )
               CALL LINE_WRITF( '%1w%6.1f\\', CUTW2( I ) )
               IF ( K.EQ.3 .AND. I.LT.NCUT ) THEN
                  CALL PRTBUF( STATUS )
                  CALL LINE_WCONT( '%p \\' )
                  K = 0
               END IF
               K = K + 1
            END IF
         END DO
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%2w end.\\' )
         CALL PRTBUF( STATUS )
         CALL PRTEOL( STATUS )
      END IF

 999  CONTINUE

      END
