      SUBROUTINE USR_PLNET( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PLNET

*  Description:
*      The graphics display is opened, and the current net spectrum
*      is plotted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PLNET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     09-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     07-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
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
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMNET'
      INCLUDE 'CMWAV'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMSPEC'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      REAL FSNET( 1200 )

      BYTE BORDER( MAXLABEL ) ! Order number/aperture name.

      LOGICAL QUAL       ! Whether data quality plotted.
      LOGICAL ZERO       ! Whether zero line is plotted.
      LOGICAL SELFLG     ! Whether SELINE is called during plotting.
      LOGICAL RELFLG     ! Whether RELINE is called during plotting.

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER I          ! Do loop index.
      INTEGER N          ! Number of plot data.
      INTEGER POS        ! Character position.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Access Spectrum.
      CALL RDSPEC( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access spectrum\\', STATUS )
         GO TO 999

      ELSE IF ( NONET ) THEN
         CALL ERROUT( 'Error: no net spectrum\\', STATUS )
         GO TO 999
      END IF

*   Automatic RESET sequence.
      ZCLEAR = .FALSE.
      CALL GRF_CLZONE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: clearing graphics specification\\',
     :                STATUS )
         GO TO 999
      END IF

*   Open graphics display.
      CALL GRF_OPZONE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT('Error: graphics unavailable\\', STATUS )
         GO TO 999
      END IF

*   Reset line style.
      CALL GRF_RSLINE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   Add the new data arrays into storage so that they can be used
*   to define display limits if needed (only use).
      NPOINT = 0

      N = MIN( ( MAXPOINT - NPOINT ), NWAV )
      IF ( N .LT. NWAV ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( WAVAIR( I ) )
         QPLOT( NPOINT + I ) = QNET( I )
         YPLOT( NPOINT + I ) = REAL( SNET( I ) )
      END DO

      NPOINT = NPOINT + N

*   HIST.
      CALL RDPARL( 'HIST\\', .FALSE., 1, HISTLN, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'HIST\\', STATUS )
         GO TO 999
      END IF

*   QUAL.
      CALL RDPARL( 'QUAL\\', .FALSE., 1, QUAL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'QUAL\\', STATUS )
         GO TO 999
      END IF

*   Design axes and annotation if undefined.
      IF ( .NOT. DRAWN ) THEN

*      XL - x-limits, UNDEF means use data defined limits.
         CALL XLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*      YL - y-limits, UNDEF means use data defined limits.
         CALL YLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*      Plant axis labels.
         CALL STR_MOVE( WLABEL, MAXLABEL, XLAB )
         CALL STR_MOVE( WUNITS, MAXLABEL, XUN )
         CALL STR_MOVE( NLABEL, MAXLABEL, YLAB )
         CALL STR_MOVE( NUNITS, MAXLABEL, YUN )
         CALL STR_MOVE( TITLE, MAXTITLE, GTITLE )
         IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
            CALL STR_WRITI( '%p (ORDER=%i)\\', ORDER, MAXLABEL, BORDER,
     :                      POS )
            CALL STR_APPND( BORDER, MAXTITLE, GTITLE )

         ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
            CALL STR_WRITS( '%p (APERTURE=%s)\\', APERS( 1, ORDER ),
     :                      MAXLABEL, BORDER, POS )
            CALL STR_APPND( BORDER, MAXTITLE, GTITLE )
         END IF

*      Determine axis limits one way or another.
         CALL GRF_DEAXES( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: defining axis limits\\', STATUS )
            GO TO 999
         END IF

*      Force zero level to be included (unless YL has been set).
         IF ( NOYL ) THEN
            IF ( YLIM( 1 ) .GT. 0.0 ) THEN
               YLIM( 1 ) = 0.0

            ELSE IF ( YLIM( 2 ) .LT. 0.0 ) THEN
               YLIM( 2 ) = 0.0
            END IF
         END IF
      END IF

*   Set remaining plotting flags.
      ZERO = .TRUE.
      SELFLG = .TRUE.
      RELFLG = .TRUE.

*   Copy of Spectrum for plot.
      DO I = 1, NWAV
        FWAVAIR( I ) = REAL( WAVAIR( I ) )
        FSNET( I ) = REAL( SNET( I ) )
      END DO

*   Plot net spectrum.
      IF ( HISTLN ) THEN
         CALL GRF_HICURV( 3, NWAV, FWAVAIR, FSNET, QNET, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )

      ELSE
         CALL GRF_POCURV( 3, NWAV, FWAVAIR, FSNET, QNET, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )
      END IF

 999  CONTINUE

      END
