      SUBROUTINE USR_PLGRS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PLGRS

*  Description:
*     The GROSS and BKG is displayed.  The Mean Background is scaled by
*     the assigned Object Slit Width (ROBJ) and added to NET to produce
*     GROSS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PLGRS( STATUS )

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
*     06-OCT-94 (MJC):
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
      INCLUDE 'CMBKG'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMSPEC'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER (MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80)

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      INTEGER DQ_AND     ! Data quality AND.
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      REAL SBKC( 1200 )  ! Scaled mean background.
      REAL SGRS( 1200 )  ! Pseudo Gross spectrum.
      REAL SCALE         ! Mean background scaling factor.

      BYTE BORDER( MAXLABEL ) ! Order number/aperture name.

      LOGICAL QUAL       ! Whether data quality plotted.
      LOGICAL ZERO       ! Whether zero line is plotted.
      LOGICAL SELFLG     ! Whether SELINE is called durint plotting.
      LOGICAL RELFLG     ! Whether RELINE is called during plotting.

      INTEGER QGRS( 1200 ) ! Gross data quality.
      INTEGER QBKC( 1200 ) ! Mean background data quality.
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

      ELSE IF ( NONET .OR. NOBKG .OR. NOEXTP ) THEN
         CALL ERROUT( 'Error: cannot construct gross spectrum\\',
     :                STATUS )
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
         CALL ERROUT( 'Error: graphics unavailable\\', STATUS )
         GO TO 999
      END IF

*   Reset line style.
      CALL GRF_RSLINE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*   Initialise data store.
      NPOINT = 0

*   Construct gross and smooth background.
      SCALE = REAL( ABS( ROBJ( 2 ) - ROBJ( 1 ) ) )

      DO I = 1, NWAV
         QBKC( I ) = QBKG( I )
         QGRS( I ) = QNET( I )

         IF ( DQ_AND( QBKG( I ), 1 ) .EQ. 0 ) THEN
            SBKC( I ) = SBKG( I ) * SCALE
            IF ( DQ_AND( QNET( I ), 1 ) .EQ. 0 ) THEN
               SGRS( I ) = REAL( SNET( I ) ) + SBKC( I )

            ELSE
               QGRS( I ) = 1
            END IF
         END IF
      END DO

*   Gross.
      N = MIN( ( MAXPOINT - NPOINT ), NWAV )
      IF ( N .LT. NWAV ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( WAVAIR( I ) )
         QPLOT( NPOINT + I ) = QGRS( I )
         YPLOT( NPOINT + I ) = SGRS( I )
      END DO

      NPOINT = NPOINT + N

*   Smooth background.
      N = MIN( ( MAXPOINT - NPOINT ), NWAV )
      IF ( N .LT. NWAV ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( WAVAIR( I ) )
         QPLOT( NPOINT + I ) = QBKC( I )
         YPLOT( NPOINT + I ) = SBKC( I )
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
         CALL STR_MOVE( 'Gross\\', MAXLABEL, YLAB )
         CALL STR_MOVE( '(FN)\\', MAXLABEL, YUN )
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

*      Force zero level to be included (unless YL specified).
         IF ( NOYL ) THEN
            IF ( YLIM( 1 ) .GT. 0.0 ) THEN
               YLIM( 1 ) = 0.0

            ELSE IF ( YLIM( 2 ) .LT. 0.0 ) THEN
               YLIM( 2 ) = 0.0
            END IF
         END IF
      END IF

*   Set remaining plotting flags.
      ZERO = .FALSE.
      SELFLG = .TRUE.
      RELFLG = .FALSE.

      DO I = 1, NWAV
         FWAVAIR( I ) = REAL( WAVAIR( I ) )
      END DO

*   Plot gross spectrum.
      IF ( HISTLN ) THEN
         CALL GRF_HICURV( 3, NWAV, FWAVAIR, SGRS, QGRS, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )

      ELSE
         CALL GRF_POCURV( 3, NWAV, FWAVAIR, SGRS, QGRS, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )
      END IF

      QUAL = .FALSE.
      ZERO = .TRUE.
      SELFLG = .FALSE.
      RELFLG = .TRUE.

*   Plot smooth background.
      IF ( HISTLN ) THEN
         CALL GRF_HICURV( 3, NWAV, FWAVAIR, SBKC, QBKC, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )

      ELSE
         CALL GRF_POCURV( 3, NWAV, FWAVAIR, SBKC, QBKC, CHAR( 0 ), 0,
     :                    QUAL, ZERO, SELFLG, RELFLG )
      END IF

 999  CONTINUE

      END
