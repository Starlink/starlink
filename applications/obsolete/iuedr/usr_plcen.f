      SUBROUTINE USR_PLCEN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PLCEN

*  Purpose:
*     The graphics display is opened, and the current centroid
*     spectrum is plotted.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PLCEN( STATUS )

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
*     05-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMCEN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'
      INCLUDE 'CMCOLR'
      INCLUDE 'CMSIST'
      INCLUDE 'CMSPEC'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS    ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      REAL*8 R1
      REAL*8 R2

      REAL   Y1( 1200 ) ! Centroids as used.
      REAL   FSCEN( 1200 )

      BYTE BORDER( MAXLABEL ) ! Order number/aperture name.

      LOGICAL QUAL      ! Whether data quality flags are plotted.
      LOGICAL ZERO      ! Whether zero line is plotted.
      LOGICAL SELFLG    ! Whether SELINE is called during plotting.
      LOGICAL RELFLG    ! Whether RELINE is called during plotting.

      INTEGER Q1( 1200 ) ! DQ for Y1.
      INTEGER I         ! Do loop index.
      INTEGER N         ! Number of plot data.
      INTEGER POS       ! Character position.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Access Spectrum.
      CALL RDSPEC( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access spectrum\\', STATUS )
         GO TO 999

      ELSE IF ( NOCEN .OR. NOEXTP ) THEN
         CALL ERROUT( 'Error: no centroid spectrum\\', STATUS )
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

*   Centroids as used.
      N = MIN( ( MAXPOINT - NPOINT ), NWAV )

      IF ( N .LT. NWAV ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( WAVAIR( I ) )
         IF ( CENSH ) THEN
            Y1( I ) = REAL( SCEN( I ) + AGCEN )

         ELSE
            Y1( I ) = REAL( SCEN( I ) + DCEN( I ) )
         END IF

         Q1( I ) = 0
         QPLOT( NPOINT + I ) = Q1( I )
         YPLOT( NPOINT + I ) = Y1( I )
      END DO

      NPOINT = NPOINT + N

*   Centroids as determined.
      N = MIN( ( MAXPOINT - NPOINT ), NWAV )
      IF ( N .LT. NWAV ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = REAL( WAVAIR( I ) )
         QPLOT( NPOINT + I ) = QCEN( I )
         YPLOT( NPOINT + I ) = REAL( SCEN( I ) )
      END DO

      NPOINT = NPOINT + N

*   Design axes and annotation if undefined.
      IF ( .NOT. DRAWN ) THEN

*      XL - x-limits, UNDEF means use data defined limits.
         CALL XLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*      YL - y-limits, UNDEF means use data defined limits.
         CALL YLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*      Plant axis labels.
         CALL STR_MOVE( WLABEL, MAXLABEL, XLAB )
         CALL STR_MOVE( WUNITS, MAXLABEL, XUN )
         CALL STR_MOVE( 'Centroid\\', MAXLABEL, YLAB )
         CALL STR_MOVE( '(pixels)\\', MAXLABEL, YUN )
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

*      Force zero level to be included.
         IF ( NOYL ) THEN
            R1 = -ABS( ROBJ( 2 ) - ROBJ( 1 ) ) / 2.0
            R2 = ABS( ROBJ( 2 ) - ROBJ( 1 ) ) / 2.0
            IF ( YLIM( 1 ) .GT. R1 ) YLIM( 1 ) = R1
            IF ( YLIM( 2 ) .LT. R2 ) YLIM( 2 ) = R2
         END IF
      END IF

*   Set plotting flags.
      QUAL = .FALSE.
      ZERO = .FALSE.
      SELFLG = .TRUE.
      RELFLG = .FALSE.

*   Centroids as used.
      DO I = 1, NWAV
         FWAVAIR( I ) = REAL( WAVAIR( I ) )
         FSCEN( I ) = REAL( SCEN( I ) )
      END DO

      CALL GRF_POCURV( 3, NWAV, FWAVAIR, Y1, Q1, CHAR( 0 ), 0, QUAL,
     :                 ZERO, SELFLG, RELFLG )
      ZERO = .TRUE.
      SELFLG = .FALSE.
      RELFLG = .TRUE.

*   Centroids as indicated by data.
      CALL GRF_POCURV( 3, NWAV, FWAVAIR, FSCEN, QCEN, CHAR( 0 ), 0,
     :                 QUAL, ZERO, SELFLG, RELFLG )

 999  CONTINUE

      END
