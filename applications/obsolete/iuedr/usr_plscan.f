      SUBROUTINE USR_PLSCAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_PLSCAN

*  Purpose:
*     Plot the current scan spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_PLSCAN( STATUS )

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

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      INTEGER DQ_AND     ! Data quality AND.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'
      INCLUDE 'CMCOLR'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Local Variables :
      REAL X1( 4096 )      ! Temporary x-array.
      REAL FFSCAN( 4096 )  ! Temporary y-array.

      LOGICAL QUAL         ! Whether data quality plotted.
      LOGICAL ZERO         ! Whether zero line is plotted.
      LOGICAL SELFLG       ! Whether SELINE is called during plot.
      LOGICAL RELFLG       ! Whether RELINE is called during plot.

      CHARACTER*( 80 ) CTITLE ! Title temporary.

      INTEGER IQSCAN( 4096 ) ! Temporary quality array.
      INTEGER ACTVAL       ! Parameter value count.
      INTEGER I            ! Do loop index.
      INTEGER N            ! Number of plot data.
      INTEGER NCHAR        ! Filled length of title character string.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NOSCAN ) THEN
         CALL ERROUT( 'Error: no scan\\', STATUS )
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

*   Expand x-axis.
      DO I = 1, NSCAN
         X1( I ) = REAL( I - 1 ) * REAL( DUSCAN ) + REAL( U1SCAN )
      END DO

*   Add the new data arrays into storage so that they can be used
*   to define display limits if needed (only use).
      NPOINT = 0

      N = MIN( ( MAXPOINT - NPOINT ), NSCAN )
      IF ( N .LT. NSCAN ) THEN
         CALL ERROUT( 'Too many points for autoscale\\', STATUS )
         STATUS = SAI__OK
      END IF

      DO I = 1, N
         XPLOT( NPOINT + I ) = X1( I )
         QPLOT( NPOINT + I ) = QSCAN( I )
         YPLOT( NPOINT + I ) = REAL( FSCAN( I ) )
      END DO

      NPOINT = NPOINT + N

*   Legend request.
      NLEGND = 1

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

*      XL - x-limits, UNDEF means use data defined limits
         CALL XLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*      YL - y-limits, UNDEF means use data defined limits
         CALL YLGET( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*      Plant axis labels.
         CALL STR_MOVE( ULABEL, MAXLABEL, XLAB )
         CALL STR_MOVE( UUNITS, MAXLABEL, XUN )
         CALL STR_MOVE( SLABEL, MAXLABEL, YLAB )
         CALL STR_MOVE( SUNITS, MAXLABEL, YUN )
         CALL STR_MOVE( TITLE, MAXTITLE, GTITLE)

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

*      Extra legend line.
         CALL GEN_STOC( STITLE, 80, CTITLE, NCHAR )
      END IF

*   Set remaining plotting flags.
      ZERO = .TRUE.
      SELFLG = .TRUE.
      RELFLG = .TRUE.

*   Copy of Scan data for plot.
      DO I = 1, NSCAN
         FFSCAN( I ) = REAL( FSCAN( I ) )
         IQSCAN( I ) = QSCAN( I )
      END DO

*   Plot scan spectrum.
      IF ( HISTLN ) THEN
         CALL GRF_HICURV( 3, NSCAN, X1, FFSCAN, IQSCAN, CTITLE, NCHAR,
     :                    QUAL, ZERO, SELFLG, RELFLG )

      ELSE
         CALL GRF_POCURV( 3, NSCAN, X1, FFSCAN, IQSCAN, CTITLE, NCHAR,
     :                    QUAL, ZERO, SELFLG, RELFLG )
      END IF

 999  CONTINUE

      END
