      SUBROUTINE PICKLO( STATUS )
*+
*  Name:
*     SUBROUTINE PICKLO

*  Purpose:
*     Use cursor to pick spectrum and get (DX,DY) from LORES scan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PICKLO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The cursor is read on the current DIAGRAM which is assumed to
*     have U as the horizontal axis.
*     The current dispersion relations are used to
*     and generate a shift (DX,DY) assuming that the cursor is set on
*     the spectrum peak.
*     The SCAN is left undefined if DISPSG,DISPLG have been undated since
*     the old dispersion constants are locked up in the scan, and the
*     previous values of DISPSG,DISPLG would be hidden.
*     This assumes that the GRID consists a single scan at V=V1.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     15-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*     17-FEB-95 (MJC):
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
      INCLUDE 'CMGRAF'
      INCLUDE 'CMSCAN'
      INCLUDE 'CMDISP'

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      LOGICAL STR_SIMLR  ! Caseless string equality.

*  Local Variables:
      REAL*8 DX          ! X-shift.
      REAL*8 DY          ! Y-shift.
      REAL*8 GX          ! Global x-shift.
      REAL*8 GY          ! Global y-shift.
      REAL*8 DR
      REAL*8 W           ! W-value.
      REAL*8 XNEW
      REAL*8 XOLD
      REAL*8 YNEW
      REAL*8 YOLD

      REAL   F           ! Y-axis cursor hit value.
      REAL   R           ! R-value.

      LOGICAL NOGXY      ! Whether GX,GY are valid.

      INTEGER CURMOD     ! Cursor mode (0 = GKS default).
      INTEGER I          ! Cursor hit index.
      INTEGER IAPER      ! Aperture index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is a scan.
      IF ( NOSCAN ) THEN
         CALL ERROUT( 'Error: no scan\\', STATUS )
         GO TO 999
      END IF

*  Open zone.
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\',
     :                STATUS )
         GO TO 999
      END IF

*  Aperture/resolution.
      CALL DEFAPR( 1, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
         GO TO 999
      END IF

*  Define template.
      CALL LOTEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         GO TO 999
      END IF

*  Check that the axis label makes sense.
      IF ( .NOT. STR_SIMLR( 'R\\', XLAB ) ) THEN
         CALL ERROUT( 'Error: wrong kind of scan\\', STATUS )
         GO TO 999
      END IF

*  Cursor loop.
      NOGXY = .TRUE.
      CURMOD = 0

 100  CONTINUE
      CALL GRF_CUZONE( '12', CURMOD, I, R, F, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading cursor\\', STATUS )
         GO TO 200

      ELSE IF ( I .LE. 0 ) THEN
         GO TO 200
      END IF

      W = V1SCAN
      DR = DBLE( R )
      CALL LINE_WRITF( '%p (R, W) = (%.2f,\\', DR )
      CALL LINE_WRITF( '%.2f)\\', W )
      CALL PRTBUF( STATUS )

*  Find shift in geometric coordinates.
      CALL WTOG( 0.0d0, W, XOLD, YOLD )
      CALL WTOG( DBLE( R ), W, XNEW, YNEW )
      DX = XNEW - XOLD
      DY = YNEW - YOLD
      CALL LINE_WRITF( '%p Relative geometric shift (%.2f,\\', DX )
      CALL LINE_WRITF( '%.2f)\\', DY )
      CALL PRTBUF( STATUS )

*  Write GSHIFT.
      GX = DX + DISPSG( IAPER )
      GY = DY + DISPLG( IAPER )
      CALL LINE_WRITF( '%p Absolute geometric shift (%.2f,\\', GX )
      CALL LINE_WRITF( '%.2f)\\', GY )
      CALL PRTBUF( STATUS )
      NOGXY = .FALSE.
      GO TO 100
 200  CONTINUE

*  Use last (GX,GY) for DISPSG,DISPLG
      IF ( STATUS.EQ.SAI__OK .AND. .NOT.NOGXY ) THEN
         DISPSG( IAPER ) = GX
         DISPLG( IAPER ) = GY
         CALL LINE_WCONT(
     :          '%p Last shift retained, scan left undefined.\\' )
         CALL PRTBUF( STATUS )
         CALL MODCAL
         NOSCAN = .TRUE.
      END IF

 999  CONTINUE

      END
