      SUBROUTINE PICKHI( STATUS )
*+
*  Name:
*     SUBROUTINE PICKHI

*  Description:
*     Use cursor to pick order and get (DX,DY) from HIRES scan.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PICKHI( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The cursor is read on the current DIAGRAM which is assumed to
*     have U as the horizontal axis.
*     The current dispersion relations are used to pick the nearest order
*     and generate a shift (DX,DY) assuming that the cursor is set on
*     the order peak.
*     This assumes that GRID consists a single scan at V=V1.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     09-AUG-94 (MJC):
*       IUEDR Vn. 3.1-2
*     09-JAN-95 (MJC):
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
      INCLUDE 'CMHEAD'

*  Status:
      INTEGER STATUS   ! Global status.

*  External References:
      LOGICAL STR_SIMLR ! Caseless string equality.

*  Local Variables:
      REAL*8 DX        ! X-shift.
      REAL*8 DY        ! Y-shift.
      REAL*8 GX        ! GX-shift.
      REAL*8 GY        ! GY-shift.
      REAL*8 F         ! Y-axis cursor hit value.
      REAL*8 L         ! L-value.
      REAL*8 R         ! R-value.
      REAL*8 S         ! S-value.
      REAL*8 RMIN      ! Distance of point to nearest order.
      REAL*8 DW
      REAL*8 XNEW
      REAL*8 XOLD
      REAL*8 YNEW
      REAL*8 YOLD

      REAL   U         ! U-value.
      REAL   RF

      LOGICAL NOGXY    ! Whether GX, GY defined.

      INTEGER ACTVAL   ! Parameter value count.
      INTEGER CURMOD   ! Cursor mode.
      INTEGER I        ! Cursor hit index.
      INTEGER IAPER    ! Aperture index.
      INTEGER ORD      ! Order index.
      INTEGER ORDMIN   ! Nearest M-value to (S, L).
      INTEGER ORDERS( 2 ) ! Range of orders.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is a scan.
      IF ( NOSCAN ) THEN
         CALL ERROUT( 'Error: no scan\\', STATUS )
         GO TO 999
      END IF

*  Open graphics zone.
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )
         GO TO 999
      END IF

*  Perform checks, default assignments and prompts.
      CALL DEFAPR( 2, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\', STATUS )
         GO TO 999
      END IF

*  Define template.
      CALL HITEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         GO TO 999
      END IF

*  ORDERS.
      CALL GET_ORDERS( .FALSE., ORDERS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Check that the axis label makes sense.
      IF ( .NOT.STR_SIMLR( 'U\\', XLAB ) ) THEN
         CALL ERROUT( 'Error: wrong kind of scan\\', STATUS )
         GO TO 999
      END IF

*  Cursor loop.
      NOGXY = .TRUE.
      CURMOD = 0

      DO WHILE ( .TRUE. )
         CALL GRF_CUZONE( '12', CURMOD, I, U, RF, STATUS )
         F = DBLE( RF )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )
            GO TO 400

         ELSE IF ( I .LE. 0 ) THEN
            GO TO 400
         END IF

*     (U,V) to (S,L)
         CALL UTOR( DBLE( U ), V1SCAN, S, L )

*     Find nearest order (within reason)
         RMIN = 1.E6
         ORDMIN = ORDERS( 1 )
         DO ORD = ORDERS( 1 ), ORDERS( 2 )
            CALL ORSET( ORD )
            CALL RTOW( S, L, R, DW )
            IF ( ABS( R ) .LT. ABS( RMIN ) ) THEN
               ORDMIN = ORD
               RMIN = R
            END IF
         END DO

         CALL ORSET( ORDMIN )

*     Print (M,R,W) for pick position
         CALL RTOW( S, L, R, DW )
         CALL LINE_WRITI( '%p (nearest_order, R, W) = (%i, \\', ORDMIN )
         CALL LINE_WRITF( '%.2g, \\', R )
         CALL LINE_WRITF( '%.3f)\\', DW )
         CALL PRTBUF( STATUS )

*     Find shift in geometric coordinates
         CALL WTOG( 0.0d0, DW, XOLD, YOLD )
         CALL WTOG( R, DW, XNEW, YNEW )
         DX = XNEW - XOLD
         DY = YNEW - YOLD
         CALL LINE_WRITF( '%p Relative geometric shift (%.2f,\\', DX )
         CALL LINE_WRITF( '%.2f)\\', DY )
         CALL PRTBUF( STATUS )

*     Write GSHIFT
         GX = DX + DISPSG( IAPER )
         GY = DY + DISPLG( IAPER )
         CALL LINE_WRITF( '%p Absolute geometric shift (%.2f,\\', GX )
         CALL LINE_WRITF( '%.2f)\\', GY )
         CALL PRTBUF( STATUS )
         NOGXY = .FALSE.
      END DO
 400  CONTINUE

*  Use last (GX,GY) for DISPSG,DISPLG.
      IF ( STATUS.EQ.SAI__OK .AND. .NOT.NOGXY ) THEN
         DISPSG( IAPER ) = GX
         DISPLG( IAPER ) = GY
         CALL LINE_WCONT( '%p Last Shift Retained.\\' )
         CALL PRTBUF( STATUS )
         CALL MODCAL
      END IF

 999  CONTINUE

      END
