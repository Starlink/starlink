      SUBROUTINE USR_AGSHIFT( STATUS )
*+
*  Name:
*     SUBROUTINE USR_AGSHIFT

*  Purpose:
*     Automated determination of the GSHIFT parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_AGSHIFT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Estimate GSHIFT for each order found in a HIRES image.
*     Report estimated mean GSHIFT value, number of orders found,
*     number of orders not found and spread of values estimated.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     22-AUG-94 (MJC):
*       IUEDR Vn. 3.1-3
*       Template version with "not implemented" message.
*     09-JAN-95 (MJC):
*       Standard preamble added, body of task added, does not set GSHIFT.
*     10-JAN-95 (MJC):
*       Centroid to locate orders, weighted estimate at GSHIFT,
*       GSHIFT is now set.
*     14-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       Use GET_ORDERS subroutine and GS* arrays properly limited.
*     07-APR-95 (MJC):
*       Added divide by zero checks.
*     07-NOV-95 (MJC):
*       Support for LORES data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMDISP'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSCAN'

*  Local Constants:
      REAL*8 ORDW          ! Closely spaced order extra weighting.
      PARAMETER ( ORDW = 2.0 )

      INTEGER HIORD        ! Highest acceptable order number.
      INTEGER LOORD        ! Lowest acceptable order number.
      PARAMETER ( HIORD = 125, LOORD = 65 )

*  Status:
      INTEGER STATUS       ! Global status.

*  External References:
      LOGICAL STR_SIMLR    ! Caseless string equality.

*  Local Variables:
      REAL*8 CENT          ! Order centroid.
      REAL*8 DW            ! Order coordinate, dummy wavelength.
      REAL*8 DX            ! GSHIFT in X for one Order.
      REAL*8 DY            ! GSHIFT in Y for one Order.
      REAL*8 GSA( LOORD : HIORD ) ! Estimated GSHIFTs for each order in X.
      REAL*8 GSB( LOORD : HIORD ) ! Estimated GSHIFTs for each order in Y.
      REAL*8 GX            ! GX-shift.
      REAL*8 GY            ! GY-shift.
      REAL*8 L             ! Image coordinate, line.
      REAL*8 LASTR         ! Last R value in distance-to-order.
      REAL*8 LOP           ! Last Peak value.
      REAL*8 MAXSCAN       ! Peak value in scan.
      REAL*8 MEANX         ! Mean GSHIFT in X.
      REAL*8 MEANY         ! Mean GSHIFT in Y.
      REAL*8 NORD          ! Weighted order count.
      REAL*8 OP            ! Flux value at a peak.
      REAL*8 OSHIFT        ! Shift in pixels for a particular order.
      REAL*8 PU            ! U-value at a peak.
      REAL*8 R             ! Order coordinate, radial distance.
      REAL*8 RMIN          ! Distance of point to nearest order.
      REAL*8 S             ! Image coordinate, sample.
      REAL*8 SIGFLUX       ! Flux sum during centroid.
      REAL*8 THISU         ! Looping U value.
      REAL*8 XNEW          ! New X estimate for Order.
      REAL*8 XOLD          ! Previous X estimate for Order.
      REAL*8 YNEW          ! New Y estimate for Order.
      REAL*8 YOLD          ! Previous Y estimate for Order.

      INTEGER ACTVAL       ! Number of parameter values "got".
      INTEGER I            ! Loop index.
      INTEGER IAPER        ! Aperture index.
      INTEGER IP           ! Index for Order Peak.
      INTEGER J            ! Second loop index.
      INTEGER LO           ! Last Order processed.
      INTEGER NCHAR        ! Character string length.
      INTEGER ORD          ! Loop index.
      INTEGER ORDERS( 2 )  ! Range of orders.
      INTEGER ORDMIN       ! Number of nearest order.

      CHARACTER*( 80 ) AMESSAGE ! Buffer for messages.

      BYTE ASTRING( 80 )   ! BYTE copy of buffer for messages.

      LOGICAL GSQ( LOORD : HIORD ) ! GSHIFT quality.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  HIRES data can be handled.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN

*     Check that there is a scan.
         IF ( NOSCAN ) THEN
            CALL ERROUT( 'Error: no scan\\', STATUS )
            GO TO 999
         END IF

*     Perform checks, default assignments and prompts.
         CALL DEFAPR( 2, IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Define template.
         CALL HITEM( IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Obtain ORDERS parameter value.
         CALL GET_ORDERS( .FALSE., ORDERS, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*     Step along the scan and find the nearest order for each U value.
*     Step backwards as orders are more widely spaced at the top end
*     of the scan array.
         LO = ORDERS( 1 )
         LOP = 0.0
         OP = 0.0
         DO I = NSCAN, 1, -1
            IF ( I .LE. 0 ) THEN
               GO TO 400
            END IF
            THISU = U1SCAN + DBLE( I - 1 ) * DUSCAN

*        Convert (U,V) coordinates to (S,L) coordinates.
            CALL UTOR( DBLE( THISU ), V1SCAN, S, L )

*        Find nearest order.
            RMIN = 1.E6
            LASTR = 1.E6
            ORDMIN = LO
            DO ORD = LO, 125
               CALL ORSET( ORD )
               CALL RTOW( S, L, R, DW )
               IF ( ABS( R ) .LT. ABS( RMIN ) ) THEN
                  ORDMIN = ORD
                  RMIN = R
               END IF

*        Check for having passed the closest order.
               IF ( ABS( R ) .GT. ABS( LASTR ) ) THEN
                  GO TO 200
               END IF
               LASTR = R
            END DO
 200        CONTINUE

*        Wait until first order is reached.
            IF ( ORDMIN .LT. ORDERS( 1 ) ) THEN
               GO TO 300
            END IF

*        Stop if last order is reached.
            IF ( ORDMIN .GT. ( ORDERS( 2 ) + 1 ) .OR.
     :           ORDMIN .GT. 120 ) THEN
               GO TO 400
            END IF

*        Guess the shift for the peak value for this order.
            IF ( LO .NE. ORDMIN ) THEN

*           If the peak is right on the top-edge of a bin look nearby for
*           a higher value.
*              IF ( IP .EQ. ( I + 1 ) ) THEN
*                 IF ( FSCAN( I ) .GT. FSCAN ( IP ) ) THEN
*                    IP = I
*                    OP = FSCAN( IP )
*                    IF ( FSCAN( I - 1 ) .GT. FSCAN ( IP ) ) THEN
*                       IP = I - 1
*                       I = I - 1
*                       OP = FSCAN( IP )
*                    END IF
*                 END IF
*              END IF

*           Display peak information.
               PU = U1SCAN + DBLE( IP - 1 ) * DUSCAN

               CALL MSG_FMTC( 'OP', 'A', 'Order' )
               CALL MSG_FMTI( 'OP', 'I4', LO )
               CALL MSG_FMTC( 'OP', 'A', '. Peak U: ' )
               CALL MSG_FMTD( 'OP', 'F8.3', PU )
               CALL MSG_FMTC( 'OP', 'A', ', Peak FN/Pixel: ' )
               CALL MSG_FMTD( 'OP', 'F9.3', OP )
               CALL MSG_LOAD( ' ', ' ^OP', AMESSAGE, NCHAR, STATUS )
               CALL GEN_CTOS( AMESSAGE, 132, ASTRING, NCHAR )
               CALL LINE_WRITS( '%p%s\\', ASTRING )
               CALL PRTBUF( STATUS )

*           Calculate centroid position of order based on peak.
               CENT = 0.0
               SIGFLUX = 0.0
               DO J = IP - 2, IP + 2
                  PU = U1SCAN + DBLE( J - 1 ) * DUSCAN
                  CENT = CENT + FSCAN ( J ) * PU
                  SIGFLUX = SIGFLUX + FSCAN ( J )
               END DO
               IF ( SIGFLUX .EQ. 0.0 ) THEN
                  CALL LINE_WCONT( '%p Possible divide zero avoided.\\',
     :                             STATUS )
                  CALL PRTBUF( STATUS )

               ELSE
                  CENT = CENT / SIGFLUX
               END IF

*           Calculate shift for centroid.
               CALL UTOR( CENT, V1SCAN, S, L )
               CALL ORSET( LO )
               CALL RTOW( S, L, R, DW )
               CALL WTOG( 0.0d0, DW, XOLD, YOLD )
               CALL WTOG( R, DW, XNEW, YNEW )
               DX = XNEW - XOLD
               DY = YNEW - YOLD

*           Display the shift for this centroid.
               CALL MSG_FMTC( 'OP', 'A',
     :                        '          relative geometric shift (' )
               CALL MSG_FMTD( 'OP', 'F6.3', DX )
               CALL MSG_FMTC( 'OP', 'A', ',' )
               CALL MSG_FMTD( 'OP', 'F6.3', DY )
               CALL MSG_FMTC( 'OP', 'A', ').' )
               CALL MSG_LOAD( ' ', ' ^OP', AMESSAGE, NCHAR, STATUS )
               CALL GEN_CTOS( AMESSAGE, 132, ASTRING, NCHAR )
               CALL LINE_WRITS( '%p%s\\', ASTRING )
               CALL PRTBUF( STATUS )

*           Save the guess to shift array.
               GSA( LO ) = DX
               GSB( LO ) = DY

*           Reset peak data.
               LO = ORDMIN
               OP = FSCAN ( I )

            ELSE IF ( OP .LT. FSCAN ( I ) ) THEN

*           Wait until an inter-order trough is crossed before looking
*           for the next peak.
               IF ( FSCAN ( I ) .LT. LOP ) THEN
                  CONTINUE

               ELSE
                  IP = I
                  OP = FSCAN( IP )
               ENDIF
            END IF

*        Save current Flux value for trough detection.
            LOP = FSCAN ( I )

 300        CONTINUE
         END DO
 400     CONTINUE

*     All orders accepted in first pass.
         DO I = LOORD, HIORD
            GSQ( I ) = .TRUE.
         END DO

*     Calculate Mean GSHIFT, give extra weight to closely spaced orders
*     100-110.
         MEANX = 0.0
         MEANY = 0.0
         NORD = 0.0
         IP = MIN( ORDERS( 2 ) , 120 )
         DO I = ORDERS( 1 ), IP
            IF ( GSQ( I ) ) THEN
               MEANX = MEANX + GSA( I )
               MEANY = MEANY + GSB( I )
               NORD = NORD + 1.0
               IF ( I.GE.100 .AND. I.LE.110 ) THEN
                  MEANX = MEANX + ORDW * GSA( I )
                  MEANY = MEANY + ORDW * GSB( I )
                  NORD = NORD + ORDW
               END IF
            END IF
         END DO

*     Mean shifts.
         IF ( NORD .EQ. 0 ) THEN
            CALL LINE_WCONT( '%p Possible divide zero avoided.\\',
     :                       STATUS )
            CALL PRTBUF( STATUS )

         ELSE
            MEANX = MEANX / NORD
            MEANY = MEANY / NORD
         END IF

*     Display result.
         CALL LINE_WRITF( '%p Mean relative geometric shift [%.3f,\\',
     :                    MEANX )
         CALL LINE_WRITF( '%.3f] \\', MEANY )
         CALL LINE_WRITI( 'for orders (%i,\\', ORDERS( 1 ) )
         CALL LINE_WRITI( '%i).\\', IP )
         CALL PRTBUF( STATUS )

*     Display GSHIFT.
         GX = MEANX + DISPSG( IAPER )
         GY = MEANY + DISPLG( IAPER )
         CALL LINE_WRITF( '%p Mean absolute geometric shift [%.3f,\\',
     :                    GX )
         CALL LINE_WRITF( '%.3f]\\', GY )
         CALL PRTBUF( STATUS )

*     Reject shifts 3.0 or more pixels from mean.
         DO I = ORDERS( 1 ), IP
            OSHIFT = ( GSA( I ) - MEANX ) * ( GSA( I ) - MEANX ) +
     :               ( GSB( I ) - MEANY ) * ( GSB( I ) - MEANY )
            IF ( OSHIFT .GE. 9.0 ) THEN
               GSQ( I ) = .FALSE.
            END IF
         END DO

*     Recalculate mean, rejecting "spike" values.
         MEANX = 0.0
         MEANY = 0.0
         NORD = 0.0
         DO I = ORDERS( 1 ), IP
            IF ( GSQ( I ) ) THEN
               MEANX = MEANX + GSA( I )
               MEANY = MEANY + GSB( I )
               NORD = NORD + 1.0
               IF ( I.GE.100 .AND. I.LE.110 ) THEN
                  MEANX = MEANX + ORDW * GSA( I )
                  MEANY = MEANY + ORDW * GSB( I )
                  NORD = NORD + ORDW
               END IF
            END IF
         END DO

         IF ( NORD .EQ. 0 ) THEN
            CALL LINE_WCONT( '%p Possible divide zero avoided.\\',
     :                       STATUS )
            CALL PRTBUF( STATUS )

         ELSE
            MEANX = MEANX / NORD
            MEANY = MEANY / NORD
         END IF

*     Display result.
         CALL LINE_WRITF(
     :        '%p Corrected mean relative geometric shift [%.3f,\\',
     :                    MEANX )
         CALL LINE_WRITF( '%.3f] \\', MEANY )
         CALL LINE_WRITI( 'for orders (%i,\\', ORDERS( 1 ) )
         CALL LINE_WRITI( '%i).\\', IP )
         CALL PRTBUF( STATUS )

*     Display GSHIFT.
         GX = MEANX + DISPSG( IAPER )
         GY = MEANY + DISPLG( IAPER )
         CALL LINE_WRITF(
     :        '%p Corrected mean absolute geometric shift [%.3f,\\',
     :                    GX )
         CALL LINE_WRITF( '%.3f]\\', GY )
         CALL PRTBUF( STATUS )

*     Set shift values.
         DISPSG( IAPER ) = GX
         DISPLG( IAPER ) = GY
         CALL MODCAL

*  AGSHIFT for LORES.
      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN

*     Check that there is a scan.
         IF ( NOSCAN ) THEN
            CALL ERROUT( 'Error: no scan\\', STATUS )
            GO TO 999
         END IF

*     Aperture/resolution.
         CALL DEFAPR( 1, IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Define template.
         CALL LOTEM( IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Find peak in the scan.
         MAXSCAN = FSCAN( 1 )
         IP = 1
         DO I = 1, NSCAN
            IF ( FSCAN( I ) .GT. MAXSCAN ) THEN
               MAXSCAN = FSCAN( I )
               IP = I
            END IF
         END DO

*     Calculate centroid position of order based on peak.
*     Use 9 pixels in LORES.
         CENT = 0.0
         SIGFLUX = 0.0
         DO J = IP - 4, IP + 4
            PU = U1SCAN + DBLE( J - 1 ) * DUSCAN
            CENT = CENT + FSCAN ( J ) * PU
            SIGFLUX = SIGFLUX + FSCAN ( J )
         END DO
         IF ( SIGFLUX .EQ. 0.0 ) THEN
            CALL LINE_WCONT( '%p Possible divide zero avoided.\\',
     :                             STATUS )
            CALL PRTBUF( STATUS )

         ELSE
            CENT = CENT / SIGFLUX
         END IF

         DW = V1SCAN
         CALL WTOG( 0.0d0, DW, XOLD, YOLD )
         CALL WTOG( CENT, DW, XNEW, YNEW )
         DX = XNEW - XOLD
         DY = YNEW - YOLD

*     Display the shift for this centroid.
         CALL MSG_FMTC( 'OP', 'A',
     :                  ' Relative geometric shift (' )
         CALL MSG_FMTD( 'OP', 'F6.3', DX )
         CALL MSG_FMTC( 'OP', 'A', ',' )
         CALL MSG_FMTD( 'OP', 'F6.3', DY )
         CALL MSG_FMTC( 'OP', 'A', ').' )
         CALL MSG_LOAD( ' ', ' ^OP', AMESSAGE, NCHAR, STATUS )
         CALL GEN_CTOS( AMESSAGE, 132, ASTRING, NCHAR )
         CALL LINE_WRITS( '%p%s\\', ASTRING )
         CALL PRTBUF( STATUS )

*     Display GSHIFT.
         GX = DX + DISPSG( IAPER )
         GY = DY + DISPLG( IAPER )
         CALL LINE_WRITF(
     :        '%p Corrected absolute geometric shift [%.3f,\\',
     :                    GX )
         CALL LINE_WRITF( '%.3f]\\', GY )
         CALL PRTBUF( STATUS )

*     Set shift values.
         DISPSG( IAPER ) = GX
         DISPLG( IAPER ) = GY
         CALL MODCAL

*  IUEDR internal error.
      ELSE
         CALL ERROUT( 'Error: unknown IUE Resolution\\', STATUS )
      END IF

 999  CONTINUE

      END
