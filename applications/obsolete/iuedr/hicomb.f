      SUBROUTINE HICOMB( STATUS )
*+
*  Name:
*     SUBROUTINE HICOMB

*  Purpose:
*     Combine orders in a HIRES spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HICOMB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     28-JAN-95 (MJC):
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
      INCLUDE 'CMCOMB'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMWAV'
      INCLUDE 'CMFLX'
      INCLUDE 'CMTEMP'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS         ! Global status.

*  External References:
      INTEGER DQ_AND         ! 32 bit integer AND operation.
      EXTERNAL CAHI          ! HIRES calibration routine.

*  Local Variables:
      REAL*8 WLIM( 2 )       ! Calibrated wavwelength limits for order.
      REAL*8 WSTEP           ! Wavelength step in order.
      REAL*8 XLIM( 2 )       ! X-grid wavelength limits.
      REAL*8 XSTEP           ! Global grid step.

      INTEGER ACTVAL         ! Parameter value count.
      INTEGER I              ! Loop index.
      INTEGER INDS( 2 )      ! Indices of grid changed by MASPEC.
      INTEGER IORD           ! Order index.
      INTEGER NORD           ! Number of orders mapped.
      INTEGER ORD            ! Order number.
      INTEGER ORDS( 2 )      ! Pair of orders defining range.
      INTEGER QT( MAXPOINT ) ! Temporary Q.

      LOGICAL COVER          ! Whether gaps covered by other orders.
      LOGICAL FILL           ! Whether gaps filled within order.
      LOGICAL RESET          ! Whether existing grid is reset.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there are SOME orders in spectrum.
      IF ( NOSPEC .OR. NORDER.LE.0 ) THEN
         CALL ERROUT( 'Error: no orders defined\\', STATUS )
         GO TO 999
      END IF

*  RM parameter.
      CALL RDPARL( 'RM\\', .FALSE., 1, RESET, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'RM\\', STATUS )
         GO TO 999

      ELSE IF ( RESET ) THEN
         CALL LINE_WCONT( '%p New Spectrum.\\')
         CALL PRTBUF( STATUS )
         NOCOMB = .TRUE.

      ELSE IF ( .NOT. NOCOMB ) THEN
         CALL LINE_WCONT( '%p Will combine with existing spectrum.\\' )
         CALL PRTBUF( STATUS )

         DO I = 1, NCOMB
            IF ( QCOMB( I ) .EQ. 0 ) THEN
               YCOMB( I ) = YCOMB( I ) * WCOMB( I )
            END IF
         END DO
      END IF

*  ORDERS.
      CALL GET_ORDERS( .FALSE., ORDS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Print order range.
      CALL LINE_WRITI( '%p Orders in the range (%i,\\', ORDS( 1 ) )
      CALL LINE_WRITI( '%i) can be used.\\', ORDS( 2 ) )
      CALL PRTBUF( STATUS )

*  Construct grid if none already defined.
      IF ( NOCOMB ) THEN

*     ML parameter.
 250     CONTINUE
         CALL RDPARF( 'ML\\', .FALSE., 2, XLIM, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ML\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .LT. 2 ) THEN
            CALL ERRPAR( 'ML\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE IF ( XLIM( 1 ) .EQ. XLIM( 2 ) ) THEN
            CALL ERRPAR( 'ML\\' )
            CALL ERROUT( ': must be specified\\', STATUS )

         ELSE
            IF ( XLIM( 1 ) .GT. XLIM( 2 ) ) THEN
               CALL MSC_DSWAP( XLIM( 1 ), XLIM( 2 ) )
            END IF
            GO TO 300
         END IF

         CALL CNPAR( 'ML\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'ML\\', STATUS )
            GO TO 999
         END IF

         GO TO 250
 300     CONTINUE

*     Sample rate from orders contained in range.
         NORD = 0
         DO ORD = ORDS( 1 ), ORDS( 2 )
            CALL FNORD( ORD, IORD )
            IF ( IORD .GT. 0 ) THEN
               IF ( .NOT.( WAV2S( IORD ).LT.XLIM( 1 ) .OR.
     :                     WAV1S( IORD ).GT.XLIM( 2 ) ) ) THEN
                  WLIM( 1 ) = WAV1S( IORD )
                  WLIM( 2 ) = WAV2S( IORD )
                  CALL CAWAV( 1, ORD, 2, WLIM )
                  WSTEP = ( WLIM( 2 ) - WLIM( 1 ) ) /
     :                    DBLE( NWAVS( IORD ) - 1 )

                  IF ( NORD .EQ. 0 ) THEN
                     XSTEP = WSTEP

                  ELSE
                     XSTEP = MIN( XSTEP, WSTEP )
                  END IF
                  NORD = NORD + 1
               END IF
            END IF
         END DO

*     If no orders in range need not continue.
         IF ( NORD .EQ. 0 ) THEN
            CALL ERROUT( 'Error: no orders in range\\', STATUS )
            GO TO 999
         END IF

*     MSAMP parameter.
         CALL RDPARF( 'MSAMP\\', .TRUE., 1, XSTEP, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'MSAMP\\', STATUS )
            GO TO 999

         ELSE IF ( XSTEP .GE. ( XLIM( 2 ) - XLIM( 1 ) ) ) THEN
            CALL ERROUT( 'Error: grid spacing too large\\', STATUS )

         ELSE IF ( XSTEP .EQ. 0.0 ) THEN
            CALL ERROUT( 'Error: grid spacing too small\\', STATUS )

         ELSE
            NCOMB = ( XLIM( 2 ) - XLIM( 1 ) ) / XSTEP + 1.0
            IF ( NCOMB .LT. 2 ) THEN
               CALL ERROUT( 'Error: too few points in grid\\', STATUS )

            ELSE IF ( NCOMB .GT. MAXPOINT ) THEN
               CALL ERROUT( 'Error: too many points in grid\\', STATUS )
            END IF
         END IF

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL CNPAR( 'MSAMP\\', STATUS )
            GO TO 999
         END IF

*     Fill in missing details of grid.
         XCOMB1 = XLIM( 1 )
         DXCOMB = XSTEP

         DO I = 1, NCOMB
            XCOMB( I ) = DBLE( I - 1 ) * DXCOMB + XCOMB1
            YCOMB( I ) = 0.0
            WCOMB( I ) = 0.0
            QCOMB( I ) = 1
         END DO
      END IF

*  Print actual grid.
      CALL LINE_WRITF( '%p Wavelength grid is (%.3f, \\', XCOMB1 )
      CALL LINE_WRITF( '%.3f,\\', DBLE( NCOMB ) * DXCOMB + XCOMB1 )
      CALL LINE_WRITF( '%.3f).\\', DXCOMB )
      CALL PRTBUF( STATUS )

*  Enlarge limits to include border.
      XLIM( 1 ) = XCOMB1 - DXCOMB
      XLIM( 2 ) = DBLE( NCOMB ) * DXCOMB + XCOMB1

*  Set combined spectrum undefined during change.
      NOCOMB = .TRUE.

*  Initially reset whole of temporary grid.
      INDS( 1 ) = 1
      INDS( 2 ) = NCOMB

*  FILLGAP parameter.
      CALL RDPARL( 'FILLGAP\\', .FALSE., 1, FILL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'FILLGAP\\', STATUS )
         GO TO 999
      END IF

*  COVERGAP parameter.
      CALL RDPARL( 'COVERGAP\\', .FALSE., 1, COVER, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'COVERGAP\\', STATUS )
         GO TO 999
      END IF

*  Give account of how filling will take place.
      IF ( FILL .AND. COVER ) THEN
         CALL LINE_WCONT(
     :        '%p Gaps can be filled by adjacent points/spectra.\\'
     :           )

      ELSE IF ( FILL ) THEN
         CALL LINE_WCONT(
     :        '%p Gaps can be filled by adjacent points.\\' )

      ELSE IF ( COVER ) THEN
         CALL LINE_WCONT(
     :        '%p Gaps can be filled by adjacent spectra.\\' )

      ELSE
         CALL LINE_WCONT( '%p Gaps are not filled.\\' )
      END IF
      CALL PRTBUF( STATUS )

*  Map each order in list.
      DO ORD = ORDS( 1 ), ORDS( 2 )
         CALL FNORD( ORD, IORD )
         IF ( IORD .GT. 0 ) THEN
            WLIM( 1 ) = WAV1S( IORD )
            WLIM( 2 ) = WAV2S( IORD )
            CALL CAWAV( 1, ORD, 2, WLIM )

            IF ( .NOT.( WLIM( 2 ).LT.XLIM( 1 ) .OR.
     :                  WLIM( 1 ).GT.XLIM( 2 ) ) ) THEN
               CALL RDORD( ORD, CAHI, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERRSTR( 'Error: accessing order \\' )
                  CALL ERRINT( ORD )
                  CALL ERROUT( '\\', STATUS )
                  GO TO 999
               END IF

               CALL LINE_WRITI( '%p Using order %i\\', ORD )
               CALL PRTBUF( STATUS )

               DO I = INDS( 1 ), INDS( 2 )
                  TMPWRK2( I ) = 0.0
                  TMPWRK1( I ) = 0.0
                  QT( I ) = 1
               END DO

               CALL MASPEC( FILL, 3, NCOMB, XCOMB,
     :                      TMPWRK2, TMPWRK1, QT, INDS )

               DO I = INDS( 1 ), INDS( 2 )
                  IF ( QT( I ).EQ.0 .AND. QCOMB( I ).NE.3 ) THEN
                     YCOMB( I ) = YCOMB( I ) + TMPWRK2( I )
                     WCOMB( I ) = WCOMB( I ) + TMPWRK1( I )
                     QCOMB( I ) = 0
                  END IF
               END DO

               IF ( .NOT. COVER ) THEN
                  DO  I = INDS( 1 ), INDS( 2 )
                     IF ( QT( I ) .NE. 0 ) THEN
                        YCOMB( I ) = 0.0
                        WCOMB( I ) = 0.0
                        QCOMB( I ) = 3
                     END IF
                  END DO
               END IF
            END IF
         END IF
      END DO

*  Normalise spectrum.
      DO I = 1, NCOMB
         IF ( DQ_AND( QCOMB( I ), 1 ).EQ.0 .AND.
     :        WCOMB( I ).GT.0.0 ) THEN
            YCOMB( I ) = YCOMB( I ) / WCOMB( I )

         ELSE IF ( QCOMB( I ) .NE. 3 ) THEN
            YCOMB( I ) = 0.0
            WCOMB( I ) = 0.0
            QCOMB( I ) = 1
         END IF
      END DO

*  Some labels.
      CALL STR_MOVE( TITLE, MAXLABEL, MTITLE )
      CALL STR_MOVE( WLABEL, MAXLABEL, XMLAB )
      CALL STR_MOVE( WUNITS, MAXLABEL, XMUN )
      CALL STR_MOVE( FLABEL, MAXLABEL, YMLAB )
      CALL STR_MOVE( FUNITS, MAXLABEL, YMUN )

*  Set the combined spectrum defined.
      NOCOMB = .FALSE.
      CALL MODMAP

 999  CONTINUE

      END
