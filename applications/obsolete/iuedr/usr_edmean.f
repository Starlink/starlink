      SUBROUTINE USR_EDMEAN( STATUS )
*+
*  Name:
*     SUBROUTINE USR_EDMEAN

*  Purpose:
*     Visually edit the current mean spectrum quality.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_EDMEAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The cursor is read on the current DIAGRAM which is assumed to
*     have WAVELENGTH as the horizontal axis.
*     The cursor is used to delineate ranges (or pick individual points)
*     and change the user part of the data quality.
*     The following cursor sequences are adopted:
*
*        1 1       set a range of points GOOD
*        2 2       set a range of points BAD
*        1         set point nearest in wavelength GOOD
*        2         set point nearest in wavelength BAD
*        3/4       finish
*
*     On a terminal keyboard these are the actual ASCII keys.
*     If the data quality changes after a session, the spectrum is
*     marked as requiring saving on disk.

*  Method:
*     Note that the system assumes that it is working in Air wavelengths.
*     Also it assumes that the display which the user is basing wavelength
*     ranges on actually is that of the current spectrum.
*     As a check on this, for a pick operation the cursor must be within
*     the data wavelength range (only a crude check).

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
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
      INTEGER STATUS    ! Global status.

*  External References:
      LOGICAL STR_SIMLR ! Caseless string equality.

      INTEGER DQ_AND    ! Data quality AND.

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMTEMP'

*  Local Constants:
      INTEGER BAD       ! Bad pixel cursor hit.
      INTEGER CURMOD    ! Cursor mode (GKS default = 0).
      INTEGER GOOD      ! Good pixel cursor hit.
      INTEGER MAXPOINT  ! Maximum plot size.
      PARAMETER ( BAD = 2, CURMOD = 0, GOOD = 1, MAXPOINT = 27800 )

*  Local variables:
      REAL*8 X1         ! Hit x1-coordinate.
      REAL*8 X2         ! Hit x2-coordinate.
      REAL*8 Y1         ! Hit y1-coordinate.
      REAL*8 Y2         ! Hit y2-coordinate.

      REAL XCUR         ! Cursor position.
      REAL YCUR         ! Cursor position.

      INTEGER DQ( MAXPOINT ) ! Local copy of data quality.
      INTEGER HIT1      ! First hit index.
      INTEGER HIT2      ! Second hit index.
      INTEGER I         ! Loop index.
      INTEGER NEAR      ! Index of nearest point.
      INTEGER NFOUND    ! Number found to change.

      LOGICAL FROM      ! First hit index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise FROM.
      FROM = .FALSE.

*   Access mean spectrum.
      CALL RDCOMB( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing dataset\\', STATUS )
         GO TO 999
      END IF

*   Open zone.
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )
         GO TO 999
      END IF

*   Check that the axis label makes sense.
      IF ( .NOT. STR_SIMLR( 'WAVELENGTH\\', XLAB ) ) THEN
         CALL ERROUT( 'Error: incorrect display type\\', STATUS )
         GO TO 999
      END IF

*   Copy the data quality before changes.
      DO I = 1, NCOMB
         DQ( I ) = QCOMB( I )
         TMPWRK1( I ) = XCOMB( I )
      END DO

*   Initialise XCUR, YCUR.
      XCUR = TMPWRK1( 1 )
      YCUR = 0.0

*   Cursor loop.
      DO WHILE ( .TRUE. )
         IF ( .NOT. FROM ) THEN

*         First cursor hit
            CALL GRF_CUZONE( '12', CURMOD, HIT1, XCUR, YCUR, STATUS )
            X1 = DBLE( XCUR )
            Y1 = DBLE( YCUR )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading cursor\\', STATUS )
               GO TO 300

            ELSE IF ( HIT1 .LE. 0 ) THEN
               GO TO 300

            ELSE IF ( HIT1.EQ.GOOD .OR. HIT1.EQ.BAD ) THEN
               IF ( X1.GE.TMPWRK1( 1 ) .AND.
     :              X1.LE.TMPWRK1( NCOMB ) ) THEN

*               Set FROM to be TRUE.
                  FROM = .TRUE.

*               Determine the position of the nearest good pixel.
                  NEAR = 0

                  DO I = 1, NCOMB
                     IF ( DQ_AND( DQ( I ), 1) .EQ. 0 ) THEN
                        IF ( NEAR .EQ. 0 ) THEN
                           NEAR = I

                        ELSE IF ( ABS( X1 - TMPWRK1( I ) ) .LT.
     :                            ABS( X1 - TMPWRK1( NEAR ) ) ) THEN
                           NEAR = I
                        END IF
                     END IF
                  END DO

                  IF ( NEAR .EQ. 0 ) THEN

*                  No nearest good pixel.
                     CALL LINE_WCONT(
     :                        '%p There are no points near cursor\\' )
                     CALL PRTBUF( STATUS )

                  ELSE IF ( HIT1 .EQ. GOOD ) THEN
                     IF ( DQ_AND( DQ( NEAR ), 2 ) .EQ. 0 ) THEN

                        CALL LINE_WRITF(
     :                           '%p Point at %.3f (A) already GOOD.\\',
     :                           TMPWRK1( NEAR ) )
                        CALL PRTBUF( STATUS )

                     ELSE
                        CALL LINE_WRITF(
     :                            '%p Point at %.3f (A) made GOOD.\\',
     :                            TMPWRK1( NEAR ) )
                        CALL PRTBUF( STATUS )
                        CALL DQ_WRPK( 0, 2, 1, DQ( NEAR ) )
                     END IF

                  ELSE IF ( HIT1 .EQ. BAD ) THEN
                     IF ( DQ_AND( DQ( NEAR ), 2 ) .EQ. 1 ) THEN
                        CALL LINE_WRITF(
     :                           '%p Point at %.3f (A) already BAD.\\',
     :                           TMPWRK1( NEAR ) )
                        CALL PRTBUF( STATUS )

                     ELSE
                        CALL LINE_WRITF(
     :                            '%p Point at %.3f (A) made BAD.\\',
     :                            TMPWRK1( NEAR ) )
                        CALL PRTBUF( STATUS )
                        CALL DQ_WRPK( 1, 2, 1, DQ( NEAR ) )
                     END IF
                  END IF

               ELSE
                  CALL LINE_WCONT(
     :                   '%p Cursor is outside spectrum (beware).\\' )
                  CALL PRTBUF( STATUS )
               END IF
            END IF

         ELSE IF ( FROM ) THEN

*         Second cursor hit.
            CALL GRF_CUZONE( '12', CURMOD, HIT2, XCUR, YCUR, STATUS )
            X2 = DBLE( XCUR )
            Y2 = DBLE( YCUR )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading cursor\\', STATUS )
               GO TO 300

            ELSE IF ( HIT2 .LE. 0 ) THEN
               GO TO 300

            ELSE IF ( HIT2.EQ.GOOD .OR. HIT2.EQ.BAD ) THEN

*            Reset FROM.
               FROM = .FALSE.

               IF ( X1 .GT. X2 ) THEN
                  CALL MSC_DSWAP( X1, X2 )
               END IF
               CALL LINE_WRITF( '%p Wavelength range (%.3f,\\', X1 )
               CALL LINE_WRITF( '%.3f).\\', X2 )
               CALL PRTBUF( STATUS )
               NFOUND = 0

               DO I = 1, NCOMB
                  IF ( TMPWRK1( I ).GE.X1 .AND.
     :                 TMPWRK1( I ).LE.X2 ) THEN
                     IF ( DQ_AND( DQ( I ), 1) .EQ. 0 ) THEN
                        IF ( HIT1.EQ.GOOD .AND. HIT2.EQ.GOOD ) THEN
                           IF ( DQ_AND( DQ( I ), 2 ) .NE. 0 ) THEN
                              CALL DQ_WRPK( 0, 2, 1, DQ( I ) )
                              NFOUND = NFOUND + 1
                           END IF

                        ELSE IF ( HIT1.EQ.BAD .AND. HIT2.EQ.BAD ) THEN
                           IF ( DQ_AND( DQ( I ), 2 ) .EQ. 0 ) THEN
                              CALL DQ_WRPK( 1, 2, 1, DQ( I ) )
                              NFOUND = NFOUND + 1
                           END IF
                        END IF
                     END IF
                  END IF
               END DO

               IF ( NFOUND .EQ. 0 ) THEN
                  CALL LINE_WCONT( '%p No points changed.\\' )

               ELSE IF ( HIT2 .EQ. GOOD ) THEN
                  CALL LINE_WRITI( '%p %i points made GOOD.\\',
     :                             NFOUND )

               ELSE IF ( HIT2 .EQ. BAD ) THEN
                  CALL LINE_WRITI( '%p %i points made BAD.\\',
     :                             NFOUND )
               END IF
               CALL PRTBUF( STATUS )

            ELSE
               CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )
            END IF

         ELSE
            CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )
            GO TO 300
         END IF
      END DO
 300  CONTINUE

*   If status is good, see if data quality HAS changed.
      IF ( STATUS .EQ. SAI__OK ) THEN
         NFOUND = 0
         DO I = 1, NCOMB
            IF ( DQ( I ) .NE. QCOMB( I ) ) THEN
               NFOUND = NFOUND + 1
            END IF
         END DO

         IF ( NFOUND .EQ. 0 ) THEN
            CALL LINE_WCONT( '%p Data Quality not changed.\\' )
            CALL PRTBUF( STATUS )

         ELSE
            CALL LINE_WRITI( '%p Data Quality changed for %i points.\\',
     :                       NFOUND )
            CALL PRTBUF( STATUS )

            DO I = 1, NCOMB
               QCOMB( I ) = DQ( I )
            END DO

            CALL MODMAP
         END IF
      END IF

 999  CONTINUE

      END
