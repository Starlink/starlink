      SUBROUTINE EDITLO( STATUS )

*+
*
*   Name:
*      SUBROUTINE EDITLO
*
*   Description:
*      Use graphics cursor to edit LORES data quality.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     19-OCT-94     IUEDR Vn. 3.1-9
*
*   Method:
*      The cursor is read on the current DIAGRAM which is assumed to
*      have WAVELENGTH as the horizontal axis.
*      The cursor is used to delineate ranges (or pick individual points)
*      and change the user part of the data quality.
*      The following cursor sequences are adopted:
*
*         1 1       set a range of points GOOD
*         2 2       set a range of points BAD
*         1         set point nearest in wavelength GOOD
*         2         set point nearest in wavelength BAD
*         3/4       finish
*
*      On a terminal keyboard these are the actual ASCII keys.
*      If the data quality changes after a session, the spectrum is
*      marked as requiring saving on disk.
*
*      Note that the system assumes that it is working in Air wavelengths.
*      Also it assumes that the display which the user is basing wavelength
*      ranges on actually is that of the current spectrum.
*      As a check on this, for a pick operation the cursor must be within
*      the order range (a crude check).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality
      INTEGER DQ_AND        ! data quality AND

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMWAV'
      INCLUDE 'CMNET'

*   Local constants:
      INTEGER BAD          ! BAD pixel cursor hit
      INTEGER CURMOD       ! cursor mode (GKS default = 0)
      INTEGER ERR          ! error status
      INTEGER GOOD         ! GOOD pixel cursor hit
      INTEGER MAXWAV       ! maximum number of wavelengths
      PARAMETER (BAD = 2, CURMOD = 0, ERR = -3, GOOD = 1,
     :           MAXWAV = 1200)

*   Local variables:
      REAL*8 X(MAXWAV)     ! local copy of wavelengths (coding fudge)
      REAL*8 X1            ! hit x1-coordinate
      REAL*8 X2            ! hit x2-coordinate
      REAL*8 Y1            ! hit y1-coordinate
      REAL*8 Y2            ! hit y2-coordinate

      REAL   XCUR          ! cursor coordinate
      REAL   YCUR          ! cursor coordinate

      INTEGER DQ(MAXWAV)   ! local copy of data quality
      INTEGER I            ! do loop counter
      INTEGER HIT1         ! first hit index
      INTEGER HIT2         ! second hit index
      INTEGER NEAR         ! index of neaest point
      INTEGER NFOUND       ! number found to change

      LOGICAL FROM         ! first hit index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise FROM
      FROM = .FALSE.

*   Open zone
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )
         GO TO 999
      END IF

*   Check that the axis label makes sense
      IF ( .NOT. STR_SIMLR( 'WAVELENGTH\\', XLAB ) ) THEN
         CALL ERROUT( 'Error: incorrect display type\\', STATUS )
         GO TO 999
      END IF

*   Copy the data quality before changes
      DO I = 1, NWAV
         DQ(I) = QNET(I)
         X(I) = WAVAIR(I)
      END DO

*   Initialise XCUR, YCUR
      XCUR = REAL(X(1))
      YCUR = 0.0

*   Cursor loop
      DO WHILE ( .TRUE. )
         IF ( .NOT. FROM ) THEN

*         First cursor hit
            CALL GRF_CUZONE( '12', CURMOD, HIT1, XCUR, YCUR, STATUS )
            X1 = DBLE(XCUR)
            Y1 = DBLE(YCUR)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading cursor\\', STATUS )
               GO TO 300

            ELSE IF ( HIT1 .LE. 0 ) THEN
               GO TO 300

            ELSE IF ( HIT1.EQ.GOOD .OR. HIT1.EQ.BAD ) THEN
               IF ( X1.GE.X(1) .AND. X1.LE.X(NWAV) ) THEN

*               Set FROM to be TRUE
                  FROM = .TRUE.

*               Determine the position of the nearest good pixel
                  NEAR = 0

                  DO I = 1, NWAV
                     IF ( DQ_AND(DQ(I), 1) .EQ. 0 ) THEN
                        IF ( NEAR .EQ. 0 ) THEN
                           NEAR = I

                        ELSE IF ( ABS(X1 - X(I)) .LT.
     :                            ABS(X1 - X(NEAR)) ) THEN
                           NEAR = I
                        END IF
                     END IF
                  END DO

                  IF ( NEAR .EQ. 0 ) THEN
                     CALL LINE_WCONT(
     :                        '%p There are no points near cursor\\' )
                     CALL PRTBUF( STATUS )

                  ELSE IF ( HIT1 .EQ. GOOD ) THEN
                     IF ( DQ_AND(DQ(NEAR), 2) .EQ. 0 ) THEN
                        CALL LINE_WRITF(
     :                         '%p Point at %.3f (A) already GOOD.\\',
     :                         X(NEAR) )
                        CALL PRTBUF( STATUS )

                     ELSE
                        CALL LINE_WRITF(
     :                            '%p Point at %.3f (A) made GOOD.\\',
     :                            X(NEAR) )
                        CALL PRTBUF( STATUS )
                        CALL DQ_WRPK( 0, 2, 1, DQ(NEAR) )
                     END IF

                  ELSE IF ( HIT1 .EQ. BAD ) THEN
                     IF ( DQ_AND(DQ(NEAR), 2) .EQ. 1 ) THEN
                        CALL LINE_WRITF(
     :                          '%p Point at %.3f (A) already BAD.\\',
     :                          X(NEAR) )
                        CALL PRTBUF( STATUS )

                     ELSE
                        CALL LINE_WRITF(
     :                             '%p Point at %.3f (A) made BAD.\\',
     :                             X(NEAR) )
                        CALL PRTBUF( STATUS )
                        CALL DQ_WRPK( 1, 2, 1, DQ(NEAR) )
                     END IF
                  END IF

               ELSE
                  CALL LINE_WCONT(
     :                    '%p Cursor is outside spectrum (beware).\\' )
                  CALL PRTBUF( STATUS )
               END IF
            END IF

         ELSE IF ( FROM ) THEN

*         Second cursor hit
            CALL GRF_CUZONE( '12', CURMOD, HIT2, XCUR, YCUR, STATUS )
            X2 = DBLE(XCUR)
            Y2 = DBLE(YCUR)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading cursor\\', STATUS )
               GO TO 300

            ELSE IF ( HIT2 .LE. 0 ) THEN
               GO TO 300

            ELSE IF ( HIT2.EQ.GOOD .OR. HIT2.EQ.BAD ) THEN

*            Reset FROM
               FROM = .FALSE.

               IF ( X1 .GT. X2 ) CALL MSC_DSWAP( X1, X2 )
               CALL LINE_WRITF( '%p Wavelength range (%.3f,\\', X1 )
               CALL LINE_WRITF( '%.3f).\\', X2 )
               CALL PRTBUF( STATUS )
               NFOUND = 0

               DO I = 1, NWAV
                  IF ( X(I).GE.X1 .AND. X(I).LE.X2 ) THEN
                     IF ( DQ_AND(DQ(I), 1) .EQ. 0 ) THEN
                        IF ( HIT1.EQ.GOOD .AND.
     :                       HIT2.EQ.GOOD ) THEN
                           IF ( DQ_AND(DQ(I), 2) .NE. 0 ) THEN
                              CALL DQ_WRPK( 0, 2, 1, DQ(I) )
                              NFOUND = NFOUND + 1
                           END IF

                        ELSE IF ( HIT1.EQ.BAD .AND.
     :                            HIT2.EQ.BAD ) THEN
                           IF ( DQ_AND(DQ(I), 2) .EQ. 0 ) THEN
                              CALL DQ_WRPK( 1, 2, 1, DQ(I) )
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
                  CALL LINE_WRITI( '%p %i points made BAD.\\', NFOUND )
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

*   If status is good, see if data quality HAS changed
      IF ( STATUS .EQ. SAI__OK ) THEN
         NFOUND = 0

         DO I = 1, NWAV
            IF ( DQ(I) .NE. QNET(I) ) NFOUND = NFOUND + 1
         END DO

         IF ( NFOUND .EQ. 0 ) THEN
            CALL LINE_WCONT( '%p Data Quality not changed.\\' )
            CALL PRTBUF( STATUS )

         ELSE
            CALL LINE_WRITI(
     :                '%p Data Quality changed for %i points.\\',
     :                NFOUND)
            CALL PRTBUF( STATUS )

            DO I = 1, NWAV
               QNET(I) = DQ(I)
            END DO

            CALL WRDQ( STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: updating spectrum store\\', STATUS )

            ELSE
               CALL RECAL( STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERROUT( 'Error: recalibrating spectrum\\',
     :                         STATUS )
               END IF
            END IF
         END IF
      END IF

 999  CONTINUE
      END
