      SUBROUTINE EDITIM( NAXIS1, NAXIS2, DATA, QUAL, STATUS )
*+
*  Name:
*     SUBROUTINE EDITIM
*
*  Description:
*     Use graphics cursor to edit image data quality.
*
*  Routine History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          15-DEC-88     IUEDR Vn. 2.0
*     Martin Clayton     12-OCT-94     IUEDR Vn. 3.1-6
*
*  Method:
*     The image display cursor is used to edit image data quality.
*     The cursor is used to delineate ranges (or pick individual points)
*     and change the user part of the data quality.
*     The following cursor sequences are adopted:
*
*        1 1       set a range of points GOOD
*        2 2       set a range of points BAD
*        1         set nearest point GOOD
*        2         set nearest point BAD
*        3         finish
*
*     On a terminal keyboard these are the actual ASCII keys.
*     If the data quality changes after a session, the image data quality
*     is marked as requiring saving on disk.
*     The assumption is made that the current image displayed corresponds
*     to the current dataset!
*     As a check on this, for a pick operation the cursor must be within
*     the order range (a crude check).
*
*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Import:
      INTEGER NAXIS1   ! size of axis 1
      INTEGER NAXIS2   ! size of axis 2

      INTEGER*2 DATA(NAXIS1, NAXIS2) ! Data Array

*  Import-Export:
      BYTE QUAL(NAXIS1, NAXIS2) ! data quality

*  Export:
      INTEGER STATUS   ! status return

*  External references:
      LOGICAL STR_SIMLR ! caseless string equality

      INTEGER DQ_AND   ! data quality AND

*  Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMDATA'

*  Local constants:
      INTEGER BAD      ! BAD index
      INTEGER CURMOD   ! GKS cursor mode (0 for default)
      INTEGER GOOD     ! GOOD index
      PARAMETER (BAD = 2, CURMOD = 0, GOOD = 1)

*  Local variables:
      REAL*8 X1        ! hit x1-coordinate
      REAL*8 X2        ! hit x2-coordinate
      REAL*8 Y1        ! hit y1-coordinate
      REAL*8 Y2        ! hit y2-coordinate

      REAL   XCUR      ! cursor position
      REAL   YCUR      ! cursor position

      LOGICAL FROM     ! FROM index (for cursor limits)

      INTEGER DQ       ! data quality temporary
      INTEGER HIT1     ! hit index
      INTEGER HIT2     ! hit index
      INTEGER IX       ! loop index
      INTEGER IXC      ! X-axis cursor index
      INTEGER IX1      ! X-axis cursor index for first hit
      INTEGER IX2      ! X-axis cursor index for second hit
      INTEGER IY       ! loop index
      INTEGER IYC      ! Y-axis cursor index
      INTEGER IY1      ! Y-axis cursor index for first hit
      INTEGER IY2      ! Y-axis cursor index for second hit
      INTEGER NFOUND   ! number found to change (single edit)
      INTEGER NTOTAL   ! number found to change (total)

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise FROM
      FROM = .FALSE.

*  Open zone cursor
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )

*  Check that the axis label makes sense
      ELSE
         IF ( .NOT.STR_SIMLR( 'S\\', XLAB ) .OR.
     :        .NOT.STR_SIMLR( 'L\\', YLAB ) ) THEN
            CALL ERROUT( 'Error: not an image displayed\\', STATUS )

*     Cursor loop
         ELSE
            NTOTAL = 0

*        Initialise XCUR, YCUR
            XCUR = 0.0
            YCUR = 0.0

            DO WHILE ( .TRUE. )
               IF (.NOT.FROM) THEN

*              For first cursor hit
                  CALL GRF_CUZONE( '12', CURMOD, HIT1, XCUR, YCUR,
     :                             STATUS )
                  X1 = DBLE(XCUR)
                  Y1 = DBLE(YCUR)
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERROUT('Error: reading cursor\\', STATUS )
                     GO TO 200

                  ELSE IF ( HIT1 .LE. 0 ) THEN
                     GO TO 200

                  ELSE IF ( HIT1.EQ.GOOD .OR. HIT1.EQ.BAD ) THEN
                     IXC = NINT(REAL(X1))
                     IYC = NINT(REAL(Y1))
                     CALL LINE_WRITI('%p Pixel at (%i,\\', IXC)
                     CALL LINE_WRITI('%i)\\', IYC)
                     IF ( IXC.LT.1 .OR. IXC.GT.NAXIS1 .OR.
     :                    IYC.LT.1 .OR. IYC.GT.NAXIS2 ) THEN
                        CALL LINE_WCONT( '  outside image array.\\' )

                     ELSE IF ( DATA(IXC, IYC) .EQ. DBLANK ) THEN
                        CALL LINE_WCONT( '  is blank.\\' )

                     ELSE IF ( HIT1 .EQ. GOOD ) THEN
                        FROM = .TRUE.
                        CALL DQ_UTOI( QUAL(IXC, IYC), DQ )
                        IF ( DQ_AND(DQ, 2) .EQ. 0 ) THEN
                           CALL LINE_WCONT( '  already GOOD.\\' )

                        ELSE
                           CALL LINE_WCONT( '  made GOOD.\\' )
                           CALL DQ_WRPK( 0, 2, 1, DQ )
                           CALL DQ_ITOU( DQ, QUAL(IXC, IYC) )
                           NTOTAL = NTOTAL + 1
                        END IF

                     ELSE IF ( HIT1 .EQ. BAD ) THEN
                        FROM = .TRUE.
                        CALL DQ_UTOI( QUAL(IXC, IYC), DQ )
                        IF ( DQ_AND(DQ, 2) .EQ. 1 ) THEN
                           CALL LINE_WCONT( '  already BAD.\\' )

                        ELSE
                           CALL LINE_WCONT( '  made BAD.\\' )
                           CALL DQ_WRPK( 1, 2, 1, DQ )
                           CALL DQ_ITOU( DQ, QUAL(IXC, IYC) )
                           NTOTAL = NTOTAL + 1
                        END IF
                     END IF
                     CALL PRTBUF( STATUS )
                  END IF
               ELSE IF ( FROM ) THEN
*              Second cursor hit
                  IX1 = NINT(REAL(X1))
                  IY1 = NINT(REAL(Y1))
                  CALL GRF_CUZONE( '12', CURMOD, HIT2, XCUR, YCUR,
     :                             STATUS )
                  X2 = DBLE(XCUR)
                  Y2 = DBLE(YCUR)

                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERROUT( 'Error: reading cursor\\', STATUS )
                     GO TO 200

                  ELSE IF ( HIT2 .LE. 0 ) THEN
                     GO TO 200

                  ELSE IF ( HIT2.NE.GOOD .AND. HIT2.NE.BAD ) THEN
                     CALL ERROUT( 'Error: unexpected cursor hit\\',
     :                            STATUS )

                  ELSE
                     IX2 = NINT(REAL(X2))
                     IY2 = NINT(REAL(Y2))
                     IF ( IX1 .GT. IX2 ) CALL MSC_ISWAP( IX1, IX2 )
                     IX1 = MAX(IX1, 1)
                     IX2 = MIN(IX2, NAXIS1)
                     IF ( IY1 .GT. IY2 ) CALL MSC_ISWAP( IY1, IY2 )
                     IY1 = MAX(IY1, 1)
                     IY2 = MIN(IY2, NAXIS2)
                     CALL LINE_WRITI( '%p Pixels from (%i,\\', IX1 )
                     CALL LINE_WRITI( '%i) to\\', IY1 )
                     CALL LINE_WRITI( ' (%i,\\', IX2 )
                     CALL LINE_WRITI( '%i)\\', IY2 )
                     NFOUND = 0
                     DO IY = IY1, IY2
                        DO IX = IX1, IX2
                           IF ( DATA(IX, IY) .NE. DBLANK ) THEN
                              CALL DQ_UTOI( QUAL(IX, IY), DQ )
                              IF ( DQ_AND(DQ, 1) .EQ. 0 ) THEN
                                 IF ( HIT1.EQ.GOOD .AND.
     :                                HIT2.EQ.GOOD ) THEN
                                    IF ( DQ_AND(DQ, 2) .NE. 0 ) THEN
                                       CALL DQ_WRPK( 0, 2, 1, DQ )
                                       NFOUND = NFOUND + 1
                                    END IF

                                 ELSE IF ( HIT1.EQ.BAD .AND.
     :                                     HIT2.EQ.BAD ) THEN
                                    IF ( DQ_AND(DQ, 2) .EQ. 0 ) THEN
                                       CALL DQ_WRPK( 1, 2, 1, DQ )
                                       NFOUND = NFOUND + 1
                                    END IF
                                 END IF
                                 CALL DQ_ITOU( DQ, QUAL(IX, IY) )
                              END IF
                           END IF
                        END DO
                     END DO

                     IF ( HIT1.EQ.GOOD .AND. HIT2.EQ.GOOD ) THEN
                        CALL LINE_WRITI( '  made GOOD (%i pixels).\\',
     :                                   NFOUND )
                        FROM = .FALSE.

                     ELSE IF ( HIT1.EQ.BAD .AND. HIT2.EQ.BAD ) THEN
                        CALL LINE_WRITI( '  made BAD (%i pixels).\\',
     :                                   NFOUND )
                        FROM = .FALSE.
                     END IF
                     CALL PRTBUF( STATUS )
                     NTOTAL = NTOTAL + NFOUND
                  END IF

               ELSE
                  CALL ERROUT( 'Error: unexpected cursor hit\\',
     :                         STATUS )
                  GO TO 200
               END IF
            END DO
 200        CONTINUE

*        If status is good, see if data quality HAS changed
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( NTOTAL .EQ. 0 ) THEN
                  CALL LINE_WCONT(
     :                        '%p Image Data Quality not changed.\\' )
                  CALL PRTBUF( STATUS )

               ELSE
                  CALL LINE_WRITI(
     :                 '%p Image Data Quality changed (%i pixels).\\',
     :                 NTOTAL )
                  CALL PRTBUF( STATUS )
                  CALL MODUEQ
               END IF
            END IF
         END IF
      END IF
      END
