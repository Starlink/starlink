      SUBROUTINE MODIM( NAXIS1, NAXIS2, DATA, QUAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE MODIM
*
*   Description:
*      Use the graphics cursor to edit the Image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          15-DEC-88     IUEDR Vn. 2.0
*      Martin Clayton     24-OCT-94     IUEDR Vn. 3.2
*
*   Method:
*      The image display cursor is used to modify image data.
*      The following cursor sequences are adopted:
*
*         1 2       Copy intensity of first picked pixel to the second
*         2         Prompt for replacement pixel intensity
*         3/4         finish
*
*      On a terminal keyboard these are the actual ASCII keys.
*      If the data and/or quality changes after a session, the file is
*      marked as requiring saving on disk.
*
*      The assumption is made that the current image displayed corresponds
*      to the current dataset!
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NAXIS1        ! size of axis 1
      INTEGER NAXIS2        ! size of axis 2

*   Import-Export:
      INTEGER*2 DATA(NAXIS1, NAXIS2) ! Data Array

      BYTE QUAL(NAXIS1, NAXIS2)      ! data quality

*   Export:
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

      INTEGER DQ_AND        ! data quality AND

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMDATA'

*   Local constants:
      INTEGER COPY          ! pixel FN COPY index
      INTEGER CURMOD        ! GKS cursor mode (default 0)
      INTEGER SET           ! pixel FN SET index
      PARAMETER (COPY = 1, CURMOD = 0, SET = 2)

*   Local variables:
      REAL*8 FN             ! hit pixel FN
      REAL*8 FN1            ! pixel FN for first cursor hit
      REAL*8 FN2            ! pixel FN for second cursor hit
      REAL*8 X1             ! hit x1-coordinate
      REAL*8 X2             ! hit x2-coordinate
      REAL*8 Y1             ! hit y1-coordinate
      REAL*8 Y2             ! hit y2-coordinate

      REAL   XCUR           ! cursor position
      REAL   YCUR           ! cursor position

      INTEGER ACTVAL        ! parameter value count
      INTEGER HIT           ! hit index
      INTEGER IDN           ! image pixel DN
      INTEGER IQ            ! temporary data quality
      INTEGER IQ1           ! data quality for first cursor hit
      INTEGER IQ2           ! data quality for second cursor hit
      INTEGER IXC           ! X-axis hit pixel
      INTEGER IX1           ! X-axis pixel for first cursor hit
      INTEGER IX2           ! X-axis pixel for second cursor hit
      INTEGER IYC           ! Y-axis hit pixel
      INTEGER IY1           ! Y-axis pixel for first cursor hit
      INTEGER IY2           ! Y-axis pixel for second cursor hit
      INTEGER NTOTAL        ! number found to change (total)

      LOGICAL DEF           ! whether FN is defaulted

*.

*   Open zone cursor
      CALL GRF_OPCURS( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics cursor unavailable\\', STATUS )
         GO TO 999
      END IF

*   Check that the axis label makes sense
      IF ( .NOT.STR_SIMLR( 'S\\', XLAB ) .OR.
     :     .NOT.STR_SIMLR( 'L\\', YLAB ) ) THEN
         CALL ERROUT( 'Error: incorrect display type\\', STATUS )
         GO TO 999
      END IF

*   Cursor loop
      NTOTAL = 0

*   Initailise XCUR, YCUR
      XCUR = 0.0
      YCUR = 0.0

      DO WHILE ( .TRUE. )

         CALL GRF_CUZONE( '12', CURMOD, HIT, XCUR, YCUR, STATUS )
         X1 = DBLE(XCUR)
         Y1 = DBLE(YCUR)
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )
            GO TO 200

         ELSE IF ( HIT .LE. 0 ) THEN
            GO TO 200

         ELSE IF ( HIT .EQ. SET ) THEN
            IXC = NINT(REAL(X1))
            IYC = NINT(REAL(Y1))
            CALL LINE_WRITI( '%p Modify pixel at (%i,\\', IXC )
            CALL LINE_WRITI( '%i)\\', IYC )

            IF ( IXC.LT.1 .OR. IXC.GT.NAXIS1 .OR.
     :           IYC.LT.1 .OR. IYC.GT.NAXIS2 ) THEN
               CALL PRTBUF( STATUS )
               CALL ERROUT( 'Error: pixel outside image\\', STATUS )

            ELSE
               CALL DQ_UTOI( QUAL(IXC, IYC), IQ )

               IF ( DATA(IXC, IYC).EQ.DBLANK .OR.
     :              DQ_AND(IQ, 1).NE.0 ) THEN
                  DEF = .FALSE.
                  CALL LINE_WCONT( '  currently blank.\\' )

               ELSE
                  FN = DATA(IXC, IYC) * DSCALE + DZERO
                  DEF = .TRUE.
                  CALL LINE_WRITF( '  current intensity %f\\', FN )
               END IF

               CALL PRTBUF( STATUS )
               CALL CNPAR( 'FN\\', STATUS )
               CALL RDPARF( 'FN\\', DEF, 1, FN, ACTVAL, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'FN\\', STATUS )
                  STATUS = SAI__OK

               ELSE
                  IDN = (FN - DZERO) / DSCALE
                  IF ( IDN .EQ. DATA(IXC, IYC) ) THEN
                     CALL LINE_WCONT( '%p Intensity unchanged.\\' )
                     CALL PRTBUF( STATUS )

                  ELSE IF ( IDN.GT.32767 .OR. IDN.LT.-32767 ) THEN
                     CALL ERRPAR( 'FN\\' )
                     CALL ERROUT( ': parameter outside range\\',
     :                            STATUS )

                  ELSE
                     CALL LINE_WRITF( '%p Intensity changed to %f\\',
     :                                FN )
                     CALL PRTBUF( STATUS )
                     DATA(IXC, IYC) = IDN
                     QUAL(IXC, IYC) = 0
                     NTOTAL = NTOTAL + 1
                  END IF
               END IF
            END IF

         ELSE IF ( HIT .EQ. COPY ) THEN
            IX1 = NINT(REAL(X1))
            IY1 = NINT(REAL(Y1))
            CALL LINE_WRITI( '%p Copy pixel (%i,\\', IX1 )
            CALL LINE_WRITI( '%i)\\', IY1 )

            IF ( IX1.LT.1 .OR. IX1.GT.NAXIS1 .OR.
     :           IY1.LT.1 .OR. IY1.GT.NAXIS2 ) THEN
               CALL PRTBUF( STATUS )
               CALL ERROUT( 'Error: pixel outside image\\', STATUS )

            ELSE
               CALL DQ_UTOI( QUAL(IX1, IY1), IQ1 )
               IF ( DATA(IX1, IY1).EQ.DBLANK .OR.
     :              DQ_AND(IQ1, 1).NE.0 ) THEN
                  CALL LINE_WCONT( '  currently blank.\\' )
                  CALL PRTBUF( STATUS )
                  CALL ERROUT( 'Error: cannot do that\\', STATUS )

               ELSE
                  FN1 = REAL(DATA(IX1, IY1)) *DSCALE + DZERO
                  CALL LINE_WRITF( '  current intensity %f\\', FN1 )
                  CALL PRTBUF( STATUS )
                  CALL GRF_CUZONE( '12', CURMOD, HIT, XCUR, YCUR,
     :                             STATUS )
                  X2 = DBLE(XCUR)
                  Y2 = DBLE(YCUR)
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERROUT( 'Error: reading cursor\\', STATUS )
                     GO TO 200

                  ELSE IF ( HIT .LE. 0 ) THEN
                     GO TO 200

                  ELSE IF ( HIT .NE. SET ) THEN
                     CALL ERROUT( 'Error: unexpected cursor hit\\',
     :                            STATUS )

                  ELSE
                     IX2 = NINT(REAL(X2))
                     IY2 = NINT(REAL(Y2))
                     CALL LINE_WRITI(
     :                        '%p Use to modify pixel at (%i,\\', IX2 )
                     CALL LINE_WRITI( '%i)\\', IY2 )
                     CALL DQ_UTOI( QUAL(IX2, IY2), IQ2 )
                     IF ( DATA(IX2, IY2).EQ.DBLANK .OR.
     :                    DQ_AND(IQ2, 1).NE.0 ) THEN
                        CALL LINE_WCONT( '  currently blank.\\' )

                     ELSE
                        FN2 = DBLE(DATA(IX2, IY2)) * DSCALE + DZERO
                        CALL LINE_WRITF( '  current intensity %f\\',
     :                                   FN2 )
                     END IF

                     CALL PRTBUF( STATUS )
                     DATA(IX2, IY2) = DATA(IX1, IY1)
                     QUAL(IX2, IY2) = QUAL(IX1, IY1)
                     NTOTAL = NTOTAL + 1
                  END IF
               END IF
            END IF

         ELSE
            CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )
            IF ( HIT.NE.COPY .AND. HIT.NE.SET ) GO TO 200
         END IF
      END DO
 200  CONTINUE

*   If status is good, see if image data HAS changed
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NTOTAL .EQ. 0 ) THEN
            CALL LINE_WCONT( '%p Image Data not changed.\\' )
         CALL PRTBUF( STATUS )

         ELSE
            CALL LINE_WRITI( '%p Image Data changed (%i pixels).\\',
     :                       NTOTAL )
            CALL PRTBUF( STATUS )
            CALL MODUED
            CALL MODUEQ
         END IF
      END IF

 999  CONTINUE
      END
