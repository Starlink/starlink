      SUBROUTINE ECH_DECOS_SKYLINE( NX, NY, IYBOX, IMAGE, QUALITY,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_SKYLINE

*  Purpose:
*     Replaces sky-line pixels erroneously flagged as cosmic rays.

*  Description:
*     This routine checks all pixels which have been flagged as cosmic
*     rays.  It looks for long row or column runs of bad pixels and may
*     optionally replace them if they are suspected as due to a bright
*     sky line instead.  Pixels may also be rejected if they are
*     brighter than a rejected neighbour pixel, or brighter than a user-
*     defined threshold value.

*  Invocation:
*      CALL ECH_DECOS_SKYLINE( NX, NY, IYBOX, IMAGE, QUALITY, STATUS )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IYBOX = INTEGER (Given)
*        Width of median filter applied in y direction.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx.
*     QUALITY = BYTE (Given and Returned)
*        Quality flags per pixel.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Loop through image restoring sky lines
*     Loop through columns
*      Loop through lines
*         If pixel flagged as cosmic ray then
*            Check subsequent pixels
*            If set of flagged pixels longer than twice order size then
*              Restore them
*            Endif
*         Endif
*      End loop
*     End loop
*     Loop through image rows
*      Loop through image columns
*         If pixel flagged as cosmic ray then
*            Check subsequent pixels for flags
*            If set of flagged pixels longer than twice order size then
*              Restore them
*            Endif
*         Endif
*      End loop
*     End loop
*     Report sets of connected flagged pixels
*     Count currently flagged pixels
*     Calculate median cosmic ray pixel intensity
*     Flag any pixel brighter than median value if they have flagged neighbours
*     Report number of flagged pixels now
*     Loop through image restoring sky lines (again!)
*     Repeat long flagged sequence resoration (as above)
*     Report number of restored pixels
*     Loop until no more pixels are restorable
*       Restore any pixels which are fainter than an unflagged neighbour
*     End loop
*     Report results

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_QUALITIES.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER IYBOX
      REAL IMAGE( NX, NY )

*  Arguments Given and Returned:
      BYTE QUALITY( NX, NY )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAX_COSMICS
      PARAMETER ( MAX_COSMICS = 2048 )

*  Local Variables:
      REAL COSMICS( MAX_COSMICS )
      REAL MEDIAN

      INTEGER YARRAY( 5000 )
      INTEGER CONNECTED( 0:200 )
      INTEGER I
      INTEGER IY
      INTEGER IX
      INTEGER IIY
      INTEGER IIX
      INTEGER II
      INTEGER NCR,IREPLACE
      INTEGER ORDER_SIZE
      INTEGER RESTORED
      LOGICAL FOUND
      INTEGER QRAY
      INTEGER IQUALITY
      INTEGER NCOSMIC
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Loop through image restoring sky lines.
      ORDER_SIZE = IYBOX
      RESTORED = 0

*  Loop through columns.
      DO II  = 1, NX
         DO I = 1, NY
            YARRAY( I ) = QUALITY( II, I )
         END DO
         I = 1

*     Loop through lines.
         DO WHILE ( I .LE. NY )
            NCR = 0

*        If pixel flagged as cosmic ray then.
            IF ( IAND( YARRAY( I ), QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY ) THEN

*           Check subsequent pixels.
               NCR = 1
               DO WHILE ( IAND( YARRAY( I+NCR ), QTY_COSMIC_RAY ) .EQ.
     :                    QTY_COSMIC_RAY .AND. I + NCR .LE. NY )
                  NCR = NCR + 1
               END DO

*           If set of flagged pixels longer than twice order size then.
               IF ( NCR .LT. 2 * ORDER_SIZE ) THEN
                  IF ( NCR .GE. MAX ( ORDER_SIZE / 2, 4 ) ) THEN
                     DO IREPLACE = I, I + NCR - 1
                        RESTORED = RESTORED + 1
                        QUALITY( II, IREPLACE ) =
     :                          QUALITY( II, IREPLACE ) - QTY_COSMIC_RAY
                     END DO
                  END IF
                  IF ( NCR .LE. 200 ) THEN
                     CONNECTED( NCR ) = CONNECTED( NCR ) + 1
                  END IF

               ELSE
                  CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
                  CALL CHR_ITOC( NCR, REF_STR2, NCHAR2 )
                  REPORT_STRING = ' Suspected bad column at ' //
     :                  REF_STR1( :NCHAR1 ) // ', no. pixels: ' //
     :                  REF_STR2( :NCHAR2 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
               I = I + NCR

            ELSE
               I = I + 1
            END IF
         END DO
      END DO

*  Loop through image rows.
      DO II  = 1, NY
         DO I = 1, NX
            YARRAY( I ) = QUALITY( I, II )
         END DO
         I = 1

*     Loop through image columns.
         DO WHILE ( I .LE. NX )
            NCR = 0

*        If pixel flagged as cosmic ray then.
            IF ( IAND( YARRAY( I ), QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY ) THEN
               IF ( IMAGE( I, II ) .LE. 30. ) THEN
                  QUALITY( I, II ) = QUALITY( I, II ) - QTY_COSMIC_RAY
                  I = I + 1

               ELSE

*              Check subsequent pixels for flags.
                  NCR = 1
                  DO WHILE ( IAND( YARRAY( I + NCR ), QTY_COSMIC_RAY )
     :                       .EQ. QTY_COSMIC_RAY .AND. I + NCR .LE. NX )
                     ncr = ncr + 1
                  END DO

*              If set of flagged pixels longer than twice order size then.
                  IF ( NCR .GE. 2 * ORDER_SIZE ) THEN
                     CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
                     CALL CHR_ITOC( NCR, REF_STR2, NCHAR2 )
                     REPORT_STRING = ' Suspected bad row at ' //
     :                     REF_STR1( :NCHAR1 ) // ', no. pixels: ' //
     :                     REF_STR2( :NCHAR2 ) // '.'
                     CALL ECH_REPORT( 0, REPORT_STRING )

*                 Restore them.
                     DO IREPLACE = I, I + NCR - 1
                        RESTORED = RESTORED + 1
                        QUALITY( IREPLACE, II ) =
     :                          QUALITY( IREPLACE, II ) - QTY_COSMIC_RAY
                     END DO
                  END IF
                  IF ( NCR .LE. 200 )
     :               CONNECTED( NCR ) = CONNECTED( NCR ) + 1
                     I = I + NCR
                  END IF

               ELSE
                  I = I + 1
               END IF
            END DO
         END DO

*     Report sets of connected flagged pixels.
         CALL CHR_ITOC( RESTORED, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Restored ' // REF_STR1( :NCHAR1 ) //
     :         ' skyline/object pixels.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL CHR_ITOC( CONNECTED( 1 ), REF_STR2, NCHAR2 )
         REPORT_STRING = ' Number of connected CRs (1 pixel) =' //
     :         REF_STR2( :NCHAR2 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         DO i = 2, 20
            CALL CHR_ITOC( I, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( CONNECTED( I ), REF_STR2, NCHAR2 )
            REPORT_STRING = ' Number of connected CRs (' //
     :            REF_STR1( :NCHAR1 ) // ' pixels) =' //
     :            REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END DO

*     Count currently flagged pixels.
         NCOSMIC = 0
         DO IY = 1, NY
            DO IX = 1, NX
               IQUALITY = QUALITY( IX, IY )
               IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :              QTY_COSMIC_RAY ) THEN
                  IF ( NCOSMIC .LT. MAX_COSMICS ) THEN
                     NCOSMIC = NCOSMIC + 1
                     COSMICS( NCOSMIC ) = IMAGE( IX, IY )

                  ELSE
                     GO TO 100
                  END IF
               END IF
            END DO
         END DO
  100    CONTINUE

*     Find median cosmic ray pixel intensity.
         IF ( NCOSMIC .GT. 0 ) THEN
            CALL ECH_MEAN_MEDIAN( NCOSMIC, COSMICS, .TRUE., MEDIAN,
     :           .FALSE., STATUS )
         END IF
         CALL CHR_RTOC( MEDIAN, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Median Cosmic-ray count per pixel: ' //
     :         REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Flag any pixel brighter than median value if they have flagged
*     neighbours.
         NCOSMIC = 0
         DO IY = 3, NY - 2
            DO IX = 3, NX - 2
            IQUALITY = QUALITY( IX, IY )
            IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY ) THEN
               DO IIX = -2, 2
                  DO IIY = -2, 2
                     IF ( IIX + IIY .NE. 0 ) THEN
                        IQUALITY = QUALITY( IX + IIX, IY + IIY )
                        IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :                       0 ) THEN
                           IF ( IMAGE( IX+IIX,IY+IIY ) .GT. MEDIAN .AND.
     :                          IMAGE( IX+IIX,IY+IIY ) .GT.
     :                          2.0 * IMAGE( IX, IY ) ) THEN
                              QUALITY( IX + IIX, IY + IIY ) =
     :                                   QUALITY( IX + IIX, IY + IIY ) +
     :                                   QTY_COSMIC_RAY
                              NCOSMIC = NCOSMIC + 1
                           END IF
                        END IF
                     END IF
                  END DO
               END DO
            END IF
         END DO
      END DO

*  Report current number of flagged pixels.
      CALL CHR_ITOC( NCOSMIC, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Removed another ' // REF_STR1( :NCHAR1 ) //
     :       ' additional suspect pixels.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Loop through image restoring sky lines (again!).
*  Repeat long flagged sequence resoration (as above).
      RESTORED = 0

*  Loop through columns.
      DO II  = 1, NX
         DO I = 1, NY
            YARRAY( I ) = QUALITY( II, I )
         END DO
         I = 1

*     Loop through lines.
         DO WHILE ( I .LE. NY )
            NCR = 0

*        If pixel flagged as cosmic ray then.
            IF ( IAND( YARRAY( I ), QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY ) THEN
               NCR = 1

*           Check subsequent pixels.
               DO WHILE ( IAND( YARRAY( I+NCR ), QTY_COSMIC_RAY ) .EQ.
     :                    QTY_COSMIC_RAY .AND. I + NCR .LE. NY )
                  NCR = NCR + 1
               END DO

*           If set of flagged pixels longer than twice order size then.
               IF ( NCR .LT. 2 * ORDER_SIZE ) THEN
                  IF ( NCR .GE. MAX( ORDER_SIZE / 2, 4 ) ) THEN
                     DO IREPLACE = I, I + NCR - 1
                        RESTORED = RESTORED + 1
                        QUALITY( II, IREPLACE ) =
     :                          QUALITY( II, IREPLACE ) - QTY_COSMIC_RAY
                     END DO
                  END IF
               END IF
               I = I + NCR

            ELSE
               I = I + 1
            END IF
         END DO
      END DO

*  Report number of restored pixels.
      CALL CHR_ITOC( RESTORED, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Restored ' // REF_STR1( :NCHAR1 ) //
     :      ' skyline/object pixels.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Loop until no more pixels are restorable.
*  Restore any pixels which are fainter than an unflagged neighbour.
      FOUND = .TRUE.
      DO WHILE ( FOUND )
         FOUND = .FALSE.
         NCR = 0
         RESTORED = 0
         DO II  = 1, NY
            DO I = 1, NX
               YARRAY( I ) = QUALITY( I, II )
            END DO
            I = 1
            DO WHILE ( I .LE. NX )
               IF ( IAND( YARRAY( I ), QTY_COSMIC_RAY ) .EQ.
     :              QTY_COSMIC_RAY ) THEN
                  DO IIX = -3, 3
                     DO IIY = -MAX( 3, ORDER_SIZE / 4 ),
     :                         MAX( 3, ORDER_SIZE / 4 )
                        IF ( IIX .NE. 0 .OR. IIY .NE. 0 ) THEN
                           IF ( I + IIX .GT. 0 .AND. I + IIX .LE. NX
     :                          .AND. II + IIY .GT. 0 .AND. II + IIY
     :                          .LE. NY ) THEN
                              QRAY = QUALITY( I + IIX, II + IIY )
                              IF ( IAND( QRAY, QTY_COSMIC_RAY ) .EQ. 0
     :                             .AND. IMAGE( I+IIX, II+IIY ) .GE.
     :                             IMAGE( I, II ) ) THEN
                                 QUALITY( I, II ) =
     :                                 QUALITY( I, II ) - QTY_COSMIC_RAY
                                 FOUND = .TRUE.
                                 RESTORED = RESTORED + 1
                                 GOTO 1
                              END IF
                           END IF
                        END IF
                     END DO
                  END DO
                  NCR = NCR + 1
  1               I =  I + 1

               ELSE
                  I = I + 1
               END IF
            END DO
         END DO

*     Report results.
         CALL CHR_ITOC( RESTORED, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Restored ' // REF_STR1( :NCHAR1 ) //
     :         ' skyline/object pixels.'
         CALL ECH_REPORT( 0, REPORT_STRING )
      END DO

      CALL CHR_ITOC( NCR, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Final count of cosmic-ray contaminated' //
     :      ' pixels: ' // REF_STR1( :NCHAR1 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

      END
