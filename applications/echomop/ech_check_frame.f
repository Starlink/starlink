      SUBROUTINE ECH_CHECK_FRAME(
     :           INIM,
     :           TRACIM,
     :           NX,
     :           NY,
     :           FRAME_CHECK,
     :           NBAD_ROWS,
     :           BAD_ROWS,
     :           NBAD_COLS,
     :           BAD_COLS,
     :           SATURATION,
     :           ARCS,
     :           NREF_FRAME,
     :           SAVE_NX,
     :           SAVE_NY,
     :           QUALITY1,
     :           QUALITY2
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CHECK_FRAME

*  Purpose:
*     Check object/trace frames for bad rows/cols/pixels.

*  Description:
*     This module checks the object and trace frame (if different)
*     for a variety of problem pixels.
*     Bad rows and columns are identified by having constant pixel
*     DN's over a large number of consecutive pixels.
*     Individual pixels may be flagged as saturated if their DN
*     exceeds a tunable threshold.
*     Bad rows and columns may also be read from lists provided by
*     the user/input dataset header.
*     A check is also made that the ARC frame names are specified
*     consistently.

*  Invocation:
*      CALL ECH_CHECK_FRAME(
*     :     INIM,
*     :     TRACIM,
*     :     NX,
*     :     NY,
*     :     FRAME_CHECK,
*     :     NBAD_ROWS,
*     :     BAD_ROWS,
*     :     NBAD_COLS,
*     :     BAD_COLS,
*     :     SATURATION,
*     :     ARCS,
*     :     NREF_FRAME,
*     :     SAVE_NX,
*     :     SAVE_NY,
*     :     QUALITY1,
*     :     QUALITY2
*     :    )

*  Arguments:
*     INIM = REAL (Given)
*        Input image data frame.
*     TRACIM = REAL (Given)
*        Trace image data frame.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     FRAME_CHECK = LOGICAL (Given)
*        Set TRUE if frame checking is enabled (row,cols etc).
*     NBAD_ROWS = INTEGER (Given)
*        Number of bad rows specified.
*     BAD_ROWS = INTEGER (Given)
*        Array of bad row numbers.
*     NBAD_COLS = INTEGER (Given)
*        Number of bad columns specified.
*     BAD_COLS = INTEGER (Given)
*        Array of bad column numbers.
*     SATURATION = REAL (Given)
*        Upper limit for unsaturated pixels.
*     ARCS = CHAR (Given and Returned)
*        Name(s) of arc lamp frame(s).
*     NREF_FRAME = INTEGER (Given and Returned)
*        Number of arc lamp frames specified.
*     SAVE_NX = INTEGER (Given and Returned)
*        Number of x pixels in image.
*     SAVE_NY = INTEGER (Given and Returned)
*        Number of y pixels in image.
*     QUALITY1 = BYTE (Given and Returned)
*        Data quality array for INPTIM object image.
*     QUALITY2 = BYTE (Given and Returned)
*        Data quality array from TRACIM input image.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}


*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     07-JUN-1996 (MJC):
*       New prologue.  Fixed infinite loop on bad column/row detection.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_QUALITIES.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL INIM( NX, NY )
      REAL TRACIM( NX, NY )
      INTEGER NBAD_ROWS
      INTEGER NBAD_COLS
      INTEGER BAD_ROWS( NBAD_ROWS )
      INTEGER BAD_COLS( NBAD_COLS )
      REAL SATURATION
      LOGICAL FRAME_CHECK

*  Arguments Returned:
      INTEGER SAVE_NX
      INTEGER SAVE_NY
      BYTE QUALITY1( NX, NY )
      BYTE QUALITY2( NX, NY )
      INTEGER NREF_FRAME
      CHARACTER*( * ) ARCS

*  Local variables:
      INTEGER MAX
      REAL THRESH

      INTEGER I
      INTEGER II
      INTEGER ISTAT
      INTEGER BCLIP
      INTEGER SCLIP
      INTEGER TBCLIP
      INTEGER TSCLIP
      INTEGER COUNT
      INTEGER LPTR
      INTEGER FNPTR
      INTEGER LATEST
      INTEGER AT_PIXEL
      INTEGER MAX_COUNT
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      LOGICAL MORE
      LOGICAL CLIPPED

      CHARACTER*80 ARCS_PAR
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3

*  Functions called:
      INTEGER ICH_DELIM,ICH_VERIF
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      SAVE_NX = NX
      SAVE_NY = NY

*  25 percent consecutive pixels must be same value.
      THRESH = 25

      CALL CHR_ITOC( NX, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Data frame X-dimension is ' //
     :      REF_STR1( :NCHAR1 ) // ' pixels.'
      CALL ECH_REPORT( 0, REPORT_STRING )
      CALL CHR_ITOC( NY, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Data frame Y-dimension is ' //
     :      REF_STR1( :NCHAR1 ) // ' pixels.'
      CALL ECH_REPORT( 0, REPORT_STRING )

*  Process bad row list.
      IF ( NBAD_ROWS .GT. 0 .AND. BAD_ROWS( 1 ) .GT. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Bad row list has been supplied: processing.' )
         DO II = 1, NBAD_ROWS
            CALL CHR_ITOC( BAD_ROWS( II ), REF_STR1, NCHAR1 )
            IF ( BAD_ROWS( II ) .GT. 0 .AND. BAD_ROWS( II ) .LE. NY )
     :           THEN
               REPORT_STRING = ' Flagging bad row of pixels at Y=' //
     :               REF_STR1( :NCHAR1 ) // '.'
               DO I = 1, NX
                  QUALITY1( I, BAD_ROWS( II ) ) = QTY_BAD_ROW
                  QUALITY2( I, BAD_ROWS( II ) ) = QTY_BAD_ROW
               END DO

            ELSE
               REPORT_STRING = ' Illegal bad row number= ' //
     :               REF_STR1( :NCHAR1 ) // '.'
            END IF
            CALL ECH_REPORT( 0, REPORT_STRING )
         END DO
      END IF

*  Process bad column list.
      IF ( NBAD_COLS .GT. 0 .AND. BAD_COLS( 1 ) .GT. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' Bad column list has been supplied: processing.' )
         DO II = 1, NBAD_COLS
            CALL CHR_ITOC( BAD_COLS( II ), REF_STR1, NCHAR1 )
            IF ( BAD_COLS( II ) .GT. 0 .AND. BAD_COLS( II ) .LE. NX )
     :           THEN
               REPORT_STRING = ' Flagging bad column of pixels at X=' //
     :               REF_STR1( :NCHAR1 ) // '.'
               DO I = 1, NY
                  QUALITY1( BAD_COLS( II ), I ) = QTY_BAD_COLUMN
                  QUALITY2( BAD_COLS( II ), I ) = QTY_BAD_COLUMN
               END DO

            ELSE
               REPORT_STRING = ' Illegal bad column number= ' //
     :               REF_STR1( :NCHAR1 ) // '.'
            END IF
            CALL ECH_REPORT( 0, REPORT_STRING )
         END DO
      END IF

      IF ( FRAME_CHECK ) THEN

*     Check for non-user-specified bad rows.
         DO II = 1, NY
            LATEST = INT( INIM( 1, II ) )
            CLIPPED = .TRUE.
            DO WHILE ( CLIPPED )
               CLIPPED = .FALSE.
               COUNT = 0
               MAX_COUNT = 0
               DO I = 2, NX
                  IF ( INIM( I, II ) .GT. 0.0 .AND.
     :                 INT( INIM( I, II ) ) .EQ. LATEST .AND.
     :                 QUALITY1( I, II ) .EQ. 0 ) THEN
                     COUNT = COUNT + 1

                  ELSE
                     COUNT = 0
                     LATEST = INT( INIM( I, II ) )
                  END IF
                  IF ( COUNT .GT. MAX_COUNT ) THEN
                     MAX_COUNT = COUNT
                     AT_PIXEL = I - COUNT
                  END IF
               END DO

               IF ( MAX_COUNT .GT. THRESH * NX / 100 ) THEN
                  CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
                  CALL CHR_ITOC( AT_PIXEL, REF_STR2, NCHAR2 )
                  CALL CHR_ITOC( MAX_COUNT, REF_STR3, NCHAR3 )
                  REPORT_STRING = ' Clipping bad row ' //
     :                  REF_STR1( :NCHAR1 ) // ' from pixel ' //
     :                  REF_STR2( :NCHAR2 ) // ' for ' //
     :                  REF_STR3( :NCHAR3 ) // ' pixels.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  CLIPPED = .TRUE.
                  DO I = AT_PIXEL, AT_PIXEL + MAX_COUNT
                     QUALITY1( I, II ) = QTY_BAD_ROW
                     QUALITY2( I, II ) = QTY_BAD_ROW
                  END DO
               END IF
            END DO
         END DO

*     Check for bad columns, bad values and saturated pixels.
         IF ( SATURATION .LE. 0.0 ) SATURATION = 1.0E20
         SCLIP = 0
         BCLIP = 0
         TSCLIP = 0
         TBCLIP = 0
         DO I = 1, NX
            IF ( INIM( I, 1 ) .EQ. ECH__BAD_REAL ) THEN
               BCLIP = BCLIP + 1
               QUALITY1( I, 1 ) = QTY_NEG_PIXEL

            ELSE IF ( INIM( I, 1 ) .GE. SATURATION ) THEN
               SCLIP = SCLIP + 1
               QUALITY1( I, 1 ) = QTY_SATURATED
            END IF
            IF ( TRACIM( I, 1 ) .EQ. ECH__BAD_REAL ) THEN
               TBCLIP = TBCLIP + 1
               QUALITY2( I, 1 ) = QTY_NEG_PIXEL

            ELSE IF ( TRACIM( I, 1 ) .GE. SATURATION ) THEN
               TSCLIP = TSCLIP + 1
               QUALITY2( I, 1 ) = QTY_SATURATED
            END IF
            LATEST = INT( INIM( I, 1 ) )
            CLIPPED = .TRUE.
            DO WHILE ( CLIPPED )
               CLIPPED = .FALSE.
               COUNT = 0
               MAX_COUNT = 0
               DO II = 2, NY
                  IF ( INIM( I, II ) .EQ. ECH__BAD_REAL ) THEN
                     BCLIP = BCLIP + 1
                     QUALITY1( I, II ) = QTY_NEG_PIXEL

                  ELSE IF ( INIM( I, II ) .GE. SATURATION ) THEN
                     SCLIP = SCLIP + 1
                     QUALITY1( I, II ) = QTY_SATURATED
                  END IF
                  IF ( TRACIM( I, II ) .EQ. ECH__BAD_REAL ) THEN
                     TBCLIP = TBCLIP + 1
                     QUALITY2( I, II ) = QTY_NEG_PIXEL

                  ELSE IF ( TRACIM( I, II ) .GE. SATURATION ) THEN
                     TSCLIP = TSCLIP + 1
                     QUALITY2( I, II ) = QTY_SATURATED
                  END IF
                  IF ( INIM( I, II ) .GT. 0.0 .AND.
     :                 INT( INIM( I, II ) ) .EQ. LATEST .AND.
     :                 QUALITY1( I, II ) .EQ. 0 ) THEN
                     COUNT = COUNT + 1

                  ELSE
                     COUNT = 0
                     LATEST = INT( INIM( I, II ) )
                  END IF
                  IF ( COUNT .GT. MAX_COUNT ) THEN
                     MAX_COUNT = COUNT
                     AT_PIXEL = I - COUNT
                  END IF
               END DO

               IF ( MAX_COUNT .GT. THRESH * NX / 100 ) THEN
                  CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
                  CALL CHR_ITOC( AT_PIXEL, REF_STR2, NCHAR2 )
                  CALL CHR_ITOC( MAX_COUNT, REF_STR3, NCHAR3 )
                  REPORT_STRING = ' Clipping bad column ' //
     :                  REF_STR1( :NCHAR1 ) // ' from pixel ' //
     :                  REF_STR2( :NCHAR2 ) // ' for ' //
     :                  REF_STR3( :NCHAR3 ) // ' pixels.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  CLIPPED = .TRUE.
                  DO II = AT_PIXEL, AT_PIXEL + MAX_COUNT
                     QUALITY1( I, II ) = QTY_BAD_COLUMN
                     QUALITY2( I, II ) = QTY_BAD_COLUMN
                  END DO
               END IF
            END DO
         END DO

*     Report results of clipping...
*     Saturation in the input frame.
         CALL CHR_RTOC( SATURATION, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Saturation level assumed to be ' //
     :         REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         IF ( SCLIP .GT. 0 ) THEN
            CALL CHR_ITOC( SCLIP, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Total of ' // REF_STR1( :NCHAR1 ) //
     :            ' saturated pixels flagged in INPTIM.'

         ELSE
            REPORT_STRING = ' No saturated pixels found in input frame.'
         END IF
         CALL ECH_REPORT( 0, REPORT_STRING )

*     BAD pixels in the input frame.
         IF ( BCLIP .GT. 0 ) THEN
            CALL CHR_ITOC( BCLIP, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Total of ' // REF_STR1( :NCHAR1 ) //
     :            ' BAD pixels found in INPTIM.'

         ELSE
            REPORT_STRING = ' No BAD pixels found in input frame.'
         END IF
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Saturation in the trace frame.
         IF ( TSCLIP .GT. 0 ) THEN
            CALL CHR_ITOC( TSCLIP, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Total of ' // REF_STR1( :NCHAR1 ) //
     :            ' saturated pixels flagged in TRACIM.'

         ELSE
            REPORT_STRING = ' No saturated pixels found in trace frame.'
         END IF
         CALL ECH_REPORT( 0, REPORT_STRING )

*     BAD pixels in the trace frame.
         IF ( TBCLIP .GT. 0 ) THEN
            CALL CHR_ITOC( TBCLIP, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Total of ' // REF_STR1( :NCHAR1 ) //
     :            ' BAD pixels found in TRACIM.'

         ELSE
            REPORT_STRING = ' No BAD pixels found in trace frame.'
         END IF
         CALL ECH_REPORT( 0, REPORT_STRING )

      ELSE
         CALL ECH_REPORT ( 0,
     :        ' Frame checking for bad row/columns is disabled.' )
         CALL ECH_REPORT ( 0,
     :        ' Set parameter TUNE_FCHECK=YES to enable.' )
      END IF

      ARCS_PAR = 'ARC'
      CALL ECH_GET_PARAMETER( ARCS_PAR, 'CHAR', 0.0, .FALSE., ARCS, 0,
     :     ISTAT )
      CALL ECH_SETUP_OBJECT_REF( ARCS_PAR, 0, 0, 0, ARCS, 0.0, .FALSE.,
     :     ECH__IMAGE_LIST )
      NREF_FRAME = 1
      MORE = ARCS .NE. ' ' .AND. ARCS .NE. 'NONE' .AND. ARCS .NE. 'none'
      DO WHILE ( MORE )

*     Look for the name of an arc type in ARCS.
         LPTR = ICH_DELIM( ARCS, FNPTR, ',' ) - 1
         IF ( LPTR .LE. 0 ) LPTR = LEN( ARCS )

*     Position on next file name in ARCS.
         IF ( LPTR .GE. LEN( ARCS ) ) THEN
            MORE = .FALSE.

         ELSE
            FNPTR = ICH_VERIF( ARCS, LPTR + 1, ', ' )
            MORE = FNPTR .NE. 0
         END IF
         IF ( MORE ) THEN
            NREF_FRAME = NREF_FRAME + 1
            IF ( NREF_FRAME .GT. 3 ) THEN
               CALL ECH_REPORT( 0,
     :              ' Too many arc frames specified, some ignored.' )
               MORE = .FALSE.
            END IF
         END IF
      END DO
      NREF_FRAME = MAX( 1, NREF_FRAME )

      END
