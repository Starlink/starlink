      SUBROUTINE ECH_DECOSMIC_1(
     :           IMAGE,
     :           NX,
     :           NY,
     :           IXBOX,
     :           IYBOX,
     :           CR_MINTEN,
     :           CRCLEAN,
     :           MAX_CR,
     :           INTERPOLATE,
     :           IMAGE2,
     :           IMAGE3,
     :           QUALITY,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOSMIC_1

*  Purpose:
*     Allows user-specified cosmic ray clipping.

*  Description:
*     This routine applies two median filters (user sizable) to the input
*     object image. It then divedes the filtered version into the original
*     and histograms the result.  This histogram is then displayed and
*     the user is allowed to select a clip point.  After clipping the
*     rejected points are re-examined for possible sky lines.

*  Invocation:
*     CALL ECH_DECOSMIC_1(
*     :    IMAGE,
*     :    NX,
*     :    NY,
*     :    IXBOX,
*     :    IYBOX,
*     :    CR_MINTEN,
*     :    CRCLEAN,
*     :    MAX_CR,
*     :    INTERPOLATE,
*     :    IMAGE2,
*     :    IMAGE3,
*     :    QUALITY,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE2 = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE3 = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     MAX_CR = INTEGER (Given)
*        Maximum CR pixels wanted.
*     CR_MINTEN = REAL (Given)
*        Minimum intensity of CR pixels.
*     CRCLEAN = LOGICAL (Given)
*        TRUE if cleaning required.
*     INTERPOLATE = LOGICAL (Given)
*        TRUE if image interpolation required.
*     QUALITY = BYTE (Given and Returned)
*        Quality flags per pixel.
*     IXBOX = INTEGER (Given)
*        Width of median filter applied in x direction.
*     IYBOX = INTEGER (Given)
*        Width of median filter applied in y direction.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If cosmic ray cleaning enabled (TUNE_TRCRC=YES) then
*      Remove any previous flagged cosmic rays
*      Zero histogram
*      Calculate x and y median images
*      If minimum cosmic ray intensity not specified then
*        Determine min intensity (as mean of +ve pixels)
*      Else
*        Set min intensity to value supplied
*      Endif
*      Divide original image by median filtered images
*      Histogram resulting image
*      If no value of TUNE_MAXCR supplied then
*        Loop accepting cursor/keyoard input from user
*           If 'S' key was pressed last then re-calc histogram and re-plot
*           Get cursor/key from user
*           If 'S' key pressed then
*              Get new upper bin limit from user
*              Re-calculate histogram
*           Else if 'Q' key pressed then exit loop
*           Else if 'A' key pressed then exit loop
*           Else report percentage of pixels corresponding to cursor x-position
*           Endif
*        End loop
*      Endif
*      If last key press was 'A' then
*      Automatically clip worst TUNE_MAXCR pixels
*      Restore any suspected sky line pixels
*      Else abandon without flagging any pixels
*      Endif
*     Else
*       Report that routine is not enabled at present
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)
*     Norman Gray, Starlink (norman@astro.gla.ac.uk)

*  History:
*     1992 Sept 1 : Initial release
*     2004 Aug 20 : Removed reference to Figaro routine (NG)

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_QUALITIES.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER IXBOX
      INTEGER IYBOX
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL CR_MINTEN
      LOGICAL CRCLEAN
      REAL IMAGE2( NX, NY )
      REAL IMAGE3( NX, NY )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER HIHIST
      PARAMETER ( HIHIST = 1000 )

      INTEGER BOXMAX
      PARAMETER ( BOXMAX = 50 )

*  Local Variables:
      REAL XVALS( 5000 )
      REAL WORK( BOXMAX )
      INTEGER WORKI( BOXMAX )
      REAL SHIST( 0: HIHIST )
      REAL ULIMIT
      REAL XCUR
      REAL XH
      REAL XM
      REAL YH
      REAL YM
      REAL MEANIE
      REAL RMIN,RMAX
      REAL TOTAL,RLIMIT

      INTEGER HIST( 0: HIHIST )
      INTEGER OSIZE
      INTEGER I
      INTEGER IY
      INTEGER IX
      INTEGER OPTIONS
      INTEGER MCOUNT
      INTEGER II
      INTEGER INDEX
      INTEGER LIMIT
      INTEGER MAX_CR
      INTEGER GOOD_LIMIT
      INTEGER COUNT
      INTEGER BINS
      INTEGER IQUALITY

      LOGICAL DONE
      LOGICAL INTERPOLATE
      LOGICAL BADOUT            ! for call to KPG1_BMEDR

      CHARACTER*1 CH

      INCLUDE 'SAE_PAR'         ! For SAI__OK

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      IF ( IXBOX .GT. BOXMAX .OR. IYBOX .GT. BOXMAX ) THEN
*      Work arrays are too small
         CALL ECH_REPORT( 0, 'ech_decosmic_1: Work arrays too small!' )
*      I have no idea what the correct error return is here.
*      This one is a guess.
         STATUS = ECH__DIM_CONFLICT
         RETURN
      ENDIF

*  If cosmic ray cleaning enabled (TUNE_TRCRC=YES) then.
      IF ( CRCLEAN ) THEN

*     Remove any previously flagged cosmic rays.
         DO IY = 1, NY
            DO IX = 1, NX
               IQUALITY = QUALITY( IX, IY )
               IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :              QTY_COSMIC_RAY ) THEN
                  QUALITY( IX, IY ) = QUALITY( IX, IY ) - QTY_COSMIC_RAY
               ENDIF
            END DO
         END DO

*     Zero histogram.
         DO I = 1, HIHIST
            HIST( I ) = 0
         END DO

*     Calculate X- and Y-median images.
*     This used to call Figaro's GEN_MEDFLT routine, with arguments
*         CALL GEN_MEDFLT( IMAGE, NX, NY, IXBOX, 1, WORK, IMAGE2 )
*         CALL GEN_MEDFLT( IMAGE, NX, NY, 1, IYBOX, WORK, IMAGE3 )
*     The kaplibs routine has a slightly different way of specifying the
*     size of the box, but since gen_medflt actually used a box with a
*     width rounded down to the next odd number, the result is the same
*     whether ixbox is even or odd.
         CALL ECH_REPORT( 0, ' Calculating X-median image.' )
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, NY, IMAGE,
     :        IXBOX/2, 0,       ! specifies half-width of smoothing box
     :        1,                ! need have only 1 good pixel in box
     :        IMAGE2, BADOUT,
     :        WORK, WORKI,
     :        STATUS )
         CALL ECH_REPORT( 0, ' Calculating Y-median image.' )
         CALL KPG1_BMEDR( .FALSE., .FALSE., .FALSE.,
     :        NX, NY, IMAGE,
     :        0, IYBOX/2,
     :        1,
     :        IMAGE3, BADOUT,
     :        WORK, WORKI,
     :        STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ECH_REPORT( 0, ' Error doing median filtering' )
            GOTO 9999
         ENDIF

*     If minimum cosmic ray intensity not specified then
*     determine min intensity (as mean of +ve pixels).
         IF ( CR_MINTEN .EQ. 0.0 ) THEN
            DO I = 1, NY
               DO II = 1, NX
                  IF ( IMAGE( II, I ) .GT. 0.0 ) THEN
                     MEANIE = MEANIE + IMAGE( II, I )
                     MCOUNT = MCOUNT + 1
                  ENDIF
               END DO
            END DO
            MEANIE = MEANIE / FLOAT( MAX( 1, MCOUNT ) )
            WRITE ( REPORT_STRING, 1000 ) MEANIE
            CALL ECH_REPORT( 0, REPORT_STRING )

*     Set min intensity to value supplied.
         ELSE
            MEANIE = CR_MINTEN
            WRITE ( REPORT_STRING, 1001 ) MEANIE
            CALL ECH_REPORT( 0, REPORT_STRING )
         ENDIF

*     Divide the median filtered image into the original and
*     Histogram the resulting image.
         CALL ECH_REPORT( 0,' Histogramming...' )
         RMIN = 1.0e20
         RMAX = -1.0e20
         TOTAL = 0
         DO I  = 1, NY

*        Divide original image by median filtered images.
            DO II = 1, NX
               IF ( IMAGE( II, I ) .GT. MEANIE .AND.
     :              IMAGE2( II,I ) .GT. 0.0 .AND.
     :              IMAGE3( II,I ) .GT. 0.0 ) THEN
                  IMAGE2( II, I ) = SQRT (
     :                              IMAGE( II, I ) / IMAGE2( II, I ) *
     :                              IMAGE( II, I ) / IMAGE3( II, I ) )
                  IF ( IMAGE2( II, I ) .LT. RMIN )
     :               RMIN = IMAGE2( II, I )
                  IF ( IMAGE2( II, I ) .GT. RMAX )
     :               RMAX = IMAGE2( II, I )
                  TOTAL = TOTAL + 1

               ELSE
                  IMAGE2( II, I ) = ECH__BAD_REAL
               ENDIF
            END DO
         END DO

*     Histogram resulting image.
         RMAX = 10.0
         DO I  = 1, NY
            DO II = 1, NX
               IF ( IMAGE2( II, I ) .GT. 0.0 ) THEN
                  INDEX = INT( ( IMAGE2( II, I ) - RMIN ) /
     :                         ( RMAX - RMIN ) * 1000.0 )
                  INDEX = MIN( INDEX, HIHIST )
                  HIST( INDEX ) = HIST( INDEX ) + 1
               ENDIF
            END DO
         END DO

*     If no MAX_CR supplied, use interactive examination of
*     the histogram to help decision.
         IF ( MAX_CR .EQ. 0 ) THEN
            DONE = .FALSE.
            BINS = HIHIST
            XH = 5.0
            DO I = 1, BINS
               XVALS( I ) = RMIN + ( RMAX -RMIN ) / 1000.  * FLOAT( I )
            END DO
            CH = 'S'

*        Loop accepting cursor/keyoard input from user.
            DO WHILE ( .NOT. DONE )

*           If 'S' key was pressed last then re-calc histogram and re-plot.
               IF ( CH .EQ. 'S' .OR. CH .EQ. 'S') THEN
                  XM = MAX( 0.0, 2.0 - RMAX )
                  XH = RMAX
                  YM = 1.0E20
                  YH = -1.0E20
                  DO I = 1, HIHIST
                     SHIST( I ) = 0.0
                     IF ( HIST( I ) .GT. 0. )
     :                  SHIST( I ) = LOG( FLOAT( HIST( I ) ) )
                     XVALS( I ) = RMIN + ( RMAX - RMIN ) /
     :                            1000.0 * FLOAT( I )
                  END DO
                  DO I = 1, BINS
                     IF ( SHIST( I ) .LT. YM ) YM = SHIST( I )
                     IF ( SHIST( I ) .GT. YH ) YH = SHIST( I )
                  END DO
                  OPTIONS = 0
                  CALL ECH_PLOT_GRAPH(
     :                 BINS, XVALS, SHIST, XM, XH, YM, YH,
     :                 'Intensity/Median', 'LOG(Counts)',
     :                 'Image/Medians histogram', 0.0, 0.0,
     :                 OPTIONS, 'BINS', STATUS )
                  CALL ECH_REPORT( 0, ' ' )
                  CALL ECH_REPORT( 0, ' Type A to abandon.' )
                  CALL ECH_REPORT ( 0,
     :                 ' Type S to replot  with new upper bin limit.' )
                  CALL ECH_REPORT( 0,
     :                 ' Type Q to perform clip at cursor position.' )
                  CALL ECH_REPORT( 0,
     :                 ' Type <space> to view percentage of pixels.' )
               ENDIF

*           Get cursor/key from user.
               CH = ' '
               CALL ECH_READ_GRPH_CURSOR( STATUS )
               CH = USER_INPUT_CHAR
               XCUR = X_CURSOR

*           If 'S' key pressed then.
               IF ( CH .EQ. 'S' .OR. CH .EQ. 's' ) THEN

*              Get new upper bin limit from user.
  133             CONTINUE
                  CALL ECH_GET_PARAMETER(
     :                 'INSTANT-PROMPT=New upper limit',
     :                 'REAL', ULIMIT, .FALSE., ' ', 0, STATUS )
                  IF ( ULIMIT .LT. 0.0 .OR. ULIMIT .GT. 100.0 ) GOTO 133
                  RMAX = ULIMIT
                  DO I = 1, HIHIST
                     HIST( I ) = 0
                  END DO

*              Re-calculate histogram.
                  DO I  = 1, NY
                     DO II = 1, NX
                        IF ( IMAGE2( II,I ) .GT. 0.0 ) THEN
                           INDEX = INT( ( IMAGE2( II, I ) - RMIN ) /
     :                             ( RMAX - RMIN ) * 1000.0 )
                           INDEX = MIN( INDEX, HIHIST )
                           HIST( INDEX ) = HIST( INDEX ) + 1
                        ENDIF
                     END DO
                  END DO
                  DO I = 1, HIHIST
                     IF ( ( RMAX - RMIN ) / 1000.0 * FLOAT( I ) .LT.
     :                      ULIMIT ) THEN
                        BINS = I
                     ENDIF
                  END DO
                  XH = RMAX

*           Else if 'Q' key pressed then exit loop.
               ELSE IF ( CH .EQ. 'Q' .OR. CH .EQ. 'q' ) THEN
                  MAX_CR = INT( RLIMIT * TOTAL / 100 )
                  CALL ECH_REPORT( 0,  ' ' )
                  WRITE ( REPORT_STRING, 1002 ) MAX_CR
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  DONE = .TRUE.

*           Else if 'A' key pressed then exit loop.
               ELSE IF ( CH .EQ. 'A' .OR. CH .EQ. 'a' ) THEN
                  DONE = .TRUE.

*           Else report percentage of pixels corresponding to cursor x-position.
               ELSE
                  RLIMIT = 0
                  LIMIT = 1
                  DO WHILE ( ( RMAX - RMIN ) / 1000.0 * FLOAT( LIMIT ) +
     :                       RMIN .LT. XCUR )
                     LIMIT = LIMIT + 1
                  END DO
                  DO I = LIMIT, HIHIST
                     RLIMIT = RLIMIT + HIST( I )
                  END DO
                  RLIMIT = RLIMIT / TOTAL * 100
                  WRITE ( REPORT_STRING, 1003 ) ( RMAX - RMIN ) /
     :                  1000.0 * FLOAT( LIMIT ) + RMIN, RLIMIT
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  MAX_CR = INT( RLIMIT * TOTAL / 100 )
                  WRITE ( REPORT_STRING, 1004 ) MAX_CR
                  CALL ECH_REPORT( 0, REPORT_STRING )
               ENDIF
            END DO
         ENDIF

*     Count back N events from the 'high' significance end of
*     the histogram, finding the limit at which the expected
*     number of cosmic rays (depends upon instrument/CCD etc)
*     occurs.

*     If last key press was 'A' then.
         IF ( CH .NE. 'A' .AND. CH .NE. 'a' ) THEN
            LIMIT = HIST( 0 )
            I = 0
            DO WHILE ( LIMIT .LT. total - MAX_CR .AND. (i .LT. hihist) )
               I = I + 1
               LIMIT = LIMIT + HIST ( I )
            END DO
            GOOD_LIMIT = I

*        Loop thru image copying it to the output image, and replacing
*        all suspected cosmic rays with a bad value flag.
            DO I  = 1, NY
               DO II = 1, NX
                  IF ( IMAGE2( II, I ) .GT. 0.0 ) THEN
                     INDEX = INT( ( IMAGE2( II, I ) - RMIN ) /
     :                       ( RMAX - RMIN ) * 1000.0 )
                     IF ( INDEX .GE. GOOD_LIMIT ) THEN
                        QUALITY( II, I ) = QTY_COSMIC_RAY
                     ENDIF

                  ELSE IF ( IMAGE( II, I ) .LT. 0.0 ) THEN
                     QUALITY( II, I ) = QTY_NEG_PIXEL
                  ENDIF
               END DO
            END DO

*     Restore any suspected sky line pixels.
            OSIZE = MAX( IXBOX, IYBOX )
            CALL ECH_DECOS_SKYLINE( NX, NY, OSIZE, IMAGE, QUALITY,
     :           STATUS )
            WRITE ( REPORT_STRING, 1005 )
     :            GOOD_LIMIT * ( RMAX - RMIN ) / 1000.0 + RMIN
            CALL ECH_REPORT( 0, report_string )

*        Interpolate if required
            IF ( INTERPOLATE ) THEN

*           Now loop thru original image copying it to the output
*           image. Whenever the corresponding value in image2
*           suggests that the pixel is a cosmic ray, interpolate
*           a replacement value for it.
               CALL ECH_REPORT( 0,
     :              ' Interpolation disabled at current stage.' )
               DO I = 3, NY - 2
                  COUNT = 0
                  DO II = 3, NX - 2
                     INDEX = INT( ( IMAGE2( II, I ) - RMIN ) /
     :                       ( RMAX - RMIN ) * 1000.0 )
                     IF ( INDEX .GE. GOOD_LIMIT ) THEN
                        COUNT = COUNT + 1
                     ENDIF
                  END DO
                  WRITE ( REPORT_STRING, 1006 )  COUNT, I
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END DO
            ENDIF

*     Else abandon without flagging any pixels.
         ELSE
           CALL ECH_REPORT( 0,
     :          ' De-cosmic abandoned: no pixels flagged.' )
         ENDIF

      ELSE
         CALL ECH_REPORT ( 0,
     :        ' Trace frame cosmic-ray cleaning is disabled.' )
         CALL ECH_REPORT ( 0,
     :        ' Set hidden parameter TUNE_CRTRC=YES to enable.')
      ENDIF

 1000 FORMAT ( 1X,
     :  'Min CR intensity set to Mean of positive pixels = ',F15.2 )
 1001 FORMAT ( 1X,'Min CR intensity set to user value of = ',F15.2 )
 1002 FORMAT ( 1X, 'MAX_CR set to ',I6 )
 1003 FORMAT ( 1X, 'Up to ',F8.2,'  => ',F8.2 )
 1004 FORMAT ( 1X, 'would generate a MAX_CR count of ',I6 )
 1005 FORMAT ( 1X, 'Limiting excess value is ',F12.2 )
 1006 FORMAT ( 1X, 'Replaced ',I5,' pixels on line ',I5 )

*   Error exit
 9999 CONTINUE

      END

      REAL FUNCTION DECOSMIC_INTERP( IMAGE, XS, YS, NX, NY, QUALITY )

C     This subroutine does interpolation of various kinds to try
C     and decide the most 'accurate' method to use for any given
C     dataset. Accuracy is determined by the way the algorithm
C     performs when 'replacing' pixels adjacent to the pixel of
C     interest.
C
C     The accuracy of any algorithm is represented by the quantity
C
C               |                                    2     |
C               |     |                            |       |
C               |     |   Actual - Interpolated    |       |
C               |     |         i              i   |       |
C               |                                          |
C      M E A N  |     --------------------------------     |
C      (i=1,n)  |                                          |
C               |               Interpolated               |
C               |                           i              |
C
C          where the i is a pixel neighbouring the pixel of
C          interest.
C
      IMPLICIT NONE
      INCLUDE 'ECH_QUALITIES.INC'
      INTEGER   ALGORITHMS
      PARAMETER  ( ALGORITHMS = 6 )
C
      INTEGER NX
      INTEGER NY
      INTEGER XS
      INTEGER YS
      REAL    image ( XS, YS )
      BYTE    quality ( XS, YS )
      REAL    MEAN_ACCURACY ( ALGORITHMS )
      REAL    VALUE
      INTEGER BEST_ALGORITHM
      REAL    BEST
      REAL    VSAVE
      INTEGER I,J,IC
      INTEGER COUNT
      INTEGER Check
      REAL    FLAG

      DATA FLAG / -1.0e20 /

C     Algorithm 1 interpolates using FIG_FIXAREA ( from BCSUBS )
C
C  Each of the 24 pixels in the 5x5 box centered upon the pixel of interest
C  are run thru' the FIG_FIXAREA interpolater. The resulting values are then
C  subtracted from the original, and scaled by it.
C
      DO IC = 1, ALGORITHMS
      COUNT = 0
      MEAN_ACCURACY ( IC ) = 0.0
      DO I = -2, 2
       DO J = -2, 2
        IF ( (I .EQ. 0 .OR. J .EQ. 0) .AND. I+J .NE. 0 ) THEN
         check = quality(nx+j,ny+i)
         IF ( IAND(check,qty_cosmic_ray) .NE.
     :                                    qty_cosmic_ray ) THEN
          VALUE =  image( NX+J, NY+I )
          VSAVE = VALUE
          IF ( image( NX+J, NY+I ) .NE. FLAG ) THEN
             COUNT = COUNT + 1
             VALUE = ABS (  ( VALUE - image ( NX+J, NY+I ) )  /
     :                               image ( NX+J, NY+I ) )
             MEAN_ACCURACY ( IC ) = MEAN_ACCURACY ( IC ) + VALUE
          ENDIF
          image ( NX+J, NY+I ) = VSAVE
         ENDIF
        ENDIF
       END DO
      END DO
      MEAN_ACCURACY ( IC ) = MEAN_ACCURACY ( IC ) / FLOAT ( COUNT )
      END DO
C
C     Determine most accurate algorithm
C
      BEST = 1.0e20
      DO I = 1, ALGORITHMS
         IF ( MEAN_ACCURACY ( I ) .LT. BEST ) THEN
             BEST_ALGORITHM = I
             BEST = MEAN_ACCURACY ( I )
         ENDIF
      END DO
C
C
C     Use 'best' algorithm to generate missing pixel value
C
      IF ( BEST_ALGORITHM .GE. 1 .AND. BEST_ALGORITHM .LE. 6 ) THEN
          VSAVE =  image ( NX, NY )
          DECOSMIC_INTERP = FLOAT (INT ( image ( NX, NY ) + 0.5 ) )
          image ( NX, NY ) = VSAVE
C
      ENDIF
      END


