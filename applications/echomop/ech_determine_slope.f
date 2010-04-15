      SUBROUTINE ECH_DETERMINE_SLOPE(
     :           IMAGE,
     :           NX,
     :           NY,
     :           XBOX,
     :           USE_MEDIAN,
     :           FRAME_CHECK,
     :           ORDER_SLOPE,
     :           DSAMPLE_XCOORD,
     :           DSAMPLE_YCOORD,
     :           DIAGONAL,
     :           DIAGONAL2,
     :           SMOOTH,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DETERMINE_SLOPE

*  Purpose:
*     Measure slope of orders across input image.
*
*  Description:
*     This routine determines the slope of the orders in an echellogram.
*     The method used is to first collapse or median filter samples of
*     20 columns lying across the corner-corner diagonals of the input
*     frame into a 1-d array, and then apply a triangle filter to these data.

*  Invocation:
*     CALL ECH_DETERMINE_SLOPE(
*    :     IMAGE,
*    :     NX,
*    :     NY,
*    :     XBOX,
*    :     USE_MEDIAN,
*    :     FRAME_CHECK,
*    :     ORDER_SLOPE,
*    :     DSAMPLE_XCOORD,
*    :     DSAMPLE_YCOORD,
*    :     DIAGONAL,
*    :     DIAGONAL2,
*    :     SMOOTH,
*    :     STATUS
*    :    )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     XBOX = INTEGER (Given)
*        Sampling box size in x pixels.
*     USE_MEDIAN = LOGICAL (Given)
*        TRUE if median filtering is to be used.
*     FRAME_CHECK = LOGICAL (Given)
*        TRUE if checking enabled.
*     ORDER_SLOPE = LOGICAL (Given)
*        order_slope.
*     DIAGONAL = REAL (Temporary Workspace)
*        Calculated functions.
*     DIAGONAL2 = REAL (Temporary Workspace)
*        Calculated functions.
*     DSAMPLE_XCOORD = REAL (Temporary Workspace)
*        X coordinates of diagonally samples.
*     DSAMPLE_YCOORD = REAL (Temporary Workspace)
*        Y coordinates of diagonally samples.
*     SMOOTH = REAL (Temporary Workspace)
*        Smoothing workspace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Ouput status conditions.

*  Method:
*     Loop through input frame summing within the sampling box
*     End loop
*      Triangle filter to enhance peaks (11 pixel filter)
*      Smooth
*      Loop stepping outwards from centre of diagonally sampled arrays
*         If this is a local peak then record it
*         Endif
*         If this is a local peak then record it
*         Endif
*         If this is a local peak then record it
*         Endif
*         If this is a local peak then record it
*         Endif
*     End loop
*     Calculate estimated slope for each peak pair located
*     Setup result for caller

*  Bugs :
*     None known.

*  Authors :
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History :
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL ORDER_SLOPE
      REAL IMAGE( NX, NY )
      INTEGER XBOX
      LOGICAL USE_MEDIAN
      LOGICAL FRAME_CHECK

*  Workspace:
      REAL DIAGONAL( NX )
      REAL DIAGONAL2( NX )
      REAL SMOOTH( NX )
      REAL DSAMPLE_XCOORD( NX )
      REAL DSAMPLE_YCOORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL SLOPE( MAX_ALLOWED_ORDERS )
      REAL WEIGHT( MAX_ALLOWED_ORDERS )
      REAL TRIANGLE_FN( -100 : 100 )
      REAL NORMALIZER

      INTEGER LOWER_LEFT( MAX_ALLOWED_ORDERS )  ! Lower-left peaks in diagonal.
      INTEGER LOWER_RIGHT( MAX_ALLOWED_ORDERS ) ! Lower-right peaks in diagonal.
      INTEGER UPPER_LEFT( MAX_ALLOWED_ORDERS )  ! Upper-left peaks in diagonal.
      INTEGER UPPER_RIGHT( MAX_ALLOWED_ORDERS ) ! Upper-right peaks in diagonal.
      INTEGER I
      INTEGER XSTRT
      INTEGER XEND
      INTEGER ISCALE
      INTEGER YINDEX
      INTEGER IX
      INTEGER INDEX
      INTEGER N_SAMPLE
      INTEGER PEAKS_LL
      INTEGER PEAKS_LR
      INTEGER PEAKS_UL
      INTEGER PEAKS_UR
      INTEGER SCOUNT
      INTEGER ITRI
      INTEGER NCHAR1
      INTEGER NCHAR2

      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

      IF ( frame_check ) THEN

*  Define limits of sampling box
      xstrt = xbox / 2
      xend = nx - xbox / 2

*  Loop through input frame summing within the sampling box
      n_sample = 0

      DO ix = xstrt, xend, MAX( 1, nx / ny )

*        Initialise sum array element and counter
         n_sample = n_sample + 1
         diagonal( n_sample ) = 0.0
         diagonal2( n_sample ) = 0.0
         dsample_ycoord( n_sample ) = FLOAT( ny ) *
     :         ( 1.0 - FLOAT( ix ) / FLOAT( nx ) )
         dsample_xcoord( n_sample ) = FLOAT( ix + xbox / 2 )
         yindex = INT( dsample_ycoord( n_sample ) + 0.5 )
         yindex = MIN( MAX( 1, yindex ), nx )
         CALL ECH_MEAN_MEDIAN( XBOX, IMAGE( IX, YINDEX ), USE_MEDIAN,
     :        .FALSE., DIAGONAL( N_SAMPLE ), STATUS )
         CALL ECH_MEAN_MEDIAN( XBOX, IMAGE( NX - IX - XBOX / 2 + 1,
     :        YINDEX ), USE_MEDIAN, .FALSE., DIAGONAL2( N_SAMPLE ),
     :        STATUS )
      END DO

*  Triangle filter to enhance peaks (11 pixel filter)
       DO itri = -3, 3
          triangle_fn ( itri ) = 0.75 *
     :                     ( 1.0 - ABS ( FLOAT ( itri ) /
     :                                     3. )  ) + 0.25
       END DO
       DO iscale = 4, n_sample - 3
          smooth ( iscale ) = 0.0
          DO itri = -3, 3
             smooth ( iscale ) = smooth ( iscale ) +
     :                           diagonal ( iscale + itri )
     :                                     * triangle_fn ( itri )
          END DO
       END DO
       DO iscale = 1, n_sample
          diagonal ( iscale ) = smooth ( iscale )
       END DO
       DO iscale = 4, n_sample - 3
          smooth ( iscale ) = 0.0
          DO itri = -3, 3
             smooth ( iscale ) = smooth ( iscale ) +
     :                           diagonal2( iscale + itri )
     :                                     * triangle_fn ( itri )
          END DO
       END DO
       DO iscale = 1, n_sample
          diagonal2( iscale ) = smooth ( iscale )
       END DO

*  Smooth
       DO i = 1, 3
            DO iscale = 2, n_sample-1
               smooth ( iscale ) =  ( diagonal ( iscale ) +
     :                                diagonal ( iscale-1 ) +
     :                                diagonal ( iscale+1 ) ) / 3.0
            END DO
            DO iscale = 2, n_sample-1
               diagonal ( iscale )  = smooth ( iscale )
            END DO
       END DO

       DO i = 1, 3
            DO iscale = 2, n_sample-1
               smooth ( iscale ) =  ( diagonal2( iscale ) +
     :                                diagonal2( iscale-1 ) +
     :                                diagonal2( iscale+1 ) ) / 3.0
            END DO
            DO iscale = 2, n_sample-1
               diagonal2( iscale )  = smooth ( iscale )
            END DO
       END DO

*      Search diagonals for highest (independent) peaks

*      Loop stepping outwards from centre of diagonally sampled arrays
       peaks_ll = 0
       peaks_ul = 0
       peaks_ur = 0
       peaks_lr = 0

       DO iscale = 1, n_sample/3

*         Process diagonal array 1, upper left hand section

*         If this is a local peak then record it
          index = n_sample / 2 - iscale
          IF ( diagonal ( index ) .GT. diagonal ( index-1 ) .AND.
     :      diagonal ( index+1 ) .GT. diagonal ( index+2 ) .AND.
     :      diagonal ( index-1 ) .GT. diagonal ( index-2 ) .AND.
     :      diagonal ( index+2 ) .GT. diagonal ( index+3 ) .AND.
     :      diagonal ( index-2 ) .GT. diagonal ( index-3 ) .AND.
     :      diagonal ( index ) .GT. diagonal ( index+1 ) ) THEN

            peaks_ul = peaks_ul + 1
            upper_left ( peaks_ul ) = index
          ENDIF

*         Process diagonal array 1, lower right hand section

*         If this is a local peak then record it
          index = n_sample / 2 + iscale
          IF ( diagonal ( index ) .GT. diagonal ( index-1 ) .AND.
     :      diagonal ( index+1 ) .GT. diagonal ( index+2 ) .AND.
     :      diagonal ( index-1 ) .GT. diagonal ( index-2 ) .AND.
     :      diagonal ( index+2 ) .GT. diagonal ( index+3 ) .AND.
     :      diagonal ( index-2 ) .GT. diagonal ( index-3 ) .AND.
     :      diagonal ( index ) .GT. diagonal ( index+1 ) ) THEN

            peaks_lr = peaks_lr + 1
            lower_right ( peaks_lr ) = index
          ENDIF

*         Process diagonal array 2, upper right hand section

*         If this is a local peak then record it
          index = n_sample / 2 - iscale
          IF ( diagonal2( index ) .GT. diagonal2( index-1 ) .AND.
     :      diagonal2( index+1 ) .GT. diagonal2( index+2 ) .AND.
     :      diagonal2( index-1 ) .GT. diagonal2( index-2 ) .AND.
     :      diagonal2( index+2 ) .GT. diagonal2( index+3 ) .AND.
     :      diagonal2( index-2 ) .GT. diagonal2( index-3 ) .AND.
     :      diagonal2( index ) .GT. diagonal2( index+1 ) ) THEN

            peaks_ur = peaks_ur + 1
            upper_right ( peaks_ur ) = index
          ENDIF

*         Process diagonal array 2, lower left hand section

*         If this is a local peak then record it
          index = n_sample / 2 + iscale
          IF ( diagonal2( index ) .GT. diagonal2( index-1 ) .AND.
     :      diagonal2( index+1 ) .GT. diagonal2( index+2 ) .AND.
     :      diagonal2( index-1 ) .GT. diagonal2( index-2 ) .AND.
     :      diagonal2( index+2 ) .GT. diagonal2( index+3 ) .AND.
     :      diagonal2( index-2 ) .GT. diagonal2( index-3 ) .AND.
     :      diagonal2( index ) .GT. diagonal2( index+1 ) ) THEN

            peaks_ll = peaks_ll + 1
            lower_left ( peaks_ll ) = index
          ENDIF
      END DO

*     Calculate estimated slope for each peak pair located
      scount = 0
      DO index = 1, MIN ( peaks_ul, peaks_ur )
         scount = scount + 1
         slope ( scount ) =(
     :                       dsample_yCOORD( upper_right ( index )) -
     :                       dsample_yCOORD( upper_left ( index )) ) /
     :                     (
     :      FLOAT ( nx+1 ) - dsample_xCOORD( upper_right ( index )) -
     :                       dsample_xCOORD( upper_left ( index )) )
         weight ( scount ) =
     :       FLOAT ( nx+1 ) -dsample_xCOORD( upper_right ( index ) ) -
     :                       dsample_xCOORD( upper_left ( index ) )

      END DO

      DO index = 1, MIN ( peaks_ll, peaks_lr )

         scount = scount + 1
         slope ( scount ) =(
     :                       dsample_yCOORD( lower_right ( index )) -
     :                       dsample_yCOORD( lower_left ( index )) ) /
     :                     (
     :                       dsample_xCOORD( lower_right ( index )) -
     :      ( FLOAT ( nx )-dsample_xCOORD( lower_left ( index )) )  )
         weight ( scount ) =
     :                       dsample_xCOORD( lower_right ( index ) ) -
     :      ( FLOAT ( nx )-dsample_xCOORD( lower_left ( index )) )

      END DO

*     Calculate weighted average of slopes
      order_slope = 0.0
      normalizer = 0.0
      DO index = 1, scount
         normalizer = normalizer + weight ( index )
         order_slope = order_slope + weight ( index ) * slope ( index )

      END DO
      IF ( normalizer .GT. 0.0 ) THEN
         order_slope = order_slope / normalizer
      ENDIF

*     Setup result for caller
      IF ( scount .GT. 0 ) THEN
         status = 0
         IF ( IAND ( report_mode, rpm_full + rpm_info ) .GT. 0 ) THEN
            CALL CHR_ITOC( SCOUNT, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( ORDER_SLOPE, REF_STR2, NCHAR2 )
            REPORT_STRING = ' Order slope estimated (from ' //
     :            REF_STR1( :NCHAR1 ) // ' values) at ' //
     :            REF_STR2( :NCHAR2 ) // '.'
             CALL ECH_REPORT( 0, report_string )
         ENDIF
      ELSE
         CALL ECH_SET_CONTEXT( 'PROBLEM', 'Bad diagonals' )
         IF ( IAND( report_mode, rpm_full +
     :        rpm_info + rpm_error ) .GT. 0 ) THEN
            REPORT_STRING =
     :            ' Could not locate diagonal peaks to determine slope.'
             CALL ECH_REPORT( 0, report_string )
         ENDIF
         status = ECH__NO_SLOPE
      ENDIF

      ELSE
         order_slope = 0.0
         CALL ECH_REPORT( 0,
     :        ' Order slope assumed to be close to 0.' )
         CALL ECH_REPORT( 0,
     :   ' Set TUNE_FCHECK=YES to enable order slope estimation.' )
      ENDIF

      END
