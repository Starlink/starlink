      SUBROUTINE ECH_DECOS_PROB(
     :           PROB,
     :           PROB_THRESH,
     :           NX,
     :           NY,
     :           QUALITY,
     :           MEAN,
     :           SIGMA,
     :           SIGMA_THRESH,
     :           MAX,
     :           IORD,
     :           PIXEL_COUNT,
     :           LAST_PROB,
     :           DATA,
     :           DATA_SRT,
     :           XAXIS,
     :           YAXIS,
     :           GAUSSIAN,
     :           INDEX_X,
     :           INDEX_Y,
     :           CLIPPED_COUNT,
     :           CLIP,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOS_PROB

*  Purpose:
*     Clips cosmic ray pixels and plots CDFs.

*  Description:
*     This routine checks that the automatic cosmic-ray iterative loop
*     can continue clipping further rays, else it displays the final
*     fitted and observed CDFs.

*  Invocation:
*     CALL ECH_DECOS_PROB(
*     :          PROB,
*     :          PROB_THRESH,
*     :          NX,
*     :          NY,
*     :          QUALITY,
*     :          MEAN,
*     :          SIGMA,
*     :          SIGMA_THRESH,
*     :          MAX,
*     :          IORD,
*     :          PIXEL_COUNT,
*     :          LAST_PROB,
*     :          CLIPPED_COUNT,
*     :          CLIP,
*     :          DATA,
*     :          DATA_SRT,
*     :          XAXIS,
*     :          YAXIS,
*     :          GAUSSIAN,
*     :          INDEX_X,
*     :          INDEX_Y,
*     :          STATUS
*     :         )

*  Arguments:
*    PROB_THRESH = REAL (Given)
*       Probability threshold at which to stop clipping.
*    PROB = DOUBLE (Given)
*       Probability of CDF fit.
*    NX = INTEGER (Given)
*       Number of columns in frame.
*    NY = INTEGER (Given)
*       Number of rows in frame.
*    QUALITY = BYTE (Given)
*       Input data frame quality flags array.
*    MEAN = REAL (Given)
*       Mean pixel intensity.
*    SIGMA = REAL (Given)
*       Observed sigma.
*    SIGMA_THRESH = REAL (Given)
*       Clipping point threshold in sigma.
*    IORD = INTEGER (Given)
*       Order number being processed.
*    PIXEL_COUNT = INTEGER (Given)
*       Count of pixels in CDF.
*    MAX = REAL (Returned)
*       Maximum difference observed-theoretical.
*    LAST_PROB = DOUBLE (Returned)
*       Last Probability of CDF fit.
*    CLIPPED_COUNT = INTEGER (Returned)
*       Count of pixel clipped from this order.
*    CLIP = LOGICAL (Returned)
*       TRUE when clipping pixels.
*    DATA = REAL (Temporary Workspace)
*       Ratios of observed/predicted energy.
*    DATA_SRT = REAL (Temporary Workspace)
*       Ratios of observed/predicted energy.
*    XAXIS = REAL (Temporary Workspace)
*       Theoretical data points.
*    YAXIS = REAL (Temporary Workspace)
*       Theoretical CDF values.
*    GAUSSIAN = REAL (Temporary Workspace)
*       Theoretical gaussian values.
*    INDEX_X = INTEGER*2 (Temporary Workspace)
*       X coords of pixels.
*    INDEX_Y = INTEGER*2 (Temporary Workspace)
*       Y coords of pixels.
*    STATUS = INTEGER (Given and Returned)
*       Input/Output status conditions.

*  Method:
*     If probability greater than threshold value OR probability greater then
*            10% AND not improved since last clip OR all values fall
*            within n sigma then
*        Disable further clipping
*        Plot theoretical / observed CDFs
*     Else
*        Record probability, and set clipping active
*        Set clip point at mean + n sigma
*        Loop thru observed data values
*            If data exceeds clip point then
*               Replace responsible image pixel with flag value
*             Endif
*        End loop
*     Endif
*     Report clip statistics for this iteration

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
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_QUALITIES.INC'

*  Arguments Given:
      DOUBLE PRECISION PROB
      REAL PROB_THRESH
      INTEGER NX
      INTEGER NY
      BYTE QUALITY( NX, NY )
      REAL MEAN
      REAL SIGMA
      REAL SIGMA_THRESH
      INTEGER IORD

*  Arguments Returned:
      INTEGER PIXEL_COUNT
      DOUBLE PRECISION LAST_PROB
      REAL MAX
      INTEGER CLIPPED_COUNT
      LOGICAL CLIP

*  Workspace:
      REAL data( nx * max_slice_pixels )
*          ! Ratios of observed/predicted energy.
      REAL data_srt( nx * max_slice_pixels )
*          ! Sorted Ratios of observed/predicted energy.
      REAL xaxis( nx * max_slice_pixels )
*          ! Theoretical data points.
      REAL yaxis( nx * max_slice_pixels )
*          ! Theoretical CDF values.
      REAL gaussian( nx * max_slice_pixels )
*          ! Theoretical gaussian values.
      INTEGER*2 index_x( nx * max_slice_pixels )
*          ! X coords of pixels.
      INTEGER*2 index_y( nx * max_slice_pixels )
*          ! Y coords of pixels.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL XH
      REAL XM
      REAL YH
      REAL YM

      INTEGER IX
      INTEGER OPTIONS
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      CHARACTER*64 TITLE
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      MAX = DATA_SRT( PIXEL_COUNT )

*  If probability greater than threshold value OR probability greater than
*  10% AND not improved since last clip OR all values fall
*  within n sigma then
      IF ( PROB .GT. DBLE( PROB_THRESH ) .OR. ( PROB .GT. 0.1 .AND.
     :     PROB - LAST_PROB .LT. 0.01 ) .OR.
     :     MAX .LT. MEAN + SIGMA * SIGMA_THRESH ) THEN

*     Disable further clipping.
         CLIP = .FALSE.

*     Plot theoretical / observed CDFs.
         YH = 1.0
         YM = 0.0
         XH = MEAN + 3.0 * SIGMA
         XM = MEAN - 3.0 * SIGMA
         CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
         CALL CHR_ITOC( PIXEL_COUNT, REF_STR2, NCHAR2 )
         CALL CHR_DTOC( PROB, REF_STR3, NCHAR3 )
         TITLE = ' Order ' // REF_STR1( :NCHAR1 ) // ': ' //
     :         REF_STR2( :NCHAR2 ) // ' pixels, probability: ' //
     :         REF_STR3( :NCHAR3 ) // '.'
         OPTIONS = 0
         CALL ECH_PLOT_GRAPH( PIXEL_COUNT, DATA_SRT, YAXIS, XM, XH,
     :        YM, YH, 'Count', 'Accepted CDF', TITLE, 0., 0.,
     :        OPTIONS, 'LINES', STATUS )
         OPTIONS = GRPH_OVERLAY + GRPH_SET_COLOUR
         CALL ECH_PLOT_GRAPH( PIXEL_COUNT, XAXIS, GAUSSIAN, XM, XH,
     :        YM, YH, 'GREEN', ' ', ' ', 0., 0.,
     :        OPTIONS, 'LINES', STATUS )

      ELSE

*     Record probability, and set clipping active.
         LAST_PROB = PROB
         CLIP = .TRUE.

*     Set clip point at mean + n sigma.
         MAX = MEAN + SIGMA_THRESH * SIGMA

*     Loop thru observed data values.
         DO IX = 1, PIXEL_COUNT

*         If data exceeds clip point then.
             IF ( DATA( IX ) .GE. MAX ) THEN

*            Replace responsible image pixel with flag value.
                QUALITY( INDEX_X( IX ), INDEX_Y( IX ) ) =
     :          QUALITY( INDEX_X( IX ), INDEX_Y( IX ) ) +
     :          QTY_COSMIC_RAY
                CLIPPED_COUNT = CLIPPED_COUNT + 1
             END IF
         END DO
      END IF

*  Report clip statistics for this iteration.
      CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
      CALL CHR_DTOC( PROB, REF_STR2, NCHAR2 )
      REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :      ': Probability of fit to CDF: ' //
     :      REF_STR2( :NCHAR2 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )
      CALL CHR_ITOC( CLIPPED_COUNT, REF_STR1, NCHAR1 )
      CALL CHR_RTOC( SIGMA_THRESH, REF_STR2, NCHAR2 )
      REPORT_STRING = ' Clipped ' // REF_STR1( :NCHAR1 ) //
     :      ' pixels using sigma=' //  REF_STR2( :NCHAR2 ) // '.'
      CALL ECH_REPORT( 0, REPORT_STRING )

      END
