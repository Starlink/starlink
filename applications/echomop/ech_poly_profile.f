      SUBROUTINE ECH_POLY_PROFILE(
     :           IMAGE,
     :           QUALITY,
     :           NX,
     :           NY,
     :           ORDER_NUMBER,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           TRACE_POLYNOMIAL,
     :           INTERACTIVE,
     :           PFL_SUBSAMPLES,
     :           MAX_SKY_PIXELS,
     :           USE_NXF,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           SKY_MODEL,
     :           OBJ_NPOLY,
     :           OBJREJ,
     :           THRESH,
     :           POLY_PROFILES,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           TOTAL,
     :           TCOUNT,
     :           PFIT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_POLY_PROFILE

*  Purpose:
*     Fits polynomials to spatial behaviour of profile with wavelength.

*  Invocation:
*     CALL ECH_POLY_PROFILE(
*     :    IMAGE,
*     :    QUALITY,
*     :    NX,
*     :    NY,
*     :    ORDER_NUMBER,
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    TRACE_POLYNOMIAL,
*     :    INTERACTIVE,
*     :    PFL_SUBSAMPLES,
*     :    MAX_SKY_PIXELS,
*     :    USE_NXF,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    OBJ_MASK,
*     :    SKY_MASK,
*     :    SKY_MODEL,
*     :    OBJ_NPOLY,
*     :    OBJREJ,
*     :    THRESH,
*     :    POLY_PROFILES,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    TOTAL,
*     :    TCOUNT,
*     :    PFIT,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     TRACE_POLYNOMIAL = REAL (Given)
*        Polynomial coefficients tracing the orders.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if user to edit profiles/limits.
*     DEK_BELOW = INTEGER (Given and Returned)
*        Lower dekker limit in pixels from trace.
*     DEK_ABOVE = INTEGER (Given and Returned)
*        Upper dekker limit in pixels from trace.
*     MAX_SKY_PIXELS = INTEGER (Given and Returned)
*        Size of mask arrays.
*     OBJ_MASK = INTEGER (Returned)
*        Non zero values denoting object presence.
*     SKY_MASK = INTEGER (Returned)
*        Non zero values denoting object presence.
*     SKY_MODEL = REAL (Given)
*        Modeled sky intensities at offsets from trace.
*     PFL_SUBSAMPLES = INTEGER (Given and Returned)
*        Number of subsamples to use for profiles.
*     POLY_PROFILES = DOUBLE (Given and Returned)
*        Polynomial profile fits.
*     FIT_WORK_XINT = INTEGER (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     FIT_WORK_3XDOUBLE = DOUBLE (Temporary Workspace)
*        Workspace array for NAG routine in fitter.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     ORDER_NUMBER = INTEGER (Given)
*        Number of order being processed.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     USE_NXF = FLOAT (Given)
*        Fraction of columns to use.
*        (>=1.0 also sets order-by-order processsing).
*     OBJ_NPOLY = INTEGER (Given)
*        Maximum number of coefficients to use in object profile x fits.
*     OBJREJ = INTEGER (Given)
*        Number of reject cycles.
*     THRESH = FLOAT (Given)
*        Rejection threshold (sigma) for fit.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coords of order trace path.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coords of order trace path.
*     TOTAL = REAL (Given and Returned)
*        Total fluxes per order and column.
*     TCOUNT = INTEGER (Given and Returned)
*        Count of pixels used per increment.
*     PFIT = FLOAT (Given)
*        Profile fit.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Estimate the spatial extent in pixels
*     Set appropriate subsampling rate
*     Loop thru orders
*       Count object pixels within dekker
*       If a good trace polynomial is avaliable for this order then
*         Calculate the order trace
*       Endif
*       Loop thru positive pixel offsets
*        If pixel is at an object pixel location
*          Loop thru substeps
*             Loop while re-fit flag set
*               If not interactive, then clear re-fit flag now
*                   Loop thru all x
*                      Calculate exact y-coord and offset of pixel centre
*                           If a good pixel, add it to dataset
*               End loop
*               Normalise totals by number of contrbuting pixels
*               If some good pixels found then
*                  Plot graph if interactive mode
*                  If polynomial fit requested then fit to data
*                  Else calculate mean
*                  Endif
*                  Evaluate fit at all x positions
*                  If interactive then overplot on graph
*                    Get user option input
*                  Endif
*                  Save fitted coeffs
*               Else report no pixels available at this offset
*               Endif
*             End loop
*          End loop
*        Endif
*       End loop
*       Repeat above for offsets below trace
*     End loop

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
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL USE_NXF
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*          ! Polynomials to follow paths of orders.
      LOGICAL INTERACTIVE
      INTEGER DEK_BELOW
      INTEGER DEK_ABOVE
      INTEGER MAX_SKY_PIXELS
      INTEGER ORDER_NUMBER
      INTEGER PFL_SUBSAMPLES
      INTEGER OBJ_NPOLY
      CHARACTER*( * ) FITTER
      INTEGER OBJREJ
      REAL THRESH

*  Workspace:
      DOUBLE PRECISION PFIT( NX )
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Arguments Returned:
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*           ! Non-zero where object pixels are.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
*           ! Non-zero where sky pixels are.
      REAL SKY_MODEL( NX, -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2,
     :      N_ORDERS )
*           ! Modelled sky intensities.
      DOUBLE PRECISION POLY_PROFILES( MAXIMUM_POLY, -PFL_SUBSAMPLES/ 2:
     :      PFL_SUBSAMPLES/2, N_ORDERS )
*           ! Polys for profiles.
      REAL TOTAL( NX, N_ORDERS )
      INTEGER TCOUNT( NX, N_ORDERS )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXDATA
      PARAMETER ( MAXDATA = 5000 )

*  Local Variables:
      DOUBLE PRECISION TEMP_COEFFS( MAX_FIT_COEFFS )
      DOUBLE PRECISION SAMPLE ( MAXDATA )
      DOUBLE PRECISION XSAMPLE ( MAXDATA )
      DOUBLE PRECISION DDUMMY( 2 )

      REAL X_POINTS ( MAXDATA )
      REAL Y_POINTS ( MAXDATA )
      REAL RDUMMY( 2 )
      REAL DIST
      REAL VALUE
      REAL XMIN
      REAL XMAX
      REAL YMAX

      INTEGER I
      INTEGER II
      INTEGER IP
      INTEGER III
      INTEGER IV
      INTEGER SORD
      INTEGER EORD
      INTEGER IORDER_NUMBER
      INTEGER LORDER_NUMBER
      INTEGER UORDER_NUMBER
      INTEGER ORDER_SIZE
      INTEGER SUBSTEPS
      INTEGER IORD
      INTEGER INDEX
      INTEGER IY
      INTEGER OPTIONS
      INTEGER OBJPIX
      INTEGER COUNTS
      INTEGER NCHAR1

      LOGICAL REFIT

      CHARACTER*64 TITLE
      CHARACTER*16 REF_STR1

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Estimate the spatial extent in pixels, set appropriate subsampling rate.
      CALL ECH_CALC_PROFSAMP( NY, N_ORDERS, PFL_SUBSAMPLES, DEK_BELOW,
     :     DEK_ABOVE, ORDER_SIZE, SUBSTEPS, STATUS )

      LORDER_NUMBER = 0
      UORDER_NUMBER = 0
      IF ( ORDER_NUMBER .EQ. 0 .AND. USE_NXF .GE. 1.0 ) THEN
         LORDER_NUMBER = 1
         UORDER_NUMBER = N_ORDERS
      ENDIF

      DO IORDER_NUMBER = LORDER_NUMBER, UORDER_NUMBER
         SORD = 1
         EORD = N_ORDERS
         IF ( IORDER_NUMBER .GT. 0 ) THEN
            SORD = IORDER_NUMBER
            EORD = IORDER_NUMBER
            CALL CHR_ITOC( SORD, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Deriving profile polyomials for order ' //
     :            REF_STR1( :NCHAR1 ) // '.'

         ELSE
            REPORT_STRING = ' Using all order information to derive' //
     :            ' profile X-dependency.'
         ENDIF
         CALL ECH_REPORT( 0, REPORT_STRING )

         OBJPIX = 0
         DO III = DEK_BELOW, DEK_ABOVE
            IF ( OBJ_MASK( III, SORD ) .NE. 0 ) THEN
               OBJPIX = OBJPIX + 1
            ENDIF
         END DO
         OBJPIX = OBJPIX + 2

      DO IORD = SORD, EORD

*     Check for a good trace polynomial for this order.
         IF ( TRACE_POLYNOMIAL( 1, IORD ) .EQ. ECH__BAD_DOUBLE ) THEN
            GO TO 100
         END IF

*     Calculate the order trace.
         CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :        TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD,
     :        Y_TRACE_COORD, STATUS )
         DO I = 1, NX
            TOTAL( I, IORD ) = 0.0
            TCOUNT( I, IORD ) = 0
         END DO

         DO III = DEK_BELOW + 1, DEK_ABOVE - 1
            IF ( OBJ_MASK( III, IORD ) .NE. 0 .OR.
     :           OBJ_MASK( III - 1, IORD ) .NE. 0 .OR.
     :           OBJ_MASK( III + 1, IORD ) .NE. 0  ) THEN
               DO I = 1, NX
                  IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + III
                  IF ( IY .GE. 1 .AND. IY .LE. NY ) THEN
                     IF ( QUALITY( I, IY ) .EQ. 0 .AND.
     :                    IMAGE( I, IY ) .NE. ECH__BAD_REAL ) THEN
                        VALUE = IMAGE( I, IY ) -
     :                          SKY_MODEL( I, III, IORD )
                        IF ( VALUE .GT. 0.0 ) THEN
                           TOTAL( I, IORD ) = TOTAL( I, IORD ) + VALUE
                           TCOUNT( I, IORD ) = TCOUNT( I, IORD ) + 1
                        END IF
                     END IF
                  END IF
               END DO
            END IF
         END DO
  100    CONTINUE
      END DO

*  Subsample the profile.
*  =====================

*  Loop through positive pixel offsets.
       DO III = 0, DEK_ABOVE

*        If pixel is at an object pixel location
         IF ( obj_mask( iii, sord ) .NE. 0 .OR.
     :        obj_mask( iii - 1, sord ) .NE. 0 ) THEN

           IF ( III .EQ. 0 ) THEN
              TITLE = ' Calculating object pixel fractions at' //
     :                ' trace centre.'

           ELSE IF ( III .EQ. 1 ) THEN
              TITLE = ' Calculating object pixel fractions at' //
     :                ' 1 pixel above trace.'

           ELSE IF ( III .GT. 1 ) THEN
              CALL CHR_ITOC( III, REF_STR1, NCHAR1 )
              TITLE = ' Calculating object pixel fractions at ' //
     :                REF_STR1( :NCHAR1 ) // ' pixels above trace.'
           END IF
           CALL ECH_REPORT( 0, TITLE )

*          Loop thru substeps
           DO ii = 0, substeps - 1
              index = iii * substeps + ii
              IF ( INDEX .EQ. 0 ) THEN
                 TITLE = 'Object pixel fractions at trace centre.'

              ELSE IF ( INDEX .EQ. 1 ) THEN
                 TITLE = 'Object pixel fractions at 1 substep' //
     :                   ' above trace.'
              ELSE
                 CALL CHR_ITOC( INDEX, REF_STR1, NCHAR1 )
                 TITLE = 'Object pixel fractions at ' //
     :                REF_STR1( :NCHAR1 ) // ' substeps above trace.'
              END IF

*             Loop while re-fit flag set
              refit = .TRUE.
              DO WHILE ( refit )

*            If not interactive, then clear re-fit flag now.
                IF ( .NOT. interactive ) refit = .FALSE.

                DO I = 1, NX
                  XSAMPLE( I )=0.0
                  SAMPLE( I )=0.0
                END DO

                counts = 0

*            Process each order in turn.
                DO IORD = SORD, EORD

*              Only process orders for which there is a trace available.
                  IF ( TRACE_POLYNOMIAL( 1, IORD ) .NE.
     :                 ECH__BAD_DOUBLE ) THEN

*                Evaluate the trace.
                    CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :                   TRACE_POLYNOMIAL( 1, IORD ),
     :                   X_TRACE_COORD, Y_TRACE_COORD, STATUS )

*                Process each X step in turn.
                    DO i = 1, nx

*                   Only use increments where pixels are present.
                       IF ( TCOUNT( I, IORD ) .GT. 0 ) THEN

*                     Find position of this pixel centre.
                         dist = FLOAT( ii ) / FLOAT( substeps ) +
     :                       FLOAT( iii ) + REAL( y_trace_coord( i ) )

*                     Find integer part of trace Y-coord.
                         IY = INT( DIST + 0.5 )

                         IF ( ABS( FLOAT( iy ) - dist ) .LE.
     :                        1 / FLOAT( substeps ) ) THEN
                           IF ( iy .GT. 0 .AND. iy .LE. ny  ) THEN
                            IF ( quality( i, iy ) .EQ. 0 .AND.
     :                           IMAGE( I, IY ) .NE. ECH__BAD_REAL )
     :                           THEN

*                           If a good pixel, add it to dataset
                             IF ( counts+2 .LT. maxdata ) THEN
                                value = image( i, iy )
     :                                    - sky_model( i, iii, iord )
                                IF ( value .GT. 0.0 ) THEN
                                  counts = counts + 1
                                  sample( i ) = sample(i)+ value /
     :                                         ( total ( i,iord ) *
     :                                   FLOAT( objpix ) /
     :                                   FLOAT( tcount( i,iord ) ) )
                                  xsample( i ) = xsample( i ) + 1.0
                                  IF ( tcount( i, iord ) .GE.
     :                                 objpix * 2 / 3 ) THEN
                                     sample( i ) = sample( i ) + value /
     :                                             total( i, iord )
                                     xsample( i ) = xsample( i ) + 1.0
                                  ENDIF
                                ENDIF
                             ENDIF
                            ENDIF
                           ENDIF
                         ENDIF
                       ENDIF
                    END DO
                  ENDIF
                END DO

*               Normalise totals by number of contrbuting pixels
                counts = 0
                ymax = .1
                DO i = 1, nx
                   IF ( INT(xsample(i)) .GT. 0 ) THEN
                      counts = counts + 1
                      sample(counts) = sample(i)/xsample(i)
                      IF ( sample(counts) .GT. ymax )
     :                            ymax = sample ( counts )
                      xsample(counts) = DBLE ( i )
                   ENDIF
                END DO

*               If some good pixels found then
                IF ( counts .GT. 0 ) THEN
                   xmin = FLOAT ( nx )
                   xmax = 1.
                   DO ip = 1, counts
                      x_points (ip) = REAL( xsample(ip))
                      y_points (ip) = REAL(sample(ip))
                      IF ( x_points(ip) .LT. xmin )
     :                   xmin = x_points(ip)
                      IF ( x_points(ip) .GT. xmax )
     :                   xmax = x_points(ip)
                   END DO
                   options = 0

*                  Plot graph if interactive mode
                   IF ( interactive )
     :                CALL ECH_PLOT_GRAPH( COUNTS, X_POINTS, Y_POINTS,
     :                     XMIN, XMAX, 0.0, YMAX, 'Sample',
     :                     'Point profile', TITLE, 0.0, 0.0,
     :                     OPTIONS, 'POINTS', STATUS )

                   DO i = 1, maximum_poly
                      temp_coeffs ( i ) = 0.0
                   END DO
                   DO ip = 1, counts
                      x_points (ip) = SQRT ( MAX ( 1.,
     :                                             REAL(sample(ip)) ) )
                   END DO

*                  If polynomial fit requested then fit to data
                   IF ( obj_npoly .GT.1 ) THEN
                      CALL ECH_FITTER ( fitter, obj_npoly,
     :                                  temp_coeffs, counts,
     :                                  xsample, sample, x_points,
     :                                  0, 5.0, status )

*                  Else calculate mean
                   ELSE
                      DO iv = 1, counts
                         temp_coeffs ( 1 ) = temp_coeffs ( 1 ) +
     :                                                   sample ( iv )
                      END DO
                      temp_coeffs ( 1 ) = temp_coeffs ( 1 ) /
     :                                                    DBLE(counts)
                   ENDIF


*                  Evaluate fit at all x positions
                   ip = 0
                   DO i = INT(xmin), INT(xmax),
     :                            MAX ( 1,INT(xmax-xmin)*2/maxdata )
                      ip = ip + 1
                      x_points ( ip ) = FLOAT ( i )
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                                 temp_coeffs, 1, x_points(ip),
     :                                 value, status )
                      pfit ( ip ) = DBLE ( value )
                      y_points ( ip ) = REAL(pfit(ip))
                   END DO

                   options = grph_overlay +
     :                       grph_set_colour +
     :                       grph_set_line_style

*                  If interactive then overplot on graph
                   IF ( interactive ) THEN
                   CALL ECH_PLOT_GRAPH( IP, X_POINTS, Y_POINTS,
     :                  0.0, 0.0, 0.0, 0.0, 'RED', 'DASH', ' ',
     :                  0.0, 0.0, OPTIONS, 'LINES', STATUS )

*                    Get user option input
                   CALL ECH_REPORT( 0, ' ' )
                   CALL ECH_REPORT( 0,
     :                  ' Use +- to change fit degree' )
                   CALL ECH_REPORT( 0,
     :               '     A to exit and continue non-interactively' )
                   CALL ECH_REPORT( 0,
     :                  '     F to change fitter' )
                   CALL ECH_REPORT( 0,
     :                  '     Q to quit fitting' )
                   CALL ECH_REPORT( 0,
     :                  '     E to exit when fit is OK' )
                   CALL ECH_REPORT( 0, ' ' )
                   CALL ECH_READ_GRPH_CURSOR( status )
                   IF ( user_input_char .EQ. 'E' ) refit = .FALSE.

                   IF ( user_input_char .EQ. 'Q' ) THEN
                           CALL ECH_REPORT( 0,
     :       ' Further fitting abandoned at user request.' )
                          GO TO 999
                   ENDIF

                   IF ( user_input_char .EQ. 'A' ) THEN
                      refit = .FALSE.
                      interactive = .FALSE.
                   ENDIF

                   IF ( user_input_char .EQ. 'F' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, 1, rdummy,
     :                     rdummy, ECH__NEXT_FITTER )
                   ENDIF

                   IF ( user_input_char .EQ. '+' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, maximum_poly,
     :                     rdummy,rdummy, ECH__INC_NUMCOEFF )
                   ENDIF
                   IF ( user_input_char .EQ. '-' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, maximum_poly,
     :                     rdummy,rdummy, ECH__DEC_NUMCOEFF )
                   ENDIF
                   ENDIF

*                  Save fitted coeffs
                   DO iord = sord,eord
                      DO iv = 1, maximum_poly
                         poly_profiles(iv,index,iord) = 0.0
                      END DO
                      DO iv = 1, obj_npoly
                         poly_profiles(iv,index,iord) = temp_coeffs(iv)
                      END DO
                   END DO

*               Else report no pixels available at this offset
                ELSE
                   IF ( counts .EQ. 0 ) CALL ECH_REPORT( 0,
     :   '            No pixels sampled at this offset from trace.' )

                ENDIF
              END DO
           END DO
         ENDIF
        END DO

*       Repeat above for offsets below trace
       DO iii = 0, ABS( dek_below )
         IF ( obj_mask( -iii,sord ) .NE. 0 .OR.
     :        obj_mask( -iii+1, sord ) .NE. 0 ) THEN

           IF ( III .EQ. 0 ) THEN
              TITLE = ' Calculating object pixel fractions at' //
     :                ' trace centre.'

           ELSE IF ( III .EQ. 1 ) THEN
              TITLE = ' Calculating object pixel fractions at' //
     :                ' 1 pixel below trace.'

           ELSE IF ( III .GT. 1 ) THEN
              CALL CHR_ITOC( III, REF_STR1, NCHAR1 )
              TITLE = ' Calculating object pixel fractions at ' //
     :                REF_STR1( :NCHAR1 ) // ' pixels below trace.'
           END IF
           CALL ECH_REPORT( 0, TITLE )

           DO ii = 0, substeps-1
            IF ( iii .GT. 0 .OR. ii .GT. 0 ) THEN
              index = -iii * substeps - ii
              IF ( INDEX .EQ. 0 ) THEN
                 TITLE = 'Object pixel fractions at trace centre.'

              ELSE IF ( INDEX .EQ. -1 ) THEN
                 TITLE = 'Object pixel fractions at 1 substep' //
     :                   ' below trace.'
              ELSE
                 CALL CHR_ITOC( -INDEX, REF_STR1, NCHAR1 )
                 TITLE = 'Object pixel fractions at ' //
     :                REF_STR1( :NCHAR1 ) // ' substeps below trace.'
              END IF

*            Loop while re-fit flag set
              refit = .TRUE.
              DO WHILE ( refit )

*              If not interactive, then clear re-fit flag now
                IF ( .NOT. interactive ) refit = .FALSE.

                DO i = 1, nx
                  xsample(i)=0.
                  sample(i)=0.
                END DO
                counts = 0
                DO iord = sord, eord
                  IF ( trace_polynomial ( 1,iord ) .NE.
     :                         ECH__BAD_DOUBLE ) THEN
                    CALL ECH_CALC_TRACE( nx, maximum_poly,
     :                   trace_polynomial( 1, iord ),
     :                   x_trace_coord, y_trace_coord,
     :                   status  )

                    DO i = 1, nx
                       IF ( tcount ( i,iord ) .GT. 0 ) THEN
                         iy = INT ( y_trace_coord ( i ) + 0.5  -
     :                             FLOAT ( ii ) / FLOAT ( substeps )
     :                                         ) - iii

                         dist = ABS ( - FLOAT ( ii ) /
     :                                  FLOAT ( substeps ) -
     :                         FLOAT ( iii ) +
     :                       REAL(y_trace_coord ( i ) ) )

                         IF ( ABS ( FLOAT ( iy ) - dist ) .LE.
     :                                  1 / FLOAT ( substeps ) ) THEN

                           IF ( iy .GT. 0 .AND. iy .LE. ny  ) THEN
                            IF ( quality( i, iy ) .EQ. 0 .AND.
     :                           IMAGE( I, IY ) .NE. ECH__BAD_REAL )
     :                           THEN
                             IF ( counts+2 .LT. maxdata ) THEN
                                value = image( i, iy )
     :                                    - sky_model ( i, iii,iord )
                                IF ( value .GT. 0. ) THEN
                                  counts = counts + 1
                                  sample ( i ) = sample(i)+value /
     :                                         ( total ( i,iord ) *
     :                                   FLOAT ( objpix ) /
     :                                   FLOAT ( tcount ( i,iord ) ) )
                                  xsample ( i ) = xsample(i)+1.
                                  IF ( tcount(i,iord) .GE.
     :                                                objpix*2/3 ) THEN
                                   sample ( i ) =
     :                               sample ( i ) + value /
     :                                         total ( i,iord )
                                   xsample ( i ) = xsample(i)+1.
                                  ENDIF
                                ENDIF
                             ENDIF
                            ENDIF
                           ENDIF
                         ENDIF
                       ENDIF
                    END DO
                  ENDIF
                END DO

                counts = 0
                ymax = 0.1
                DO i = 1, nx
                   IF ( INT(xsample(i)) .GT. 0 ) THEN
                      counts = counts + 1
                      sample(counts) = sample(i)/xsample(i)
                      xsample(counts) = DBLE ( i )
                      IF ( sample(counts) .GT. ymax )
     :                            ymax = sample ( counts )
                   ENDIF
                END DO
                IF ( counts .GT. 0 ) THEN

                   xmin = FLOAT ( nx )
                   xmax = 1.
                   DO ip = 1, counts
                      x_points (ip) = REAL(xsample(ip))
                      y_points (ip) = REAL(sample(ip))
                      IF ( x_points(ip) .LT. xmin )
     :                   xmin = x_points(ip)
                      IF ( x_points(ip) .GT. xmax )
     :                   xmax = x_points(ip)
                   END DO
                   options = 0
                   IF ( interactive )
     :                CALL ECH_PLOT_GRAPH( COUNTS, X_POINTS, Y_POINTS,
     :                     XMIN, XMAX, 0.0, YMAX, 'Sample',
     :                     'Point profile', TITLE, 0.0, 0.0,
     :                     OPTIONS, 'POINTS', STATUS )

                   DO i = 1, maximum_poly
                      temp_coeffs ( i ) = 0.0
                   END DO
                   DO ip = 1, counts
                      x_points( ip ) = SQRT( MAX( 1.,
     :                      REAL( sample( ip ) ) ) )
                   END DO
                   IF ( obj_npoly .GT.1 ) THEN
                      CALL ECH_FITTER( fitter, obj_npoly,
     :                     temp_coeffs, counts,
     :                     xsample, sample, x_points,
     :                     0, 5.0, status )
                   ELSE
                      DO iv = 1, counts
                         temp_coeffs( 1 ) = temp_coeffs( 1 ) +
     :                                      sample( iv )
                      END DO
                      temp_coeffs( 1 ) = temp_coeffs( 1 ) /
     :                                   DBLE( counts )
                   ENDIF
                   ip = 0
                   DO i = INT(xmin), INT(xmax),
     :                             MAX ( 1,INT(xmax-xmin)*2/maxdata )
                      ip = ip + 1
                      x_points ( ip ) = FLOAT ( i )
                      CALL ECH_FEVAL( fitter, obj_npoly,
     :                     temp_coeffs, 1, x_points( ip ),
     :                     value, status )
                      pfit( ip ) = DBLE( value )
                      y_points( ip ) = REAL( pfit( ip ) )
                   END DO

                   IF ( interactive ) THEN
                      options = grph_overlay +
     :                       grph_set_colour +
     :                       grph_set_line_style
                      CALL ECH_PLOT_GRAPH( IP, X_POINTS, Y_POINTS,
     :                  0.0, 0.0, 0.0, 0.0, 'RED', 'DASH', ' ',
     :                  0.0, 0.0, OPTIONS, 'LINES', STATUS )

*               Get option input.
                   CALL ECH_REPORT( 0, ' ' )
                   CALL ECH_REPORT( 0,
     :                ' Use +- to change fit degree' )
                   CALL ECH_REPORT( 0,
     :                '     A to exit and continue non-interactively' )
                   CALL ECH_REPORT( 0,
     :                '     F to change fitter' )
                   CALL ECH_REPORT( 0,
     :                '     Q to quit fitting' )
                   CALL ECH_REPORT( 0,
     :                '     E to exit when fit is OK' )
                   CALL ECH_REPORT( 0, ' ' )
                   CALL ECH_READ_GRPH_CURSOR( status )
                   IF ( user_input_char .EQ. 'E' ) refit = .FALSE.

                   IF ( user_input_char .EQ. 'Q' ) THEN
                           CALL ECH_REPORT( 0,
     :       ' Further fitting abandoned at user request.' )
                          GO TO 999
                   ENDIF

                   IF ( user_input_char .EQ. 'A' ) THEN
                      refit = .FALSE.
                      interactive = .FALSE.
                   ENDIF

                   IF ( user_input_char .EQ. 'F' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, 1, rdummy,
     :                     rdummy, ECH__NEXT_FITTER )
                   ENDIF

                   IF ( user_input_char .EQ. '+' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, maximum_poly,
     :                     rdummy,rdummy, ECH__INC_NUMCOEFF )
                   ENDIF
                   IF ( user_input_char .EQ. '-' ) THEN
                      CALL ECH_FEVAL ( fitter, obj_npoly,
     :                     ddummy, maximum_poly,
     :                     rdummy,rdummy, ECH__DEC_NUMCOEFF )
                   ENDIF

                   ENDIF

                   DO iord = sord,eord
                      DO iv = 1, maximum_poly
                         poly_profiles(iv,index,iord) = 0.0
                      END DO
                      DO iv = 1, obj_npoly
                         poly_profiles(iv,index,iord) = temp_coeffs(iv)
                      END DO
                   END DO


                ELSE
                   IF ( counts .EQ. 0 ) CALL ECH_REPORT( 0,
     :   '            No pixels sampled at this offset from trace.' )
                ENDIF
              END DO
            ENDIF
           END DO
         ENDIF
       END DO
      END DO

 999  CONTINUE

      END
