      SUBROUTINE ECH_EXTR_QUICK(
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           NO_ARC,
     :           NO_ERRORS,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           DEK_BELOW, DEK_ABOVE,
     :           READOUT, PHOTON,
     :           MAX_SKY_PIXELS,
     :           IMAGE,
     :           IMAGE_ERR,
     :           QUALITY,
     :           ARC_FRAME,
     :           SPEC,
     :           SPECSIG,
     :           ARCSPEC,
     :           ARCSIG,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_QUICK

*  Purpose:
*     Quick-look extraction using simple sum and mean sky.

*  Description:
*     This routine extracts the profile for a single increment in wavelength
*     along with raw intensities, errors, and arc intensity.

*  Method:
*     A simple summation of all 'good' pixels in the x-increment is used.
*     Bad pixels are excluded from the sums which are then rescaled to
*     re-normalise for any absent pixels.

*  Invocation:
*     CALL ECH_EXTR_QUICK(
*    :     NX,
*    :     NY,
*    :     N_ORDERS,
*    :     NO_ARC,
*    :     NO_ERRORS,
*    :     OBJ_MASK,
*    :     SKY_MASK,
*    :     MAXIMUM_POLY,
*    :     TRACE_POLYNOMIAL,
*    :     DEK_BELOW,
*    :     DEK_ABOVE,
*    :     READOUT,
*    :     PHOTON,
*    :     MAX_SKY_PIXELS,
*    :     IMAGE,
*    :     IMAGE_ERR,
*    :     QUALITY,
*    :     ARC_FRAME,
*    :     SPEC,
*    :     SPECSIG,
*    :     ARCSPEC,
*    :     ARCSIG,
*    :     X_TRACE_COORD,
*    :     Y_TRACE_COORD,
*    :     STATUS
*    :    )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     NO_ARC = LOGICAL (Given)
*        TRUE if no arc frame provided.
*     NO_ERRORS = LOGICAL (Given)
*        TRUE if no error array provided.
*     OBJ_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     SKY_MASK = INTEGER (Given)
*        Status of each pixel across profile.
*     DEK_BELOW = INTEGER (Given)
*        Status of each pixel across profile.
*     DEK_ABOVE = INTEGER (Given)
*        Status of each pixel across profile.
*     READOUT = REAL (Given)
*        Readout noise level in counts.
*     PHOTON = REAL (Given)
*        Photon to ADU conversion factor.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum extent of sky and therefore dekker.
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     IMAGE_ERR = REAL (Given)
*        Input errors frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Input quality frame image of dimensions nx columns and ny rows.
*     ARC_FRAME = REAL (Given)
*        Input ARC frame image of dimensions nx columns and ny rows.
*     SPEC = REAL (Returned)
*        Calculated object pixel intensities.
*     SPECSIG = REAL (Returned)
*        Calculated object pixel errors.
*     ARCSPEC = REAL (Returned)
*        Calculated arc intensities at increment.
*     ARCSIG = REAL (Returned)
*        Calculated arc errors.
*     X_TRACE_COORD = DOUBLE (Temporary Workspace)
*        X coordinates of order trace.
*     Y_TRACE_COORD = DOUBLE (Temporary Workspace)
*        Y coordinates of order trace.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Coefficients of fit describing order path across frame.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate minimum variance floor
*     Exit right away if the order is disabled
*     Determine object pixel offset limits
*     Calculate coordinates of order trace across frame
*     Loop thru increments along the order
*        Clear totals
*        If pixel is within frame boundaries and part of object then
*              If ok to extract this pixel then add to sky estimate
*        Endif
*        Loop thru all contributing pixels in increment
*           If pixel is within frame boundaries and part of object then
*                 If ok to extract this pixel then
*                    Calculate sky, object and variance for pixel
*                    Add to running totals for object and arc
*                 Else increment 'hit' counter
*                 Endif
*           Endif
*        End loop
*        Re-Normalise estimate to compensate for dead pixels
*        Else set variance negative
*        Endif
*     End loop
*     Overlay limits of dekker on image if available
*     Overlay object limits on image if available

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
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER MAXIMUM_POLY
      INTEGER MAX_SKY_PIXELS
      REAL ARC_FRAME( NX, NY )
      INTEGER IORD
      INTEGER N_ORDERS
      REAL READOUT
      REAL PHOTON
      LOGICAL NO_ERRORS
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
      LOGICAL GOT_QUALITY
      LOGICAL NO_ARC
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      REAL IMAGE_ERR( NX, NY )
      BYTE QUALITY( NX, NY )

*  Arguments Returned:
      REAL ARCSPEC( NX,N_ORDERS )
      REAL ARCSIG( NX,N_ORDERS )
      REAL SPEC( NX,N_ORDERS )
      REAL SPECSIG( NX,N_ORDERS )
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2: MAX_SKY_PIXELS/2, N_ORDERS )

*  Workspace:
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL VAR
      REAL VAR0
      REAL CMIN
      REAL DAT
      REAL STAR
      REAL SKY_ESTIMATE
      REAL INPUT_VAR
      REAL VMIN
      REAL VMAX

      INTEGER NBAD
      INTEGER I
      INTEGER SKY_COUNT
      INTEGER USED_COUNT
      INTEGER OBJ_COUNT
      INTEGER IPROFILE
      INTEGER YCOORD
      INTEGER OBJ_BELOW
      INTEGER IWAVE
      INTEGER OBJ_ABOVE
      INTEGER LOPLOT
      INTEGER HIPLOT
      INTEGER OPTIONS
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL GOT_AN_ARC
      LOGICAL PIXEL_OK

      CHARACTER*80 GTITLE
      CHARACTER*8 REF_STR1
      CHARACTER*8 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Calculate minimum variance floor.
      VAR0 = READOUT * READOUT
      IF ( PHOTON .EQ. 0.0 ) PHOTON = 1.0
      CMIN = ( V0_ALPHA * V0_BETA / ( V0_BETA-1. ) ) **2. /
     :       PHOTON - VAR0 * PHOTON
      CMIN = MAX( 0., CMIN )
      GOT_QUALITY = .TRUE.
      GOT_AN_ARC = .NOT. NO_ARC

      DO IORD = 1, N_ORDERS
         NBAD = 0

*     Next order if this one is disabled.
         IF ( TRACE_POLYNOMIAL( 1, IORD ) .EQ. ECH__BAD_DOUBLE ) THEN
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            REPORT_STRING = ' Order ' // REF_STR1( :NCHAR1 ) //
     :            ' disabled: no extraction.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            GO TO 800
         END IF

*     Determine object pixel offset limits.
         OBJ_BELOW = 999
         OBJ_ABOVE = -999
      DO i = -max_sky_pixels / 2, max_sky_pixels / 2
         IF ( obj_mask( i,iord ) .NE. 0 .AND. obj_below .GT. i )
     :      obj_below = i
         IF ( obj_mask( i,iord ) .NE. 0 .AND. obj_above .LT. i )
     :      obj_above = i
      END DO
      obj_count = obj_above - obj_below + 1
      IF ( obj_count .GT. 0 ) THEN

*      Calculate coordinates of order trace across frame
      CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :     TRACE_POLYNOMIAL( 1, IORD ), X_TRACE_COORD, Y_TRACE_COORD,
     :     STATUS )

*      Loop thru increments along the order
       DO iwave = 1, nx

*         Clear totals
          sky_count = 0
          obj_count = 0
          sky_estimate = 0.0
          spec(iwave,iord) = 0.
          specsig(iwave,iord) = 0.
          IF ( got_an_arc ) THEN
             arcspec(iwave,iord) = 0.
             arcsig(iwave,iord) = 0.
          ENDIF

          DO iprofile = dek_below(iord), dek_above(iord)
             IF ( SKY_MASK( IPROFILE, IORD ) .EQ.1 )
     :          SKY_COUNT = SKY_COUNT + 1
             YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IPROFILE

*         If pixel is within frame boundaries and part of object then
             IF ( ycoord .GT. 0 .AND. ycoord .LE. ny ) THEN
                IF( sky_MASK(iprofile,iord).EQ. 1 ) THEN

*                 Check pixel quality.
                     PIXEL_OK = .TRUE.
                     IF ( GOT_QUALITY ) THEN
                        IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
                           PIXEL_OK = .FALSE.
                        END IF
                     END IF

*                 Check for BAD value pixel.
                     IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :                  PIXEL_OK = .FALSE.

*               If ok to extract this pixel then add to sky estimate
                   IF ( PIXEL_OK ) THEN
                     sky_estimate = sky_estimate + image(iwave,ycoord)
                   ENDIF
                ENDIF
             ENDIF
          END DO
          sky_estimate = sky_estimate / FLOAT ( MAX ( 1,sky_count ) )

*         Loop thru all contributing pixels in increment
          USED_COUNT = 0
          DO IPROFILE = OBJ_BELOW, OBJ_ABOVE
             IF ( OBJ_MASK( IPROFILE, IORD ).EQ.1 )
     :            OBJ_COUNT = OBJ_COUNT + 1
             YCOORD = INT( Y_TRACE_COORD( IWAVE ) + 0.5 ) + IPROFILE

*            If pixel is within frame boundaries and part of object then
             IF ( ycoord .GT. 0 .AND. ycoord .LE. ny ) THEN
                IF( obj_MASK(iprofile,iord).EQ. 1 ) THEN

*                 Check pixel quality.
                     PIXEL_OK = .TRUE.
                     IF ( GOT_QUALITY ) THEN
                        IF ( QUALITY( IWAVE, YCOORD ) .GT. 0 ) THEN
                           PIXEL_OK = .FALSE.
                        END IF
                     END IF

*                 Check for BAD value pixel.
                     IF ( IMAGE( IWAVE, YCOORD ) .EQ. ECH__BAD_REAL )
     :                  PIXEL_OK = .FALSE.

*                  If ok to extract this pixel then
                   IF ( pixel_ok ) THEN

*                     Calculate sky, object and variance for pixel
                      dat = image( iwave, ycoord )
                      used_count = used_count + 1
                      star = dat - sky_estimate
                      IF ( no_errors ) THEN
                         input_var = 0.0

                      ELSE
                         input_var = image_err(iwave,ycoord)**2.
                      ENDIF
                      var =  ( var0 + input_var )
     :           + MAX( cmin, ABS(dat), ABS(sky_estimate) ) /
     :                    MAX(1.,photon)

*                     Add to running totals for object and arc
                      spec(iwave,iord) = spec(iwave,iord) + star
                      specsig(iwave,iord) = specsig(iwave,iord)+var
                      IF ( got_an_arc ) THEN
                         IF ( ARC_FRAME( IWAVE, YCOORD ) .NE.
     :                        ECH__BAD_REAL ) THEN
                         arcspec(iwave,iord) = arcspec(iwave,iord) +
     :                                      arc_frame(iwave,ycoord)
                         arcsig ( iwave,iord ) =
     :                      SQRT ( ABS ( arcspec ( iwave,iord ) ) )

                         ELSE
                            ARCSPEC( IWAVE, IORD ) = 0.0
                            ARCSIG( IWAVE, IORD ) = 0.0
                         END IF
                      ENDIF

*                  Else increment 'hit' counter
                   ELSE
                      NBAD = NBAD + 1
                   ENDIF
                ENDIF
             ENDIF
          END DO

*         Re-Normalise estimate to compensate for dead pixels
          IF ( used_count .GT. 0 ) THEN
             spec(iwave,iord) = spec(iwave,iord)/FLOAT (used_count ) *
     :                                  FLOAT ( obj_count ) * photon
             specsig(iwave,iord) = ABS ( specsig(iwave,iord) /
     :                                FLOAT (used_count ) *
     :                                FLOAT ( obj_count ) )
     :                                     * photon * MAX(1.,photon)
             IF ( got_an_arc ) THEN
                arcspec(iwave,iord) = arcspec(iwave,iord) /
     :                                  FLOAT (used_count ) *
     :                                  FLOAT ( obj_count ) * photon
                arcsig(iwave,iord) = arcsig(iwave,iord) /
     :                                  FLOAT (used_count ) *
     :                                  FLOAT ( obj_count )
     :                                     * photon * MAX(1.,photon)
             ENDIF

*         Else set variance negative
          ELSE
            specsig(iwave,iord) = -1.
          ENDIF
       END DO

*      Overlay limits of dekker on image if available
            loplot = INT(y_trace_coord(nx/2))+dek_below(iord)-10
            hiplot = INT(y_trace_coord(nx/2))+dek_above(iord)+10
            LOPLOT = MAX( 1, LOPLOT )
            HIPLOT = MIN( HIPLOT, NY )
            VMIN = 0.0
            VMAX = 0.0
            OPTIONS = GRPH_GEN_XAXIS + GRPH_CALC_MINMAX
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            GTITLE = ' Overlaid dekker/object limits for order ' //
     :            REF_STR1( :NCHAR1 )

            CALL ECH_PLOT_GRAPH( nx * ny, image, image,
     :           1.0, FLOAT( nx ), FLOAT( loplot ), FLOAT( hiplot ),
     :           'X pixels', 'Y pixels', GTITLE,
     :           VMIN, VMAX, options, 'IMAGING', status )
          options = grph_overlay
          DO iwave = 1, nx
             y_coord( iwave ) = y_trace_coord( iwave ) +
     :             dek_below( iord )
             x_coord( iwave ) = FLOAT( iwave )
          END DO

          CALL ECH_GR_SET_COLOUR( COL_RED )
          options = grph_overlay
          CALL ECH_PLOT_GRAPH( nx, x_coord, y_coord,
     :         1.0, FLOAT( nx ), 1.0, FLOAT( ny ),
     :         ' ', ' ', ' ', 255.0, 0.0, options, '*', status )
          DO iwave = 1, nx
             y_coord( iwave ) = y_trace_coord( iwave ) +
     :             dek_above( iord )
          END DO
          options = grph_overlay
          CALL ECH_PLOT_GRAPH( nx, x_coord, y_coord,
     :         1.0, FLOAT( nx ), 1.0, FLOAT( ny ),
     :         ' ', ' ', ' ', 255.0, 0.0, options, '*', status )

*      Overlay object limits on image if available
*      Set colour to BLUE for these plots.
          CALL ECH_GR_SET_COLOUR( COL_BLUE )

*      Overlay the lower object-limit path.
          DO IWAVE = 1, NX
             Y_COORD( IWAVE ) = Y_TRACE_COORD( IWAVE ) + OBJ_BELOW
          END DO
          options = grph_overlay
          CALL ECH_PLOT_GRAPH( nx, x_coord, y_coord,
     :         1.0, FLOAT( nx ), 1.0, FLOAT( ny ),
     :         ' ', ' ', ' ', 1.0, 0.0, options, 'POINTS', status )

*      Overlay the upper object-limit path.
          DO IWAVE = 1, NX
             Y_COORD( IWAVE ) = Y_TRACE_COORD( IWAVE ) + OBJ_ABOVE
          END DO
          options = grph_overlay
          CALL ECH_PLOT_GRAPH( NX, X_COORD, Y_COORD,
     :         1.0, FLOAT( NX ), 1.0, FLOAT( NY ),
     :         ' ', ' ', ' ', 1.0, 0.0, OPTIONS, 'POINTS', STATUS )


            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            IF ( NBAD .GT. 1 ) THEN
               CALL CHR_ITOC( NBAD, REF_STR2, NCHAR2 )
               REPORT_STRING = ' Rejected ' // REF_STR2( :NCHAR2 ) //
     :              ' pixels in order ' // REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )

            ELSE IF ( NBAD .EQ. 1 ) THEN
               REPORT_STRING = ' Rejected one pixel in order ' //
     :               REF_STR1( :NCHAR1 ) // '.'
               CALL ECH_REPORT( 0, REPORT_STRING )
            ENDIF
            REPORT_STRING = ' Extracted order ' //
     :            REF_STR1( :NCHAR1 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE
            CALL CHR_ITOC( IORD, REF_STR1, NCHAR1 )
            REPORT_STRING = ' No pixels marked as Object in' //
     :            ' order ' // REF_STR1( :NCHAR1 ) //
     :            ': no extraction.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         ENDIF

  800    CONTINUE
      END DO

*  Reset plot colour to black.
      CALL ECH_GR_SET_COLOUR( COL_BLACK )

  999 CONTINUE

      END
