      SUBROUTINE ECH_PLOT_TRACES( NNX, NNY, A_FRAME, N_ORDERS,
     :           MAXIMUM_POLY, POLYNOMIALS, X_TRACE_COORD,
     :           Y_TRACE_COORD, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_PLOT_TRACES

*  Purpose:
*     Plot order traces.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants and Variables:
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_GRAPHICS.INC'

*  Arguments:
      INTEGER NNX
      INTEGER NNY
      REAL A_FRAME( NNX, NNY )
      INTEGER N_ORDERS
      INTEGER MAXIMUM_POLY
      DOUBLE PRECISION POLYNOMIALS( MAXIMUM_POLY, N_ORDERS )
      DOUBLE PRECISION X_TRACE_COORD( NNX )
      DOUBLE PRECISION Y_TRACE_COORD( NNX )
      INTEGER STATUS

*  Local Constants:
      INTEGER PLOT_ARRY
      PARAMETER ( PLOT_ARRY = 401 )

      REAL FLAG
      PARAMETER ( FLAG = -1.0E20 )

*  Local Variables:
      REAL X_TO_PLOT( PLOT_ARRY )
      REAL Y_TO_PLOT( PLOT_ARRY )
      REAL VMIN
      REAL VMAX

      INTEGER ORDER_NUMBER
      INTEGER OPTIONS
      INTEGER I
      INTEGER INDEX
      INTEGER GSTATUS

      LOGICAL DONE_AXES
*.

*  Display the image if requested.
      IF ( .NOT. GRAPHICS_SETUP ) THEN
         GSTATUS = 0
         CALL ECH_SETUP_GRAPHICS( GSTATUS )
      END IF
      IF ( IMG_DISPLAY ) THEN
         OPTIONS = GRPH_GEN_XAXIS + GRPH_CALC_MINMAX
         VMIN = 0.0
         VMAX = 0.0
         CALL ECH_PLOT_GRAPH( NNX * NNY, A_FRAME, A_FRAME,
     :        1., FLOAT( NNX ), 1., FLOAT( NNY ), 'X pixels',
     :        'Y pixels', 'Overlaid order traces',
     :        VMIN, VMAX, OPTIONS, 'IMAGING',
     :        STATUS )
         DONE_AXES = .TRUE.
         CALL ECH_GR_SET_COLOUR( COL_RED )

      ELSE
         DONE_AXES = .FALSE.
         CALL ECH_GR_SET_COLOUR( COL_BLACK )
      END IF

      DO ORDER_NUMBER = 1, N_ORDERS
         IF ( POLYNOMIALS( 1, ORDER_NUMBER ) .GT. FLAG / 2.0 ) THEN
            CALL ECH_CALC_TRACE( NNX, MAXIMUM_POLY,
     :           POLYNOMIALS( 1, ORDER_NUMBER ), X_TRACE_COORD,
     :           Y_TRACE_COORD, STATUS )
            INDEX = 0
            DO I = 1, NNX, MAX( 1, NNX / ( PLOT_ARRY / 2 ) )
               INDEX = INDEX + 1
               X_TO_PLOT( INDEX ) = REAL( X_TRACE_COORD( I ) )
               Y_TO_PLOT( INDEX ) = REAL( Y_TRACE_COORD( I ) )
            END DO

            IF ( DONE_AXES ) THEN
               OPTIONS = GRPH_OVERLAY

            ELSE
               OPTIONS = 0
               DONE_AXES = .TRUE.
            END IF
            CALL ECH_PLOT_GRAPH( INDEX, X_TO_PLOT, Y_TO_PLOT, 1.0,
     :           FLOAT( NNX ), 1.0, FLOAT( NNY ), 'X pixels',
     :           'Y pixels', 'Fitted Order traces', 0.0, 0.0, OPTIONS,
     :           'LINES', STATUS )
         END IF
      END DO

*  Reset plot colour to black.
      IF ( IMG_DISPLAY ) THEN
         CALL ECH_GR_SET_COLOUR( COL_BLACK )
      END IF

      END
