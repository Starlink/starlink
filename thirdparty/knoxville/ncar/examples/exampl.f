      PROGRAM EXAMPL
*+
*   Simple program to illustrate the use of AUTOGRAPH with SGS:
*      a set of X,Y points is plotted with AUTOGRAPH and then the mean 
*      Y value marked with a horizontal line drawn with SGS.
*+

      INTEGER N
      PARAMETER( N = 10 )

      CHARACTER * 20 WS
      INTEGER I, IBASE, ISTAT
      REAL X( N ), Y( N ), WIND( 4 ), VIEWP( 4 )
      REAL TOTAL, AMEAN, ANDC, XST, XEN

      DATA X / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
      DATA Y / 1, 2, 4, 3, 5, 6, 4, 5, 6, 10 /

*  Get the workstation name.
      WRITE ( *, '( 1X, A )', ERR=999 ) 'Workstation'
      READ ( *, '( A )', ERR=999 ) WS

*  Open SGS.
      CALL SGS_OPEN( WS, IBASE, ISTAT )
      IF ( ISTAT .NE. 0 ) GO TO 999

*  Obtain the viewport limits and set the limits for AUTOGRAPH.
      CALL GQNT( 1, IERR, WIND, VIEWP )
      CALL AGSETP( 'GRAPH.', VIEWP, 4 )

*  Draw the graph.
      CALL EZXY( X, Y, N, 'Autograph example_$' )

*  Find the mean Y value.
      TOTAL = 0.0

      DO 10 I = 1, N
         TOTAL = TOTAL + Y( I )
 10   CONTINUE

      AMEAN = TOTAL / REAL( N )

*  Convert the end points of thhe line to fractional co-ordinates (i.e. NDC)
*  before restoring SGS state.
      ANDC = CUFY( AMEAN )
      XST = CUFX( 1.0 )
      XEN = CUFX( 10.0 )

*  Re-establish the SGS zone
      CALL SGS_SELZ( IBASE, ISTAT )

*  Set the world co-ordinates to match the NDC (i.e. fractional co-ordinates).
      CALL SGS_SW( VIEWP( 1 ), VIEWP( 2 ), VIEWP( 3 ), VIEWP( 4 ), 
     :             ISTAT )

*  Draw the line and close SGS.
      CALL SGS_LINE( XST, ANDC, XEN, ANDC )
      CALL SGS_CLOSE

 999  CONTINUE

      END
