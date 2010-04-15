      SUBROUTINE GRFA(N,X,Y)
*+
*     - - - - - -
*       G R F A     (GKS emulation of GRPCKG)
*     - - - - - -
*
*   Fill area
*
*   Given
*      N        i     Number of points
*      X        i     X coordinates (scaled)
*      Y        i     Y coordinates (scaled)
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRXBUF   r()   Line buffer X
*      GRYBUF   r()   Line buffer Y
*      GRBPT    i     Buffer pointer
*      GRXSCL   r()   Coordinate scale X
*      GRYSCL   r()   Coordinate scale Y
*      GRXORG   r()   X origin
*      GRYORG   r()   Y origin
*      GRVPVI   l()   Viewport visible
*
*   Constants from GRECOM.INC
*      GRBSIZ   i     Size of line buffer
*
*   Deficiencies
*      The maximum number of points is limited by the size of the line buffers
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INTEGER N
      REAL X(N),Y(N)

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      INTEGER I
      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRFA - No PGPLOT device open', GRNODO)
      ELSE

         IF (GRVPVI(GRCIDE)) THEN
*        Clear out any pending polyline
            CALL GRFLU0

*        Copy data to buffers
            DO 10 I = 1,MIN(N,GRBSIZ)
               GRXBUF(I) = X(I) * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
               GRYBUF(I) = Y(I) * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
   10       CONTINUE

*         Send buffer to GKS
            CALL GFA(MIN(N,GRBSIZ),GRXBUF,GRYBUF)
         END IF
      END IF
      END
