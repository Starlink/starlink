      SUBROUTINE snx_EZRXY (XDRA, YDRA, NPTS, XLAB, YLAB, GLAB)

*+
*
*  - - - - - -
*   E Z R X Y
*  - - - - - -
*
*  Version of NCAR AUTOGRAPH routine EZXY which includes
*  axis labelling.
*
*  Given:
*     XDRA     r()     array of x data
*     YDRA     r()     array of y data
*     NPTS     i       number of points
*     XLAB     c       x axis label (bottom)
*     YLAB     c       y axis label (left)
*     GLAB     c       graph label (top)
*
*  Called:  snx_AGLAB, EZXY
*
*  P T Wallace   Starlink   April 1986
*
*+

      IMPLICIT NONE

      REAL XDRA,YDRA
      INTEGER NPTS
      CHARACTER*(*) XLAB,YLAB,GLAB



*  Set up axis labels
      CALL snx_AGLAB('B',XLAB)
      CALL snx_AGLAB('L',YLAB)
      CALL snx_AGLAB('T',GLAB)

*  Plot the graph
      CALL EZXY(XDRA,YDRA,NPTS,CHAR(0))

      END
