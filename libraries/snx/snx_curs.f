      SUBROUTINE snx_CURS (X,Y,N)

*+
*
*  - - - - -
*   C U R S
*  - - - - -
*
*  Read a cursor position
*
*  Given:
*     X,Y    r     where cursor is to be preset to if possible
*
*  Returned:
*     X,Y    r     where cursor was when choice was made
*     N      i     choice selected
*
*  X,Y are USER coordinates (i.e. data coordinates)
*
*  Called: PLOTIT,
*          sgs_FLUSH, sgs_SETCU, sgs_REQCU,
*          snx_TO, snx_AGCS, snx_AGUGX, snx_AGUGY, snx_AGGUX, snx_AGGUY
*
*  This routine may only be called after NCAR AUTOGRAPH has been
*  used to plot a graph, thus having specified the mapping between
*  the coordinate systems involved.
*
*  Variations of choice device, cursor visibility, echo type, etc
*  may be made by direct SGS/GKS calls prior to calling this
*  routine.
*
*  P T Wallace   Starlink   May 1987
*
*+

      IMPLICIT NONE

      REAL X,Y
      INTEGER N

      REAL XG,YG

      REAL snx_AGUGX,snx_AGUGY,snx_AGGUX,snx_AGGUY



*  Flush
      CALL PLOTIT(0,0,2)
      CALL sgs_FLUSH

*  Save NCAR normalisation transformation
      CALL snx_TO('SGS')

*  Make the SGS world coordinates match the AUTOGRAPH grid coordinates
      CALL snx_AGCS

*  Transform preset user coordinates to grid coordinates
      XG = snx_AGUGX(X)
      YG = snx_AGUGY(Y)

*  Set the cursor position (if possible)
      CALL sgs_SETCU(XG,YG)

*  Get a cursor position (grid coordinates)
      CALL sgs_REQCU(XG,YG,N)

*  Convert to user coordinates
      X = snx_AGGUX(XG)
      Y = snx_AGGUY(YG)

*  Restore NCAR normalisation transformation
      CALL snx_TO('NCAR')

      END
