      real function rx2rchn(x,nchans,xpos)
*+
* Name:
*    RX2RCHN

* Invocation:
*   (REAL) = RX2RCHN(X,NCHANS,XPOS)
* Purpose:
*   Converts a real x array value to channel number.
*
*   Description:
*   Converts a real x array value to channel number.
*
*   Arguments:
*    X(NCHANS) = REAL ARRAY (Given)
*        X (channel number) array
*    NCHANS = INTEGER (Given)
*        Number of channels
*    XPOS = REAL (Given)
*        Position to convert
*    RX2RCHN = REAL (Returned)
*        Channel number with value XPOS
*- ---------------------------------------------------------------------
      implicit none
      integer nchans
      real x(nchans)
      real xpos
      real disper
      disper = (x(nchans)-x(1))/(nchans-1)
      rx2rchn = 1.0 + (real(xpos)-x(1))/disper
      end
