      integer function rx2chn(x,nchans,xpos)
*+
* Name:
*    RX2CHN

* Invocation:
*   (INTEGER) = RX2CHN(X,NCHANS,XPOS)
* Purpose:
*   Converts a real x array value to channel number.
*
* Description:
*   Converts a real x array value to channel number.
*
* Arguments:
*    X(NCHANS) = REAL ARRAY (Given)
*        X (channel number) array
*    NCHANS = INTEGER (Given)
*        Number of channels
*    XPOS = REAL (Given)
*        Position to convert
* Returned value:
*    RX2CHN = INTEGER
*        Channel number with value nearest to XPOS
*- ---------------------------------------------------------------------
      implicit none
      integer nchans
      real x(nchans)
      real xpos
      real disper
      disper = (x(nchans)-x(1))/real(nchans-1)
      rx2chn = 1+nint((xpos-x(1))/disper)
      end
