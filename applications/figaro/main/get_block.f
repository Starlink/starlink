      integer function get_block(ystart,nwindow,xsect)
*+
* Name:
*    GET_BLOCK

* Invocation:
*   (INTEGER) = GET_BLOCK(YSTART,NWINDOW,XSECT)

* Purpose:
*  To convert a cross-section number into a block number.

* Description:
*  To convert a cross-section number into a block number.

* Arguments:
*    YSTART = INTEGER (Given)
*
*    NWINDOW = INTEGER (Given)
*
*    XSECT = INTEGER (Given)
*
*  Returned = INTEGER (Given)
*
*    GET_BLOCK = INTEGER (Given)
*        Block number

* History:
*  TNW, 8/6/92 Simplified (why was it ever so complicated?).
*-
      implicit none
      integer ystart,xsect
      integer nwindow

* Check that value is rounded up, not down.

      get_block = 1 + (xsect-ystart)/nwindow
      end
