      subroutine zero_short(data,ix)
*+
* Name:
*    ZERO_SHORT

* Invocation:
*    CALL ZERO_SHORT(DATA,IX)

* Purpose:
*   This just zeroes out a region of integer*2 array...

* Description:
*   This just zeroes out a region of integer*2 array...
*
* Arguments:
*    IX = INTEGER (Given)
*        Dimension of array
*    DATA(IX) = INTEGER*2 ARRAY (Returned)
*        Array to be zeroed
* History:
*   Altered to use gen_fill, T.N.Wilkins 6/7/88
*   Removed call to gen_fill and reinstated simple Fortran equivalent,
*   A C Davenhall 19/12/00.
*-
      implicit none
      integer ix
      integer*2 data(ix)
*
      integer i
      do i=1,ix
        data(i)=0
      enddo

C     call gen_fill(ix*2,0,data)
      end
