      subroutine zero_real(data,ix)
*+
* Name:
*    ZERO_REAL

* Invocation:
*    CALL ZERO_REAL(DATA,IX)

* Purpose:
*   Zero out a region of real array.

* Description:
*   Zero out a region of real array.

* Arguments:
*    IX = INTEGER (Given)
*        Dimension of array
*    DATA(IX) = REAL ARRAY (Returned)
*        Array to be zeroed

* History:
*  Altered to use gen_fill, 6/7/88 T.N.Wilkins
*  Removed call to gen_fill and reinstated simple Fortran equivalent,
*  A C Davenhall 19/12/00.
*-
      implicit none
      integer ix
      real data(ix)

      integer i
      do i=1,ix
        data(i)=0.0
      enddo

C     call gen_fill(ix*4,0,data)
      end
