      subroutine zero_int(data,ix)
*+
* Name:
*    ZERO_INT

* Invocation:
*    CALL ZERO_INT(DATA,IX)

* Purpose:
*   Zero out a region of integer array.

* Description:
*   Zero out a region of integer array.

* Arguments:
*    IX = INTEGER (Given)
*        Dimension of array
*    DATA(IX) = INTEGER ARRAY (Returned)
*        Array to be zeroed
*
*  Altered to use gen_fill, T.N.Wilkins 6/7/88
*  Removed call to gen_fill and reinstated simple Fortran equivalent,
*  A C Davenhall 19/12/00.
*-
      implicit none
      integer ix
      integer data(ix)

      integer i
      do i=1,ix
        data(i)=0
      enddo

C     call gen_fill(ix*4,0,data)
      end
