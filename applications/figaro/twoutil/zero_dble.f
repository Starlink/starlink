      subroutine zero_dble(data,ix)
*+
* Name:
*    ZERO_DBLE

* Invocation:
*    CALL ZERO_DBLE(DATA,IX)

* Purpose:
*   Zero out a region of double precision array.

* Description:
*   Zero out a region of double precision array.

* Arguments:
*    IX = INTEGER (Given)
*        Dimension of array
*    DATA(IX) = DOUBLE PRECISION ARRAY (Returned)
*        Array to be zeroed
*-
      implicit none
      integer ix
      double precision data(ix)
* ---------------------------------------------------------------------
      integer i
      do i=1,ix
        data(i)=0.0d0
      enddo
      end
