      real function median( y, N)
*+
* Name:
*    MEDIAN

* Invocation:
*   (REAL) = MEDIAN( Y, N)

* Purpose:
*   find the median of the sorted values y(1) ......y(N)

* Description:
*   find the median of the sorted values y(1) ......y(N)
      implicit none
      integer n
      real y(n)
*-
      integer mptr, mptr2
      mptr = (N/2) + 1
      mptr2 = N - mptr + 1
      MEDIAN = (Y(mptr) + Y(mptr2)) / 2.0
      end
