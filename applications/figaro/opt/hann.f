      subroutine hann( y, N)
*+
* Name:
*    HANN

* Invocation:
*    CALL HANN( Y, N)

* Purpose:
*   3- point Smooth Y() by moving averages weighted by 1/4 , 1/2 , 1/4

* Description:
*   3- point Smooth Y() by moving averages weighted by 1/4 , 1/2 , 1/4
*   this is called HANNING
      implicit none

      integer n
      real y(n)
*-
      integer  i , nm1
      real y1 , y2, y3
      nm1 = n - 1

      Y2 = y(1)
      Y3 = y(2)

      do i = 2 , nm1
        y1  = y2
        y2 = y3
        y3  = y(I + 1)
        y(i) = ( y1 + y2 + y3) / 4.0

      end do

      end
