      subroutine s5( y, N,  work , save, NW,  err)
*+
* Name:
*    S5

* Invocation:
*    CALL S5( Y, N,  WORK , SAVE, NW,  ERR)
* Purpose:
*   Smooth Y() by running medians of 5
* Description:
*   Smooth Y() by running medians of 5
      implicit none
      integer n, NW, err
      real y(n),  work(nw), save(nw)
*-
      integer  FIVE
      Parameter (FIVE = 5)
      logical CHANGE
      real ymed1 , ymed2

      call medof3( y(1) , y(2) , y(3) , ymed1 , CHANGE)
      call medof3( y(N) , y(N-1) , y(N-2) , ymed2 , CHANGE)
      call runmed(Y , N , FIVE , work , save , nw, err)
      Y(2) = ymed1
      Y(N - 1 ) = ymed2

      end
