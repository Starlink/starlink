      subroutine s4( y, N, endsav, work , save, NW,  err)
*+
* Name:
*    S4

* Invocation:
*    CALL S4( Y, N, ENDSAV, WORK , SAVE, NW,  ERR)

* Purpose:
* Description:
* Smooth Y() by running medians of 4

      implicit none
      integer n, NW, err
      real y(n), endm1, endsav, work(nw), save(nw)
*-
      real two
      integer four
      Parameter (TWO = 2.0)
      Parameter (FOUR = 4)

* even length Medians offset the output sequence to the High end,
* since they cannot be symmetric. ENDSAV is left holding Y(N) since
* there is no other room for it. Y(1) is unchanged.

      endsav = y(n)
      endm1 = y(n-1)
      call runmed(y , n , FOUR , work , save, nw , err)
      Y(2) = (Y(1) + y(2) ) / TWO
      Y(N) = (endm1 + endsav) / TWO

      end
