      subroutine s2( y, N, endsav )
*+
* Name:
*    S2

* Invocation:
*    CALL S2( Y, N, ENDSAV )

* Purpose:
*  Smooth Y() by running medians ( means) of  2
* Description:
*  Smooth Y() by running medians ( means) of  2
*  used to receter results of running medians of 4
*  ENDSAV holds the original Y(N)
      implicit none
      integer n
      real y(n), endsav
*-
      integer nm1, two ,I
      Parameter (TWO = 2)

* even length Medians offset the output sequence to the High end,
* since they cannot be symmetric. ENDSAV is left holding Y(N) since
* there is no other room for it. Y(1) is unchanged.


      nm1 = n-1
      do i = 2, nm1

        Y(i) = ( Y(i + 1) + y(i) ) / TWO
      end do
      y(N) = endsav
      end
