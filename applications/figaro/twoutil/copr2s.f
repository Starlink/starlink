      subroutine copr2s(n,r,s)
*+
* Name:
*    COPR2S

* Invocation:
*    CALL COPR2S(N,R,S)
* Purpose:
*  To copy a real array to an integer*2 array.

* Description:
*  To copy a real array to an integer*2 array.
*
* Arguments:
*      N = INTEGER (Given)
*        Number of elements of arrays
*      R(N) = REAL ARRAY (Given)
*        Real array
*      S(N) = INTEGER*2 ARRAY (Returned)
*        Integer*2 array
*
* History:
*   T.N.Wilkins, Cambridge, 19-JUN-1989
*-
      implicit none
      integer n
      real r(n)
      integer*2 s(n)

*

      integer i

      do i = 1, n
        s(i) = nint(r(i))
      end do
      end
