      program silly
      implicit none
      integer n, i
      real determinant
      parameter (n=2)
      real twobytwo(2,2) /4*-1/
      do i = 1, 100000
         call mkidentity (twobytwo, n)
      enddo
      print *, determinant (twobytwo)
      end
