      subroutine pda_vaxpy(p, w, a, x, y)
c
c  ***  set w = a*x + y  --  w, x, y = p-vectors, a = scalar  ***
c
      integer p
      double precision a, w(p), x(p), y(p)
c
      integer i
c
      do 10 i = 1, p
 10      w(i) = a*x(i) + y(i)
      return
      end
