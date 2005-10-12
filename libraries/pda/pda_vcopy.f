      subroutine pda_vcopy(p, y, x)
c
c  ***  set y = x, where x and y are p-vectors  ***
c
      integer p
      double precision x(p), y(p)
c
      integer i
c
      do 10 i = 1, p
 10      y(i) = x(i)
      return
      end
