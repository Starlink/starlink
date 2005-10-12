      subroutine pda_vscopy(p, y, s)
c
c  ***  set p-vector y to scalar s  ***
c
      integer p
      double precision s, y(p)
c
      integer i
c
      do 10 i = 1, p
 10      y(i) = s
      return
      end
