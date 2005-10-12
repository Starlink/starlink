      subroutine pda_vvmulp(n, x, y, z, k)
c
c ***  set x(i) = y(i) * z(i)**k, 1 .le. i .le. n (for k = 1 or -1)  ***
c
      integer n, k
      double precision x(n), y(n), z(n)
      integer i
c
      if (k .ge. 0) go to 20
      do 10 i = 1, n
 10      x(i) = y(i) / z(i)
      go to 999
c
 20   do 30 i = 1, n
 30      x(i) = y(i) * z(i)
 999  return
c  ***  last card of pda_vvmulp follows  ***
      end
