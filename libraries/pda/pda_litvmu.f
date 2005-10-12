      subroutine pda_litvmu(n, x, l, y)
c
c  ***  solve  (l**t)*x = y,  where  l  is an  n x n  lower triangular
c  ***  matrix stored compactly by rows.  x and y may occupy the same
c  ***  storage.  ***
c
      integer n
      double precision x(n), l(1), y(n)
      integer i, ii, ij, im1, i0, j, np1
      double precision xi, zero
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
      do 10 i = 1, n
 10      x(i) = y(i)
      np1 = n + 1
      i0 = n*(n+1)/2
      do 30 ii = 1, n
         i = np1 - ii
         xi = x(i)/l(i0)
         x(i) = xi
         if (i .le. 1) go to 999
         i0 = i0 - i
         if (xi .eq. zero) go to 30
         im1 = i - 1
         do 20 j = 1, im1
              ij = i0 + j
              x(j) = x(j) - xi*l(ij)
 20           continue
 30      continue
 999  return
c  ***  last card of pda_litvmu follows  ***
      end
