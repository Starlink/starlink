      subroutine pda_lvmul(n, x, l, y)
c
c  ***  compute  x = l*y, where  l  is an  n x n  lower triangular
c  ***  matrix stored compactly by rows.  x and y may occupy the same
c  ***  storage.  ***
c
      integer n
      double precision x(n), l(1), y(n)
c     dimension l(n*(n+1)/2)
      integer i, ii, ij, i0, j, np1
      double precision t, zero
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
      np1 = n + 1
      i0 = n*(n+1)/2
      do 20 ii = 1, n
         i = np1 - ii
         i0 = i0 - i
         t = zero
         do 10 j = 1, i
              ij = i0 + j
              t = t + l(ij)*y(j)
 10           continue
         x(i) = t
 20      continue
 999  return
c  ***  last card of pda_lvmul follows  ***
      end
