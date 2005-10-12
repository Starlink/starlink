      subroutine pda_ltvmul(n, x, l, y)
c
c  ***  compute  x = (l**t)*y, where  l  is an  n x n  lower
c  ***  triangular matrix stored compactly by rows.  x and y may
c  ***  occupy the same storage.  ***
c
      integer n
      double precision x(n), l(1), y(n)
c     dimension l(n*(n+1)/2)
      integer i, ij, i0, j
      double precision yi, zero
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
      i0 = 0
      do 20 i = 1, n
         yi = y(i)
         x(i) = zero
         do 10 j = 1, i
              ij = i0 + j
              x(j) = x(j) + yi*l(ij)
 10           continue
         i0 = i0 + i
 20      continue
 999  return
c  ***  last card of pda_ltvmul follows  ***
      end
