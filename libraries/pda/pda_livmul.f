      subroutine pda_livmul(n, x, l, y)
c
c  ***  solve  l*x = y, where  l  is an  n x n  lower triangular
c  ***  matrix stored compactly by rows.  x and y may occupy the same
c  ***  storage.  ***
c
      integer n
      double precision x(n), l(1), y(n)
      external pda_dotprd
      double precision pda_dotprd
      integer i, j, k
      double precision t, zero
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
      do 10 k = 1, n
         if (y(k) .ne. zero) go to 20
         x(k) = zero
 10      continue
      go to 999
 20   j = k*(k+1)/2
      x(k) = y(k) / l(j)
      if (k .ge. n) go to 999
      k = k + 1
      do 30 i = k, n
         t = pda_dotprd(i-1, l(j+1), x)
         j = j + i
         x(i) = (y(i) - t)/l(j)
 30      continue
 999  return
c  ***  last card of pda_livmul follows  ***
      end
