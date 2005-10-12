      subroutine pda_lsqrt(n1, n, l, a, irc)
c
c  ***  compute rows n1 through n of the cholesky factor  l  of
c  ***  a = l*(l**t),  where  l  and the lower triangle of  a  are both
c  ***  stored compactly by rows (and may occupy the same storage).
c  ***  irc = 0 means all went well.  irc = j means the leading
c  ***  principal  j x j  submatrix of  a  is not positive definite --
c  ***  and  l(j*(j+1)/2)  contains the (nonpos.) reduced j-th diagonal.
c
c  ***  parameters  ***
c
      integer n1, n, irc
      double precision l(1), a(1)
c     dimension l(n*(n+1)/2), a(n*(n+1)/2)
c
c  ***  local variables  ***
c
      integer i, ij, ik, im1, i0, j, jk, jm1, j0, k
      double precision t, td, zero
c
c  ***  intrinsic functions  ***
c/+
      double precision dsqrt
c/
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
c  ***  body  ***
c
      i0 = n1 * (n1 - 1) / 2
      do 50 i = n1, n
         td = zero
         if (i .eq. 1) go to 40
         j0 = 0
         im1 = i - 1
         do 30 j = 1, im1
              t = zero
              if (j .eq. 1) go to 20
              jm1 = j - 1
              do 10 k = 1, jm1
                   ik = i0 + k
                   jk = j0 + k
                   t = t + l(ik)*l(jk)
 10                continue
 20           ij = i0 + j
              j0 = j0 + j
              t = (a(ij) - t) / l(j0)
              l(ij) = t
              td = td + t*t
 30           continue
 40      i0 = i0 + i
         t = a(i0) - td
         if (t .le. zero) go to 60
         l(i0) = dsqrt(t)
 50      continue
c
      irc = 0
      go to 999
c
 60   l(i0) = t
      irc = i
c
 999  return
c
c  ***  last card of pda_lsqrt  ***
      end
