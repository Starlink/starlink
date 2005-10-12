      subroutine pda_lupdat(beta, gamma, l, lambda, lplus, n, w, z)
c
c  ***  compute lplus = secant update of l  ***
c
c  ***  parameter declarations  ***
c
      integer n
      double precision beta(n), gamma(n), l(1), lambda(n), lplus(1),
     1                 w(n), z(n)
c     dimension l(n*(n+1)/2), lplus(n*(n+1)/2)
c
c--------------------------  parameter usage  --------------------------
c
c   beta = scratch vector.
c  gamma = scratch vector.
c      l (input) lower triangular matrix, stored rowwise.
c lambda = scratch vector.
c  lplus (output) lower triangular matrix, stored rowwise, which may
c             occupy the same storage as  l.
c      n (input) length of vector parameters and order of matrices.
c      w (input, destroyed on output) right singular vector of rank 1
c             correction to  l.
c      z (input, destroyed on output) left singular vector of rank 1
c             correction to  l.
c
c-------------------------------  notes  -------------------------------
c
c  ***  application and usage restrictions  ***
c
c        this routine updates the cholesky factor  l  of a symmetric
c     positive definite matrix to which a secant update is being
c     applied -- it computes a cholesky factor  lplus  of
c     l * (i + z*w**t) * (i + w*z**t) * l**t.  it is assumed that  w
c     and  z  have been chosen so that the updated matrix is strictly
c     positive definite.
c
c  ***  algorithm notes  ***
c
c        this code uses recurrence 3 of ref. 1 (with d(j) = 1 for all j)
c     to compute  lplus  of the form  l * (i + z*w**t) * q,  where  q
c     is an orthogonal matrix that makes the result lower triangular.
c        lplus may have some negative diagonal elements.
c
c  ***  references  ***
c
c 1.  goldfarb, d. (1976), factorized variable metric methods for uncon-
c             strained optimization, math. comput. 30, pp. 796-811.
c
c  ***  general  ***
c
c     coded by david m. gay (fall 1979).
c     this subroutine was written in connection with research supported
c     by the national science foundation under grants mcs-7600324 and
c     mcs-7906671.
c
c------------------------  external quantities  ------------------------
c
c  ***  intrinsic functions  ***
c/+
      double precision dsqrt
c/
c--------------------------  local variables  --------------------------
c
      integer i, ij, j, jj, jp1, k, nm1, np1
      double precision a, b, bj, eta, gj, lj, lij, ljj, nu, s, theta,
     1                 wj, zj
      double precision one, zero
c
c  ***  data initializations  ***
c
c/6
      data one/1.d+0/, zero/0.d+0/
c/7
c     parameter (one=1.d+0, zero=0.d+0)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      nu = one
      eta = zero
      if (n .le. 1) go to 30
      nm1 = n - 1
c
c  ***  temporarily store s(j) = sum over k = j+1 to n of w(k)**2 in
c  ***  lambda(j).
c
      s = zero
      do 10 i = 1, nm1
         j = n - i
         s = s + w(j+1)**2
         lambda(j) = s
 10      continue
c
c  ***  compute lambda, gamma, and beta by goldfarb*s recurrence 3.
c
      do 20 j = 1, nm1
         wj = w(j)
         a = nu*z(j) - eta*wj
         theta = one + a*wj
         s = a*lambda(j)
         lj = dsqrt(theta**2 + a*s)
         if (theta .gt. zero) lj = -lj
         lambda(j) = lj
         b = theta*wj + s
         gamma(j) = b * nu / lj
         beta(j) = (a - b*eta) / lj
         nu = -nu / lj
         eta = -(eta + (a**2)/(theta - lj)) / lj
 20      continue
 30   lambda(n) = one + (nu*z(n) - eta*w(n))*w(n)
c
c  ***  update l, gradually overwriting  w  and  z  with  l*w  and  l*z.
c
      np1 = n + 1
      jj = n * (n + 1) / 2
      do 60 k = 1, n
         j = np1 - k
         lj = lambda(j)
         ljj = l(jj)
         lplus(jj) = lj * ljj
         wj = w(j)
         w(j) = ljj * wj
         zj = z(j)
         z(j) = ljj * zj
         if (k .eq. 1) go to 50
         bj = beta(j)
         gj = gamma(j)
         ij = jj + j
         jp1 = j + 1
         do 40 i = jp1, n
              lij = l(ij)
              lplus(ij) = lj*lij + bj*w(i) + gj*z(i)
              w(i) = w(i) + lij*wj
              z(i) = z(i) + lij*zj
              ij = ij + i
 40           continue
 50      jj = jj - j
 60      continue
c
 999  return
c  ***  last card of pda_lupdat follows  ***
      end
