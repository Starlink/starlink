      subroutine pda_wzbfgs (l, n, s, w, y, z)
c
c  ***  compute  y  and  z  for  pda_lupdat  corresponding to bfgs update.
c
      integer n
      double precision l(1), s(n), w(n), y(n), z(n)
c     dimension l(n*(n+1)/2)
c
c--------------------------  parameter usage  --------------------------
c
c l (i/o) cholesky factor of hessian, a lower triang. matrix stored
c             compactly by rows.
c n (input) order of  l  and length of  s,  w,  y,  z.
c s (input) the step just taken.
c w (output) right singular vector of rank 1 correction to l.
c y (input) change in gradients corresponding to s.
c z (output) left singular vector of rank 1 correction to l.
c
c-------------------------------  notes  -------------------------------
c
c  ***  algorithm notes  ***
c
c        when  s  is computed in certain ways, e.g. by  pda_gqtstp  or
c     dbldog,  it is possible to save n**2/2 operations since  (l**t)*s
c     or  l*(l**t)*s is then known.
c        if the bfgs update to l*(l**t) would reduce its determinant to
c     less than eps times its old value, then this routine in effect
c     replaces  y  by  theta*y + (1 - theta)*l*(l**t)*s,  where  theta
c     (between 0 and 1) is chosen to make the reduction factor = eps.
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
c  ***  functions and subroutines called  ***
c
      external pda_dotprd, pda_livmul, pda_ltvmul
      double precision pda_dotprd
c pda_dotprd returns inner product of two vectors.
c pda_livmul multiplies l**-1 times a vector.
c pda_ltvmul multiplies l**t times a vector.
c
c  ***  intrinsic functions  ***
c/+
      double precision dsqrt
c/
c--------------------------  local variables  --------------------------
c
      integer i
      double precision cs, cy, eps, epsrt, one, shs, ys, theta
c
c  ***  data initializations  ***
c
c/6
      data eps/0.1d+0/, one/1.d+0/
c/7
c     parameter (eps=0.1d+0, one=1.d+0)
c/
c
c+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++
c
      call pda_ltvmul(n, w, l, s)
      shs = pda_dotprd(n, w, w)
      ys = pda_dotprd(n, y, s)
      if (ys .ge. eps*shs) go to 10
         theta = (one - eps) * shs / (shs - ys)
         epsrt = dsqrt(eps)
         cy = theta / (shs * epsrt)
         cs = (one + (theta-one)/epsrt) / shs
         go to 20
 10   cy = one / (dsqrt(ys) * dsqrt(shs))
      cs = one / shs
 20   call pda_livmul(n, z, l, y)
      do 30 i = 1, n
 30      z(i) = cy * z(i)  -  cs * w(i)
c
 999  return
c  ***  last card of pda_wzbfgs follows  ***
      end
