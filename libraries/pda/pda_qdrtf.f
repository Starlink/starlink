c
c***********************************************************************
c
c     q d r t f
c
c***********************************************************************
c
      subroutine pda_qdrtf(n, x, nf, f, uip, urp, ufp)
c
c  this routine evaluates the objective function f(x) described in the
c  main program above.  it stores in urp(*,2) and urp(*,3) some
c  information useful in evaluating the gradient and hessian of f, and
c  it stores  nf  in uip(1) to identify the x corresponding to this
c  information.  f(x) has the form  f(x) = phi(q(x)),  where  q  is a
c  quadratic form and  phi(y) = y**0.5.  the gradient of  f  is
c  g(x) = phiprm(q(x))*gq(x),  where  phiprm  is the derivative of phi
c  and  gq  is the gradient of q.  this routine stores phiprm(q(x)) in
c  urp(1,3) and gq(x) in urp(*,2).  the hessian of f is
c  h(x) = phi2prm(q(x))*gq(x)*gq(x)**t + phiprm(q(x))*hq(x),  where
c  phi2prm  is the second derivative of phi, **t denotes transpose,
c  and  hq  is the hessian of q.  this routine stores phi2prm(q(x)) in
c  urp(2,3).  the subroutines pda_qdrtg and pda_pda_qdrtgh given below would work
c  without change on any other choice of phi.  pda_qdrtg would also work
c  with any other differentiable function q.  pda_pda_qdrtgh, on the other
c  hand, assumes that  hq(x)  is the matrix  a  described in the main
c  program above.
c
      integer n, nf, uip(1)
      double precision x(n), f, urp(n,3)
      external ufp
c/+
      real float
      double precision dsqrt
c/
      integer i
      double precision dn, f2, t, t1
c
c
      uip(1) = nf
      dn = n
      t = 0.d+0
      do 10 i = 1, n
         urp(i,2) = urp(i,1)*x(i) - float(i)
         t = t + urp(i,2)
 10      continue
      f2 = 0.d+0
      do 20 i = 1, n
         t1 = dn*urp(i,2) + t
         f2 = f2 + t1*urp(i,2)
         urp(i,2) = urp(i,1) * t1
 20      continue
      f2 = 1.d+0  +   0.5d+0 * f2
      f = dsqrt(f2)
      urp(1,3) = 0.5d+0 / f
      urp(2,3) = -0.5d+0 / (f * f2)
 999  return
      end
