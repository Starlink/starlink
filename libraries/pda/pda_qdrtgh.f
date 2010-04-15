c
c***********************************************************************
c
c     q d r t g h
c
c***********************************************************************
c
      subroutine pda_pda_qdrtgh(n, x, nf, g, h, uip, urp, pda_qdrtf)
c
c  this routine evaluates the gradient and hessian of the objective
c  function f(x) described in the main program above.  see the comments
c  there and in subroutine pda_qdrtf above.  note that the  h  returned is
c  the lower triangle of the hessian, stored row-wise.
c
      integer n, nf, uip(1)
      double precision x(n), g(n), h(1), urp(n,3)
c     dimension h(n*(n+1)/2)
      external pda_qdrtf
c
      integer i, j, k
      double precision dn, f, t1, t2
c
      if (nf .ne. uip(1)) call pda_qdrtf(n, x, nf, f, uip, urp,
     :                                   pda_qdrtf)
      k = 0
      dn = n
      do 20 i = 1, n
         g(i) = urp(1,3) * urp(i,2)
         t1 = urp(1,3) * urp(i,1)
         t2 = urp(i,2) * urp(2,3)
         do 10 j = 1, i
              k = k + 1
              h(k) = t2*urp(j,2) + t1*urp(j,1)
 10           continue
         h(k) = h(k) + dn*urp(i,1)*t1
 20      continue
 999  return
      end
