c
c***********************************************************************
c
c     q d r t g
c
c***********************************************************************
c
      subroutine pda_qdrtg(n, x, nf, g, uip, urp, pda_qdrtf)
c
c  this routine evaluates the gradient of the objective function f(x)
c  described in the main program above.  see the comments there and in
c  subroutine pda_qdrtf above.
c
      integer n, nf, uip(1)
      double precision x(n), g(n), urp(n,3)
      external pda_qdrtf
c
      integer i
      double precision f
c
      if (nf .ne. uip(1)) call pda_qdrtf(n, x, nf, f, uip, urp,
     1                                   pda_qdrtf)
      do 10 i = 1, n
 10      g(i) = urp(1,3) * urp(i,2)
 999  return
      end
