      subroutine pda_evalf (f,ns,ips,xs,n,x,sfx,nfe)
c
      integer ns,n,nfe
      integer ips(*)
      double precision f,xs(*),x(n),sfx
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_evalf evaluates the function f at a point defined by x
c with ns of its components replaced by those in xs.
c
c input
c
c   f      - user supplied function f(n,x) to be optimized
c
c   ns     - subspace dimension
c
c   ips    - permutation vector
c
c   xs     - double precision ns-vector to be mapped to x
c
c   n      - problem dimension
c
c   x      - double precision n-vector
c
c   nfe    - number of function evaluations
c
c output
c
c   sfx    - signed value of f evaluated at x
c
c   nfe    - incremented number of function evaluations
c
c common
c
      integer nsmin,nsmax,irepl,ifxsw,nfstop,nfxe
      double precision alpha,beta,gamma,delta,psi,omega,
     *     bonus,fstop,fxstat,ftest
      logical minf,initx,newx
c
      common /pda_usubc/ alpha,beta,gamma,delta,psi,omega,nsmin,
     *               nsmax,irepl,ifxsw,bonus,fstop,nfstop,
     *               nfxe,fxstat(4),ftest,minf,initx,newx
c
      double precision fbonus,sfstop,sfbest
      logical new
c
      common /pda_isubc/ fbonus,sfstop,sfbest,new
c
c local variables
c
      integer i
      double precision fx
      logical newbst
c
      save
c
c subroutines and functions
c
      external f,pda_fstats
c
c-----------------------------------------------------------
c
      do 10 i = 1,ns
        x(ips(i)) = xs(i)
   10 continue
      newx = new .or. irepl .ne. 2
      fx = f(n,x)
      if (irepl .eq. 0) then
        if (minf) then
          sfx = fx
        else
          sfx = -fx
        end if
      else if (new) then
        if (minf) then
          sfx = fx
          newbst = fx .lt. ftest
        else
          sfx = -fx
          newbst = fx .gt. ftest
        end if
        if (initx .or. newbst) then
          if (irepl .eq. 1) call pda_fstats (fx,1,.true.)
          ftest = fx
          sfbest = sfx
        end if
      else
        if (irepl .eq. 1) then
          call pda_fstats (fx,1,.false.)
          fx = fxstat(ifxsw)
        end if
        ftest = fx+fbonus*fxstat(4)
        if (minf) then
          sfx = ftest
          sfbest = fx
        else
          sfx = -ftest
          sfbest = -fx
        end if
      end if
      nfe = nfe+1
      return
      end
