      subroutine pda_subplx (f,n,tol,maxnfe,mode,scale,x,fx,nfe,
     *                   work,iwork,iflag)
c
      integer n,maxnfe,mode,nfe,iwork(*),iflag
      double precision f,tol,scale(*),x(n),fx,work(*)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_subplx uses the subplex method to solve unconstrained
c optimization problems.  The method is well suited for
c optimizing objective functions that are noisy or are
c discontinuous at the solution.
c
c pda_subplx sets default optimization options by calling the
c subroutine pda_subopt.  The user can override these defaults
c by calling pda_subopt prior to calling pda_subplx, changing the
c appropriate common variables, and setting the value of
c mode as indicated below.
c
c By default, pda_subplx performs minimization.
c
c input
c
c   f      - user supplied function f(n,x) to be optimized,
c            declared external in calling routine
c
c   n      - problem dimension
c
c   tol    - relative error tolerance for x (tol .ge. 0.)
c
c   maxnfe - maximum number of function evaluations
c
c   mode   - integer mode switch with binary expansion
c            (bit 1) (bit 0) :
c            bit 0 = 0 : first call to pda_subplx
c                  = 1 : continuation of previous call
c            bit 1 = 0 : use default options
c                  = 1 : user set options
c
c   scale  - scale and initial stepsizes for corresponding
c            components of x
c            (If scale(1) .lt. 0.,
c            abs(scale(1)) is used for all components of x,
c            and scale(2),...,scale(n) are not referenced.)
c
c   x      - starting guess for optimum
c
c   work   - double precision work array of dimension .ge.
c            2*n + nsmax*(nsmax+4) + 1
c            (nsmax is set in subroutine pda_subopt.
c            default: nsmax = min(5,n))
c
c   iwork  - integer work array of dimension .ge.
c            n + int(n/nsmin)
c            (nsmin is set in subroutine pda_subopt.
c            default: nsmin = min(2,n))
c
c output
c
c   x      - computed optimum
c
c   fx     - value of f at x
c
c   nfe    - number of function evaluations
c
c   iflag  - error flag
c            = -2 : invalid input
c            = -1 : maxnfe exceeded
c            =  0 : tol satisfied
c            =  1 : limit of machine precision
c            =  2 : fstop reached (fstop usage is determined
c                   by values of options minf, nfstop, and
c                   irepl. default: f(x) not tested against
c                   fstop)
c            iflag should not be reset between calls to
c            pda_subplx.
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
      integer i,j,ifsptr,ins,insfnl,insptr,ipptr,isptr,
     *        istep,istptr,ns,nsubs
      double precision bnsfac(3,2),dum,scl,sfx,xpscl
      logical cmode
c
      save
c
c subroutines and functions
c
      external f,pda_sortd,pda_evalf,pda_partx,
     *           pda_setstp,pda_simplx,pda_subopt
c   blas
      external pda_dcopy
c   fortran
      intrinsic abs,mod
c
c data
c
      data ((bnsfac(i,j),i=1,3),j=1,2) /-1.d0,-2.d0,0.d0,
     *      1.d0,0.d0,2.d0/
c-----------------------------------------------------------
c
      if (mod(mode,2) .eq. 0) then
c
c       first call, check input
c
        if (n .lt. 1) go to 120
        if (tol .lt. 0.d0) go to 120
        if (maxnfe .lt. 1) go to 120
        if (scale(1) .ge. 0.d0) then
          do 10 i = 1,n
            xpscl = x(i)+scale(i)
            if (xpscl .eq. x(i)) go to 120
   10     continue
        else
          scl = abs(scale(1))
          do 20 i = 1,n
            xpscl = x(i)+scl
            if (xpscl .eq. x(i)) go to 120
   20     continue
        end if
        if (mod(mode/2,2) .eq. 0) then
          call pda_subopt (n)
        else
          if (alpha .le. 0.d0) go to 120
          if (beta .le. 0.d0 .or. beta .ge. 1.d0) go to 120
          if (gamma .le. 1.d0) go to 120
          if (delta .le. 0.d0 .or. delta .ge. 1.d0)
     *        go to 120
          if (psi .le. 0.d0 .or. psi .ge. 1.d0) go to 120
          if (omega .le. 0.d0 .or. omega .ge. 1.d0)
     *        go to 120
          if (nsmin .lt. 1 .or. nsmax .lt. nsmin .or.
     *        n .lt. nsmax) go to 120
          if (n .lt. ((n-1)/nsmax+1)*nsmin) go to 120
          if (irepl .lt. 0 .or. irepl .gt. 2) go to 120
          if (ifxsw .lt. 1 .or. ifxsw .gt. 3) go to 120
          if (bonus .lt. 0.d0) go to 120
          if (nfstop .lt. 0) go to 120
        end if
c
c       initialization
c
        istptr = n+1
        isptr = istptr+n
        ifsptr = isptr+nsmax*(nsmax+3)
        insptr = n+1
        if (scale(1) .gt. 0.d0) then
          call pda_dcopy (n,scale,1,work,1)
          call pda_dcopy (n,scale,1,work(istptr),1)
        else
          call pda_dcopy (n,scl,0,work,1)
          call pda_dcopy (n,scl,0,work(istptr),1)
        end if
        do 30 i = 1,n
          iwork(i) = i
   30   continue
        nfe = 0
        nfxe = 1
        if (irepl .eq. 0) then
          fbonus = 0.d0
        else if (minf) then
          fbonus = bnsfac(ifxsw,1)*bonus
        else
          fbonus = bnsfac(ifxsw,2)*bonus
        end if
        if (nfstop .eq. 0) then
          sfstop = 0.d0
        else if (minf) then
          sfstop = fstop
        else
          sfstop = -fstop
        end if
        ftest = 0.d0
        cmode = .false.
        new = .true.
        initx = .true.
        call pda_evalf (f,0,iwork,dum,n,x,sfx,nfe)
        initx = .false.
      else
c
c       continuation of previous call
c
        if (iflag .eq. 2) then
          if (minf) then
            sfstop = fstop
          else
            sfstop = -fstop
          end if
          cmode = .true.
          go to 70
        else if (iflag .eq. -1) then
          cmode = .true.
          go to 70
        else if (iflag .eq. 0) then
          cmode = .false.
          go to 90
        else
          return
        end if
      end if
c
c     subplex loop
c
   40 continue
        do 50 i = 1,n
          work(i) = abs(work(i))
   50   continue
        call pda_sortd (n,work,iwork)
        call pda_partx (n,iwork,work,nsubs,iwork(insptr))
        call pda_dcopy (n,x,1,work,1)
        ins = insptr
        insfnl = insptr+nsubs-1
        ipptr = 1
c
c       simplex loop
c
   60   continue
          ns = iwork(ins)
   70     continue
          call pda_simplx (f,n,work(istptr),ns,iwork(ipptr),
     *                 maxnfe,cmode,x,sfx,nfe,work(isptr),
     *                 work(ifsptr),iflag)
          cmode = .false.
          if (iflag .ne. 0) go to 110
          if (ins .lt. insfnl) then
            ins = ins+1
            ipptr = ipptr+ns
            go to 60
          end if
c
c       end simplex loop
c
        do 80 i = 1,n
          work(i) = x(i)-work(i)
   80   continue
c
c       check termination
c
   90   continue
        istep = istptr
        do 100 i = 1,n
          if (max(abs(work(i)),abs(work(istep))*psi)/
     *        max(abs(x(i)),1.d0) .gt. tol) then
            call pda_setstp (nsubs,n,work,work(istptr))
            go to 40
          end if
          istep = istep+1
  100   continue
c
c     end subplex loop
c
      iflag = 0
  110 continue
      if (minf) then
        fx = sfx
      else
        fx = -sfx
      end if
      return
c
c     invalid input
c
  120 continue
      iflag = -2
      return
      end
