      subroutine pda_subopt (n)
c
      integer n
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_subopt sets options for pda_subplx.
c
c input
c
c   n      - problem dimension
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
      save
c
c subroutines and functions
c
c   fortran
      intrinsic min
c
c-----------------------------------------------------------
c
c***********************************************************
c simplex method strategy parameters
c***********************************************************
c
c alpha  - reflection coefficient
c          alpha .gt. 0
c
      alpha = 1.d0
c
c beta   - contraction coefficient
c          0 .lt. beta .lt. 1
c
      beta = .5d0
c
c gamma  - expansion coefficient
c          gamma .gt. 1
c
      gamma = 2.d0
c
c delta  - shrinkage (massive contraction) coefficient
c          0 .lt. delta .lt. 1
c
      delta = .5d0
c
c***********************************************************
c subplex method strategy parameters
c***********************************************************
c
c psi    - simplex reduction coefficient
c          0 .lt. psi .lt. 1
c
      psi = .25d0
c
c omega  - step reduction coefficient
c          0 .lt. omega .lt. 1
c
      omega = .1d0
c
c nsmin and nsmax specify a range of subspace dimensions.
c In addition to satisfying  1 .le. nsmin .le. nsmax .le. n,
c nsmin and nsmax must be chosen so that n can be expressed
c as a sum of positive integers where each of these integers
c ns(i) satisfies   nsmin .le. ns(i) .ge. nsmax.
c Specifically,
c     nsmin*ceil(n/nsmax) .le. n   must be true.
c
c nsmin  - subspace dimension minimum
c
      nsmin = min(2,n)
c
c nsmax  - subspace dimension maximum
c
      nsmax = min(5,n)
c
c***********************************************************
c subplex method special cases
c***********************************************************
c nelder-mead simplex method with periodic restarts
c   nsmin = nsmax = n
c***********************************************************
c nelder-mead simplex method
c   nsmin = nsmax = n, psi = small positive
c***********************************************************
c
c irepl, ifxsw, and bonus deal with measurement replication.
c Objective functions subject to large amounts of noise can
c cause an optimization method to halt at a false optimum.
c An expensive solution to this problem is to evaluate f
c several times at each point and return the average (or max
c or min) of these trials as the function value.  pda_subplx
c performs measurement replication only at the current best
c point. The longer a point is retained as best, the more
c accurate its function value becomes.
c
c The common variable nfxe contains the number of function
c evaluations at the current best point. fxstat contains the
c mean, max, min, and standard deviation of these trials.
c
c irepl  - measurement replication switch
c irepl  = 0, 1, or 2
c        = 0 : no measurement replication
c        = 1 : pda_subplx performs measurement replication
c        = 2 : user performs measurement replication
c              (This is useful when optimizing on the mean,
c              max, or min of trials is insufficient. Common
c              variable initx is true for first function
c              evaluation. newx is true for first trial at
c              this point. The user uses subroutine pda_fstats
c              within his objective function to maintain
c              fxstat. By monitoring newx, the user can tell
c              whether to return the function evaluation
c              (newx = .true.) or to use the new function
c              evaluation to refine the function evaluation
c              of the current best point (newx = .false.).
c              The common variable ftest gives the function
c              value that a new point must beat to be
c              considered the new best point.)
c
      irepl = 0
c
c ifxsw  - measurement replication optimization switch
c ifxsw  = 1, 2, or 3
c        = 1 : retain mean of trials as best function value
c        = 2 : retain max
c        = 3 : retain min
c
      ifxsw = 1
c
c Since the current best point will also be the most
c accurately evaluated point whenever irepl .gt. 0, a bonus
c should be added to the function value of the best point
c so that the best point is not replaced by a new point
c that only appears better because of noise.
c pda_subplx uses bonus to determine how many multiples of
c fxstat(4) should be added as a bonus to the function
c evaluation. (The bonus is adjusted automatically by
c pda_subplx when ifxsw or minf is changed.)
c
c bonus  - measurement replication bonus coefficient
c          bonus .ge. 0 (normally, bonus = 0 or 1)
c        = 0 : bonus not used
c        = 1 : bonus used
c
      bonus = 1.d0
c
c nfstop = 0 : f(x) is not tested against fstop
c        = 1 : if f(x) has reached fstop, pda_subplx returns
c              iflag = 2
c        = 2 : (only valid when irepl .gt. 0)
c              if f(x) has reached fstop and
c              nfxe .gt. nfstop, pda_subplx returns iflag = 2
c
      nfstop = 0
c
c fstop  - f target value
c          Its usage is determined by the value of nfstop.
c
c minf   - logical switch
c        = .true.  : pda_subplx performs minimization
c        = .false. : pda_subplx performs maximization
c
      minf = .true.
      return
      end
