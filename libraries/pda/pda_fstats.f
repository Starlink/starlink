      subroutine pda_fstats (fx,ifxwt,reset)
c
      integer ifxwt
      double precision fx
      logical reset
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_fstats modifies the common /pda_usubc/ variables nfxe,fxstat.
c
c input
c
c   fx     - most recent evaluation of f at best x
c
c   ifxwt  - integer weight for fx
c
c   reset  - logical switch
c            = .true.  : initialize nfxe,fxstat
c            = .false. : update nfxe,fxstat
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
c local variables
c
      integer nsv
      double precision fscale,f1sv
c
      save
c
c subroutines and functions
c
c   fortran
      intrinsic abs,max,min,sqrt
c
c-----------------------------------------------------------
c
      if (reset) then
        nfxe = ifxwt
        fxstat(1) = fx
        fxstat(2) = fx
        fxstat(3) = fx
        fxstat(4) = 0.d0
      else
        nsv = nfxe
        f1sv = fxstat(1)
        nfxe = nfxe+ifxwt
        fxstat(1) = fxstat(1)+ifxwt*(fx-fxstat(1))/nfxe
        fxstat(2) = max(fxstat(2),fx)
        fxstat(3) = min(fxstat(3),fx)
        fscale = max(abs(fxstat(2)),abs(fxstat(3)),1.d0)
        fxstat(4) = fscale*sqrt((
     *              (nsv-1)*(fxstat(4)/fscale)**2+
     *              nsv*((fxstat(1)-f1sv)/fscale)**2+
     *              ifxwt*((fx-fxstat(1))/fscale)**2)
     *              /(nfxe-1))
      end if
      return
      end
