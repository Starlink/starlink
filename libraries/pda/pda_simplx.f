      subroutine pda_simplx (f,n,step,ns,ips,maxnfe,cmode,x,fx,
     *                   nfe,s,fs,iflag)
c
      integer n,ns,maxnfe,nfe,iflag
      integer ips(ns)
      double precision f,step(n),x(n),fx,s(ns,ns+3),fs(ns+1)
      logical cmode
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_simplx uses the Nelder-Mead simplex method to minimize the
c function f on a subspace.
c
c input
c
c   f      - function to be minimized, declared external in
c            calling routine
c
c   n      - problem dimension
c
c   step   - stepsizes for corresponding components of x
c
c   ns     - subspace dimension
c
c   ips    - permutation vector
c
c   maxnfe - maximum number of function evaluations
c
c   cmode  - logical switch
c            = .true.  : continuation of previous call
c            = .false. : first call
c
c   x      - starting guess for minimum
c
c   fx     - value of f at x
c
c   nfe    - number of function evaluations
c
c   s      - double precision work array of dimension .ge.
c            ns*(ns+3) used to store simplex
c
c   fs     - double precision work array of dimension .ge.
c            ns+1 used to store function values of simplex
c            vertices
c
c output
c
c   x      - computed minimum
c
c   fx     - value of f at x
c
c   nfe    - incremented number of function evaluations
c
c   iflag  - error flag
c            = -1 : maxnfe exceeded
c            =  0 : simplex reduced by factor of psi
c            =  1 : limit of machine precision
c            =  2 : reached fstop
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
      integer i,icent,ih,il,inew,is,itemp,j,npts
      double precision pda_dist,dum,fc,fe,fr,tol
      logical small,updatc
c
      save
c
c subroutines and functions
c
      external f,pda_calcc,pda_dist,pda_evalf,pda_newpt,
     *           pda_order,pda_start
c   blas
      external pda_dcopy
c   fortran
      intrinsic min
c
c-----------------------------------------------------------
c
      if (cmode) go to 50
      npts = ns+1
      icent = ns+2
      itemp = ns+3
      updatc = .false.
      call pda_start (n,x,step,ns,ips,s,small)
      if (small) then
        iflag = 1
        return
      end if
      if (irepl .gt. 0) then
        new = .false.
        call pda_evalf (f,ns,ips,s(1,1),n,x,fs(1),nfe)
      else
        fs(1) = fx
      end if
      new = .true.
      do 10 j = 2,npts
        call pda_evalf (f,ns,ips,s(1,j),n,x,fs(j),nfe)
   10 continue
      il = 1
      call pda_order (npts,fs,il,is,ih)
      tol = psi*pda_dist(ns,s(1,ih),s(1,il))
c
c     main loop
c
   20 continue
        call pda_calcc (ns,s,ih,inew,updatc,s(1,icent))
        updatc = .true.
        inew = ih
c
c       reflect
c
        call pda_newpt (ns,alpha,s(1,icent),s(1,ih),.true.,
     *              s(1,itemp),small)
        if (small) go to 40
        call pda_evalf (f,ns,ips,s(1,itemp),n,x,fr,nfe)
        if (fr .lt. fs(il)) then
c
c         expand
c
          call pda_newpt (ns,-gamma,s(1,icent),s(1,itemp),
     *                .true.,s(1,ih),small)
          if (small) go to 40
          call pda_evalf (f,ns,ips,s(1,ih),n,x,fe,nfe)
          if (fe .lt. fr) then
            fs(ih) = fe
          else
            call pda_dcopy (ns,s(1,itemp),1,s(1,ih),1)
            fs(ih) = fr
          end if
        else if (fr .lt. fs(is)) then
c
c         accept reflected point
c
          call pda_dcopy (ns,s(1,itemp),1,s(1,ih),1)
          fs(ih) = fr
        else
c
c         contract
c
          if (fr .gt. fs(ih)) then
            call pda_newpt (ns,-beta,s(1,icent),s(1,ih),.true.,
     *                  s(1,itemp),small)
          else
            call pda_newpt (ns,-beta,s(1,icent),s(1,itemp),
     *                  .false.,dum,small)
          end if
          if (small) go to 40
          call pda_evalf (f,ns,ips,s(1,itemp),n,x,fc,nfe)
          if (fc .lt. min(fr,fs(ih))) then
            call pda_dcopy (ns,s(1,itemp),1,s(1,ih),1)
            fs(ih) = fc
          else
c
c           shrink simplex
c
            do 30 j = 1,npts
              if (j .ne. il) then
                call pda_newpt (ns,-delta,s(1,il),s(1,j),
     *                      .false.,dum,small)
                if (small) go to 40
                call pda_evalf (f,ns,ips,s(1,j),n,x,fs(j),nfe)
              end if
   30       continue
          end if
          updatc = .false.
        end if
        call pda_order (npts,fs,il,is,ih)
c
c       check termination
c
   40   continue
        if (irepl .eq. 0) then
          fx = fs(il)
        else
          fx = sfbest
        end if
   50   continue
        if (nfstop .gt. 0 .and. fx .le. sfstop .and.
     *      nfxe .ge. nfstop) then
          iflag = 2
        else if (nfe .ge. maxnfe) then
          iflag = -1
        else if (pda_dist(ns,s(1,ih),s(1,il)) .le. tol .or.
     *           small) then
          iflag = 0
        else
          go to 20
        end if
c
c     end main loop, return best point
c
      do 60 i = 1,ns
        x(ips(i)) = s(i,il)
   60 continue
      return
      end
