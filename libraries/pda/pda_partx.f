      subroutine pda_partx (n,ip,absdx,nsubs,nsvals)
c
      integer n,nsubs,nsvals(*)
      integer ip(n)
      double precision absdx(n)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_partx partitions the vector x by grouping components of
c similar magnitude of change.
c
c input
c
c   n      - number of components (problem dimension)
c
c   ip     - permutation vector
c
c   absdx  - vector of magnitude of change in x
c
c   nsvals - integer array dimensioned .ge. int(n/nsmin)
c
c output
c
c   nsubs  - number of subspaces
c
c   nsvals - integer array of subspace dimensions
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
      integer i,nleft,ns1,ns2,nused
      double precision asleft,as1,as1max,as2,gap,gapmax
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
      nsubs = 0
      nused = 0
      nleft = n
      asleft = absdx(1)
      do 10 i = 2,n
        asleft = asleft+absdx(i)
   10 continue
   20 continue
      if (nused .lt. n) then
        nsubs = nsubs+1
        as1 = 0.d0
        do 30 i = 1,nsmin-1
          as1 = as1+absdx(ip(nused+i))
   30   continue
        gapmax = -1.d0
        do 40 ns1 = nsmin,min(nsmax,nleft)
          as1 = as1+absdx(ip(nused+ns1))
          ns2 = nleft-ns1
          if (ns2 .gt. 0) then
            if (ns2 .ge. ((ns2-1)/nsmax+1)*nsmin) then
              as2 = asleft-as1
              gap = as1/ns1-as2/ns2
              if (gap .gt. gapmax) then
                gapmax = gap
                nsvals(nsubs) = ns1
                as1max = as1
              end if
            end if
          else
            if (as1/ns1 .gt. gapmax) then
              nsvals(nsubs) = ns1
              return
            end if
          end if
   40   continue
        nused = nused+nsvals(nsubs)
        nleft = n-nused
        asleft = asleft-as1max
        go to 20
      end if
      return
      end
