      subroutine pda_deflt(alg, iv, liv, lv, v)
c
c  ***  supply ***sol (version 2.3) default values to iv and v  ***
c
c  ***  alg = 1 means regression constants.
c  ***  alg = 2 means general unconstrained optimization constants.
c
      integer liv, lv
      integer alg, iv(liv)
      double precision v(lv)
c
      external pda_imdcon, pda_vdflt
      integer pda_imdcon
c pda_imdcon... returns machine-dependent integer constants.
c pda_vdflt.... provides default values to v.
c
      integer miv, mv
      integer miniv(2), minv(2)
c
c  ***  subscripts for iv  ***
c
      integer algsav, covprt, covreq, dtype, hc, ierr, inith, inits,
     1        ipivot, ivneed, lastiv, lastv, lmat, mxfcal, mxiter,
     2        nfcov, ngcov, npda_vdflt, outlev, parprt, parsav, perm,
     3        prunit, qrtyp, rdreq, rmat, solprt, statpr, vneed,
     4        vsave, x0prt
c
c  ***  iv subscript values  ***
c
c/6
      data algsav/51/, covprt/14/, covreq/15/, dtype/16/, hc/71/,
     1     ierr/75/, inith/25/, inits/25/, ipivot/76/, ivneed/3/,
     2     lastiv/44/, lastv/45/, lmat/42/, mxfcal/17/, mxiter/18/,
     3     nfcov/52/, ngcov/53/, npda_vdflt/50/, outlev/19/, parprt/20/,
     4     parsav/49/, perm/58/, prunit/21/, qrtyp/80/, rdreq/57/,
     5     rmat/78/, solprt/22/, statpr/23/, vneed/4/, vsave/60/,
     6     x0prt/24/
c/7
c     parameter (algsav=51, covprt=14, covreq=15, dtype=16, hc=71,
c    1           ierr=75, inith=25, inits=25, ipivot=76, ivneed=3,
c    2           lastiv=44, lastv=45, lmat=42, mxfcal=17, mxiter=18,
c    3           nfcov=52, ngcov=53, npda_vdflt=50, outlev=19, parprt=20,
c    4           parsav=49, perm=58, prunit=21, qrtyp=80, rdreq=57,
c    5           rmat=78, solprt=22, statpr=23, vneed=4, vsave=60,
c    6           x0prt=24)
c/
      data miniv(1)/80/, miniv(2)/59/, minv(1)/98/, minv(2)/71/
c
c-------------------------------  body  --------------------------------
c
      if (alg .lt. 1 .or. alg .gt. 2) go to 40
      miv = miniv(alg)
      if (liv .lt. miv) go to 20
      mv = minv(alg)
      if (lv .lt. mv) go to 30
      call pda_vdflt(alg, lv, v)
      iv(1) = 12
      iv(algsav) = alg
      iv(ivneed) = 0
      iv(lastiv) = miv
      iv(lastv) = mv
      iv(lmat) = mv + 1
      iv(mxfcal) = 200
      iv(mxiter) = 150
      iv(outlev) = 1
      iv(parprt) = 1
      iv(perm) = miv + 1
      iv(prunit) = 0
      iv(solprt) = 1
      iv(statpr) = 1
      iv(vneed) = 0
      iv(x0prt) = 1
c
      if (alg .ge. 2) go to 10
c
c  ***  regression  values
c
      iv(covprt) = 3
      iv(covreq) = 1
      iv(dtype) = 1
      iv(hc) = 0
      iv(ierr) = 0
      iv(inits) = 0
      iv(ipivot) = 0
      iv(npda_vdflt) = 32
      iv(parsav) = 67
      iv(qrtyp) = 1
      iv(rdreq) = 3
      iv(rmat) = 0
      iv(vsave) = 58
      go to 999
c
c  ***  general optimization values
c
 10   iv(dtype) = 0
      iv(inith) = 1
      iv(nfcov) = 0
      iv(ngcov) = 0
      iv(npda_vdflt) = 25
      iv(parsav) = 47
      go to 999
c
 20   iv(1) = 15
      go to 999
c
 30   iv(1) = 16
      go to 999
c
 40   iv(1) = 67
c
 999  return
c  ***  last card of pda_deflt follows  ***
      end
