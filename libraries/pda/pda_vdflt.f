      subroutine pda_vdflt(alg, lv, v)
c
c  ***  supply ***sol (version 2.3) default values to v  ***
c
c  ***  alg = 1 means regression constants.
c  ***  alg = 2 means general unconstrained optimization constants.
c
      integer alg, lv
      double precision v(lv)
c/+
      double precision dmax1
c/
      external pda_rmdcon
      double precision pda_rmdcon
c pda_rmdcon... returns machine-dependent constants
c
      double precision machep, mepcrt, one, sqteps, three
c
c  ***  subscripts for v  ***
c
      integer afctol, bias, cosmin, decfac, delta0, dfac, dinit, dltfdc,
     1        dltfdj, dtinit, d0init, epslon, eta0, fuzz, huberc,
     2        incfac, lmax0, lmaxs, phmnfc, phmxfc, rdfcmn, rdfcmx,
     3        rfctol, rlimit, rsptol, sctol, sigmin, tuner1, tuner2,
     4        tuner3, tuner4, tuner5, xctol, xftol
c
c/6
      data one/1.d+0/, three/3.d+0/
c/7
c     parameter (one=1.d+0, three=3.d+0)
c/
c
c  ***  v subscript values  ***
c
c/6
      data afctol/31/, bias/43/, cosmin/47/, decfac/22/, delta0/44/,
     1     dfac/41/, dinit/38/, dltfdc/42/, dltfdj/43/, dtinit/39/,
     2     d0init/40/, epslon/19/, eta0/42/, fuzz/45/, huberc/48/,
     3     incfac/23/, lmax0/35/, lmaxs/36/, phmnfc/20/, phmxfc/21/,
     4     rdfcmn/24/, rdfcmx/25/, rfctol/32/, rlimit/46/, rsptol/49/,
     5     sctol/37/, sigmin/50/, tuner1/26/, tuner2/27/, tuner3/28/,
     6     tuner4/29/, tuner5/30/, xctol/33/, xftol/34/
c/7
c     parameter (afctol=31, bias=43, cosmin=47, decfac=22, delta0=44,
c    1           dfac=41, dinit=38, dltfdc=42, dltfdj=43, dtinit=39,
c    2           d0init=40, epslon=19, eta0=42, fuzz=45, huberc=48,
c    3           incfac=23, lmax0=35, lmaxs=36, phmnfc=20, phmxfc=21,
c    4           rdfcmn=24, rdfcmx=25, rfctol=32, rlimit=46, rsptol=49,
c    5           sctol=37, sigmin=50, tuner1=26, tuner2=27, tuner3=28,
c    6           tuner4=29, tuner5=30, xctol=33, xftol=34)
c/
c
c-------------------------------  body  --------------------------------
c
      machep = pda_rmdcon(3)
      v(afctol) = 1.d-20
      if (machep .gt. 1.d-10) v(afctol) = machep**2
      v(decfac) = 0.5d+0
      sqteps = pda_rmdcon(4)
      v(dfac) = 0.6d+0
      v(delta0) = sqteps
      v(dtinit) = 1.d-6
      mepcrt = machep ** (one/three)
      v(d0init) = 1.d+0
      v(epslon) = 0.1d+0
      v(incfac) = 2.d+0
      v(lmax0) = 1.d+0
      v(lmaxs) = 1.d+0
      v(phmnfc) = -0.1d+0
      v(phmxfc) = 0.1d+0
      v(rdfcmn) = 0.1d+0
      v(rdfcmx) = 4.d+0
      v(rfctol) = dmax1(1.d-10, mepcrt**2)
      v(sctol) = v(rfctol)
      v(tuner1) = 0.1d+0
      v(tuner2) = 1.d-4
      v(tuner3) = 0.75d+0
      v(tuner4) = 0.5d+0
      v(tuner5) = 0.75d+0
      v(xctol) = sqteps
      v(xftol) = 1.d+2 * machep
c
      if (alg .ge. 2) go to 10
c
c  ***  regression  values
c
      v(cosmin) = dmax1(1.d-6, 1.d+2 * machep)
      v(dinit) = 0.d+0
      v(dltfdc) = mepcrt
      v(dltfdj) = sqteps
      v(fuzz) = 1.5d+0
      v(huberc) = 0.7d+0
      v(rlimit) = pda_rmdcon(5)
      v(rsptol) = 1.d-3
      v(sigmin) = 1.d-4
      go to 999
c
c  ***  general optimization values
c
 10   v(bias) = 0.8d+0
      v(dinit) = -1.0d+0
      v(eta0) = 1.0d+3 * machep
c
 999  return
c  ***  last card of pda_vdflt follows  ***
      end
