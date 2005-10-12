      subroutine pda_itsum(d, g, iv, liv, lv, p, v, x)
c
c  ***  print iteration summary for ***sol (version 2.3)  ***
c
c  ***  parameter declarations  ***
c
      integer liv, lv, p
      integer iv(liv)
      double precision d(p), g(p), v(lv), x(p)
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  ***  local variables  ***
c
      integer alg, i, iv1, m, nf, ng, ol, pu
c/6
      real model1(6), model2(6)
c/7
c     character*4 model1(6), model2(6)
c/
      double precision nreldf, oldf, preldf, reldf, zero
c
c  ***  intrinsic functions  ***
c/+
      integer iabs
      double precision dabs, dmax1
c/
c  ***  no external functions or subroutines  ***
c
c  ***  subscripts for iv and v  ***
c
      integer algsav, dstnrm, f, fdif, f0, needhd, nfcall, nfcov, ngcov,
     1        ngcall, niter, nreduc, outlev, preduc, prntit, prunit,
     2        reldx, solprt, statpr, stppar, sused, x0prt
c
c  ***  iv subscript values  ***
c
c/6
      data algsav/51/, needhd/36/, nfcall/6/, nfcov/52/, ngcall/30/,
     1     ngcov/53/, niter/31/, outlev/19/, prntit/39/, prunit/21/,
     2     solprt/22/, statpr/23/, sused/64/, x0prt/24/
c/7
c     parameter (algsav=51, needhd=36, nfcall=6, nfcov=52, ngcall=30,
c    1           ngcov=53, niter=31, outlev=19, prntit=39, prunit=21,
c    2           solprt=22, statpr=23, sused=64, x0prt=24)
c/
c
c  ***  v subscript values  ***
c
c/6
      data dstnrm/2/, f/10/, f0/13/, fdif/11/, nreduc/6/, preduc/7/,
     1     reldx/17/, stppar/5/
c/7
c     parameter (dstnrm=2, f=10, f0=13, fdif=11, nreduc=6, preduc=7,
c    1           reldx=17, stppar=5)
c/
c
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c/6
      data model1(1)/4h    /, model1(2)/4h    /, model1(3)/4h    /,
     1     model1(4)/4h    /, model1(5)/4h  g /, model1(6)/4h  s /,
     2     model2(1)/4h g  /, model2(2)/4h s  /, model2(3)/4hg-s /,
     3     model2(4)/4hs-g /, model2(5)/4h-s-g/, model2(6)/4h-g-s/
c/7
c     data model1/'    ','    ','    ','    ','  g ','  s '/,
c    1     model2/' g  ',' s  ','g-s ','s-g ','-s-g','-g-s'/
c/
c
c-------------------------------  body  --------------------------------
c
      pu = iv(prunit)
      if (pu .eq. 0) go to 999
      iv1 = iv(1)
      if (iv1 .gt. 62) iv1 = iv1 - 51
      ol = iv(outlev)
      alg = iv(algsav)
      if (iv1 .lt. 2 .or. iv1 .gt. 15) go to 370
      if (iv1 .ge. 12) go to 120
      if (iv1 .eq. 2 .and. iv(niter) .eq. 0) go to 390
      if (ol .eq. 0) go to 120
      if (iv1 .ge. 10 .and. iv(prntit) .eq. 0) go to 120
      if (iv1 .gt. 2) go to 10
         iv(prntit) = iv(prntit) + 1
         if (iv(prntit) .lt. iabs(ol)) go to 999
 10   nf = iv(nfcall) - iabs(iv(nfcov))
      iv(prntit) = 0
      reldf = zero
      preldf = zero
      oldf = dmax1(dabs(v(f0)), dabs(v(f)))
      if (oldf .le. zero) go to 20
         reldf = v(fdif) / oldf
         preldf = v(preduc) / oldf
 20   if (ol .gt. 0) go to 60
c
c        ***  print short summary line  ***
c
         if (iv(needhd) .eq. 1 .and. alg .eq. 1) write(pu,30)
 30   format(/10h   it   nf,6x,1hf,7x,5hreldf,3x,6hpreldf,3x,5hreldx,
     1       2x,13hmodel  stppar)
         if (iv(needhd) .eq. 1 .and. alg .eq. 2) write(pu,40)
 40   format(/11h    it   nf,7x,1hf,8x,5hreldf,4x,6hpreldf,4x,5hreldx,
     1       3x,6hstppar)
         iv(needhd) = 0
         if (alg .eq. 2) go to 50
         m = iv(sused)
         write(pu,100) iv(niter), nf, v(f), reldf, preldf, v(reldx),
     1                 model1(m), model2(m), v(stppar)
         go to 120
c
 50      write(pu,110) iv(niter), nf, v(f), reldf, preldf, v(reldx),
     1                 v(stppar)
         go to 120
c
c     ***  print long summary line  ***
c
 60   if (iv(needhd) .eq. 1 .and. alg .eq. 1) write(pu,70)
 70   format(/11h    it   nf,6x,1hf,7x,5hreldf,3x,6hpreldf,3x,5hreldx,
     1       2x,13hmodel  stppar,2x,6hd*step,2x,7hnpreldf)
      if (iv(needhd) .eq. 1 .and. alg .eq. 2) write(pu,80)
 80   format(/11h    it   nf,7x,1hf,8x,5hreldf,4x,6hpreldf,4x,5hreldx,
     1       3x,6hstppar,3x,6hd*step,3x,7hnpreldf)
      iv(needhd) = 0
      nreldf = zero
      if (oldf .gt. zero) nreldf = v(nreduc) / oldf
      if (alg .eq. 2) go to 90
      m = iv(sused)
      write(pu,100) iv(niter), nf, v(f), reldf, preldf, v(reldx),
     1             model1(m), model2(m), v(stppar), v(dstnrm), nreldf
      go to 120
c
 90   write(pu,110) iv(niter), nf, v(f), reldf, preldf,
     1             v(reldx), v(stppar), v(dstnrm), nreldf
 100  format(i6,i5,d10.3,2d9.2,d8.1,a3,a4,2d8.1,d9.2)
 110  format(i6,i5,d11.3,2d10.2,3d9.1,d10.2)
c
 120  if (iv(statpr) .lt. 0) go to 430
      go to (999, 999, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310,
     1       330, 350, 520), iv1
c
 130  write(pu,140)
 140  format(/26h ***** x-convergence *****)
      go to 430
c
 150  write(pu,160)
 160  format(/42h ***** relative function convergence *****)
      go to 430
c
 170  write(pu,180)
 180  format(/49h ***** x- and relative function convergence *****)
      go to 430
c
 190  write(pu,200)
 200  format(/42h ***** absolute function convergence *****)
      go to 430
c
 210  write(pu,220)
 220  format(/33h ***** singular convergence *****)
      go to 430
c
 230  write(pu,240)
 240  format(/30h ***** false convergence *****)
      go to 430
c
 250  write(pu,260)
 260  format(/38h ***** function evaluation limit *****)
      go to 430
c
 270  write(pu,280)
 280  format(/28h ***** iteration limit *****)
      go to 430
c
 290  write(pu,300)
 300  format(/22h ***** pda_stopx *****)
      go to 430
c
 310  write(pu,320)
 320  format(/44h ***** initial f(x) cannot be computed *****)
c
      go to 390
c
 330  write(pu,340)
 340  format(/37h ***** bad parameters to assess *****)
      go to 999
c
 350  write(pu,360)
 360  format(/43h ***** gradient could not be computed *****)
      if (iv(niter) .gt. 0) go to 480
      go to 390
c
 370  write(pu,380) iv(1)
 380  format(/14h ***** iv(1) =,i5,6h *****)
      go to 999
c
c  ***  initial call on pda_itsum  ***
c
 390  if (iv(x0prt) .ne. 0) write(pu,400) (i, x(i), d(i), i = 1, p)
 400  format(/23h     i     initial x(i),8x,4hd(i)//(1x,i5,d17.6,d14.3))
c     *** the following are to avoid undefined variables when the
c     *** function evaluation limit is 1...
      v(dstnrm) = zero
      v(fdif) = zero
      v(nreduc) = zero
      v(preduc) = zero
      v(reldx)  = zero
      if (iv1 .ge. 12) go to 999
      iv(needhd) = 0
      iv(prntit) = 0
      if (ol .eq. 0) go to 999
      if (ol .lt. 0 .and. alg .eq. 1) write(pu,30)
      if (ol .lt. 0 .and. alg .eq. 2) write(pu,40)
      if (ol .gt. 0 .and. alg .eq. 1) write(pu,70)
      if (ol .gt. 0 .and. alg .eq. 2) write(pu,80)
      if (alg .eq. 1) write(pu,410) v(f)
      if (alg .eq. 2) write(pu,420) v(f)
 410  format(/11h     0    1,d10.3)
c365  format(/11h     0    1,e11.3)
 420  format(/11h     0    1,d11.3)
      go to 999
c
c  ***  print various information requested on solution  ***
c
 430  iv(needhd) = 1
      if (iv(statpr) .eq. 0) go to 480
         oldf = dmax1(dabs(v(f0)), dabs(v(f)))
         preldf = zero
         nreldf = zero
         if (oldf .le. zero) go to 440
              preldf = v(preduc) / oldf
              nreldf = v(nreduc) / oldf
 440     nf = iv(nfcall) - iv(nfcov)
         ng = iv(ngcall) - iv(ngcov)
         write(pu,450) v(f), v(reldx), nf, ng, preldf, nreldf
 450  format(/9h function,d17.6,8h   reldx,d17.3/12h func. evals,
     1   i8,9x,11hgrad. evals,i8/7h preldf,d16.3,6x,7hnpreldf,d15.3)
c
         if (iv(nfcov) .gt. 0) write(pu,460) iv(nfcov)
 460     format(/1x,i4,50h extra func. evals for covariance and diagnost
     1ics.)
         if (iv(ngcov) .gt. 0) write(pu,470) iv(ngcov)
 470     format(1x,i4,50h extra grad. evals for covariance and diagnosti
     1cs.)
c
 480  if (iv(solprt) .eq. 0) go to 999
         iv(needhd) = 1
         write(pu,490)
 490  format(/22h     i      final x(i),8x,4hd(i),10x,4hg(i)/)
         do 500 i = 1, p
              write(pu,510) i, x(i), d(i), g(i)
 500          continue
 510     format(1x,i5,d16.6,2d14.3)
      go to 999
c
 520  write(pu,530)
 530  format(/24h inconsistent dimensions)
 999  return
c  ***  last card of pda_itsum follows  ***
      end
