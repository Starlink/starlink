      subroutine pda_parck(alg, d, iv, liv, lv, n, v)
c
c  ***  check ***sol (version 2.3) parameters, print changed values  ***
c
c  ***  alg = 1 for regression, alg = 2 for general unconstrained opt.
c
      integer alg, liv, lv, n
      integer iv(liv)
      double precision d(n), v(lv)
c
      external pda_rmdcon, pda_vcopy, pda_vdflt
      double precision pda_rmdcon
c pda_rmdcon -- returns machine-dependent constants.
c pda_vcopy  -- copies one vector to another.
c pda_vdflt  -- supplies default parameter values to v alone.
c/+
      integer max0
c/
c
c  ***  local variables  ***
c
      integer i, ii, iv1, j, k, l, m, miv1, miv2, ndfalt, parsv1, pu
      integer ijmp, jlim(2), miniv(2), ndflt(2)
c/6
      integer varnm(2), sh(2)
      real cngd(3), dflt(3), vn(2,34), which(3)
c/7
c     character*1 varnm(2), sh(2)
c     character*4 cngd(3), dflt(3), vn(2,34), which(3)
c/
      double precision big, machep, tiny, vk, vm(34), vx(34), zero
c
c  ***  iv and v subscripts  ***
c
      integer algsav, dinit, dtype, dtype0, epslon, inits, ivneed,
     1        lastiv, lastv, lmat, nextiv, nextv, nvdflt, oldn,
     2        parprt, parsav, perm, prunit, vneed
c
c
c/6
      data algsav/51/, dinit/38/, dtype/16/, dtype0/54/, epslon/19/,
     1     inits/25/, ivneed/3/, lastiv/44/, lastv/45/, lmat/42/,
     2     nextiv/46/, nextv/47/, nvdflt/50/, oldn/38/, parprt/20/,
     3     parsav/49/, perm/58/, prunit/21/, vneed/4/
c/7
c     parameter (algsav=51, dinit=38, dtype=16, dtype0=54, epslon=19,
c    1           inits=25, ivneed=3, lastiv=44, lastv=45, lmat=42,
c    2           nextiv=46, nextv=47, nvdflt=50, oldn=38, parprt=20,
c    3           parsav=49, perm=58, prunit=21, vneed=4)
c     save big, machep, tiny
c/
c
      data big/0.d+0/, machep/-1.d+0/, tiny/1.d+0/, zero/0.d+0/
c/6
      data vn(1,1),vn(2,1)/4hepsl,4hon../
      data vn(1,2),vn(2,2)/4hphmn,4hfc../
      data vn(1,3),vn(2,3)/4hphmx,4hfc../
      data vn(1,4),vn(2,4)/4hdecf,4hac../
      data vn(1,5),vn(2,5)/4hincf,4hac../
      data vn(1,6),vn(2,6)/4hrdfc,4hmn../
      data vn(1,7),vn(2,7)/4hrdfc,4hmx../
      data vn(1,8),vn(2,8)/4htune,4hr1../
      data vn(1,9),vn(2,9)/4htune,4hr2../
      data vn(1,10),vn(2,10)/4htune,4hr3../
      data vn(1,11),vn(2,11)/4htune,4hr4../
      data vn(1,12),vn(2,12)/4htune,4hr5../
      data vn(1,13),vn(2,13)/4hafct,4hol../
      data vn(1,14),vn(2,14)/4hrfct,4hol../
      data vn(1,15),vn(2,15)/4hxcto,4hl.../
      data vn(1,16),vn(2,16)/4hxfto,4hl.../
      data vn(1,17),vn(2,17)/4hlmax,4h0.../
      data vn(1,18),vn(2,18)/4hlmax,4hs.../
      data vn(1,19),vn(2,19)/4hscto,4hl.../
      data vn(1,20),vn(2,20)/4hdini,4ht.../
      data vn(1,21),vn(2,21)/4hdtin,4hit../
      data vn(1,22),vn(2,22)/4hd0in,4hit../
      data vn(1,23),vn(2,23)/4hdfac,4h..../
      data vn(1,24),vn(2,24)/4hdltf,4hdc../
      data vn(1,25),vn(2,25)/4hdltf,4hdj../
      data vn(1,26),vn(2,26)/4hdelt,4ha0../
      data vn(1,27),vn(2,27)/4hfuzz,4h..../
      data vn(1,28),vn(2,28)/4hrlim,4hit../
      data vn(1,29),vn(2,29)/4hcosm,4hin../
      data vn(1,30),vn(2,30)/4hhube,4hrc../
      data vn(1,31),vn(2,31)/4hrspt,4hol../
      data vn(1,32),vn(2,32)/4hsigm,4hin../
      data vn(1,33),vn(2,33)/4heta0,4h..../
      data vn(1,34),vn(2,34)/4hbias,4h..../
c/7
c     data vn(1,1),vn(2,1)/'epsl','on..'/
c     data vn(1,2),vn(2,2)/'phmn','fc..'/
c     data vn(1,3),vn(2,3)/'phmx','fc..'/
c     data vn(1,4),vn(2,4)/'decf','ac..'/
c     data vn(1,5),vn(2,5)/'incf','ac..'/
c     data vn(1,6),vn(2,6)/'rdfc','mn..'/
c     data vn(1,7),vn(2,7)/'rdfc','mx..'/
c     data vn(1,8),vn(2,8)/'tune','r1..'/
c     data vn(1,9),vn(2,9)/'tune','r2..'/
c     data vn(1,10),vn(2,10)/'tune','r3..'/
c     data vn(1,11),vn(2,11)/'tune','r4..'/
c     data vn(1,12),vn(2,12)/'tune','r5..'/
c     data vn(1,13),vn(2,13)/'afct','ol..'/
c     data vn(1,14),vn(2,14)/'rfct','ol..'/
c     data vn(1,15),vn(2,15)/'xcto','l...'/
c     data vn(1,16),vn(2,16)/'xfto','l...'/
c     data vn(1,17),vn(2,17)/'lmax','0...'/
c     data vn(1,18),vn(2,18)/'lmax','s...'/
c     data vn(1,19),vn(2,19)/'scto','l...'/
c     data vn(1,20),vn(2,20)/'dini','t...'/
c     data vn(1,21),vn(2,21)/'dtin','it..'/
c     data vn(1,22),vn(2,22)/'d0in','it..'/
c     data vn(1,23),vn(2,23)/'dfac','....'/
c     data vn(1,24),vn(2,24)/'dltf','dc..'/
c     data vn(1,25),vn(2,25)/'dltf','dj..'/
c     data vn(1,26),vn(2,26)/'delt','a0..'/
c     data vn(1,27),vn(2,27)/'fuzz','....'/
c     data vn(1,28),vn(2,28)/'rlim','it..'/
c     data vn(1,29),vn(2,29)/'cosm','in..'/
c     data vn(1,30),vn(2,30)/'hube','rc..'/
c     data vn(1,31),vn(2,31)/'rspt','ol..'/
c     data vn(1,32),vn(2,32)/'sigm','in..'/
c     data vn(1,33),vn(2,33)/'eta0','....'/
c     data vn(1,34),vn(2,34)/'bias','....'/
c/
c
      data vm(1)/1.0d-3/, vm(2)/-0.99d+0/, vm(3)/1.0d-3/, vm(4)/1.0d-2/,
     1     vm(5)/1.2d+0/, vm(6)/1.d-2/, vm(7)/1.2d+0/, vm(8)/0.d+0/,
     2     vm(9)/0.d+0/, vm(10)/1.d-3/, vm(11)/-1.d+0/, vm(13)/0.d+0/,
     3     vm(15)/0.d+0/, vm(16)/0.d+0/, vm(19)/0.d+0/, vm(20)/-10.d+0/,
     4     vm(21)/0.d+0/, vm(22)/0.d+0/, vm(23)/0.d+0/, vm(27)/1.01d+0/,
     5     vm(28)/1.d+10/, vm(30)/0.d+0/, vm(31)/0.d+0/, vm(32)/0.d+0/,
     6     vm(34)/0.d+0/
      data vx(1)/0.9d+0/, vx(2)/-1.d-3/, vx(3)/1.d+1/, vx(4)/0.8d+0/,
     1     vx(5)/1.d+2/, vx(6)/0.8d+0/, vx(7)/1.d+2/, vx(8)/0.5d+0/,
     2     vx(9)/0.5d+0/, vx(10)/1.d+0/, vx(11)/1.d+0/, vx(14)/0.1d+0/,
     3     vx(15)/1.d+0/, vx(16)/1.d+0/, vx(19)/1.d+0/, vx(23)/1.d+0/,
     4     vx(24)/1.d+0/, vx(25)/1.d+0/, vx(26)/1.d+0/, vx(27)/1.d+10/,
     5     vx(29)/1.d+0/, vx(31)/1.d+0/, vx(32)/1.d+0/, vx(33)/1.d+0/,
     6     vx(34)/1.d+0/
c
c/6
      data varnm(1)/1hp/, varnm(2)/1hn/, sh(1)/1hs/, sh(2)/1hh/
      data cngd(1),cngd(2),cngd(3)/4h---c,4hhang,4hed v/,
     1     dflt(1),dflt(2),dflt(3)/4hnond,4hefau,4hlt v/
c/7
c     data varnm(1)/'p'/, varnm(2)/'n'/, sh(1)/'s'/, sh(2)/'h'/
c     data cngd(1),cngd(2),cngd(3)/'---c','hang','ed v'/,
c    1     dflt(1),dflt(2),dflt(3)/'nond','efau','lt v'/
c/
      data ijmp/33/, jlim(1)/0/, jlim(2)/24/, ndflt(1)/32/, ndflt(2)/25/
      data miniv(1)/80/, miniv(2)/59/
c
c...............................  body  ................................
c
      pu = 0
      if (prunit .le. liv) pu = iv(prunit)
      if (alg .lt. 1 .or. alg .gt. 2) go to 340
      if (iv(1) .eq. 0) call pda_deflt(alg, iv, liv, lv, v)
      iv1 = iv(1)
      if (iv1 .ne. 13 .and. iv1 .ne. 12) go to 10
      miv1 = miniv(alg)
      if (perm .le. liv) miv1 = max0(miv1, iv(perm) - 1)
      if (ivneed .le. liv) miv2 = miv1 + max0(iv(ivneed), 0)
      if (lastiv .le. liv) iv(lastiv) = miv2
      if (liv .lt. miv1) go to 300
      iv(ivneed) = 0
      iv(lastv) = max0(iv(vneed), 0) + iv(lmat) - 1
      iv(vneed) = 0
      if (liv .lt. miv2) go to 300
      if (lv .lt. iv(lastv)) go to 320
 10   if (alg .eq. iv(algsav)) go to 30
         if (pu .ne. 0) write(pu,20) alg, iv(algsav)
 20      format(/43h the first parameter to pda_deflt should be,i3,
     1          12h rather than,i3)
         iv(1) = 82
         go to 999
 30   if (iv1 .lt. 12 .or. iv1 .gt. 14) go to 60
         if (n .ge. 1) go to 50
              iv(1) = 81
              if (pu .eq. 0) go to 999
              write(pu,40) varnm(alg), n
 40           format(/8h /// bad,a1,2h =,i5)
              go to 999
 50      if (iv1 .ne. 14) iv(nextiv) = iv(perm)
         if (iv1 .ne. 14) iv(nextv) = iv(lmat)
         if (iv1 .eq. 13) go to 999
         k = iv(parsav) - epslon
         call pda_vdflt(alg, lv-k, v(k+1))
         iv(dtype0) = 2 - alg
         iv(oldn) = n
         which(1) = dflt(1)
         which(2) = dflt(2)
         which(3) = dflt(3)
         go to 110
 60   if (n .eq. iv(oldn)) go to 80
         iv(1) = 17
         if (pu .eq. 0) go to 999
         write(pu,70) varnm(alg), iv(oldn), n
 70      format(/5h /// ,1a1,14h changed from ,i5,4h to ,i5)
         go to 999
c
 80   if (iv1 .le. 11 .and. iv1 .ge. 1) go to 100
         iv(1) = 80
         if (pu .ne. 0) write(pu,90) iv1
 90      format(/13h ///  iv(1) =,i5,28h should be between 0 and 14.)
         go to 999
c
 100  which(1) = cngd(1)
      which(2) = cngd(2)
      which(3) = cngd(3)
c
 110  if (iv1 .eq. 14) iv1 = 12
      if (big .gt. tiny) go to 120
         tiny = pda_rmdcon(1)
         machep = pda_rmdcon(3)
         big = pda_rmdcon(6)
         vm(12) = machep
         vx(12) = big
         vx(13) = big
         vm(14) = machep
         vm(17) = tiny
         vx(17) = big
         vm(18) = tiny
         vx(18) = big
         vx(20) = big
         vx(21) = big
         vx(22) = big
         vm(24) = machep
         vm(25) = machep
         vm(26) = machep
         vx(28) = pda_rmdcon(5)
         vm(29) = machep
         vx(30) = big
         vm(33) = machep
 120  m = 0
      i = 1
      j = jlim(alg)
      k = epslon
      ndfalt = ndflt(alg)
      do 150 l = 1, ndfalt
         vk = v(k)
         if (vk .ge. vm(i) .and. vk .le. vx(i)) go to 140
              m = k
              if (pu .ne. 0) write(pu,130) vn(1,i), vn(2,i), k, vk,
     1                                    vm(i), vx(i)
 130          format(/6h ///  ,2a4,5h.. v(,i2,3h) =,d11.3,7h should,
     1               11h be between,d11.3,4h and,d11.3)
 140     k = k + 1
         i = i + 1
         if (i .eq. j) i = ijmp
 150     continue
c
      if (iv(nvdflt) .eq. ndfalt) go to 170
         iv(1) = 51
         if (pu .eq. 0) go to 999
         write(pu,160) iv(nvdflt), ndfalt
 160     format(/13h iv(nvdflt) =,i5,13h rather than ,i5)
         go to 999
 170  if ((iv(dtype) .gt. 0 .or. v(dinit) .gt. zero) .and. iv1 .eq. 12)
     1                  go to 200
      do 190 i = 1, n
         if (d(i) .gt. zero) go to 190
              m = 18
              if (pu .ne. 0) write(pu,180) i, d(i)
 180     format(/8h ///  d(,i3,3h) =,d11.3,19h should be positive)
 190     continue
 200  if (m .eq. 0) go to 210
         iv(1) = m
         go to 999
c
 210  if (pu .eq. 0 .or. iv(parprt) .eq. 0) go to 999
      if (iv1 .ne. 12 .or. iv(inits) .eq. alg-1) go to 230
         m = 1
         write(pu,220) sh(alg), iv(inits)
 220     format(/22h nondefault values..../5h init,a1,14h..... iv(25) =,
     1          i3)
 230  if (iv(dtype) .eq. iv(dtype0)) go to 250
         if (m .eq. 0) write(pu,260) which
         m = 1
         write(pu,240) iv(dtype)
 240     format(20h dtype..... iv(16) =,i3)
 250  i = 1
      j = jlim(alg)
      k = epslon
      l = iv(parsav)
      ndfalt = ndflt(alg)
      do 290 ii = 1, ndfalt
         if (v(k) .eq. v(l)) go to 280
              if (m .eq. 0) write(pu,260) which
 260          format(/1h ,3a4,9halues..../)
              m = 1
              write(pu,270) vn(1,i), vn(2,i), k, v(k)
 270          format(1x,2a4,5h.. v(,i2,3h) =,d15.7)
 280     k = k + 1
         l = l + 1
         i = i + 1
         if (i .eq. j) i = ijmp
 290     continue
c
      iv(dtype0) = iv(dtype)
      parsv1 = iv(parsav)
      call pda_vcopy(iv(nvdflt), v(parsv1), v(epslon))
      go to 999
c
 300  iv(1) = 15
      if (pu .eq. 0) go to 999
      write(pu,310) liv, miv2
 310  format(/10h /// liv =,i5,17h must be at least,i5)
      if (liv .lt. miv1) go to 999
      if (lv .lt. iv(lastv)) go to 320
      go to 999
c
 320  iv(1) = 16
      if (pu .eq. 0) go to 999
      write(pu,330) lv, iv(lastv)
 330  format(/9h /// lv =,i5,17h must be at least,i5)
      go to 999
c
 340  iv(1) = 67
      if (pu .eq. 0) go to 999
      write(pu,350) alg
 350  format(/10h /// alg =,i5,15h must be 1 or 2)
c
 999  return
c  ***  last card of pda_parck follows  ***
      end
