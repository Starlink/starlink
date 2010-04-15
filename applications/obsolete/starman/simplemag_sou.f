CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_APERMAG -- Perform Aperture Photometry
C
C  alan penny                   RAL                        1991 Nov

      subroutine smp_apermag ( )

      implicit none
      include 'apermag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

C--
      integer iptbi, iptbo
Cbegin


      call smp_ap_gcl ( iptbi, iptbo )					!Get user input
      if ( ST_FAILED ) return

      call smp_ap_work ( %val(iptbi), %val(iptbo) )			!Do photometry


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_AP_GCL -- Get the user input
C
C  alan penny                RAL                      1991 Nov

      subroutine smp_ap_gcl ( iptbi, iptbo )

      implicit none
      include 'apermag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      integer iptbi	!o: Pointer to input table
      integer iptbo	!o: Pointer to output table
C--
      integer istat, iv, k
      character*50 title
      character*20 head(14)
      data head / 'X', 'Y', 'Magnitude', 'Error', 'Peak', 'Dx', 'Dy',
     +            'Flux', 'Number', 'Invalid', 'Sky', 'SKy Error',
     +            'Sky Number', 'Sky Invalid' /
Cbegin



      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,istat)	!
      if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL

      call optabr ( 'INSTARS', iptbi, TBVX, TBY, .false., istat )	!
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get1r ( 'APDIA', APDIA, 20.0, 0.1, 200.0 )			!Get star aperture

      SKYDIAIN = 30.0							!Get sky aperture
      SKYDIAOUT = 40.0
      call get2r ( 'SKYDIAS', SKYDIAIN, SKYDIAOUT, .true., 0.1, 200.0 )

      call get1b ( 'CENTRE', CENTRE, .false. )				!Centre star before photometry?

      call get1r ( 'POISV', POISV, 1.0, 1.0e-10, 1.0e10 )		!Get Poisson converter

      call get1r ( 'NOISE', NOISE, 0.0, 0.0, 1.0e10 )			!Get extra noise per pixel

      call get1b ( 'TYPING', TYPING, .true. )				!Typing results as work done?

      TBXO = 14								!Open output table
      TBVXO = TBXO + 5
      call optabw ( 'OUT', iptbo, TBVXO, TBY, .false., istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call gtdesc ( 'INSTARS', 'TITLE', title, 'Output from Apermag',	!
     +                 iv, istat )
      if ( istat.ne.0 ) title = 'Output from Apermag'
      call get1c ( 'TITLE', title, title, .true. )
      call ptdesc ( 'OUT', 'TITLE', title )

      do k = 1, TBXO							!
         call pthead ( 'OUT', k, head(k), istat )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_AP_WORK -- Do the photometry of the stars
C
C  alan penny                RAL                      1991 Nov

      subroutine smp_ap_work ( tbin, tbout )

      implicit none
      include 'apermag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      real          tbin(TBVX,TBY)	!i: Star list
      real          tbout(TBVXO,TBY)	!o: Output results
C--
      integer k, ninval, kninval, nbadb, ngoodb, iter, numpix, lxg, lyg
      real    rv, stferr, stmerr, stfl, amag, amaga, aerr,
     +        atop, top, skraw, sklev, skarea, skerr, topb, xa, ya, rx,
     +        ry, dx, dy, dxo, dyo, rms, av, starea, sterr, ht,
     +        base, straw, ax, ay, x, y
      character text*72, textp*40
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call coprr ( tbin, TBVX, TBY, 1, 5, 1, TBY, tbout, TBVXO, TBY, 	!Copy names
     +             1, 1 )

      if ( TYPING ) call printo ( ' Number    X        Y      Mag'//	!Type header
     +                  '   Error   Peak     Flux       Sky    Bad' )

      lxg = min(100,int(APDIA))
      lyg = lxg
      do k = 1, TBY							!Do the stars

         x = tbin(6,k)							!Position
         y = tbin(7,k)

         dx = 0.0							!Centre on new position?
         dy = 0.0
         if ( CENTRE ) then
            if ( IMTYPE.eq.'SHORT' ) then
               call gauss2sa ( %val(IPIM), NX, NY, x, y, lxg, lyg, 0,
     +                         2.0, 2.0, INVAL, 20, amag, ht, base,
     +                         dxo, dyo, xa, ya, rx, ry, rms, iter,
     +                         ninval )
            else
               call gauss2ra ( %val(IPIM), NX, NY, x, y, lxg, lyg, 0,
     +                         2.0, 2.0, RINVAL, 20, amag, ht, base,
     +                         dxo, dyo, xa, ya, rx, ry, rms, iter,
     +                         ninval )
            endif
            if ( amag.lt.49.0 ) then
               x = xa
               y = ya
               dx = dxo
               dy = dyo
            endif
         endif

         if ( IMTYPE.eq.'SHORT' ) then					!Measure
            call smp_ap_doits ( %val(IPIM), x, y, .false., 0.0, APDIA,
     +                          POISV, NOISE, straw, av, starea, sterr,
     +                          ninval, numpix, top)

            call smp_ap_doits ( %val(IPIM), x, y, .false., SKYDIAIN, 	!Measure Sky
     +                          SKYDIAOUT, POISV, NOISE, skraw, sklev,
     +                          skarea, skerr, nbadb, ngoodb, topb )
         else
            call smp_ap_doitr ( %val(IPIM), x, y, .false., 0.0, APDIA,
     +                          POISV, NOISE, straw, av, starea, sterr,
     +                          ninval, numpix, top)

            call smp_ap_doitr ( %val(IPIM), x, y, .false., SKYDIAIN,
     +                          SKYDIAOUT, POISV, NOISE, skraw, sklev,
     +                          skarea, skerr, nbadb, ngoodb, topb )
         endif

         amag = 50.0							!Calc Flux, Mag, and Error
         stmerr = 0.0
         if ( numpix.ne.0 .and. ngoodb.ne.0 ) then
            stfl = straw - sklev*starea
            if ( stfl.gt.1.0e-20 ) amag = 30.0 - 2.5*alog10(stfl)
            stferr = 0.0
            rv = (skerr+NOISE)*starea + straw/POISV
            if ( rv.gt.0.0 ) stferr = sqrt(rv)
            stmerr = 0.0
            if ( stfl.gt.0.0 ) stmerr = 2.5*alog10(1.0+abs(stferr/stfl))
         endif

         if ( TYPING ) then
            amaga = trunc(amag,2)					!Type the result
            aerr = trunc(stmerr,1)
            ax = trunc(x,6)
            ay = trunc(y,6)
            kninval = min(99999,ninval)
            atop = trunc(top,5)
            textp = '(1x,i5,2f9.2,f8.3,f6.3,f8.1,2f10.1,i5)'
            if ( stfl.gt.9999999.9 .or. sklev.gt.9999999.9 ) textp =
     +         '(1x,i5,2f9.2,f8.3,f6.3,f8.1,2e10.7,i5)'
            write ( text, textp ) k, ax, ay, amaga, aerr, atop,
     +                            stfl, sklev, kninval
            call printo ( text )
         endif

         tbout(6,k) = x							!Store the result
         tbout(7,k) = y
         tbout(8,k) = amag
         tbout(9,k) = stmerr
         tbout(10,k) = top
         tbout(11,k) = dx
         tbout(12,k) = dy
         tbout(13,k) = stfl
         tbout(14,k) = real(numpix)
         tbout(15,k) = real(ninval)
         tbout(16,k) = sklev
         tbout(17,k) = skerr
         tbout(18,k) = real(ngoodb)
         tbout(19,k) = real(nbadb)

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_AP_DOITR -- Calc flux through aperture (star/sky/annulus)
C
C  alan penny          ral          1991 Nov

      subroutine smp_ap_doitr ( im, ax, ay, dome, xradi, xrado, poisv,
     +                       noise, flux, av, area, err, nbad, ngood,
     +                       top )

      implicit none
      include 'ST_IMAGE_INC'

      real      im(NX,NY)	!i: Input image
      real      ax              !i: X position
      real      ay              !i: Y position
      logical   dome		!i: Do median averaging
      real      xradi		!i: Inner radius
      real      xrado		!i: Outer radius
      real      poisv		!i: Poisson converter
      real      noise		!i: Extra noise per p[xel
      real      flux		!o: Total flux
      real      av		!o: Average in area
      real	area		!o: Area used
      real      err		!o: Std dev of average
      integer   nbad		!o: No of bad pixels
      integer   ngood		!o: No of good pixels
      real      top		!o: Highest value
C--
      double precision tsum, tsumsq, tarea, esum, earea
      integer j, k, jj, kk, nf, kxs, kxe, kys, kye, km
      real rmax, rmin, dist1, dist2, dist3, dist4, xd, yd,
     +     dmax, dmin, zdx, zdy, zd, rv, radi, rado
      real rva(20000)
Cbegin


      radi = min(xradi,xrado)
      rado = max(xradi,xrado)

      kxs = ax - rado - 1.0
      kxe = ax + rado + 1.0
      kys = ay - rado - 1.0
      kye = ay + rado + 1.0

      if ( kxs.gt.NX .or. kxe.lt.1 .or. kys.gt.NY .or. kye.lt.1 ) then
         flux = 0.0
         av = 0.0
         area = 0.0
         err = 0.0
         nbad = 0
         ngood = 0
         top = 0.0
         return
      endif

      kxs = max(1,min(NX,kxs))
      kxe = max(1,min(NX,kxe))
      kys = max(1,min(NY,kys))
      kye = max(1,min(NY,kye))

      rmax = rado*rado
      rmin = radi*radi

      top = 0.0
      nbad = 0
      ngood = 0
      tsum = 0.0d0
      tsumsq = 0.0d0
      tarea = 0.0d0
      esum = 0.0d0
      earea = 0.0d0
      do k = kys, kye
         yd = real(k) - ay
         do j = kxs, kxe
            xd = real(j) - ax
            dist1 = xd*xd             + yd*yd
            dist2 = (xd+1.0)*(xd+1.0) + yd*yd
            dist3 = xd*xd             + (yd+1.0)*(yd+1.0)
            dist4 = (xd+1.0)*(xd+1.0) + (yd+1.0)*(yd+1.0)
            dmin = min(dist1,dist2,dist3,dist4)
            dmax = max(dist1,dist2,dist3,dist4)
            if ( dmin.le.rmax .and. dmax.ge.rmin ) then			!In annulus?
               rv = im(j,k)
               if ( rv.eq.RINVAL ) then
                  nbad = nbad + 1
               else
                  ngood = ngood + 1
                  if ( dmax.ge.rmax .or. dmin.le.rmin ) then		!Stradle edge?
                     nf = 0
                     do jj = 1, 5
                        zdy = yd + 0.1 + 0.2*(jj-1)
                        do kk = 1, 5
                           zdx = xd + 0.1 + 0.2*(kk-1)
                           zd = zdx*zdx + zdy*zdy
                           if ( zd.le.rmax .and. zd.ge.rmin ) nf=nf + 1
                        enddo
                     enddo
                     if ( nf.ne.0 ) then
                        rv = rv*real(nf)/25.0
                        earea = earea + dble(real(nf)/25.0)
                        esum = esum + dble(rv)
                     endif
                  else							!In entirely
                     if ( km.lt.20000 ) then
                        km = km + 1
                        rva(km) = rv
                     endif
                     tsum = tsum + dble(rv)
                     tsumsq = tsumsq + dble(rv)*dble(rv)
                     if ( tarea.eq.0.0d0 ) then
                        top = rv
                     else
                        top = max(top,rv)
                     endif
                     tarea = tarea + 1.0d0
                  endif
               endif
            endif
         enddo
      enddo

      area = tarea + earea						!Total area
      flux = BS*(tsum+esum) + BZ*area					!Total flux
      top  = BS*top + BZ						!Max value

      if ( dome ) then							!Calc average

         av = 0.0							! Use median
         if ( km.gt.0 ) then
            call sort1r ( rva, km )
            j = km/2
            k = (km+1)/2
            if ( j.ne.k ) then
               av = rva(k)
            else
               av = (rva(j)+rva(j+1))/2.0
            endif
         endif
         av = BS*av + BZ
         err = flux*poisv + tarea*noise*noise
         err = max(0.0,err)
         if ( err.gt.0.0 ) err = sqrt(err)/poisv

      else

         av = 0.0							! Use mean
         if ( area.ne.0.0 ) av = flux/area
         err = 0.0
         if ( tarea.gt.2.5d0 ) then
            err = (tsumsq-(tsum*tsum/tarea))/
     +              ((tarea-1.0d0)*(tarea-2.0d0))
            if ( err.gt.0.0 ) err = BS*sqrt(err)
         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_AP_DOITS -- Calc flux through aperture (star/sky/annulus)
C
C  alan penny          ral          1991 Nov

      subroutine smp_ap_doits ( im, ax, ay, dome, xradi, xrado, poisv,
     +                       noise, flux, av, area, err, nbad, ngood,
     +                       top )

      implicit none
      include 'ST_IMAGE_INC'

      integer*2 im(NX,NY)	!i: Input image
      real      ax              !i: X position
      real      ay              !i: Y position
      logical   dome		!i: Do median averaging
      real      xradi		!i: Inner radius
      real      xrado		!i: Outer radius
      real      poisv		!i: Poisson converter
      real      noise		!i: Extra noise per p[xel
      real      flux		!o: Total flux
      real      av		!o: Average in area
      real	area		!o: Area used
      real      err		!o: Std dev of average
      integer   nbad		!o: No of bad pixels
      integer   ngood		!o: No of good pixels
      real      top		!o: Highest value
C--
      double precision tsum, tsumsq, tarea, esum, earea
      integer j, k, jj, kk, nf, kxs, kxe, kys, kye, km, kv
      real rmax, rmin, dist1, dist2, dist3, dist4, xd, yd,
     +     dmax, dmin, zdx, zdy, zd, rv, radi, rado
      integer kva(20000)
Cbegin


      radi = min(xradi,xrado)
      rado = max(xradi,xrado)

      kxs = ax - rado - 1.0
      kxe = ax + rado + 1.0
      kys = ay - rado - 1.0
      kye = ay + rado + 1.0

      if ( kxs.gt.NX .or. kxe.lt.1 .or. kys.gt.NY .or. kye.lt.1 ) then
         flux = 0.0
         av = 0.0
         area = 0.0
         err = 0.0
         nbad = 0
         ngood = 0
         top = 0.0
         return
      endif

      kxs = max(1,min(NX,kxs))
      kxe = max(1,min(NX,kxe))
      kys = max(1,min(NY,kys))
      kye = max(1,min(NY,kye))

      rmax = rado*rado
      rmin = radi*radi

      top = 0.0
      nbad = 0
      ngood = 0
      tsum = 0.0d0
      tsumsq = 0.0d0
      tarea = 0.0d0
      esum = 0.0d0
      earea = 0.0d0
      do k = kys, kye
         yd = real(k) - ay
         do j = kxs, kxe
            xd = real(j) - ax
            dist1 = xd*xd             + yd*yd
            dist2 = (xd+1.0)*(xd+1.0) + yd*yd
            dist3 = xd*xd             + (yd+1.0)*(yd+1.0)
            dist4 = (xd+1.0)*(xd+1.0) + (yd+1.0)*(yd+1.0)
            dmin = min(dist1,dist2,dist3,dist4)
            dmax = max(dist1,dist2,dist3,dist4)
            if ( dmin.le.rmax .and. dmax.ge.rmin ) then			!In annulus?
               kv = im(j,k)
               if ( kv.eq.INVAL ) then
                  nbad = nbad + 1
               else
                  ngood = ngood + 1
                  if ( dmax.ge.rmax .or. dmin.le.rmin ) then		!Stradle edge?
                     nf = 0
                     do jj = 1, 5
                        zdy = yd + 0.1 + 0.2*(jj-1)
                        do kk = 1, 5
                           zdx = xd + 0.1 + 0.2*(kk-1)
                           zd = zdx*zdx + zdy*zdy
                           if ( zd.le.rmax .and. zd.ge.rmin ) nf=nf + 1
                        enddo
                     enddo
                     if ( nf.ne.0 ) then
                        rv = real(kv)*real(nf)/25.0
                        earea = earea + dble(real(nf)/25.0)
                        esum = esum + dble(rv)
                     endif
                  else							!In entirely
                     if ( km.lt.20000 ) then
                        km = km + 1
                        kva(km) = kv
                     endif
                     rv = real(kv)
                     tsum = tsum + dble(rv)
                     tsumsq = tsumsq + dble(rv)*dble(rv)
                     if ( tarea.eq.0.0d0 ) then
                        top = rv
                     else
                        top = max(top,rv)
                     endif
                     tarea = tarea + 1.0d0
                  endif
               endif
            endif
         enddo
      enddo

      area = tarea + earea						!Total area
      flux = BS*(tsum+esum) + BZ*area					!Total flux
      top  = BS*top + BZ						!Max value

      if ( dome ) then							!Calc average

         av = 0.0							! Use median
         if ( km.gt.0 ) then
            call sort1i ( kva, km )
            j = km/2
            k = (km+1)/2
            if ( j.ne.k ) then
               av = kva(k)
            else
               av = real(kva(j)+kva(j+1))/2.0
            endif
         endif
         av = BS*av + BZ
         err = flux*poisv + tarea*noise*noise
         err = max(0.0,err)
         if ( err.gt.0.0 ) err = sqrt(err)/poisv

      else

         av = 0.0							! Use mean
         if ( area.ne.0.0 ) av = flux/area
         err = 0.0
         if ( tarea.gt.2.5d0 ) then
            err = (tsumsq-(tsum*tsum/tarea))/
     +              ((tarea-1.0d0)*(tarea-2.0d0))
            if ( err.gt.0.0 ) err = BS*sqrt(err)
         endif

      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUFIT.F
C
C Contains:-
C
C SMP_GAUFIT     Perform the GAUFIT programme
C SMP_GF_GCL     Get user input
C SMP_GF_FIND    Fit stars
C SMP_GF_STARRJ  Calc mean radius
C SMP_GF_RMEAN   Calc mean radii
C SMP_GF_FIT     Sum the data round the stars into a mean and fit it
C SMP_GF_RESID   Calc residuals from the summed data; store
C SMP_GF_RADIAL  Make table of radial plot of data and fit
C SMP_GF_ADDINT  Add data from short area, interpolated, to real area


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GAUFIT -- Perform the GAUFIT programme
C
C  alan penny                  ral                  1991 Nov

      subroutine smp_gaufit ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'
C--
      integer istat, ipres, iptb, nbx, nby
      real rxm, rym
Cbegin


      call smp_gf_gcl ( iptb, nbx, nby, istat )				!Get user input
      if ( istat.ne.0 ) ST_FAILED = .true.

      call gtwrkr ( 'WORK', 9*TBY, ipres, istat )			!Open results space
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call azeror ( %val(ipres), 9*TBY )

      call smp_gf_find ( %val(iptb), %val(ipres), nbx, nby )		!Find good stars

      call smp_gf_starrj ( %val(ipres) )				!Find which stars are wanted for fit

      call smp_gf_rmean ( %val(ipres), rxm, rym )			!Calc mean radii

      call smp_gf_fit ( %val(ipres), rxm, rym, 0 )			!Make mean data fit

      call smp_gf_fit ( %val(ipres), rxm, rym, 1 )			!Make mean interpolated data fit


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_GCL -- Get user input
C
C  alan penny                  ral                  1991 Nov

      subroutine smp_gf_gcl ( iptb, nbx, nby )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      integer    iptb		!o: Pointer to input star table
      integer    nbx		!o: X Size of box round stars
      integer    nby		!o: Y Size of box round stars
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      istat = 0
      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .true., istat )		!Get image
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL,IMTITLE,istat) 	!Get image details
      if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL

      call optabr ( 'INSTARS', IPTB, tbvx, TBY, .false., istat )	!Get star list
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      nbx = 20
      nby = 20
      call get2i ( 'BOX', nbx, nby, .true., 2, 200 )			!Get size of boxes round stars
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_FIND -- Fit stars
C
C  alan penny                  ral                  1991 Nov

      subroutine smp_gf_find ( tb, res, nbx, nby )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      real         tb(TBVX,TBY)		!i: Star list
      real         res(9,TBY)		!o: Results
      integer      nbx			!i: X Size of box round stars
      integer      nby			!i: Y Size of box round stars
C--
      real rnbx, rnby, xa, ya, amag, ht, base, dxo, dyo, anx, any, rx,
     +     ry, adxo, adyo, arx, ary, arms, ah, rms
      integer k, nin, ninval, iter
      character text*72, txt*3
      logical typing
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      rnbx = real(nbx)/6.0
      rnby = real(nby)/6.0

      call get1b ( 'TYPING', typing, .true. )				!Type out all fits?
      if ( ST_FAILED ) return

      if ( typing ) then
         call printo ( ' ' )
         write ( text, '(1x,43x,''Gauss'')' )
         call printo ( text )
         write ( text, '(1x,''Star  Mag  Height  Dx   Dy '',
     +                   ''  Rms Its In   Rx    Ry '')' )
         call printo ( text )
      endif

      do k = 1, TBY							!Do for all stars

         xa = tb(6,k)
         ya = tb(7,k)

         if ( IMTYPE.eq.'SHORT' ) then					!Fit
            call gauss2sa ( %val(IPIM), NX, NY, xa, ya, nbx, nby, 0,
     +                      rnbx, rnby, INVAL, 20, amag, ht, base, dxo,
     +                      dyo, anx, any, rx, ry, rms, iter, ninval )
         else
            call gauss2ra ( %val(IPIM), NX, NY, xa, ya, nbx, nby, 0,
     +                      rnbx, rnby, RINVAL, 20, amag, ht, base, dxo,
     +                      dyo, anx, any, rx, ry, rms, iter, ninval )
         endif

         amag = amag - 2.5*alog10(BS)
         ht = ht*BS
         base = base*BS + BZ
         rms = rms*BS

         res(1,k) = anx							!Store
         res(2,k) = any
         res(3,k) = rx
         res(4,k) = ry
         res(5,k) = iter
         res(6,k) = ninval
         res(8,k) = amag

         if ( typing ) then
            ah = trunc(ht,4)
            adxo = trunc(dxo,2)
            adyo = trunc(dyo,2)
            arms = trunc(rms,3)
            arx = trunc(rx,2)
            ary = trunc(ry,2)
            nin = min(ninval,99)
            txt = '   '
            if ( ninval.gt.0 .or. iter.gt.19 .or.amag.gt.49.0 )
     +         txt = 'rej'
            write ( text,
     +         '(1x,i4,f6.2,f7.1,2f5.1,f6.1,i3,i3,2f6.2,2x,a3)' ) k,
     +         amag, ah, adxo, adyo, arms, iter, nin, arx, ary, txt
            call printo ( text )
          endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_STARRJ -- Calc mean radius
C    Take a list of star mags,radii,iterations, and invalid points
C    and decide which are of acceptable quality.
C
C  alan penny            RAL                  1991 Nov

      subroutine smp_gf_starrj ( res )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real      res(9,TBY)		!i/o: Residuals
C--
      real radj(2), rad, rnum
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY							!Reject on Magnitude,Iterations,Invalid
         if ( res(8,k).gt.49.0 ) res(9,k) = 1.0
         if ( res(5,k).gt.19.1 ) res(9,k) = 1.0
         if ( res(6,k).gt.0.1 ) res(9,k) = 1.0
      enddo

      rad = 0.0								!Reject on Radii
      rnum = 0.0
      do k = 1, TBY
         if ( res(9,k).lt.0.5 ) then
            rad = rad + res(3,k) + res(4,k)
            rnum = rnum + 2.0
         endif
      enddo
      if ( rnum.gt.0.1 ) then
         rad = rad/rnum
         radj(1) = 0.5*rad
         radj(2) = 1.5*rad
      else
         radj(1) = 1.0
         radj(2) = 10.0
      endif

      call get2r ( 'RADLIMS', radj(1), radj(2), .true., 0.1, 100.0 )
      if ( ST_FAILED ) return

      do k = 1, TBY
         if ( res(3,k).lt.radj(1) .or. res(3,k).gt.radj(2) )res(9,k)=1.0
         if ( res(4,k).lt.radj(1) .or. res(4,k).gt.radj(2) )res(9,k)=1.0
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_RMEAN -- Calc mean radii
C
C  alan penny                  ral                  1991 Nov

      subroutine smp_gf_rmean ( res, rxm, rym )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real      res(9,TBY)	!i: Results
      real      rxm		!o: Mean X radius
      real      rym		!o: Mean Y radius
C--
      real seeing
      integer nrad, k
      character text*72
Cbegin


      if ( ST_FAILED ) return

      rxm = 0.0
      rym = 0.0
      nrad = 0
      do k = 1, TBY
         if ( res(9,k).lt.0.5 ) then
            rxm = rxm + res(3,k)
            rym = rym + res(4,k)
            nrad = nrad + 1
         endif
      enddo
      if ( nrad.eq.0 ) nrad = 1
      rxm = rxm/real(nrad)
      rym = rym/real(nrad)
      if (abs(rxm).gt.99.0) rxm = sign(99.0,rxm)
      if (abs(rym).gt.99.0) rym = sign(99.0,rym)

      call printo ( ' ' )
      write ( text, '(1x,'' Mean radii are:- '',f6.2,'' : '',f6.2)' )
     +        rxm, rym
      call printo ( text )
      seeing = 1.65*((rxm+rym)/2.0)
      write ( text, '(1x,'' Seeing (FWHM) = '',f6.2)' ) seeing
      call printo ( text )

      call put1r ( 'RX1', rxm )
      call put1r ( 'RY1', rym )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_FIT -- Sum the data round the stars into a mean and fit it
C   Sums the data from a number of stars in an image and fits a mean
C   Gaussian to them.
C   Method of summing is either taking pixels and adding to nearest
C   pixel, or interpolating to exact pixel posn.
C   Output is typed to terminal and put in the Starlink parameters.
C
C   alan penny                 RAL                       1991 Nov

      subroutine smp_gf_fit ( res, rxm, rym, kt )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      real         res(9,TBY)		!i/o: Results
      real         rxm			!i: Mean X radius
      real         rym			!i: Mean Y radius
      integer      kt			!i: Type of summing of data (0=simple;1=interpolated)
C--
      character text*72
      logical doit
      integer kx, ky, ipwa, ipwb, ipwc, istat, k, kxs, kxe, kys, kye,
     +        iter, ipws, ipwd
      real xd, yd, amag, ht, base, xa, ya, rms, rxa, rya, seeing, a, b,
     +     ah, arms, ab, arx, ary
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      if ( kt.eq.0 ) then
         call get1b ( 'DOSUM', doit, .true. )				!Do simple fit?
      else
         call get1b ( 'DOINT', doit, .true. )				!Do interpolated fit?
      endif
      if ( ST_FAILED ) return
      if ( .not.doit ) return

      kx = min(200,max(2,nint(6.0*rxm)))				!Get the star analysis box size
      ky = min(200,max(2,nint(6.0*rym)))
      call get2i ( 'SUMBOX', kx, ky, .true., 2, 200 )
      if ( ST_FAILED ) return

      call gtwrkr ( 'WORKA', kx*ky, ipwa, istat )			!Open work space
      if ( istat.ne.0 ) ST_FAILED = .true.
      call gtwrkr ( 'WORKB', kx*ky, ipwb, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      call gtwrkr ( 'WORKC', kx*ky, ipwc, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      call gtwrkr ( 'WORKD', kx*ky, ipwd, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( IMTYPE.eq.'SHORT' ) then
         call gtwrks ( 'WORKS', kx*ky, ipws, istat )
      else
         call gtwrkr ( 'WORKS', kx*ky, ipws, istat )
      endif
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call azeror ( %val(ipwa), kx*ky )					!Load data into summed area
      do k = 1, TBY
         if ( res(9,k).lt.0.5 ) then
            kxs = res(1,k) - real((kx/2))
            kxe = kxs + kx - 1
            kys = res(2,k) - real((ky/2))
            kye = kys + ky - 1
            if ( kt.eq.0 ) then
               if ( IMTYPE.eq.'SHORT' ) then
                  call copss ( %val(IPIM), NX, NY, kxs, kxe, kys, kye,
     +                         %val(ipws), kx, ky, 1, 1 )
                  call copssr ( %val(ipws), kx, ky, BS, BZ, INVAL,
     +                          %val(ipwb) )
               else
                  call coprr ( %val(IPIM), NX, NY, kxs, kxe, kys, kye,
     +                         %val(ipws), kx, ky, 1, 1 )
                  call coprrr ( %val(ipws), kx, ky, BS, BZ, RINVAL,
     +                          %val(ipwb) )
               endif
               call aaddr ( %val(ipwb), %val(ipwa), %val(ipwa), kx*ky )
            else
               xd = res(1,k) - int(res(1,k))
               yd = res(2,k) - int(res(2,k))
               call smp_gf_addint ( k, %val(ipwa), kx, ky, kxs, kys,
     +                              xd, yd, %val(ipwb) )
            endif
         endif
      enddo

      call gauss2r ( %val(ipwa), kx, ky, 0, rxm, rym, 20, amag, ht,	!Fit
     +               base, xa, ya, rxa, rya, rms, iter )

      ah = trunc(ht,7)							!Type out fit
      ab = trunc(base,5)
      arms = trunc(rms,5)
      arx = trunc(rxa,2)
      ary = trunc(rya,2)
      call printo ( ' ' )
      write ( text, '(1x, ''Fit to averaged data is:- '',f7.3,
     +              '' : '',f7.3)' ) arx, ary
      call printo ( text )
      seeing = 1.65*(rxa+rya)/2.0
      write ( text, '(1x, ''Seeing for averaged data is:- '',f7.3)' )
     +               seeing
      call printo ( text )

      if ( kt.eq.0 ) then						!Put radii to program parameters
         call put1r ( 'RX2', rxa )
         call put1r ( 'RY2', rya )
      else
         call put1r ( 'RX3', rxa )
         call put1r ( 'RY3', rya )
      endif

      call smp_gf_resid ( %val(ipwa), kx, ky, %val(ipwd), %val(ipwb),	!Output fit and residuals
     +                    %val(ipwc),  xa, ya, a, b, rxa, rya, kt )

      call smp_gf_radial ( %val(ipwd), kx, ky, xa, ya, ht, rxa, rya,kt)

      call wrkcan ( 'WORKA' )						!Clear work space
      call wrkcan ( 'WORKB' )
      call wrkcan ( 'WORKC' )
      call wrkcan ( 'WORKD' )
      call wrkcan ( 'WORKS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_RESID -- Calc residuals from the summed data; store
C
C  alan penny               RAL               1991 Nov

      subroutine smp_gf_resid ( data, kx, ky, resid, wka, wkb, xa,
     +                          ya, a, b, rx, ry, kt )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      integer    kx			!i: X size of box
      integer    ky			!i: Y size of box
      real       data(kx,ky)		!i: Mean data
      real       resid(kx,ky)		!o: Residuals
      real       wka(kx,ky)		!o: Work space
      real       wkb(kx,ky)		!o: Work space
      real       xa			!i: X posn of star
      real       ya			!i: Y posn of star
      real       a			!i: Height of star
      real       b			!i: Base
      real       rx			!i: X Rdaius of star
      real       ry			!i: Y Radius of star
      integer    kt			!i: Type of summing of data (0=simple;1=interpolated)
C--
      integer j, k, ipo, istat, iv
      real  dy, dd, rmin, rmax, rv
      character outim*6
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ky							!Calc residuals
         dy = (abs(real(k)-ya)/ry)**2.0
         do j = 1, kx
            dd = dy + (abs(real(j)-xa)/rx)**2.0
            dd = min(dd,50.0)
            resid(j,k) = data(j,k) - (b + a*exp(-1.0*dd))
         enddo
      enddo

      outim = 'OUT1IM'							!Open output image?
      if ( kt.eq.1 ) outim = 'OUT2IM'
      call opimsw ( outim, ipo, kx, ky, .false., istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call alimr ( resid, kx*ky, rmin, rmax )				!Scale residuals for 'short' output
      call asubkr ( resid, rmin, wka, kx*ky )
      if ( abs(rmax-rmin).gt.1.0e-6 ) then
         rv = 40000.0/(rmax-rmin)
         call amulkr ( wka, rv, wkb, kx*ky )
      else
         call amovr ( wkb, wka, kx*ky )
      endif
      call asubkr ( wka, 20000.0, wkb, kx*ky )
      call achtrs ( wkb, %val(ipo), kx*ky)

      call ptdesr ( outim, 'BSCALE', rv )				!Put scale to output image
      call ptdesr ( outim, 'BZERO', 1.5*rv )
      iv = INT_INVALSI
      call ptdesi ( outim, 'INVAL', iv )
      call ptdesr ( outim, 'RX', rx )
      call ptdesr ( outim, 'RY', ry )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_RADIAL -- Make table of radial plot of data and fit
C
C alan penny           RAL                 1991 Nov

      subroutine smp_gf_radial ( resid, kx, ky, xa, ya, ht, rx, ry,
     +                           kt )

      implicit none
      include 'STARMAN_INC'

      integer     kx			!i: X size of aarry
      integer     ky			!i: Y size of aarry
      real        resid(kx,ky)		!i: Residuals array
      real        xa			!i: X posn of star
      real        ya			!i: Y posn of star
      real        ht			!i: Height of star
      real        rx			!i: X radius of star
      real        ry			!i: Y radius of star
      integer     kt			!i: Type of summing of data (0=simple;1=interpolated)
C--
      real xmax, xmaxa, ymax, ymaxa, drmax, anum, anumpt, dx, dy,
     +     dr, pd
      integer  j, k, kd, next, kfound, kgap, iptbo, istat, ntot
      real dist(200), pdata(200), pfit(200)
      integer num(200)
      character table*7
Cbegin


      if ( ST_FAILED ) return

      xmax = (xa-1.0)/rx						!Calc no of radial bins
      xmaxa = (real(kx) - xa)/rx
      ymax = (ya - 1.0)/ry
      ymaxa = (real(ky) - ya)/ry
      if (xmaxa.gt.xmax) xmax = xmaxa
      if (ymaxa.gt.ymax) ymax = ymaxa
      drmax = sqrt(xmax*xmax+ymax*ymax)
      anumpt = 15.0*drmax
      if (anumpt.gt.200.0) anumpt = 200.0

      call azeroi ( num, 200 )						!Zero radial profile values
      call azeror ( dist, 200 )
      call azeror ( pdata, 200 )
      call azeror ( pfit, 200 )

      do k = 1, ky							!Calc radial profile of fit and residuals
         do j = 1, kx
            dx = real(j) - xa
            dy = real(k) - ya
            dr = sqrt((dx/rx)**2.0+(dy/ry)**2.0)
            if (dr.le.drmax) then
               kd = 1 + int((anumpt-1.0)*(dr/drmax))
               dist(kd) = dist(kd) + dr*((rx+ry)/2.0)
               num(kd) = num(kd) + 1
               pd = ht*exp(-1.0*dr**2.0)
               pfit(kd) = pfit(kd) + pd
               pdata(kd) = pdata(kd) + pd + resid(j,k)
            endif
         enddo
      enddo

      do k = 1, 200-1							!Bunch up if any points have no data
         if ( num(k).eq.0 ) then
            next = k
            kfound = 0
            do j = k+1,200
               if ( kfound.eq.0 .and. num(j).ne.0 ) then
                  kfound = 1
                  next = j
               endif
            enddo
            kgap = next - k
            num(k) = num(k+kgap)
            dist(k) = dist(k+kgap)
            pfit(k) = pfit(k+kgap)
            pdata(k) = pdata(k+kgap)
            num(k+kgap) = 0
         endif
      enddo
      ntot = 0
      do k = 1, 200
         if ( num(k).ne.0 ) ntot = ntot + 1
      enddo

      do k = 1, ntot							!Divide by number of points added to get mean
         anum = real(num(k))
         pdata(k) = pdata(k)/anum
         dist(k) = dist(k)/anum
         pfit(k) = pfit(k)/anum
      enddo

      table = 'OUT1TB'							!Store the data
      if ( kt.eq.1 ) table = 'OUT2TB'
      call optabw ( table, iptbo, 8, ntot, .true., istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call coptrr ( dist, ntot, %val(iptbo), 8, ntot, 6, 1 )
      call coptrr ( pdata, ntot, %val(iptbo), 8, ntot, 7, 1 )
      call coptrr ( pfit, ntot, %val(iptbo), 8, ntot, 8, 1 )

      call pthead ( table, 1, 'DISTANCE', istat )
      call pthead ( table, 2, 'DATA', istat )
      call pthead ( table, 3, 'FIT', istat )
      call ptdesr ( table, 'RX', rx )
      call ptdesr ( table, 'RY', ry )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GF_ADDINT -- Add data from short area, interpolated, to real area
C   This subroutine adds part of the I*2 array KWHOLE
C   into the Real array PART. With an interpolated shift of XD,YD.
C   Interpolation is done by cubic running fit over 4 points in X
C   and then the same in Y. The 2 points nearest any edge are taken
C   straight over, not interpolated.
C
C   alan penny               RAL                     1991 Nov

      subroutine smp_gf_addint ( knum, part, kx, ky, kxs, kys, xd, yd,
     +                           yy )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      integer      knum			!i: Number of star being used
      integer      kx			!i: X Box size round stars
      integer      ky			!i: Y Box size round stars
      real         part(kx,ky)		!i/o: Array to add box into
      integer      kxs			!i: X posn of blh of box in image
      integer      kys			!i: Y posn of blh of box in image
      real         xd			!i: Part pixel shift in X
      real         yd			!i: Part pixel shift in Y
      real         yy(kx,ky)		!o: Work array
C--
      double precision sd(4), x(4), y(4), xa, ya, xaa, yaa, chi, rms,
     +                 a(10)
      integer k, j, kxe, kye, nin
      real    ymax, ymin, yz, yza
Cbegin


      if ( ST_FAILED ) return

      xa = 2.0 + xd							!Set up interpolation factors
      xaa = xa*xa
      ya = 2.0 + yd
      yaa = ya*ya
      do k = 1,4
         x(k) = k
         sd(k) = 1.0
      enddo

      kxe = kxs + kx - 1
      kye = kys + ky - 1
      if ( IMTYPE.eq.'SHORT' ) then
         call copvsr ( %val(IPIM), NX, NY, yy, kx, ky, kxs, kxe, kys, 	!Copy section of
     +                 kye, BS, BZ, INVAL, RINVAL, nin, 0 )		! input array into work area,
      else
         call copvrr ( %val(IPIM), NX, NY, yy, kx, ky, kxs, kxe, kys, 	!Copy section of
     +                 kye, BS, BZ, RINVAL, nin, 0 )			! input array into work area,
      endif

      if ( nin.ne.0 ) then
         call pargi ( knum )
         call pargi ( nin )
         call printd (
     +        'WARNING: Star No %d has %d INVALID pixels - Not used' )
         return
      endif

      call alimr ( yy, kx*ky, ymin, ymax )
      do k = 1, ky							!Scale work array so max value = 1 (needed so POLFIT works)
         do j = 1, kx
            yy(j,k) = yy(j,k)/ymax
         enddo
      enddo

      do k = 2, ky-2							!Interpolate inside work array in X direction
         yza = yy(1,k)
         do j = 2, kx-2
            y(1) = yy(j-1,k)
            y(2) = yy(j,k)
            y(3) = yy(j+1,k)
            y(4) = yy(j+2,k)
            call polfit ( x, y, sd, 4, 3, 0, a, chi, rms )
            yz = a(1) + a(2)*xa + a(3)*xaa
            yy(j-1,k) = yza
            yza = yz
         enddo
      enddo

      do j = 2, kx-2							!Interpolate inside work array in Y direction
         yza = yy(j,1)
         do k = 2, ky-2
            y(1) = yy(j,k-1)
            y(2) = yy(j,k)
            y(3) = yy(j,k+1)
            y(4) = yy(j,k+2)
            call polfit ( x, y, sd, 4, 3, 0, a, chi, rms )
            yz = a(1) + a(2)*ya + a(3)*yaa
            yy(j,k-1) = yza
            yza = yz
         enddo
      enddo

      do k = 1, ky							!Add work array (scaled back from=1) to output array
         do j = 1, kx
            part(j,k) = part(j,k) + yy(j,k)*ymax
         enddo
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUMAG.F
C
C Contains:-
C
C SMP_GAUMAG


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GAUMAG -- Perform gaussian profile fitting photometry
C
C   alan penny                     RAL                  1991 Nov

      subroutine smp_gaumag ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

C--
      integer iptbi, iptbo
Cbegin


      call smp_gm_gcl ( iptbi, iptbo )					!Open input/output; get controls
      if ( ST_FAILED ) return

      call smp_gm_doit ( %val(iptbi), %val(iptbo) )			!Do the work


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GM_GCL -- Get user input; open input and output
C
C   alan penny                     RAL                  1991 Nov

      subroutine smp_gm_gcl ( iptbi, iptbo )

      implicit none
      include 'gaumag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      integer iptbi	!o: Pointer to input table
      integer iptbo	!o: Pointer to output table
C--
      integer istat, iv, k
      character*50 title
      character*20 head(12)
      data head / 'X', 'Y', 'Magnitude', 'Dx', 'Dy', 'Iterations',
     +       'Rms', 'Numinval', 'Height', 'Base', 'Rx', 'Ry' /
Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL,IMTITLE,istat)	!
      if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL

      call optabr ( 'INSTARS', iptbi, TBVX, TBY, .false., istat )	!
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get1b ( 'PARFIX', PARFIX, .false. )				!

      RXM = 3.0								!
      RYM = 3.0
      call get2r ( 'RADII', RXM, RYM, .true., 0.1, 200.0 )

      NBX = 6.0*RXM							!
      NBY = 6.0*RYM
      call get2i ( 'BOX', NBX, NBY, .true., 3, 100 )

      call optabw ( 'OUT', iptbo, 17, TBY, .false., istat )		!
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call gtdesc ( 'INSTARS', 'TITLE', title, 'Output from Gaumag',	!
     +                 iv, istat )
      if ( istat.ne.0 ) title = 'Output from Gaumag'
      call get1c ( 'TITLE', title, title, .true. )
      call ptdesc ( 'OUT', 'TITLE', title )

      do k = 1, 12							!
         call pthead ( 'OUT', k, head(k), istat )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMP_GM_DOIT -- Fit the data
C
C   alan penny                     RAL                  1991 Nov

      subroutine smp_gm_doit ( tbin, tbout )

      implicit none
      include 'gaumag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_TABLE_INC'

      real          tbin(TBVX,TBY)	!i: Star list
      real          tbout(17,TBY)	!o: Output results
C--
      character*77 text
      integer k, ninval, iter, kw, kinval
      real x, y, xa, ya, rx, ry, rms, axa, aya, arx, ary, arms,
     +     dx, dy, ah, ab, ht, base, amag
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      write ( text, '(''  Star  Mag Inv Iter  Rms  Dx   Dy  Height'',
     +                '' Base   Xpos   Ypos   Radx   Rady'')' )
      call printo ( text )

      kw = 0
      if ( PARFIX ) kw = 1
      do k = 1, TBY

         x = tbin(6,k)
         y = tbin(7,k)

         if ( IMTYPE.eq.'SHORT' ) then
            call gauss2sa ( %val(IPIM), NX, NY, x, y, NBX, NBY, kw,
     +                      RXM, RYM, INVAL, 20, amag, ht, base, dx,
     +                      dy, xa, ya, rx, ry, rms, iter, ninval )
         else
            call gauss2ra ( %val(IPIM), NX, NY, x, y, NBX, NBY, kw,
     +                      RXM, RYM, RINVAL, 20, amag, ht, base, dx,
     +                      dy, xa, ya, rx, ry, rms, iter, ninval )
         endif

         amag = amag - 2.5*alog10(BS)
         ht = ht*BS
         base = base*BS + BZ
         rms = rms*BS

         arms = trunc(rms,4)						!Type results
         ah = trunc(ht,5)
         ab = trunc(base,5)
         axa = trunc(xa,4)
         aya = trunc(ya,4)
         arx = trunc(rx,3)
         ary = trunc(ry,4)
         kinval = min(ninval,999)
         write ( text, '(1x, i4,1x,f5.2,1x,i3,2x,i2,1x,f5.0,1x,
     +                     2(f4.0,1x),2f6.0,2(f6.1,1x),2(f6.2,1x))' )
     +         k, amag, kinval, iter, arms, dx, dy, ah, ab, axa,
     +         aya, arx, ary
         call printo ( text )

         tbout(6,k) = xa						!Store results
         tbout(7,k) = ya
         tbout(8,k) = amag
         tbout(9,k) = dx
         tbout(10,k) = dy
         tbout(11,k) = real(iter)
         tbout(12,k) = rms
         tbout(13,k) = real(ninval)
         tbout(14,k) = ht
         tbout(15,k) = base
         tbout(16,k) = rx
         tbout(17,k) = ry

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SIMPLEMAG -- (Program) Perform simple aperture or profile fitting photometry
C  See SIMPLEMAG.HLP for description
C
C  a j penny            ral                   1990 dec

      subroutine simplemag ( ierradam )

      implicit none

      integer      ierradam             !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_simplemag

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SIMPLEMAG.FOR
C
C Contains:-
C   T_SIMPLEMAG   Perform: aperture photometry; Gauss radii fit: Profile photometry


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_SIMPLEMAG -- Perform: aperture photometry; Gauss radii fit: Profile photometry
C
C  alan penny                     RAL                      1991 Nov

      subroutine t_simplemag ()

      implicit none
      include 'simplemag.inc'
C--
      integer k

      character*1000 topt
      data topt / 'aperture:gaussian:radius' /
      integer nthelp
      parameter ( nthelp=5 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,nthelp) /
     + 'Option     Function',
     + '------     --------',
     + 'Aperture   Aperture photometry',
     + 'Gaussian   Gaussian profile fitting photometry' ,
     + 'Radius     Determine mean Gaussian radii' /
Cbegin


      call get_job ( 'OPTION', topt, k, 1, thelp, nthelp )		!Choose program

      if ( k.eq.1 ) call smp_apermag

      if ( k.eq.2 ) call smp_gaufit

      if ( k.eq.3 ) call smp_gaumag


      end

