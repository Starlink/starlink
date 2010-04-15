CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is STARFLIBS.FOR
C
C    It contains star and sky s/rs:-
C
C BOXELI           Calculates size of box needed round a rotated star
C CVOLUME          Performs an integration under a 2-D curve defined by
C GETD             Get x,y, and height out of a table, using the header code
C GET_MPROF        Get the star profile from the profile file on disk
C HTVAL            Calc height of lorentz profile given coded input
C POP(AS)(FM)(ISR) Add/Sub star with pixel (flags/magic) to/fm (i:s:r) array
C PROSET           Set up values for s/r POP(AS)(FM)(SR)
C PROFVAL          Calc profile value at a pixel.
C SKY_0            Find background and noise in an real or integer*2 image
C SKY_1(RS)        Find background and noise in boxes in a (real/int*2) image
C SKY_2(RS)        Find the sky, given a (real/int*2) sky map table, and a position
C SUBDIV           Calc pixel sub-division needed for given profile


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BOXELI -- Calc size of box needed round a rotated star
C  The star is elliptical, rotated
C
C a j penny                    dao                  1988-04-19

      subroutine boxeli ( rx, ry, theta, dx, dy )

      implicit none
      include 'STARMAN_INC'

      real rx		!i: Star major axis radius
      real ry 		!i: Star minor axis radius
      real theta	!i: Angle of major axis to X
      real dx		!o: X size of box
      real dy		!o: Y size of box
C--
      real gx2, gy2, t, s, c, f
Cbegin


      if ( ST_FAILED ) return

      if ( abs(rx).gt.0.0001 .and. abs(ry).gt.0.00001 ) then
         gx2 = 1.0/(rx*rx)
         gy2 = 1.0/(ry*ry)
         t = (tan(theta))**2.0
         s = (sin(theta))**2.0
         c = (cos(theta))**2.0
         f = (gy2-gx2)/(gx2+t*gy2)
         dy = sqrt(s*gx2*((1.0+f)**2.0)+c*gy2*((1.0-t*f)**2.0))
         dy = 1.0/dy
         f = (gy2-gx2)/(gy2+t*gx2)
         dx = sqrt(c*gx2*((1+t*f)**2.0)+s*gy2*((-1.0+f)**2.0))
         dx = 1.0/dx
      else
         dx = 1.0
         dy = 1.0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CVOLUME -- Calc volume under a star profile.
C  Performs an integration under a 2-D curve defined by some of
C  the parameters in the array PROF and a residuals map.
C
C      a j penny                     stsci                1987-02-21


      subroutine cvolume ( prof, domap, map, mx, my, mz,
     +                     mapnum, mapx, mapy, magnif, radius, vol )

      implicit none
      include 'STARMAN_INC'

      real prof(9)	 !i: parameters of the function to be fitted.
      logical domap	 !i: Flag whether to use map
      integer mx	 !i: Maximum Map x size in Map array
      integer my	 !i: Maximum Map y size in Map array
      integer mz	 !i: Total no of maps in Map array
      real map(mx,my,mz) !i: Array containing all maps
      integer mapnum	 !i: Number of map to actually use
      integer mapx	 !i: Actual Map x size
      integer mapy	 !i: Actual Map x size
      integer magnif	 !i: Map pixel subdivision
      real radius	 !i: Radius out to which to calculate volume
      real vol		 !o: Volume under profile, out to specified radius
C--
      real avol, fxa, fya, fxb, fyb, pa, rslim, xxa, xxb,
     +     yya, yyb, gg, pab, pac, dd, rr, dx, dy, x, y, val
      integer kradlim, j, ja, k
Cbegin


      if ( ST_FAILED ) return

C  Start with centre pixel

      vol = 1.0

C  Set up parameters

      fxa = 1.0/30.0
      fxa = fxa*fxa
      fya = 1.0/30.0
      fya = fya*fya
      fxb = prof(1)/(30.0*prof(4))
      fxb = fxb*fxb
      fyb = prof(2)/(30.0*prof(5))
      fyb = fyb*fyb
      pa = 0.5*prof(3)

C  Translate radius edge

      kradlim = radius*30.0
      rslim = radius*radius*30.0*30.0

C  Do one quadrant, steps of 1/30 out to 3.33 star radii. Then *4

      avol = 0.0
      do k = 1, min(kradlim,100)
         y = k
         do j = 1, min(kradlim,100)
            x = j
            call htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val )
            avol = avol + val
         enddo
      enddo
      vol = vol + 4.0*avol

      if ( kradlim.ge.101 ) then

C Now do the quadrant in steps of 1/15, top half of 3.3 to 10 star radii

         avol = 0.0
         do k = 101, min(kradlim,299), 2
            y = real(k) + 0.5
            do j = 1, min(kradlim,299), 2
               x = real(j) + 0.5
               call htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val )
               avol = avol + val
            enddo
         enddo
         vol = vol + 4.0*avol*4.0

C Now do the quadrant in steps of 1/15, remainder of 3.3 - 10 star radii

         avol = 0.0
         do k = 1, 99, 2
            y = real(k) + 0.5
               do j = 101, min(kradlim,299), 2
                  x = real(j) + 0.5
                  call htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val)
                  avol = avol + val
            enddo
         enddo
         vol = vol + 4.0*avol*4.0

      endif

      if ( kradlim.ge.305 ) then

C  Now do from 10 to 20 star radii, in steps of 10, right half

         avol = 0.0
         do k = 305, min(kradlim,595), 10
            y = real(k) + 0.5
            do j = 5, min(kradlim,595), 10
               x = real(j) + 0.5
               call htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val )
               avol = avol + val
            enddo
         enddo
         vol = vol + 4.0*avol*100.0

C  Now do from 10 to 20 star radii, in steps of 10, top left part

         avol = 0.0
         do k = 5, 295, 10
            y = real(k) + 0.5
            do j = 305, min(kradlim,595), 10
               x = real(j) + 0.5
               call htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val )
               avol = avol + val
            enddo
         enddo
         vol = vol + 4.0*avol*100.0

      endif

C  Do one X arm

      avol = 0.0
      do j = 1, min(kradlim,600)
         xxa = real(j*j)/900.0
         xxb = xxa*prof(1)*prof(1)/(prof(4)*prof(4))
         gg = xxa
         pab = pa*(1.0+sqrt(xxb))
         pac = pab*alog10(gg)
         if (pac.lt.25.0) then
            avol = avol + 1.0/(1.0+10.0**pac)
         endif
      enddo
      vol = vol + 2.0*avol

C  Do one Y arm

      avol = 0.0
      do k = 1, min(kradlim,600)
         yya = real(k*k)/900.0
         yyb = yya*prof(2)*prof(2)/(prof(5)*prof(5))
         gg = yya
         pab = pa*(1.0+sqrt(yyb))
         pac = pab*alog10(gg)
         if (pac.lt.25.0) then
            avol = avol + 1.0/(1.0+10.0**pac)
         endif
      enddo
      vol = vol + 2.0*avol

C  Correct for area of pixels

      vol = prof(1)*prof(2)*vol/900.0

C  Add the Gaussian component

      avol = 0.0
      ja = min(3.0*radius,60.0)
      do k = -1*ja, ja
         y = prof(2)*real(k)/3.0
         do j = -1*ja, ja
            dd = j*j + k*k
            if ( dd.lt.(3.0*3.0*radius*radius) ) then
               x = prof(1)*real(j)/3.0
               dd = sqrt(x*x+y*y)/prof(8)
               avol = avol + exp( -1.0*(min(40.0,(dd**prof(9)))) )
            endif
         enddo
      enddo
      vol = vol + prof(7)*avol*prof(1)*prof(2)/9.0

C  Now add the residuals map

      if (domap) then
         avol = 0.0
         rr = radius*radius*real(magnif*magnif)
         do k = 1, mapy
            do j = 1, mapx
               dx = j - mapx/2
               dy = k - mapy/2
               dd = dx*dx + dy*dy
               if ( dd.le.rr ) then
                  avol = avol + map(j,k,mapnum)
               endif
            enddo
         enddo
         vol = vol + (avol/(real(magnif)*real(magnif)))
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETD -- Get x,y, and height out of a table, using the header code
C
C    It is assumed that if there is no 'X' or 'Y' headers, then
C    the x,y is stored in columns 6,7 (the 1st data columns).
C    If there is no 'H' header then the height is assumed to be in
C    col number 'nhtcol' (and is set to 0.0 if there is no such col)
C
C    If there is no header for a particular value, the
C    default values in the input prof,magnifk,mapxk,mapyk,mapnumk are
C    taken. IF HOWEVER 'PBOSS' is set, this makes prof override for
C    profile.
C
C
C     a j penny                stsci                   1987-02-24

      subroutine getd ( tab, tbvx, tby, khead, num, x, y, h, nhtcol,
     +                  profk, prof, mapnum, mapx, mapy, magnif,
     +                  magnifk, mapxk, mapyk, mapnumk, pboss )

      implicit none
      include 'STARMAN_INC'

      integer  tbvx		!i: X size of table
      integer  tby		!i: Y size of table
      real     tab(tbvx,tby)	!i: Table input
      integer  khead(16)	!i: Column locations of various data
      integer  num		!i: Row of table to get data from
      integer  mapnum		!i: Map number
      integer  mapx		!o: Map X size
      integer  mapy		!o: Map Y size
      integer  magnif		!o: Map magnification
      integer  magnifk		!i: Default map magnification
      integer  mapxk		!i: Default map X size
      integer  mapyk		!i: Default map Y size
      integer  mapnumk		!i: Default map number
      integer  nhtcol		!i: Column number (-5) holding height
      real     profk(9)		!i: Default profile
      logical  pboss		!i: Flag (true=use array;
				!   false=take std profile)
      real     prof(9)		!o: Output profile
      real     x		!o: X position
      real     y		!o: Y position
      real     h		!o: Height
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      x = tab(6,num)
      if ( khead(1).ne.0 ) x = tab(khead(1)+5,num)			!Position
      y = tab(7,num)
      if ( khead(2).ne.0 ) y = tab(khead(2)+5,num)

      h = 0.0								!Height
      if ( khead(3).eq.0 ) then
         if ( nhtcol.ne.0 .and. nhtcol.ge.1 .and. nhtcol+5.le.tbvx )
     +      h = tab(nhtcol+5,num)
      else
         h = tab(khead(3)+5,num)
      endif

      call amovr ( profk, prof, 9 )					!Main profile
      mapnum = mapnumk
      mapx = mapxk
      mapy = mapyk
      magnif = magnifk

      if ( .not.pboss ) then						!File profile overwrites?
         do k = 1, 9
            if ( khead(k+3).ne.0 ) then
               if ( k.eq.6 ) then
                  prof(k) = tab(khead(k+3)+5,num)*(3.1415927/180.0)
               else
                  prof(k) = tab(khead(k+3)+5,num)
               endif
            endif
         enddo
         if ( khead(13).ne.0 ) mapnum = nint(tab(khead(13)+5,num))
         if ( khead(14).ne.0 ) mapx = nint(tab(khead(14)+5,num))
         if ( khead(15).ne.0 ) mapy = nint(tab(khead(15)+5,num))
         if ( khead(16).ne.0 ) magnif = nint(tab(khead(16)+5,num))
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET_MPROF -- Get the star profile from the profile file on disk

C  a j penny                 stsci              1987-02-2

      subroutine get_mprof ( file, ipinr, prof, mx, my, mz, magnif,
     +                       mapx, mapy, pbs, pbz, qbase, vol,
     +                       volrad, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) 	file		!i: File name
      integer 		ipinr		!o: Pointer to data
      real 		prof(9)		!o: Profile
      integer 		mx		!o: Map X size
      integer 		my		!o: Map Y size
      integer 		mz		!o: Map Z size
      integer 		magnif		!o: Map magnification
      integer 		mapx		!o: Map X size?
      integer 		mapy		!o: Map Y size?
      real 		pbs		!o: Map value scale
      real 		pbz		!o: Map X size
      real              qbase		!o: Wing profile base
      real 		vol		!o: Profile volume
      real 		volrad		!o: Profile volume calced in this
					!   radius
      integer 		ierr		!o: Error flag (0=ok:1=bad file:
					!               2=descriptor wrong)
C--
      real angle
      integer istat, lyma
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      ipinr = 1
      call amovkr ( 1.0, prof, 9 )				!Default profile
      mx = 1
      my = 1
      mz = 1
      magnif = 1
      mapx = 1
      mapy = 1
      pbs = 1.0
      pbz = 0.0
      vol = 1.0
      volrad = 1.0

      call opimsr ( file, ipinr, mx, lyma, .true., istat )	!Get image. If can't, return
      if ( ST_FAILED .or. istat.ne.0 ) then
         ierr = 1
         return
      endif

      call gtdesr ( file, 'BSCALE',   pbs,      1.0, istat )	!Get descriptors
      ierr = max(ierr,istat)
      call gtdesr ( file, 'BZERO',    pbz,      0.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'RX',       prof(1),  4.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'RY',       prof(2),  4.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'P',        prof(3),  2.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'PRX',      prof(4), 20.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'PRY',      prof(5), 20.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'THETA',    angle,    0.0, istat )
      ierr = max(ierr,istat)
      prof(6) = angle*3.1415926536/180.0
      call gtdesr ( file, 'QH',       prof(7),  0.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'QR',       prof(8), 20.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'QP',       prof(9),  2.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'QBASE',      qbase,  0.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'VOL',      vol,      0.0, istat )
      ierr = max(ierr,istat)
      call gtdesr ( file, 'VOLRAD',   volrad,   0.0, istat )
      ierr = max(ierr,istat)
      call gtdesi ( file, 'MAPMAX',   mz,         1, istat )
      ierr = max(ierr,istat)
      my = lyma/mz
      call gtdesi ( file, 'MAGNIF', magnif,     1, istat )
      ierr = max(ierr,istat)

      call gtdesi ( file, 'MAPX',     mapx,      mx, istat )
      call gtdesi ( file, 'MAPY',     mapy,      my, istat )

      if ( ierr.ne.0 ) ierr = 2


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C HTVAL -- Calc height of lorentz profile given coded input
C          parameters ( only used by s/r CVOLUME)
C
C      a j penny                     stsci                1987-05-29

      subroutine htval ( x, y, fxa, fxb, fya, fyb, pa, rslim, val )

      implicit none
      include 'STARMAN_INC'

      real  x		!i: X position in profile
      real  y		!i: Y position in profile
      real  fxa		!i: Profile X radius
      real  fxb		!i: Profile X power radius
      real  fya		!i: Profile Y radius
      real  fyb		!i: Profile Y power radius
      real  pa		!i: Profile power
      real  rslim	!i: Max distance out squared 'val' calculated for
      real  val		!o: Profile height at that posn
C--
      real xx, yy, xxa, xxb, yya, yyb, gg, hh, pab, pac
Cbegin


      if ( ST_FAILED ) return

      val = 0.0

      xx = x*x
      yy = y*y
      if ( (xx+yy).lt.rslim ) then
         xxa = xx*fxa
         xxb = xx*fxb
         yya = yy*fya
         yyb = yy*fyb
         gg = xxa + yya
         hh = sqrt(xxb+yyb)
         pab = pa*(1.0+hh)
         pac = pab*alog10(gg)
         if (pac.lt.25.0) then
            val = 1.0/(1.0+10.0**pac)
        endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPAFI -- Add a star to an integer image with pixel flags
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popafi ( kdata, nx, ny, bs, kdo, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer  	kdata(nx,ny)  	!i/o: image
      real      bs              !i: Scale of image
      logical	kdo(nx,ny)	!i: image pixel flags
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap	 	!i: Flag whether to use map
      integer   mx	 	!i: Maximum Map x size in Map array
      integer   my	 	!i: Maximum Map y size in Map array
      integer   mz	 	!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum	 	!i: Number of map to actually use
      integer   mapx	 	!i: Actual Map x size
      integer   mapy	 	!i: Actual Map x size
      integer   magnif	 	!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the addition, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
           if ( kdo(j,k) ) then
             call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                      sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                      domap, map, mx, my, mx, mapnum, mapx,
     +                      mapy, magnif )

             val = real(kdata(j,k)) + val/bs
             kdata(j,k) = nint(min(2147480000.0,max(-2147480000.0,val)))
           endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPAMI -- Add a star to an integer image with pixel magic values
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popami ( kdata, nx, ny, bs, inval, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer   kdata(nx,ny)  	!i/o: image
      real      bs              !i: image value scale
      integer   inval           !i: image bad pixel magic value
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap		!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif		!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the addition, for valid pixels in the desired area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( kdata(j,k).ne.inval ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               val = real(kdata(j,k)) + val/bs
               kdata(j,k)=nint(min(2147480000.0,max(-2147480000.0,val)))
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPAMR -- Add a star to a real image with pixel magic values
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popamr ( rdata, nx, ny, bs, rinval, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      real 	rdata(nx,ny)  	!i/o: image
      real      bs              !i: image value scale
      real      rinval          !i: image bad pixel magic value
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap		!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif		!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the addition, for valid pixels in the desired area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( rdata(j,k).ne.rinval ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               rdata(j,k) = rdata(j,k) + val/bs
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPAMS -- Add a star to an integer*2 image with pixel magic values
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popams ( kdata, nx, ny, bs, inval, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer*2 kdata(nx,ny)  	!i/o: image
      real      bs              !i: image value scale
      integer   inval           !i: image bad pixel magic value
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap		!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif		!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the addition, for valid pixels in the desired area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( kdata(j,k).ne.inval ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               val = real(kdata(j,k)) + val/bs
               kdata(j,k)=nint(min(32767.0,max(-32768.0,val)))
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPSFI -- Subtract a star from an integer image with pixel flags
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popsfi ( kdata, nx, ny, bs, kdo, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer  	kdata(nx,ny)  	!i/o: image
      real      bs              !i: Scale of image
      logical	kdo(nx,ny)	!i: image pixel flags
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap	 	!i: Flag whether to use map
      integer   mx	 	!i: Maximum Map x size in Map array
      integer   my	 	!i: Maximum Map y size in Map array
      integer   mz	 	!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum	 	!i: Number of map to actually use
      integer   mapx	 	!i: Actual Map x size
      integer   mapy	 	!i: Actual Map x size
      integer   magnif	 	!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the subtraction, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
           if ( kdo(j,k) ) then
             call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                      sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                      domap, map, mx, my, mx, mapnum, mapx,
     +                      mapy, magnif )

             val = real(kdata(j,k)) - val/bs
             kdata(j,k) = nint(min(2147480000.0,max(-2147480000.0,val)))
           endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPSFR -- Subtract a star from a real image with pixel flags
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popsfr ( data, nx, ny, bs, kdo, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      real	data(nx,ny)  	!i/o: image
      real      bs              !i: Scale of image
      logical	kdo(nx,ny)	!i: image pixel flags
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap	 	!i: Flag whether to use map
      integer   mx	 	!i: Maximum Map x size in Map array
      integer   my	 	!i: Maximum Map y size in Map array
      integer   mz	 	!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum	 	!i: Number of map to actually use
      integer   mapx	 	!i: Actual Map x size
      integer   mapy	 	!i: Actual Map x size
      integer   magnif	 	!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the subtraction, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
           if ( kdo(j,k) ) then
              call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                       sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                       domap, map, mx, my, mx, mapnum, mapx,
     +                       mapy, magnif )
              data(j,k) = data(j,k) - val/bs
           endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPSFS -- Subtract a star from a short image with pixel flags
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popsfs ( sdata, nx, ny, bs, kdo, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer*2	sdata(nx,ny)  	!i/o: image
      real      bs              !i: Scale of image
      logical	kdo(nx,ny)	!i: image pixel flags
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap	 	!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif	 	!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the subtraction, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( kdo(j,k) ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               val = real(sdata(j,k)) - val/bs
               sdata(j,k) = nint(min(32767.0,max(-32768.0,val)))
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPSMR -- Subtract a star from a real image with pixel magic values
C
C   a.j.penny                   ral                    1988-08-06

      subroutine popsmr ( rdata, nx, ny, bs, rinval, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      real 	rdata(nx,ny)  	!i/o: image
      real      bs              !i: image value scale
      real      rinval          !i: image bad pixel magic value
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap		!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif		!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the subtraction, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( rdata(j,k).ne.rinval ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               rdata(j,k) = rdata(j,k) - val/bs
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POPSMS -- Subtract a star from a short image with magic values
C
C   a.j.penny                   ral                    1988-08-06


      subroutine popsms ( sdata, nx, ny, bs, inval, xp, yp, hp, prof,
     +                    map, mx, my, mz, mapnum,
     +                    mapx, mapy, magnif, domap, ierr, kw )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of image
      integer	ny		!i: Y size of image
      integer*2	sdata(nx,ny)  	!i/o: image
      real      bs              !i: Scale of image
      integer   inval		!i: image pixel magic value
      real	xp		!i: star x posn
      real	yp		!i: star y posn
      real	hp      	!i: star height
      real	prof(9)		!i: profile (rx,ry,p,prx,pry,angle(radians),
				!            wh,wp,wr)
      logical   domap	 	!i: Flag whether to use map
      integer   mx		!i: Maximum Map x size in Map array
      integer   my		!i: Maximum Map y size in Map array
      integer   mz		!i: Total no of maps in Map array
      real      map(mx,my,mz) 	!i: Array containing all maps
      integer   mapnum		!i: Number of map to actually use
      integer   mapx		!i: Actual Map x size
      integer   mapy		!i: Actual Map x size
      integer   magnif		!i: Map pixel subdivision
      integer	ierr		!o: Error flag (0=ok:1=no placement)
      integer   kw(4)		!o: Bounds of area affected (lxs,lxe,lys,lye)
C--
      real co, si, sim, qh, qr, qp, val, ap, hx2, hy2, gx2, gy2
      integer lxs, lxe, lys, lye, j, k, lx, ly
      logical ok
Cbegin


      if ( ST_FAILED ) return

C  Set up parameters and check if ok

      call proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye, co,
     +              si, sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp, nx,
     +              ny, ok )
      ierr = 0
      if ( .not.ok ) then
         ierr = 1
         return
      endif

C  Do the subtraction, for valid pixels in the right area, with scaling

      do k = lys, lye
         do j = lxs, lxe
            if ( sdata(j,k).ne.inval ) then
               call profval ( val, j, k, xp, yp, hp, lx, ly, co, si,
     +                        sim, gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                        domap, map, mx, my, mx, mapnum, mapx,
     +                        mapy, magnif )
               val = real(sdata(j,k)) - val/bs
               sdata(j,k) = nint(min(32767.0,max(-32768.0,val)))
            endif
         enddo
      enddo

      kw(1) = lxs
      kw(2) = lxe
      kw(3) = lys
      kw(4) = lye


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PROSET -- Set up values for s/r POP(AS)(FM)(SR)
C
C  a j penny              ral                1989-08-04

      subroutine proset ( xp, yp, hp, prof, lx, ly, lxs, lxe, lys, lye,
     +                    co, si, sim, gx2, gy2, ap, hx2, hy2, qh, qr,
     +                    qp, nx, ny, ok )

      implicit none
      include 'STARMAN_INC'

      real xp		!i: X position wrt profile centre
      real yp		!i: Y position wrt profile centre
      real hp		!i: Star height
      real prof(9)	!i: Profile
      real co		!o: Cosine (profile angle)
      real si		!o: Sine (profile angle)
      real sim		!o: -1*sine (profile angle)
      real qh		!o: Gaussian profile height
      real qr		!o: Gaussian profile radius
      real qp		!o: Gaussian profile power
      real ap		!o: Power/2.0
      real hx2		!o: 1/(X radius squared)
      real hy2		!o: 1/(Y radius squared)
      real gx2		!o: 1/(X power radius squared)
      real gy2		!o: 1/(Y power radius squared)
      integer lxs	!o: X start of box to work in
      integer lxe	!o: X end of box to work in
      integer lys	!o: Y start of box to work in
      integer lye	!o: Y end of box to work in
      integer lx	!o: Recommended X pixel sub-division
      integer ly	!o: Recommended Y pixel sub-division
      integer nx	!i: X image size
      integer ny	!i: Y image size
      logical ok	!o: Error flag (true=ok;false=bad)
C--
      real gx, gy, hx, hy, p, theta, gav, hav, dl, ha, hal, hag, yap
Cbegin


      if ( ST_FAILED ) return

      ok = .true.

C  Check to see if profile radii all +ve

      if ( prof(1).le.0.0 .or. prof(2).le.0.0 .or. prof(4).le.0.0 .or.
     +     prof(5).le.0.0 ) then
         ok = .false.
         return
      endif
      if ( prof(7).ne.0.0 .and. prof(8).le.0.0 ) then
         ok = .false.
         return
      endif

C  Check to see if height is non-zero

      if ( hp.eq.0.0 ) then
         ok = .false.
         return
      endif

C  Load image parameters

      gx = 1.0/prof(1)
      gy = 1.0/prof(2)
      hx = 1.0/prof(4)
      hy = 1.0/prof(5)
      gav = (gx+gy)/2.0
      hav = (hx+hy)/2.0
      p = prof(3)
      theta = prof(6)
      co = cos(theta)
      si = sin(theta)
      sim = -1.0*si

      qh = prof(7)
      if ( qh.ne.0.0 ) qr = 1.0/prof(8)
      qp = prof(9)

C  Calculate distance (dl) out to work to, as set by image value = 0.2

      dl = 0.0
      ha = hp
      hal = hp
      hag = hp*qh
      do while (ha.gt.0.2)

         dl = dl + 1.0

C  Lorentz part
C  Calculate hal = hp/(1.0+(dl*gav)**(p*(1.0+dl*hav))), avoiding overfow

         if (hal.gt.0.01) then
            yap = p*(1.0+dl*hav)*alog(dl*gav)
            if (yap.lt.20.0) then
               hal = hp/(1.0+exp(yap))
            else
               hal = 0.0
            endif
         endif

C  Gaussian part

         if ( hag.gt.0.01 ) then
            hag = hp*qh*exp(-1.0*((dl*qr)**qp))
         endif

         ha = hal + hag

      enddo

C  Box in image to work in

      lxs = xp - dl
      lxe = lxs + 2*dl + 1.0
      lys = yp - dl
      lye = lys + 2*dl + 1.0

C  Return if none in area

      if ( lxs.gt.nx .or. lxe.lt.1 .or. lys.gt.ny .or. lye.lt.1 ) then
         ok = .false.
         return
      endif

C  Check only work in area

      lxs = max(1,min(lxs,nx))
      lxe = max(1,min(lxe,nx))
      lys = max(1,min(lys,ny))
      lye = max(1,min(lye,ny))

C Preload more image parameters

      lx = 1 + int(2.9/min(prof(1),prof(2)))
      ly = lx
      ap = prof(3)/2.0
      gx2 = 1.0/(prof(1)*prof(1))
      gy2 = 1.0/(prof(2)*prof(2))
      hx2 = 1.0/(prof(4)*prof(4))
      hy2 = 1.0/(prof(5)*prof(5))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PROFVAL -- Calc profile value at a pixel.
C
C   a.j.penny                   stsCi                 1987-02-20

      subroutine profval ( val, j, k, xp, yp, hp, lx, ly, co, si, sim,
     +                     gx2, gy2, ap, hx2, hy2, qh, qr, qp,
     +                     domap, map, mx, my, mz, mapnum, mapx, mapy,
     +                     magnif )

      implicit none
      include 'STARMAN_INC'

      real    val	    !o: Profile value
      integer j	 	    !i: X position
      integer k	 	    !i: Y position
      real    xp	    !i: X position wrt profile centre
      real    yp	    !i: Y position wrt profile centre
      real    hp	    !i: Star height
      integer lx	    !i: X pixel sub-division
      integer ly	    !i: Y pixel sub-division
      real    co	    !i: Cosine (profile angle)
      real    si	    !i: Sine (profile angle)
      real    sim	    !i: -1*sine (profile angle)
      real    gx2	    !i: 1/(X power radius squared)
      real    gy2	    !i: 1/(Y power radius squared)
      real    ap	    !i: Power/2.0
      real    hx2	    !i: 1/(X radius squared)
      real    hy2	    !i: 1/(Y radius squared)
      real    qh	    !i: Gaussian profile height
      real    qr	    !i: Gaussian profile radius
      real    qp	    !i: Gaussian profile power
      logical domap	    !i: Flag whether to use map
      integer mx	    !i: Maximum Map x size in Map array
      integer my	    !i: Maximum Map y size in Map array
      integer mz	    !i: Total no of maps in Map array
      real    map(mx,my,mz) !i: Array containing all maps
      integer mapnum	    !i: Number of map to actually use
      integer mapx	    !i: Actual Map x size
      integer mapy	    !i: Actual Map x size
      integer magnif	    !i: Map pixel subdivision
C--
      real dx, dy, ay, ax, dxa, dya, da, db, dxb, dyb, zt, dla, z, vala,
     +     dfdx, dfdy, dxm, dym
      integer llx, lly, mapx2, mapy2
      real rinter
      external rinter
Cbegin


      if ( ST_FAILED ) return

      dx = real(j) - xp
      dy = real(k) - yp
      z = 0.0
      do lly = 1, ly
         do llx = 1, lx
            ay = -0.5 + ((0.5+real(lly)-1.0)/real(ly))
            ax = -0.5 + ((0.5+real(llx)-1.0)/real(lx))
            dxa = dx + ax
            dya = dy + ay
            dla = sqrt(dxa*dxa+dya*dya)
            if (dla.lt.0.00001) dla = 0.00001
            dxb = dxa*co + dya*si
            dyb = dxa*sim + dya*co
            da = dxb*dxb*gx2 + dyb*dyb*gy2
            if (da.lt.0.00001) da = 0.00001
            db = sqrt(dxb*dxb*hx2+dyb*dyb*hy2)
            zt = ap*(1.0+db)*alog(da)
            if (zt.gt.10.0) zt = 10.0
            z = z + 1.0/(1.0+exp(zt))

            if (qh.ne.0.0) then
               zt = (dla*qr)**qp
               if (zt.gt.10.0) zt = 10.0
               z = z + qh*exp(-1.0*zt)
            endif

         enddo
      enddo

      val = hp*(z/real(lx*ly))

C     And the profile map at the pixel

      if ( domap ) then
         dxm = dx*real(magnif)
         dym = dy*real(magnif)
         mapx2 = mapx/2
         mapy2 = mapy/2
         ax = mapx2 + dxm
         ay = mapy2 + dym
         vala = rinter ( map, mx, my, mz, mapnum, mapx, mapy,
     +                   ax, ay, dfdx, dfdy )
         val = val + hp*vala
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SKY_0 -- Find background and noise in a real or integer*2 image
C
C  Divide image into 50x50 squares. Get square with lowest mean
C  value. Assume this is the background.
C
C  Take this area. Make a histogram. Fit a Gaussian to the
C  peak. Take mean and std dev of that Gaussian as the mean
C  and std dev of the image background.
C
C   a j penny                   stsci             86-12-02

      subroutine sky_0 ( im, gtype, nx, ny, inval, rinval, skylev,
     +                   skystd )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: Image X size
      integer   ny		!i: Image Y size
      integer*2 im(*)		!i: Input image
      character*(*)  gtype	!i: Image type (REAL/SHORT)
      integer   inval		!i: Image (int*2) invalid number flag
      real      rinval		!i: Image (real) invalid number flag
      real      skylev		!o: Background level
      real      skystd		!o: Background noise
C--

      integer kgram(1000), kxc(2), kyc(2), nxbox, nybox, j, k, kl
      integer jl, istat, kmin, kbin, num, lmax, lmin, kv, iter
      real rkgram(1000), aam1, astd1, aam, astd, xo, a, b, r, pow,
     +     rva(1), rbin, rmin, rmax
      integer jfita(5,4)
      data jfita / 0, 1, 1, 0, 1,
     +             0, 0, 0, 0, 1,
     +             0, 1, 1, 0, 0,
     +             0, 1, 1, 0, 1 /
Cbegin


      if ( ST_FAILED ) return

      kxc(1) = 1 							!Get 1st background for
      kxc(2) = min(50,nx)						! b.l.h. 50x50 box
      kyc(1) = 1
      kyc(2) = min(50,ny)
      if ( gtype.eq.'SHORT' ) then
         call ranges ( im, nx, ny, kxc, kyc, inval, aam1, astd1, istat )
      else
         call ranger ( im, nx, ny, kxc, kyc, rinval, aam1, astd1, istat)
      endif

      nxbox = 1 + (nx-1)/50						!Find position of 50x50
      nybox = 1 + (ny-1)/50						! box with lowest mean
      j = 1
      k = 1
      jl = j
      kl = k
      do k = 1, nybox
         do j = 1, nxbox
            kxc(1) = 1 + (j-1)*50
            kxc(2) = min((50+(j-1)*50),nx)
            kyc(1) = 1 + (k-1)*50
            kyc(2) = min((50+(k-1)*50),ny)
            if ( gtype.eq.'SHORT' ) then
               call ranges ( im, nx, ny, kxc, kyc, inval,aam,astd,istat)
            else
               call ranger ( im, nx, ny, kxc, kyc,rinval,aam,astd,istat)
            endif
            if ( aam.lt.aam1 ) then
               aam1 = aam
               astd1 = astd
               jl = j
               kl = k
            endif
         enddo
      enddo

      rmin = aam1 - 6.0*astd1						!Take mean +/- 6 std dev
      rmax = aam1 + 6.0*astd1
      rbin = 1.0 + (rmax-rmin)/1000.0
      kbin = rbin
      kmin = rmin
      num = (rmax-rmin-1)/kbin
      kxc(1) = 1 + (jl-1)*50						!Define area
      kxc(2) = min((50+(jl-1)*50),nx)
      kyc(1) = 1 + (kl-1)*50
      kyc(2) = min((50+(kl-1)*50),ny)
      if ( gtype.eq.'SHORT' ) then				 	!Make histogram
         call hgrams ( im, nx, ny, kxc(1), kxc(2), kyc(1), kyc(2),
     +                 kmin, kgram, 1000, kbin, inval )
      else
         call hgramr ( im, nx, ny, kxc(1), kxc(2), kyc(1), kyc(2),
     +                 rmin, kgram, 1000, rbin, rinval )
      endif

      lmin = kgram(1)							!Fit 1-D Gaussian to histogram
      lmax = kgram(1)
      xo = 1
      do k = 1, 1000
         kv = kgram(k)
         rkgram(k) = kgram(k)
         lmin = min(lmin,kv)
         if ( kv.gt.lmax ) then
            lmax = kv
            xo = k
         endif
      enddo
      a = lmin
      b = lmax - lmin
      r = astd/(1.5*rbin)
      pow = 2.0
      do j = 2, 4
         call gauss1r ( rkgram, rva, 1000, .false., 1, num, a, b, r,
     +           pow, xo, jfita(1,j), 0.004, 0.004, 20, 0.5, iter, 0 )
      enddo
      r = min(real(num),max(0.1,r))
      xo = min(real(num),max(1.0,xo))

      lmin = min(real(num),max(1.0,(xo-2.0*r)))				!Refine fit by fitting to
      lmax = max(1.0,min(real(num),(xo+2.0*r)))				! histogram peak
      if ( (lmin+1).ge.lmax ) then
         skylev = rmin - 1 + rbin*real(lmin+lmax)/2.0
         skystd = rbin*real(lmax-lmin)/4.0
      else
         do j = 2, 4
            call gauss1r ( rkgram, rva, 1000, .false., lmin, lmax, a,
     +      b, r, pow, xo, jfita(1,j), 0.004, 0.004, 20, 0.5, iter, 0 )
         enddo
         r = min(real(num),max(0.1,r))
         xo = min(real(lmax),max(real(lmin),xo))
         skylev = rmin - 1 + xo*rbin					!Turn histogram mean, std dev
         skystd = r*rbin						! to image mean, std dev
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SKY_1R -- Find background and noise in boxes in a real image
C
C   a j penny                   stsci             86-12-02

      subroutine sky_1r ( im, nx, ny, rinval, msky, asky, nxsky, nysky,
     +                    csky, bsky, nxsd, nysd, failed )

      implicit none
      include 'STARMAN_INC'

      integer        nx, ny			!i: Image size
      real           im(nx,ny)			!i: Input image
      real           rinval			!i: Image (real) invalid number flag
      real           msky(nx,ny)		!o: Sky level
      real           asky(nx,ny)		!o: Sky level + 1 std dev
      integer        nxsky, nysky		!i: No of sky boxes
      real           bsky(nxsky,nysky)		!o: Mean + 1 std dev in sky boxes
      real           csky(nxsky,nysky)		!o: Mean levels in sky boxes
      integer        nxsd			!i: X Size of sky boxes
      integer        nysd			!i: Y Size of sky boxes
      logical        failed			!i/o: Failure flag. True = exit
C--

      integer kgram(1024), kxc(2), kyc(2), j, k, istat,
     +        num, numbin, lmin, lmax, kv, kkmin, kkmax,
     +        iter, kxa, kxb, kya, kyb, ja, kk
      real rkgram(1024), aam, astd, a, b, r, xo, val, skylev, avtot,
     +     pow, rva(1), rmin, rmax, rbin, rsky
      logical allinv
      integer jfita(5,4)
      data jfita / 0, 1, 1, 0, 1,
     +             0, 0, 0, 0, 1,
     +             0, 1, 1, 0, 0,
     +             0, 1, 1, 0, 1 /
Cbegin


      if ( ST_FAILED ) return

      if ( failed ) return						!Check failure

      numbin = min( 1024, int(500.0*real(nxsd)*real(nysd)/4096.0) )	!Get mean levels and scatters

      skylev = 0.0
      val = 1.0
      do k = 1, nysky
         do j = 1, nxsky

            kxc(1) = 1 + (j-1)*nxsd
            kxc(2) = min((nxsd+(j-1)*nxsd),nx)
            kyc(1) = 1 + (k-1)*nysd
            kyc(2) = min((nysd+(k-1)*nysd),ny)

            call azzallbadr ( im, nx, ny, rinval, kxc, kyc, allinv )	!Check some valid

            if ( .not.allinv ) then					!Default level
               call ranger ( im, nx, ny, kxc,kyc,rinval,aam,astd,istat)
               skylev = aam
               val = aam + astd
            endif

            if ( .not.allinv .and. 					!Better level
     +          (((kxc(2)-kxc(1)).ge.9).and.((kyc(2)-kyc(1)).ge.9)))then
               skylev = aam
               val = aam + 1.0
               rmin = aam - 6.0*astd
               rmax = aam + 6.0*astd
               rbin = (rmax-rmin)/real(numbin)
               num =  1 + int((rmax-rmin)/rbin)

               if ( num.gt.4 ) then
                  kxa = 1 + (j-1)*nxsd
                  kxb = min((nxsd+(j-1)*nxsd),nx)
                  kya = 1 + (k-1)*nysd
                  kyb = min((nysd+(k-1)*nysd),ny)
                  call hgramr ( im, nx, ny, kxa, kxb, kya, kyb, rmin,
     +                          kgram, numbin, rbin, rinval )
                  lmin = kgram(1)
                  lmax = kgram(1)
                  xo = 1
                  avtot = 0.0
                  do kk = 1, numbin
                     kv = kgram(kk)
                     rkgram(kk) = kgram(kk)
                     avtot = avtot + kv
                     lmin = min(lmin,kv)
                     if ( kv.gt.lmax ) then
                        lmax = kv
                        xo = kk
                     endif
                  enddo
                  if ( avtot.ne.0.0 ) then
                     if ( (real(lmax)/avtot).le.0.4 ) then
                        a = lmin
                        b = lmax - lmin
                        r = astd/(1.5*rbin)
                        pow = 2.0
                        do ja = 2, 4
                           call gauss1r ( rkgram, rva, numbin, .false.,
     +                          1, num, a, b, r, pow, xo, jfita(1,ja),
     +                          0.004, 0.004, 20, 0.5, iter,  0 )
                        enddo
                        r = min(real(numbin),max(0.1,r))
                        xo = min(real(numbin),max(1.0,xo))
                        kkmin = min(real(numbin),max(1.0,(xo-2.0*r)))
                        kkmax = max(1.0,min(real(numbin),(xo+2.0*r)))
                        if ( (kkmin+1).ge.kkmax ) then
                          skylev =rmin+(rbin*real(kkmin+kkmax)/2.0)-rbin
                          val = skylev + 1.5*rbin*real(kkmax-kkmin)/4.0
                        else
                           do ja = 2, 4
                              call gauss1r ( rkgram, rva, numbin,
     +                        .false., kkmin, kkmax, a, b, r, pow, xo,
     +                        jfita(1,ja) ,0.004, 0.004, 20, 0.5,iter,0)
                           enddo
                           r = min(real(numbin),max(0.1,r))
                           xo = min(real(numbin),max(1.0,xo))
                           skylev = rmin + xo*rbin - rbin
                           val =  skylev + 1.5*r*rbin
                        endif
                     endif
                  endif
               endif
            endif

            bsky(j,k) = val						!Load level
            csky(j,k) = skylev

         enddo
      enddo

      do k = 1, ny							!Turn means into a sky map
         do j = 1, nx
            call sky_2r ( bsky, nxsky, nysky, j, k, rsky, nxsd, nysd )
            asky(j,k) = rsky
         enddo
      enddo
      do k = 1, ny
         do j = 1, nx
            call sky_2r ( csky, nxsky, nysky, j, k, rsky, nxsd, nysd )
            msky(j,k) = rsky
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SKY_1S -- Find background and noise in boxes in a real image
C
C   a j penny                   stsci             86-12-02

      subroutine sky_1s ( im, nx, ny, inval, msky, asky, nxsky, nysky,
     +                    csky, bsky, nxsd, nysd, failed )

      implicit none
      include 'STARMAN_INC'

      integer        nx, ny			!i: Image size
      integer*2      im(nx,ny)			!i: Input image
      integer        inval			!i: Image (real) invalid number flag
      real           msky(nx,ny)		!o: Sky level
      real           asky(nx,ny)		!o: Sky level + 1 std dev
      integer        nxsky, nysky		!i: No of sky boxes
      real           bsky(nxsky,nysky)		!o: Mean + 1 std dev in sky boxes
      real           csky(nxsky,nysky)		!o: Mean levels in sky boxes
      integer        nxsd			!i: X Size of sky boxes
      integer        nysd			!i: Y Size of sky boxes
      logical        failed			!i/o: Failure flag. True = exit
C--

      integer kgram(1024), kxc(2), kyc(2), j, k, istat, kmin, kmax,
     +        kbin, num, numbin, lmin, lmax, kv, kkmin, kkmax,
     +        iter, kxa, kxb, kya, kyb, ja, kk
      real rkgram(1024), aam, astd, a, b, r, xo, val, skylev, avtot,
     +     pow, rva(1), rsky
      logical allinv
      integer jfita(5,4)
      data jfita / 0, 1, 1, 0, 1,
     +             0, 0, 0, 0, 1,
     +             0, 1, 1, 0, 0,
     +             0, 1, 1, 0, 1 /
Cbegin


      if ( ST_FAILED ) return

      if ( failed ) return						!Check failure

      numbin = min( 1024, int(500.0*real(nxsd)*real(nysd)/4096.0) )	!Get mean levels and scatters

      skylev = 0.0
      val = 1.0
      do k = 1, nysky
         do j = 1, nxsky

            kxc(1) = 1 + (j-1)*nxsd
            kxc(2) = min((nxsd+(j-1)*nxsd),nx)
            kyc(1) = 1 + (k-1)*nysd
            kyc(2) = min((nysd+(k-1)*nysd),ny)

            call azzallbads ( im, nx, ny, inval, kxc, kyc, allinv )	!Check some valid

            if ( .not.allinv ) then					!Default level
               call ranges ( im, nx, ny, kxc,kyc,inval,aam,astd,istat)
               skylev = aam
               val = aam + astd
            endif

            if ( .not.allinv .and. 					!Better level
     +          (((kxc(2)-kxc(1)).ge.9).and.((kyc(2)-kyc(1)).ge.9)))then
               skylev = aam
               val = aam + 1.0
               kmin = aam - 6.0*astd
               kmax = aam + 6.0*astd
               kbin = 1 + (kmax-kmin)/numbin
               num =  (kmax-kmin+1)/kbin

               if ( num.gt.4 ) then
                  kxa = 1 + (j-1)*nxsd
                  kxb = min((nxsd+(j-1)*nxsd),nx)
                  kya = 1 + (k-1)*nysd
                  kyb = min((nysd+(k-1)*nysd),ny)
                  call hgrams ( im, nx, ny, kxa, kxb, kya, kyb, kmin,
     +                          kgram, numbin, kbin, inval )
                  lmin = kgram(1)
                  lmax = kgram(1)
                  xo = 1
                  avtot = 0.0
                  do kk = 1, numbin
                     kv = kgram(kk)
                     rkgram(kk) = kgram(kk)
                     avtot = avtot + kv
                     lmin = min(lmin,kv)
                     if ( kv.gt.lmax ) then
                        lmax = kv
                        xo = kk
                     endif
                  enddo
                  if ( avtot.ne.0.0 ) then
                     if ( (real(lmax)/avtot).le.0.4 ) then
                        a = lmin
                        b = lmax - lmin
                        r = astd/(1.5*kbin)
                        pow = 2.0
                        do ja = 2, 4
                           call gauss1r ( rkgram, rva, numbin, .false.,
     +                          1, num, a, b, r, pow, xo, jfita(1,ja),
     +                          0.004, 0.004, 20, 0.5, iter,  0 )
                        enddo
                        r = min(real(numbin),max(0.1,r))
                        xo = min(real(numbin),max(1.0,xo))
                        kkmin = min(real(numbin),max(1.0,(xo-2.0*r)))
                        kkmax = max(1.0,min(real(numbin),(xo+2.0*r)))
                        if ( (kkmin+1).ge.kkmax ) then
                          skylev = kmin - 1 + kbin*real(kkmin+kkmax)/2.0
                          val = skylev + 1.5*kbin*real(kkmax-kkmin)/4.0
                        else
                           do ja = 2, 4
                              call gauss1r ( rkgram, rva, numbin,
     +                        .false., kkmin, kkmax, a, b, r, pow, xo,
     +                        jfita(1,ja) ,0.004, 0.004, 20, 0.5,iter,0)
                           enddo
                           r = min(real(numbin),max(0.1,r))
                           xo = min(real(numbin),max(1.0,xo))
                           skylev = kmin - 1 + xo*kbin
                           val =  skylev + 1.5*r*kbin
                        endif
                     endif
                  endif
               endif
            endif

            bsky(j,k) = val						!Load level
            csky(j,k) = skylev

         enddo
      enddo

      do k = 1, ny							!Turn means into a sky map
         do j = 1, nx
            call sky_2r ( bsky, nxsky, nysky, j, k, rsky, nxsd, nysd )
            asky(j,k) = rsky
         enddo
      enddo
      do k = 1, ny
         do j = 1, nx
            call sky_2r ( csky, nxsky, nysky, j, k, rsky, nxsd, nysd )
            msky(j,k) = rsky
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SKY_2S -- Find the sky, given a sky map table, and a position
C
C  a j penny                      stsci               1987 dec

      subroutine sky_2s ( asky, nxsky, nysky, kx, ky, ksky, nxsd, nysd )

      implicit none
      include 'STARMAN_INC'

      integer   nxsky			!i: X size of sky map table
      integer   nysky			!i: Y size of sky map table
      integer*2 asky(nxsky,nysky)	!i: Sky map table
      integer	kx			!i: X position
      integer   ky			!i: Y position
      integer	ksky			!o: Sky value
      integer	nxsd			!i: X box size in sky map table
      integer	nysd			!i: Y box size in sky map table
C--
      integer kxa, kya, kxb, kyb
      real usky, tsky, dx, dy, xa, ya
Cbegin


      if ( ST_FAILED ) return

      xa = 1.0 + (real(kx)-(real(nxsd)/2.0))/real(nxsd)			!Set up position relative to the grid of box mean points
      ya = 1.0 + (real(ky)-(real(nysd)/2.0))/real(nysd)
      kxa = xa
      kxb = kxa + 1
      kya = ya
      kyb = kya + 1
      dx = xa - kxa
      dy = ya - kya

      if ( kxa.lt.1 .and. kya.lt.1 ) then				!Do the interpolation for the
         ksky = asky(1,1)						! cases at the corners, at the edges,
      elseif ( kxa.lt.1 .and. kya.ge.nysky ) then			! in the body
         ksky = asky(1,nysky)
      elseif ( kxa.ge.nxsky .and. kya.lt.1 ) then
         ksky = asky(nxsky,1)
      elseif ( kxa.ge.nxsky .and. kya.ge.nysky ) then
         ksky = asky(nxsky,nysky)
      elseif ( kxa.lt.1 ) then
         ksky = asky(1,kya)+dy*(asky(1,kyb)-asky(1,kya))
      elseif ( kxa.ge.nxsky ) then
         ksky = asky(nxsky,kya)+dy*(asky(nxsky,kyb)-asky(nxsky,kya))
      elseif ( kya.lt.1 ) then
         ksky = asky(kxa,1)+dx*(asky(kxb,1)-asky(kxa,1))
      elseif ( kya.ge.nysky ) then
         ksky = asky(kxa,nysky)+dx*(asky(kxb,nysky)-asky(kxa,nysky))
      else
         tsky = asky(kxa,kya)+dx*(asky(kxb,kya)-asky(kxa,kya))
         usky = asky(kxa,kyb)+dx*(asky(kxb,kyb)-asky(kxa,kyb))
         ksky = tsky+dy*(usky-tsky)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SKY_2R -- Find the sky, given a sky map table, and a position
C
C  a j penny                      stsci               1987 dec

      subroutine sky_2r ( asky, nxsky, nysky, kx, ky, rsky, nxsd, nysd )

      implicit none
      include 'STARMAN_INC'

      integer   nxsky			!i: X size of sky map table
      integer   nysky			!i: Y size of sky map table
      real      asky(nxsky,nysky)	!i: Sky ymap table
      integer	kx			!i: X position
      integer   ky			!i: Y position
      real      rsky			!o: Sky value
      integer	nxsd			!i: X box size in sky map table
      integer	nysd			!i: Y box size in sky map table
C--
      integer kxa, kya, kxb, kyb
      real usky, tsky, dx, dy, xa, ya
Cbegin


      if ( ST_FAILED ) return

      xa = 1.0 + (real(kx)-(real(nxsd)/2.0))/real(nxsd)			!Set up position relative to the grid of box mean points
      ya = 1.0 + (real(ky)-(real(nysd)/2.0))/real(nysd)
      kxa = xa
      kxb = kxa + 1
      kya = ya
      kyb = kya + 1
      dx = xa - kxa
      dy = ya - kya

      if ( kxa.lt.1 .and. kya.lt.1 ) then				!Do the interpolation for the
         rsky = asky(1,1)						! cases at the corners, at the edges,
      elseif ( kxa.lt.1 .and. kya.ge.nysky ) then			! in the body
         rsky = asky(1,nysky)
      elseif ( kxa.ge.nxsky .and. kya.lt.1 ) then
         rsky = asky(nxsky,1)
      elseif ( kxa.ge.nxsky .and. kya.ge.nysky ) then
         rsky = asky(nxsky,nysky)
      elseif ( kxa.lt.1 ) then
         rsky = asky(1,kya)+dy*(asky(1,kyb)-asky(1,kya))
      elseif ( kxa.ge.nxsky ) then
         rsky = asky(nxsky,kya)+dy*(asky(nxsky,kyb)-asky(nxsky,kya))
      elseif ( kya.lt.1 ) then
         rsky = asky(kxa,1)+dx*(asky(kxb,1)-asky(kxa,1))
      elseif ( kya.ge.nysky ) then
         rsky = asky(kxa,nysky)+dx*(asky(kxb,nysky)-asky(kxa,nysky))
      else
         tsky = asky(kxa,kya)+dx*(asky(kxb,kya)-asky(kxa,kya))
         usky = asky(kxa,kyb)+dx*(asky(kxb,kyb)-asky(kxa,kyb))
         rsky = tsky+dy*(usky-tsky)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBDIV -- Calc pixel sub-division needed for given profile
C
C   a j penny                 dao           1988-04-25

      subroutine subdiv ( r1, r2, ksubdiv )

      implicit none
      include 'STARMAN_INC'

      real	r1		!i: 1st radius
      real	r2		!i: 2nd radius
      integer	ksubdiv		!o: Pixel sub-division
C--
      real	ar
Cbegin


      if ( ST_FAILED ) return

      ar = min(r1,r2)
      if ( ar.lt.0.001 ) then
         ksubdiv = 1
      else
         ksubdiv = 1 + int(2.9/ar)
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  STARFLIBV.F
C
C  It contains vector operations in the same format as the IRAF ones:-
C
C
C AADD(DIRS)     Add two vectors to make a third. C = A + B
C AADDK(DIRS)    Add constant to vector to make second B = A + K
C AAVG(DIRS)     Mean and standard deviation of an array
C ACHT(IRS:DIRS) Load a (real:int:short) array into a (dble:real:int:short) array
C ACHVAL(IRS)    Change a particular value in elements in a (short:int:real) vector
C ADIV(DIRS)     Divide array by another. C = A/B
C ADIVK(DIRS)    Divide array by constant.  B = A/K
C ALIM(DIRS)     Min and max of a vector
C AMEDIAN(IRS)   Median of (int:real:short) vector
C AMOV(BDIRS)    Load an array into another  B = A
C AMOVK(BDIRS)   Load a constant into an array  A = K
C AMUL(DIRS)     Multiply two arrays C = A.B
C AMULK(DIRS)    Multiply an array by a constant B = A.K
C ANUMINC(IR)    Load numbers (1,2,3,..) into array
C ASUB(DIRS)     Subtract a vector from another vector C = A - B
C ASUBK(DIRS)    Subtract a constant from a vector B = A - K
C ASUM(DIRS)     Sum a vector
C AZERO(BDIRS)   Zero a vector

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AADDD -- Adds two double precision vectors to make a third. C = A + B
C   a j penny                 dao           1988-04-25
      subroutine aaddd ( a, b, c, n )
      implicit none
      integer	n		!i: no of elements to add
      double precision 	a(n)	!i: 1st input vector
      double precision	b(n)	!i: 2nd input vector
      double precision	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) + b(j)
      enddo
      end
C AADDI -- Adds two integer vectors to make a third. C = A + B
C   a j penny                 dao           1988-04-25
      subroutine aaddi ( a, b, c, n )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: 1st input vector
      integer	b(n)	!i: 2nd input vector
      integer	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) + b(j)
      enddo
      end
C AADDR -- Adds two real vectors to make a third. C = A + B
C   a j penny                 dao           1988-04-25
      subroutine aaddr ( a, b, c, n )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: 1st input vector
      real	b(n)	!i: 2nd input vector
      real	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) + b(j)
      enddo
      end
C AADDS -- Adds two short integer vectors to make a third. C = A + B
C   a j penny                 dao           1988-04-25
      subroutine aadds ( a, b, c, n )
      implicit none
      integer	n	!i: no of elements to add
      integer*2	a(n)	!i: 1st input vector
      integer*2	b(n)	!i: 2nd input vector
      integer*2	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) + b(j)
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AADDKD -- Adds dble prec constant to vector to make second B=A+K
C   a j penny                 dao           1988-04-25
      subroutine aaddkd ( a, k, b, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision	k	!i: Constant to add
      double precision	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) + k
      enddo
      end
C AADDKI -- Adds integer constant to integer vector to make second B=A+K
C   a j penny                 dao           1988-04-25
      subroutine aaddki ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: input vector
      integer	k	!i: Constant to add
      integer	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) + k
      enddo
      end
C AADDKR -- Adds real constant to real vector to make second B+A+K
C   a j penny                 dao           1988-04-25
      subroutine aaddkr ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: input vector
      real	k	!i: Constant to add
      real	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) + k
      enddo
      end
C AADDKS -- Adds short integer constant to vector to make second B+A+K
C   a j penny                 dao           1988-04-25
      subroutine aaddks ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer*2	a(n)	!i: input vector
      integer*2	k	!i: Constant to add
      integer*2	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) + k
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AAVGD -- Calcs the mean and standard deviation of a double precision array
C
C   a j penny                 dao           1988-04-25

      subroutine aavgd ( a, n, av, sd )
      implicit none
      integer		n	!i: no of elements in vector
      double precision 	a(n)	!i: input vector
      double precision 	av	!o: Mean
      double precision 	sd	!o: Standard deviation
C--
      integer j
Cbegin
      double precision ds, dss, dn

      av = 0.0d0
      sd = 0.0d0
      if ( n.lt.1 ) return

      ds = 0.0d0
      dss = 0.0d0
      do j = 1, n
         ds = ds + a(j)
         dss = dss + a(j)*a(j)
      enddo

      dn = dble(n)
      av = ds/dn
      if ( n.gt.1 ) then
         dss = ( dss-ds*ds/dn)/(dn-1.0d0)
         if ( dss.gt.1.0d-20 ) sd = dsqrt(dss)
      endif
      end
C AAVGI -- Calcs the mean and standard deviation of an integer array
      subroutine aavgi ( a, n, av, sd )
      implicit none
      integer	n	!i: no of elements in vector
      integer	a(n)	!i: input vector
      real	av	!o: Mean
      real	sd	!o: Standard deviation
C--
      integer j
      double precision ds, dss, dn
      real rv
Cbegin
      av = 0.0
      sd = 0.0
      if ( n.lt.1 ) return

      ds = 0.0
      dss = 0.0
      do j = 1, n
         rv = real(a(j))
         ds = ds + rv
         dss = dss + rv*rv
      enddo

      dn = dble(n)
      av = ds/dn
      if ( n.gt.1 ) then
         dss = ( dss-ds*ds/dn)/(dn-1.0d0)
         if ( dss.gt.1.0d-20 ) sd = sqrt(dss)
      endif
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AAVGR -- Calcs the mean and standard deviation of a real array
      subroutine aavgr ( a, n, av, sd )
      implicit none
      integer	n	!i: no of elements in vector
      real	a(n)	!i: input vector
      real	av	!o: Mean
      real	sd	!o: Standard deviation
C--
      integer j
      double precision ds, dss, dn
Cbegin
      av = 0.0
      sd = 0.0
      if ( n.lt.1 ) return

      ds = 0.0
      dss = 0.0
      do j = 1, n
         ds = ds + a(j)
         dss = dss + a(j)*a(j)
      enddo

      dn = dble(n)
      av = ds/dn
      if ( n.gt.1 ) then
         dss = ( dss-ds*ds/dn)/(dn-1.0d0)
         if ( dss.gt.1.0d-20 ) sd = sqrt(dss)
      endif
      end
C AAVGS -- Calcs the mean and standard deviation of a short integer array
      subroutine aavgs ( a, n, av, sd )
      implicit none
      integer	n	!i: no of elements in vector
      integer*2 a(n)	!i: input vector
      real	av	!o: Mean
      real	sd	!o: Standard deviation
C--
      integer j
      double precision ds, dss, dn
      real rv
Cbegin
      av = 0.0
      sd = 0.0
      if ( n.lt.1 ) return

      ds = 0.0
      dss = 0.0
      do j = 1, n
         rv = real(a(j))
         ds = ds + rv
         dss = dss + rv*rv
      enddo

      dn = dble(n)
      av = ds/dn
      if ( n.gt.1 ) then
         dss = ( dss-ds*ds/dn)/(dn-1.0d0)
         if ( dss.gt.1.0d-20 ) sd = sqrt(dss)
      endif
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTID -- Load an integer array into a double precision array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtid ( a, b, n )
      implicit none
      integer            n	!i: No of points
      integer            a(n)	!i: input array
      double precision   b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTIR -- Load an integer array into a real array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtir ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer   a(n)	!i: input array
      real	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTIS -- Load an integer array into a short integer array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtis ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer   a(n)	!i: input array
      integer*2	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = min(max(a(j),-32768),32767)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTRD -- Load a real array into an double precision array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtrd ( a, b, n )
      implicit none
      integer       	n	!i: No of points
      real		a(n)	!i: input array
      double precision  b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTSI -- Load a short integer array into an integer array
C
C   a j penny                    dao	         1988-05-16

      subroutine achtsi ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHTSR -- Load a short integer array into a real aarray
C
C   a j penny                    dao	         1988-05-16

      subroutine achtsr ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      real 	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end
C ACHTSD -- Load a short integer array into a doubel precision aarray
C
C   a j penny                    dao	         1988-05-16

      subroutine achtsd ( a, b, n )
      implicit none
      integer           n	!i: No of points
      integer*2         a(n)	!i: input array
      double precision  b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHVALI -- Change a particular value in elements in an integer vector
C
C alan penny                    ral            1991 March

      subroutine achvali ( a, b, c, n )

      implicit none
      integer	n		!i: No of elements
      integer   a(n)		!i/o: Vector
      integer   b		!i: Value to change
      integer   c		!i: Value to change to
C--
      integer k
Cbegin

      do k = 1, n
         if ( a(k).eq.b ) a(k) = c
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHVALR -- Change a particular value in elements in a real vector
C
C alan penny                    ral            1991 March

      subroutine achvalr ( a, b, c, n )

      implicit none
      integer	n		!i: No of elements
      real      a(n)		!i/o: Vector
      real	b		!i: Value to change
      real	c		!i: Value to change to
C--
      integer k
Cbegin

      do k = 1, n
         if ( a(k).eq.b ) a(k) = c
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACHVALS -- Change a particular value in elements in an int*2 vector
C
C alan penny                    ral            1991 March

      subroutine achvals ( a, b, c, n )

      implicit none
      integer	 n		!i: No of elements
      integer*2  a(n)		!i/o: Vector
      integer*2  b		!i: Value to change
      integer*2  c		!i: Value to change to
C--
      integer k
Cbegin

      do k = 1, n
         if ( a(k).eq.b ) a(k) = c
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADIVD -- Divide array by another - double precision C=A/B
C   a j penny                    dao	         1988-05-16
      subroutine adivd ( a, b, c, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: input array
      double precision	b(n)	!i: dividing array
      double precision	c(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)/b(j)
      enddo
      end
C ADIVI -- Divide array by another - integer C=A/B
C   a j penny                    dao	         1988-05-16
      subroutine adivi ( a, b, c, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!i: input array
      integer	b(n)	!i: dividing array
      integer	c(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)/b(j)
      enddo
      end
C ADIVR -- Divide array by another - real C=A/B
C   a j penny                    dao	         1988-05-16
      subroutine adivr ( a, b, c, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: input array
      real	b(n)	!i: dividing array
      real 	c(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)/b(j)
      enddo
      end
C ADIVS -- Divide array by another - short integer C=A/B
C   a j penny                    dao	         1988-05-16
      subroutine adivs ( a, b, c, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer*2	b(n)	!i: dividing array
      integer*2	c(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)/b(j)
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADIVKD -- Divides array by constant - double precision B=A/K
C   a j penny                    dao	         1988-05-16
      subroutine adivkd ( a, k, b, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: input array
      double precision	k	!i: dividing constant
      double precision	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)/k
      enddo
      end
C ADIVKI -- Divides array by constant - integer B=A/K
C   a j penny                    dao	         1988-05-16
      subroutine adivki ( a, k, b, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!i: input array
      integer	k	!i: dividing constant
      integer	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)/k
      enddo
      end
C ADIVKR -- Divides array by constant - real B=A/K
C   a j penny                    dao	         1988-05-16
      subroutine adivkr ( a, k, b, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: input array
      real	k	!i: dividing constant
      real 	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)/k
      enddo
      end
C ADIVKS -- Divides array by constant - short integer B=A/K
C   a j penny                    dao	         1988-05-16
      subroutine adivks ( a, k, b, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer*2	k	!i: dividing constant
      integer*2	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)/k
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALIMD -- Finds the min and max of a double precision vector
C
C   a j penny                 dao           1988-04-25

      subroutine alimd ( a, n, rmin, rmax )
      implicit none
      integer		n	!i: no of elements to useadd
      double precision	a(n)	!i: input vector
      double precision	rmin	!o: minimum
      double precision	rmax	!o: maximum
C--
      integer j
Cbegin
      rmin = a(1)
      rmax = a(1)
      do j = 1, n
         rmin = min(rmin,a(j))
         rmax = max(rmax,a(j))
      enddo
      end
C ALIMI -- Finds the min and max of an integer vector
      subroutine alimi ( a, n, rmin, rmax )
      implicit none
      integer	n	!i: no of elements to useadd
      integer	a(n)	!i: input vector
      integer	rmin	!o: minimum
      integer	rmax	!o: maximum
C--
      integer j
Cbegin
      rmin = a(1)
      rmax = a(1)
      do j = 1, n
         rmin = min(rmin,a(j))
         rmax = max(rmax,a(j))
      enddo
      end
C ALIMR -- Finds the min and max of a real vector
      subroutine alimr ( a, n, rmin, rmax )
      implicit none
      integer	n	!i: no of elements to useadd
      real	a(n)	!i: input vector
      real	rmin	!o: minimum
      real	rmax	!o: maximum
C--
      integer j
Cbegin
      rmin = a(1)
      rmax = a(1)
      do j = 1, n
         rmin = min(rmin,a(j))
         rmax = max(rmax,a(j))
      enddo
      end
C ALIMS -- Finds the min and max of a short integer vector
      subroutine alims ( a, n, rmin, rmax )
      implicit none
      integer	n	!i: no of elements to useadd
      integer*2	a(n)	!i: input vector
      integer*2	rmin	!o: minimum
      integer*2	rmax	!o: maximum
C--
      integer j
Cbegin
      rmin = a(1)
      rmax = a(1)
      do j = 1, n
         rmin = min(rmin,a(j))
         rmax = max(rmax,a(j))
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMEDIANI -- Median of integer vector
C
C   alan penny             ral         1994 Nov

      subroutine amediani ( a, n, rm )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: Data size
      integer   a(n)		!i/o: Data (sorted on exit)
      real      rm		!o: Median
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      rm = 0.0						!Defaults

      if ( n.lt.1 ) return

      if ( n.eq.1 ) then
         rm = a(1)
         return
      endif

      call sort1i ( a, n )
      if ( (2*(n/2)).eq.n ) then
         k = (n/2)
         rm = (real(a(k))+real(a(k+1)))/2.0
      else
         k = (n/2) + 1
         rm = a(k)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMEDIANR -- Median of real vector
C
C   alan penny             ral         1994 Nov

      subroutine amedianr ( a, n, rm )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: Data size
      real      a(n)		!i/o: Data (sorted on exit)
      real      rm		!o: Median
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      rm = 0.0						!Defaults

      if ( n.lt.1 ) return

      if ( n.eq.1 ) then
         rm = a(1)
         return
      endif

      call sort1r ( a, n )
      if ( (2*(n/2)).eq.n ) then
         k = (n/2)
         rm = (a(k)+a(k+1))/2.0
      else
         k = (n/2) + 1
         rm = a(k)
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMEDIANS -- Median of short vector
C
C   alan penny             ral         1994 Nov

      subroutine amedians ( a, n, rm )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: Data size
      integer*2 a(n)		!i/o: Data (sorted on exit)
      real      rm		!o: Median
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      rm = 0.0						!Defaults

      if ( n.lt.1 ) return

      if ( n.eq.1 ) then
         rm = a(1)
         return
      endif

      call sort1i ( a, n )
      if ( (2*(n/2)).eq.n ) then
         k = (n/2)
         rm = (real(a(k))+real(a(k+1)))/2.0
      else
         k = (n/2) + 1
         rm = a(k)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMOVB -- Load a boolean array into another  B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovb ( a, b, n )
      implicit none
      integer	n	!i: No of points
      logical	a(n)	!i: input array
      logical	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo
      end
C AMOVD -- Load a double precision array into another  B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovd ( a, b, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: input array
      double precision	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo
      end
C AMOVI -- Moves an integer array to another B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovi ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!i: Input array
      integer 	b(n)	!o: Output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo
      end
C AMOVR -- Load a real array into a real array  B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovr ( a, b, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: input array
      real	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo
      end
C AMOVS -- Load a short integer array into another  B = A
C   a j penny                    dao	         1988-05-16
      subroutine amovs ( a, b, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer*2	b(n)	!o: output array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j)
      enddo
      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMOVKB -- Load a constant into a logical array  A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovkb ( k, a, n )
      implicit none
      logical 	k 	!i: constant to load
      integer	n	!i: No of points
      logical 	a(n)	!o: Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = k
      enddo
      end
C AMOVKD -- Load a constant into a double precision array A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovkd ( dk, a, n )
      implicit none
      double precision	dk 	!i: constant to load
      integer		n	!i: No of points
      double precision	a(n)	!o: Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = dk
      enddo
      end
C AMOVKI -- Load a constant into a integer array  A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovki ( k, a, n )
      implicit none
      integer	k 	!i: constant to load
      integer	n	!i: No of points
      integer	a(n)	!o: Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = k
      enddo
      end
C AMOVKR -- Load a constant into a real array  A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovkr ( rk, a, n )
      implicit none
      real	rk	!i: constant to load
      integer	n	!i: No of points
      real	a(n)	!o: Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = rk
      enddo
      end
C AMOVKS -- Load an integer constant into a short integer array   A = K
C   a j penny                    dao	         1988-05-16
      subroutine amovks ( k, a, n )
      implicit none
      integer*2	k 	!i: constant to load
      integer	n	!i: No of points
      integer*2	a(n)	!o: Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = k
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMULD -- Multiplies two double precision arrays C = A.B
C   a j penny                    dao	         1988-05-16
      subroutine amuld ( a, b, c, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: Input 1 Array
      double precision	b(n)	!i: Input 2 Array
      double precision	c(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)*b(j)
      enddo
      end
C AMULI -- Multiplies two integer arrays C = A.B
C   a j penny                    dao	         1988-05-16
      subroutine amuli ( a, b, c, n )
      implicit none
      integer	n	!i: No of points
      integer 	a(n)	!i: Input 1 Array
      integer	b(n)	!i: Input 2 Array
      integer	c(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)*b(j)
      enddo
      end
C AMULR -- Multiplies two real arrays C = A.B
C   a j penny                    dao	         1988-05-16
      subroutine amulr ( a, b, c, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: Input 1 Array
      real	b(n)	!i: Input 2 Array
      real	c(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)*b(j)
      enddo
      end
C AMULS -- Multiplies two short integer arrays C = A.B
C   a j penny                    dao	         1988-05-16
      subroutine amuls ( a, b, c, n )
      implicit none
      integer		n	!i: No of points
      integer*2		a(n)	!i: Input 1 Array
      integer*2		b(n)	!i: Input 2 Array
      integer*2		c(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j)*b(j)
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMULKD -- Multiplies a double precision array by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine amulkd ( a, dk, b, n )
      implicit none
      double precision	dk 	!i: constant to load
      integer		n	!i: No of points
      double precision	a(n)	!i: Input Array
      double precision	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = dk*a(j)
      enddo
      end
C AMULKI -- Multiplies an integer array by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine amulki ( a, k, b, n )
      implicit none
      integer	k 	!i: constant to load
      integer	n	!i: No of points
      integer	a(n)	!i: Input Array
      integer	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = k*a(j)
      enddo
      end
C AMULKR -- Multiplies a real array by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine amulkr ( a, rk, b, n )
      implicit none
      real	rk 	!i: constant to load
      integer	n	!i: No of points
      real	a(n)	!i: Input Array
      real	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = rk*a(j)
      enddo
      end
C AMULKS -- Multiplies a short integer array by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine amulks ( a, k, b, n )
      implicit none
      integer*2		k 	!i: constant to load
      integer		n	!i: No of points
      integer*2		a(n)	!i: Input Array
      integer*2		b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = k*a(j)
      enddo
      end

C ANUMINCI -- Load increasing integer numbers (1,2,3,..) into array
C   a j penny                    ral           1994 Nov
      subroutine anuminci ( a, n )
      implicit none
      integer		n	!i: No of points
      integer		a(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = j
      enddo
      end

C ANUMINCR -- Load increasing real numbers (1,2,3,..) into array
C   a j penny                    ral           1994 Nov
      subroutine anumincr ( a, n )
      implicit none
      integer		n	!i: No of points
      real		a(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         a(j) = j
      enddo
      end

C ANUMINCS -- Load increasing short numbers (1,2,3,..) into array
C   a j penny                    ral           1994 Nov
      subroutine anumincs ( a, n )
      implicit none
      integer		n	!i: No of points
      integer*2		a(n)	!o: Output Array
C--
      integer j
      integer*2 k
Cbegin
      do j = 1, n
         k = min(j,32767)
         a(j) = k
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUBD -- Subtracts a double precision vector from another vector C = A - B
C
C   a j penny                 dao           1988-04-25

      subroutine asubd ( a, b, c, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision 	a(n)	!i: 1st input vector
      double precision 	b(n)	!i: 2nd input vector to subtract
      double precision 	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) - b(j)
      enddo
      end
C ASUBI -- Subtracts an integer vector from another vector C = A - B
      subroutine asubi ( a, b, c, n )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: 1st input vector
      integer	b(n)	!i: 2nd input vector to subtract
      integer	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) - b(j)
      enddo
      end
C ASUBR -- Subtracts a real vector from another vector C = A - B
      subroutine asubr ( a, b, c, n )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: 1st input vector
      real	b(n)	!i: 2nd input vector to subtract
      real	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) - b(j)
      enddo
      end
C ASUBS -- Subtracts a short integer vector from another vector C = A - B
      subroutine asubs ( a, b, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer*2		a(n)	!i: 1st input vector
      integer*2		b(n)	!i: 2nd input vector to subtract
      integer*2		c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         c(j) = a(j) - b(j)
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUBKD -- Subtracts a constant from a double precision vector B = A - K
C
C   a j penny                 dao           1988-04-25

      subroutine asubkd ( a, k, b, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision 	a(n)	!i: input vector
      double precision 	k	!i: constant to subtract
      double precision 	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) - k
      enddo
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUBKI -- Subtracts a constant from an integer vector B = A - K
      subroutine asubki ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: input vector
      integer	k	!i: constant to subtract
      integer	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) - k
      enddo
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUBKR -- Subtracts a constant from a real vector B = A - K
      subroutine asubkr ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: input vector
      real	k	!i: constant to subtract
      real	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) - k
      enddo
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUBKS -- Subtracts a constant from a short integer vector B = A - K
      subroutine asubks ( a, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer*2	a(n)	!i: input vector
      integer*2	k	!i: constant to subtract
      integer*2	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         b(j) = a(j) - k
      enddo
      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUMD -- Sums a double precision vector
C
C   a j penny                 dao           1988-04-25

      subroutine asumd ( a, n, sum )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision	sum 	!o: sum
C--
      integer j
Cbegin
      sum = 0.0d0
      do j = 1, n
         sum = sum + a(j)
      enddo
      end
C ASUMI -- Sums an integer vector
      subroutine asumi ( a, n, sum )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: input vector
      real	sum 	!o: sum
C--
      double precision dsum
      integer j
Cbegin
      dsum = 0.0d0
      do j = 1, n
         dsum = dsum + dble(a(j))
      enddo
      sum = dsum
      end
C ASUMR -- Sums a real vector
      subroutine asumr ( a, n, sum )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: input vector
      real 	sum 	!o: sum
C--
      double precision dsum
      integer j
Cbegin
      dsum = 0.0d0
      do j = 1, n
         dsum = dsum + dble(a(j))
      enddo
      sum = dsum
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ASUMS -- Sums a short integer vector
      subroutine asums ( a, n, sum )
      implicit none
      integer	n	!i: no of elements to add
      integer*2	a(n)	!i: input vector
      real	sum 	!o: sum
C--
      double precision dsum
      integer j
Cbegin
      dsum = 0.0d0
      do j = 1, n
         dsum = dsum + dble(a(j))
      enddo
      sum = dsum
      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMZEROB -- Load .FALSE. into a logical array  A = .FALSE.
C   a j penny                    dao	         1988-05-16
      subroutine azerob ( a, n )
      implicit none
      integer	n	!i: No of points
      logical 	a(n)	!o: Array
C--
      integer j
      logical zero
      parameter ( zero=.false. )
Cbegin
      do j = 1, n
         a(j) = zero
      enddo
      end
C AZEROD -- Load zeroes into a double precision array A = 0.0d0
C   a j penny                    dao	         1988-05-16
      subroutine azerod ( a, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!o: Array
C--
      integer j
      double precision zero
      parameter ( zero=0.0d0 )
Cbegin
      do j = 1, n
         a(j) = zero
      enddo
      end
C AZEROI -- Load zeroes into a integer array  A = 0
C   a j penny                    dao	         1988-05-16
      subroutine azeroi ( a, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!o: Array
C--
      integer j
      integer zero
      parameter ( zero=0 )
Cbegin
      do j = 1, n
         a(j) = zero
      enddo
      end
C AZEROR -- Load zeroes into a real array  A = 0.0
C   a j penny                    dao	         1988-05-16
      subroutine azeror ( a, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!o: Array
C--
      integer j
      real zero
      parameter ( zero=0.0 )
Cbegin
      do j = 1, n
         a(j) = zero
      enddo
      end
C AZEROS -- Load zeros into a short integer array   A = 0
C   a j penny                    dao	         1988-05-16
      subroutine azeros ( a, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!o: Array
C--
      integer j
      integer*2 zero
      parameter ( zero=0 )
Cbegin
      do j = 1, n
         a(j) = zero
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      This is STARFLIBVZ.F
C
C  It contains vector operations with magic values.
C
C AZADDK(DIRS)       Add a constant from a magic vector B = A + K
C AZSUBK(DIRS)       Subtract a constant from a magic vector B = A - K
C AZMULK(DIRS)       Multiply a magic array by a constant B = A.K
C AZDIVK(DIRS)       Divide a magic array by a constant B = A/K
C AZADD(DIRS)        Add one vector from another with magic values C = A + B
C AZSUB(DIRS)        Subtract one vector from another with magic values C = A - B
C AZMUL(DIRS)        Multiply one vector from another with magic values C = A*B
C AZDIV(DIRS)        Divide one vector from another with magic values C = A/B
C AZCHT(IRS:DIRS)    Load a (real:int:short) array into a (DRIS) array with magic values
C AZLIM(DIRS)        Find the min and max of a vector with magic values
C AZZALLBAD(RS)      Check for some ok in an image box with magic values
C


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZADDKD -- Adds dble prec constant to magic vector to make second B=A+K
C   a j penny                 dao           1988-04-25
      subroutine azaddkd ( a, ak, k, b, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	k	!i: Constant to add
      double precision	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) + k
      enddo
      end
C AZADDKI -- Adds integer constant to integer magic vector to make second B=A+K
C   a j penny                 dao           1988-04-25
      subroutine azaddki ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer	a(n)	!i: input vector
      integer   ak      !i: Magic value
      integer	k	!i: Constant to add
      integer	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) + k
      enddo
      end
C AZADDKR -- Adds real constant to real magic vector to make second B+A+K
C   a j penny                 dao           1988-04-25
      subroutine azaddkr ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      real	a(n)	!i: input vector
      real      ak      !i: Magic value
      real	k	!i: Constant to add
      real	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) + k
      enddo
      end
C AZADDKS -- Adds short integer constant to magic vector to make second B+A+K
C   a j penny                 dao           1988-04-25
      subroutine azaddks ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to add
      integer*2	a(n)	!i: input vector
      integer*2 ak      !i: Magic value
      integer*2	k	!i: Constant to add
      integer*2	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) + k
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZSUBKD -- Subtracts dble prec constant to magic vector to make second B=A-K
C   a j penny                 dao           1988-04-25
      subroutine azsubkd ( a, ak, k, b, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	k	!i: Constant to add
      double precision	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) - k
      enddo
      end
C AZSUBKI -- Subtracts integer constant to magic vector to make second B=A-K
C   a j penny                 dao           1988-04-25
      subroutine azsubki ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to operate one
      integer	a(n)	!i: input vector
      integer   ak      !i: Magic value
      integer	k	!i: Constant to subtract
      integer	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) - k
      enddo
      end
C AZSUBKR -- Subtracts real constant to magic vector to make second B=A-K
C   a j penny                 dao           1988-04-25
      subroutine azsubkr ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to oeprate on
      real	a(n)	!i: input vector
      real      ak      !i: Magic value
      real	k	!i: Constant to subtract
      real	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) - k
      enddo
      end
C AZSUBKS -- Subtracts short integer constant to magic vector to make second B=A-K
C   a j penny                 dao           1988-04-25
      subroutine azsubks ( a, ak, k, b, n )
      implicit none
      integer	n	!i: no of elements to operate on
      integer*2	a(n)	!i: input vector
      integer*2 ak      !i: Magic value
      integer*2	k	!i: Constant to subtract
      integer*2	b(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j) - k
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZMULKD -- Multiplies a double precision magic vector by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine azmulkd ( a, ak, dk, b, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: Input Array
      double precision	ak 	!i: Magic value
      double precision	dk 	!i: constant to multiply by
      double precision	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = dk*a(j)
      enddo
      end
C AZMULKI -- Multiplies an integer magic vector by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine azmulki ( a, ak, k, b, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!i: Input Array
      integer   ak 	!i: Magic value
      integer	k 	!i: constant to multiply by
      integer	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = k*a(j)
      enddo
      end
C AZMULKR -- Multiplies a real magic vector by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine azmulkr ( a, ak, rk, b, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: Input Array
      real      ak 	!i: Magic value
      real	rk 	!i: constant to multiply by
      real	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = rk*a(j)
      enddo
      end
C AZMULKS -- Multiplies a short integer array by a constant B = A.K
C   a j penny                    dao	         1988-05-16
      subroutine azmulks ( a, ak, k, b, n )
      implicit none
      integer		n	!i: No of points
      integer*2		a(n)	!i: Input Array
      integer*2		ak 	!i: Magic value
      integer*2		k 	!i: constant to multiply by
      integer*2		b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = k*a(j)
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZDIVKD -- Divides a double precision magic vector by a constant B = A/K
C   a j penny                    dao	         1988-05-16
      subroutine azdivkd ( a, ak, dk, b, n )
      implicit none
      integer		n	!i: No of points
      double precision	a(n)	!i: Input Array
      double precision	ak 	!i: Magic value
      double precision	dk 	!i: constant to divide by
      double precision	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j)/dk
      enddo
      end
C AZDIVKI -- Divides an integer magic vector by a constant B = A/K
C   a j penny                    dao	         1988-05-16
      subroutine azdivki ( a, ak, k, b, n )
      implicit none
      integer	n	!i: No of points
      integer	a(n)	!i: Input Array
      integer   ak 	!i: Magic value
      integer	k 	!i: constant to didvide by
      integer	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j)/k
      enddo
      end
C AZDIVKR -- Divides a real magic vector by a constant B = A/K
C   a j penny                    dao	         1988-05-16
      subroutine azdivkr ( a, ak, rk, b, n )
      implicit none
      integer	n	!i: No of points
      real	a(n)	!i: Input Array
      real      ak 	!i: Magic value
      real	rk 	!i: constant to divide by
      real	b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j)/rk
      enddo
      end
C AZDIVKS -- Divides a short integer array by a constant B = A/K
C   a j penny                    dao	         1988-05-16
      subroutine azdivks ( a, ak, k, b, n )
      implicit none
      integer		n	!i: No of points
      integer*2		a(n)	!i: Input Array
      integer*2		ak 	!i: Magic value
      integer*2		k 	!i: constant to divide by
      integer*2		b(n)	!o: Output Array
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak ) b(j) = a(j)/k
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZSUBD -- Subtract dble prec vector from another with magic values C = A - B
C   a j penny                 dao           1988-04-25
      subroutine azsubd ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	b(n)	!i: input vector
      double precision  bk      !i: Magic value
      double precision	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) - b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZSUBI -- Subtract integer vector from another with magic values C = A - B
C   a j penny                 dao           1988-04-25
      subroutine azsubi ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer           a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer           b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer           c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) - b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZSUBR -- Subtract real vector from another with magic values C = A - B
C   a j penny                 dao           1988-04-25
      subroutine azsubr ( a, ak, b, bk, c, n )
      integer		n	!i: no of elements to add
      real              a(n)	!i: input vector
      integer           ak      !i: Magic value
      real              b(n)	!i: input vector
      integer           bk      !i: Magic value
      real              c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) - b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZSUBS -- Subtract short vector from another with magic values C = A - B
C   a j penny                 dao           1988-04-25
      subroutine azsubs ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer*2         a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer*2         b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer*2         c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) - b(j)
         else
            c(j) = ak
         endif
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZADDD -- Add dble prec vector from another with magic values C = A + B
C   a j penny                 dao           1988-04-25
      subroutine azaddd ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	b(n)	!i: input vector
      double precision  bk      !i: Magic value
      double precision	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) + b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZADDI -- Add integer vector from another with magic values C = A + B
C   a j penny                 dao           1988-04-25
      subroutine azaddi ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer           a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer           b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer           c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) + b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZADDR -- Add real vector from another with magic values C = A + B
C   a j penny                 dao           1988-04-25
      subroutine azaddr ( a, ak, b, bk, c, n )
      integer		n	!i: no of elements to add
      real              a(n)	!i: input vector
      integer           ak      !i: Magic value
      real              b(n)	!i: input vector
      integer           bk      !i: Magic value
      real              c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) + b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZADDS -- Add short vector from another with magic values C = A + B
C   a j penny                 dao           1988-04-25
      subroutine azadds ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer*2         a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer*2         b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer*2         c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j) + b(j)
         else
            c(j) = ak
         endif
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZMULD -- Multiply dble prec vector from another with magic values C = A * B
C   a j penny                 dao           1988-04-25
      subroutine azmuld ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	b(n)	!i: input vector
      double precision  bk      !i: Magic value
      double precision	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j)*b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZMULI -- Multiply integer vector from another with magic values C = A * B
C   a j penny                 dao           1988-04-25
      subroutine azmuli ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer           a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer           b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer           c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j)*b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZMULR -- Multiply real vector from another with magic values C = A * B
C   a j penny                 dao           1988-04-25
      subroutine azmulr ( a, ak, b, bk, c, n )
      integer		n	!i: no of elements to add
      real              a(n)	!i: input vector
      integer           ak      !i: Magic value
      real              b(n)	!i: input vector
      integer           bk      !i: Magic value
      real              c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j)*b(j)
         else
            c(j) = ak
         endif
      enddo
      end
C AZMULS -- Multiply short vector from another with magic values C = A * B
C   a j penny                 dao           1988-04-25
      subroutine azmuls ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer*2         a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer*2         b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer*2         c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            c(j) = a(j)*b(j)
         else
            c(j) = ak
         endif
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZDIVD -- Divide dble prec vector from another with magic values C = A / B
C   a j penny                 dao           1988-04-25
      subroutine azdivd ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      double precision	a(n)	!i: input vector
      double precision  ak      !i: Magic value
      double precision	b(n)	!i: input vector
      double precision  bk      !i: Magic value
      double precision	c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            if ( b(j).ne.0d0 ) then
               c(j) = a(j)/b(j)
            else
               c(j) = ak
            endif
         else
            c(j) = ak
         endif
      enddo
      end
C AZDIVI -- Divide integer vector from another with magic values C = A / B
C   a j penny                 dao           1988-04-25
      subroutine azdivi ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer           a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer           b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer           c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            if ( b(j).ne.0 ) then
               c(j) = a(j)/b(j)
            else
               c(j) = ak
            endif
         else
            c(j) = ak
         endif
      enddo
      end
C AZDIVR -- Subtract real vector from another with magic values C = A / B
C   a j penny                 dao           1988-04-25
      subroutine azdivr ( a, ak, b, bk, c, n )
      integer		n	!i: no of elements to add
      real              a(n)	!i: input vector
      integer           ak      !i: Magic value
      real              b(n)	!i: input vector
      integer           bk      !i: Magic value
      real              c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            if ( b(j).ne.0.0 ) then
               c(j) = a(j)/b(j)
            else
               c(j) = ak
            endif
         else
            c(j) = ak
         endif
      enddo
      end
C AZDIVS -- Divide short vector from another with magic values C = A / B
C   a j penny                 dao           1988-04-25
      subroutine azdivs ( a, ak, b, bk, c, n )
      implicit none
      integer		n	!i: no of elements to add
      integer*2         a(n)	!i: input vector
      integer           ak      !i: Magic value
      integer*2         b(n)	!i: input vector
      integer           bk      !i: Magic value
      integer*2         c(n)	!o: output vector
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).ne.ak .and. b(j).ne.bk ) then
            if ( b(j).ne.0 ) then
               c(j) = a(j)/b(j)
            else
               c(j) = ak
            endif
         else
            c(j) = ak
         endif
      enddo
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTIR -- Load an integer array into a real array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtir ( a, ka, b, kb, n )
      implicit none
      integer	n	!i: No of points
      integer   a(n)	!i: input array
      integer   ka	!i: input magic value
      real	b(n)	!o: output array
      real      kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTID -- Load an integer array into a double precisionn array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtid ( a, ka, b, kb, n )
      implicit none
      integer           n	!i: No of points
      integer           a(n)	!i: input array
      integer		ka	!i: input magic value
      double precision 	b(n)	!o: output array
      double precision  kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTIS -- Load an integer array into a short integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtis ( a, ka, b, kb, n )
      implicit none
      integer	n	!i: No of points
      integer   a(n)	!i: input array
      integer   ka	!i: input magic value
      integer*2	b(n)	!o: output array
      integer*2 kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka .or. a(j).lt.-32768 .or. a(j).gt.32767 ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTRD -- Load a real array into an double precision array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtrd ( a, ka, b, kb, n )
      implicit none
      integer       	n	!i: No of points
      real		a(n)	!i: input array
      real              ka	!i: Input magic value
      double precision  b(n)	!o: output array
      double precision  kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTRI -- Load a real array into an integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtri ( a, ka, min, max, b, kb, n )
      implicit none
      integer       	n	!i: No of points
      real		a(n)	!i: input array
      real              ka	!i: Input magic value
      real              min	!i: Input max valid value
      real              max	!i: Input max valid value
      integer           b(n)	!o: output array
      integer           kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( (a(j).eq.ka) .or. (a(j).lt.min) .or. (a(j).gt.max) ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTRS -- Load a real array into an integer*2 array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtrs ( a, ka, min, max, b, kb, n )
      implicit none
      integer       	n	!i: No of points
      real		a(n)	!i: input array
      real              ka	!i: Input magic value
      real              min	!i: Input max valid value
      real              max	!i: Input max valid value
      integer*2         b(n)	!o: output array
      integer*2         kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( (a(j).eq.ka) .or. (a(j).lt.min) .or. (a(j).gt.max) ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTSI -- Load a short integer array into an integer array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtsi ( a, ka, b, kb, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer*2 ka	!i: Input magic value
      integer	b(n)	!o: output array
      integer   kb	!i: Output magic value
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTSR -- Load a short integer array into a real array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtsr ( a, ka, b, kb, n )
      implicit none
      integer	n	!i: No of points
      integer*2	a(n)	!i: input array
      integer*2 ka	!i: Magic input flag
      real 	b(n)	!o: output array
      real      kb	!o: Magic output flag
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZCHTSD -- Load a short integer array into a doubel precision array with magic values
C
C   a j penny                    dao	         1988-05-16

      subroutine azchtsd ( a, ka, b, kb, n )
      implicit none
      integer            n	!i: No of points
      integer*2	         a(n)	!i: input array
      integer*2          ka	!i: Magic input flag
      double precision   b(n)	!o: output array
      double precision   kb	!o: Magic output flag
C--
      integer j
Cbegin
      do j = 1, n
         if ( a(j).eq.ka ) then
            b(j) = kb
         else
            b(j) = a(j)
         endif
      enddo

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZLIMD -- Finds the min and max of an dble prec vector with magic values
      subroutine azlimd ( a, n, rinval, rmin, rmax, ierr )
      implicit none
      integer            n       !i: no of elements to useadd
      double precision   a(n)    !i: input vector
      double precision   rinval  !i: Magic value
      double precision   rmin    !o: minimum
      double precision   rmax    !o: maximum
      integer            ierr    !o: Error flag 0=ok; 1=no good points
C--
      integer j
      logical found
Cbegin
      found = .false.
      do j = 1, n
         if ( a(j).ne.rinval ) then
            if ( .not.found ) then
               rmin = a(j)
               rmax = a(j)
               found = .true.
            else
               rmin = min(rmin,a(j))
               rmax = max(rmax,a(j))
            endif
         endif
      enddo
      ierr = 0
      if ( .not.found ) then
         ierr = 1
         rmin = 0.0d0
         rmax = 0.0d0
      endif
      end
C AZLIMI -- Finds the min and max of an integer vector with magic values
      subroutine azlimi ( a, n, rinval, rmin, rmax, ierr )
      implicit none
      integer   n       !i: no of elements to useadd
      integer   a(n)    !i: input vector
      integer   rinval  !i: Magic value
      integer   rmin    !o: minimum
      integer   rmax    !o: maximum
      integer   ierr    !o: Error flag 0=ok; 1=no good points
C--
      integer j
      logical found
Cbegin
      found = .false.
      do j = 1, n
         if ( a(j).ne.rinval ) then
            if ( .not.found ) then
               rmin = a(j)
               rmax = a(j)
               found = .true.
            else
               rmin = min(rmin,a(j))
               rmax = max(rmax,a(j))
            endif
         endif
      enddo
      ierr = 0
      if ( .not.found ) then
         ierr = 1
         rmin = 0
         rmax = 0
      endif
      end
C AZLIMR -- Finds the min and max of a real vector with magic values
      subroutine azlimr ( a, n, rinval, rmin, rmax, ierr )
      implicit none
      integer   n       !i: no of elements to useadd
      real      a(n)    !i: input vector
      real      rinval  !i: Magic value
      real      rmin    !o: minimum
      real      rmax    !o: maximum
      integer   ierr    !o: Error flag 0=ok; 1=no good points
C--
      integer j
      logical found
Cbegin
      found = .false.
      do j = 1, n
         if ( a(j).ne.rinval ) then
            if ( .not.found ) then
               rmin = a(j)
               rmax = a(j)
               found = .true.
            else
               rmin = min(rmin,a(j))
               rmax = max(rmax,a(j))
            endif
         endif
      enddo
      ierr = 0
      if ( .not.found ) then
         ierr = 1
         rmin = 0.0
         rmax = 0.0
      endif
      end
C AZLIMS -- Finds the min and max of a short integer vector with magic values
      subroutine azlims ( a, n, rinval, rmin, rmax, ierr )
      implicit none
      integer   n       !i: no of elements to useadd
      integer*2 a(n)    !i: input vector
      integer*2 rinval  !i: Magic value
      integer*2 rmin    !o: minimum
      integer*2 rmax    !o: maximum
      integer   ierr    !o: Error flag 0=ok; 1=no good points
C--
      integer j
      logical found
Cbegin
      found = .false.
      do j = 1, n
         if ( a(j).ne.rinval ) then
            if ( .not.found ) then
               rmin = a(j)
               rmax = a(j)
               found = .true.
            else
               rmin = min(rmin,a(j))
               rmax = max(rmax,a(j))
            endif
         endif
      enddo
      ierr = 0
      if ( .not.found ) then
         ierr = 1
         rmin = 0
         rmax = 0
      endif
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZZALLBADR -- Check for some ok in a real image box with magic values
C
C   a j penny                   stsci             86-12-02

      subroutine azzallbadr ( im, nx, ny, rinval, kxc, kyc, allbad )

      implicit none

      integer   nx, ny			!i: Image size
      real      im(nx,ny)		!i: Input image
      real      rinval			!i: Image (int*2) invalid number flag
      integer   kxc(2)			!i: X range to check
      integer   kyc(2)			!i: Y range to check
      logical   allbad			!i: No 'good' values?
C--
      integer j, k
Cbegin


      allbad = .true.
      do k = kyc(1), kyc(2)
         do j = kxc(1), kxc(2)
            if ( im(j,k).ne.rinval ) allbad = .false.
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AZZALLBADS -- Check for some ok in an int*2 image box with magic values
C
C   a j penny                   stsci             86-12-02

      subroutine azzallbads ( im, nx, ny, inval, kxc, kyc, allbad )

      implicit none

      integer   nx, ny			!i: Image size
      integer*2 im(nx,ny)		!i: Input image
      integer   inval			!i: Image (int*2) invalid number flag
      integer   kxc(2)			!i: X range to check
      integer   kyc(2)			!i: X range to check
      logical   allbad			!i: No 'good' values?
C--
      integer j, k
Cbegin


      allbad = .true.
      do k = kyc(1), kyc(2)
         do j = kxc(1), kxc(2)
            if ( im(j,k).ne.inval ) allbad = .false.
         enddo
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is STARFLIB_CHO.F
C
C    It contains s/rs for getting choices from the user:-
C
C SETUP_OPTION_S   Set up option in simple (key/panel) option choice
C SETUP_OPTION     Set up option in (key/panel) option choice
C GET_CHOICE       Get (key/panel) option choice
C CHOICE_PANEL_SW  Change between keyboard/panel input

C GET_OPTION       Get name in option name list from user chosen option name
C GET_JOB          Get number in option name list from user chosen option name

C TYPE_HCHOICE     Type out help at a program start on using the OPTION input

C CH_SETUP         Set up defaults before using choices
C GCMDLST          Squeeze command list and determine its character
C GCMDLSTA         Squeeze command list(s) and determine its/their character
C GETDEFT          Get default option from list of defaults (character)
C HX_HLOAD         Put out help panel help on chosen option  (ignores case)



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SETUP_OPTION_S -- Set up option in simple (key/panel) option choice
C
C   alan penny                      ral                    1993 Jul

      subroutine setup_option_s ( ktopt, set_num, koutside,
     +                            options, key_option, ncode )

      implicit none

      character*12  ktopt             		!o: Chosen option
      integer       set_num			!i: Arbitrary number for this set of options
						!    (must be different for different sets)
      logical       koutside			!i: Is this called from outside loop?
      character*(*) options			!i: Options list (a:b:c) (in order of appearence)
      character*(*) key_option			!i: Keyboard option name
      integer       ncode			!i: Panel number
C--
Cbegin


      call setup_option ( ktopt, set_num, koutside,
     +                    1, options, 'OPTIONS',
     +                    'List of options', key_option, ncode,
     +                    0, 1, ' ',
     +                    0, ' ',
     +                    0, ' ',
     +                    0, ' ',
     +                    0, 1, 1, ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SETUP_OPTION -- Set up option in (key/panel) option choice
C
C   alan penny                      ral                    1993 Jul

      subroutine setup_option ( ktopt, set_num, koutside,
     +                          sect_num, sect_text, sect_head,
     +                          title, option, ncode,
     +                          do_opt_text, opt_num, opt_text,
     +                          do_opt_head, opt_head,
     +                          do_opt_help, opt_help,
     +                          do_help_ext, help_num, help_text,
     +                          do_def_text, def_x, def_y, def_text )

      implicit none

      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'
      include 'ST_DS_PANEL_INC'

      character*12  ktopt             		!o: Chosen option
      integer       set_num			!i: Arbitrary number for this set of options
						!    (must be different for different sets)
      logical       koutside			!i: Is this called from outside loop?

      integer       sect_num			!i: Number of sections in panel
      character*(*) sect_head(sect_num)		!i: Headers for sections
      character*(*) sect_text(sect_num)		!i: Options in each section (in order of appearence)

      character*(*) title			!i: Title of panel
      character*(*) option			!i: Panel option name
      integer       ncode			!i: Panel number

      integer       do_opt_text			!i: Use option list? (0=no;1=yes)
      integer       opt_num			!i: Number of options (=1 if not use)
      character*(*) opt_text(opt_num)		!i: Options (= ' ' if not use)

      integer       do_opt_head			!i: Use Headers? (0=no;1=yes)
      character*(*) opt_head(opt_num)		!i: Header describing option (= ' ' if not use)

      integer       do_opt_help			!i: Use Help Box? (0=no;1=yes)
      character*(*) opt_help(6,opt_num)		!i: 6 line explanation of option
						!    (= ' ' if not use)

      integer       do_help_ext			!i: Use extra Help lines? (0=no;1=yes)
      integer       help_num			!i: Lines of extra help text (=1 if not use)
      character*(*) help_text(help_num)		!i: Extra help text (= ' ' if not use)

      integer       do_def_text			!i: Use defaults list? (0=no;1=yes)
      integer       def_x			!i: Number of default options
      integer       def_y			!i: Number of different sets of default options
      character*(*) def_text(def_x,def_y)	!i: Default options
C--
      integer j, k, ka, lens, kcon(200), jcon(200), kla(200), nmax
      integer kx, ky, knum, kb, kta(200), ktb(200)
      character*2000 texta
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( koutside ) then						!Set up defaults

         ktopt = ' '

         KLEF_X = 0
         KLEF_Y = 1
         LOOPT_HELP = .true.
         KTDEF_HELP = ' '
         TBHELP     = ' '

         CH_TITLE = ' '
         CH_OPTION = ' '
         CH_NCODE = 0

         CH_HELP_NUM = 1
         CH_HELP_TEXT(1) = ' '

         DOHPANEL = .true.
         CH_OPT_NUM = 1
         CH_OPT_TEXT(1) = ' '
         CH_OPT_HEAD(1) = ' '
         do k = 1, 6
            CH_OPT_HELP(k,1) = ' '
         enddo

         CH_SECT_NUM = 1
         CH_SECT_HEAD(1) = ' '
         CH_SECT_TEXT(1) = ' '

         CH_DEF_X = 1
         CH_DEF_Y = 1
         CH_DEF_TEXT(1,1) = ' '

      endif

      if ( set_num.eq.CH_NCODE ) return					!Check to see if needed

      if ( opt_num.gt.CH_MAX_OPT_NUM ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       OPT_NUM too large' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
           ST_FAILED = .true.
         return
      endif

      if ( opt_num.lt.1 ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       OPT_NUM less than 1' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( sect_num.gt.CH_MAX_SECT_NUM ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       SECT_NUM too large' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( def_x.gt.CH_MAX_DEF_X ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       DEF_X too large' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( def_x.lt.1 ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       DEF_X less than 1 ' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( def_y.gt.CH_MAX_DEF_Y ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       DEF_Y too large' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( def_y.lt.1 ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       DEF_Y less than 1 ' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      if ( (help_num+2+opt_num).gt.CH_MAX_HELP_NUM ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       HELP_NUM too large' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif

      kx = PNSNX/91							!Check enough space on Panel for buttons
      if ( (kx*91).ne.PNSNX ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       PNSNX not multiple of 91' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif
      ky = PNSNY/26
      if ( (ky*26).ne.PNSNY ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       PNSNY not multiple of 26' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif
      ka = 0								!Work out where the buttons end
      do k = 1, sect_num
         call gcmdlsta ( sect_text(k), texta, kb, knum, kta, ktb )
         ka = ka + 1 + knum
         if ( (ka-((ka/ky)*ky)).eq.(ky-1) ) ka = ka + 1
      enddo
      if ( ka.gt.((kx*ky)-5) ) then
         call printo ( 'ERROR: Programmer error in s/r XX_CH_SETUP' )
         call printo ( '       Not enough room in Panel for buttons')
         call printo ( '       Three gaps needed after Help button' )
         call printo ( '       Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
         ST_FAILED = .true.
         return
      endif


      CH_TITLE = title
      CH_OPTION = option
      CH_NCODE = ncode
      if ( do_opt_text.eq.0 ) DOHPANEL = .false.
      if ( do_opt_help.eq.0 .and. do_opt_head.eq.0)DOHPANEL=.false.

      CH_HELP_NUM = 2
      CH_HELP_TEXT(1) = 'Option        Function'
      CH_HELP_TEXT(2) = '------        --------'
      if ( do_opt_text.eq.0 ) then
         CH_HELP_NUM = 3
         CH_HELP_TEXT(3) = '   No -option- help available'
      else
         CH_HELP_NUM = CH_HELP_NUM + opt_num
      endif
      if ( do_help_ext.eq.1 ) CH_HELP_NUM = CH_HELP_NUM + help_num

      if ( do_opt_text.eq.1 ) then
         do k = 1, opt_num
            jcon(k) = k
         enddo
         nmax = 1
         do k = 1, opt_num
            kla(k) = lens(opt_text(k))
            nmax = max(nmax,kla(k))
         enddo
         do j = nmax, 1, -1
            do k = 1, opt_num
               ka = jcon(k)
               if ( j.gt.kla(ka) ) then
                  kcon(k) = -1
               else
                  kcon(k) = ichar(opt_text(ka)(j:j))
               endif
            enddo
            call sort2i ( kcon, jcon, opt_num )
         enddo
         ka = 2
         if ( do_opt_text.eq.0 ) ka = 3
         do k = 1, opt_num
            CH_HELP_TEXT(k+ka)(1:12) = opt_text(jcon(k))
            CH_HELP_TEXT(k+ka)(13:14) = '  '
            if ( do_opt_head.eq.1 ) then
               CH_HELP_TEXT(k+ka)(15:68) = opt_head(jcon(k))
            else
               CH_HELP_TEXT(k+ka)(15:68)='No description available'
            endif
         enddo

         CH_OPT_NUM = opt_num
         do k = 1, opt_num
            CH_OPT_TEXT(k) = opt_text(k)
            if ( do_opt_head.eq.1 ) then
               CH_OPT_HEAD(k) = opt_head(k)
            else
               CH_OPT_HEAD(k) = 'No description available'
            endif
            if ( do_opt_help.eq.1 ) then
                do j = 1, 6
                   CH_OPT_HELP(j,k) = opt_help(j,k)
                enddo
            else
                do j = 1, 6
                   CH_OPT_HELP(j,k) = ' '
                enddo
                CH_OPT_HELP(3,k) = 'No help available'
            endif
         enddo

      endif

      if ( do_help_ext.eq.1 .and. help_num.gt.0 ) then
         ka = 3
         if ( do_opt_text.eq.1 ) ka = 2 + opt_num
         do k = 1, help_num
            CH_HELP_TEXT(k+ka) = help_text(k)
         enddo
      endif

      CH_SECT_NUM = sect_num
      do k = 1, sect_num
         CH_SECT_TEXT(k) = sect_text(k)
         CH_SECT_HEAD(k) = sect_head(k)
      enddo

      if ( DOPANEL ) then						!Load panel
         call ds_p_sload ( CH_SECT_TEXT, CH_SECT_HEAD, CH_SECT_NUM,
     +                     CH_NCODE )
         call ds_p_pttit ( CH_TITLE )
         if ( DOHPANEL ) call ds_p_hx_load ( CH_TITLE )
      endif

      if ( koutside ) then
         if ( do_def_text.eq.1 ) then
            CH_DEF_X = def_x
            CH_DEF_Y = def_y
            do k = 1, def_y
               do j = 1, def_x
                 CH_DEF_TEXT(j,k) = def_text(j,k)
               enddo
            enddo
            ktopt = def_text(1,1)
         else
            CH_DEF_TEXT(1,1) = ' '
            ktopt = ' '
         endif
      endif


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET_CHOICE -- Get (key/panel) option choice
C
C   alan penny                      ral                    1993 Jul

      subroutine get_choice ( ktopt, kdohpanel )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'ST_DS_PANEL_INC'
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12    ktopt		!i/o: Option string
      integer         kdohpanel		!i: Use Help Panel? (0=no;1=yes)
C--
      integer ierr
      character*70 kd
Cbegin


      if ( ST_FAILED ) return

      call getdeft ( CH_DEF_TEXT, CH_MAX_DEF_X, CH_MAX_DEF_Y, 		!Default option
     +               CH_DEF_X, CH_DEF_Y, KLEF_X, KLEF_Y, LOOPT_HELP,
     +               ktopt )
      if ( ST_FAILED ) return
      if ( .not.LOOPT_HELP ) CH_AUTOSTART = .false.

      if ( ktopt.ne.' ' ) KTDEF_HELP = ktopt				!Choice

      kd = KTDEF_HELP
      if ( .not.CH_AUTOSTART ) then
         if ( DOPANEL ) then
            call ds_p_gsbox ( CH_SECT_TEXT, CH_SECT_HEAD, CH_SECT_NUM,
     +                        ktopt, kd, CH_HELP_TEXT, CH_HELP_NUM,
     +                        CH_OPT_NUM, CH_OPT_TEXT, CH_OPT_HEAD,
     +                        CH_OPT_HELP, CH_TITLE, DOHPANEL, ierr )
         else
            call get_option ( 'OPTION', CH_SECT_TEXT, CH_SECT_NUM,
     +                        ktopt, kd, CH_HELP_TEXT, CH_HELP_NUM )
            if ( ST_FAILED ) return
         endif
      endif
      KTDEF_HELP = kd

      if ( kdohpanel.eq.1 .and. DOHPANEL ) call hx_hload ( ktopt,
     +            CH_OPT_TEXT, CH_OPT_HEAD, CH_OPT_HELP, CH_OPT_NUM )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHOICE_PANEL_SW -- Change between keyboard/panel input
C
C  a j penny                          dao              1988-04-19

      subroutine choice_panel_sw ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'
      include 'ST_DS_PANEL_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call ds_p_switchs ( CH_TITLE, CH_SECT_TEXT, CH_SECT_HEAD,         !Change panel/keyboard inpu
     +                    CH_SECT_NUM, CH_NCODE )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET_OPTION -- Get name in option name list from user chosen option name
C  User chooses from a list of option names, this s/r returns the number of
C  that option. If 'help' entered, help given. S/r cycles until a 'correct'
C  reply is given.
C
C   alan penny               ral                       1990 Jan

      subroutine get_option ( option, cmdlsts, nopt, ktopt, ktdefs,
     +                        thelp, nh )

      implicit none
      include 'STARMAN_INC'

      character*(*) option		!i: Name of programme parameter to access
      integer       nopt		!i: Number of lists
      character*(*) cmdlsts(nopt)	!i: List of option names
      character*(*) ktopt		!o: Name of chosen option
      character*(*) ktdefs		!i: Default option name
      integer       nh			!i: No of help text lines
      character*68  thelp(1)		!i: Help text
C--
      integer k, ks, ke, kd, kp, ktlen, kl, kexact, kopt, kdef, num,
     +        ksw(200), klw(200), jcon(200)
      character text*40, texta*40, ktdef*10
      character*2000 cmdlst, textc
      logical loop

      integer lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      textc = cmdlsts(1)
      if ( nopt.gt.1 ) then
         do k = 2, nopt
            kl = lens(textc)
            textc = textc(1:kl)//':'//cmdlsts(k)
         enddo
      endif

      if ( textc.eq.' ' ) return					!Check for null reference entry

      call gcmdlsta  ( textc, cmdlst, kl, num, ksw, klw )		!Squeeze command list

      kdef = 1								!Find location in list of default
      do k = 1, num
         ks = ksw(k)
         ke = ks + klw(k) - 1
         if ( ktdef.eq.cmdlst(ks:ke) ) kdef = k
      enddo

      do k = 1, 200							!Order of listing for 'not found'
         jcon(k) = k							! is as input
      enddo

      loop = .true.							!Get entry, put into lower case
      do while ( loop )							! Loop back if help asked for

         call get1c ( option, texta, ktdefs, .true. )			!Get entry
         if ( ST_FAILED ) return
         call lbgone ( texta )						!Make input Lower case and remove leading blanks
         ktlen = lens(texta)
         call lowcase ( texta, text )

         if ( text.eq.'help' ) then					!Issue help if that is entry
            call tyhelp ( thelp, nh, cmdlst, jcon, ksw, klw, num )
         else

            kd = -1							!Find if any match in entry
            kexact = 0							! and if so what
            if ( text.ne.' ' ) then
               kd = 0
               do k = 1, num
                  ks = ksw(k)
                  ke = ks + klw(k) - 1
                  if ( index(cmdlst(ks:ke),text(1:ktlen)).eq.1 ) then
                     if ( ktlen.eq.(ke-ks+1) ) kexact = k
                     kp = k
                     kd = kd + 1
                  endif
               enddo
            endif

            loop = .false.							!Check entry
            if ( kd.eq.-1 ) then
               kopt = kdef
            elseif ( kd.eq.0 ) then
               call printo ( 'ERROR: Not found - ignored - try again' )
               loop = .true.
            elseif ( kexact.ne.0 ) then
               kopt = kexact
            elseif ( kd.eq.1 ) then
               kopt = kp
            else
               call printo ( 'ERROR: Ambiguous - ignored - try again' )
               loop = .true.
            endif

         endif

      enddo

      ks = ksw(kopt)
      ke = ks + klw(kopt) - 1
      ktopt = cmdlst(ks:ke)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET_JOB -- Get number in option name list from user chosen option name
C  User chooses from a list of option names, this s/r returns the number of
C  that option. If 'help' entered, help given. S/r cycles until a 'correct'
C  reply is given.
C
C   alan penny               ral                       1990 Jan

      subroutine get_job ( option, cmdlsts, kopt, kdefs, thelp, nh )

      implicit none
      include 'STARMAN_INC'

      character*(*) option	!i: Name of programme parameter to access
      character*(*) cmdlsts	!i: List of option names
      integer       kopt	!o: Posn in list of chosen option
      integer       kdefs	!i: Posn in list of default option name
      integer       nh		!i: No of help text lines
      character*68  thelp(1)	!i: Help text
C--
      integer k, ks, ke, kd, kp, ktlen, kdef, j, kl, kexact,
     +        jcon(200), num, kds, kde, ksw(200), klw(200), lens
      character*40 text
      character*2000 cmdlst
      logical loop
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( cmdlsts.eq.' ' ) return					!Check for null reference entry

      call gcmdlst  ( cmdlsts, cmdlst, kdefs, kdef, kds, kde, kl, num,	!Squeeze command list
     +                jcon, ksw, klw )

      loop = .true.							!Get entry, put into lower case
      do while ( loop )							! Loop back if help asked for

         call get1c ( option, text, cmdlst(kds:kde), .true. )		!Get entry
         if ( ST_FAILED ) return
         call lbgone ( text )						!Make input Lower case and remove leading blanks
         ktlen = lens(text)
         do k = 1, ktlen
            j = ichar(text(k:k))
            if ( j.ge.65 .and.  j.le.90 ) text(k:k) = char(j+32)
         enddo

         if ( text.eq.'help' ) then					!Issue help if that is entry
            call tyhelp ( thelp, nh, cmdlst, ksw, klw, jcon, num )
         else

            kd = -1							!Find if any match in entry
            kexact = 0							! and if so what
            if ( text.ne.' ' ) then
               kd = 0
               do k = 1, num
                  ks = ksw(k)
                  ke = ks + klw(k) - 1
                  if ( index(cmdlst(ks:ke),text(1:ktlen)).eq.1 ) then
                     if ( ktlen.eq.(ke-ks+1) ) kexact = k
                     kp = k
                     kd = kd + 1
                  endif
               enddo
            endif

            loop = .false.							!Check entry
            if ( kd.eq.-1 ) then
               kopt = kdef
            elseif ( kd.eq.0 ) then
               call printo ( 'ERROR: Not found - ignored - try again' )
               loop = .true.
            elseif ( kexact.ne.0 ) then
               kopt = kexact
            elseif ( kd.eq.1 ) then
               kopt = kp
            else
               call printo ( 'ERROR: Ambiguous - ignored - try again' )
               loop = .true.
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TYPE_HCHOICE -- Type out help at a program start on using the OPTION input
C
C  a j penny                          dao              1988-04-19

      subroutine type_hchoice ()


      implicit none
      include 'STARMAN_INC'

C--
      integer nline, j
      parameter ( nline=22 )
      character*68 tline (nline)
      data ( tline(j),j=1,10 ) /
     + ' ',
     + '       GENERAL INTRODUCTION to LOOPING INSTRUCTIONS',
     + 'You can choose from a list of OPTIONS, either by:-',
     + ' a) If a "TV" type screen is being used, by using a window',
     + '    "panel" full of "buttons" for inputting choices.',
     + ' b) Using the keyboard and the normal window.',
     + '  ',
     + 'In this later case, then:-',
     + '1) You have a list of OPTIONs to choose from; you choose one;',
     + '   the computer performs that OPTION (which may require '/
      data ( tline(j),j=11,20 ) /
     + '   further user interaction); the list is offered again.',
     + '   For example, you might be offered the line:-',
     + '   "  OPTION - Which action to perform? /"image"/ >  "',
     + '   To (say) get an image in a file, reply "image" (without',
     + '   the quote marks). You are then asked for the name of the ',
     + '   file, the computer accesses it, and offers OPTION again.',
     + '2) A "default" choice is given ("image" in the above example).',
     + '   The default is selected by doing the "carriage return".',
     + '3) Get Help by typing "?" or "??" (for full help), for input.',
     + '4) At the start, the default changes at each choice, with a '/
      data ( tline(j),j=21,nline ) /
     + '   succession of defaults being "recommended".',
     + ' '/
Cbegin


      if ( ST_FAILED ) return

      do j = 1, nline
         call printo ( tline(j) )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CH_SETUP -- Set up defaults before using choices
C
C   a j penny                 ral                1994 April

      subroutine ch_setup ()

      implicit none
      include 'ST_CHOICE_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      CH_AUTOSTART = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GCMDLST -- Squeeze command list and determine its character
C
C  a j penny                          dao              1988-04-19

      subroutine gcmdlst ( cmdlsts, cmdlst, kdefs, kdef, kds, kde,
     +                     kl, num, jcon, ksw, klw )


      implicit none
      include 'STARMAN_INC'

      character*(*)	cmdlsts		!i: Input list
      character*(*)	cmdlst		!o: List removed of blanks,
					!   multiple ':'s, etc
      integer		kdefs		!i: Input default option number
      integer		kdef		!o: Output default option number
      integer		kds		!o: Start of default option
      integer		kde		!o: End of default option
      integer		kl		!o: Character length of ouput command
					!   list
      integer		num		!o: Number of options in list
      integer		jcon(200)	!o: Alphabetical order of options
      integer		ksw(200)	!o: Start characters of options
      integer		klw(200)	!o: Character length of options
C--
      integer j, k, ja, ka, lens, kcon(200), nmax
      logical more
      external lens
Cbegin


      if ( ST_FAILED ) return

      kl = lens(cmdlsts)
      call lowcase ( cmdlsts, cmdlst )					!Transfer input, and lower case it

      cmdlst(kl+1:) = ' '						!Remove blanks in reference entry
      k = 1
      do while ( k.lt.kl )
         if ( cmdlst(k:k).eq.' ' ) then
            do j = k, kl-1
               cmdlst(j:j) = cmdlst(j+1:j+1)
            enddo
            kl = kl - 1
            k = k - 1
         endif
         k = k + 1
      enddo

      k = 1								!Remove multiple ':'s
      do while ( k.lt.kl )
         if ( cmdlst(k:k).eq.':' ) then
            more = .true.
            ka = k
            do while ( more )
               ka = ka + 1
               if ( cmdlst(ka:ka).ne.':' ) more = .false.
            enddo
            ja = ka - k - 1
            if ( ja.ne.0 ) then
               do j = k+1, kl - ja
                  cmdlst(j:j) = cmdlst(j+ja:j+ja)
               enddo
               kl = kl - ja
               cmdlst(kl+1:) = ' '
            endif
         endif
         k = k + 1
      enddo

      if ( cmdlst(1:1).eq.':' ) then					!Remove lead or end ':'
          do k = 1, kl-1
             cmdlst(k:k) = cmdlst(k+1:k+1)
          enddo
          kl = kl - 1
      endif
      if ( cmdlst(kl:kl).eq.':' ) then
         cmdlst(kl:kl) = ' '
         kl = kl - 1
      endif

      num = 0								!Find number of words
      do k = 1, kl
         if ( cmdlst(k:k).eq.':' ) num = num + 1
      enddo
      num = num + 1
      num = min(200,num)

      kdef = min(num,max(1,kdefs))					!Get default entry
      kds = 1
      kde = 1
      k = 1
      do j = 1, kl
         if ( cmdlst(j:j).eq.':' ) then
            k = k + 1
            if ( k.eq.kdef ) kds = j + 1
            if ( k.eq.kdef+1 ) kde = j - 1
         endif
      enddo
      if ( kdef.eq.num ) kde = kl

      nmax = 0								!Max length of any option
      ka = 0								! and start and length ofeach option
      do k = 1, kl
         if ( k.eq.1 .or. cmdlst(k:k).eq.':' ) then
            ka = ka + 1
            ksw(ka) = k
            j = k
            if ( k.ne.1 ) then
               ksw(ka) = ksw(ka) + 1
               j = j + 1
            endif
            do while ( j.lt.kl .and. cmdlst(j:j).ne.':' )
               j = j + 1
            enddo
            klw(ka) = j - ksw(ka)
            if ( j.eq.kl ) klw(ka) = klw(ka) + 1
            nmax = max(nmax,klw(ka))
         endif
      enddo

      do k = 1, num
         jcon(k) = k
      enddo
      do j = nmax, 1, -1
         do k = 1, num
            ka = jcon(k)
            if ( j.gt.klw(ka) ) then
               kcon(k) = -1
            else
               ja = ksw(ka) + j - 1
               kcon(k) = ichar(cmdlst(ja:ja))
            endif
         enddo
         call sort2i ( kcon, jcon, num )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GCMDLSTA -- Squeeze command list(s) and determine its/their character
C
C  a j penny                          dao              1988-04-19

      subroutine gcmdlsta ( cmdlsts, cmdlst, kl, num, ksw, klw )


      implicit none
      include 'STARMAN_INC'

      character*(*)	cmdlsts		!i: Input list
      character*(*)	cmdlst		!o: List removed of blanks, multiple ':'s, etc
      integer		kl		!o: Character length of ouput command list
      integer		num		!o: Number of options in list
      integer		ksw(200)	!o: Start characters of options
      integer		klw(200)	!o: Character length of options
C--
      integer j, k, ja, ka, lens, nmax
      logical more
      external lens
Cbegin


      if ( ST_FAILED ) return

      kl = lens(cmdlsts)
      call lowcase ( cmdlsts, cmdlst )					!Transfer input, and lower case it

      cmdlst(kl+1:) = ' '						!Remove blanks in reference entry
      k = 1
      do while ( k.lt.kl )
         if ( cmdlst(k:k).eq.' ' ) then
            do j = k, kl-1
               cmdlst(j:j) = cmdlst(j+1:j+1)
            enddo
            kl = kl - 1
            k = k - 1
         endif
         k = k + 1
      enddo

      k = 1								!Remove multiple ':'s
      do while ( k.lt.kl )
         if ( cmdlst(k:k).eq.':' ) then
            more = .true.
            ka = k
            do while ( more )
               ka = ka + 1
               if ( cmdlst(ka:ka).ne.':' ) more = .false.
            enddo
            ja = ka - k - 1
            if ( ja.ne.0 ) then
               do j = k+1, kl - ja
                  cmdlst(j:j) = cmdlst(j+ja:j+ja)
               enddo
               kl = kl - ja
               cmdlst(kl+1:) = ' '
            endif
         endif
         k = k + 1
      enddo

      if ( cmdlst(1:1).eq.':' ) then					!Remove lead or end ':'
          do k = 1, kl-1
             cmdlst(k:k) = cmdlst(k+1:k+1)
          enddo
          kl = kl - 1
      endif
      if ( cmdlst(kl:kl).eq.':' ) then
         cmdlst(kl:kl) = ' '
         kl = kl - 1
      endif

      num = 0								!Find number of words
      do k = 1, kl
         if ( cmdlst(k:k).eq.':' ) num = num + 1
      enddo
      num = num + 1
      num = min(200,num)

      nmax = 0								!Max length of any option
      ka = 0								! and start and length ofeach option
      do k = 1, kl
         if ( k.eq.1 .or. cmdlst(k:k).eq.':' ) then
            ka = ka + 1
            ksw(ka) = k
            j = k
            if ( k.ne.1 ) then
               ksw(ka) = ksw(ka) + 1
               j = j + 1
            endif
            do while ( j.lt.kl .and. cmdlst(j:j).ne.':' )
               j = j + 1
            enddo
            klw(ka) = j - ksw(ka)
            if ( j.eq.kl ) klw(ka) = klw(ka) + 1
            nmax = max(nmax,klw(ka))
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETDEFT -- Get default option from list of defaults (character)
C
C    alan penny                    ral       1990 jan

      subroutine getdeft ( kopta, knx, kny, nx, ny, kl_x, kl_y,
     +                     loopt, kopt )

      implicit none
      include 'STARMAN_INC'

      integer       knx			!i: Max no of times defaults can be asked for
      integer       kny			!i: Max No of different sets of possible defaults
      character*12  kopta(knx,kny)	!i: Default series
      integer       nx			!i: No of times defaults can be asked for
      integer       ny			!i: No of different sets of possible defaults
      integer       kl_x		!i/o: Number of times have asked for default
      integer       kl_y		!i/o: Last Y posn in defaults array
      logical       loopt		!i/o: Following correct series of defaults?
      character*(*) kopt		!i/o: Last default/Chosen new default
C--
      logical ok
      integer j
Cbegin


      if ( ST_FAILED ) return

      kl_x = kl_x + 1
      if ( kl_x.lt.nx .and. loopt ) then
         ok = .false.
         do j = kl_y, ny
            if ( .not.ok .and. kopt.eq.kopta(kl_x,j) ) then
               ok = .true.
               kl_y = j
            endif
         enddo
         if ( ok ) then
            kopt = kopta((kl_x+1),kl_y)
         else
            loopt = .false.
         endif
      else
         loopt = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C HX_HLOAD -- Put out help panel help on chosen option  (ignores case)
C
C   alan penny                        ral              1990-01-31

      subroutine hx_hload ( ktopt, topt, thead, thelp, nopt )

      implicit none
      include 'ST_CHOICE_INC'

      character*(*)   ktopt		!i: Chosen option
      integer         nopt		!i: No of possible options
      character*12    topt(nopt)	!i: Possible options  (ignores case)
      character*68    thead(nopt)	!i: Single line helps
      character*68    thelp(6,nopt)	!i: Fuller helps
C--
      logical there, found
      integer j, k, ka, kb, kl, kch, ktlen, ktlena
      character topta*12, toptb*12, texta*12, textb*50, textc*50
      character ohead*68, ohelp*545, ohelp1*68, bopt*12
      integer lens
      external lens

      character*68  otbhead, otbhelp(8)
      data otbhead, (otbhelp(j),j=1,8) /
     + 'Help for the buttons',
     + ' ', ' ',
     + '    *************************************************',
     + '    YOU WILL GET HELP HERE ON A BUTTON WHEN YOU PRESS',
     + '                    THE BUTTON YOU WANT THE HELP ON  ',
     + '    *************************************************',
     + ' ',' ' /

      character*68  ntbhead, ntbhelp(6)
      data ntbhead, (ntbhelp(j),j=1,6) /
     + 'Help for this button',
     + ' ', ' ',
     + '    ***  ***   ***  ***   ***   ***   ***   ***   ***   ***',
     + '    PANEL HELP IS NOT YET AVAILABLE FOR THIS BUTTON      ',
     + '    ***  ***   ***  ***   ***   ***   ***   ***   ***   ***',
     + ' ' /

      character*68  t1head, t1help(6)
      data t1head, (t1help(j),j=1,6) /
     + 'HELP',
     + 'Pressing this button gets a type-out of the list of options ',
     + 'and also some general help on the options.',
     + 'This button also has a display of the -state- of the program.',
     + 'WAITING = waiting for you to do something like press a button',
     + 'Other messages are: WORKING HELPING PANNING ZOOMING ',
     + 'NOT YET    READY   ONE GOT. The meaning will be clear in use.' /

Cbegin


      found = .false.
      ktlen = lens ( ktopt )
      if ( ktlen.ge.1 ) then
         ktlen = min(50,ktlen)
         textb = ktopt(1:ktlen)
         call lowcase ( textb, textc )
         call lbgone ( textc )
         ktlen = lens ( textc )
         ktlen = min(12,ktlen)
         texta = textc(1:ktlen)

         kch = 0
         do while ( .not.found .and. kch.lt.nopt )
            kch = kch + 1
            toptb = topt(kch)
            call lowcase ( toptb, topta )
            ktlena = lens(topta)
            if ( ktlena.eq.ktlen ) then
               there = .true.
               do j = 1, ktlen
                  if ( topta(j:j).ne.texta(j:j) ) there = .false.
               enddo
               if ( there ) found = .true.
            endif
         enddo
         kl = 0
         if ( .not.found ) then
            if ( ktopt.eq.'XX_HELP' ) then
               kl = 1
            else
               kl = 2
            endif
            found = .true.
         endif
      endif

      if ( found ) then
          bopt = ' '
          k = 1
          if ( kl.eq.0 ) bopt = topt(kch)
          if ( kl.eq.1 ) bopt = t1head
          if ( kl.eq.2 ) bopt = texta
          k = lens(bopt)
          ohead = 'Help for the  - '//bopt(1:k)//' -  panel button'
      endif

      do j = 1, 8*68
         ohelp(j:j) = ' '
      enddo
      ohelp(545:545) = 'T'


      do j = 1, 8

         if ( found ) then
            if ( j.eq.1 ) then
               if ( kl.eq.0 ) then
                  ohelp1 = thead(kch)
               elseif ( kl.eq.1 ) then
                  ohelp1 = t1head
               else
                  ohelp1 = ntbhead
               endif
            elseif ( j.eq.2 ) then
               ohelp1 = ' '
            else
               if ( kl.eq.0 ) then
                  ohelp1 = thelp(j-2,kch)
               elseif ( kl.eq.1 ) then
                  ohelp1 = t1help(j-2)
               else
                  ohelp1 = ntbhelp(j-2)
               endif
            endif
         endif

         ka = 1 + (j-1)*68
         k = min(68,lens(ohelp1))
         if ( k.gt.0 ) then
            kb = ka + k - 1
            ohelp(ka:kb) = ohelp1(1:k)
         endif

      enddo

      call ds_p_hx_hload ( ohead, ohelp )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARFLIB_PRE.FOR
C
C  It contains:-
C
C GET1(BCIR)     Get a (boolean:character string:integer:real) from the CL
C GET(2345)(IR)  Get (2:3:4:5) (integers:reals) from the CL
C GTIMZD         Get descriptors BSACLE, BZERO, INVAL, TITLE form a real or int*2 image
C GTWRK(IRS)     Open (int:real:short) computer work space
C OPIM(IRSU)R    Open an input 2d read-only (real:int*2:unsigned int*2:int) image
C OPIM4ZR        Open an input 4d read-only real/int/int*2 image
C OPIMZ(RW)      Open an input 2d read-only/write-only real or int*2 image
C OPIM(IRS)W     Open an output 2d write-only (real:integer*2:integer) image
C OPIM4(IRS)W    Open an output 4d write-only (real:integer*2:integer) image
C PUT(123)(IR)   Put (integer:real) (1:2:3) numbers to the CL



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET1B -- Get a boolean value from the CL
C
C       a j penny              stsci                1987 oct 16

      subroutine get1b ( param, bool, inbool )

      implicit none

      character*(*) param	!i: Parameter name
      logical bool		!o: Boolean value got
      logical inbool		!i: Default boolean value
C--
      logical bv(5)
      integer num
Cbegin


      num = 1
      bv(1) = inbool
      call getgb ( param, bv, num, .true. )
      bool = bv(1)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET1I -- Get 1 integer from CL
C
C   aj penny                        dao             1988-04-19


      subroutine get1i ( param, i1, in, imin, imax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      integer	    i1		!o: Integer - gotten value
      integer	    in		!i: Integer - default value
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      integer iv(5)
Cbegin


      iv(1) = in
      call getgi ( param, iv, 1, .true., imin, imax )
      i1 = iv(1)

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET2I -- Get 2 integers from the CL
C
C   aj penny                        dao             1988-04-19


      subroutine get2i ( param, i1, i2, def, imin, imax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      integer	    i1		!i/o: Integers (defaults/gotten values)
      integer       i2		!i/o
      logical       def		!i: Is a null rsponse valid?
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      integer iv(5)
Cbegin


      iv(1) = i1
      iv(2) = i2
      call getgi ( param, iv, 2, def, imin, imax )
      i1 = iv(1)
      i2 = iv(2)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET3I -- Get 3 integers from the CL
C
C    a j penny                 dao                   1988-04-19


      subroutine get3i ( param, i1, i2, i3, def, imin, imax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      integer	    i1		!i/o: Integers (defaults/gotten values)
      integer       i2		!i/o
      integer       i3		!i/o:
      logical       def		!i: Is a null rsponse valid?
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      integer iv(5)
Cbegin


      iv(1) = i1
      iv(2) = i2
      iv(3) = i3
      call getgi ( param, iv, 3, def, imin, imax )
      i1 = iv(1)
      i2 = iv(2)
      i3 = iv(3)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET4I -- Get 4 integers from the CL
C
C    a j penny                 ral                   1988-Nov


      subroutine get4i ( param, i1, i2, i3, i4, def, imin, imax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      integer	    i1		!i/o: Integers (defaults/gotten values)
      integer       i2		!i/o
      integer       i3		!i/o:
      integer       i4		!i/o:
      logical       def		!i: Is a null rsponse valid?
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      integer iv(5)
Cbegin


      iv(1) = i1
      iv(2) = i2
      iv(3) = i3
      iv(4) = i4
      call getgi ( param, iv, 4, def, imin, imax )
      i1 = iv(1)
      i2 = iv(2)
      i3 = iv(3)
      i4 = iv(4)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET5I -- Get 5 integers from the CL
C
C    a j penny                 ral                   1988-Nov


      subroutine get5i ( param, i1, i2, i3, i4, i5, def, imin, imax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      integer	    i1		!i/o: Integers (defaults/gotten values)
      integer       i2		!i/o
      integer       i3		!i/o:
      integer       i4		!i/o:
      integer       i5		!i/o:
      logical       def		!i: Is a null rsponse valid?
      integer       imin	!i: Min allowed value
      integer       imax	!i: Max allowed value
C--
      integer iv(5)
Cbegin


      iv(1) = i1
      iv(2) = i2
      iv(3) = i3
      iv(4) = i4
      iv(5) = i5
      call getgi ( param, iv, 5, def, imin, imax )
      i1 = iv(1)
      i2 = iv(2)
      i3 = iv(3)
      i4 = iv(4)
      i5 = iv(5)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET1R -- Get 1 real from CL
C
C   aj penny                        dao             1988-04-19


      subroutine get1r ( param, r1, rin, rmin, rmax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      real          r1		!o: Real - gotten value
      real          rin		!i: Real - default value
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      real  rv(5)
Cbegin


      rv(1) = rin
      call getgr ( param, rv, 1, .true., rmin, rmax )
      r1 = rv(1)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET2R -- Get 2 reals from the CL
C
C   aj penny                        dao             1988-04-19


      subroutine get2r ( param, r1, r2, def, rmin, rmax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      real          r1		!i/o: Reals (defaults/gotten values)
      real          r2		!i/o:
      logical       def		!i: Is a null rsponse valid?
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      real rv(5)
Cbegin


      rv(1) = r1
      rv(2) = r2
      call getgr ( param, rv, 2, def, rmin, rmax )
      r1 = rv(1)
      r2 = rv(2)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET3R -- Get 3 reals from the CL
C
C   aj penny                        dao             1988-04-19


      subroutine get3r ( param, r1, r2, r3, def, rmin, rmax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      real          r1		!i/o: Reals (defaults/gotten values)
      real          r2		!i/o:
      real          r3		!i/o:
      logical       def		!i: Is a null rsponse valid?
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      real rv(5)
Cbegin


      rv(1) = r1
      rv(2) = r2
      rv(3) = r3
      call getgr ( param, rv, 3, def, rmin, rmax )
      r1 = rv(1)
      r2 = rv(2)
      r3 = rv(3)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET4R -- Get 4 reals from the CL
C
C   aj penny                        dao             1988-04-19


      subroutine get4r ( param, r1, r2, r3, r4, def, rmin, rmax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      real          r1		!i/o: Reals (defaults/gotten values)
      real          r2		!i/o:
      real          r3		!i/o:
      real          r4		!i/o:
      logical       def		!i: Is a null rsponse valid?
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      real rv(5)
Cbegin


      rv(1) = r1
      rv(2) = r2
      rv(3) = r3
      rv(4) = r4
      call getgr ( param, rv, 4, def, rmin, rmax )
      r1 = rv(1)
      r2 = rv(2)
      r3 = rv(3)
      r4 = rv(4)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GET5R -- Get 5 reals from the CL
C
C   aj penny                        dao             1988-04-19


      subroutine get5r ( param, r1, r2, r3, r4, r5, def, rmin, rmax )

      implicit none

      character*(*) param	!i: Parameter to access in CL
      real          r1		!i/o: Reals (defaults/gotten values)
      real          r2		!i/o:
      real          r3		!i/o:
      real          r4		!i/o:
      real          r5		!i/o:
      logical       def		!i: Is a null rsponse valid?
      real          rmin	!i: Min allowed value
      real          rmax	!i: Max allowed value
C--
      real rv(5)
Cbegin


      rv(1) = r1
      rv(2) = r2
      rv(3) = r3
      rv(4) = r4
      rv(5) = r5
      call getgr ( param, rv, 5, def, rmin, rmax )
      r1 = rv(1)
      r2 = rv(2)
      r3 = rv(3)
      r4 = rv(4)
      r5 = rv(5)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTIMZD -- Get descriptors BSACLE, BZERO, INVAL, TITLE form a real or int*2 image
C
C  alan penny              ral          1990 Jan

      subroutine gtimzd ( name, gtype, bs, bz, inval, rinval, title,
     +                    ierr )

      implicit none
      include 'ST_LIMITS_INC'

      character*(*) name        !i: parameter name of image
      character*(*) gtype       !i: Image type ('REAL', 'SHORT')
      real          bs          !o: Image scale
      real          bz          !o: Image zero
      integer       inval      	!o: Int*2 Image bad pixel flag (undef if 'REAL')
      real          rinval      !o: Real Image bad pixel flag  (undef if 'SHORT')
      character*(*) title       !o: Image title
      integer       ierr        !o: Error flag (0=ok)
C--
Cbegin


      if ( gtype.eq.'SHORT') then
         call gtimsd ( name, bs, bz, inval, title, ierr )
      elseif ( gtype.eq.'REAL') then
         call gtimrd ( name, bs, bz, rinval, title, ierr )
      else
         ierr = 1
         call printo ( ' ERROR: programmer error in s/r GTIMZD' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
        endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTWRKI -- Open integer*4 computer work space
C
C  alan penny              ral          1990 Jan

      subroutine gtwrki ( text, n, ip, ierr )

      implicit none

      character*(*) text	!i: Name to give to work space
      integer       n		!i: Size to allocate (in words)
      integer       ip		!o: Pointer to allocated space
      integer       ierr	!o: Error flag: 0=ok, 1=bad
C--
Cbegin


      call gtwrkg ( text, n, 'INT', ip, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTWRKR -- Open real computer work space
C
C  alan penny              ral          1990 Jan

      subroutine gtwrkr ( text, n, ip, ierr )

      implicit none

      character*(*) text	!i: Name to give to work space
      integer       n		!i: Size to allocate (in words)
      integer       ip		!o: Pointer to allocated space
      integer       ierr	!o: Error flag: 0=ok, 1=bad
C--
Cbegin


      call gtwrkg ( text, n, 'REAL', ip, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTWRKS -- Open short computer work space
C
C  alan penny              ral          1990 Jan

      subroutine gtwrks ( text, n, ip, ierr )

      implicit none

      character*(*) text	!i: Name to give to work space
      integer       n		!i: Size to allocate (in words)
      integer       ip		!o: Pointer to allocated space
      integer       ierr	!o: Error flag: 0=ok, 1=bad
C--
Cbegin


      call gtwrkg ( text, n, 'SHORT', ip, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMIR -- Open an input 2d read-only integer*4 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimir ( name, ipin, nx, ny, def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name	!i: Name of parameter to hold image name
      integer       ipin	!o: Image pointer
      integer       nx		!o: Image X size
      integer       ny		!o: Image Y size
      logical       def		!i: if true, null reply accepted
      integer       ierr	!o: Error 0=ok, 1=bad, 2=null and acceptable
				!         3=wrong type
C--
      character*6 gtype
Cbegin


      call opimgr ( name, ipin, nx, ny, gtype, def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and. gtype.ne.'INT' ) then
         call printo ( 'WARNING: Image not integer type' )
         ierr = 3
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMRR -- Open an input 2d read-only real image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimrr ( name, ipin, nx, ny, def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name	!i: Name of parameter to hold image name
      integer       ipin	!o: Image pointer
      integer       nx		!o: Image X size
      integer       ny		!o: Image Y size
      logical       def		!i: if true, null reply accepted
      integer       ierr	!o: Error 0=ok, 1=bad, 2=null and acceptable
				!         3=wrong type
C--
      character*6 gtype
Cbegin


      call opimgr ( name, ipin, nx, ny, gtype, def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and. gtype.ne.'REAL' ) then
         call printo ( 'WARNING: Image not real type' )
         ierr = 3
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMSR -- Open an input 2d read-only integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimsr ( name, ipin, nx, ny, def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name	!i: Name of parameter to hold image name
      integer       ipin	!o: Image pointer
      integer       nx		!o: Image X size
      integer       ny		!o: Image Y size
      logical       def		!i: if true, null reply accepted
      integer       ierr	!o: Error 0=ok, 1=bad, 2=null and acceptable
				!         3=wrong type
C--
      character*6 gtype
Cbegin


      call opimgr ( name, ipin, nx, ny, gtype, def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and. gtype.ne.'SHORT' ) then
         call printo ( 'WARNING: Image not integer*2 type' )
         ierr = 3
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMUR -- Open an input 2d read-only unsigned integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimur ( name, ipin, nx, ny, def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name	!i: Name of parameter to hold image name
      integer       ipin	!o: Image pointer
      integer       nx		!o: Image X size
      integer       ny		!o: Image Y size
      logical       def		!i: if true, null reply accepted
      integer       ierr	!o: Error 0=ok, 1=bad, 2=null and acceptable
				!         3= wrong type
C--
      character*6 gtype
Cbegin


      call opimgr ( name, ipin, nx, ny, gtype, def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and. gtype.ne.'USHORT' ) then
         call printo ( 'WARNING: Image not unsigned integer*2 type' )
         ierr = 3
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMZR -- Open an input 2d read-only real or integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimzr ( name, ipin, nx, ny, gtype, def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name	!i: Name of parameter to hold image name
      integer       ipin	!o: Image pointer
      integer       nx		!o: Image X size
      integer       ny		!o: Image Y size
      character*(*) gtype	!o: Image type
      logical       def		!i: if true, null reply accepted
      integer       ierr	!o: Error 0=ok, 1=bad, 2=null and acceptable
				!         3=not real or short type
C--
      character*40 text
Cbegin


      call opimgr ( name, ipin, nx, ny, gtype, def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and.
     +     gtype.ne.'REAL' .and. gtype.ne.'SHORT' ) then
         write ( text, '('' ERROR: Image type is: '',a)') gtype
         call printo ( text )
         call printo ( 'ERROR: Can only use REAL or SHORT images' )
         ierr = 3
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4ZR -- Open an input 4d read-only real/int/integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan


      subroutine opim4zr ( name, ipin, nx, ny, nz, nt, ndim, gtype,
     +                     def, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) name		!i: Name of parameter to get image name
      integer       ipin		!o: Image pointer
      integer       nx			!o: Image X size
      integer       ny			!o: Image Y size
      integer       nz			!o: Image Z size
      integer       nt			!o: Image T size
      integer       ndim		!o: No of dimensions
      character*(*) gtype	        !o: Image type ('REAL'/'INT'/'SHORT')
      logical       def			!i: if true, null reply accepted
      integer       ierr		!o: Error 0=ok, 1=bad, 2=null and acceptable
					!         3=wrong type
C--
      character*40 text
Cbegin


      call opim4gr ( name, ipin, nx, ny, nz, nt, ndim, gtype,
     +                     def, ierr )
      if ( ST_FAILED ) return
      if ( ierr.eq.0 .and. gtype.ne.'REAL' .and.
     +     gtype.ne.'INT' .and. gtype.ne.'SHORT' ) then
         write ( text, '(''ERROR: Image type is: '',a)') gtype
         call printo ( text )
         call printo ( 'ERROR: Can only use REAL, INT or SHORT images' )
         ierr = 3
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMZW -- Open an output 2d read-only real or integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opimzw ( name, gtype, ip, nx, ny, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      character*(*) gtype 		!i: Image type ('SHORT', 'REAL')
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
					!         3=not REAL/SHORT
C--
Cbegin


      if ( gtype.ne.'REAL' .and. gtype.ne.'SHORT' ) then
         ierr = 3
         call printo ( ' ERROR: Programmer error in s/r OPIMZW' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
      else
         call opimgw ( name, gtype, ip, nx, ny, def, ierr )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4ZW -- Open an outnput 4d read-only real or integer*2 image
C  The parameter name is left attached
C
C      alan penny             ral                1990 jan

      subroutine opim4zw ( name, gtype, ip, nx, ny, nz,nt,def,ierr)

      implicit none

      character*(*) name		!i: Parameter name for image
      character*(*) gtype 		!i: Image type ('SHORT', 'REAL')
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      integer	    nz			!i: Image Z size
      integer	    nt			!i: Image T size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
					!         3=not REAL/SHORT
C--
Cbegin


      if ( gtype.ne.'REAL' .and. gtype.ne.'SHORT' ) then
         ierr = 3
         call printo ( ' ERROR: Programmer error in s/r OPIM4ZW' )
         call printo ( '        Code needs rewriting: Contact'//
     +                 ' person who wrote the program' )
      else
         call opim4gw ( name, gtype, ip, nx, ny, nz, nt,def,ierr)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMIW -- Open an output 2d write-only integer*4 image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opimiw ( name, ip, nx, ny, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opimgw ( name, 'INT', ip, nx, ny, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMRW -- Open an output 2d write-only real image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opimrw ( name, ip, nx, ny, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opimgw ( name, 'REAL', ip, nx, ny, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIMSW -- Open an output 2d write-only integer*2 image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opimsw ( name, ip, nx, ny, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opimgw ( name, 'SHORT', ip, nx, ny, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4IW -- Open an output 4d write-only integer*4 image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opim4iw ( name, ip, nx, ny, nz, nt, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      integer	    nz			!i: Image Z size
      integer	    nt			!i: Image T size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opim4gw ( name, 'INT', ip, nx, ny, nz, nt, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4RW -- Open an output 4d write-only real image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opim4rw ( name, ip, nx, ny, nz, nt, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      integer	    nz			!i: Image Z size
      integer	    nt			!i: Image T size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opim4gw ( name, 'REAL', ip, nx, ny, nz, nt, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPIM4SW -- Open an output 4d write-only integer*2 image
C  The parameter name is left attached
C
C    a j penny                 ral                  1988-07-10

      subroutine opim4sw ( name, ip, nx, ny, nz, nt, def, ierr )

      implicit none

      character*(*) name		!i: Parameter name for image
      integer       ip			!o: Pointer to image
      integer	    nx			!i: Image X size
      integer	    ny			!i: Image Y size
      integer	    nz			!i: Image Z size
      integer	    nt			!i: Image T size
      logical	    def			!i: If true, null reply is accepted
      integer	    ierr		!o: Error 0=ok, 1=bad, 2=blank and ok
C--
Cbegin


      call opim4gw ( name, 'SHORT', ip, nx, ny, nz, nt, def, ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT1I -- Put an integer number to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put1i ( name, ival )

      implicit none

      character*(*)	name		!i: Name of parameter
      integer           ival		!i: Value to load parameter
C--
      integer iv(3)
Cbegin


      iv(1) = ival
      call putgi ( name, 1, iv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT2I -- Put two integer numbers to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put2i ( name, ival1, ival2 )

      implicit none

      character*(*)	name		!i: Name of parameter
      integer		ival1		!i: 1st value to load parameter
      integer		ival2		!i: 2nd value to load parameter
C--
      integer iv(3)
Cbegin


      iv(1) = ival1
      iv(2) = ival2
      call putgi ( name, 2, iv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT3I -- Put three integer numbers to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put3i ( name, ival1, ival2, ival3 )

      implicit none

      character*(*)	name		!i: Name of parameter
      integer 		ival1		!i: 1st value to load parameter
      integer		ival2		!i: 2nd value to load parameter
      integer		ival3		!i: 3rd value to load parameter
C--
      integer  iv(3)
Cbegin


      iv(1) = ival1
      iv(2) = ival2
      iv(3) = ival3
      call putgi ( name, 3, iv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT1R -- Put a real number to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put1r ( name, val )

      implicit none

      character*(*)	name		!i: Name of parameter
      real		val		!i: Value to load parameter
C--
      real  rv(3)
Cbegin


      rv(1) = val
      call putgr ( name, 1, rv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT2R -- Put two real numbers to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put2r ( name, val1, val2 )

      implicit none

      character*(*)	name		!i: Name of parameter
      real		val1		!i: 1st value to load parameter
      real		val2		!i: 2nd value to load parameter
C--
      real rv(3)
Cbegin


      rv(1) = val1
      rv(2) = val2
      call putgr ( name, 2, rv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PUT3R -- Put three real numbers to the CL
C
C    a j penny                ral         1988-09-07

      subroutine put3r ( name, val1, val2, val3 )

      implicit none

      character*(*)	name		!i: Name of parameter
      real		val1		!i: 1st value to load parameter
      real		val2		!i: 2nd value to load parameter
      real		val3		!i: 3rd value to load parameter
C--
      real rv(3)
Cbegin


      rv(1) = val1
      rv(2) = val2
      rv(3) = val3
      call putgr ( name, 3, rv )


      end

