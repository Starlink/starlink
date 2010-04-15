CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEASURE -- Gets star magnitudes by profile fitting
C
C   A.J.Penny                 RAL                 1990 Jan

      subroutine measure ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_measure

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   This is MESUB.F
C
C  It Contains:-
C
C ME_RLOAD      Load and set stars from star list to RES area.
C ME_XYTRAN     Transform XY positions
C ME_XYTRANA    Transform XY posns from an array to another array
C ME_INBOX      Load box size, see if in image, calc if do map
C ME_EHEIGHT    Find rough heights of stars at given position
C ME_RHEIGHT    Refine the rough magnitudes of stars with rough heights
C ME_ESTH       Do actual work of rough estimate of the star height.
C ME_SULIST     Find companion stars
C ME_NONELC     Flag none as cleaned
C ME_CHEXTRA    Check the effect of the extras on premeasured stars.
C ME_MAGS       Fit Lorentzians to a list of stars
C ME_OLDRES     Put out the previous loop's result and clean
C ME_TWOS       Take 2 nearby undone stars and fit as one.
C ME_STADD      Cancel a star and restore it to a cleaned image
C ME_DORES      Arrange storage of result and clean of image
C ME_DOVOL      Do the volume calculations for all the stars
C ME_BEFCLEAR   Remove all stars that have a 'pure before' fit from an image
C ME_CLEAN      Remove the fitted stars from the input image.
C ME_CLEANA     Display of cleaning
C ME_SETMEAS    Set up Lorentz fit
C ME_RESULT     Store the fit result fit and type out a line of result
C ME_SORTL      Sort an order list of decreasing star heights
C ME_CPBEFORE   Load before measures for valid value (mag<49) stars
C ME_WEEDM      Remove stars that have merged with another
C ME_REMSTAR    Remove star from list to be fitted and clear RES, output
C ME_REMCOMP    Remove first close undone companion from star's companion list
C ME_SETDONE    Set flags that star has been done.
C ME_STARDO    (FN) For this loop number and operation number, do star?
C ME_PARDO      Find if a star ready for parallel doing
C ME_CLEANDO    Find whether to clean a star



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_RLOAD -- Load and set stars from star list to RES area.
C   Does this into the RES area, after clearing the rows concerned. It flags
C   them all as not done (mag=50.0). It also deals with the loading the
C   default profile to use. The list can be put in starting at a defined row.
C
C   a j penny                    stsci               1987-02-22

      subroutine me_rload ( intb, tbxvi, tbyi, res, lhead, koff )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      integer	tbxvi			!i: Input list X size
      integer	tbyi			!i: Input list Y size
      real	intb(tbxvi,tbyi)	!i: Input list
      real	res(TBX,TBY)		!i/o: Results array
      integer	lhead(16)		!i: Column posn in input list of data
      integer	koff			!i: Data to go into res starting at
					!   (koff+1) row
C--
      integer j, k, ka, smapnum, smapx, smapy, smagnif
      real x, y, xa, ya, h, sprof(9)
Cbegin


      if ( ST_FAILED ) return						!Failure check


      call coprr ( intb, tbxvi, tbyi, 6, 7, 1, tbyi, 		        !Copy input to input posn
     +             %val(IPINAE), 3, TBY, 1, 1+koff )			! store

      do k = 1, tbyi							!Get data to store for all
									! stars in the input list

         ka = k + koff							!Row offset
         call azeror ( res(1,ka), TBX )					!Clear the row

         call getd ( intb, tbxvi, tbyi, lhead, k, x, y, h, 0, PROFK, 	!Get data from file
     +               sprof, smapnum, smapx, smapy, smagnif,
     +               MAGNIFK, MAPXK, MAPYK, MAPNUMK, PBOSS )

         xa = x
         ya = y
         x = XCOEFFS(1) + xa*XCOEFFS(2) + ya*XCOEFFS(3)
         y = YCOEFFS(1) + xa*YCOEFFS(2) + ya*YCOEFFS(3)

         if ( smapnum.gt.MZ ) then					!Check map numbers
            call pargi ( k )
            call pargi ( smapnum )
            call printd ( 'ERROR: Star number %d has Map number %d')
            call pargi ( MZ )
            call printd ( '       - Maximum allowed is %d ' )
            ST_FAILED = .true.
         endif

         res(1,ka) = x							!Input position and height
         res(2,ka) = y
         res(9,ka) = h
         res(49,ka) = h
         res(3,ka) = 50.0						!Set mag not done
         call amovr ( sprof, res(12,ka), 9 ) 				!Profile
         res(11,ka) = smapnum
         res(38,ka) = smapx
         res(39,ka) = smapy
         res(40,ka) = smagnif

         do j = 1, 15							!Set checks
            call wcheck ( res(34,ka), j, .false. )
            call wcheck ( res(35,ka), j, .false. )
         enddo
         call wcheck ( res(34,ka), 9, .true. )

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_XYTRAN -- Transform positions
C
C   a j penny              ral                  1994-09-02

      subroutine me_xytran ( tb, nx, ny )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      integer	nx			!i: Input table
      integer	ny			!i: Input table Y size
      real	tb(nx,ny)		!i/o: Input/output table
C--
      integer j
      real x, y
Cbegin


      if ( ST_FAILED ) return						!Failure check

      do j = 1, ny
         x = tb(1,j)
         y = tb(2,j)
         tb(1,j) = XCOEFFS(1) + x*XCOEFFS(2) + y*XCOEFFS(3)
         tb(2,j) = YCOEFFS(1) + x*YCOEFFS(2) + y*YCOEFFS(3)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_XYTRANA -- XY transform from an array to another array
C
C     a j penny                ral                 1994-09-01

      subroutine me_xytrana ( a, nxa, nya, b, nxb, nyb, kyo )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      integer  nxa              !i: X size of input array
      integer  nya              !i: Y size of input array
      real     a(nxa,nya)       !i: Input array
      integer  nxb              !i: X size of output array
      integer  nyb              !i: Y size of output array
      real     b(nxb,nyb)       !o: Output array
      integer  kyo              !i: Row to start at
C--
      integer j
      real x, y
Cbegin


      if ( ST_FAILED ) return

      do j = 1, nya
         x = a(6,j)
         y = a(7,j)
         b(6,j+kyo) = XCOEFFS(1) + x*XCOEFFS(2) + y*XCOEFFS(3)
         b(7,j+kyo) = YCOEFFS(1) + x*YCOEFFS(2) + y*YCOEFFS(3)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_INBOX -- Load box size, see if in image, calc if do map
C
C    a j penny           stsci   1988-03-11

      subroutine me_inbox ( res, tdomap )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real	res(TBX,TBY)		!i/o: Results array
      logical	tdomap(TBY)		!o:   Flags for use map for a star
C--
      real rxl, ryl, thetal, rx, ry, theta, xbox, ybox, xst, xen, yst,
     +     yen
      integer smapnum, smapx, smapy, k, mapnumb(100), kk, kktot, smapk
      logical inimage, sdomap, domapb(100)
Cbegin


      if ( ST_FAILED ) return						!Failure check

      rxl = 0.0
      ryl = 0.0
      thetal = 0.0
      xbox = 0.0
      ybox = 0.0
      sdomap = .false.
      kktot = 1
      call azeroi ( mapnumb, 100 )
      call azerob ( domapb, 100 )

      do k = 1, TBY							!Do all the stars

         if ( LXSIZE.eq.0 .or. LYSIZE.eq.0  ) then			!Find box
            rx = res(12,k)
            ry = res(13,k)
            theta = res(17,k)
            if ( rx.ne.rxl. or. ry.ne.ryl .or. theta.ne.thetal ) then
               call boxeli ( rx, ry, theta, xbox, ybox )
               rxl = rx
               ryl = ry
               theta = thetal
            endif
            res(29,k) = nint(10.0*xbox)
            res(30,k) = nint(10.0*ybox)
         else
            xbox = real(LXSIZE)/10.0
            ybox = real(LYSIZE)/10.0
            res(29,k) = LXSIZE
            res(30,k) = LYSIZE
         endif

         xst = res(1,k) - 5.0*xbox					!Find if out of image
         xen = xst + 10.0*xbox + 1.0
         yst = res(2,k) - 5.0*ybox
         yen = yst + 10.0*ybox + 1.0
         if ( xst.gt.real(NX) .or. xen.lt.1.0 .or.
     +        yst.gt.real(NY) .or. yen.lt.1.0 ) then
            inimage = .false.
            call wcheck ( res(34,k), 9, .false. )
         endif

         tdomap(k) = DOMAP						!Find if do the map
         if ( inimage .and. DOMAP ) then
            smapnum = nint(res(11,k))
            smapk = 0
            do kk = 1, kktot
               if ( smapnum.eq.mapnumb(kk) ) smapk = kk
            enddo
            if ( smapk.ne.0 ) then
               tdomap(k) = domapb(smapk)
            else
               smapx = res(38,k)
               smapy = res(39,k)
               call rchzero ( %val(IPRMAP), MX, MY, MZ, smapnum,
     +                        smapx, smapy, sdomap )
               tdomap(k) = sdomap
               if ( kktot.ne.100 ) then
                  kktot = kktot + 1
                  domapb(kktot) = sdomap
                  mapnumb(kktot) = smapnum
               endif
            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_EHEIGHT -- Find rough heights of stars at given position
C      by max height above image mean sky at those posns, with some
C      safety checks.
C
C   a j penny                   stsci                    1987-02-22

      subroutine me_eheight ( im, res )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)		!i: Input image
      real	res(TBX,TBY)		!i/o: results array
C--
      integer k
      real x, y, xbox, ybox, rx, ry, prx, pry, p, ht
      logical rcheck
      external rcheck
Cbegin


      if ( ST_FAILED ) return						!Failure check

      do k = 1, TBY							!Do for all the stars

         if ( LOOPNUM.eq.2 .and. res(3,k).lt.49.0 ) then		!Height found already?

            res(49,k) = res(9,k)

         else

            if ( rcheck(res(34,k),9) .and.				!Get rough height by
     +          (ESTH_DO .or. res(49,k).eq.0.0) ) then			! finding max in small box
               x = res(1,k)						! round the star, and sky
               y = res(2,k)						! by min in a larger box.
               xbox = res(29,k)
               ybox = res(30,k)
               rx = res(12,k)
               ry = res(13,k)
               p = res(14,k)
               prx = res(15,k)
               pry = res(16,k)
               call me_esth ( im, x, y, xbox, ybox, rx, ry, prx, pry,
     +                        p, ht )
               res(49,k) = ht						!store result
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_RHEIGHT -- Refine the rough magnitudes of stars with rough heights
C
C   a j penny                   stsci                    1987-10-25

      subroutine me_rheight ( res )

      implicit none
      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real	res(TBX,TBY)		!i/o: Results array
C--
      integer k, kx, ky, ns
      real h, hmean, amean, sm, hmin, amag
      logical rcheck
      external rcheck
Cbegin


      if ( ST_FAILED ) return						!Failure check

      sm = 0.0								!No of stars and mean mag
      ns = 0
      do k = 1, TBY
         if ( rcheck(res(34,k),9) ) then
            h = res(49,k)
            if ( h.gt.2.0 .and. h.lt.60000.0 ) then
               sm = sm + 30.0 - 2.5*alog10(h)
               ns = ns + 1
            endif
         endif
      enddo

      if ( ns.eq.0 ) then
         amean = 20.0
         call printo ( 'No rough magnitudes can be measured' )
      else
         amean = sm/real(ns)
      endif

      hmean = 10.0**(max(1.0e-5,((30.0-amean)/2.5)))			!Refine mean
      hmin = hmean/100.0						!min = 5 mags fainter

      do k = 1, TBY							!Refine heights
         if ( rcheck(res(34,k),9) ) then				! for good stars

            h = res(49,k)						!Reset any heights which
									! are more than 5 mags
            if ( h.lt.0.1 ) h = hmin					! away from the mean to
            amag = 30.0 - 2.5*alog10(h)					! those limits. Also if
            if ( amag.gt.49.0 ) h = hmin				! fit has failed, set the
            if ( h.lt.hmin ) h = hmin					! height to the min

            kx = res(1,k)						!If centre outside
            ky = res(2,k)
            if (kx.lt.1 .or. kx.gt.NX .or. ky.lt.1 .or. ky.gt.NY) h=hmin

            res(49,k) = h

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_ESTH -- Do actual work of rough estimate of the star height.
C     Looks for max in 3x3 box for peak. look for min in measuring
C     box. Deal with invalid peak by finding nearest unsaturated
C     and using profile to get true height. Returns 0 if star
C     outside image.
C
C     Max allowed height is 1.0e7, min is 0.001.
C
C      a j penny                  stsci               1987-02-25

      subroutine me_esth ( im, x, y, xbox, ybox, rx, ry, prx,
     +                     pry, p, ht )

      implicit none

      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real      im(NX,NY)		!i: Input image
      real	x			!i: Star X posn
      real	y			!i: Star Y posn
      real	xbox			!i: Star X box size
      real	ybox			!i: Star Y box size
      real	rx			!i: Star Maj axis radius
      real	ry			!i: Star Min axis radius
      real	prx			!i: Star Maj axis power radius
      real	pry			!i: Star Min axis power radius
      real	p			!i: Star power
      real	ht 			!o: Estimated height
C--
      integer nxst, nxen, nyst, nyen, nval, j, k, kout
      real xb, yb, d, dd, pow, vsky, vmax
Cbegin


      ht = 0.0001							!Default

      nxst = x - xbox/2.0						!Box position
      nxen = nxst + xbox - 1
      nyst = y - ybox/2.0
      nyen = nyst + ybox - 1

      if ( nxst.gt.NX .or. nxen.lt.1 .or. 				!If none of box
     +     nyst.gt.NY .or. nyen.lt.1 ) return				! in image, return

      nxst = max(1,min(NX,nxst))					!Find sky as
      nxen = max(1,min(NX,nxen))					! min valid value
      nyst = max(1,min(NY,nyst))					! in box. If all
      nyen = max(1,min(NY,nyen))					! invalid, sky=0
      nval = 0								! (sky<=32767)
      vsky = INT_MAXRR
      do k = nyst, nyen
         do j = nxst, nxen
            if ( im(j,k).ne.RINVAL ) then
               nval = nval + 1
               vsky = min(vsky,real(im(j,k)))
            endif
         enddo
      enddo
      if ( nval.eq.0 ) vsky = 0.0

      nxst = x - 1.0							!Find star height as
      nxen = nxst + 2 							! max valid value in
      nyst = y - 1.0							! 3x3 round star posn
      nyen = nyst + 2							! If that box in image
      nval = 0								! and has valid vals
      vmax = 0.0
      if ( nxst.le.NX .and. nyst.le.NY .and.
     +     nxen.ge.1  .and. nyen.ge.1 ) then
         nxst = max(1,min(NX,nxst))
         nxen = max(1,min(NX,nxen))
         nyst = max(1,min(NY,nyst))
         nyen = max(1,min(NY,nyen))
         do k = nyst, nyen
            do j = nxst, nxen
               if ( im(j,k).ne.RINVAL ) then
                  nval = nval + 1
                  vmax = max(vmax,real(im(j,k)))
               endif
            enddo
         enddo
      endif
      ht = vmax - vsky

C  If only invalid pixels in this box, then star is probably saturated.
C  And if oustide area, probably bright. So calculate rough height by
C  expanding box until there are valid pixels, and from distance of
C  these from input position, say what the peak height of the star is.

      if ( nval.eq.0 ) then
         kout = 0
         xb = 1.0
         yb = 1.0
         nval = 0
         do while ( nval.eq.0 .and. kout.lt.NX )
            kout = kout + 1
            xb = xb + 1.0
            yb = yb + 1.0
            nxst = x - xb
            nxen = x + xb
            nyst = y - yb
            nyen = y + yb
            vmax = 0.0
            if ( nxst.le.NX .and. nyst.le.NY .and.
     +           nxen.ge.1  .and. nyen.ge.1 ) then
               nxst = max(1,min(NX,nxst))
               nxen = max(1,min(NX,nxen))
               nyst = max(1,min(NY,nyst))
               nyen = max(1,min(NY,nyen))
               do k = nyst, nyen
                  do j = nxst, nxen
                     if ( im(j,k).ne.RINVAL ) then
                        vmax = max(vmax,real(im(j,k)))
                        nval = nval + 1
                     endif
                  enddo
              enddo
            endif
         enddo

         d = kout							!height from distance
         d = d/((rx+ry)/2.0)						! to 1st good value
         dd = d/((prx+pry)/2.0)						! + that value
         pow = min((p*(1.0+dd)*alog10(d)),10.0)
         ht = (vmax-vsky)*(1.0+(10.0**pow))
      endif

      ht = max(0.001,ht)						!Height must be +ve
      ht = min(1.0e7,ht)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SULIST -- Find companion stars
C     This reads through a list of star XY positions
C     and magnitudes, together with the star profile, and works out
C     which stars have companions close enough to affect photometry.
C     It makes for each star a list of those stars which are close
C     enough, and also the measuring box size for that star.
C     It stores all this back in the working array.
C
C      a j penny                  stsci               1987-02-25

      subroutine me_sulist ( res )

      implicit none
      include 'measure.inc'
      include 'STARMAN_INC'

      real	res(TBX,TBY)		!i/o: Working data array
C--
      integer j, k, bnst(SMAX), ja, jb, kd, ktot
      real bimps(SMAX), xbox, ybox, dx, dy, r, aimp, totimp, amag,
     +     bmag, adx, ady, dxm, dym, ahp, bhp, xpa, ypa
      logical inside, lookat, rcheck
      external rcheck
Cbegin


      if ( ST_FAILED ) return						!Failure check

      do k = 1, TBY							!Do for each star
       if ( rcheck(res(34,k),9) ) then

        xpa = res(1,k)							!Star profile
        ypa = res(2,k)

C  Put the star itself as the first one and clear the other SMAX-1 spaces
C  in the list.

        bnst(1) = k
        bimps(1) = 0.0
        totimp = 0.0

C Find and store total importance of companions. First empty work array

        call azeroi ( bnst(2), SMAX-1 )
        call amovkr ( 1.0, bimps(2), SMAX-1 )

C  Go through the XY list and find for each star its importance
C  according to the formula
C
C             Imp = mag(star) - mag(central star) - 6 + d/(star radius)
C
C  Thus the more important the star is the more -ve Imp is. If the
C  star is important enough to worry about (Imp less than 0)
C  then the star is inserted in the main star list with the more
C  important stars first. If more than seven stars are important
C  enough to get on the list, only the seven most important are saved

        bhp = res(49,k)
        if ( bhp.gt.0.1 ) then
          bmag = 30.0 - 2.5*alog10(bhp)
          do ja = 1, TBY

C  Do not do for itself or if outside area or if not important

            lookat = .true.
            if ( ja.eq.k ) lookat = .false.
            if ( .not.rcheck(res(34,ja),9) ) lookat = .false.
            ahp = res(49,ja)
            if ( ahp.lt.0.1 ) lookat = .false.
            xbox = max( res(29,ja), res(29,k) )
            ybox = max( res(30,ja), res(30,k) )
            dx = res(1,ja) - xpa
            dy = res(2,ja) - ypa
            if ( abs(dx).gt.3.0*abs(xbox) ) lookat = .false.
            if ( abs(dy).gt.3.0*abs(ybox) ) lookat = .false.

            if ( lookat ) then

               amag = 30.0 - 2.5*alog10(ahp)

               dxm = dx/(1.25*xbox/10.0)
               dym = dy/(1.25*ybox/10.0)
               r = sqrt(dxm*dxm+dym*dym)
               aimp = amag - bmag - 6.0 + r

               adx = abs(dx)
               ady = abs(dy)
               inside = .false.
               if ( adx.le.(xbox/2.0) .and. ady.le.(ybox/2.0) ) then
                  inside = .true.
                  aimp = min(0.0,aimp)
               endif

C                   If it is important, put it in the main star
C                   list at the right place

               if ( aimp.lt.0.0 .or. inside ) then
                  totimp = totimp - aimp
                  kd = 0
                  do jb = 2, SMAX
                     if ( kd.eq.0 .and. aimp.lt.bimps(jb) ) then
                        if ( jb.ne.SMAX ) then
                           do j = SMAX, jb+1, -1
                              bimps(j) = bimps(j-1)
                              bnst(j) = bnst(j-1)
                           enddo
                        endif
                        bimps(jb) = aimp
                        bnst(jb) = ja
                        kd = 1
                     endif
                  enddo
               endif

             endif
          enddo

          ktot = 0							!Load results array
          do j = 2, SMAX
            if ( j.le.8 ) res(20+j,k) = real(bnst(j))
            if ( j.gt.8 ) res(40+j-8,k) = real(bnst(j))
            if ( bnst(j).ne.0 ) ktot = ktot + 1
          enddo
          res(31,k) = totimp
          res(21,k) = ktot

        endif
       endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_NONECL -- Flag none as cleaned
C
C  a j penny              ral           1990-09-22

      subroutine me_nonecl ( res )

      implicit none
      include 'measure.inc'

      real    res(TBX,TBY) 			!i/o: results array
C--
      integer k
Cbegin


      do k = 1, TBY
         call wcheck ( res(34,k), 12, .false. )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CHEXTRA -- Check the effect of the extras on premeasured stars.
C
C  a j penny              stsci                 1987-03-01

      subroutine me_chextra ( res )

      implicit none
      include 'measure.inc'
      include 'STARMAN_INC'

      real    res(TBX,TBY) 			!i/o: results array
C--
      integer j, k, ja, numcomps
      logical again
Cbegin


      if ( ST_FAILED .or. .not. (BEFORE.and.EXTRA) ) return		!Check want to do

      do k = 1, TBYA							!Check done before stars
         numcomps = nint(res(21,k))					! to see if needed to do
         if ( numcomps.ne.0 ) then					! again because of extra
            again = .false.						! input; if so, change
            do j = 1, numcomps 						! flag to not yet done,
               if ( j.le.7 ) ja = nint(res(22+j-1,k))			! reset output for them.
               if ( j.gt.7 ) ja = nint(res(41+j-8,k))
               if ( ja.ne.0 .and. ja.gt.TBYA .and. ja.le.TBY ) then
                  again = .true.
               endif
            enddo
            if ( again ) then
               do j = 1, 15
                  call wcheck ( res(34,k), j, .false. )
                  call wcheck ( res(35,k), j, .false. )
               enddo
               call wcheck ( res(34,k), 9, .true. )
               res(3,k) = 50.0
               res(9,k) = 0.0
            endif
         endif
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_MAGS -- Fit Lorentzians to a list of stars
C  This is either done entirely in the main machine or the
C  actual calculation sent out to a farm of parallel processors.
C
C  It goes through the stars, one by one, doing undone ones,
C  fitting and storing results.
C
C  If a star is are flagged as having close companions, that star
C  and its companions are treated as one group and done together,
C  in a small box centred on the main star, and only the result
C  for the main star is recorded.
C
C  The stars are done in order of brightness (found roughly before).
C
C  As the starting posns and mags for the companions are taken from
C  the working list, any companion which has been done before
C  will have a better input posn, mag. But which stars ARE close
C  companions is calculated beforehand and is not affected by any
C  re-calculation in this s/r.
C
C  a j penny                   ral                 1990-01-28

      subroutine me_mags ( wim, res, inae )

      implicit none
      include 'measure.inc'
      include 'ST_IMAGE_INC'

      real      wim(NX,NY)		!i: Working image
      real	res(TBX,TBY)		!i/o: Results array
      real	inae(3,TBY)		!i: Original positions
C--
      integer k, lxsor, lysor, klxs, klys, jstar, ksent, kser, ksera,
     +        numpari, kn, nploop, ipkn, nleft, istat
      real xo, yo, baseo, basej
      logical toobig, pmore, doit
      character*79 texta

      logical me_stardo
      external me_stardo
Cbegin


      DFIRST = .true.							!Set up clean display
      DSECOND = .true.
      numpari = 1

      if ( DOPAR ) then
         call me_par_sii ( numpari, istat )				!Get no of par processors
         call gtwrki ( 'KN', TBY, ipkn, istat )
         call me_order ( res, %val(ipkn) )
         nleft = 0
         do k = 1, TBY
            if ( me_stardo(k,res) ) nleft = nleft + 1
         enddo
      else
         ipkn = 1
      endif

      call me_eheight ( wim, %val(IPRES) )				!Get rough heights for any
									! set zero by previous s/rs

      SHIFTF = 2.0							!Can move star only a bit

      call printo ( ' ' )						!Type header to user
      if ( NUMOP.eq.2 ) call printo (
     +                           'Doing undone ones on cleaned image' )
      if ( LOOPNUM.eq.2 ) call printo ( '2nd loop' )
      write ( texta, '( '' '', '' Star'', ''   Mag  '', '' Height '',
     +         ''  Dx  '', ''  Dy  '', ''  Chi '', '' Its '',
     +         ''Inval'', '' Error'', ''  Imp '', '' Diff-2'',
     +         ''   Damp'' )' )
      call printo ( texta )
      KPOUT = 0

      ksent = 0
      ksera = 0
      nploop = 0
      pmore = .true.							!Redo till all PAR done
      do while ( pmore )
      nploop = nploop + 1
      pmore = .false.
      kser = 0								!Do all wanted stars
      do while ( kser.lt.TBY .and. ksera.lt.NUMBER )

         call azerob ( SUCCESS, SMAX )					!Load 'not done'

         kser = kser + 1
         KSTAR = nint(res(37,kser))
         doit = .true.
CX         call me_pardo ( res, %val(ipkn), pmore, nploop, doit )

         if ( doit .and. me_stardo(KSTAR,res) ) then			!Do if not done and in image

            LX = nint(res(29,KSTAR))					!Box size
            LY = nint(res(30,KSTAR))
            LXS = res(1,KSTAR) - real(LX)/2.0				!Box blh corner posn
            LYS = res(2,KSTAR) - real(LY)/2.0

            lxsor = inae(1,KSTAR) - real(LX)/2.0
            lysor = inae(2,KSTAR) - real(LY)/2.0
            klxs = abs(LXS-lxsor)*10
            klys = abs(LYS-lysor)*10

            if ( LOOPNUM.eq.2 .and. nint(res(21,KSTAR)).eq.0 .and. 	!See if already done for
     +           nint(inae(3,KSTAR)).eq.0 .and. klxs.le.LX .and.	! single, stationary star
     +           klys.le.LY .and. nint(res(3,KSTAR)-50.0).ne.0 ) then
               call me_oldres ( res )					!Use previous result
            else
               ksera = ksera + 1
               call me_setmeas ( wim, %val(IPRWORK), %val(IPBWORK),	!Set for fit
     +                           res, %val(IPDOM), inae, baseo, istat )
               call me_toobig ( KSTAR, toobig )				!Check if too big for transputers
               if ( istat.ne.0 ) then					!All pixels bad
                  xo = inae(1,KSTAR)
                  yo = inae(2,KSTAR)
                  call me_result ( %val(IPTBOUT), res, xo, yo, baseo,
     +                             KSTAR, 1, 0 )
               elseif ( .not.DOPAR .or. toobig ) then
                  HTFIX = .true.
                  call me_fit ( %val(IPRWORK), %val(IPBWORK), 		!Main machine fit
     +                          %val(IPRMAP), 3, %val(IPFWORK), 1 )
                  HTFIX = .false.
                  call me_fit ( %val(IPRWORK), %val(IPBWORK), 		!Main machine fit
     +                          %val(IPRMAP), 30, %val(IPFWORK), 1 )
                  if ( .not.SUCCESS(1) ) then
                     call me_setmeas ( wim, %val(IPRWORK), 		!Set for fit
     +                    %val(IPBWORK),res, %val(IPDOM), inae,
     +                    baseo, istat )
                     call me_fit ( %val(IPRWORK), %val(IPBWORK),
     +                          %val(IPRMAP), 30, %val(IPFWORK), 2 )
                  endif
                  call me_dores ( KSTAR, baseo, 0, 0, 0, res,inae, kn )	!Store reply
               else							!Use par procs
                  call printo ( 'WARNING: Programmer error - HTFIX'//
     +                          ' not done' )
                  call printo ( '        Code needs rewriting: '//
     +                 ' contact person who wrote the program' )
                   if ( ksent.ge.numpari ) then				!If filled farm, wait
                     call me_parin ( basej, jstar )			! for any reply
                     call me_dores ( jstar, basej, 0, 1, 0,res,inae,kn)	!Store reply
                  endif
                  call me_parout ( %val(IPRWORK),%val(IPRMAP),baseo,30)	!Push calcs out to farm
                  ksent = ksent + 1
               endif
            endif

         endif

      enddo
      enddo

      if ( DOPAR ) then							!If using farm, get
         if ( ksent.eq.0 ) then   					! the last results
            call me_par_fend						! and store them
         else
            do k = 1, min(ksent,numpari)
               if ( k.eq.1 .and. ksent.lt.numpari ) call me_par_fend
               call me_parin ( basej, jstar )
               call me_dores ( jstar, basej, 0, 1, 0, res, inae, kn )	!Store reply
               if ( k.eq.1 .and. ksent.ge.numpari ) call me_par_fend
            enddo
         endif
      endif

      DLOCN = DLOCN + 1							!Tidy up any display clean
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 1 )
      DLOCN = DLOCN + 1
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 2 )

      call me_weedm ( res )						!Remove merges from
									! results lists
      if ( DOPAR ) call wrkcan ( 'KN' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OLDRES -- Put out the previous loop's result and clean
C
C  a j penny                      ral          1990-07-26

      subroutine me_oldres ( res )

      implicit none
      include 'measure.inc'

      real	res(TBX,TBY)			!i/o: Results array
C--
      real trunc, amag, ah, adx, ady, achi, aerr, aimp, adiffm, adamp
      external trunc
      integer ks, kiter, kninval
      character texta*79, rtype*2
Cbegin


      ks = KSTAR
      call me_setdone ( ks, .true., res, %val(IPTBOUT) )		!Set tried and done

      amag    = trunc(res(3,ks),2)					!Type out some fit values
      ah      = trunc(res(9,ks),5)
      adx     = trunc(res(4,ks),3)
      ady     = trunc(res(5,ks),3)
      achi    = trunc(res(7,ks),4)
      aerr    = trunc(res(50,ks),2)
      aimp    = trunc(res(31,ks),4)
      adiffm  = trunc(res(48,ks),2)
      adamp   = trunc(res(51,ks),2)
      kiter    = nint(res(6,ks))
      kninval  = nint(res(8,ks))
      rtype   = ' r'
      write ( texta, '(1x,i5,f7.2,f8.1,2f6.1,f7.1,i4,i5,f7.3,f7.2,
     +                 f7.4,f7.3,a2)')
     +        ks, amag, ah, adx, ady, achi, kiter, kninval, aerr,
     +        aimp, adiffm, adamp, rtype
      call printo ( texta )
      KPOUT = KPOUT + 1
      if ( KPOUT.eq.(20*(KPOUT/20))) then
         write ( texta, '( '' '', '' Star'', ''   Mag  '',
     +         '' Height '', ''  Dx  '', ''  Dy  '', ''  Chi '',
     +         '' Its '', ''Inval'', '' Error'', ''  Imp '',
     +         '' Diff-2'', ''   Damp'' )' )
         call printo ( texta )
      endif

      call me_clean ( ks, %val(IPDOM), res, 0 )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_TWOS -- Take 2 nearby undone stars and fit as one.
C  Looks at undone ones and if there are other undone ones within 4
C  radii, then eliminates those, and puts one star at the centre and fits.
C
C  a j penny                      stsci                 1987-02-25

      subroutine me_twos ( res, inae, krem )

      implicit none
      include 'measure.inc'

      real	res(TBX,TBY)			!i/o: Results array
      real	inae(3,TBY)			!i:   Posns, heights
      integer	krem(TBY)			!i/o: Flag for star removed
						!     (0=no;1=yes)
C--
      integer  k, kser, krema, numpari, ksent, jstar, kn, istat
      real     xo, yo, baseo, basej
      character*79 texta
      logical  found, toobig

      logical  me_stardo
      external me_stardo
Cbegin


      DFIRST = .true.							!Set up clean display
      DSECOND = .true.
      numpari = 1

      if ( DOPAR ) call me_par_sii ( numpari, istat )			!Get no of par processors

      call me_eheight ( %val(IPCLEAN), res )				!Estimate rough heights of
									! failed stars

      SHIFTF = 3.5							!Can move star a bit

      call printo ( ' ' )						!Type header to user
      call printo ( 'Collapsing undone pairs to single stars' )
      if ( LOOPNUM.eq.2 ) call printo ( '2nd loop' )
      write ( texta, '( '' '', '' Star'', ''   Mag  '', '' Height '',
     +          ''  Dx  '', ''  Dy  '', ''  Chi '', '' Its '',
     +          ''Inval'', '' Comps'', ''  Imp '', '' Diff-2'',
     +          ''   Damp'' )' )
      call printo ( texta )
      KPOUT = 0

C  Go through stars doing undone ones, fitting and storing results
C  Only do for set number.

      kser = 0
      ksent = 0
      do while ( kser.lt.TBY .and. kser.lt.NUMBER )			!Do all wanted stars

         kser = kser + 1						!Check number
         KSTAR = nint(res(37,kser))
         call azerob ( SUCCESS, SMAX )

         if ( me_stardo(KSTAR,res) ) then				!If star in image and not
									! yet done and has an
            call me_remcomp ( KSTAR, res, %val(IPTBOUT), found, krema)	! undone companion close,
            krem(KSTAR) = krema						! do it, removing star
									! from companion list
            if ( found ) then

               LX = nint(res(29,KSTAR))					!Box size
               LY = nint(res(30,KSTAR))

               call me_setmeas ( %val(IPCLEAN), %val(IPRWORK), 		!Get the data from the
     +                           %val(IPBWORK), res, %val(IPDOM), inae,
     +                           baseo, istat )				! image and measure star
               call me_toobig ( KSTAR, toobig )				!Check if too big for transputers
               if ( istat.ne.0 ) then					!All pixels bad
                  xo = inae(1,KSTAR)
                  yo = inae(2,KSTAR)
                  call me_result ( %val(IPTBOUT), res, xo, yo, baseo,
     +                             KSTAR, 1, 0)
               elseif ( .not.DOPAR .or. toobig ) then
                  HTFIX = .true.
                  call me_fit ( %val(IPRWORK), %val(IPBWORK), 		!Main machine fit
     +                          %val(IPRMAP), 3, %val(IPFWORK), 1 )
                  HTFIX = .false.
                  call me_fit ( %val(IPRWORK), %val(IPBWORK), 		!Main machine fit
     +                          %val(IPRMAP), 30, %val(IPFWORK), 1 )
                  if ( .not.SUCCESS(1) ) then
                     call me_setmeas ( %val(IPCLEAN), %val(IPRWORK), 		!Set for fit
     +                    %val(IPBWORK),res, %val(IPDOM), inae,
     +                    baseo, istat )
                     call me_fit ( %val(IPRWORK), %val(IPBWORK),
     +                          %val(IPRMAP), 30, %val(IPFWORK), 2 )
                  endif
                  call me_dores ( KSTAR, baseo, 0, 0, 0, res, inae, kn)	!Store reply
                  call me_remstar ( res, %val(IPTBOUT), inae, 		!Remove 'removed' star
     +                              krem(KSTAR) )
               else
                  if ( ksent.ge.numpari ) then				!If filled farm, wait
                     call me_parin ( basej, jstar )			! for next reply
                     call me_dores ( jstar, basej, 0, 1, 0, res, inae,	!Store reply
     +                               kn )
                     call me_remstar ( res, %val(IPTBOUT), inae, 	!Remove 'removed' star
     +                                 krem(jstar) )
                  endif
                  call me_parout ( %val(IPRWORK),%val(IPRMAP),baseo,30)	!Push calcs out to farm
                  ksent = ksent + 1
               endif

            endif

         else
            call me_setdone ( KSTAR, .false., res, %val(IPTBOUT) )	!Flag star as having been
         endif								! looked at

      enddo

      if ( DOPAR ) then							!Get last results from farm
         if ( ksent.eq.0 ) then
            call me_par_fend
         else
            do k = 1, min(ksent,numpari)
               if ( k.eq.1 .and. ksent.lt.numpari ) call me_par_fend
               call me_parin ( basej, jstar )
               call me_dores ( jstar, basej, 0, 1, 0, res, inae, kn ) 	!Store reply
               call me_remstar ( res, %val(IPTBOUT), inae, krem(jstar))	!Remove 'removed' star
               if ( k.eq.1 .and. ksent.ge.numpari ) call me_par_fend
            enddo
         endif
      endif

      DLOCN = DLOCN + 1							!Tidy up any display clean
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 1 )
      DLOCN = DLOCN + 1
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 2 )

      call me_weedm ( res )						!Remove merges

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_STADD -- Cancel a star and restore it to a cleaned image
C
C  a j penny                      ral                 1990-02--25

      subroutine me_stadd ( ks, tdomap, res, out, inae )

      implicit none
      include 'measure.inc'
      include 'ST_IMAGE_INC'

      integer	ks		!i:   Star number
      logical	tdomap(TBY)	!i:   Flags for use map for a star
      real	res(TBX,TBY)	!i/o: Results array
      real	out(TBXVO,TBY)	!o:   Output results array
      real	inae(3,TBY)	!i:   Original positions
C--
      integer smapnum, smagnif, smapx, smapy, ierr, kw(4)
      real x, y, h, sprof(9)
      logical doit, sdomap, rcheck
      external rcheck
Cbegin


      doit = .false.							!Add star to image?
      if ( DODISP .or. REDOCL .or. CLSTORE .or. TWOS ) doit = .true.
      if ( rcheck(res(33+LOOPNUM,ks),1) ) doit = .false.
      if ( .not.rcheck(res(33+LOOPNUM,ks),2*NUMOP+1) ) doit = .false.
      if ( .not.rcheck(res(34,ks),9) ) doit = .false.

      if ( doit ) then							!This star wanted?
         call amovr ( res(12,ks), sprof, 9 )				!Add to clean image
         smapnum = nint(res(11,ks))
         smagnif = nint(res(40,ks))
         smapx = nint(res(38,ks))
         smapy = nint(res(39,ks))
         x = res(1,ks)
         y = res(2,ks)
         h = res(9,ks)
         sdomap = tdomap(ks)
         call popamr ( %val(IPCLEAN), NX, NY, 1.0, RINVAL, x, y, h,
     +                 sprof, %val(IPRMAP), MX, MY, MZ, smapnum,
     +                 smapx, smapy, smagnif, sdomap, ierr, kw )
         call wcheck ( res(34,ks), 12, .false. )
      endif

      res(1,ks) = inae(1,ks)
      res(2,ks) = inae(2,ks)
      res(3,ks) = 50.0
      res(4,ks) = 0.0
      res(5,ks) = 0.0
      res(9,ks) = 0.0
      res(32,ks) = 0.0
      res(33,ks) = 0.0
      res(48,ks) = 0.0
      res(49,ks) = 0.0
      res(50,ks) = 0.0

      call amovr ( res(1,ks),  out(1+5,ks),  5 )			!Copy to output
      call amovr ( res(9,ks),  out(9+5,ks),  1 )
      call amovr ( res(32,ks), out(32+5,ks), 4 )
      call amovr ( res(48,ks), out(48+5,ks), 3 )

      call me_setdone ( ks, .true., res, out )				!Set tried and done

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_DORES -- Arrange storage of result and clean of image
C
C  a j penny                      ral                 1990-02--25

      subroutine me_dores ( ks, abase, kf1, kf2, kf3, res, inae, kn )

      implicit none
      include 'measure.inc'

      integer	ks		!i: Star number
      real	abase		!i: Base to apply
      integer	kf1		!i: Flag for successful fit (0/1=y/n)
      integer	kf2		!i: Flag for single/par proc (0/1)
      integer	kf3		!i: Flag for clean (0/1=y/n)
      real	res(TBX,TBY)	!i/o: Results array
      real	inae(3,TBY)	!i: Original positions
      integer	kn		!o: Flag (0=ok,cleaned: -1=not ok, not
				!   cleaned: +ve=ok, cleaned, another [kn]
				!   restored)
C--
      real xo, yo, hpa, xpa, ypa, xa, ya, ab
      integer kd
      logical brighter
      character text*72
Cbegin


      xo = inae(1,ks)
      yo = inae(2,ks)

      call me_chclose ( ks, res, kd, brighter, kf2 )			!See about another close star

      if ( kd.eq.0 ) then						!No close star

         kn = 0
         call me_result ( %val(IPTBOUT), res, xo, yo, abase,ks,kf1,kf2)
         if ( kf3.eq.0 ) call me_clean ( ks, %val(IPDOM), res, kf2 )

      elseif ( brighter ) then						!This star fainter than previous

         kn = -1*kd
         write ( text, '('' Star no '', i5, '' not used as too close '',
     +                '' to previously fitted star no '',i5)' ) ks, kd
         call printo ( text )
         if ( kf2.eq.0 ) then
            hpa = HP(1)
            xpa = XP(1) + LXS - 1.0
            ypa = YP(1) + LYS - 1.0
         else
            hpa = P_HP(1)
            xpa = P_XP(1) + P_LXS - 1.0
            ypa = P_YP(1) + P_LYS - 1.0
         endif
         write ( text, '(1x,i5,f14.1,2x,2f12.2)' ) ks, hpa, xpa, ypa
         call printo ( text )
         write ( text, '(1x,i5,f14.1,2x,2f12.2)' ) kd, res(9,kd),
     +                                          res(1,kd), res(2,kd)
         call printo ( text )

         call wcheck ( res(34,ks), 10, .true. )				!Flag star as having merged

         call me_result ( %val(IPTBOUT), res, xo, yo, abase, ks, 1,kf2)

      else
         kn = kd
         write ( text, '('' Star no '', i5, '' set to zero - too '',
     +         ''close to new brighter star no '',i5)' ) kd, ks
         call printo ( text )
         write ( text, '(1x,i5,f14.1,2x,2f12.2)' ) kd, res(9,kd),
     +                                          res(1,kd), res(2,kd)
         call printo ( text )
         if ( kf2.eq.0 ) then
            hpa = HP(1)
            xpa = XP(1) + LXS - 1.0
            ypa = YP(1) + LYS - 1.0
         else
            hpa = P_HP(1)
            xpa = P_XP(1) + P_LXS - 1.0
            ypa = P_YP(1) + P_LYS - 1.0
         endif
         write ( text, '(1x,i5,f14.1,2x,2f12.2)' ) ks, hpa, xpa, ypa
         call printo ( text )

         call wcheck ( res(34,kd), 10, .true. )				!Flag star as having merged

         if ( kf3.eq.0 ) then
            call me_stadd ( kd, %val(IPDOM), res, %val(IPTBOUT), inae )
            call me_clean ( ks, %val(IPDOM), res, kf2 )
         endif
         call me_result ( %val(IPTBOUT), res, xo, yo, abase, ks, 0,kf2)
         xa = res(1,kd)
         ya = res(2,kd)
         ab = res(10,kd)
         call me_result ( %val(IPTBOUT), res, xa, ya, ab, kd, 1, kf2 )
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_DOVOL -- Do the volume calculations for all the stars
C
C  a j penny                     stsci                  1987-02-01

      subroutine me_dovol ( res, tdomap )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      real	res(TBX,TBY)		!i/o: Results array
      logical	tdomap(TBY)		!i:   Flags for use map for a star
C--
      integer j, ja, smapx, smapy, smapnum, smagnif
      logical voldone, same, sdomap
      real sprof(9), vol
      logical rcheck
      external rcheck
Cbegin


      if ( ST_FAILED ) return						!Failure check

      do KSTAR = 1, TBY							!Do all the stars

         if ( rcheck(res(34,KSTAR),9) ) then				!See if in image

            call amovr ( res(12,KSTAR), sprof, 9 )			!See if same profile as
            smapnum = nint(res(11,KSTAR))				! one already done
            smapx   = nint(res(38,KSTAR))
            smapy   = nint(res(39,KSTAR))
            smagnif = nint(res(40,KSTAR))
            sdomap  = tdomap(KSTAR)

            voldone = .false.
            ja = 0
            do while ( .not.voldone .and. ja.lt.TBY )
               ja = ja + 1
               if (res(36,ja).ne.0.0) then
                  if ( abs(res(11,KSTAR)-res(11,ja)).lt.0.1 ) then
                     same = .true.
                     do j = 1 ,9
                        if ( sprof(j).ne.res(11+j,ja) ) same = .false.
                     enddo
                     if ( smapnum.ne.nint(res(11,ja)) ) same = .false.
                     if ( same ) voldone = .true.
                  endif
               endif
            enddo

            if ( voldone ) then						!If done copy it, else
               vol = res(36,ja)						! calc it. Then store it.
            else
               call cvolume ( sprof, sdomap, %val(IPRMAP), MX, MY, MZ,
     +                     smapnum, smapx, smapy, smagnif, RADIUS, vol)
            endif
            res(36,KSTAR) = vol

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_BEFCLEAR -- Remove all stars that have a 'pure before' fit from an image
C    Only those with the desired bit set in res(35,k), and height > 0.0.
C
C  a j penny                     ral               1990 sep 22

      subroutine me_befclear ( res )

      implicit none
      include 'measure.inc'

      real	res(TBX,TBY)		!i/o: Results array
C--
      integer  k, kcl
      logical  rcheck
      external rcheck
Cbegin


      call printo ( 'Cleaning before stars' )
      DFIRST = .true.							!Set up clean display
      DSECOND = .true.

      kcl = 0
      do k = 1, TBY
         if ( rcheck(res(34,k),11) ) then
            call me_clean ( k, %val(IPDOM), res, 2 )
            kcl = kcl + 1
         endif
      enddo

      DLOCN = DLOCN + 1							!Tidy up any display clean
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 1 )
      DLOCN = DLOCN + 1
      if ( DLOCN.eq.4 ) DLOCN = 1
      call me_cleana ( 2 )

      if ( kcl.eq.0 ) then
         call printo ( ' - No before stars cleaned' )
      else
         call pargi ( kcl )
         call printd ( ' - %d before stars cleaned' )
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CLEAN -- Remove a fitted star from an image
C    Only those with the desired bit set in res(35,k), and height > 0.0.
C
C  a j penny                     stsci                  1987-02-01

      subroutine me_clean ( ks, tdomap, res, km )

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'

      integer   ks			!i:   Star number
      logical	tdomap(TBY)		!i:   Flags for use map for a star
      real	res(TBX,TBY)		!i/o: Results array
      integer   km			!i:   Fitting method (0=single;1=par;
					!     2=file)
C--
      integer  smapnum, smagnif, smapx, smapy, kw(4), ierr
      real     x, y, h, sprof(9)
      logical  sdomap
      logical  me_cleando
      external me_cleando
Cbegin


      if ( .not.DODISP .and. .not.REDOCL .and. .not.CLSTORE .and.	!Wanted in general?
     +     .not.TWOS ) return

      if ( .not.me_cleando(ks,res) ) return				!This star wanted?

      call amovr ( res(12,ks), sprof, 9 )				!Clean image
      smapnum = nint(res(11,ks))
      smagnif = nint(res(40,ks))
      smapx = nint(res(38,ks))
      smapy = nint(res(39,ks))
      x = res(1,ks)
      y = res(2,ks)
      h = res(9,ks)
      sdomap = tdomap(ks)
      call popsmr ( %val(IPCLEAN), NX, NY, 1.0, RINVAL, x, y, h, sprof,
     +              %val(IPRMAP), MX, MY, MZ, smapnum, smapx, smapy,
     +              smagnif, sdomap, ierr, kw )
      call wcheck ( res(34,ks), 12, .true. )
      if ( ierr.ne.0 ) return

      if ( .not.DODISP ) return						!Displaying?

      if ( DFIRST ) DLOCN = 3						!Start counter

      DLOCN = DLOCN + 1
      if ( DLOCN.eq.4 ) DLOCN = 1

      DLOC(DLOCN,1) = kw(1)						!Store posn
      DLOC(DLOCN,2) = kw(2)
      DLOC(DLOCN,3) = kw(3)
      DLOC(DLOCN,4) = kw(4)

      if ( km.eq.0 ) then						!Store blue box posn
         DLOCB(DLOCN,1) = LXS
         DLOCB(DLOCN,2) = LXS + LX - 1
         DLOCB(DLOCN,3) = LYS
         DLOCB(DLOCN,4) = LYS + LY - 1
      elseif ( km.eq.1 ) then
         DLOCB(DLOCN,1) = P_LXS
         DLOCB(DLOCN,2) = P_LXS + P_LX - 1
         DLOCB(DLOCN,3) = P_LYS
         DLOCB(DLOCN,4) = P_LYS + P_LY - 1
      elseif ( km.eq.2 ) then
         DLOCB(DLOCN,1) = res(1,ks) - res(29,ks)/2.0
         DLOCB(DLOCN,2) = res(1,ks) + res(29,ks)/2.0 - 1.0
         DLOCB(DLOCN,3) = res(2,ks) - res(30,ks)/2.0
         DLOCB(DLOCN,4) = res(2,ks) + res(30,ks)/2.0 - 1.0
      endif

      call me_cleana ( 0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CLEANA -- Display of cleaning
C
C  a j penny                     stsci                  1987-02-01

      subroutine me_cleana ( kopt )

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer	kopt		!i: Flag as to which to do
C--
      integer k, kxs, kxe, kys, kye
Cbegin


      if ( .not.DODISP .and. .not.REDOCL .and. .not.CLSTORE .and.	!Wanted in general?
     +     .not.TWOS ) return

      if ( .not.DSECOND ) then						!Clear last but one area
         if ( DLOCN.eq.1 ) k = 2
         if ( DLOCN.eq.2 ) k = 3
         if ( DLOCN.eq.3 ) k = 1
         kxs = nint(min(DLOC(k,1),DLOCB(k,1))) - 2*DSCOMFX
         kxs = max(1,min(NX,(kxs)))
         kxe = nint(max(DLOC(k,2),DLOCB(k,2))) + 2*DSCOMFX
         kxe = max(1,min(NX,(kxe)))
         kys = nint(min(DLOC(k,3),DLOCB(k,3))) - 2*DSCOMFY
         kys = max(1,min(NY,(kys)))
         kye = nint(max(DLOC(k,4),DLOCB(k,4))) + 2*DSCOMFY
         kye = max(1,min(NY,(kye)))
         call me_idisp ( %val(IPCLEAN), NX, NY, kxs, kxe, kys, kye )
      endif

      if ( .not.DFIRST .and. (kopt.eq.0 .or. kopt.eq.1)  ) then		!Clear last area
         if ( DLOCN.eq.1 ) k = 3
         if ( DLOCN.eq.2 ) k = 1
         if ( DLOCN.eq.3 ) k = 2
         kxs = nint(min(DLOC(k,1),DLOC(k,1))) - 2*DSCOMFX
         kxs = max(1,min(NX,(kxs)))
         kxe = nint(max(DLOC(k,2),DLOC(k,2))) + 2*DSCOMFX
         kxe = max(1,min(NX,(kxe)))
         kys = nint(min(DLOC(k,3),DLOC(k,3))) - 2*DSCOMFY
         kys = max(1,min(NY,(kys)))
         kye = nint(max(DLOC(k,4),DLOC(k,4))) + 2*DSCOMFY
         kye = max(1,min(NY,(kye)))
         call me_idisp ( %val(IPCLEAN), NX, NY, kxs, kxe, kys, kye )
         call ds_box ( DLOC(k,1), DLOC(k,2), DLOC(k,3), DLOC(k,4), 1 )	!Add new red and green boxes
         call ds_box ( DLOCB(k,1),DLOCB(k,2),DLOCB(k,3),DLOCB(k,4), 2 )
      endif

      if ( kopt.eq.0 ) call ds_box ( DLOCB(DLOCN,1), DLOCB(DLOCN,2), 	!Paint blue box
     +                              DLOCB(DLOCN,3), DLOCB(DLOCN,4), 3 )

      if ( .not.DFIRST ) DSECOND = .false.				!Update start checks
      DFIRST = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SETMEAS -- Set up Lorentz fit
C    Given up to 15 stars from an XY list which are somewhere in
C    the image, it copies the appropriate part of the frame into a
C    working area, and sets up the parameters for the standard fitting
C    subroutine.
C
C  a j penny                   stsci                       1987-02-25

      subroutine me_setmeas ( im, imuse, imuseb, res, tdomap,
     +                        inae, baseo, ok )

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)		!i:   Input image
      real      imuse(LX,LY)		!o:   Area of input image to use
      logical	imuseb(LX,LY)		!o:   Flagged pixels of 'imuse'
      real	res(TBX,TBY)		!i/o: Results array
      logical	tdomap(TBY)		!i:   Flags for use of profile maps
      real	inae(3,TBY)		!i:   Original posns, heights
      real	baseo			!o:   Starting fit base level
      integer   ok			!o:   Bad area flag (0=ok;1=bad-all
					!     invalid, all out of image)
C--
      real var, rv
CX      integer ns(SMAX)
      integer j, k, lxe, lye, lxsor, lysor, ja
      logical doit

      logical rcheck
      external rcheck
Cbegin


      LXS = res(1,KSTAR) - real(LX)/2.0					!Get box position
      lxe = lxs + LX - 1
      LYS = res(2,KSTAR) - real(LY)/2.0
      lye = lys + LY - 1

CX      ns(1) = KSTAR
      HPI(1) = res(49,KSTAR)						!Get the star positions
      XPI(1) = res(1,KSTAR) - LXS + 1					! and heights in terms of
      YPI(1) = res(2,KSTAR) - LYS + 1					! this area
      XPOR(1) = inae(1,KSTAR) - LXS + 1
      YPOR(1) = inae(2,KSTAR) - LYS + 1
      call amovr ( res(12,KSTAR), PROF(1,1), 9 )
      MAPNUM(1) = nint(res(11,KSTAR))
      MAPX(1) = nint(res(38,KSTAR))
      MAPY(1) = nint(res(39,KSTAR))
      MAGNIF(1) = nint(res(40,KSTAR))
      CDOMAP(1) = tdomap(KSTAR)

      NST = res(21,KSTAR) + 1						!Get total star no in fit

      if ( NST.ge.2 ) then						!Load companions
         ja = 1
         do k = 2, NST
            if ( k.le.8 ) j = nint(res(20+k,KSTAR))			! to be done
            if ( k.gt.8 ) j = nint(res(40+k-8,KSTAR))
            doit = .true.

            if ( rcheck(res(33+LOOPNUM,j),1) ) doit = .false. 		!do star?
            if ( rcheck(res(34,j),10) ) doit = .false.			!Merged?
            if ( NUMOP.ge.2 .and. rcheck(res(34,j),12) ) doit = .false.	!Cleaned from image?
            if ( doit ) then
               ja = ja + 1
CX               ns(ja) = j
               HPI(ja) = res(49,j)
               XPI(ja) = res(1,j) - LXS + 1
               YPI(ja) = res(2,j) - LYS + 1
               XPOR(ja) = inae(1,j) - LXS + 1
               YPOR(ja) = inae(2,j) - LYS + 1
               call amovr ( res(12,j), PROF(1,ja), 9 )
               MAPNUM(ja) = nint(res(11,j))
               MAPX(ja) = nint(res(38,j))
               MAPY(ja) = nint(res(39,j))
               MAGNIF(ja) = nint(res(40,j))
               CDOMAP(ja) = tdomap(j)
            endif
         enddo
         NST = ja
      endif

      call amovkb ( .true., imuseb, LX*LY )				!Load default bad pixel flags

      BASE = 0.0							!Load default base
      XSLOPE = 0.0
      YSLOPE = 0.0
      baseo = BASE

      ok = 1
      if ( lxe.le.LXS .or. lye.le.LYS .or.				!Check to see if box in
     +     lxs.ge.NX  .or. lxe.le.1   .or.				! main image, and is at
     +     lys.ge.NY  .or. lye.le.1         ) return			! least 2x2
      ok = 0

      call coprr ( im, NX, NY, LXS, lxe, LYS, lye, imuse, LX, LY, 1, 1)	!Copy the selected
      NINVAL = 0							! region into working area
      do k = 1, LY				             		! and scale
         do j = 1, LX
            if ( imuse(j,k).eq.RINVAL ) then
               imuseb(j,k) = .false.
               NINVAL = NINVAL + 1
            else
               imuse(j,k) = imuse(j,k)
            endif
         enddo
      enddo

      if ( NINVAL.eq.(LX*LY) ) then
         ok = 1
         return
      endif

      if ( LOOPNUM.eq.1 ) then						!Sky level
         rv = 1.0e10
         do k = 1, LY
            do j = 1, LX
               if ( imuseb(j,k) ) rv = min(rv,imuse(j,k))
            enddo
         enddo
         BASE = rv
         var = max(0.0001,(ZGAIN*BASE+ZNOISE*ZNOISE))
         BASE = BASE + sqrt(var)
         XSLOPE = 0.0
         YSLOPE = 0.0
      else
         BASE =  res(10,KSTAR)
         XSLOPE = res(32,KSTAR)
         YSLOPE = res(33,KSTAR)
         lxsor = inae(1,KSTAR) - real(lx)/2.0
         lysor = inae(2,KSTAR) - real(ly)/2.0
         BASE = BASE + real(LXS-lxsor)*XSLOPE + real(LYS-lysor)*YSLOPE
      endif
      baseo = BASE


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_RESULT -- Store the fit result fit and type out a line of result
C
C   a j penny                        stsci                 87-03-06

      subroutine me_result ( out, res, xo, yo, baseo, ks, ok, kt )

      implicit none

      include 'measure.inc'

      real	out(TBXVO,TBY)		!o:   Output results array
      real	res(TBX,TBY)		!i/o: Results array
      real	xo			!i:   Starting X posn
      real	yo			!i:   Starting Y posn
      real	baseo			!i:   Fitted base level
      integer   ks			!i:   Star that has been fitted
      integer   ok			!i:   Flag that fit has been done
					!     (0=yes;1=no)
      integer   kt			!i:   Flag for where results come
					!     from (0=single;1=prallel procs)
C--
      integer j, k, jin, num
      real amag, ah, adx, ady, achi, ar, aimp, adiffm, aerr, adamp
      logical doneit
      character texta*79, rtype*2, text*72

      integer r_iter, r_ninval, r_lxs, r_lys
      real    r_hp(SMAX), r_xp(SMAX), r_yp(SMAX), r_base, r_xslope,
     +        r_yslope, r_diffm, r_errm, r_rchi, r_damp
      logical r_success

      real trunc
      external trunc
Cbegin


      if ( kt.eq.0 ) then						!Single proc results
         rtype = ' s'
         r_success = SUCCESS(1)
         r_hp(1)   = HP(1)
         r_xp(1)   = XP(1)
         r_yp(1)   = YP(1)
         r_lxs     = LXS
         r_lys     = LYS
         r_rchi    = RCHI
         r_ninval  = NINVAL
         r_base    = BASE
         r_xslope  = XSLOPE
         r_yslope  = YSLOPE
         r_diffm   = DIFFM
         r_errm    = ERRM
         r_damp    = RDAMP
         r_iter    = ITER
         num = max(0,min(14,nint(res(21,ks))))
         if ( num.ge.1) then
            do j = 2, num + 1
               r_hp(j) = HP(j)
               r_xp(j) = XP(j)
               r_yp(j) = YP(j)
            enddo
         endif
      else							!Parallel results
         rtype = ' p'
         r_success = P_SUCCESS(1)
         r_hp(1)   = P_HP(1)
         r_xp(1)   = P_XP(1)
         r_yp(1)   = P_YP(1)
         r_lxs     = P_LXS
         r_lys     = P_LYS
         r_rchi    = P_RCHI
         r_ninval  = P_NINVAL
         r_base    = P_BASE
         r_xslope  = P_XSLOPE
         r_yslope  = P_YSLOPE
         r_diffm   = P_DIFFM
         r_errm    = P_ERRM
         r_damp    = P_RDAMP
         r_iter    = P_ITER
         num = max(0,min(14,nint(res(21,ks))))
         if ( num.ge.1) then
            do j = 2, num + 1
               r_hp(j) = P_HP(j)
               r_xp(j) = P_XP(j)
               r_yp(j) = P_YP(j)
            enddo
         endif
      endif

      doneit = .false.
      if ( ok.eq.0 .and. r_success .and. r_hp(1).ne.0.0 ) then
         amag = r_hp(1)*res(36,ks)					!Fit ok, so store
         if ( amag.gt.1.0e-8 ) then
            amag = 30.0 - 2.5*alog10(amag)
            doneit = .true.
         else
            amag = 50.0
         endif
         res(1,ks) = r_xp(1) + real(r_lxs) - 1.0
         res(2,ks) = r_yp(1) + real(r_lys) - 1.0
         res(3,ks) = amag
         res(4,ks) = res(1,ks) - xo
         res(5,ks) = res(2,ks) - yo
         res(6,ks) = real(r_iter)
         res(7,ks) = r_rchi
         res(8,ks) = r_ninval
         res(9,ks) = r_hp(1)
         res(10,ks) = r_base
         res(32,ks) = r_xslope
         res(33,ks) = r_yslope
         res(48,ks) = r_diffm
         res(49,ks) = res(49,ks)
         res(50,ks) = r_errm
         res(51,ks) = r_damp
      else
         res(1,ks) = xo						!Fit bad, set null params
         res(2,ks) = yo
         res(3,ks) = 50.0
         res(4,ks) = 0.0
         res(5,ks) = 0.0
         res(6,ks) = r_iter
         res(7,ks) = r_rchi
         res(8,ks) = r_ninval
         res(9,ks) = 0.0
         res(10,ks) = baseo
         res(32,ks) = 0.0
         res(33,ks) = 0.0
         res(48,ks) = r_diffm
         res(49,ks) = res(49,ks)
         res(50,ks) = 0.0
         res(51,ks) = r_damp
      endif

      do j = 1, TBX							!Check on limits to fit
         ar = res(j,ks)
         if ( abs(ar).gt.999999.0 ) ar = sign(999999.0,ar)
         res(j,ks) = ar
      enddo

      call amovr ( res(1,ks),  out(1+5,ks),  10 )			!Copy to output
      call amovr ( res(32,ks), out(32+5,ks),  4 )
      call amovr ( res(48,ks), out(48+5,ks),  4 )

      call me_setdone ( ks, doneit, res, out )				!Set tried and done or not

      amag    = trunc(res(3,ks),2)					!Type out some fit values
      ah      = trunc(res(9,ks),5)
      adx     = trunc(res(4,ks),3)
      ady     = trunc(res(5,ks),3)
      achi    = trunc(res(7,ks),4)
      aerr    = trunc(res(50,ks),2)
      aimp    = trunc(res(31,ks),4)
      adiffm  = trunc(res(48,ks),2)
      adamp   = trunc(res(51,ks),2)
      write ( texta, '(1x,i5,f7.2,f8.1,2f6.1,f7.1,i4,i5,f7.3,f7.2,
     +                 f7.4,f7.3,a2)')
     +        ks, amag, ah, adx, ady, achi, r_iter, r_ninval, aerr,
     +        aimp, adiffm, adamp, rtype
      call printo ( texta )
      KPOUT = KPOUT + 1
      if ( KPOUT.eq.(20*(KPOUT/20))) then
         write ( texta, '( '' '', '' Star'', ''   Mag  '',
     +         '' Height '', ''  Dx  '', ''  Dy  '', ''  Chi '',
     +         '' Its '', ''Inval'', '' Error'', ''  Imp '',
     +         '' Diff-2'', ''   Damp'' )' )
         call printo ( texta )
      endif

      if ( INFORM.gt.2 ) then						!Type full result, if doing
         num = max(0,min(14,nint(res(21,ks))))
         if ( num.ge.1) then
            do j = 1, num
               jin = 22 + j - 1
               if ( j.gt.7 ) jin = 41 + j - 8
               k = nint(res(jin,ks))
               ah = r_hp(j+1)
               adx = r_xp(j+1) - r_xp(1)
               ady = r_yp(j+1) - r_yp(1)
               ah =  trunc(ah,5)
               adx = trunc(adx,5)
               ady = trunc(ady,5)
               write ( text, '(6x,i5,f9.1,2f8.2)' ) k, ah, adx, ady
               call printo ( text )
            enddo
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SORTL -- Sort an order list of decreasing star heights
C     puts it into column 37 of the working array
C
C    a j penny               stsci                   1987-02-25

      subroutine me_sortl ( res, tempa, tempb )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      real	res(TBX,TBY)			!i/o: working array
      real	tempa(TBY)			!o: work space
      real	tempb(TBY)			!o: work space
C--
      integer k
      real temp
      logical rcheck
      external rcheck
Cbegin


      if ( ST_FAILED ) return						!Failure check

      do k = 1, TBY							!Load heights into temp array, set
         tempa(k) = res(49,k)						! up start order. If no height,
         if ( .not.rcheck(res(34,k),9) ) tempa(k) = -1.0		! height, then load as very small
         tempb(k) = k							! (-1 in fact)
      enddo

      call sort2r ( tempa, tempb, TBY ) 				!sort heights (ascending order)

      do k = 1, TBY/2							!put the posns in list, into
         temp = tempb(k)						!descending height order
         tempb(k) = tempb(TBY+1-k)
         tempb(TBY+1-k) = temp
      enddo

      do k = 1, TBY							!Load posns to working array
         res(37,k) = tempb(k)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CPBEFORE -- Load before measures for valid value (mag<49) stars
C
C   a j penny                 dao           1988-04-25

      subroutine me_cpbefore ( bef, res )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      real	bef(TBXVB,TBYB)		!i:   Stars measured before results
					!     array
      real	res(TBX,TBY)		!i/o: Results array
C--
      integer k
Cbegin


      if ( .not. BEFORE .or. ST_FAILED ) return				!Error check

      do k = 1, TBYB							!Load them
         if ( res(8,k).lt.49.0 ) then
            call amovr ( bef(6,k), res(1,k), TBXVB-5 )
            call wcheck ( res(34,k), 11, .true. )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_WEEDM -- Remove stars that have merged with another
C
C   a j penny                stsci               1987-03-2

      subroutine me_weedm ( res )

      implicit none
      include 'measure.inc'

      real	res(TBX,TBY)		!i/o: Results array
C--
      integer k, j, kc
      real dx, dy, arad, dd, ra, xa, ya
      character text*72
Cbegin


      call printo ( 'Weeding out close stars' )
      kc = 0
      do k = 1, TBY
         if ( res(3,k).lt.49.0 ) then
            ra = res(12,k) + res(13,k)
            xa = res(1,k)
            ya = res(2,k)
            do j = 1, TBY
               if ( j.ne.k .and. res(3,j).lt.49.0 ) then
                  arad = (ra+res(12,j)+res(13,j))/4.0
                  dx = xa - res(1,j)
                  dy = ya - res(2,j)
                  dd = sqrt(0.000001 + dx*dx + dy*dy)
                  if ( dd.lt.0.3*arad .and. res(9,j).lt.res(9,k) ) then
                     call me_remstar ( res, %val(IPTBOUT), %val(IPINAE),
     +                                 j )
                     write ( text, '('' Removed star '',i5,
     +                     '' - too close to star no '', i5)' ) j, k
                     call printo ( text )
                     call wcheck ( res(34,j), 10, .true. )		!Flag star as merged
                     kc = 1
                  endif
               endif
            enddo
         endif
      enddo
      if ( kc.eq.0 ) then
         call printo ( '  - No stars weeded' )
      else
         call pargi ( kc )
         call printd ( '  - %d Stars weeded' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CHCLOSE -- Find brightest fitted star too close to target star
C  It only looks at stars done on present loop and present operation
C
C     a j penny                              dao           1988-05-03

      subroutine me_chclose ( k, res, ka, brighter, kt )

      implicit none

      include 'measure.inc'

      integer	k		!i: Target star
      real	res(TBX,TBY)	!i: Results array
      integer	ka		!o: Brightest too close done star found
				!   (=0 if none such)
      logical	brighter	!o: False= no found or fainter than target
				!   True=found brighter star
      integer	kt		!i: Flag for single/par proc (0/1)
C--
      integer j
      real dx, dy, dd, arad, ht, xa, ya, ha, ra, ratio
      logical rcheck
      external rcheck
Cbegin


      ka = 0
      brighter = .false.

      if ( kt.eq.0 ) then						!Get star values
         ha = HP(1)
         xa = XP(1) + LXS - 1.0
         ya = YP(1) + LYS - 1.0
      else
         ha = P_HP(1)
         xa = P_XP(1) + P_LXS - 1.0
         ya = P_YP(1) + P_LYS - 1.0
      endif

      if ( ha.le.1.0e-8 ) return					!Check star to look at

      ra = res(12,k) + res(13,k)
      ht = 0.0
      do j = 1, TBY
         if ( j.ne.k .and. res(3,j).lt.49.0 .and.
     +        rcheck(res(33+LOOPNUM,j),(2*NUMOP+1)) ) then
            dx = xa - res(1,j)
            dy = ya - res(2,j)
            arad = (ra+res(12,j)+res(13,j))/4.0
            dd = dx*dx + dy*dy
            if ( dd.lt.0.3*0.3*arad*arad ) then
               if ( ka.eq.0 ) then
                  ht = res(9,j)
                  ka = j
               elseif ( res(9,j).gt.ht ) then
                  ht = res(9,j)
                  ka = j
               endif
            endif
         endif
      enddo

      ratio = ht/ha
      if ( ka.ne.0 .and. ratio.gt.1.00001 ) brighter = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_REMSTAR -- Remove star from list to be fitted; clear RES, output
C
C     a j penny                              dao           1988-05-03

      subroutine me_remstar ( res, out, inae, k )

      implicit none

      include 'measure.inc'

      real	res(TBX,TBY)	!i/o: results
      real	out(TBXVO,TBY)	!o: Output results
      real	inae(3,TBY)	!i: Original positions
      integer	k		!i: Star to remove
C--
Cbegin


      if ( k.lt.1. or. k.gt.TBY ) return

      call wcheck ( res(34,k), 1, .true. )
      call wcheck ( res(35,k), 1, .true. )
      call amovr ( res(34,k), out(34+5,k), 2 )
      res(1,k) = inae(1,k)
      res(2,k) = inae(2,k)
      res(3,k) = 50.0
      res(9,k) = 0.0
      res(49,k) = 0.0
      res(50,k) = 0.0
      out(1+5,k) = res(1,k)
      out(2+5,k) = res(2,k)
      out(3+5,k) = 50.0
      out(9+5,k) = 0.0
      out(49+5,k) = 0.0
      out(50+5,k) = 0.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_REMCOMP -- Remove first close undone companion of star from comp list
C  And also moves star (if comp found) to mean position. In effect
C  sets the star as the single replacement of the two stars, flagging
C  the second star as removed.
C
C      a j penny                   dao                1988-04-03

      subroutine me_remcomp ( ks, res, out, found, krem )

      implicit none

      include 'measure.inc'

      integer   ks				!i:   Star number
      real	res(TBX,TBY)			!i/o: Results array
      real	out(TBXVO,TBY)			!i:   Output results array
      logical	found				!o:   Flag for a close
						!     undone comp found
      integer	krem				!o:   Star number of found
						!     companion
C--
      integer k, jin, jout, kl, kk, kncomp
      real dx, dy, dlim, dd
Cbegin


      found = .false.
      krem = 0
      kncomp = nint(res(21,ks))

      if ( kncomp.lt.1 ) return								!No companions

      kl = 1
      do while ( .not.found .and. kl.le.kncomp )
         if ( kl.le.7 ) kk = nint(res(22+kl-1,ks))
         if ( kl.gt.7 ) kk = nint(res(41+kl-8,ks))
         if ( kk.gt.0 ) then
            if ( res(3,kk).gt.49.0 ) then
               dx = res(1,ks) - res(1,kk)
               dy = res(2,ks) - res(2,kk)
               dlim = 4.0*(res(12,ks)+res(13,ks)+res(12,kk)+
     +                    res(13,kk))/4.0
               dd = dx*dx + dy*dy
               if ( dd.lt.dlim*dlim ) then
                  res(1,ks) = (res(1,ks)+res(1,kk))/2.0
                  res(2,ks) = (res(2,ks)+res(2,kk))/2.0
                  res(49,ks) = (res(49,ks)+res(49,kk))/2.0
                  found = .true.
                  krem = kk
               endif
            endif
         endif
         if ( .not.found ) kl = kl + 1
      enddo

      if ( found ) then

         if ( kncomp.eq.1 ) then							!Remove from companion list
            res(22,ks) = 0.0
            out(22+5,ks) = 0.0
         elseif ( kl.ne.14 ) then
            do k = kl, kncomp
               if ( k.eq.7 ) then
                  jin = 41
                  jout = 28
               elseif ( k.gt.7 ) then
                  jin = 41 + k - 7
                  jout = jin - 1
               else
                  jin = 21 + 1 + k
                  jout = jin - 1
               endif
               res(jout,ks) = res(jin,ks)
               out(jout+5,ks) = res(jin,ks)
            enddo
            res(jin,ks) = 0.0
            out(jin+5,ks) = 0.0
         endif

         res(47,ks) = 0.0
         res(21,ks) = res(21,ks) - 1.0
         out(47+5,ks) = 0.0
         out(21+5,ks) = res(21,ks)

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SETDONE -- Set flags that star has been done.
C  Sets flags for star has been tried, and, maybe, done
C
C    a j penny                     dao                   1988-05-03

      subroutine me_setdone ( ks, doneit, res, out )

      implicit none

      include 'measure.inc'

      integer   ks			!i: Star number
      logical	doneit			!i: Flag for star has been done
      real	res(TBX,TBY)		!o: Results array
      real	out(TBXVO,TBY)		!o: Output results array
C--
Cbegin


      call wcheck ( res(33+LOOPNUM,ks), 2*NUMOP+1, .true. )
      if ( doneit ) then
         call wcheck ( res(33+LOOPNUM,ks), 2, .true. )
         call wcheck ( res(33+LOOPNUM,ks), 2*NUMOP+2, .true. )
      endif
      call amovr ( res(34,ks), out(34+5,ks), 2 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_STARDO -- (FN) For this loop number and operation number, do star?
C      Also see if in image.
C
C    a j penny                 stsci        1988 03 10

      logical function me_stardo ( ks, res )

      implicit none
      include 'measure.inc'

      integer   ks				!i: Star number
      real      res(TBX,TBY)			!i: Results array
C--
      logical rcheck
      external rcheck
Cbegin


      me_stardo = .true.
      if ( rcheck(res(33+LOOPNUM,ks),1) ) me_stardo = .false.
      if ( rcheck(res(33+LOOPNUM,ks),2) ) me_stardo = .false.
      if ( rcheck(res(33+LOOPNUM,ks),2*NUMOP+1) ) me_stardo = .false.

      if ( .not.rcheck(res(34,ks),9) ) me_stardo = .false.		!In image?

      if ( rcheck(res(34,ks),10) ) me_stardo = .false.			!Merged?


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_ORDER -- Make list of locations in order list
C
C    a j penny                 ral               1991 May

      subroutine me_order ( res, knord )

      implicit none
      include 'measure.inc'

      real      res(TBX,TBY)		!i: Results array
      integer   knord(TBY)		!o: Location in order list of a star
C--
      integer j, k
Cbegin


      do k = 1, TBY
         j = nint(res(37,k))
         knord(j) = k
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PARDO -- Find if a star ready for parallel doing
C  Looks at its companions to see if brighter ones done first
C
C    a j penny                 ral               1991 May

      subroutine me_pardo ( res, knord, pmore, nploop, doit )

      implicit none
      include 'measure.inc'

      real      res(TBX,TBY)		!i: Results array
      integer   knord(TBY)		!i: Location in order list of a star
      logical   pmore			!i/o: More parallels to do?
      integer   nploop			!i: Complete measure loop number
      logical   doit			!o: Do this star now?
C--
      integer k, kord, kn, num
      logical me_stardo, notyet
      external me_stardo
Cbegin


      if ( .not.DOPAR ) then						!Only if doing parallel
         doit = .true.
         return
      endif

      kord = knord(KSTAR)						!Order in doing of this star
      num = res(21,KSTAR)						!Number of companions

      k = 0								!Any undone ones that should
      notyet = .false.							! have been done first?
      do while ( k.lt.num .and. .not.notyet )
         k = k + 1
         if ( k.le.7 ) then
            kn = res((21+k),KSTAR)
         else
            kn = res((40+k-7),KSTAR)
         endif
         if ( .not.me_stardo(kn,res) ) then
            if ( knord(kn).lt.kord ) notyet = .true.
         endif
      enddo

      doit = .true.							!Load output
      if ( notyet ) then
         doit = .false.
         pmore = .true.
CX         call pargi ( nploop )
CX         call pargi ( KSTAR )
CX         call printd ( 'Loop %d - not doing star number %d yet' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CLEANDO -- Find whether to clean a star
C
C    a j penny               stsci               1988-03-10

      logical function me_cleando ( ks, res )

      implicit none
      include 'measure.inc'

      integer   ks			!i: Star number
      real	res(TBX,TBY)		!i: Results array
C--
      logical rcheck
      external rcheck
Cbegin


      me_cleando = .true.
      if ( rcheck(res(33+LOOPNUM,ks),1) ) me_cleando = .false.
      if ( .not.rcheck(res(33+LOOPNUM,ks),2) ) me_cleando = .false.
      if ( .not.rcheck(res(33+LOOPNUM,ks),2*NUMOP+2) )
     +                                       me_cleando = .false.

      if ( .not.rcheck(res(34,ks),9) ) me_cleando = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_TOOBIG -- Check if fit needs are too big for parallel processors
C
C     a j penny                ral                  1990-05-07

      subroutine me_toobig ( ks, toobig )

      implicit none

      include 'measure.inc'

      integer	ks		!i: Star number
      logical	toobig		!o: True = too big for par procs, false = not
C--
      integer mapmin, mapmax
      character text*72
Cbegin


      toobig = .false.

      if ( .not.DOPAR ) return

      call alimi ( MAPNUM, NST, mapmin, mapmax )

      if ( LX*LY.gt.70*70 .or. MX*MY*mapmax.gt.70*70*SMAX .or.
     +     LX*LY*NST.gt.70*70*SMAX ) toobig = .true.

      if ( toobig ) then
         write ( text, '('' Star no '', i5,
     +         '' - Fit needs too great for parallel processors'')' ) ks
         call printo ( text )
         call printo ( '   - Main machine used instead' )
      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is MESUBA.F
C
C   It contains:-
C
C ME_FIT        Fits up to 15 stars to an array
C ME_NPF        Calc no of free parameters
C ME_SLOAD      Load the simultaneous eqns
C ME_UPDATE     Update fit parameters
C ME_LDHXY      Load heights, posns into parameters
C ME_OUTCLEAN   Fix or Clean and Remove stars too far outside box
C ME_CONPROF    Convert profile parameters to ME_FIT ones for each star
C ME_DOVALD     Calcs how far out from the stars you have to go.
C ME_LOOPFIX    Fix stationary stars? sky release?
C ME_LOOPREM    Remove some stars from fit?



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_FIT -- Fits up to 15 stars to an array
C     The data in an array are fitted by a model of a sloping
C     background and up to 8 (or NUMS) 2-D profiled stars.
C
C      The NSFIT stars all have the same fixed input profile and
C      have their heights and positions calculated.
C
C
C      The details of the profile fitting are given in STARMAN.HLP
C
C
C      The input profile and star heights and positions and the
C      background must be close to the real values for the fit to
C      work.
C
C      1 It keeps the profile fixed.
C      2 It keeps the position of any star which is outside the
C        box fixed.
C      3 It deals with INVALID pixels, by not using any
C        pixels that are so flagged.
C      4 For each pixel, if one of the stars is so far away that
C        that star makes no contribution to the pixel, the pixel is
C        not used in the part of the iteration concerned with that
C        star.
C      5 The iterations are done until the fit settles down, or until
C        the number of iterations exceed the input limit.
C      6 There is a flag for making the iterations stop after
C        the position and height of the first star in the list has
C        settled, regardless of the other stars.
C
C
C
C
C   HISTORY
C     1-D Lorentz profile fitting program by AJPenny 1975
C     Complete rewrite into 2-D by J Straede  AAO  1977
C     Moved to VAX by K F Hartley RGO   1982
C     Adapted by A J Penny RGO 1983
C     Made into fast fixed profile version AJP RGO 1983-4
C     Variable damping factor method (Marquadt method) added ral 1988
C
C    a j penny                  ral                    90-04-14


      subroutine me_fit ( imuse, imuseb, map, iterlim, imusef,
     +                    ktype )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

      real      imuse(LX,LY)		!i: input array to fit
      logical 	imuseb(LX,LY)		!i: Flags for doing pixels
      real	map(MX,MY,MZ)   	!i: profile map
      integer	iterlim			!i: Max no iterations allowed
      real	imusef(LX,LY,SMAX)	!o: fitting work space
      integer   ktype			!i: Fit type (1=chi lessens;2 =chi helps
C--
      real    cmat(NUMMAX*NUMMAX)	!the simultaneous eqn matrix
      real    cvec(NUMMAX)		!the simultaneous eqn vector
      real    cmat1(NUMMAX*NUMMAX)	!the simultaneous eqn matrix 1
      real    cvec1(NUMMAX)		!the simultaneous eqn vector 1
      real    cmat2(NUMMAX*NUMMAX)	!the simultaneous eqn matrix 2
      real    cvec2(NUMMAX)		!the simultaneous eqn vector 2
      real    rvec(NUMMAX)		!the simultaneous eqn solution
      integer jfit(NUMMAX)		!Fit control parameters -2=out fix:
					!-1 = fix: 1 = use: 0 = not use
      integer jfit1(NUMMAX)		!Storage Fit control parameters -2=out fix:
					!-1 = fix: 1 = use: 0 = not use
      real    hp1(SMAX)			! 1 height for stars
      real    xp1(SMAX)			! 1 X posn for stars
      real    yp1(SMAX)			! 1 Y posn for stars
      real    hp2(SMAX)			! 2 height for stars
      real    xp2(SMAX)			! 2 X posn for stars
      real    yp2(SMAX)			! 2 Y posn for stars
      real    a1, b1, c1		!Plane sky parameters y = (a+b.x+c.y)
      real    a2, b2, c2		!Plane sky parameters y = (a+b.x+c.y)
      real    chisq1			!chi-squared sum 1
      real    chisq2			!chi-squared sum 2
      real    dchi			!fractional change in chi-squared
      logical chchi			!Had a loop when delta chi was small?
      logical endloop			!End if of looping?
      logical loop			!Loop control flag
      real    damp			!Damping factor in simul eqns
      real    dampmin			!Min allowed damping factor
      real    hplr			!last 'real' loop height of star 1
      integer npf			!No of free parameters at moment
      integer npf1			!No of free parameters before
      integer npfin			!No of free parameters at loop start
      integer nloop			!Number of loops
      integer ngloop			!Number of good loops
      integer j, k, kk, klasthp, lasthp, ierr
      real hplast(3), rx, ry, dh, hp2is, rv
      logical remed, dampup, isok

      INTEGER II
Cbegin


      PRINTIT = -2
      CALL ME_PRINT1 ( IMUSE, IMUSEB, MAP )

      if ( NST.gt.SMAX ) return						!Check if too many stars

      call me_conprof 							!Load profile values

      call me_dovald 							!Calc mean 1D profile

      do k = 1, NST
         rx = 1.0/GX(k)
         ry = 1.0/GY(k)
         call subdiv ( rx, ry, LBOX(k) )				!subdivision amounts
         XMSTEP(k) = 0.7*(rx+ry)/2.0					!max profile shift step
         YMSTEP(k) = 0.7*(rx+ry)/2.0					!in one loop
      enddo

      call me_ldhxy ( imuse, imuseb, map, hp1, xp1, yp1, jfit, npf,	!Load fit params
     +                a1, b1, c1, hplr, lasthp, hplast, imusef )

      CALL ME_PRINT2 ( A1, B1, C1, HP1, XP1, YP1 )

      nloop = 0								!No of calc loops
      ngloop = 0							!chi-sq down loops
      chchi = .false.							!had a small chi-sq diff?
      dampup = .false.

      call me_npf ( jfit, NUMMAX, npf )					!Load sim eqns
      npfin = npf

      call me_sload ( xp1, yp1, hp1, a1, b1, c1, jfit, npf, nloop,
     +                imuse, imuseb, map, imusef, cmat1, cvec1, chisq1 )

      damp = 0.01							!Start loop

      IF ( PRINTIT.GT.0 ) THEN
       WRITE ( 6,'(''Z1C  NLOOP,NGLOOP,A1,B1,C1,LX,LY,CHISQ1,DAMP'')')
       WRITE ( 6, '(3X,''  '',2I4,3F8.3,2I3,G17.6,F9.4)' )
     +            NLOOP, NGLOOP, A1,B1,C1,LX,LY,CHISQ1,DAMP
      ENDIF

      isok = .true.
      loop = .true.
      do while ( loop )

         call amovr ( cmat1, cmat, npf*npf )				!Load eqns
         call amovr ( cvec1, cvec, npf )
         do k = 1, npf							!Apply damping factor
            kk = (k-1)*npf + k						! to the sim sqns
            cmat(kk) = cmat(kk)*(1.0+damp)
         enddo
         call simulx ( cvec, cmat, rvec, npf )				!Solve the sim eqns


        IF ( PRINTIT.GT.1 ) THEN
        DO II = 1, NST
           WRITE ( 6, '(''Z2  I H1 X1 Y1 H2 X2 Y2 endloop'')' )
           WRITE ( 6, '('' I H X Y'',I3,6F10.3,L)' ) II,HP1(II),
     +             XP1(II),YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
        ENDDO
        ENDIF

         call me_update ( rvec, jfit, a1, b1, c1, xp1, yp1, hp1,	!Apply corrns to
     +                    a2, b2, c2, xp2, yp2, hp2 )			! 1 and put into 2


        IF ( PRINTIT.GT.1 ) THEN
        DO II = 1, NST
           WRITE ( 6, '(''Z3 I H1 X1 Y1 H2 X2 Y2 endloop'')' )
           WRITE ( 6, '(''  '',I3,6F10.3,L)') II,HP1(II),XP1(II),
     +                 YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
        ENDDO
        ENDIF

         if ( damp.lt.0.5 ) then					!Fix stars and
            call amovi ( jfit, jfit1, NUMMAX )				! release sky?
            call me_loopfix ( xp1, yp1, jfit, xp2, yp2, ngloop )
            npf1 = npf
            call me_npf ( jfit, NUMMAX, npf )
         endif

        IF ( PRINTIT.GT.1 ) THEN
        DO II = 1, NST
           WRITE ( 6, '(''Z4 I H1 X1 Y1 H2 X2 Y2 '')' )
           WRITE ( 6, '(''  '',I3,6F10.3)' ) II,HP1(II),XP1(II),
     +                YP1(II),HP2(II),XP2(II),YP2(II)
        ENDDO
        ENDIF

         call me_sload ( xp2, yp2, hp2, a2, b2, c2, jfit, npf, nloop,	!Load sim eqns
     +             imuse, imuseb, map, imusef, cmat2, cvec2, chisq2 )

         IF ( PRINTIT.GT.0 ) THEN
            DCHI = CHISQ2-CHISQ1
            WRITE ( 6,
     +         '(''NLOOP,NGLOOP,A2,B2,C2,LX,LY,CHISQ2,DAMP,DCHI'')' )
            WRITE ( 6, '(3X,'' '',2I4,3F8.3,2I3,G17.6,F9.4,F12.4)' )
     +      NLOOP,NGLOOP,A2,B2,C2,LX,LY,CHISQ2,DAMP,DCHI
         ENDIF

         dh = abs(hp2(1)-hplr)/max(0.000001,hplr)
         dchi = (chisq2-chisq1)/max(0.000001,chisq1)

         IF ( PRINTIT.GT.0 ) THEN
            WRITE ( 6, '(''Z6 CHISQ1,CHISQ2,CHCHI,DCHI,DH'')' )
            WRITE ( 6, '(3X,'' '',2G17.6,L4,2G17.6)' )
     +             CHISQ1,CHISQ2,CHCHI,DCHI,DH
         ENDIF

         if ( chisq2.gt.chisq1 ) then					!was this fit better?
            damp = damp*sqrt(5.0) 					!No, increasse damping
            dampup = .true.
            if ( damp.gt.29.0 ) then
               hp2is = hp2(1)
               damp = damp/sqrt(5.0)
               if ( ktype.eq.1 ) then
                  loop = .false.
                  isok = .false.
                  rv = 0.0
                  if ( hp2(1).gt.0.0 .and. hplast(lasthp).gt.0.0 )
     +               rv = -2.5*alog10(hp2(1)/hplast(lasthp))
                  if ( abs(rv).gt.0.01 ) hp2(1) = 0.0
                  IF ( PRINTIT.GE.-1 ) WRITE ( 6,
     +                '(''Z6A Damp>29, fit not better, exit'')' )
                  call amovi ( jfit1, jfit, NUMMAX )
                  npf = npf1
               endif
            endif
         endif

         if (((chisq2.le.chisq1).and.ktype.eq.1) .or. ktype.eq.2) then	!Better? or ktype=2?
            ngloop = ngloop + 1
            endloop = .false.						!End on chi?
            if ( chchi .and. dchi.lt.0.0 .and. dchi.gt.-0.001 .and.
     +           (.not.HTFIX.and.dh.lt.0.001) ) endloop = .true.
            if ( dchi.lt.0.0 .and. dchi.gt.-0.001 ) chchi = .true.
            if ( .not.HTFIX .and. dh.lt.0.0002 ) endloop = .true.

            IF ( PRINTIT.GT.-1 ) THEN
               DO II = 1, NST
                  WRITE ( 6, '(''Z7 I H1 X1 Y1 H2 X2 Y2 endloop'')' )
                  WRITE ( 6, '(''  '',I3,6F10.3,L)' ) II,HP1(II),
     +             XP1(II),YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
               ENDDO
            ENDIF

            if ( .not.endloop ) then
               call amovr ( xp2, xp1, NST )				!store present
               call amovr ( yp2, yp1, NST )				!fit as last
               call amovr ( hp2, hp1, NST )
               a1 = a2
               b1 = b2
               c1 = c2
               chisq1 = chisq2
               call amovr ( cmat2, cmat1, npf*npf )
               call amovr ( cvec2, cvec1, npf )

               hplr = hp2(1)						!Update last
               lasthp = lasthp + 1					! results records
               if ( lasthp.eq.4 ) lasthp = 1
               hplast(lasthp) = hplr
               klasthp = lasthp - 2
               if ( klasthp.lt.1 ) klasthp = klasthp + 3

               dampmin = 0.001
               if ( npf.gt.15 ) dampmin = 0.01
               if ( .not.dampup ) damp = max(dampmin,(damp/sqrt(10.0)))
               dampup = .false.

               IF ( PRINTIT.GT.1 ) THEN
                  DO II = 1, NST
                     WRITE ( 6, '(''Z8 I H1 X1 Y1 H2 X2 Y2 endloop'')')
                     WRITE ( 6, '(''  '',I3,6F10.3,L)') II,HP1(II),
     +               XP1(II),YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
                  ENDDO
               ENDIF

               remed = .false.						!Remove stars?
               if ( damp.lt.0.5 ) call me_looprem ( xp1, yp1, hp1,
     +                            jfit, imuse, imuseb, remed, map )

               IF ( PRINTIT.GT.1 ) THEN
                  DO II = 1, NST
                     WRITE ( 6,
     +                   '(''Z9 I H1 X1 Y1 H2 X2 Y2 endloop remed'')')
                     WRITE ( 6, '(''  '',I3,6F10.3,2L)') II,HP1(II),
     +           XP1(II),YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP,REMED
                  ENDDO
               ENDIF

               if ( remed ) then
                  call me_npf ( jfit, NUMMAX, npf )
                  call amovi ( jfit, jfit1, NUMMAX )
                  npf1 = npf
                  call me_sload ( xp1, yp1, hp1, a1, b1, c1, jfit1, 	!Load sim eqns
     +                            npf1, nloop, imuse, imuseb, map,
     +                            imusef, cmat1, cvec1, chisq1 )
               endif


               CALL ME_PRINT3 ( HP1, XP1, YP1, HP2, XP2, YP2, REMED,
     +                 ENDLOOP, NLOOP, NGLOOP,A1,B1,C1,CHISQ1,DAMP )

            endif

            IF ( PRINTIT.GT.0 ) THEN
               WRITE ( 6, '(''Z10B ENDLOOP,NLOOP,ITERLIM,NPF'')' )
               WRITE ( 6,  '(3X,'' '',L4,3I5)' ) ENDLOOP,NLOOP,
     +                                           ITERLIM,NPF
            ENDIF

            if ( endloop .or. nloop.ge.iterlim .or. npf.le.0 )
     +          loop = .false.

         endif

         IF ( PRINTIT.GT.-1 ) THEN
            DO II = 1, NST
               WRITE ( 6, '(''Z10BA I H1 X1 Y1 H2 X2 Y2 endloop'')' )
               WRITE ( 6,  '(''  '',I3,6F10.3,L)' )II,HP1(II),XP1(II),
     +                       YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
            ENDDO
         ENDIF

         if ( nloop.ge.iterlim ) loop = .false.

         if ( damp.lt.0.5 .and. dh.lt.0.0002 .and.			!Height found even
     +        abs(dchi).lt.0.001 ) loop = .false.			! tho chi inc a bit
         if ( abs(dchi).lt.0.0001 ) loop = .false.			!Chi-sq change v small
         if ( dchi.lt.0.0 .and. 					!Moved too far
     +        ( abs(xp2(1)-XPOR(1)).gt.SHIFTF*RAD(1) .or.
     +          abs(yp2(1)-YPOR(1)).gt.SHIFTF*RAD(1) )  ) then
            hp2(1) = 0.0
            IF (PRINTIT.GE.-1)WRITE ( 6,
     +               '(''Z10BB Moved too far: SHIFTF,RAD'')' )
     +                               ,SHIFTF,RAD(1)
            loop = .false.
         endif

         CALL ME_PRINT4 (HP1,XP1,YP1,HP2,XP2,YP2,REMED,ENDLOOP,NLOOP,
     +     NGLOOP,A1,B1,C1,A2,B2,C2,DCHI,CHISQ1,DAMP,CHISQ2,DH,LOOP)

      enddo


      call azerob ( SUCCESS, NST )					!Flag 1st star ok
      if ( hp2(1).gt.0.0 ) SUCCESS(1) = .true.
      if ( hp2(1).eq.0.0 ) then						!If failed, posn
         XP(1) = XPOR(1)						! set to original
         YP(1) = YPOR(1)
      endif
      if ( ktype.eq.2 .or. (ktype.eq.1.and.SUCCESS(1)) ) then

         ITER = nloop				 			!note no of iterations

         BASE   = a2							!Set for exit
         XSLOPE = b2
         YSLOPE = c2
         call amovr ( xp2, XP, NST )
         call amovr ( yp2, YP, NST )
         call amovr ( hp2, HP, NST )

         call amovr ( xp2, XPI, NST )					!Put back into start
         call amovr ( yp2, YPI, NST )					! in case needed again
         call amovr ( hp2, HPI, NST )


         DIFFM = 0.0							!final - last-1
         rv = HP(1)
         if ( .not.isok ) rv = hp2is
         if ( rv.gt.0.0 .and. hplast(lasthp).gt.0.0 ) DIFFM =
     +                                -2.5*alog10(rv/hplast(lasthp))

         RDAMP = damp
         RCHI = chisq2/(max(1.0,real(LX*LY-NINVAL-npfin)))		!Reduced chi-squared

         call matinv ( cmat2, cmat1, npf, npf, ierr )			!Find Error on magnitude
         ERRM = 0.0
         if ( ierr.eq.0 ) then
            j = 0							! (cmat1 used as
            do k = 1, 6							!  work space for inverse)
               if ( jfit(k).eq.1 ) j = j + 1
            enddo
            rv = sqrt(abs(cmat2(j+((j-1)*npf))))
            if ( HP(1).gt.0.0 ) then
               if ( (rv/HP(1)).gt.1.0e-5 )
     +            ERRM=2.5*alog10(1.0+(rv/HP(1)))
            endif
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_NPF -- Calc no of free parameters
C
C   a j penny          ral           1988-08-04

      subroutine me_npf ( jfit, nummax, npf )

      implicit none

      integer	nummax		!i: Max poss no of parameters
      integer	jfit(nummax)	!i: Fit control flags
      integer	npf		!o: No of free parameters
C--
      integer k
Cbegin


      npf = 0 								!No of free params
      do k = 1, nummax
         if ( jfit(k).eq.1 ) npf = npf + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SLOAD -- Load the simultaneous eqns
C  Takes the present fit and data and loads up the
C  simultaneous eqns. Thus the output is the matrix CMAT
C  and the vector CVEC. It also calcs the chi-sq of that
C  fit (i.e. the present one before the sim eqns fit to be done)
C  from the data.
C
C  a j penny               ral              1990-04-14

      subroutine me_sload ( xpt, ypt, hpt, a, b, c, jfit, npf, nloop,
     +                      imuse, imuseb, map, imusef, cmat, cvec,
     +                      chisq )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

      real	xpt(SMAX)		!i: X posn of stars
      real	ypt(SMAX)		!i: Y posn of stars
      real	hpt(SMAX)		!i: height of stars
      real	a, b, c			!i: Plane sky params y = (a+b.x+c.y)
      integer	jfit(NUMMAX)		!i: Fit control param (-2=out fix)
      integer	npf			!i: No of free parameters
      integer   nloop                   !i:
      real	imuse(LX,LY)		!i: Image area
      logical	imuseb(LX,LY)		!i: Flags for pixel use
      real	map(MX,MY,MZ)		!i: profile map array
      real	imusef(LX,LY,SMAX)	!i/o: store for sim eqn load vals
      real	cmat(npf,npf)   	!o: the simultaneous eqn matrix
      real	cvec(npf)		!o: the simultaneous eqn vector
      real	chisq			!o: Chi-squared of fit
C--
      real erf(303)			!error functions
					!-1 = fix: 1 = use: 0 = not use
      real dlim(1000)
      integer j, k, nac1, nac2, llx, lly, kk, ja, jx, jy, jfitx,
     +        jfity, jfitxy, jfith, n, lgo, nacd
      real dx, dy, dxa, dya, dxb, dyb, alp, dfdx, dfdy, qt, qa, f9,
     +     f4, alg, a0, afact, algo, gg, f, ddxa, ddxb, ddxc, ddya,
     +     ddyb, ddyc, ggagph, zd, av, x, y, z, dres, resval, xdm,
     +     ydm, var, xm, ym, f4s, qd, qdd, g2a, alg2a, alg2, aon2,
     +     dx2, dy2, g2am2, hh

      real rinter
      external rinter
      real tiny
      data tiny/1e-10/

      INTEGER I11,I22,I33
Cbegin


      IF ( PRINTIT.GT.0 ) THEN
         WRITE ( 6, '(''Z16 ME_SLOAD IN'')' )
         I11 = 7
         I22 = 0
         I33 = 0
         WRITE ( 6, '(20X,A10,3I5)' ) 'I11 I22 I33',I11,I22,I33
      ENDIF

      if ( SMAX.gt.1000 ) then						!arrays too big
         call printo ( 'ERROR: Too many stars for s/r ME_SLOAD' )
         return
      endif

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_SLOAD 1'')' )

      nloop = nloop + 1							!One more calc

      call azeror ( cmat, npf*npf )					!Zero simul eqn arrays
      call azeror ( cvec, npf )

      do k = 1, NST							!Calc the distance from
         if ( jfit(3*(k-1)+3+3).ne.0 ) then				! each star to calculate
            kk = 1
            av = 1.0
            do while ( av.gt.0.1 .and. kk.lt.200 )
               kk = kk + 1
               av = hpt(k)*VALD(kk,k)
            enddo
            dlim(k) = max ( real(kk), 2.5*max((1.0/GX(k)),(1.0/GY(k))))
         endif
      enddo

      do k = 1, NST							!see if loading or
	 if ( .not.GETIT(k) ) then
            if ( jfit(3*(k-1)+1+3).eq.-1 .and.
     +           jfit(3*(k-1)+2+3).eq.-1 ) LOADIT(k) = .true.
         endif
      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3x,''ME_SLOAD 2'')' )

      chisq = 0.0							!Loop through the pixels
      do jy = 1, LY
       do jx = 1, LX

         if ( imuseb(jx,jy) ) then					!If pixel ok, use it

           x = real(jx)
           y = real(jy)

C  Calc error fns for background; Start calc of value of profile (Z)
C                                  Z = A + B.x + C.y

           nac1 = 0
           if ( jfit(1).eq.1 ) then
             nac1 = nac1 + 1
             erf(nac1) = 1.0
           endif
           if ( jfit(2).eq.1 ) then
             nac1 = nac1 + 1
             erf(nac1) = x
           endif
           if ( jfit(3).eq.1 ) then
             nac1 = nac1 + 1
             erf(nac1) = y
           endif
           z = a + b*x + c*y

C  Accumulate profile and error functions over all stars
C                                  Z = Z + star1 + star2 + ....

           call azeror ( erf(nac1+1), (nummax-(nac1+1)+1) )

           do k = 1, NST						!Do the stars

             nacd = 0
             if ( k.ne.1 ) then
               do j = 1, k-1
                 if ( jfit((j-1)*3+1+3).eq.1 ) nacd = nacd + 1
                 if ( jfit((j-1)*3+2+3).eq.1 ) nacd = nacd + 1
                 if ( jfit((j-1)*3+3+3).eq.1 ) nacd = nacd + 1
               enddo
             endif

             n = 3*(k-1) + 1 + 3
             jfitx = jfit(n)

             if ( jfitx.ne.0 ) then					!If there is a star

               jfity = jfit(n+1)
               jfitxy = max(jfitx,jfity)
               jfith = jfit(n+2)

               dxa = x - xpt(k)						!Star dependant parameters
               dya = y - ypt(k)

               if ( abs(dxa).le.dlim(k).and.abs(dya).le.dlim(k) ) then	!See if calc effect on the
									! pixel of this star
                 if ( GETIT(k) ) then
                    erf(nac1+nacd+1) = imusef(jx,jy,k)
                    z = z + hpt(k)*erf(nac1+nacd+1)
                 else

                   if ( abs(dxa).lt.4.0 .and. abs(dya).lt.4.0 ) then	!If far from star, no need
                     afact = LBOX(k)*LBOX(k)				! to subdivide, so only do
                     lgo = LBOX(k)					! once. Else find factors
                     algo = lgo
                   else
                     afact = 1.0
                     lgo = 1
                     algo = 1.0
                   endif

                   zd = 0.0						!Subdivide pixel if
                   do lly = 1, lgo					! profile sharp
                     do llx = 1, lgo

                       nac2 = nac1 + nacd

                       xdm = -0.5 + (0.5+real(llx)-1.0)/algo
                       ydm = -0.5 + (0.5+real(lly)-1.0)/algo
                       dxb = x - xpt(k) + xdm
                       dyb = y - ypt(k) + ydm

                       dx = dxb*CO(k) + dyb*SI(k)			!Calc the fit analysis
                       dy = dxb*SIM(k) + dyb*CO(k)			! parameters at this point
                       if ( abs(dx).lt.0.0001 ) dx = sign(0.0001,dx)
                       if ( abs(dy).lt.0.0001 ) dy = sign(0.0001,dy)
                       if ( abs(dxb).lt.0.0001 ) dxb = sign(0.0001,dxb)
                       if ( abs(dyb).lt.0.0001 ) dyb = sign(0.0001,dyb)
                       a0 = hpt(k)
                       dx2 = dx*dx
                       dy2 = dy*dy
                       gg = dx2*GX2(k) + dy2*GY2(k)
                       hh = dx2*HX2(k) + dy2*HY2(k)
                       f = sqrt(hh)
                       alp = P(k)*(f+1.0)
                       aon2 = alp/2.0
                       alg2 = alog(gg)
                       alg = alg2/2.0
                       alg2a = alg2*(aon2-1.0)
                       if ( alg2a.lt.60.0 .and. alg2a.gt.-60.0 ) then
                          g2am2 = exp(alg2a)
                       else
                          g2am2 = 0.0
                       endif
                       g2a = g2am2*gg
                       if ( alg2a.lt.60.0 ) then
                          f4 = 1.0/(1.0+g2a)
                          if ( f4.lt.tiny ) f4 = 0.0
                       else
                          f4 = 0.0
                       endif
                       f4s = f4*f4
                       f9 = a0*f4s*g2am2
                       if ( qh(k).ne.0.0 ) then
                          qd = dxb*dxb + dyb*dyb
                          qdd = QF1(k)*(qd**QF2(k))
                          qa = exp(-1.0*min(40.0,qdd))
                          qt = QF3(k)*a0*qa*(qd**QF4(k))
                       else
                          qa = 1.0
                          qt = 0.0
                       endif

                       if ( CDOMAP(k) ) then				!Profile map
                          xm = MAPX(k)/2 + dxb*MAGNIF(k)
                          ym = MAPY(k)/2 + dyb*MAGNIF(k)
                          resval = rinter ( MAP, MX, MY, MZ,
     +                                   MAPNUM(k), MAPX(k), MAPY(k),
     +                                   xm, ym, dfdx, dfdy )
                          dfdx = dfdx*MAGNIF(k)
                          dfdy = dfdy*MAGNIF(k)
                       endif

                       if ( jfitxy.eq.1 ) then				!Error functions for
                         ggagph = gg*alg*p(k)/f				! position and intensity
                         if ( jfitx.eq.1 ) then				! for each star
                           nac2 = nac2 + 1
                           ddxa = GX2(k)*CO(k)*dx + GY2(k)*SIM(k)*dy
                           ddxb = HX2(k)*CO(k)*dx + HY2(k)*SIM(k)*dy
                           ddxc = f9*(alp*ddxa+ggagph*ddxb)
                           ddxc = ddxc + qt*dxb
                           erf(nac2) = erf(nac2) + ddxc
                           if ( CDOMAP(k) ) then
                             erf(nac2) = erf(nac2) - a0*dfdx
                           endif
                         endif
                         if ( jfity.eq.1 ) then
                           nac2 = nac2 + 1
                           ddya = GX2(k)*SI(k)*dx + GY2(k)*CO(k)*dy
                           ddyb = HX2(k)*SI(k)*dx + HY2(k)*CO(k)*dy
                           ddyc = f9*(alp*ddya+ggagph*ddyb)
                           ddyc = ddyc + qt*dyb
                           erf(nac2) = erf(nac2) + ddyc
                           if ( CDOMAP(k) ) then
                             erf(nac2) = erf(nac2) - a0*dfdy
                           endif
                         endif
                       endif
                       if ( jfith.eq.1 ) then
                         nac2 = nac2 + 1
                         erf(nac2) = erf(nac2) + (f4+qa*QH(k))
                         if ( CDOMAP(k) ) then
                           erf(nac2) = erf(nac2) + resval
                         endif
                       endif

                       zd = zd + a0*(f4+qa*QH(k))			!Calc height of fit here
                       if ( CDOMAP(k) ) zd = zd + a0*resval
                     enddo
                   enddo

                   do j = nac1+nacd+1, nac2				!Average the small squares
                      erf(j) = erf(j)/afact				! at this point
                   enddo
                   z = z + zd/afact

                   if ( LOADIT(k) ) imusef(jx,jy,k) = erf(nac2)

                 endif
               endif
             endif

           enddo							!Loop back for next star

           dres = imuse(jx,jy) - z					!Calcs residual
           if ( abs(dres).gt.1.0e9 ) dres = sign(1.0e9,dres)

           var = ZGAIN*abs(imuse(jx,jy)) + ZNOISE*ZNOISE
           if ( var.gt.1.0e-7 ) chisq = chisq + dres*dres/var

         IF ( PRINTIT.GT.4 ) WRITE ( 6, '(''K,JX,JY,Z,DRES'',4I,F12.5)')
     +                              K,JX,JY,Z,DRES

           if ( abs(dres).gt.1.0e10 ) dres = 1.0e10*sign(1.0,dres)	!Accumulate vector and
           if ( abs(dres).lt.1.0e-10 ) dres = 1.0e-10*sign(1.0,dres)	! matrix contributions at
           do j = 1, npf						! this pixel
              if(abs(erf(j)).gt.1.0e10)erf(j) = 1.0e10*sign(1.0,erf(j))
              if(abs(erf(j)).lt.1.0e-10)erf(j)=1.0e-10*sign(1.0,erf(j))
           enddo

           do j = 1, npf
              cvec(j) = cvec(j) + erf(j)*dres
              do ja = j, npf
                 cmat(ja,j) = cmat(ja,j) + erf(j)*erf(ja)
              enddo
           enddo

         endif

       enddo
      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_SLOAD 3'')' )

      do k = 2, npf							!Fill in rest of matrix
         do j = 1, k-1
            cmat(j,k) = cmat(k,j)
         enddo
      enddo


      do k = 1, NST							!Check sim eqn store flags
         if ( LOADIT(k) ) then
            LOADIT(k) = .false.
            GETIT(k) = .true.
         endif
      enddo

      IF ( PRINTIT.GT.0 ) THEN
      I11 = 8
      I22 = 0
      I33 = 0
      WRITE ( 6,  '(3X,''QR end'',3I5)') I11,I22,I33
      ENDIF

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_SLOAD OUT'')' )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_UPDATE -- Update fit parameters
C
C  a j penny               ral               1988-08-03

      subroutine me_update ( rvec, jfit, a1, b1, c1, xp1, yp1,
     +                       hp1, a2, b2, c2, xp2, yp2, hp2 )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

      real	rvec(NUMMAX)	!i: Correcting factors
      integer	jfit(NUMMAX)	!i: Fit control parameters
      real	a1		!i: input sky base
      real	b1		!i: input sky X slope
      real	c1		!i: input sky Y slope
      real	xp1(SMAX)	!i: input star x posns
      real	yp1(SMAX)	!i: input star y posns
      real	hp1(SMAX)	!i: input star heights
      real	a2		!o: output sky base
      real	b2		!o: output sky X slope
      real	c2		!o: output sky Y slope
      real	xp2(SMAX)	!o: output star x posns
      real	yp2(SMAX)	!o: output star y posns
      real	hp2(SMAX)	!o: output star heights
C--
      integer k, nac
      real dd, ds, dx, dy, dh
      character  text*72

      REAL DHA, DXA, DYA, DRES
Cbegin

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(2X,''ME_UPDATE IN'')' )

      nac = 0
      dd = 0.0								!Sky update
      if ( jfit(1).eq.1 ) then
         nac = nac + 1
         dd = rvec(nac)
         ds = 20.0 + (abs(a1)/5.0)
         if ( abs(dd).gt.ds ) dd = sign(ds,dd)
      endif
      a2 = a1 + dd

      dd = 0.0
      if ( jfit(2).eq.1 ) then
         nac = nac + 1
         dd = rvec(nac)
         ds = 1.0 + (abs(b1)/10.0)
         if ( abs(dd).gt.ds ) dd = sign(ds,dd)
      endif
      b2 = b1 + dd

      dd = 0.0
      if ( jfit(3).eq.1 ) then
         nac = nac + 1
         dd = rvec(nac)
         ds = 1.0 + (abs(c1)/10.0)
         if ( abs(dd).gt.ds ) dd = sign(ds,dd)
      endif
      c2 = c1 + dd

      do k = 1, NST							!stars update

         DXA = 0.0
         DYA = 0.0
         DHA = 0.0

         dx = 0.0							!new X posn
         if ( jfit(3*(k-1)+1+3).eq.1 ) then
            nac = nac + 1
            dx = rvec(nac)
            DXA = DX
            if ( abs(dx).gt.XMSTEP(k) ) dx = sign(XMSTEP(k),dx)
         endif
         xp2(k) = xp1(k) + dx

         dy = 0.0							!new Y posn
         if ( jfit(3*(k-1)+2+3).eq.1 ) then
            nac = nac + 1
            dy = rvec(nac)
            DYA = DY
            if ( abs(dy).gt.YMSTEP(k) ) dy = sign(YMSTEP(k),dy)
         endif
         yp2(k) = yp1(k) + dy


         dh = 0.0							!new height
         if ( jfit(3*(k-1)+3+3).eq.1 ) then
            nac = nac + 1
            dh = rvec(nac)
            DHA = DH
            ds = 10.0 + (abs(hp1(k))/5.0)
            if ( abs(dh).gt.ds ) dh = sign(ds,dh)
         endif
         hp2(k) = hp1(k) + dh

         IF ( PRINTIT.GE.-1 ) THEN
         WRITE ( 6,  '('' ME_UPD 0  K HP2 XP2 YP2'',i3,3f12.3)')
     +                          K,HP2(k),XP2(k),YP2(k)
         ENDIF

         IF ( PRINTIT.GT.0 ) THEN
         WRITE ( 6, '(''ME_UPD 1  K,DHA,DH,DXA,DX,DYA,DY'')')
         WRITE ( 6,  '(''   '',i3,2f13.4,4f10.4)')
     +                  K,DHA,DH,DXA,DX,DYA,DY
CX         IF ( K.EQ.1 .OR. NLOOP.EQ.1 ) THEN
         IF ( K.EQ.1 ) THEN
            WRITE ( 6, '(''ME_UPD 2  HP2,XP2,YP2'')' )
            WRITE ( 6, '(3d)' )HP2,XP2,YP2
         ENDIF
         WRITE ( 6, '(''ME_UPD 3  K, d xp1,  d yp1, d xpk, d ypk'')')
         TEXT = ' '
         WRITE ( TEXT(7:12), '(I3,3x)' ) K
         DRES = XP(K)-XP(1)
         WRITE ( TEXT(31:35), '(F5.1)' ) DRES
         DRES = YP(K)-YP(1)
         WRITE ( TEXT(36:40), '(F5.1)' ) DRES
         DRES = XP(K)-XPor(K)
         WRITE ( TEXT(42:46), '(F5.1)' ) DRES
         DRES = YP(K)-YPor(K)
         WRITE ( TEXT(47:51), '(F5.1)' ) DRES
         WRITE ( 6,'(1X,A)' ) ( TEXT  )

         ENDIF

      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_UPDATE OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_LDHXY -- Load heights, posns into parameters
C
C   a j penny                   ral              1988-07-31

      subroutine me_ldhxy ( imuse, imuseb, map, hp1, xp1, yp1, jfit,
     +                 npf, a1, b1, c1, hplr, lasthp, hplast, imusef )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

      real	imuse(LX,LY)		!i/o: Image array
      logical	imuseb(LX,LY)		!i: Image array pixel flags
      real	map(MX,MY,MZ)		!i: Profile map
      real	hp1(SMAX)		!o: Heights of stars
      real	xp1(SMAX)		!o: X posns of stars
      real	yp1(SMAX)		!o: Y posns of stars
      integer	jfit(NUMMAX)		!o: Fit control parameters
      integer   npf			!o: Number of free parameters
      real	a1			!o: sky zero
      real	b1			!o: sky x slope
      real	c1			!o: sky y slope
      real	hplr			!o: last real loop height for 1st
					!   stars
      integer	lasthp			!o: Posn in last-3 height loop
      real	hplast(3)		!o: last 3 heights store
      real	imusef(LX,LY,SMAX)	!o: store for sim eqn load vals
C--
      integer k
      logical remed
Cbegin

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LDHXY IN '')')

      call azerob ( LOADIT, SMAX )					!Clear sim eqn store
      call azerob ( GETIT, SMAX )
      call azeror ( imusef, LX*LY*SMAX )

      call amovr ( HPI, hp1, SMAX )					!Load input heights
      call amovr ( XPI, xp1, SMAX )					!Load input x posns
      call amovr ( YPI, yp1, SMAX )					!Load input y posns
      a1 = BASE								!Load base, slope,
      b1 = XSLOPE
      c1 = YSLOPE

      call azeroi ( jfit, NUMMAX )					!Clear fit params
      jfit(1) = 1							!fix flat slope
      jfit(2) = -1
      jfit(3) = -1
      call amovki ( 1, jfit(4), 3*NST )					!load stars JFITs
      if ( POSNFIX ) then						!Fix star posns if wanted
         do k = 1, NST
            jfit(3*(k-1)+1+3) = -1
            jfit(3*(k-1)+2+3) = -1
         enddo
      endif
      if ( HTFIX ) then							!Fix star heights if wanted
         do k = 1, NST
            jfit(3*(k-1)+3+3) = -1
         enddo
         call me_npf ( jfit, NUMMAX, npf )
      endif

      call me_outclean ( imuse, imuseb, map, xp1, yp1, hp1, jfit,remed)	!If fairly outside box,
									! fix or clean out star

      do k = 1, NST							!Clear zero height stars
         if ( hp1(k).eq.0.0 ) call azeroi ( jfit(3*(k-1)+3+3), 3 )
      enddo

      call azerob ( SUCCESS, NST )					!Success flags to failure

      call amovkr ( hp1(1), hplast, 3 ) 				!Clear the last 3
      lasthp = 3							! results records and

      hplr = hp1(1)

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LDHXY OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OUTCLEAN -- Fixes or Cleans and Removes stars too far outside box
C
C     a j penny            ral                   1988-08-06

      subroutine me_outclean ( imuse, imuseb, map, xp1, yp1, hp1,
     +                         jfit, remed )

      implicit none

      include 'measure.inc'

      real	imuse(LX,LY)		!i/o: Image array
      logical	imuseb(LX,LY)		!i:   Image array pixel flags
      real	map(MX,MY,MZ)		!i:   Profile map
      real	xp1(SMAX)		!i/o: X posns
      real	yp1(SMAX)		!i/o: Y posns
      real	hp1(SMAX)		!i/o: Heights
      integer	jfit(NUMMAX)		!i:   Fit control params
					!     (-1=fix;0=none;1=variable)
      logical	remed			!i/o: Set true if any removed;
					!     otherwise unchanged
C--
      integer k, mapnumi, magnifi, mapxi, mapyi, ierr, kww(4)
      real xpti, ypti, hpti, profi(9), rv
      logical cdomapi

      INTEGER J
Cbegin


      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_OUTCLEAN IN'')')

      do k = 1, NST
         if ( jfit(3*(k-1)+3+3).ne.0 ) then
            rv = 1.5*RAD(k)
            if ( xp1(k).lt.(1.0-rv) .or.
     +           xp1(k).gt.(LX+rv)  .or.
     +           yp1(k).lt.(1.0-rv) .or.
     +           yp1(k).gt.(LY+rv)       ) then
               if ( OUTFIX(LOOPNUM) ) then
                  jfit(3*(k-1)+1+3) = 0
                  jfit(3*(k-1)+2+3) = 0
                  jfit(3*(k-1)+3+3) = 0
                  xpti = xp1(k)
                  ypti = yp1(k)
                  hpti = hp1(k)
                  call amovr ( PROF(1,k), profi, 9 )
                  mapnumi = MAPNUM(k)
                  cdomapi = CDOMAP(k)
                  mapxi = MAPX(k)
                  mapyi = MAPY(k)
                  magnifi = MAGNIF(k)

                IF ( PRINTIT.GT.0 ) THEN
                WRITE ( 6,  '(3X,''X Y H '',3D)' )
     +          XPTI,YPTI,HPTI
                WRITE ( 6,  '(3X,''Mapn mapx mapy magn '',5I)')
     +          MAPNUMI,MAPXI,MAPYI,MAGNIFI,K
                WRITE ( 6,  '(3X,''prof1-5 '',5D)')(PROFI(J),J=1,5)
                WRITE ( 6,  '(3X,''prof6-10 '',4D)')(PROFI(J),J=6,9)
                ENDIF

                  call popsfr ( imuse, LX, LY, 1.0, imuseb, xpti, ypti,
     +                          hpti, profi, map, MX, MY, MZ, mapnumi,
     +                      mapxi, mapyi, magnifi, cdomapi, ierr, kww )
                  xp1(k) = XPOR(k)
                  yp1(k) = YPOR(k)
                  hp1(k) = 0.0
                  remed = .true.
               else
                  jfit(3*(k-1)+1+3) = -1
                  jfit(3*(k-1)+2+3) = -1
               endif
            endif
         endif
      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_OUTCLEAN OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CONPROF -- Convert profile parameters to ME_FIT ones for each star
C    Force radii, powers into acceptable ranges.
C
C    a j penny                  ral         1990-04-14

      subroutine me_conprof ( )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

C--
      integer k
Cbegin


      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_CONPROF IN'')')

      do k = 1, NST

         GX(k) = 1.0/max(0.1,PROF(1,k))					!Invert the profile radii
         GY(k) = 1.0/max(0.1,PROF(2,k))					! to match s/r convention
         P(k)  = max(0.1,min(20.0,PROF(3,k)))
         HX(k) = 1.0/max(0.1,PROF(4,k))
         HY(k) = 1.0/max(0.1,PROF(5,k))
         THETA(k) = PROF(6,k)
         QH(k) = PROF(7,k)
         if ( qh(k).ne.0.0 ) then
            QR(k) = 1.0/max(0.1,PROF(8,k))
            QP(k) = max(0.1,min(1000.0,PROF(9,k)))
         else
            QR(k) = 10.0
            QP(k) = 2.0
         endif

         CO(k) = cos(THETA(k))						!Set up the constants
         SI(k)  = sin(THETA(k))
         SIM(k)  = -1.0*SI(k)
         QF1(k) = QR(k)**QP(k)
         QF2(k) = QP(k)/2.0
         QF3(k) = QP(k)*qh(k)*QF1(k)
         QF4(k) = (QP(k) - 2.0)/2.0

         GX2(k) = GX(k)*GX(k)						!Squares of the 4 radii
         GY2(k) = GY(k)*GY(k)
         HX2(k) = HX(k)*HX(k)
         HY2(k) = HY(k)*HY(k)

      enddo

      do k = 1, NST
         RAD(k) = 0.5*(max(0.1,PROF(2,k))+max(0.1,PROF(1,k)))
      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_CONPROF OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_DOVALD -- Calcs how far out from the stars you have to go.
C      It calculates the mean 1D profile (VALD). This is then used
C      in the actual fitting, by seeing how far from each star one
C      has to go before the star pixel value is less than 0.1 and
C      thus it can be ignored.
C
C  a j penny                     ral                     1990-04-14

      subroutine me_dovald ( )

      implicit none

      include 'measure.inc'
      include 'meas_fit.inc'

C--
      integer j, k, jcha, jchb
      real dd, gav, hav, vala, valb, pow
Cbegin


      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_DOVALD IN'')')

      do k = 1, NST
         jcha = 0
         jchb = 0
         do j = 1, NUMV
            dd = real(j)

            if ( jcha.eq.0 ) then
               gav = max(GX(k),GY(k))
               hav = max(HX(K),HY(k))
               pow = P(k)*(1.0+(dd*hav))*alog(dd*gav)
               vala = 1.0/( 1.0 + exp(min(20.0,pow)) )
               if ( vala.lt.1.0e-10 ) jcha = 1
            else
               vala = 1.0e-10
            endif

            if ( jchb.eq.0 ) then
               valb = QH(k)*exp( -1.0*(min(20.0,(dd*QR(k))**QP(k))) )
               if ( valb.lt.1.0e-10 ) jchb = 1
            else
               valb = 1.0e-10
            endif

            VALD(j,k) = vala + valb

         enddo
      enddo

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_DOVALD OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_LOOPFIX -- Fix stationary stars?
C
C  a j penny                     stsci                   1987-03-01

      subroutine me_loopfix ( xp1, yp1, jfit, xp2, yp2, ngloop )

      implicit none

      include 'measure.inc'

      real	xp1(SMAX)		!i/o: Star old X posns
      real	yp1(SMAX)		!i/o: Star old Y posns
      integer	jfit(NUMMAX)		!i:   Fit control params
					!     (-1=fix;0=none;1=variable)
      real	xp2(SMAX)		!i/o: Star new X posns
      real	yp2(SMAX)		!i/o: Star new Y posns
      integer   ngloop			!i:   No of good loops
C--
      integer k
Cbegin


      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LOOPFIX IN'')')

      do k = 1, NST							!Fix stars not moving
         if ( jfit(3*(k-1)+3+3).ne.0 ) then
            if ( abs(xp1(k)-xp2(k)).lt.0.002 .and.
     +           abs(yp1(k)-yp2(k)).lt.0.002 ) then
               if ( jfit(3*(k-1)+1+3).eq.1 ) jfit(3*(k-1)+1+3) = -1
               if ( jfit(3*(k-1)+2+3).eq.1 ) jfit(3*(k-1)+2+3) = -1
            endif
         endif
      enddo

      if ( jfit(2).eq.-1 .and. ngloop.ge.1 ) then			!Release sky?
         jfit(2) = 1
         jfit(3) = 1
      endif

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LOOPFIX OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_LOOPREM -- Remove some stars from fit?
C   Remove any too close to others and those stars that are at
C   zero or below and those outside image.
C
C  a j penny                     stsci                   1987-03-01

      subroutine me_looprem ( xp1, yp1, hp1, jfit, imuse, imuseb,
     +                        remed, map )

      implicit none

      include 'measure.inc'

      real	imuse(LX,LY)		!i/o: Image array
      logical	imuseb(LX,LY)		!i:   Image array pixel use flags
      integer	jfit(NUMMAX)		!i:   Fit control params
					!     (-1=fix;0=none;1=variable)
      real	xp1(SMAX)		!i/o: Star X posns
      real	yp1(SMAX)		!i/o: Star Y posns
      real	hp1(SMAX)		!i/o: Star heights
      logical	remed			!o:   Any removed?
      real	map(MX,MY,MZ)		!i:   Profile map
C--
      integer j, k, ka, kr, kt
      real dsx, dsy
Cbegin


      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LOOPREM IN'')')

      remed = .false.							!Set default

      do k = 1, NST							!If star too faint, remove
         if ( jfit(3*(k-1)+3+3).ne.0 ) then				! it from fit
            if ( hp1(k).lt.0.0000001 ) then
               if ( jfit(3*(k-1)+1+3).ne.0 ) remed = .true.
               if ( jfit(3*(k-1)+2+3).ne.0 ) remed = .true.
               if ( jfit(3*(k-1)+3+3).ne.0 ) remed = .true.
               jfit(3*(k-1)+1+3) = 0
               jfit(3*(k-1)+2+3) = 0
               jfit(3*(k-1)+3+3) = 0
               xp1(k) = XPOR(k)
               yp1(k) = YPOR(k)
               hp1(k) = 0.0
            endif
         endif
      enddo

C  Remove stars migrating to other star posns (within 0.1 radius of
C  each other). Remove lower of pair - but not the main - first star.
C  Add removed star height to target star and average the positions

        IF ( PRINTIT.GT.1 ) THEN
        DO K = 1, NST
           WRITE ( 6, '(''Z17 I H X Y'')' )
           WRITE ( 6,  '(''  '',I3,6F10.3)')K,HP1(K),XP1(K),YP1(K)
        ENDDO
        ENDIF

      do k = 1, NST
         if ( jfit(3*(k-1)+3+3).ne.0 ) then
            do ka = k+1, NST
               if ( jfit(3*(ka-1)+3+3).ne.0 ) then
                  dsx = abs(xp1(k)-xp1(ka))
                  dsy = abs(yp1(k)-yp1(ka))
                  if ( (dsx.lt.0.1*(RAD(k)+RAD(ka))) .and.
     +                 (dsy.lt.(0.1/(RAD(k)+RAD(ka)))) ) then
                     if ( k.eq.1 .or. hp1(k).gt.hp1(ka) ) then
                        kr = ka
                        kt = k
                     else
                        kr = k
                        kt = ka
                     endif
                     do j = 1, 3
                        if (jfit(3*(kr-1)+j+3).ne.0) remed = .true.
                        jfit(3*(kr-1)+j+3) = 0
                     enddo
                     xp1(kr) = XPOR(kr)
                     yp1(kr) = YPOR(kr)
                     hp1(kr) = 0.0
                     xp1(kt) = (xp1(kt)+xp1(kr))/2.0
                     yp1(kt) = (yp1(kt)+yp1(kr))/2.0
                     hp1(kt) = hp1(kt) + hp1(kr)
                  endif
               endif
            enddo
         endif
      enddo

        IF ( PRINTIT.GT.1 ) THEN
        DO K = 1, NST
           WRITE ( 6,  '(''K H  X Y'')')
           WRITE ( 6,  '(''  '',I3,6F10.3)')K,HP1(K),XP1(K),YP1(K)
        ENDDO
        ENDIF

      call me_outclean ( imuse, imuseb, map, xp1, yp1, hp1, jfit,remed)	!If fairly outside box,
									! fix/remove star

      IF ( PRINTIT.GT.0 ) WRITE ( 6,  '(3X,''ME_LOOPREM OUT'')')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PRINT1 --
C

      SUBROUTINE ME_PRINT1 ( IMUSE, IMUSEB, MAP )

      implicit none
      include 'measure.inc'

      real      imuse(LX,LY)		!i: input array to fit
      logical 	imuseb(LX,LY)		!i: Flags for doing pixels
      real	map(MX,MY,MZ)   	!i: profile map
C--
      integer j,k
Cbegin

CX      WRITE ( 6, '('' '')')
CX      WRITE ( 6, '(''*******************'')')
CX      WRITE ( 6, '(''PRINT IT? (0=N/1..=Y)'')')
CX      WRITE ( 6, '(''*******************'')')
CX      ACCEPT*,PRINTIT

      IF ( PRINTIT.GT.0 ) WRITE ( 6, '(''X1 NST '',I5)') NST
      IF ( PRINTIT.GT.2 ) THEN
         DO K = 1, LY
            WRITE ( 6, '(''Z-3 IMUSE'')')
            WRITE ( 6,  '(1X,'' '',20i5)')(int(IMUSE(J,K)),
     +                                J=1,MIN(20,LX))
         ENDDO
         WRITE ( 6, '(''Z-2 IMAGE FLAGS '')')
         DO K = 1, LY
            WRITE ( 6,  '(1X,''SB'',30L3)')(IMUSEB(J,K),J=1,MIN(30,LX))
         ENDDO
         WRITE ( 6, '(''Z-1 MX MY MZ  '',3i5)') MX,MY,MZ
         WRITE ( 6, '(''Z0 IMAP'')')
         DO K = 1, MY
            WRITE ( 6,  '(1X,''SC'',20I5)')(INT(1000*MAP(J,K,1)),
     +                                J=1,MIN(20,MX))
         ENDDO
         WRITE ( 6, '('' '')' )
         WRITE ( 6, '(''Z1 I1 - X Y H xpor ypor'',5d)')
     +           HPI(1), XPI(1), YPI(1), XPOR(1), YPOR(1)
      ENDIF

      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PRINT2 --
C

      SUBROUTINE ME_PRINT2 ( a1, b1, c1, hp1, xp1, yp1 )

      implicit none
      include 'measure.inc'

      real    a1, b1, c1
      real    hp1(SMAX)			! 1 height for stars
      real    xp1(SMAX)			! 1 X posn for stars
      real    yp1(SMAX)			! 1 Y posn for stars

C--
      integer ii
Cbegin

      IF ( PRINTIT.GT.-1 ) THEN
         WRITE ( 6, '(1X,''Z1A  A B C'',2X,F9.1,2F8.3)')A1,B1,C1
         WRITE ( 6, '(''Z1B H X Y'')' )
      IF ( NST.EQ.1) WRITE ( 6, '(1X,2X,F10.1,2F7.2)' )
     +                       HP1(1),XP1(1),YP1(1)
      IF ( NST.EQ.2) WRITE ( 6, '(1X,2(2X,F10.1,2F7.2))' )
     +                       (HP1(II),XP1(II),YP1(II),II=1,2)
      IF ( NST.GE.3) WRITE ( 6, '(1X,3(2X,F10.1,2F7.2))' )
     +                       (HP1(II),XP1(II),YP1(II),II=1,3)
      IF ( NST.EQ.4) WRITE ( 6, '(1X,1(2X,F10.1,2F7.2))' )
     +                       HP1(4),XP1(4),YP1(4)
      IF ( NST.EQ.5) WRITE ( 6, '(1X,2(2X,F10.1,2F7.2))' )
     +                       (HP1(II),XP1(II),YP1(II),II=4,5)
      IF ( NST.EQ.6) WRITE ( 6, '(1X,3(2X,F10.1,2F7.2))' )
     +                       (HP1(II),XP1(II),YP1(II),II=4,6)
      ENDIF


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PRINT3 --
C

      SUBROUTINE ME_PRINT3 ( HP1, XP1, YP1, HP2, XP2,YP2, REMED,
     +                 ENDLOOP, NLOOP, NGLOOP,A1,B1,C1,CHISQ1,DAMP )

      implicit none
      include 'measure.inc'

      real    hp1(SMAX)			! 1 height for stars
      real    xp1(SMAX)			! 1 X posn for stars
      real    yp1(SMAX)			! 1 Y posn for stars
      real    hp2(SMAX)			! 1 height for stars
      real    xp2(SMAX)			! 1 X posn for stars
      real    yp2(SMAX)			! 1 Y posn for stars
      logical remed, endloop
      integer NLOOP, NGLOOP
      real    A1,B1,C1,CHISQ1,DAMP
C--
      integer ii
Cbegin

               IF ( PRINTIT.GT.1 ) THEN
                  DO II = 1, NST
                     WRITE (6,'(''Z10 I H1 X1 Y1 H2 X2 Y2 endloop'')')
                     WRITE ( 6, '(''  '',I3,6F10.3,L)' )II,HP1(II),
     +            XP1(II),YP1(II),HP2(II),XP2(II),YP2(II),ENDLOOP
                  ENDDO
               ENDIF

               IF ( PRINTIT.GT.0 ) THEN
                  IF(REMED)THEN
                     WRITE ( 6,
     +                      '('' Z10A  NLOOP, NGLOOP,A1,B1,C1,LX,LY,'',
     +                      ''CHISQ1,DAMP'')')
                     WRITE ( 6,
     +                     '(20X,'' '',2I4,3F8.3,2I3,F10.4,F9.4)' )
     +                   NLOOP, NGLOOP,A1,B1,C1,LX,LY,CHISQ1,DAMP
                     WRITE ( 6,  '(20x,''REMOVED STAR(S)'')')
                  ENDIF
               ENDIF

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PRINT4 --
C

      SUBROUTINE ME_PRINT4 (HP1,XP1,YP1,HP2,XP2,YP2,REMED,ENDLOOP,
     +                      NLOOP,
     +     NGLOOP,A1,B1,C1,A2,B2,C2,DCHI,CHISQ1,DAMP,CHISQ2,DH,LOOP )

      implicit none
      include 'measure.inc'

      real    hp1(SMAX)			! 1 height for stars
      real    xp1(SMAX)			! 1 X posn for stars
      real    yp1(SMAX)			! 1 Y posn for stars
      real    hp2(SMAX)			! 1 height for stars
      real    xp2(SMAX)			! 1 X posn for stars
      real    yp2(SMAX)			! 1 Y posn for stars
      logical remed, endloop,LOOP
      integer NLOOP, NGLOOP
      real    A1,B1,C1,A2,B2,C2,DCHI,CHISQ1,DAMP,CHISQ2,DH
C--
      INTEGER II
Cbegin


      IF ( PRINTIT.GT.-1 ) THEN

         WRITE ( 6, '(''Z11 DAMP,DCHI,A1,B1,C1,A2,B2,C2'')')
         WRITE ( 6, '(1X,F8.3,F7.3,2(2X,F9.1,2F8.3))' ) DAMP,DCHI,
     +                                A1,B1,C1,A2,B2,C2
         WRITE ( 6, '(''Z12 HP1,XP1,YP1'')')
         IF ( NST.EQ.1 ) WRITE ( 6, '(1X,1(2X,F10.1,2F7.2))')
     +                            HP1(1),XP1(1),YP1(1)
         IF ( NST.EQ.2 ) WRITE ( 6,  '(1X,2(2X,F10.1,2F7.2))')
     +                           (HP1(II),XP1(II),YP1(II),II=1,2)
         IF ( NST.GE.3 ) WRITE ( 6,  '(1X,3(2X,F10.1,2F7.2))')
     +                           (HP1(II),XP1(II),YP1(II),II=1,3)
         IF ( NST.EQ.4 ) WRITE ( 6,  '(1X,1(2X,F10.1,2F7.2))')
     +                           (HP1(II),XP1(II),YP1(II),II=4,4)
         IF ( NST.EQ.5 ) WRITE ( 6,  '(1X,2(2X,F10.1,2F7.2))')
     +                           (HP1(II),XP1(II),YP1(II),II=4,5)
         IF ( NST.EQ.6 ) WRITE ( 6,  '(1X,3(2X,F10.1,2F7.2))')
     +                           (HP1(II),XP1(II),YP1(II),II=4,6)
         WRITE ( 6, '('' '')')

      ENDIF

      IF ( PRINTIT.GT.0 ) THEN
         WRITE ( 6, '(''Z13 DAMP,DCHI,DH,SHIFTF,LOOP'')')
         WRITE ( 6,  '(3X,'' '',4F12.4,L4)')DAMP,DCHI,DH,SHIFTF,LOOP
         WRITE ( 6, '(
     +     ''Z14 HP2(1),XP2(1),YP2(1),,XPOR(1),YPOR(1),RAD(1)'')')
         WRITE ( 6,  '(3X,'' '',6F12.4)')HP2(1),XP2(1),YP2(1),
     +                   XPOR(1),YPOR(1),RAD(1)
      ENDIF

      IF ( PRINTIT.GE.-1 ) THEN
         WRITE ( 6, '('' '')')
         WRITE ( 6, '(''-----------------'')')
         WRITE ( 6, '(''Z15A chisq1  chisq2  damp endloop '')')
         WRITE ( 6,  '(1X,3F15.3,L)') CHISQ1, CHISQ2,DAMP,ENDLOOP
         WRITE ( 6,  '(1X,''Z15B Base Xslope Yslope 1st '',3f8.3,2x,
     +                '' 2nd '',3f8.3)')a1,b1,c1,a2,b2,c2
         WRITE ( 6, '(''Z15C I H1 X1 Y1 H2 X2 Y2'')')
         DO II = 1, NST
             WRITE ( 6,  '(3X,I2,6F8.2)')II,HP1(II),XP1(II),YP1(II),
     +                   HP2(II),XP2(II),YP2(II)
         ENDDO
         WRITE ( 6, '(''-----------------'')')
      ENDIF


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is FAKE_MESUBPAR.F
C
C  It has subroutines with calls to prallel processors
C
C  Contains Parallel Processing s/rs:-
C
C ME_PARLOAD   Load parallel processors
C ME_PAROUT    Send data (image,map,params in common block) to par processors
C ME_PARIN     Take array from par processors, store results in common block
C ME_PAR_SII    Call PAR_SII
C ME_PAR_FEND   Call PAR_FEND



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PARLOAD -- Load parallel processors
C
C     a j penny                ral                   1990-05-07

      subroutine me_parload ( inform, numpar, ierr )

      implicit none

      integer   inform		!i: Level at which to give progress messages
      integer   numpar		!i: Number of processors to load
      integer	ierr		!o: Error flag (0=ok)
C--
Cbegin

      ierr = 0
      call printo ( 'No parallel processing available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PAROUT -- Send data (image,map,params in common block) to par processors
C
C    alan penny                ral              1989-08-14

      subroutine me_parout ( imuse, map, baseo, iterlim )

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'

      real      imuse(LX*LY)		!i: input array to fit
      real	map(MX,MY,MZ) 		!i: profile map
      real      baseo         		!i: Default base
      integer   iterlim                 !i: Max no of iterations allowed
C--
Cbegin


      call printo ( 'No parallel processing available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PARIN -- Take array from par processors, store results in common block
C
C    alan penny                ral              1989-08-14

      subroutine me_parin ( basej, jstar )

      implicit none

      real        basej         !o: Default base
      integer     jstar         !o: No of star being dealt with
C--
Cbegin


      basej = 0.0
      jstar = 0
      call printo ( 'No parallel processing available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PAR_SII -- Call par_sii
C
C   a.j.penny                   ral                    1991 July

      subroutine me_par_sii (  numpar, istat )

      implicit none

      integer   numpar		!o: Number of parallel processors
      integer   istat		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      call printo ( 'ERROR: s/r ME_PAR_SII called' )
      call printo ( '       Parallel processors not available' )

      istat = 0

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_PAR_FEND -- Call PAR_FEND
C
C   a.j.penny                   ral                    1991 July

      subroutine me_par_fend (  )

      implicit none
C--
Cbegin


      call printo ( 'ERROR: s/r ME_PAR_FEND called' )
      call printo ( '       Parallel processors not available' )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is MEASURE.FOR
C
C  It contains:-
C
C T_MEASURE    Executes the MEASURE program
C ME_MASTER    Measure the stars
C ME_SETUP     Set up defaults
C ME_GCL       Get input/out measures
C ME_OPWORK    Open work space
C ME_OPWORK1   Open work space for fitting
C ME_ARRSET    Descale for BS BZ
C ME_OLOAD     Load output list with present values
C ME_CHBEFORE  Check to see if an input list is the output of MEASURE
C ME_OPDISP    Open display
C ME_IDISP     Display part of image



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_MEASURE -- Executes the MEASURE program
C
C   For a decription of this program, see MEASURE.HLP
C
C                      A History
C
C     Following work by John Pilkington on adapting some radio-astronomy
C     programs to stellar photometry in 1969, A J Penny made extensive
C     modifications to single star, then multiple stars fitting.
C     The fitting subroutine was developed further by John Straede at
C     the AAO in the course of the Vela Pulsar work, following the
C     lines of these programs.
C     A version written by KFHartley at RGO on 16/10/81
C     Modified extensively by KFH during March/April 82
C     Extensively modified by AJPenny during April/May 82
C     Completely revamped by AJPenny 83-90
C
C   a j penny                 dao           1988-04-25

      subroutine t_measure

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
C--
      integer*2 ivs
      integer   nqx(2), nqy(2), ierr, istat, iv
      real      rv
Cbegin


      call me_setup							!Set up defaults

      if ( INFORM.gt.1 ) call xxtime ( 'Started BEGIN' )

      call me_gcl							!Get the input/output
      if ( ST_FAILED ) return

      call me_opwork 							!Open working areas
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started DISPLAY' )

      if ( DODISP ) then						!Open display and display image
         call me_opdisp ( ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
         else
            call amovr ( %val(IPIMA), %val(IPCLEAN), NX*NY )
            call me_idisp ( %val(IPIMA), NX, NY, 1, NX, 1, NY )
         endif
      endif
      if ( ST_FAILED ) return

      if ( DOPAR ) then							!Load parallel processors
         call me_parload ( INFORM, NUMPAR, istat )
         if ( istat.ne.0 ) ST_FAILED = .true.
         if ( ST_FAILED ) return
      endif
      if ( ST_FAILED ) return

      if ( INFORM.gt.0 ) call printo ( 'Setting up fitting' )		!Let users know progress

      call achtsr ( %val(IPMAP), %val(IPRMAP), MX*MY*MZ )		!Profile map to work area.
      call arrsc  (  %val(IPRMAP), MX, MY*MZ, PBS, PBZ )

      call me_rload ( %val(IPTBA), TBXVA, TBYA, %val(IPRES), KHEAD, 0 )	!Clear work  array; Load
      if ( ST_FAILED ) return						! posns, heights, etc

      if ( EXTRA ) call me_rload ( %val(IPTBE), TBXVE, TBYE, 		!Do same for any 2nd file
     +                             %val(IPRES), KHEADE, TBYA )
      if ( ST_FAILED ) return
      call me_xytran ( %val(IPINAE), 3, TBY )

      if ( INFORM.gt.1 ) call xxtime ( 'Started BEFORE' )

      call me_cpbefore ( %val(IPTBB), %val(IPRES) )			!Load any before list,
      if ( ST_FAILED ) return						! for valid values

      call me_inbox ( %val(IPRES), %val(IPDOM) )			!Calc boxes round stars,
      if ( ST_FAILED ) return						! see what stars have
									!  boxes in image

      call me_opwork1 ( %val(IPRES) )					!Open fitting work space
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started DOVOL' )

      call me_dovol ( %val(IPRES), %val(IPDOM) )			!Calc the profile volumes
      if ( ST_FAILED ) return


      call me_eheight ( %val(IPIMA), %val(IPRES) )			!Get rough heights
      call me_rheight ( %val(IPRES) )					!Refine rough mags
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started SULIST' )

      call me_sulist ( %val(IPRES) )					!Find bright, close companions
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started CHEXTRA' )

      call me_chextra ( %val(IPRES) )					!Check effect extra stars
      if ( ST_FAILED ) return						!(if any) on premeasures

      call coprr ( %val(IPRES), TBX, TBY, 21, 21, 1, TBY,	 	!Note 1st loop no of
     +             %val(IPINAE), 3, TBY, 3, 1 )				! companion stars
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started OLOAD' )

      call me_oload 							!Load output list with present state
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started SORTL' )

      call me_sortl ( %val(IPRES), %val(IPHA), %val(IPHB) )		!Find order to do them in
      if ( ST_FAILED ) return

      if ( INFORM.gt.1 ) call xxtime ( 'Started RANGES' )

      nqx(1) = 1							!Find mean sky level
      nqx(2) = NX
      nqy(1) = 1
      nqy(2) = NY
      call ranger ( %val(IPIMA), NX, NY, nqx, nqy, RINVAL, SKY, rv,iv)
      if ( ST_FAILED ) return

      call me_master							!Do the measures
      if ( ST_FAILED ) return

      if ( CLSTORE ) then						!Store subtracted image
         call azsubkr ( %val(IPCLEAN), RINVAL, BZ, %val(IPIMA), NX*NY )
         call azdivkr ( %val(IPIMA), RINVAL, BS, %val(IPCLEAN), NX*NY )
         if ( IMTYPE.eq.'SHORT' ) then
            ivs = INVAL
            call azchtrs ( %val(IPCLEAN), RINVAL, INT_MINRR, INT_MAXRR,
     +                     %val(IPIMCL), ivs, NX*NY )
         else
            call amovr ( %val(IPCLEAN), %val(IPIMCL), NX*NY )
         endif
      endif
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_MASTER -- Measure the stars
C
C   a j penny                 dao           1988-04-25

      subroutine me_master

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer kdo
Cbegin


      if ( ST_FAILED ) return						!Failure check

      if ( DODISP .or. CLSTORE .or. REDOCL .or. TWOS ) then		!Load cleaning image
         call amovr ( %val(IPIMA), %val(IPCLEAN), NX*NY )
      endif

      kdo = 1								!Do the stars once or twice
      if ( RECYCLE ) kdo = 2						! in cycle
      do LOOPNUM = 1, kdo

         if ( LOOPNUM.eq.2 ) then					!If 2nd loop, recalc
            call me_eheight ( %val(IPIMA),  %val(IPRES) )		! importance of neighbours
            call me_rheight ( %val(IPRES) )				! and new measure order
            call me_sulist  ( %val(IPRES) )
            call me_sortl   ( %val(IPRES), %val(IPHA), %val(IPHB) )
            call me_nonecl  ( %val(IPRES) )				! Flag none as cleaned
         endif

         if ( LOOPNUM.eq.2 ) then					!Re-load input image
            if ( DODISP .or. CLSTORE .or. REDOCL .or. TWOS ) then
               call amovr ( %val(IPIMA), %val(IPCLEAN), NX*NY )
            endif
            if ( DODISP ) call me_idisp ( %val(IPCLEAN), NX, NY,
     +                                    1, NX, 1, NY )
         endif

         NUMOP = 1							!Do the actual work of fit Lorentzians to the
         call me_mags ( %val(IPIMA), %val(IPRES), %val(IPINAE) )	! stars, then store results in output file

         if ( REDOCL .or. TWOS .or. CLSTORE ) call me_befclear (	!Remove 'before' stars, not done, from clean image
     +                                                 %val(IPRES) )

         if ( REDOCL ) then						!Redo cleaned image; clean
            NUMOP = 2							! any done
            call me_mags ( %val(IPCLEAN), %val(IPRES), %val(IPINAE) )
         endif

         if ( TWOS ) then						!Do undone pairs on the
            NUMOP = 3							! cleaned image, cleaning
            call me_twos ( %val(IPRES), %val(IPINAE), %val(IPKREM) )	! any done
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_SETUP -- Set up defaults
C
C   A J PENNY               stsci              1987 Oct 87

      subroutine me_setup ()

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'
C--
Cbegin


      NX = 1 								!Default array sizes
      NY = 1
      TBXVA = 1
      TBYA = 1
      TBXVB = 1
      TBYB = 1
      TBXVE = 1
      TBYE = 1
      TBY = 1
      MX = 1
      MY = 1
      MZ = 1

      IPTBA = 1								!Fake pointers
      IPTBB = 1								! for failure
      IPTBE = 1
      IPTBOUT = 1
      IPRES = 1
      IPDOM = 1
      IPRMAP = 1
      IPIM = 1
      IPIMA = 1
      IPINAE = 1
      IPHA = 1
      IPHB = 1
      IPCLEAN = 1
      IPKREM = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_GCL -- Get input/out measures
C    Any failures give exit from this s/r at once
C
C   A J PENNY               stsci              1987 Oct 87

      subroutine me_gcl ()

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer*2  ivs
      integer imap, istat
      real    qbase
      logical flag
      character text*72
Cbegin


      if ( ST_FAILED ) return

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!Get the image data array
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,istat)
      if ( ST_FAILED ) return
      call gtwrkr ( 'IPIMA', NX*NY, IPIMA, istat )
      if ( IMTYPE.eq.'SHORT' ) then
         RINVAL = INVAL
         ivs = INVAL
         call azchtsr ( %val(IPIM), ivs, %val(IPIMA), RINVAL, NX*NY )
      else
         call amovr ( %val(IPIM), %val(IPIMA), NX*NY )
      endif
      call me_arrset ( %val(IPIMA), BS, BZ, RINVAL, NX*NY )
      if ( ST_FAILED ) return

      call optabr ( 'INSTARS', IPTBA, TBXVA, TBYA, .false., istat )	!Seek list of star posns
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call gettheh ( 'INSTARS', TBXVA, KHEAD )

      call get_mprof ( 'PROFILE', IPMAP, PROFK, MX, MY, MZ, MAGNIFK, 	!Get profile file
     +            MAPXK, MAPYK, PBS, PBZ, qbase, VOLK, VOLRADK, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Profile unusable' )
         return
      endif
      MAPNUMK = 1
      imap = istat

      call optabr ( 'EXTRA', IPTBE, TBXVE, TBYE, .true., istat )	!Input an extra star list.
      if ( ST_FAILED ) return						! If none, open fake one
      if ( istat.eq.2 ) then
         EXTRA = .false.
         IPTBE = IPTBA
      else if ( istat.eq.0 ) then
         call gettheh ( 'EXTRA', TBXVE, KHEADE )
         EXTRA = .true.
      else
         ST_FAILED = .true.
         return
      endif
      if ( ST_FAILED ) return

      TBY = TBYA							!Calc total no of stars
      if ( EXTRA ) TBY = TBYA + TBYE

      XCOEFFS(1) = 0.0
      XCOEFFS(2) = 1.0
      XCOEFFS(3) = 0.0
      YCOEFFS(1) = 0.0
      YCOEFFS(2) = 0.0
      YCOEFFS(3) = 1.0
      call get3r ( 'XCOEFF', XCOEFFS(1), XCOEFFS(2), XCOEFFS(3),
     +             .true., -1.0e20, 1.0e20 )
      call get3r ( 'YCOEFF', YCOEFFS(1), YCOEFFS(2), YCOEFFS(3),
     +             .true., -1.0e20, 1.0e20 )

      call optabw ( 'OUT', IPTBOUT, TBXVO, TBY, .false., istat )	!Open output results file
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      text = IMTITLE
      if ( text.eq.' ' ) text = 'Star results from MEASURE'
      call get1c ( 'OUTTIT', STITLE, text, .true. )
      if ( ST_FAILED ) return

      call opimzw ( 'OUTIM', IMTYPE, IPIMCL, NX, NY, .true., istat )	!Get if to store star
      if ( istat.eq.1 ) ST_FAILED = .true.				! subtracted image. If
      if ( ST_FAILED ) return  						! so open its output
									! file and copy headers
      if ( istat.eq.2 ) then
         CLSTORE = .false.
      else
         CLSTORE = .true.
         text = IMTITLE
         if ( text.eq.' ' ) text = 'Cleaned image from MEASURE'
         call get1c  ( 'OIMTIT', CTITLE, text, .true. )
         if ( ST_FAILED ) return
         call icopdes ( 'IN', 'OUTIM' , istat )
         call ptdesc ( 'OUTIM', 'TITLE', CTITLE )
         if ( IMTYPE.eq.'SHORT' ) then
            call ptdesi ( 'OUTIM', 'INVAL', INVAL )
         else
            call ptdesr ( 'OUTIM', 'INVAL', RINVAL )
         endif
         call ptdesr ( 'OUTIM', 'BSCALE', BS )
         call ptdesr ( 'OUTIM', 'BZERO', BZ )
      endif

      call optabr ( 'BEFORE', IPTBB, TBXVB, TBYB, .true., istat )	!Bring in any previous
      if ( ST_FAILED ) return
      if ( istat.eq.2 ) then						! measures
         BEFORE = .false.
         IPTBB = IPTBA
      else if ( istat.eq.0 ) then
         call me_chbefore
         if ( .not.BEFORE ) ST_FAILED = .true.
      else
         ST_FAILED = .true.
      endif
      if ( ST_FAILED ) return

      DOMAP = .false.							!use the profile map?
      if ( imap.eq.0 ) call get1b ( 'DOMAP', DOMAP, .true. )
      if ( ST_FAILED ) return

      PBOSS = .false.							!profile map overrides?
      if ( imap.eq.0 ) call get1b ( 'PYES', PBOSS, .true. )		! star list profiles?
      if ( ST_FAILED ) return

      flag = .false.							!Get if calc trial heights
      if ( KHEAD(3).ne.0 ) flag = .true.				! or to take them from
      if ( EXTRA ) then							! input file
         if ( KHEADE(3).ne.0 ) flag = .true.
      endif
      ESTH_DO = .false.
      if ( flag ) then
         call get1b ( 'HEIGHTS', flag, .true. )
         if ( ST_FAILED ) return
         if ( .not.flag ) ESTH_DO = .true.
      endif

      call get1b ( 'REDOCL', REDOCL, .true. )				!redo cleaned image?
      if ( ST_FAILED ) return

      call get1b ( 'DOTWOS', TWOS, .true. )				!put star at centre of
      if ( ST_FAILED ) return						! close undone doubles?

      call get1b ( 'RECYCLE', RECYCLE, .true. )				!recycle at end, retry
      if ( ST_FAILED ) return						! with the new heights
									! and posns found?

      call get1b ( 'OFIX1', OUTFIX(1), RECYCLE )			!Fix points outside box?
      if ( ST_FAILED ) return
      if ( RECYCLE ) call get1b ( 'OFIX2', OUTFIX(2), .true. )
      if ( ST_FAILED ) return

      call get1r ( 'RADIUS', RADIUS, 30.0, 1.0, 1.0e8 )			!Get radius to integrate
      if ( ST_FAILED ) return						! out to for VOL
      call ptdesr ( 'OUT', 'RADIUS', RADIUS )

      call get1i ( 'NUMBER', NUMBER, TBY, 1, TBY )			!Get number to do
      if ( ST_FAILED ) return

      LXSIZE = 0							!Get box size
      LYSIZE = 0
      call get2i ( 'BOX', LXSIZE, LYSIZE, .true., 0, 200 )
      if ( ST_FAILED ) return

      call get1b ( 'FIX', POSNFIX, .false. )				!Get if to fix posns
      if ( ST_FAILED ) return

      call get1r ( 'GAIN',  ZGAIN,  1.0, 1.0e-8, 1.0e8 )		!Get poisson values of
      call get1r ( 'NOISE', ZNOISE, 0.0, -1.0e8, 1.0e8 )		! input image
      if ( ST_FAILED ) return

      call get1b ( 'DODISP', DODISP, .false. )				!Display progress on image
      if ( ST_FAILED ) return

      DOPAR = .false.
CXX      call get1b ( 'DOPAR', DOPAR, .false. )				!Get if use parallel
CXX      if ( DOPAR ) call get1i ( 'NUMPAR', NUMPAR, 16, 1, 100000 )

      call get_job ( 'INFORM', 'none:standard:more:full', INFORM,	!Get if type info/times
     +               2, ' ', 0 )
      if ( ST_FAILED ) return
      INFORM = INFORM - 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OPWORK -- Open work space
C
C   a j penny                 dao           1988-04-25

      subroutine me_opwork ()

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( .not.ST_FAILED .and.
     +     (DODISP.or.CLSTORE.or.REDOCL.or.TWOS) )then
         call gtwrkr ( 'WDISP',     NX*NY, IPCLEAN, istat )
      endif
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'RES', TBX*TBY,  IPRES, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'INAE',  3*TBY, IPINAE, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'TDOMAP',  TBY,  IPDOM, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'RMAP', MX*MY*MZ,IPRMAP,istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'HTSA',    TBY,   IPHA, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'HTSB',    TBY,   IPHB, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'KREM',    TBY, IPKREM, istat)
      if ( istat.ne.0 ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OPWORK1 -- Open work space for fitting
C
C   a j penny                 dao           1988-04-25

      subroutine me_opwork1 ( res )

      implicit none

      include 'measure.inc'
      include 'STARMAN_INC'

      real	res(TBX,TBY)		!i: Data storage array
C--
      integer k, ksize, istat
Cbegin


      if ( ST_FAILED ) return						!Failure check

      MAXAREA = 1							!Max box area
      if ( LXSIZE.ne.0 .and. LYSIZE.ne.0 ) then
         MAXAREA = LXSIZE*LYSIZE
      else
         do k = 1, TBY
            ksize = nint(res(29,k))*nint(res(30,k))
            MAXAREA = max(MAXAREA,ksize)
         enddo
      endif

      call gtwrkr ( 'EAREA', MAXAREA, IPRWORK, istat )			!Open work
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'BAREA',  MAXAREA, IPBWORK,
     +                                    istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'FAREA', SMAX*MAXAREA,
     +                                    IPFWORK, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_ARRSET -- Descale for BS BZ
C
C   a j penny                 dao           1988-04-25

      subroutine me_arrset  ( in, abs, abz, arinval, n )

      implicit none
      include 'STARMAN_INC'

      integer     n		!i: No of values
      real        in(n)		!i/o: Array
      real        abs		!i: Scale
      real        abz		!i: Zero
      real        arinval	!i: Invalid magic value
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, n
         if ( in(j).ne.arinval ) in(j) = abs*in(j) + abz
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OLOAD -- Load output list with present values
C
C   a j penny                 dao           1988-04-25

      subroutine me_oload ( )

      implicit none
      include 'measure.inc'
      include 'STARMAN_INC'
C--
      integer k, istat
Cbegin


      if ( ST_FAILED ) return						!Failure check

      call azeror ( %val(IPTBOUT), TBXVO*TBY )				!Load output stars file
									! list starting contents

      call coprr ( %val(IPTBA), TBXVA, TBYA, 1, 5, 1, TBYA,		!Copy identifiers to output
     +             %val(IPTBOUT), TBXVO, TBY, 1, 1 )
      if ( EXTRA ) call coprr ( %val(IPTBE), TBXVE, TBYE, 1, 5, 1, TBYE,
     +                          %val(IPTBOUT), TBXVO, TBY, 1, (TBYA+1) )
      call me_xytrana ( %val(IPTBA), TBXVA, TBYA,
     +                  %val(IPTBOUT), TBXVO, TBY, 0 )
      if ( EXTRA ) call me_xytrana ( %val(IPTBE), TBXVE, TBYE,
     +                               %val(IPTBOUT), TBXVO, TBY, TBYA )

      call tcopdes ( 'INSTARS', 'OUT' , istat )				!Copy file descriptors
      call ptdesc ( 'OUT', 'TITLE', STITLE )				! to output. Load other headers
      do k = 1, TBX
         call pthead ( 'OUT', k, HEADER(k), istat )
      enddo

      call coprr ( %val(IPRES), TBX, TBY, 1, TBX, 1, TBY,		!Load output list with
     +             %val(IPTBOUT), TBXVO, TBY, 6, 1 )			! present reduction state


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_CHBEFORE -- Check to see if an input list is the output of MEASURE
C
C     a j penny                stsci                   1987-02-24

      subroutine me_chbefore ( )

      implicit none

      include 'measure.inc'
C--
      character*20  ahead
      integer k, istat
Cbegin


      BEFORE = .true.							!Set up default flag

      if ( TBXVB.ne.(TBX+5) .or. TBYA.ne.TBYB ) then
         call printo (
     +      'ERROR: Before table must be same size as Input table')
         BEFORE = .false.
         return
      endif

      do k = 1, TBX							!Headers are OK?
         call gthead ( 'BEFORE', k, ahead, istat )
         if ( istat.ne.0 ) then
            call pargi ( k )
            call printd ( 'ERROR: Header %d in Before table wrong' )
            BEFORE = .false.
         elseif ( ahead.ne.HEADER(k) ) then
            call pargi ( k )
            call pargc ( ahead )
            call printd ( 'ERROR: Header %d in Before table is %c' )
            call pargc ( HEADER(k) )
            call printd ( 'ERROR:      it should be: %c ' )
            BEFORE = .false.
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_OPDISP -- Open display
C
C     a j penny                ral                   1990-05-07

      subroutine me_opdisp ( ierr )

      implicit none

      include 'measure.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer	ierr		!o: Error flag (0=ok)
C--
      character text*72
Cbegin


      ierr = 0								!Failure flag

      call ds_sdef ( 1, 1 )						!Set Display defaults

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) return

      DSNXS = 1								!Image size
      DSNXE = NX
      DSNYS = 1
      DSNYE = NY

      text = IMTITLE
      if ( text.eq.' ' ) text = 'MEASURE progress on image'
      call ds_init ( text, 0, ierr )					!Open display
      if ( ierr.ne.0 ) return
      call ds_pttit ( text )

      call ds_gtcomf ( 1 )						!Get image display size compression

      call ds_imgscl ( %val(IPIMA), NX, NY, 'REAL', 1, NX, 1, NY )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ME_IDISP -- Display part of image
C
C     a j penny                ral                  1990-05-07

      subroutine me_idisp ( im, nx, ny, nxs, nxe, nys, nye )

      implicit none
      include 'ST_DS_GEN_INC'

      integer       nx		!i: X size of image
      integer       ny		!i: Y size of image
      real          im(*)	!i: Image
      integer       nxs		!i: X start of image
      integer       nxe		!i: X end of image
      integer       nys		!i: Y start of image
      integer       nye		!i: Y end of image
C--
      integer kxs, kys
Cbegin


      kxs = 1 + (nxs-1)/DSCOMFX
      kys = 1 + (nys-1)/DSCOMFY
      call ds_acim ( im, nx, ny, 'REAL', nxs, nxe, nys, nye,
     +               kxs, kys, .false.)


      end

