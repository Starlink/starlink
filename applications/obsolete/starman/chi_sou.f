CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI -- This program finds chi values for rejection from MEASURES output
C
C             For a decription of this program, see CHI.HLP
C
C   A.J.Penny                 RAL            1994 May

      subroutine chi (ierradam)

      implicit none
      integer   ierradam
C--
Cbegin

      call starman_start

      call t_chi

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    T_CHI.F
C
C   Contains:-
C T_CHI         Look at MEASURE output to flag 'bad' stars
C CHI_GCL       Get CL input
C CHI_LOADDAT   Load star mags, chi. Get no of good stars
C CHI_GETDAT    Extract wanted data from the input file
C CHI_DLIM      Get distance limits
C CHI_NUMITS    Get iteration number limits
C CHI_CHILAW    Calc a curve under the Chi - Mag distribution of points
C CHI_PCDATA    Plot data and use cursor to get run of allowed Chi values
C CHI_AUTOC     Get automatically the run of allowed Chi values with magn
C CHI_PLOT      Plot out Chi values
C CHI_AUTOPLOT  Plot out automatic Chi values
C CHI_CUR       Use cursor to get and mark Chi limits
C CHI_OUTLINE   Turn cursor posns to 100 output Chi limits
C CHI_CHINUM    How many out by chi
C CHI_STORE     Store results
C CHI_ALIMR     Finds the min and max of a real vector  with use flag
C CHI_AAVGR     Mean and std dev of a column in the got data



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_CHI -- Look at MEASURE output to flag 'bad' stars
C
C   a j penny                 dao           1988-04-25

      subroutine t_chi ()

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ipa, istat
Cbegin


      call chi_gcl							!Set up and get CL input

      call gtwrkr ( 'WORKA', 7*TBY, ipa, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.

      call chi_loaddat ( %val(ipa) )					!Load star mags,Chis;no of stars

      if ( DOGRAPH ) then						! Open graph device
         call gd_open ( istat )
         if ( istat.ne.0 ) ST_FAILED = .true.
      endif

      if ( MODE.ne.'auto' ) then
         call chi_dlim ( %val(ipa) )					!Get distance limits
         call chi_numits ( %val(ipa) )					!Get iteration number limits
      endif

      call chi_chilaw  ( %val(ipa) )					!Remove Chi-Mag growth effect

      if ( MODE.eq.'auto' ) then					!Get CHI limits with magn
          call chi_autoc ( %val(ipa) )
      else
          call chi_pcdata  ( %val(ipa) )
      endif

      if ( DOGRAPH ) call gd_close

      call chi_outline ( %val(ipa) )					!Make output line

      call chi_chinum  ( %val(ipa) )					!How many out by chi

      call chi_store   ( %val(ipa) )					!Store results


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_GCL -- Get CL input
C
C   a j penny                 dao           1988-04-25

      subroutine chi_gcl ()

      implicit none

      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer k, iv, istat
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN', IPT, TBVX, TBY, .false., istat )		!Get Magnitude table
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      if ( TBVX.lt.13 ) then
         call printo (
     +        'ERROR: File not good Chi input -less than 8 columns' )
         ST_FAILED = .true.
         return
      endif

      call gtdesc ( 'IN', 'TITLE', TITLE, ' ', iv, istat )		!Put title

      call get_job ( 'USE', 'auto:inter', k, 1, ' ', 0 )
      MODE = 'auto'
      if ( k.eq.2 ) MODE = 'inter'

      call get1i ( 'NUMINV', INVLIM, 0, 0, 1000000 )

      DOGRAPH = .true.
      if ( MODE.eq.'auto' ) then
         call get1i ( 'NUMITS',  ITLIM,  29,   0,   1000 )
         call get1r ( 'OFFCEN',   DLIM, 2.0, 0.0, 1.0e10 )
         call get1r ( 'CHILIM', CHILIM, 2.0, 0.0, 1.0e10 )
         call get1b ( 'GRAPH', DOGRAPH, .false. )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_LOADDAT -- Load star mags, Chis. Get no of good stars
C
C   a j penny                 dao           1988-04-25

      subroutine chi_loaddat ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      real ama, chi, dx, dy, dd, dr, rmaj, rmin
      integer k, ktota, ktotb, ktotc, ktotd, ktote, ninval, iter
Cbegin


      if ( ST_FAILED ) return

      ktota = 0
      ktotb = 0
      ktotc = 0
      ktotd = 0
      ktote = 0
      do k = 1, TBY							!Load good stars
         call chi_getdat ( %val(IPT), k, ama, dx, dy, iter, chi,
     +                     ninval, rmaj, rmin )
         data(k,1) = ama
         data(k,2) = chi
         data(k,3) = dx
         data(k,4) = dy
         data(k,5) = iter
         data(k,6) = ninval
         data(k,7) = 0.0

         if ( ama.gt.49.9 ) then
            data(k,7) = 1.0
            ktota = ktota + 1
         elseif ( ninval.gt.INVLIM ) then
            data(k,7) = 2.0
            ktotb = ktotb + 1
         endif

         if ( MODE.eq.'auto' ) then
            if ( iter.gt.ITLIM ) then
               data(k,7) = 4.0
               ktotc = ktotc + 1
            endif
            dx = data(k,3)
            dy = data(k,4)
            dd = sqrt((dx*dx)+(dy*dy))
            dr = sqrt((rmaj*rmaj)+(rmin*rmin))
            if ( (dd/dr).gt.DLIM ) then
               data(k,7) = 3.0
               ktotd = ktotd + 1
            endif
         endif

         if ( data(k,7).lt.0.5 ) ktote = ktote + 1

      enddo

      call printo ( ' ' )						!Print out results so far

      call pargi ( TBY )
      call printd ( 'Total no of stars = %d ' )
      call pargi ( ktota )
      call printd ( 'No of stars with no good fit = %d ' )
      call pargi ( ktotb )
      call printd ( 'No of stars with too many invalid pixels = %d ' )
      if ( MODE.eq.'auto' ) then
         call pargi ( ktotc )
         call printd ( 'No of stars with too many iterations  = %d ' )
         call pargi ( ktotd )
         call printd ( 'No of stars too far from input posns  = %d ' )
      endif

      call pargi ( ktote )
      call printd ( 'No of good fitted stars = %d ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_GETDAT -- Extract wanted data from the input file
C
C  a j penny                     dao               1988-05-17

      subroutine chi_getdat ( tb, k, amag, dx, dy, iter, chi,
     +                        ninval, rmaj, rmin )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real	tb(TBVX,TBY)		!i: Input table
      integer	k			!i: Star to get
      real	amag			!o: magnitude
      real	dx			!o: X posn shift
      real	dy			!o: Y posn shift
      integer	iter			!o: No of iterations
      real	chi			!o: Chi value
      integer	ninval			!o: No of invalid points
      real	rmaj			!o: Profile major axis
      real	rmin			!o: Profile minor axis
C--
Cbegin


      if ( ST_FAILED ) return

      amag = tb(8,k)
      dx = tb(9,k)
      dy = tb(10,k)
      iter = nint(min(10000.0,max(0.0,tb(11,k))))
      chi = tb(12,k)
      ninval = nint(min(1.0e6,max(0.0,tb(13,k))))
      rmin = tb(17,k)
      rmaj = tb(18,k)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_DLIM -- Get distance limits
C
C   a.j.penny                   rgo                    83-8-14

      subroutine chi_dlim ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)		!i: Data
C--
      integer j, js, k, ka, kdd, kd(120), jd(30), jmax, km, kdiv,
     +        kgood, kout
      real dd, dx, dy, ajmax, dmax, x, y, rv
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ( 'Limits allowed for distance from input posns?' )
      call printo ( ' ' )

      call azeroi ( kd, 120 )
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            dx = data(k,3)
            dy = data(k,4)
            dd = 4.0*sqrt((dx*dx)+(dy*dy))
            kdd = min(120.0,dd)
            kd(kdd) = kd(kdd) + 1
         endif
      enddo

      km = 1
      do k = 1, 120
         if ( kd(k).ne.0 ) km = k
      enddo
      if ( km.gt.60 ) then
         dmax = 30.0
         kdiv = 4
      elseif ( km.gt.30 ) then
         dmax = 15.0
         kdiv = 3
      elseif ( km.gt.15 ) then
         dmax = 10.0
         kdiv = 2
      else
         dmax = 7.5
         kdiv = 1
      endif

      js = 0
      do j = 1, 30
         jd(j) = 0
         do k = 1, kdiv
            ka = js + k
            jd(j) = jd(j) + kd(ka)
         enddo
         js = js + kdiv
      enddo

      jmax = 0
      do j = 1, 30
         jmax = max(jmax,jd(j))
      enddo
      ajmax = 1.1*real(jmax)

      call gd_dobox ( 0.0, dmax, 'Distance from Input Position',
     +                0.0, ajmax, 'Number of stars', ' ', 0 )

      call pgbbuf
      call pgmove ( 0.0, 0.0 )

      x = 0.0
      do k = 1, 30
         y = jd(k)
         call pgdraw ( x, y )
         x = x + 0.25*real(kdiv)
         call pgdraw ( x, y )
      enddo
      call pgdraw ( x, 0.0 )
      call pgebuf

      rv = real(km+1)/4.0
      call get1r ( 'OFFCEN', DLIM, rv, 0.0, 1.0e10 )

      kout = 0
      kgood = 0
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            dx = data(k,3)
            dy = data(k,4)
            dd = sqrt((dx*dx)+(dy*dy))
            if ( dd.gt.DLIM ) then
               data(k,7) = 3.0
               kout = kout + 1
            else
               kgood = kgood + 1
            endif
         endif
      enddo
      call printo ( ' ' )
      call pargi ( kout )
      call printd ( 'No of stars too far from input posn = %d ' )
      call pargi ( kgood )
      call printd ( 'No of good fitted stars left = %d ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_NUMITS -- Get iteration number limits
C
C   a.j.penny                   rgo                    83-8-14

      subroutine chi_numits ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)		!i: Data
C--
      integer k, kdd, kd(120), kmax, km, kgood, kout
      real akmax, dmax, x, y
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ( 'Limits allowed for number of iterations?' )
      call printo ( ' ' )

      call azeroi ( kd, 120 )
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            kdd = data(k,5)
            kdd = min(120,kdd)
            kd(kdd) = kd(kdd) + 1
         endif
      enddo

      km = 1
      do k = 1, 120
         if ( kd(k).ne.0 ) km = k
      enddo
      dmax = 1.1*real(km)

      kmax = 0
      do k = 1, 120
         kmax = max(kmax,kd(k))
      enddo
      akmax = 1.1*real(kmax)

      call pgpage
      call gd_dobox ( 0.0, dmax, 'Number of iterations',
     +                0.0, akmax, 'Number of stars', ' ', 0 )

      call pgbbuf
      call pgmove ( 0.0, 0.0 )

      x = 0.0
      do k = 1, 30
         y = kd(k)
         call pgdraw ( x, y )
         x = x + 1.0
         call pgdraw ( x, y )
      enddo
      call pgdraw ( x, 0.0 )
      call pgebuf

      call get1i ( 'NUMITS', ITLIM, km, 0, 1000 )

      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
         endif
      enddo
      kout = 0
      kgood = 0
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            if ( data(k,5).gt.ITLIM ) then
               data(k,7) = 4.0
               kout = kout + 1
            else
               kgood = kgood + 1
            endif
         endif
      enddo
      call printo ( ' ' )
      call pargi ( kout )
      call printd ( 'No of stars with too many iterations = %d ' )
      call pargi ( kgood )
      call printd ( 'No of good fitted stars left = %d ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_CHILAW -- Calc a curve under the Chi-Mag distribution of points
C
C   a.j.penny                   rgo                    83-8-14

      subroutine chi_chilaw ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)		!i: Data
C--
      real ybox(25), xbox(25), xx(25), xmin, xmax, ymin, ymax, diff,
     +     chimin, sl, ze, zemin, xd, xdmin, chi, rms, slmin, f
      integer j, k, nk, numfive, ns
      double precision ta(25), tb(25), tc(25)
      logical first
Cbegin


      if ( ST_FAILED ) return

      SLOPE = 0.0							!Set default
      ZERO = 0.0
      ZMAG = 25.0
      REDUCE = .true.

      ns = TBY
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) ns = ns - 1
      enddo
      if ( ns.lt.4 ) then						!Check enough points
         call printo ( 'Less than four good values.'  )
         call printo ( ' - so cannot fit a curving base to the run '//
     +               'of Chi values with magn')
         call printo ( ' - so the raw Chi values are used.' )
         REDUCE = .false.
         return
      endif
									!Calc curving base
      call chi_alimr ( data, 1, TBY, xmin, xmax )
      call chi_alimr ( data, 2, TBY, ymin, ymax )

      call amovkr ( 1.0e10, ybox, 25 )					!Find Y min in each X box,
      call azeror ( xbox, 25 )						! and the X of that Y
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            diff = xmax - data(k,1)
            if ( diff.gt.2.1 .and. diff.lt.12.4 ) then
               j = 2.0*diff + 1.0
               if ( data(k,2).lt.ybox(j) ) then
                  ybox(j) = data(k,2)
                  xbox(j) = data(k,1)
               endif
            endif
         endif
      enddo

      do k = 1, 24							!Bunch up empty X boxes
         if ( ybox(k).gt.1.0e9 ) then
            do j = k, 24
               ybox(j) = ybox(j+1)
               xbox(j) = xbox(j+1)
            enddo
         endif
      enddo

      nk = 0								!No of full boxes + sub
      do k = 1, 25							!Y min box Y val
         if ( ybox(k).le.1.0e9 ) then
            nk = nk + 1
            ybox(k) = ybox(k) - ymin
         endif
      enddo

      if ( nk.le.2 ) then						!Calc curve
         REDUCE = .false.
      else
         numfive = min(100,nint((xmax-xmin)*5.0))
         first = .true.
         chimin = 1.0e30
         do k = 1, numfive
            xd = xmax - (k-1)*0.2
            do j = 1, nk
               xx(j) = 10.0**((xd-xbox(j))/2.5)
            enddo
            call linfit ( xx, ybox, nk, sl, ze, ta, tb, tc, chi, rms )
            if ( first .or. chi.le.chimin ) then
               first = .false.
               chimin = chi
               slmin = sl
               zemin = ze
               xdmin = xd
            endif
         enddo
         sl = slmin
         ze = max(0.0,zemin) + chimin
      endif

      if ( sl.lt.1.0e-5 ) then
         REDUCE = .false. 						!Chi must go up for bright stars
         call printo ( 'The data do not seem to have a curving' )
         call printo ( 'base to the run of Chi values with magn.' )
         call printo ( ' - so the raw Chi values are used.' )
         call printo ( ' ' )
      elseif ( ze.lt.1.0e-10 ) then
         REDUCE = .false. 						!Chi min must not be too small
         call printo ( 'The data have in them a Chi-value which' )
         call printo ( 'is too near zero for this algorithm' )
         call printo ( ' - so the raw Chi values are used.' )
         call printo ( ' ' )
      else								!Scale Chi values and load
         SLOPE = sl							! SLOPE and ZERO
         ZERO = ze
         ZMAG = xdmin
         do k = 1, TBY
            if ( data(k,7).lt.0.5 ) then
               f = sl*(10.0**((ZMAG-data(k,1))/2.5)) + ze
               data(k,2) = (data(k,2)-f)/sqrt(f/ze)
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_PCDATA -- Plot data and use cursor to get line
C
C   a j penny                 dao           1988-04-25

      subroutine chi_pcdata ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      logical loop
Cbegin


      if ( ST_FAILED ) return

      call pgpage
      loop = .true.
      do while ( loop )
         call chi_plot ( data )						! Plot diagram
         call chi_cur 							! Get Chi limits by cursor
         call get1b ( 'AGAIN', loop , .false. )				!Do again?
         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_AUTOC -- Get run of allowed Chi values with magn
C
C   a j penny                 dao           1988-04-25

      subroutine chi_autoc ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      real av, sd, xmin, xmax, tav, tsd, ya, am, f,
     +     rav(100), rsd(100)
      integer k, ka, kxmin, kxmax, knum(100), num, ipw, ipr, ipb,
     +        ierr
      character text*72
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr ( 'AUTOCR', TBY, ipr, ierr )
      call gtwrkr ( 'AUTOCW', TBY, ipw, ierr )
      call gtwrkr ( 'AUTOCB', TBY, ipb, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call chi_alimr ( data, 1, TBY, xmin, xmax )			!Get display ranges of chi,
      kxmin = int(xmin) - 1
      kxmax = int(xmax) + 1

      write ( text,'(''  Mag range   Number    Mean Chi    Std Dev'')')
      call printo ( text )
      do k = kxmin, kxmax-1

         call chi_cmmean ( data, k, %val(ipr), %val(ipw), %val(ipb),
     +                     num )
         knum(k) = num
         call meanstdr ( %val(ipr), %val(ipw), %val(ipb), num,
     +                   .true., 3.0, .false., 0.0, av, sd )
         rav(k) = av
         rsd(k) = sd

         ka = k + 1
         tav = trunc(av,3)
         tsd = trunc(sd,3)
         write ( text, '(3x,i2,1x,''-'',1x,i2,4x,i6,6x,f6.2,4x,f7.3)')
     +                  k, ka, num, tav, tsd
         call printo ( text )

      enddo

      NUMCUR = 0
      do k = kxmin, kxmax-1
         if ( knum(k).ne.0 ) then
            NUMCUR = NUMCUR + 1
            am = real(k) + 0.5
            XCUR(NUMCUR) = am
            ya = 0.0
            if ( REDUCE ) then
               f = SLOPE*(10.0**((ZMAG-am)/2.5)) + ZERO
               ya = 0.0 - f
               ya = ya/sqrt(f/ZERO)
            endif
            YCUR(NUMCUR) = ya + rav(k) + CHILIM*rsd(k)
         endif
      enddo

      if ( DOGRAPH ) call chi_autoplot ( data )

      call wrkcan ( 'AUTOCR' )
      call wrkcan ( 'AUTOCW' )
      call wrkcan ( 'AUTOCB' )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_CMMEAN -- Get mean Chi value at a mag
C
C    a j penny                        dao            1988-05-16

      subroutine chi_cmmean ( data, kmag, rr, wv, bv, num )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real     data(TBY,7)	!i: Loaded data
      integer  kmag		!i: Mag lower value
      real     rr(TBY)		!o: Chi values
      real     wv(TBY)		!o: Weight values (all = 1.0)
      logical  bv(TBY)		!o: Use values (all = .true.)
      integer  num		!o: Number of good points
C--
      integer k
      real amin, amax, xa, ya, f
Cbegin


      if ( ST_FAILED ) return

      amin = kmag
      amax = amin + 1.0
      num = 0
      do k = 1, TBY

         if ( data(k,7).lt.0.5 .and. data(k,1).ge.amin .and.
     +           data(k,1).le.amax ) then

            ya = 0.0
            if ( REDUCE ) then
               xa = data(k,1)
               f = SLOPE*(10.0**((ZMAG-xa)/2.5)) + ZERO
               ya = 0.0 - f
               ya = ya/sqrt(f/ZERO)
            endif

            num = num + 1
            rr(num) = data(k,2) - ya
            wv(num) = 1.0
            bv(num) = .true.
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_PLOT -- Plot out Chi values
C
C    a j penny                        dao            1988-05-16

      subroutine chi_plot ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      integer k
      real xmin, xmax, ymin, ymax, av, sd
      real trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      call chi_alimr ( data, 1, TBY, xmin, xmax )					!Get display ranges of chi,
      call chi_alimr ( data, 2, TBY, ymin, ymax )					!  mags and if to number points

      xmin = aint(xmin-0.05*(xmax-xmin))
      xmax = 1.0 + aint(xmax+0.05*(xmax-xmin))
      call get2r ( 'DEVLIMX', xmin, xmax, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return

      call pargr ( ymin )
      call pargr ( ymax )
      call printd ( 'Low and high Chi values = %f : %f ' )
      call chi_aavgr ( data, 2, TBY, av, sd )
      ymax = av + 3.0*sd
      ymin = ymin - 0.05*(ymax-ymin)
      ymin = trunc_e(ymin,2)
      ymax = trunc_e(ymax,2)
      call get2r ( 'DEVLIMY', ymin, ymax, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return

      if ( REDUCE ) then
         call gd_dobox ( xmin, xmax, 'Magnitude', ymin, ymax, 		!Open axes
     +          'Chi-sq value (above subtracted levels)', TITLE, 0 )
      else
         call gd_dobox ( xmin, xmax, 'Magnitude', ymin, ymax, 		!Open axes
     +                   'Chi-sq value ', TITLE, 0 )
      endif

      do k = 1, TBY							!Plot points
         if ( data(k,7).lt.0.5 ) then
            call pgpoint ( 1, data(k,1), data(k,2), 2 )
         endif
      enddo

      PLMIN = xmin							!Save Plot left hand


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_AUTOPLOT -- Plot out automatic Chi values
C
C    a j penny              ral                1994 Dec

      subroutine chi_autoplot ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      integer k
      real xmin, xmax, ymin, ymax, f, xa, ya
Cbegin


      if ( ST_FAILED ) return

      call pgpage

      call chi_alimr ( data, 1, TBY, xmin, xmax )			!Get display X range of chi
      xmin = aint(xmin-0.05*(xmax-xmin))
      xmax = 1.0 + aint(xmax+0.05*(xmax-xmin))

      call chi_alimr ( data, 2, TBY, ymin, ymax )			!Get dsiplay Y range
      do k = 1, NUMCUR
         xa = XCUR(k)
         if ( REDUCE ) then
            f = SLOPE*(10.0**((ZMAG-xa)/2.5)) + ZERO
            ya = YCUR(k) - f
            ya = ya/sqrt(f/ZERO)
         else
            ya = YCUR(k)
         endif
         if ( k.eq.1 ) then
            ymax = ya
         else
            ymax = max(ymax,ya)
         endif
      enddo
      ymax = max(0.0001,1.4*ymax)
      ymin = max(0.0001,0.8*ymin)
      ymin = alog10(ymin)
      ymax = alog10(ymax)

      if ( REDUCE ) then
         call gd_dobox ( xmin, xmax, 'Magnitude', ymin, ymax, 		!Open axes
     +   'Log10 Chi-sq value (above subtracted levels)', TITLE, 0 )
      else
         call gd_dobox ( xmin, xmax, 'Magnitude', ymin, ymax, 		!Open axes
     +                   'Log10 Chi-sq value ', TITLE, 0 )
      endif

      do k = 1, TBY							!Plot points
         if ( data(k,7).lt.0.5 ) then
            ya = alog10(data(k,2))
            call pgpoint ( 1, data(k,1), ya, 2 )
         endif
      enddo

      call pgbbuf
      do k = 1, NUMCUR							!Plot line
         xa = XCUR(k)
         if ( REDUCE ) then
            f = SLOPE*(10.0**((ZMAG-xa)/2.5)) + ZERO
            ya = YCUR(k) - f
            ya = ya/sqrt(f/ZERO)
         else
            ya = YCUR(k)
         endif
         ya = alog10(ya)
         if ( k.eq.1 ) then
            call pgmove ( xa, ya )
         else
            call pgdraw ( xa, ya )
         endif
      enddo
      call pgebuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_CUR -- Use cursor to get and mark Chi limits
C
C   a j penny                    dao                1988-05-16

      subroutine chi_cur ( )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
C--
      integer k
      logical loop
      real xa, ya, f, yb
      character*1 ch
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      if ( REDUCE ) then						!Type out information
         call printo ( '  Plot of reduced Chi vs Mag' )
         call pargr ( slope )
         call pargr ( zmag )
         call printd (
     +   '  Chi reduced by S.10**((Z-mag)/2.5):  S = %f ; Z = %f ' )
         call printo ( '  Cursor returns actual Chi ' )
      else
         call printo ( '  Plot of Chi vs Mag' )
         call printo ( '  Cursor returns Chi' )
      endif
      call printo ( ' ' )
      call printo ( '  Type space to get cursor posn.' )
      call printo ( '  Input cursor position by clicking mouse')
      call printo ('  Set cursor to left of box and click to exit.')
      call printo ( ' ' )

      NUMCUR = 0
      loop = .true.
      do while ( loop .and. NUMCUR.lt.1000 )
         call pgcurse ( xa, ya, ch )
         if ( xa.ge.PLMIN ) then
            if ( REDUCE ) then
               f = SLOPE*(10.0**((ZMAG-xa)/2.5)) + ZERO
               yb = f + ya*sqrt(f/ZERO)
            else
               yb = ya
            endif
            call pargr ( xa )
            call pargr ( yb )
            call printd ( ' Mag = %f  : Chi = %f ' )
            NUMCUR = NUMCUR + 1
            XCUR(NUMCUR) = xa
            YCUR(NUMCUR) = yb
         else
            loop = .false.
         endif
      enddo

      if ( NUMCUR.eq.0 ) return

      call sort2r ( XCUR, YCUR, NUMCUR )				!Sort cursor into ascending mag list

      call pgbbuf
      do k = 1, NUMCUR							!Plot line
         xa = XCUR(k)
         if ( REDUCE ) then
            f = SLOPE*(10.0**((ZMAG-xa)/2.5)) + ZERO
            ya = YCUR(k) - f
            ya = ya/sqrt(f/ZERO)
         else
            ya = YCUR(k)
         endif
         if ( k.eq.1 ) then
            call pgmove ( xa, ya )
         else
            call pgdraw ( xa, ya )
         endif
      enddo
      call pgebuf

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_OUTLINE -- Turn cursor posns to 100 output Chi limits and stores
C
C  ajpenny               rgo                          82-12-31

      subroutine chi_outline ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      integer j, k
      real xn(100), yn(100), xhere, xlo, xhi, ylo, yhi, f, xmax, xmaxa
      logical more
Cbegin


      if ( ST_FAILED ) return

      if ( NUMCUR.eq.0 ) then
         do k = 1, 100
            ODATA(1,k) = real(k)
            ODATA(2,k) = 1.0e7
         enddo
         return
      endif

      xmax = XCUR(NUMCUR)						!Make into a series of 100 pairs at 0.2 mag spacing
      xmaxa = real(int(xmax*5.0))/5.0 + 0.2
      xn(1) = xmaxa
      yn(1) = YCUR(NUMCUR)
      do k = 2, 100
         xn(k) = xmaxa - 0.2*(real(k-1))
         xhere = xn(k)

         more = .true.							!Find mag just fainter
         j = NUMCUR							!and brighter than here
         do while ( more )
            j = j - 1
            if ( j.lt.1 ) then
               more = .false.
               yn(k) = YCUR(1)
            else
               if ( xhere.gt.XCUR(j) ) then
                  more = .false.
                  ylo = YCUR(j+1)
                  yhi = YCUR(j)
                  xlo = XCUR(j+1)
                  xhi = XCUR(j)
                  yn(k) = YCUR(j+1) + ((xhere-xlo)/(xhi-xlo))*(yhi-ylo)
               endif
            endif
         enddo
      enddo

      do k = 1, 100							!Make result allowing for any reduced values
         ODATA(1,k) = xn(k)
         if ( REDUCE ) then
            f = SLOPE*(10.0**((ZMAG-xn(k))/2.5)) + ZERO
            ODATA(2,k) = f + yn(k)*sqrt(f/ZERO)
         else
            ODATA(2,k) = yn(k)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_CHINUM -- How many out by chi
C
C  alan penny          RAL                1991 May

      subroutine chi_chinum ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      integer kout, kgood, k, km, nkm
      real  chi, ama, akm
Cbegin


      if ( ST_FAILED ) return

      kout = 0
      kgood = 0
      do k = 1, TBY
         if ( data(k,7).lt.0.5 ) then
            chi = data(k,2)
            ama = data(k,1)
            akm = (ODATA(1,1)-ama)/0.2 + 1.0
            km = akm
            akm = akm - real(km)
            km = min(100,max(1,km))
            nkm = min(100,max(1,(1+km)))
            chilim = ODATA(2,km) + akm*(ODATA(2,nkm)-ODATA(2,km))
            if ( chi.gt.chilim ) then
               data(k,7) = 5.0
               kout = kout + 1
            else
               kgood = kgood + 1
            endif
         endif
      enddo
      call printo ( ' ' )
      call pargi ( kout )
      call printd ( 'No of stars with Chi-Sq too high = %d ' )
      call pargi ( kgood )
      call printd ( 'No of good fitted stars left = %d ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_STORE -- Store results
C
C  alan penny          RAL                1991 May

      subroutine chi_store ( data )

      implicit none
      include 'chi.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real   data(TBY,7)	!:o Loaded data
C--
      integer ipo, istat
Cbegin


      if ( ST_FAILED ) return

      call optabw ( 'OUT', ipo, 7, 100, .false., istat )		!Open output table
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      if ( TITLE.eq.' '  ) TITLE = 'Output from Chi' 			!get output table title
      call get1c ( 'TITLE', TITLE, TITLE, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', TITLE )

      call ident ( %val(ipo), 7, 100 )					!Load names

      call coprr ( ODATA, 2, 100, 1, 2, 1, 100, %val(ipo), 7, 100, 6,1)	!load data


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_ALIMR -- Finds the min and max of a real vector with use flag
C
C  alan penny          RAL                1991 May

      subroutine chi_alimr ( data, kn, n, rmin, rmax )

      implicit none
      include 'STARMAN_INC'

      integer   n           !i: no of elements in vector
      real      data(n,7)   !i: input vector
      integer   kn          !i: Vector element
      real      rmin        !o: minimum
      real      rmax        !o: maximum
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      rmin = 1.0e20
      rmax = -1.0e20
      do j = 1, n
         if ( data(j,7).lt.0.5 ) then
            rmin = min(rmin,data(j,kn))
            rmax = max(rmax,data(j,kn))
         endif
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHI_AAVGR -- Mean and std dev of a column in the got data
C
C  alan penny          RAL                1991 May

      subroutine chi_aavgr ( data, kn, n, av, sd )

      implicit none
      include 'STARMAN_INC'

      integer   n           !i: no of elements in vector
      real      data(n,7)   !i: input vector
      integer   kn          !i: Vector element
      real      av          !o: Mean
      real      sd          !o: Standard deviation
C--
      integer j
      double precision ds, dss, dn
Cbegin


      if ( ST_FAILED ) return

      av = 0.0
      sd = 0.0
      if ( n.lt.1 ) return

      ds = 0.0d0
      dss = 0.0d0
      dn = 0.0d0
      do j = 1, n
         if ( data(j,7).lt.0.5 ) then
            ds = ds + data(j,kn)
            dss = dss + data(j,kn)*data(j,kn)
            dn = dn + 1.0d0
         endif
      enddo

      if ( dn.gt.0.0d0 ) av = ds/dn
      if ( dn.gt.1.0d0 ) then
         dss = ( dss-ds*ds/dn)/(dn-1.0d0)
         if ( dss.gt.1.0d-20 ) sd = sqrt(dss)
      endif


      end

