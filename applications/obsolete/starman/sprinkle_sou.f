CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SPRINKLE -- Make table of star posns, mags and heights, for field/cluster
C  For detailed description see sprinkle.hlp
C
C    a j penny                     stsci                   1987-06-02

      subroutine sprinkle ( ierradam )

      implicit none

      integer       ierradam              !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_sprinkle

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   SPRINKLE.FOR
C
C   Contains:-
C
C T_SPRINKLE   Make table of star posns, mags and heights, for field/cluster
C SP_CLGET     Get the cluster details
C SP_FIGET     Get the field details
C SP_CLPUT     Calc and store the cluster star positions
C SP_CLCMD     Cluster cmd to working arrays; calcs number distribution,error
C SP_LIRECT    Check if a line segment is in/partly in a rectangle
C SP_FIPUT     Calc and store the field star positions
C SP_FIRAN     Find mag, col section in field file where Nth star is
C SP_FITOT     Calc no of stars in field file; check if any in range
C SP_SETUPOUT  Set up output file with headers, names
C SP_CLIM      Get the mag and colour limits of a cluster HR table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_SPRINKLE -- Make table of star posns, mags and heights, for field/cluster
C
C  For detailed description see sprinkle.hlp
C
C    a j penny                     stsci                   1987-06-02

      subroutine t_sprinkle()

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'
C--
      integer i
Cbegin


      call get1b ( 'LOG', DOLOG, .false.)			!Output, to terminal, stars as they are sprinkled?
      if ( ST_FAILED ) return

      call sp_clget 						!Get cluster details

      call sp_figet 						!Get field details

      TBYO = NCLUST + NFIELD					!Total stars to output

      if ( TBYO.gt.0 ) then

         call printo ( ' ' )
         call get1r ( 'M1FACT', HM1FACTOR, 100.0, 1.0e-8,	!Get conversion factors
     +                          1.0e8 )				! from mags to heights
         call get1r ( 'M2FACT', HM2FACTOR, HM1FACTOR, 1.0e-8,
     +                          1.0e8 )
         if ( ST_FAILED ) return

         call printo ( ' ' )
         call optabw ( 'OUT', IPO, TBVXO, TBYO, .false., i)	!Open output file
         if ( ST_FAILED ) return

         call sp_setupout ( %val(IPO) )

         if ( DOCLUST ) call sp_clput ( %val(IPO) )		!Calc,store cluster stars

         if ( DOFIELD ) call sp_fiput ( %val(IPO) )		!Calc,store field stars

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_CLGET -- Get the cluster details
C
C    a j penny                     stsci                   1987-06-02

      subroutine sp_clget()

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'
C--
      logical desbad
      real cms, cme, ccs, cce, rv
      integer istat
Cbegin


      if ( ST_FAILED ) return

      DOCLUST = .false.							!Default none
      NCLUST = 0

      call printo ( ' ' )
      call get1b ( 'DOCLUST', DOCLUST, .true. )				!Make cluster stars?
      if ( ST_FAILED ) return
      if ( .not.DOCLUST ) return

      call get1b ( 'CEVEN', CUNIFORM, .true. )				!Get type of mag, colour spread
      if ( ST_FAILED ) return

      if ( .not.CUNIFORM ) then						!Get cluster file
         call optabr ( 'CLUSTER', IPCL, TBVXCL, TBYCL, .false., istat )
         if ( ST_FAILED ) return
            if ( TBYCL.lt.1 .or. TBVXCL.lt.10 .or. TBYCL.gt.NCSECT )then
            DOCLUST = .false.
            call printo ( 'ERROR: Cluster file incorrect - ' )
            if ( TBYCL.gt.NCSECT ) call printo ( '  Too many sections')
            if ( TBVXCL.lt.10 ) call printo ( '  Too few params' )
            return
         endif
         desbad = .false.
         call gtdesr ( 'CLUSTER', 'RED', CTRED, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'CLUSTER', 'DISMOD', CTDISMOD, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         if ( desbad ) then
            call printo ( 'ERROR: Cluster CMD file has bad descriptors')
            DOCLUST = .false.
            return
         endif

         call get1r ( 'CDISMOD', CDISMOD, CTDISMOD, -1.0e8, 1.0e8 )	!Get the cluster distance and reddening
         call get1r ( 'CREDDEN', CRED, CTRED, -1.0e8, 1.0e8 )
         if ( ST_FAILED ) return

      endif

      if ( .not.CUNIFORM ) then						!Get the cluster CMD limits
         call sp_clim ( %val(IPCL), cms, cme, ccs, cce )
         CMAGS = cms + CDISMOD - CTDISMOD
         CMAGE = cme + CDISMOD - CTDISMOD
         CCOLS = ccs + CRED - CTRED
         CCOLE = cce + CRED - CTRED
      else
         CMAGS = 10.0
         CMAGE = 20.0
         CCOLS = 0.0
         CCOLE = 2.0
      endif
      call get2r ( 'CMRANGE', CMAGS, CMAGE, .true., -50.0, 50.0 )
      if ( ST_FAILED ) return
      if ( CMAGS.gt.CMAGE ) then
         rv = CMAGS
         CMAGS = CMAGE
         CMAGE = rv
      endif
      call get2r ( 'CCRANGE', CCOLS, CCOLE, .true., -20.0 ,20.0 )
      if ( ST_FAILED ) return
      call cswopr ( CCOLS, CCOLE )

      CXPOS = 50.0						 	!Get the cluster posn
      CYPOS = 50.0
      call get2r ( 'CXYPOSN', CXPOS, CYPOS, .true., -1.0e8, 1.0e8 )

      call get1r ( 'RADCORE', RADC, 10.0, 1.0e-8, 1.0e8 )		!Get the cluster radii
      call get1r ( 'RADTIDE', RADT, 100.0, 1.0e-8, 1.0e8 )

      CXS = 1.0						 		!Get the cluster boundaries
      CXE = 100.0
      call get2r ( 'CXRANGE', CXS, CXE, .true., -1.0e8, 1.0e8 )
      call cswopr ( CXS, CXE )
      CYS = 1.0
      CYE = 100.0
      call get2r ( 'CYRANGE', CYS, CYE, .true., -1.0e8, 1.0e8 )
      call cswopr ( CYS, CYE )

      call get1i ( 'CNUMBER', NCLUST, 100, 0, 100000000 )		!Get number of stars

      call get1i ( 'CSEED', NRANC, 1234567891, 1200000001, 1400000001)	!Get cluster random seed


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_FIGET -- Get the field details
C
C    a j penny                     stsci                   1987-06-02

      subroutine sp_figet ( )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'
C--
      logical desbad
      integer istat
      real    rv
Cbegin


      if ( ST_FAILED ) return

      DOFIELD = .false.							!Default none
      NFIELD = 0

      call printo ( ' ' )
      call get1b ( 'DOFIELD', DOFIELD, .true. )				!Make field stars?
      if ( ST_FAILED ) return
      if ( .not.DOFIELD ) return

      call get1b ( 'FEVEN', FUNIFORM, .true. )				!Get type of mag, colour spread
      if ( ST_FAILED ) return

      if ( .not.FUNIFORM ) then						!Get field file
         call optabr ( 'FIELD', IPFI, TBVXFI, TBYFI, .false., istat )
         if ( ST_FAILED ) return
         if ( istat.ne.0 ) then
            DOFIELD = .false.
            return
         endif
         if ( TBYFI*(TBVXFI-5).gt.NFSECT ) then
            call printo (
     +      'ERROR: Field file incorrect - Too many sections' )
            DOFIELD = .false.
            return
         endif
         desbad = .false.
         call gtdesr ( 'FIELD', 'RED', FTRED, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'FIELD', 'DISMOD', FDISMOD, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'FIELD', 'MSTART', FMAGSTART, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'FIELD', 'MSTEP', FMAGSTEP, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'FIELD', 'CSTART', FCOLSTART, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         call gtdesr ( 'FIELD', 'CSTEP', FCOLSTEP, 0.0, istat )
         if ( istat.ne.0 ) desbad = .true.
         if ( desbad ) then
            call printo ( 'ERROR: Field CMD file has bad descriptors' )
            DOFIELD = .false.
            return
         endif
         FMAGSTEP = abs(FMAGSTEP)
         FCOLSTEP = abs(FCOLSTEP)

         call get1r ( 'FDISMOD', FDISMOD, FTDISMOD, -1.0e8, 1.0e8 )	!Get the field distance and reddening
         call get1r ( 'FREDDEN', FRED,    FTRED,    -1.0e8, 1.0e8 )
         if ( ST_FAILED ) return

      endif

      if ( .not.FUNIFORM ) then						!Get the field CMD limits
         FMAGS = FMAGSTART + FDISMOD - FTDISMOD
         FMAGE = FMAGSTART + FMAGSTEP*(TBYFI-1) + FDISMOD - FTDISMOD
         FCOLS = FCOLSTART + FRED - FTRED
         FCOLE = FCOLSTART + FCOLSTEP*(TBVXFI-5) + FRED - FTRED
      else
         FMAGS = 10.0
         FMAGE = 20.0
         FCOLS = 0.0
         FCOLE = 2.0
      endif
      call get2r ( 'FMRANGE', FMAGS, FMAGE, .true., -50.0, 50.0 )
      if ( FMAGS.gt.FMAGE ) then
         rv = FMAGS
         FMAGS = FMAGE
         FMAGE = rv
      endif
      call get2r ( 'FCRANGE', FCOLS, FCOLE, .true., -20.0, 20.0 )
      call cswopr ( FCOLS, FCOLE )

      FXS = 1.0						 		!Get the field boundaries
      FXE = 100.0
      call get2r ( 'FXRANGE', FXS, FXE, .true., -1.0e8, 1.0e8 )
      call cswopr ( FXS, FXE )
      FYS = 1.0
      FYE = 100.0
      call get2r ( 'FYRANGE', FYS, FYE, .true., -1.0e8, 1.0e8 )
      call cswopr ( FYS, FYE )

      call get1i ( 'FNUMBER', NFIELD, 100, 0, 100000000 )		!Get number of stars

      call get1i ( 'FSEED', NRANF, 1234567891, 1200000001, 1400000001 ) !Get field random seed


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_CLPUT -- Calc and store the cluster star positions
C   The cluster profile is that defined by King (AJ 67,471;70,376;71,64.)
C
C    a j penny                     stsci                   1987-06-02

      subroutine sp_clput ( tbo )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real 	tbo(TBVXO,TBYO)		!o: Output file
C--
      logical error
      integer k, isum
      real dx, dy, fract, abeg, a, b, c, fsum,
     +     ar, ap, bp, am, col, x, y, theta, r, dmag, h1, h2, rv
      real p(NKING), amstart(NCSECT), amend(NCSECT),
     +     colstart(NCSECT), colend(NCSECT), rsum(NCSECT)
      character text*72
Cbegin


      if ( ST_FAILED ) return

      if ( .not.CUNIFORM ) call sp_clcmd ( %val(IPCL), amstart, amend, 	!Load File CMD values.
     +                                 colstart, colend, rsum, error )
      if ( error ) return						!Return if none in permitted range

      a = RADT*RADT/(RADC*RADC*RNKING*RNKING)				!Load King radial distribution
      b = -1.0*sqrt(1.0/(1+(RADT/RADC)**2.0))				! (turned into annulus counts) into rr
      fsum = 0.0
      do k = 1, NKING
         c = real(k)*real(k)*a
         fsum = fsum + real(k)*(1.0/sqrt(1+c) + b)**2.0
         p(k) = fsum
      enddo
      do k = 1, NKING
         p(k) = p(k)/fsum
      enddo

C  Calculate posns, mags, cols of stars. For each one check if
C  allowed posn, mag, colour ranges and if so store in output. Carry
C  on till enough stored or until too many calculated as falling
C  outside range and thus there is probably something wrong.

      isum = 0
      do while ( isum.lt.NCLUST )

         call rano ( ar, NRANC )					!Calc star radial posn
         k = 1
         do while ( k.lt.NKING .and. ar.gt.p(k) )
            k = k + 1
         enddo
         ap = 0.0
         if ( k.ne.1 ) ap = p(k-1)
         bp = p(k)
         r = real(k)
         if ( abs(bp-ap).gt.1.0e-7 ) r = r + (ar-ap)/(bp-ap)
         r = RADT*r/RNKING

         call rano ( rv, NRANC )					!Star angular posn
         theta = rv*2.0*3.14159

         dx = r*cos(theta)						!Star X,Y posn
         dy = r*sin(theta)
         x = CXPOS + dx
         y = CYPOS + dy

         if ( x.ge.CXS .and. x.le.CXE .and. 				!See if position in allowed range.
     +        y.ge.CYS .and. y.le.CYE ) then				! If so, calc mag, col

            if ( CUNIFORM ) then
               call rano ( rv, NRANC )
               am = CMAGS + (CMAGE-CMAGS)*rv
               call rano ( rv, NRANC )
               col = CCOLS + (CCOLE-CCOLS)*rv
            else
               call rano ( dmag, NRANC )
               k = 1
               do while ( k.ne.TBYCL .and. dmag.ge.rsum(k) )
                  k = k + 1
               enddo
               abeg = 0.0
               if ( k.ne.1 ) abeg = rsum(k-1)
               fract = (dmag-abeg)/(rsum(k)-abeg)
               am = amstart(k) + fract*(amend(k)-amstart(k))
               col = colstart(k) + fract*(colend(k)-colstart(k))
               am = am + CDISMOD - CTDISMOD
               col = col + CRED - CTRED

            endif

            if ( am.ge.CMAGS .and. am.le.CMAGE .and. col.ge.CCOLS .and.	!See if calculated mag, col in allowed range
     +           col.le.CCOLE ) then					! store if ok
               isum = isum + 1
               h1 = hm1factor*10.0**((20.0-am)/2.5)
               h2 = hm2factor*10.0**((20.0-(am+col))/2.5)
               tbo(6,isum) = x
               tbo(7,isum) = y
               tbo(8,isum) = h1
               tbo(9,isum) = h2
               tbo(10,isum) = am
               tbo(11,isum) = col
               tbo(12,isum) = am + col
               if ( DOLOG ) then
                  write ( text,
     +            '(1x,''C'',i6,2f10.2,2x,2f10.2,2x,2f10.2)' )
     +            isum, x, y, h1, h2, am, col
                  call printo ( text )
               endif

            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_CLCMD -- Cluster cmd to working arrays; calcs number distribution,error
C
C    a j penny                     stsci                   1987-10-15

      subroutine sp_clcmd ( tbcl, amstart, amend, colstart, colend,
     +                      rsum, error )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real 	tbcl(TBVXCL,TBYCL)	!i: Cluster file
      real 	amstart(NCSECT)		!o: Magnitude start of line segment
      real	amend(NCSECT)		!o: Magnitude end of line segment
      real	colstart(NCSECT)	!o: Colour start of line segment
      real	colend(NCSECT)		!o: Colour end of line segment
      real	rsum(NCSECT)		!o: Cumulative fraction of stars in
					!   segments up to and including this one
      logical	error			!o: Error in this s/r flag
C--
      integer num(NCSECT)
      integer k, nums
      real tot, amin, amout, colin, colout
      logical ok
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBYCL							!Read in data
         amstart(k) = tbcl(6,k)
         amend(k) = tbcl(8,k)
         colstart(k) = tbcl(7,k)
         colend(k) = tbcl(9,k)
         num(k) = max(0,nint(tbcl(10,k)))
      enddo

      nums = 0								!Calc HR diagram fractional number
      do k = 1, TBYCL							! distribution. Load into RSUM
         nums = nums + num(k)
         rsum(k) = nums
      enddo
      tot = rsum(TBYCL)
      call adivkr ( rsum, tot, rsum, TBYCL )

      error = .true.							!Check some stars in permitted range
      k = 0
      do while ( k.lt.TBYCL .and. error )
         k = k + 1
         amin = amstart(k) + CDISMOD - CTDISMOD
         amout = amend(k) + CDISMOD - CTDISMOD
         colin = colstart(k) + CRED - CTRED
         colout = colend(k) + CRED - CTRED
         if ( num(k).ne.0 ) call sp_lirect ( colin, amin, colout,
     +                          amout, CCOLS, CMAGS, CCOLE, CMAGE, ok )
         if ( ok ) error = .false.
      enddo
      if ( error ) then
         call printo ( 'ERROR: Cluster CMD not in permitted mag ' )
         call printo ( '       and/or colour range' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_LIRECT -- Check if a line segment is in/partly in a rectangle
C
C        a j penny               stsci            1987-12-23

      subroutine sp_lirect ( xa, ya, xb, yb, xc, yc, xd, yd, ok )

      implicit none
      include 'STARMAN_INC'

      real xa		!i: line segment x start
      real ya		!i: line segment y start
      real xb		!i: line segment x end
      real yb		!i: line segment y end
      real xc		!i: box x start
      real yc		!i: box y start
      real xd		!i: box x end
      real yd		!i: box y end
      logical ok	!o: =true if they line in or partly in: =false if not
C--
      real a, b, xt, yt
Cbegin


      if ( ST_FAILED ) return

      ok = .false.

      if(xa.ge.xc .and. xa.le.xd .and. ya.ge.yc .and. ya.le.yd)ok=.true.!End of line in box?
      if(xb.ge.xc .and. xb.le.xd .and. yb.ge.yc .and. yb.le.yd)ok=.true.
      if ( ok ) return

      if ( xa.eq.xb .and. ya.eq.yb ) then				!Line crosses edge of box?
         return
      else if ( xa.eq.xb ) then
         if ( xa.lt.xc .or. xa.gt.xd ) then
            return
         else
            if ( min(ya,yb).lt.min(yc,yd) .and.
     +           max(ya,yb).gt.max(yc,yd)       ) then
                 ok = .true.
                 return
            endif
         endif
      else if ( ya.eq.yb ) then
         if ( ya.lt.yc .or. ya.gt.yd ) then
            return
         else
            if ( min(xa,xb).lt.min(xc,xd) .and.
     +           max(xa,xb).gt.max(xc,xd)       ) then
                 ok = .true.
                 return
            endif
         endif
      else
          b = (yb-ya)/(xb-xa)
          a = ya - b*xa
          yt = a + b*xc
          if ( yt.ge.ya .and. yt.le.yb ) ok = .true.
          yt = a + b*xd
          if ( yt.ge.ya .and. yt.le.yb ) ok = .true.
          xt = a + b*yc
          if ( xt.ge.xa .and. xt.le.xb ) ok = .true.
          xt = a + b*yd
          if ( xt.ge.xa .and. xt.le.xb ) ok = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_FIPUT -- Calc and store the field star positions
C
C    a j penny                     stsci                   1987-06-02

      subroutine sp_fiput ( tbo )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real 	tbo(TBVXO,TBYO)		!o: Output file
C--
      integer numf, isum, km, num, kcol
      real x, y, am, col, h1, h2, rv
      logical error
      character text*72
Cbegin


      if ( ST_FAILED ) return

      if ( .not.FUNIFORM ) call sp_fitot ( %val(ipfi), numf, error )	!Get field star file total and check
      if ( error ) return

      isum = NCLUST							!Calculate the field stars and store to output
      do while ( isum.lt.TBYO )

         call rano ( rv, NRANF )					!Get star posn
         x = FXS + (FXE-FXS)*rv
         call rano ( rv, NRANF )
         y = FYS + (FYE-FYS)*rv

         if ( FUNIFORM ) then						!Get star mag, col
            call rano ( rv, NRANF )
            am = FMAGS + (FMAGE-FMAGS)*rv
            call rano ( rv, NRANF )
            col = FCOLS + (FCOLE-FCOLS)*rv
         else
            call rano ( rv, NRANF )
            num = numf*rv
            call sp_firan ( %val(ipfi), num, km, kcol )
            call rano ( rv, NRANF )
            col = FCOLSTART + real(kcol-1)*FCOLSTEP + rv*FCOLSTEP
            call rano ( rv, NRANF )
            am = FMAGSTART + (km-1)*FMAGSTEP + rv*FMAGSTEP

            am = am + FDISMOD - FTDISMOD
            col = col + FRED - FTRED

         endif

         if ( am.ge.FMAGS .and. am.le.FMAGE .and. col.ge.FCOLS .and.	!Select on mag, col range to store result
     +        col.le.FCOLE ) then
            isum = isum + 1
            h1 = hm1factor*10.0**((20.0-am)/2.5)
            h2 = hm2factor*10.0**((20.0-(am+col))/2.5)
            tbo(6,isum) = x
            tbo(7,isum) = y
            tbo(8,isum) = h1
            tbo(9,isum) = h2
            tbo(10,isum) = am
            tbo(11,isum) = col
            tbo(12,isum) = am + col
            if ( DOLOG ) then
               write ( text,'(1x,''F'',i6,2f10.2,2x,2f10.2,2x,2f10.2)')
     +         isum, x, y, h1, h2, am, col
               call printo ( text )
            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_FIRAN -- Find mag, col section in field file where Nth star is
C
C    a j penny                     stsci                   1987-10-15

      subroutine sp_firan ( tbfi, num, km, kcol )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real	tbfi(TBVXFI,TBYFI)	!i: Field star file
      integer	num			!i: Nth star in file
      integer	km			!o: Mag section number
      integer	kcol			!o: Col section number
C--
      integer nsum, kk
Cbegin


      if ( ST_FAILED ) return

      kk = 0
      nsum = 0
      do while ( nsum.lt.num )
         kk = kk + 1
         km = 1 + (kk-1)/(TBVXFI-5)
         kcol = kk - (km-1)*(TBVXFI-5)
         nsum = nsum + nint(tbfi(kcol+5,km))
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_FITOT -- Calc no of stars in field file; check if any in range
C
C    a j penny                     stsci                   1987-10-15

      subroutine sp_fitot ( tbfi, numf, error )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real	tbfi(TBVXFI,TBYFI)	!i: Field star file
      integer	numf			!o: Total no of stars in field file
      logical	error			!o: Error in this s/r flag
C--
      real ams, ame, cols, cole
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      numf = 0								!Sum no of stars
      do k = 1, TBYFI
         do j = 6, TBVXFI
            numf = numf + nint(tbfi(j,k))
         enddo
      enddo

      error  = .true.							!Check there are stars in
      do k = 1, TBYFI							! field file in permitted col, mag range
         do j = 6, TBVXFI
            if ( nint(tbfi(j,k)).ne.0 ) then
               ams = FMAGSTART + (k-1)*FMAGSTEP
               ame = ams + FMAGSTEP
               ams = ams + FDISMOD - FTDISMOD
               ame = ame + FDISMOD - FTDISMOD
               if ( ams.lt.FMAGE .and. ame.gt.FMAGS ) then
                  cols = FCOLSTART + real(j-6)*FCOLSTEP
                  cole = cols + FCOLSTEP
                  cols = cols + FRED - FTRED
                  cole = cole + FRED - FTRED
                  if ( cols.lt.FCOLE .or. cole.gt.FCOLS ) error =.false.
               endif
            endif
         enddo
      enddo

      if ( error ) then
         call printo ( 'ERROR: Field CMD file empty in permitted'  )
         call printo ( '       colour and/or mag range' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_SETUPOUT -- Set up output file with headers, names
C
C    a j penny                     stsci                   1987-06-02

      subroutine sp_setupout ( tbo )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real tbo(TBVXO,TBYO)		!o: Output file
C--
      integer k, istat
      character*20 header(TBXO)
      data header / 'X', 'Y', 'HEIGHT1', 'HEIGHT2', 'MAG1', 'COL',
     +              'MAG2' /
      character text*72
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'TITLE', text, 'Output from SPRINKLE', .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', text )
      do k = 1, TBXO
         call pthead ( 'OUT', k, header(k), istat )
      enddo

      call azeror ( tbo, TBVXO*TBYO )					!Clear output file values
      call ident ( tbo, TBVXO, TBYO )					!Load #1..#n star names


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SP_CLIM -- Get the mag and colour limits of a cluster HR table
C
C    a j penny                   stsci               1987-08-11

      subroutine sp_clim ( tbcl, cms, cme, ccs, cce )

      implicit none
      include 'sprinkle.inc'
      include 'STARMAN_INC'

      real 	tbcl(TBVXCL,TBYCL)	!i: Table
      real	cms			!o: Mag start
      real	cme			!o: Mag end
      real	ccs			!o: colour start
      real	cce			!o: colour end
C--
      integer i
Cbegin


      if ( ST_FAILED ) return

      cms = tbcl(6,1)
      cme = cms
      ccs = tbcl(7,1)
      cce = ccs
      do i = 1, TBYCL
         if ( nint(tbcl(10,i)).ne.0 ) then
            cms = min(cms,tbcl(6,i),tbcl(8,i))
            cme = max(cme,tbcl(6,i),tbcl(8,i))
            ccs = min(ccs,tbcl(7,i),tbcl(9,i))
            cce = max(cce,tbcl(7,i),tbcl(9,i))
         endif
      enddo


      end


