CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PROFILE -- Determines profiles of stars
C
C           A J Penny          RGO  RAL  STScI  DAO        1971-1988
C

      subroutine profile  ( ierradam )

      implicit none

      integer    ierradam                  !o: ADAM error flag
C--

      call starman_start

      call t_profile

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   PROSUB.FOR
C
C     It contains:-
C
C PR_ROUGH     Fit Gauss to stars in list, estimates posns, hts, radii
C PR_AREJECT   Make a mean profile
C PR_LISTFIT   Type out the fits
C PR_CHPROFPAR Change the profile parameters
C PR_LISTCOMP  List the companions
C PR_ALLSUB    Remove stars of fit (and maybe its base) from an array
C PR_GETANGLE  Get whether angle to be fixed and if so, what at
C PR_GETFIXP   Main pars (rmaj,rmin,power,prmaj,prmin) to be fixed?
C PR_LOADBL    Load bad areas from com into real areas
C PR_UPDATEFIT Update fit parameters - make means
C PR_LOADCOM   Load output file with companions
C PR_SHIFTXY   Shift all stars and companions (not blanks) by an xy amount
C PR_ADDMAP    Take the resiuduals map and adds it onto the existing Map map
C PR_COPMAP    Take the profile map and copies it onto a new sized one
C PR_DOTOP(RS) Calc rough heights of the stars (real:int*2 image)
C PR_COMPZERO  Null comps and faint comps in main store


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_ROUGH -- Fit Gauss to stars in list, estimates posns, hts, radii
C
C   a.j.penny                   STSCI                     87-02-21

      subroutine pr_rough ( )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer ktot
      parameter ( ktot=100 )
      integer inuse(ktot), k, ka, iter, ninval, nin, klx, kly
      real rxuse(ktot), ryuse(ktot), xa, ya, rms, anx, any, amag, std,
     +     height, base, dx, dy, rx, ry, ah, adx, ady, arms, arx, ary,
     +     alx, aly, seeing, rkx, rky, pout(6)
      character*70 text
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )						!Type out fit ouput header
      write ( text, '( '' '', 4x, 9x, 5x, 5x, ''    RMS  '',
     +        ''Iter- '', '' No pixs '', ''   Radius'' )' )
      call printo ( text )
      write ( text, '( '' '', ''Star'', ''   Height'', ''  Dx '',
     +       ''  Dy '',''  Errors '', ''ations'', '' Invalid '',
     +       ''   X  '', ''   Y  '' )' )
      call printo ( text )

      do k = 1, TBY							!Loop through the star list
         if ( abs(RES(34,k)).le.0.5 ) then
            ka = min(k,ktot)						!Flag as ?
            inuse(ka) = 0
         else

            xa = RES(1,k)						!Fit Gauss at star posn
            ya = RES(2,k)						!First set up input posn, radii,
            rkx = 1.2*SR(1)						!then fit
            rky = 1.2*SR(2)
            if ( IMTYPE.eq.'SHORT' ) then
               call gauss2sa ( %val(IPIM), NX, NY, xa, ya, lx, ly, 0,
     +                         rkx, rky, INVAL, 20, amag, height, base,
     +                         dx, dy, anx, any, rx, ry, rms, iter,
     +                         ninval )
            else
               call gauss2ra ( %val(IPIM), NX, NY, xa, ya, lx, ly, 0,
     +                         rkx, rky, RINVAL, 20, amag, height, base,
     +                         dx, dy, anx, any, rx, ry, rms, iter,
     +                         ninval )
            endif

            RES(1,k) = anx						!Store; posn, height, fittedbut not displayed
            RES(2,k) = any
            RES(9,k) = 1.1*height*BS
            RES(36,k) = 3.0

            ka = min(k,ktot)						!Note radii
            rxuse(ka) = rx/1.2
            ryuse(ka) = ry/1.2
            inuse(ka) = 1

            ah   = trunc(1.1*height*BS,5)				!Type out some fit values
            adx  = trunc(dx,5)
            ady  = trunc(dy,5)
            arms = trunc(rms,3)
            arx  = trunc((rx/1.2),2)
            ary  = trunc((ry/1.2),2)
            nin  = ninval
            if (nin.gt.99) nin = 99
            write ( text, '(1h ,i4,f9.1,2f5.1,f8.1,i7,i8,1x,2f6.2)' )
     +              k, ah, adx, ady, arms, iter, nin, arx, ary
            call printo ( text )

         endif
      enddo

      call printo ( ' ' )

      if ( ka.eq.0 ) then						!Check some stars found OK. If not, return
         call printo ( 'No stars found' )
         return
      endif

      rx = 0.0								!Get average profile
      call pr_avstd ( rxuse, inuse, ka, rx, std )
      rx = max(0.2,min(99.0,rx))

      ry = 0.0
      call pr_avstd ( ryuse, inuse, ka, ry, std )

      SR(1) = max(0.2,min(99.0,rx))					!Put back to store
      SR(2) = max(0.2,min(99.0,ry))
      SR(3) = 2.2
      SR(4) = SR(1)*5.0
      SR(5) = SR(2)*5.0
      SR(6) = 0.0
      SR(7) = 0.0
      SR(8) = 10.0
      SR(9) = 2.0
      do k = 1, TBY
         call amovr ( SR, RES(12,k), 9 )
      enddo

      pout(1) = trunc(SR(1),3)
      pout(2) = trunc(SR(2),4)
      pout(3) = trunc(SR(3),3)
      pout(4) = trunc(SR(4),4)
      pout(5) = trunc(SR(5),4)
      pout(6) = trunc(SR(6),4)
      write ( text, '( '' Adopted Profile ='',f7.3,f8.3,f7.3,3f8.2)' )	!Type out fit and
     +        (pout(k),k=1,6)						! recommend box size
      call printo ( text )
      seeing = trunc((SR(1)+SR(2)),4)
      write ( text, '(''  Seeing (FWHM) = '',f6.2)' ) seeing
      call printo ( text )
      call boxeli ( SR(1), SR(2), SR(6), alx, aly )
      klx = 10.0*alx
      kly = 10.0*aly
      write ( text, '(''  Box Size = '',2i5,''  Suggested box size = '',
     +                2i5)' ) lx, ly, klx, kly
      call printo ( text )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_AREJECT -- Make a mean profile
C
C   a.j.penny                   rgo                    83-z-z

      subroutine pr_areject ( flag, kopt )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      logical flag
      integer   kopt            !i: Involve user, or not? (1:2)
C--
      integer k, j, num, krejn, kaccn
      real sro(6), av, std, wsn, angle, seeing, tsr(6), pout(6), rv
      logical change
      character*70 text
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call amovr ( SR, sro, 6 )						!Note input first six prof pars

      do k = 1, TBY							!Reject off stars
         if ( RES(9,k).lt.1.0e-9 ) RES(34,k) = 0.0
         if ( RES(12,k).lt.0.04 )  RES(34,k) = 0.0
         if ( RES(12,k).gt.100.0 ) RES(34,k) = 0.0
         if ( RES(13,k).lt.0.04 )  RES(34,k) = 0.0
         if ( RES(13,k).gt.100.0 ) RES(34,k) = 0.0
         if ( RES(14,k).lt.0.04 )  RES(34,k) = 0.0
         if ( RES(14,k).gt.10.0 )  RES(34,k) = 0.0
         if ( RES(15,k).lt.0.04 )  RES(34,k) = 0.0
         if ( RES(15,k).gt.400.0 ) RES(34,k) = 0.0
         if ( RES(16,k).lt.0.04 )  RES(34,k) = 0.0
         if ( RES(16,k).gt.400.0 ) RES(34,k) = 0.0
      enddo

      do k = 17, 12, -1							!Reject poor stars, but
         num = 0							! not if less than 5 left
         do j = 1, TBY
            if (RES(34,j).gt.0.5) num = num + 1
         enddo
         if ( num.gt.5 ) then
            do j = 1, TBY
               RUSE(j) = RES(k,j)
               NUSE(j) = RES(34,j) + 0.1
            enddo
            call pr_avstd ( RUSE, NUSE, TBY, av, std )
            do j = 1, TBY
               if ( RES(34,j).gt.0.5 ) then
                  if ( abs(RUSE(j)-av).gt.(2.0*std) ) then
                     RES(34,j) = 0.0
                  endif
               endif
            enddo
         endif
      enddo

      change = .true.							!Loop getting rejected stars
      do while ( change )

         call printo ( ' ' )						!Calc mean weighted fit
         call printo (
     +           '  Parameter averaging is done using these stars:-')
         call azeror ( SR, 6 )
         krejn = 0
         kaccn = 0
         do k = 1, TBY
            if ( RES(34,k).lt.0.5 ) krejn = 1
            if ( RES(34,k).gt.0.5 ) kaccn = 1
         enddo
         if ( krejn.eq.0 ) then
            call printo ( '  No rejected stars' )
         else
            call printo ( '  Rejected stars :-' )
            do k = 1, TBY
               if ( RES(34,k).lt.0.5 ) then
                  angle = trunc((RES(17,k)*HOPI),3)
                  pout(1) = trunc(RES(9,k),6)
                  pout(2) = trunc(RES(12,k),3)
                  pout(3) = trunc(RES(13,k),4)
                  pout(4) = trunc(RES(14,k),3)
                  pout(5) = trunc(RES(15,k),4)
                  pout(6) = trunc(RES(16,k),4)
                  write ( text, '(1x,i3,f9.1,3x,f7.3,f8.3,f7.3,3f8.2)' )
     +                    k, (pout(j),j=1,6), angle
                  call printo ( text )
               endif
            enddo
         endif
         if ( kaccn.eq.0 ) then
            call printo ( '  No accepted stars' )
         else
            call printo ( '  Accepted stars :-' )
            wsn = 0.0
            call azeror ( tsr, 6 )
            do k = 1, TBY
               if ( RES(34,k).gt.0.5 ) then
                  angle = trunc((RES(17,k)*HOPI),3)
                  pout(1) = trunc(RES(9,k),6)
                  pout(2) = trunc(RES(12,k),3)
                  pout(3) = trunc(RES(13,k),4)
                  pout(4) = trunc(RES(14,k),3)
                  pout(5) = trunc(RES(15,k),4)
                  pout(6) = trunc(RES(16,k),4)
                  write ( text, '(1x,i3,f9.1,3x,f7.3,f8.3,f7.3,3f8.2)' )
     +                    k, (pout(j),j=1,6), angle
                  call printo ( text )
                  rv = max(1.0e-8,RES(11+1,k))
                  tsr(1) = tsr(1) + (1.0/rv)*RES(9,k)
                  rv = max(1.0e-8,RES(11+2,k))
                  tsr(2) = tsr(2) + (1.0/rv)*RES(9,k)
                  tsr(3) = tsr(3) + RES(11+3,k)*RES(9,k)
                  rv = max(1.0e-8,RES(11+4,k))
                  tsr(4) = tsr(4) + (1.0/rv)*RES(9,k)
                  rv = max(1.0e-8,RES(11+5,k))
                  tsr(5) = tsr(5) + (1.0/rv)*RES(9,k)
                  tsr(6) = tsr(6) + RES(11+6,k)*RES(9,k)
                  wsn = wsn + RES(9,k)
               endif
            enddo
            if ( wsn.gt.0.000005 ) then
               SR(1) = 1.0/(tsr(1)/wsn)
               SR(2) = 1.0/(tsr(2)/wsn)
               SR(3) = tsr(3)/wsn
               SR(4) = 1.0/(tsr(4)/wsn)
               SR(5) = 1.0/(tsr(5)/wsn)
               SR(6) = tsr(6)/wsn
            endif
         endif

         angle = SR(6)*HOPI						!Type out results
         angle = trunc(angle,3)
         pout(1) = trunc(SR(1),3)
         pout(2) = trunc(SR(2),4)
         pout(3) = trunc(SR(3),3)
         pout(4) = trunc(SR(4),4)
         pout(5) = trunc(SR(5),4)
         call printo ( ' ' )
         write ( text, '(''  Weighted Mean'',f8.3,f8.3,f7.3,3f8.2)' )
     +          (pout(k),k=1,5), angle
         call printo ( text )
         seeing = trunc((SR(1)+SR(2)),3)
         write ( text, '(''  Seeing (FWHM) = '',F6.2)' ) seeing
         call printo ( text )
         call printo ( ' ' )

         if ( kopt.eq.1 ) then						!Type out rejected stars, get
            call pr_starrk ( 3, change )				! angle flips and reject changes
         else
            call pr_starrk ( 0, change )				! angle flips and reject changes
         endif

      enddo

      flag = .false.							!Flag if profile has changed
      do k = 1, 6
         if ( SR(k).ne.sro(k) ) flag = .true.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_LISTFIT -- Type out the fits
C
C   a.j.penny                   STScI                    87_05-21

      subroutine pr_listfit ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer krejn, kaccn, j, k, klx, kly
      real angle, seeing, alx, aly, pout(6)
      character*70 text
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ('  Parameter averaging is done using these stars:-')
      krejn = 0
      kaccn = 0
      do k = 1, TBY
         if ( RES(34,k).lt.0.5 ) krejn = 1
         if ( RES(34,k).gt.0.5 ) kaccn = 1
      enddo
      if ( krejn.eq.0 ) then
         call printo ( '  No rejected stars' )
      else
         call printo ( '  Rejected stars :-' )
         do k = 1, TBY
            if ( RES(34,k).lt.0.5 ) then
               angle = trunc((RES(17,k)*HOPI),3)
               pout(1) = trunc(RES(9,k),6)
               pout(2) = trunc(RES(12,k),3)
               pout(3) = trunc(RES(13,k),4)
               pout(4) = trunc(RES(14,k),3)
               pout(5) = trunc(RES(15,k),4)
               pout(6) = trunc(RES(16,k),4)
               write ( text, '(1x,i3,f9.1,3x,f7.3,f8.3,f7.3,3f8.2)' )
     +                 k, (pout(j),j=1,6), angle
               call printo ( text )
            endif
         enddo
      endif
      if ( kaccn.eq.0 ) then
         call printo ( '  No accepted stars' )
      else
         call printo ( '  Accepted stars :-' )
         do k = 1, TBY
            if ( RES(34,k).gt.0.5 ) then
               angle = trunc((RES(17,k)*HOPI),3)
               pout(1) = trunc(RES(9,k),6)
               pout(2) = trunc(RES(12,k),3)
               pout(3) = trunc(RES(13,k),4)
               pout(4) = trunc(RES(14,k),3)
               pout(5) = trunc(RES(15,k),4)
               pout(6) = trunc(RES(16,k),4)
               write ( text, '(1x,i3,f9.1,3x,f7.3,f8.3,f7.3,3f8.2)' )
     +                 k, (pout(j),j=1,6), angle
               call printo ( text )
            endif
         enddo
      endif

      call printo ( ' ' )
      angle = trunc((SR(6)*HOPI),3)
      seeing = trunc((SR(1)+SR(2)),3)
      write ( text, '( ''  Weighted Mean'',f8.3,f8.3,f7.3,3f8.2)' )
     +        (SR(j),j=1,5), angle
      call printo ( text )
      pout(1) = trunc(SR(7),4)
      pout(2) = trunc(SR(8),4)
      pout(3) = trunc(SR(9),4)
      if ( FIXANGLE ) then
         write (text, '( '' qh qr qp = '',3f8.3,''  Angle is fixed'')' )
     +           (pout(j),j=1,3)
      else
         write ( text, '(''  qh qr qp = '',3f8.3,
     +           ''  Angle is variable'')' ) (pout(j),j=1,3)
      endif
      call printo ( text )
      if ( DOMAP ) then
         call printo ( '  Profile Map is used' )
      else
         call printo ( '  Profile Map is not used' )
      endif
      call printo( '  (This mean is from the last calculation of the '//
     +            'mean. The mean of ' )
      call printo ( '  the above fits may be different)' )
      write ( text, '(''  Seeing (FWHM) = '',f6.2)' ) seeing
      call printo ( text )
      call boxeli ( SR(1), SR(2), SR(6), alx, aly )
      klx = 10.0*alx
      kly = 10.0*aly
      write ( text, '(''  Box Size = '',2i5,''  Suggested box size = '',
     +                2i5)' ) lx, ly, klx, kly
      call printo ( text )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHPROFPAR -- Change the profile parameters
C
C   a.j.penny                   stsCi                    86-11-20

      subroutine pr_chprofpar ( flag )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      logical flag
C--
      real rv, rva, angle, anglea
      integer k
      character*70 atext(9)
      data atext / 'Change parameter 1 - Rmaj',
     +             'Change parameter 2 - Rmin',
     +             'Change parameter 3 - Power',
     +             'Change parameter 4 - PRmaj',
     +             'Change parameter 5 - PRmin',
     +             'Change parameter 6 - Angle (degrees)',
     +             'Change parameter 7 - Wing height ratio',
     +             'Change parameter 8 - Wing radius',
     +             'Change parameter 9 - Wing power'/
Cbegin


      if ( ST_FAILED ) return

      flag = .false.
      do k = 1, 9
         call printo ( atext(k) )
         if ( k.eq.6 ) then
            angle = SR(k)*HOPI
            anglea = angle
            call get1r ( 'VALUE', angle, angle, -1.0e20, 1.0e20 )
            if ( ST_FAILED ) return
            if ( angle.ne.anglea ) flag = .true.
            SR(k) = angle/HOPI
         else
            rv = SR(k)
            rva = rv
            call get1r ( 'VALUE', rv, rv, -1.0e20, 1.0e20 )
            if ( ST_FAILED ) return
            if ( rv.ne.rva ) flag = .true.
            SR(k) = rv
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_LISTCOMP -- List the companions
C
C  a j penny              stsci               86-11-20

      subroutine pr_listcomp ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k, j, jn, kx, ky
      real angle, dx, dy, pout(9)
      character*79 text
      external trunc
      real trunc
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'NUMBER', k, 1, 1, TBY )
      if ( ST_FAILED ) return

      call printo ( '             Star                      Profile' )
      call printo ( '   Height     X       Y     Rmaj    Rmin    P'//
     +              '     PRmaj   PRmin   Angle' )
      pout(1) = trunc(RES(9,k),6)
      pout(2) = trunc(RES(1,k),5)
      pout(3) = trunc(RES(2,k),5)
      pout(4) = trunc(RES(12,k),3)
      pout(5) = trunc(RES(13,k),3)
      pout(6) = trunc(RES(14,k),3)
      pout(7) = trunc(RES(15,k),5)
      pout(8) = trunc(RES(16,k),5)
      angle = trunc((RES(17,k)*HOPI),5)
      write ( text, '(1x,f9.1,2f8.2,3f7.3,2f8.2,f9.2)' )
     +        (pout(j),j=1,8), angle
      call printo ( text )
      dx = trunc((RES(1,k)-INXY(1,k)),5)
      dy = trunc((RES(2,k)-INXY(2,k)),5)
      write ( text, '( '' Shift of '',2f8.2,
     +                 '' from input position'')' ) dx, dy
      call printo ( text )

      call printo ( 'Close companions' )
      call printo ( ' No   Height     X       Y       X  Offset fr'//
     +              ' star Y' )
      jn = 0
      do j = 1, MAXTOT
         if ( nint(COM(6,j)).eq.1 .and. nint(COM(5,j)).eq.k ) then
            dx = trunc((COM(1,j)-RES(1,k)),5)
            dy = trunc((COM(2,j)-RES(2,k)),5)
            pout(1) = trunc(COM(3,j),7)
            pout(2) = trunc(COM(1,j),5)
            pout(3) = trunc(COM(2,j),5)
            jn = jn + 1
            if ( jn.le.MAXCL ) then
               write ( text, '(1x,i2,f9.1,2f8.2,2x,2f8.2)' ) jn,
     +                 pout(1), pout(2), pout(3), dx, dy
               call printo ( text )
            endif
         endif
      enddo

      call printo ( 'Faint companions' )
      call printo ( 'No   Height     X       Y       X  Offset fr'//
     +              ' star Y' )
      jn = 0
      do j = 1, MAXTOT
         if ( nint(COM(6,j)).eq.2 .and. nint(COM(5,j)).eq.k ) then
            dx = trunc((COM(1,j)-RES(1,k)),5)
            dy = trunc((COM(2,j)-RES(2,k)),5)
            pout(1) = trunc(COM(3,j),7)
            pout(2) = trunc(COM(1,j),5)
            pout(3) = trunc(COM(2,j),5)
            jn = jn + 1
            if ( jn.le.MAXFA ) then
               write ( text, '(1x,i2,f9.1,2f8.2,2x,2f8.2)' )
     +                 jn, pout(1), pout(2), pout(3), dx, dy
               call printo ( text )
            endif
         endif
      enddo

      call printo ( 'Bad areas' )
      call printo ( ' No     X  b.l.h. Y      X b.l.h. fr'//
     +              ' star Y      X  Size  Y' )
      jn = 0
      do j = 1, MAXTOT
         if ( nint(COM(6,j)).eq.3 .and. nint(COM(5,j)).eq.k ) then
            jn = jn + 1
            if ( jn.le.MAXBAD ) then
               dx = trunc((nint(COM(1,j))-RES(1,k)),3)
               dy = trunc((nint(COM(2,j))-RES(2,k)),3)
               kx = nint(COM(3,j)) - nint(COM(1,j)) + 1
               ky = nint(COM(4,j)) - nint(COM(2,j)) + 1
               kx = max(-999,min(999,kx))
               ky = max(-999,min(999,ky))
               pout(1) = trunc(COM(1,j),7)
               pout(2) = trunc(COM(3,j),7)
               write ( text, '(1x,i2,2i8,2x,f8.2,7x,f8.2,i7,4x,i5)' )
     +                jn, nint(pout(1)), nint(pout(2)), dx, dy, kx, ky
               call printo ( text )
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_ALLSUB -- Remove stars of fit (and maybe its base) from an array
C
C   a.j.penny                   ral                    86-01-02-1700

      subroutine pr_allsub ( data, kxd, kyd, kxsd, kysd, kbase, kxs,
     +                       kys, tres, tcom, kstar )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer   kxd		!i: X size of array
      integer   kyd		!i: Y size of array
      real      data(kxd,kyd)	!i/o: Data array (before/after subtraction)
      integer   kxsd		!i: X posn in main image of this data array
      integer   kysd		!i: Y posn in main image of this data array
      integer   kbase		!i: Flag to remove base (1=yes,other=no)
      integer   kxs		!i: X posn in main image of blh of fit array
      integer   kys		!i: Y posn in main image of blh of fit array
      real      tres(TBX,TBY)	!i: Stars results array
      real      tcom(6,MAXTOT)	!i: Companion stars/bad areas info array
      integer   kstar		!i: Star number of star being removed
C--
      real prof(9), x, y, h, sky
      integer j, k, kww(4), ierr
Cbegin


      if ( kbase.eq.1 ) then						!Remove base, if so wanted
         do k = 1, kyd
            do j = 1, kxd
               if ( data(j,k).ne.RINVAL ) then
                  x = j - (kxsd-kxs)
                  y =  k - (kysd-kys)
                  sky = tres(10,kstar)+x*tres(32,kstar)+y*tres(33,kstar)
                  data(j,k) = data(j,k) - sky
               endif
            enddo
         enddo
      endif

      call amovr ( tres(12,kstar), prof, 6 )				!Set up profile
      call amovr ( SR(7), prof(7), 3 )

      x = res(1,kstar) - kxsd + 1					!Remove main
      y = res(2,kstar) - kysd + 1
      h = res(9,kstar)
      call popsmr ( data, kxd, kyd, 1.0, RINVAL, x, y, h, prof,
     +  %val(IPMAP), MX, MY, 1, 1, MX, MY, MAGNIF, DOMAP, ierr, kww )

      do k = 1, MAXTOT							!Remove close and faint
         if ( (nint(tcom(6,k)).eq.1 .or. nint(tcom(6,k)).eq.2)  .and.	! stars fitted as well
     +        nint(tcom(5,k)).eq.kstar   ) then
            x = tcom(1,k) - kxsd + 1
            y = tcom(2,k) - kysd + 1
            h = tcom(3,k)
            call popsmr ( data, kxd, kyd, 1.0, RINVAL, x, y, h, prof,
     +                    %val(IPMAP), MX, MY, 1, 1, MX, MY, MAGNIF,
     +                    DOMAP, ierr, kww )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GETANGLE -- Get whether angle to be fixed and if so, what at
C
C   a.j.penny                   ral                    86-01-02-1200

      subroutine pr_getangle ( tfixangle, sr6, flag )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      logical	tfixangle		!i/o: old/new if angle is fixed
      real	sr6			!i/o: old/new angle (radians)
      logical	flag			!o:  Changed whether angle fixed?
C--
      logical oldfix
      real oldangle, angle
Cbegin


      if ( ST_FAILED ) return

      flag = .false.

      oldfix = tfixangle
      call get1b ( 'FIXANGLE', tfixangle, tfixangle )
      if ( ST_FAILED ) return
      oldangle = sr6*HOPI
      call get1r ( 'ANGLE', angle, oldangle, -90.0, 90.0 )
      if ( ST_FAILED ) return
      sr6 = angle/HOPI

      if ( oldangle.ne.angle ) flag = .true.
      if ( oldfix .and. .not.tfixangle ) flag = .true.
      if ( .not.oldfix .and. tfixangle ) flag = .true.



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GETFIXP -- Main pars (rmaj,rmin,power,prmaj,prmin) to be fixed?
C
C   a.j.penny                   ral                    86-01-02-1200

      subroutine pr_getfixp ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k
      character*70 text
      character*5 pname(5)
      data pname / 'Rmaj ', 'Rmin ', 'Power', 'PRmaj', 'PRmin' /
      logical oldfix, newfix
Cbegin


      if ( ST_FAILED ) return

      do k = 1, 5
         write ( text, '('' Profile parameter '',a)' ) pname(k)
         call printo ( text )
         oldfix = FIXPROF(k)
         call get1b ( 'FIXPROF', newfix, oldfix )
         if ( ST_FAILED ) return
         FIXPROF(k) = newfix
      enddo
      call printo ( 'Profile parameter Angle' )
      oldfix = FIXANGLE
      call get1b ( 'FIXPROF', FIXANGLE, oldfix )
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_LOADBL -- Load bad areas from com into real areas
C
C   a.j.penny                   ral                    86-01-02-1200

      subroutine pr_loadbl ( data, kx, ky, tcom, jxs, jys, kstar )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer kx		!i: X size of array
      integer ky		!i: Y size of array
      real    data(kx,ky)	!i/o: array to have bad areas loaded
      real    tcom(6,MAXTOT)	!i: Bad areas/comps storage array
      integer jxs		!i: X posn in main image of blh of array
      integer jys		!i: Y posn in main image of blh of array
      integer kstar		!i: Number of star being done
C--
      integer k, jx, jy, jxa, jya, jxb, jyb
Cbegin


      do k = 1, MAXTOT
         if ( nint(tcom(6,k)).eq.3 .and. nint(tcom(5,k)).eq.kstar ) then
            jxa = nint(tcom(1,k)) - jxs + 1
            jxb = nint(tcom(3,k)) - jxs + 1
            jya = nint(tcom(2,k)) - jys + 1
            jyb = nint(tcom(4,k)) - jys + 1
            do jy = jya,jyb
               do jx = jxa,jxb
                  if ( jx.ge.1 .and. jx.le.kx .and.
     +                 jy.ge.1 .and. jy.le.ky ) data(jx,jy) = RINVAL
               enddo
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_UPDATEFIT -- Update fit parameters - make means
C
C   a.j.penny                   stsCi                    87-05-21

      subroutine pr_updatefit ( num )

      implicit none
      include 'profile.inc'

      integer num		!i: Deal with stars no 1 to no 'NUM'
C--
      integer n, k, j, nstart
      real tsr(6), wsn, av, std
Cbegin


      nstart = 17							!Reject poor stars, and if more than
      if ( FIXANGLE ) nstart = 16					! 5 reject on divergence from mean
      do j = 16, 12, -1
         n = 0
         do k = 1, num
            if ( RES(34,k).gt.0.5 .and. RES(35,k).gt.0.5  ) then
               NUSE(k) = 1
               n = n + 1
            else
               NUSE(k) = 0
            endif
         enddo
         if ( n.gt.5 ) then
            do k = 1, num
               RUSE(k) = RES(j,k)
            enddo
            call pr_avstd ( RUSE, NUSE, num, av, std )
            do k = 1, num
               if ( abs(RUSE(k)-av).gt.(2.0*std) ) NUSE(k) = 0
            enddo
         endif
      enddo

      nstart = 6							!Calc mean weighted fit,
      if ( FIXANGLE ) nstart = 5					! if any good fits
      wsn = 0.0
      call azeror ( tsr, nstart )
      do k = 1, num
         if ( NUSE(k).eq.1 ) then
            do j = 1, nstart
               tsr(j) = tsr(j) + RES(11+j,k)*RES(9,k)
            enddo
            wsn = wsn + RES(9,k)
         endif
      enddo
      if ( wsn.gt.0.005 ) call adivkr ( tsr, wsn, SR, nstart )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_LOADCOM -- Load output file with companions
C
C   a.j.penny                  stsCi    86-10-29

      subroutine pr_loadcom ( com, maxtot, out, tbxva, tbya )

      implicit none

      integer  maxtot		!i: Y size of bad areas/companions array
      real     com(6,maxtot)	!i: Bad areas/companions array
      integer  tbxva		!i: X size of output array
      integer  tbya		!i: Y size of output array
      real     out(tbxva,tbya)	!i/o: Output array
C--
      integer  k, num
Cbegin


      num = 0
      do k = 1, maxtot
         if ( abs(com(6,k)).gt.0.5 ) then
            num = min(tbya,(num+1))
            call amovr ( com(1,k), out(6,num), 6 )
            if ( com(6,k).lt.-0.5 ) out(11,num) = nint(abs(com(6,k)))
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SHIFTXY -- Shift all stars, companions and bad areas by an xy amount
C
C  a j penny              stsci               86-12-23

      subroutine pr_shiftxy ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k, kw
      real dx, dy
Cbegin


      if ( ST_FAILED ) return

      dx = 0.0
      dy = 0.0
      call get2r ( 'SHIFTXY', dx, dy, .false., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      do k = 1, TBY

         RES(1,k) = RES(1,k) + dx
         RES(2,k) = RES(2,k) + dy
      enddo

      do k = 1, MAXTOT
         kw = nint(COM(6,k))
         if ( kw.eq.1 .or. kw.eq.2 ) then
            COM(1,k) = COM(1,k) + dx
            COM(2,k) = COM(2,k) + dy
         elseif ( kw.eq.3 ) then
            COM(1,k) = COM(1,k) + dx
            COM(2,k) = COM(2,k) + dy
            COM(3,k) = COM(3,k) + dx
            COM(4,k) = COM(4,k) + dy
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_ADDMAP -- Take the resiuduals map and adds it onto the existing Map map
C
C  a j penny                      stsci                   1987-01-18

      subroutine pr_addmap ( resid, residh, map )

      implicit none
      include 'profile.inc'

      real   resid(LXM,LYM)		!i: Residuals array
      real   residh(LXM,LYM)		!i: Residuals filling array
      real   map(MX,MY,1)		!i/o: Profile map
C--
      logical full
      integer j, k, js, ks, jin, kin
Cbegin


      full = .true.							!Check residuals map is full
      do k = 1, LYM							! (or completely blank). If not return
         do j = 1, LXM
            if ( residh(j,k).eq.0.0 ) full = .false.
         enddo
      enddo
      if ( .not.full ) then
         full = .true.
         do k = 1, LYM
            do j = 1, LXM
               if ( resid(j,k).ne.0.0 ) full = .false.
            enddo
         enddo
      endif
      if ( .not.full ) then
        call printo ( 'Residuals map must be zero or complete. '//
     +                'Map not made.' )
        return
      endif

      do k = 1, MY							!Load the Map
         do j = 1, MX
            js = LXM/2 - MX/2
            ks = LYM/2 - MY/2
            jin = j + js
            kin = k + ks
            if ( jin.ge.1 .and. jin.le.lxm .and.
     +           kin.ge.1 .and. kin.le.lym       ) then
               map(j,k,1) = map(j,k,1) + resid(jin,kin)
            else
               map(j,k,1) = map(j,k,1)
            endif
         enddo
      enddo
      DONEMAP = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_COPMAP -- Take the profile map and copies it onto a new sized one
C              doing sub-division change if needed.
C
C  a j penny                      stsci                   1987-01-25

      subroutine pr_copmap ( in, nxi, nyi, magi, out, nxo, nyo, mago )

      implicit none

      integer nxi		!i: X size of input profile map
      integer nyi		!i: Y size of input profile map
      real    in(nxi,nyi)	!i: Input profile map
      integer magi		!i: Magnification of input profile map
      integer nxo		!i: X size of output profile map
      integer nyo		!i: Y size of output profile map
      real    out(nxo,nyo)	!o: Output profile map
      integer mago		!i: Magnification of output profile map
C--
      integer j, js, jin, jxi, k, ks, kin, kyi
      real xin, yin, dxout, dyout, dfdx, dfdy
      real rinter
      external rinter
Cbegin


C  Load the Map

      if ( mago.ne.magi ) then

         jxi = nxi/2
         kyi = nyi/2
         do k = 1, nyo
            do j = 1, nxo
               dxout = (j-nxo/2)/real(mago)
               xin = dxout*real(magi) + jxi
               dyout = (k-nyo/2)/real(mago)
               yin = dyout*real(magi) + kyi
               out(j,k) = rinter ( in, nxi, nyi, 1, 1, nxi, nyi, xin,
     +                             yin, dfdx, dfdy )
            enddo
         enddo
         call printo ( 'Had to interpolate new profile map '//
     +                 '- pixel subdivision changed' )

      else

         js = nxi/2 - nxo/2
         ks = nyi/2 - nyo/2
         do k = 1, nyo
            do j = 1, nxo
               jin = j + js
               kin = k + ks
               if ( jin.ge.1 .and. jin.le.nxi .and.
     +              kin.ge.1 .and. kin.le.nyi       ) then
                  out(j,k) = in(jin,kin)
               else
                  out(j,k) = 0.0
               endif
            enddo
         enddo
         call printo ( 'Profile Map copied to new Map' )

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DOTOPR -- Calc rough heights of the stars (real)
C
C  a j penny                      stsci                   1987-01-18

      subroutine pr_dotopr ( im )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real    im(NX,NY)		!i: Main image
C--
      integer k, jx, jy, ksky, kxc(2), kyc(2),
     +        jj, jjs, jje, jk, jks, jke, istat
      real am, std, rv, vmax
      logical doit
Cbegin


      if ( ST_FAILED ) return

      doit = .true.
      do k = 1, TBY
         if (RES(9,k).ne.0.0) doit = .false.
      enddo

      if ( doit ) then
         kxc(1) = 1
         kxc(2) = NX
         kyc(1) = 1
         kyc(2) = NY
         call ranger ( im, NX, NY, kxc, kyc, RINVAL, am, std, istat )
         ksky = am

         do k = 1, TBY
            jx = max(1,min(NX,nint(RES(1,k))))
            jy = max(1,min(NY,nint(RES(2,k))))
            vmax = im(jx,jy)
            if ( vmax.eq.RINVAL ) vmax = 0
            jjs = max(1,min(NX,jx-1))
            jje = max(1,min(NX,jx+1))
            jks = max(1,min(NY,jy-1))
            jke = max(1,min(NY,jy+1))
            do jk = jks, jke
               do jj = jjs, jje
                  rv = im(jj,jk)
                  if ( rv.ne.RINVAL ) vmax = max(vmax,rv)
               enddo
            enddo
            rv = vmax
            if ( rv.eq.0.0 ) then
               RES(9,k) = 30000.0
            else
               RES(9,k) = BS*(rv-ksky)
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DOTOPS -- Calc rough heights of the stars (integer*2)
C
C  a j penny                      stsci                   1987-01-18

      subroutine pr_dotops ( im )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer*2 im(NX,NY)	!i: Main image
C--
      integer k, kv, kvmax, jx, jy, ksky, kxc(2), kyc(2),
     +        jj, jjs, jje, jk, jks, jke, istat
      real am, std
      logical doit
Cbegin


      doit = .true.
      do k = 1, TBY
         if (RES(9,k).ne.0.0) doit = .false.
      enddo

      if ( doit ) then
         kxc(1) = 1
         kxc(2) = NX
         kyc(1) = 1
         kyc(2) = NY
         call ranges ( im, NX, NY, kxc, kyc, INVAL, am, std, istat )
         ksky = am

         do k = 1, TBY
            jx = max(1,min(NX,nint(RES(1,k))))
            jy = max(1,min(NY,nint(RES(2,k))))
            kvmax = im(jx,jy)
            if ( kvmax.eq.INVAL ) kvmax = 0
            jjs = max(1,min(NX,jx-1))
            jje = max(1,min(NX,jx+1))
            jks = max(1,min(NY,jy-1))
            jke = max(1,min(NY,jy+1))
            do jk = jks, jke
               do jj = jjs, jje
                  kv = im(jj,jk)
                  if ( kv.ne.INVAL ) kvmax = max(kvmax,kv)
               enddo
            enddo
            kv = kvmax
            if ( kv.eq.0 ) then
               RES(9,k) = 30000.0
            else
               RES(9,k) = BS*(kv-ksky)
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_COMPZERO -- Null comps and faint comps in main store
C
C  a j penny                      stsci                   1987-04-23

      subroutine pr_compzero ( )

      implicit none
      include 'profile.inc'
C--
      integer j, k
Cbegin


      do j = 1, MAXTOT
         if ( nint(COM(6,j)).eq.1 .or.
     +        nint(COM(6,j)).eq.2       ) then
            do k = 1, 6
               COM(k,j) = 0.0
            enddo
         endif
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    PROSUBB.F
C
C    Contains:-
C
C PR_STARRJ     Which are good in a list of star mags,radii,iterations ?
C PR_STARRK     Type rejects; get/do angle flip ones;get stars accept/reject
C PR_AVSTD      Calc mean/std dev (throw out 2*std dev) of flagged vals
C PR_AVSTDA     Calc mean and std dev of flagged values
C PR_RESCALC    Calc mean residual/radial rsiduals, using fixed profile
C PR_MLOAD      Load the mean residual/radial residuals
C PR_QPSTORE    Put radial plot of profile and fit into storage file



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_STARRJ -- Which are good in a list of star mags,radii,iterations ?
C
C   a.j.penny                   rgo                    83-2-22

      subroutine pr_starrj ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      real radj(2), am, std
      integer k, num
      logical change
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY							!Set all to be accepted
         RES(34,k) = 1.0
      enddo

      num = 0
      do k = 1, TBY							!Reject on Magnitude
         if ( (TBY-num).gt.3 .and. RES(3,k).gt.49.0 ) then
            RES(34,k) = 0.0
            num = num + 1
         endif
      enddo

      do k = 1, TBY							!Reject on failed radius
         if ( RES(34,k).gt.0.5 .and. (TBY-num).gt.3 .and.
     +        ( RES(12,k).lt.0.25 .or. RES(12,k).gt.99.0 .or.
     +          RES(13,k).lt.0.25 .or. RES(13,k).gt.99.0 ) ) then
            RES(34,k) = 0.0
            num = num + 1
         endif
      enddo

      do k = 1, TBY							!Reject on discrepant X Radius
         NUSE(k) = RES(34,k) + 0.1
         RUSE(k) = RES(12,k)
      enddo
      call pr_avstd ( RUSE, NUSE, TBY, am, std )
      radj(1) = am - 2.0*std
      radj(2) = am + 2.0*std
      do k = 1, TBY
         if ( RES(34,k).gt.0.5 .and. (TBY-num).gt.3 .and.
     +        ( RES(12,k).lt.radj(1) .or. RES(12,k).gt.radj(2) ) ) then
            num = num + 1
            RES(34,k) = 0.0
         endif
      enddo

      do k = 1, TBY							!Reject on discrepant Y Radius
         NUSE(k) = RES(34,k) + 0.1
         RUSE(k) = RES(13,k)
      enddo
      call pr_avstd ( RUSE, NUSE, TBY, am, std )
      radj(1) = am - 2.0*std
      radj(2) = am + 2.0*std
      do k = 1, TBY
         if ( RES(34,k).gt.0.5  .and. (TBY-num).gt.3 .and.
     +        (RES(12,k).lt.radj(1).or.RES(13,k).gt.radj(2)) ) then
            num = num + 1
            RES(34,k) = 0.0
         endif
      enddo

      do k = 1, TBY							!Reject on number of Iterations
         if ( RES(34,k).gt.0.5  .and. (TBY-num).gt.3 .and.
     +        (RES(6,k).gt.19.1 .or.RES(8,k).gt.0.1) ) then
            RES(34,k) = 0.0
            num = num + 1
         endif
      enddo

      call pr_starrk ( 3, change )						!Type out rejected stars, and
									! get angle flips and reject nos change

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_STARRK -- Type rejects; get/do angle flip ones;get stars accept/reject
C  It returns CHANGE to tell if any changes made.
C
C   a.j.penny                   ral                    86-01-16

      subroutine pr_starrk ( kopt, change )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer kopt		!i: Change? none:fit angle:reject:both (0:1:2:3)
      logical change		!o: If any changed,true. otherwise false
C--
      integer nrej(8), k, ka, num, numb, numa
      logical ktype, kend, rejout
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      change = .false.							!Set up change flag

      numb = 0								!Type out rejected stars
      numa = 1
      rejout = .false.
      ktype = .false.
      do while ( numa.le.TBY )
         if ( RES(34,numa).lt.0.5 ) then
            numb = numb + 1
            nrej(numb) = numa
            if ( numb.eq.8 ) then
               if ( ktype ) then
                  write ( text, '(''                       '',8i4)' )
     +                    (nrej(k),k=1,8)
               else
                  write ( text, '('' Rejected stars are :- '',8i4)' )
     +                    (nrej(k),k=1,8)
               endif
               call printo ( text )
               numb = 0
               rejout = .true.
               ktype = .true.
            endif
         endif
         numa = numa + 1
      enddo
      if ( numb.ne.0 ) then
         rejout = .true.
         if ( ktype ) then
            write ( text, '(''                       '',8i4)' )
     +              (nrej(k),k=1,numb)
         else
            write ( text, '('' Rejected stars are :- '',8i4)' )
     +              (nrej(k),k=1,numb)
         endif
         call printo ( text )
      endif
      if ( .not.rejout ) then
         call printo ( 'Rejected stars are :-  None' )
      endif

      if ( kopt.eq.1 .or. kopt.eq.3 ) then
         call printo ( 'Rotate nth angle by +-180 degrees (+-n,0) ' )	!Get angle changes
         kend = .false.
         do while ( .not.kend )
            call get1i ( 'FLIP', k, 0, -1*TBY, TBY )
            if ( ST_FAILED ) return
            if ( k.ne.0 ) then
               change = .true.
               if ( k.gt.0 ) then
                  RES(17,k) = RES(17,k) + PI
               else
                  ka = -1*k
                  RES(17,ka) = RES(17,ka) - PI
               endif
            else
               kend = .true.
            endif
         enddo
      endif

      if ( kopt.eq.2 .or. kopt.eq.3 ) then
         call printo ( 'Reject or accept nth star (n,-n,0) ?' )		!Get wether to reject or accept more stars
         kend = .false.
         do while ( .not.kend )
            call get1i ( 'REJECT', k, 0, -1*TBY, TBY )
            if ( ST_FAILED ) return
            if ( k.gt.0 ) then
               change = .true.
               RES(34,k) = 0.0
               num = num + 1
            elseif ( k.lt.0 ) then
               change = .true.
               k = -1*k
               if (RES(34,k).lt.0.5) num = num - 1
               RES(34,k) = 1
            elseif ( k.eq.0 ) then
               kend = .true.
            endif
         enddo
      endif

      if ( change ) DONEMEAN = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_AVSTD -- Calc mean/std dev (throw out 2*std dev) of flagged vals
C
C   a.j.penny                   rgo                    83-z-z

      subroutine pr_avstd ( val, nuse, num, av, std )

      implicit none

      integer	num		!i: No of flagged values
      real	val(num)	!i: Values vector
      integer	nuse(num)	!i: Flags vector (1=use;other=not)
      real	av		!o: Mean
      real	std		!o: Standard deviation
C--
      logical exit
      integer k, nout, ntot, nvmax, kout, nouta
      real rv, vmax, top, bot
Cbegin


      nout = 0
      call pr_avstda ( val, nuse, num, av, std )

      exit = .false.							!Loop until mean and std dev
      do while ( .not.exit )						! no longer corrected

         call asumi ( nuse, num, rv )					!Chuck out largest residual and
         ntot = nint(rv)							! recalc mean and std dev (If more than 3)
         if ( ntot.gt.3 ) then
            vmax = 0.0
            nvmax = 1
            do k = 1, num
               if ( nuse(k).eq.1 ) then
                  rv = abs(val(k)-av)
                  if ( rv.gt.vmax ) then
                     vmax = rv
                     nvmax = k
                  endif
               endif
            enddo
            nuse(nvmax) = 0
            call pr_avstda ( val, nuse, num, av, std )
         endif

C  Chuck out all those more than 2.0 std dev and recalc mean and std
C  dev again. Then chuck out all those now more. Repeat until no more
C  chucked out.

         nouta = nout
         kout = 1
         do while ( kout.ne.0 .and. nout.lt.(num-4) )
            top = av + 2.0*std
            bot = av - 2.0*std
            kout = 0
            do k = 1, num
               if ( nuse(k).eq.1 ) then
                  if ( val(k).gt.top .or. val(k).lt.bot ) then
                     kout = 1
                     nuse(k) = 0
                     nout = nout + 1
                  endif
               endif
            enddo
            call pr_avstda ( val, nuse, num, av, std )
         enddo

         if ( nout.eq.nouta ) exit = .true.				!If none chucked out this loop, exit

      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_AVSTDA -- Calc mean and std dev of flagged values
C
C    a j penny                    dao          1988-05-16


      subroutine pr_avstda ( val, nuse, num, av, std )

      implicit none

      integer	num		!i: No of flagged values
      real	val(num)	!i: Values vector
      integer	nuse(num)	!i: Flags vector (1=use;other=not)
      real	av		!o: Mean
      real	std		!o: Standard deviation
C--
      double precision s, ss, sn
      integer k

      s = 0.0d0
      ss = 0.0d0
      sn = 0.0d0
      do k = 1, num
         if  ( nuse(k).eq.1 ) then
            s = s + dble(val(k))
            ss = ss + dble(val(k))*dble(val(k))
            sn = sn + 1.0d0
         endif
      enddo
      av = 0.0
      std = 0.0
      if ( sn.gt.0.5d0 ) then
         av = sngl(s/sn)
         if ( sn.gt.1.5d0 ) then
            std = sngl(dsqrt((ss-(s*s/sn))/(sn-1.0d0)))
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_RESCALC -- Calc mean residual/radial residuals, using fixed profile
C
C      a j penny                  ral                  85-12-31-1226

      subroutine pr_rescalc ( tres, hres, tota, totb )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real	tres(LXM,LYM)		!o: Mean residuals; fixed prof fit
      real	hres(LXM,LYM)		!o: Height of prof at pixels
      real	tota(LX,LY)		!o: Work space for image data
      real	totb(LX,LY)		!o: Work space for image data
C--
      integer  j, k, kk, ljx, ljy, next, kgap, istat, jstar,
     +         jxso(TBYMAX), jyso(TBYMAX), numpar, ksent, jxs, jys,
     +         jfit(36), jfitf(36), lxa, lya, nin, iter, numm, numf
      real     anum(200), rx, ry, aljx, aljy, xc, yc, xmaxa, ymaxa,
     +         xmax, ymax, drmax, anumpt, ah, rr, prr, p, d, da, db,
     +         ddd, ak, ag, cc(36), ccf(36)
      logical  toobig, parfix

      logical  kfixed(5)
      data     kfixed / .true., .true., .true., .true., .true. /
Cbegin


      if ( ST_FAILED ) return

      numpar = 1

      rx = SR(1)							!Get profile parameters
      ry = SR(2)
      call subdiv ( rx, ry, ljx )
      aljx = ljx
      ljy = ljx
      aljy = ljy

      call amovr ( RES, RESA, TBX*TBY )					!Load working values
      call amovr ( COM, COMA, 6*MAXTOT )

      xc = real(LX)/2.0							!Calc no of radial bins
      yc = real(LY)/2.0
      xmax = (xc-1.0)/rx
      xmaxa = (real(LX) - xc)/rx
      xmax = max(xmax,xmaxa)
      ymax = (yc - 1.0)/ry
      ymaxa = (real(LY) - yc)/ry
      ymax = max(ymax,ymaxa)
      drmax = sqrt(xmax*xmax+ymax*ymax)
      anumpt = min(15.0*drmax,200.0)

      call azeror ( tres, LXM*LYM )					!Zero radial profile values and
      call azeror ( hres, LXM*LYM )					! residuals and total height
      call azeror ( anum, 200 )
      call azeror ( PDIST, 200 )
      call azeror ( PDATA, 200 )

      if ( DOPAR ) call pr_par_sii ( numpar, istat )			!Number of par processors

      do k = 1, TBY							!Load old posns
         if ( RESA(34,k).gt.0.5 ) then
            jxso(k) = RESA(1,k) - real((LX/2)-1)
            jyso(k) = RESA(2,k) - real((LY/2)-1)
         endif
      enddo

      ksent = 0
      do k = 1, TBY							!Do all the stars
         if ( RESA(34,k).gt.0.5 ) then

            call pr_partoobig ( k, toobig )
            if ( DOPAR .and. .not.toobig ) then				!Use par processors
               if ( ksent.ge.numpar ) then
                  call pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin,
     +                            lxa, lya )
                  call pr_dores ( jstar, RESA, TBX, TBYMAX, COMA,
     +                            MAXTOT, 0, cc, ccf, jxs, jys, iter,
     +                            nin, lxa, lya, MAXCL, MAXFA )
                  call pr_mload ( tres, hres, tota, jstar, jxso, jyso,
     +                            rx, ry, aljx, aljy, drmax, anumpt,
     +                            anum )
               endif
               call pr_fitload ( totb, .true., .true., SR,
     +                           k, RESA, COMA, 1, cc, jfit, numm,
     +                           ccf, jfitf, numf, parfix, LX, LY, jxs,
     +                           jys, nin )
               call pr_parout ( totb, LX, LY, %val(IPMAP), .true.,
     +                          .true., k, 1, cc, jfit, numm, ccf,
     +                          jfitf,  numf, parfix, jxs, jys, nin,20)
               ksent = ksent + 1
            else
               call pr_solve ( tota, totb, .true., kfixed, SR, 20, k, 	!Solve for this star
     +                         RESA, COMA, 0, 1 )
               call pr_mload ( tres, hres, tota, k, jxso, jyso, rx, ry,
     +                         aljx, aljy, drmax, anumpt, anum )
            endif

         endif
      enddo

      if ( DOPAR ) then
         if ( ksent.eq.0 ) then
            call pr_par_fend
         else
            do k = 1, min(ksent,numpar)
               if ( k.eq.1 .and. ksent.lt.numpar ) call pr_par_fend
               call pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin,
     +                         lxa, lya )
               call pr_dores ( jstar, RESA, TBX, TBYMAX, COMA,
     +                         MAXTOT, 0, cc, ccf, jxs, jys, iter,
     +                         nin, lxa, lya, MAXCL, MAXFA )
               call pr_mload ( tres, hres, tota, jstar, jxso, jyso,
     +                         rx, ry, aljx, aljy, drmax, anumpt,
     +                         anum )
               if ( k.eq.1 .and. ksent.ge.numpar ) call pr_par_fend
            enddo
         endif
      endif

      do k = 1, LYM							!Scale the mean residuals
         do j = 1, LXM
            ah = hres(j,k)
            if ( ah.gt.0.005 ) then
               tres(j,k) = tres(j,k)/ah
            else
               tres(j,k) = 0.0
            endif
         enddo
      enddo

      do k = 1, 200-1							!Bunch up if any points in the
         if ( anum(k).eq.0.0 ) then					! radial profile have no data
            next = k
            kk = 0
            do j = k+1, 200
               if ( kk.eq.0 .and. anum(j).ne.0.0 ) then
                  kk = 1
                  next = j
               endif
            enddo
            kgap = next - k
            anum(k) = anum(k+kgap)
            PDIST(k) = PDIST(k+kgap)
            PDATA(k) = PDATA(k+kgap)
            anum(k+kgap) = 0.0
         endif
      enddo
      NPTOT = 0
      do k = 1, 200
         if ( anum(k).ne.0.0 ) NPTOT = NPTOT + 1
      enddo

      call adivr ( PDATA, anum, PDATA, NPTOT )				!Divide by number of points added
      call adivr ( PDIST, anum, PDIST, NPTOT )				! in in each radial point to get mean value

      rr = abs((rx+ry)/2.0)						!Calc Fit at the points
      prr = abs((SR(4)+SR(5))/2.0)
      p = SR(3)
      do k = 1, NPTOT
         d = PDIST(k)
         da = d/rr
         db = d/prr
         ddd = p*(1.0+db)*alog(da)
         if (ddd.gt.20.0) ddd = 20.0
         ak = 1.0/(1.0+exp(ddd))
         ddd = (d/SR(8))**SR(9)
         if ( ddd.gt.12.0 ) ddd = 12.0
         ag = 1.0*SR(7)*exp(-1.0*ddd)
         PFIT(k) = ak + ag
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MLOAD -- Load the mean residual/radial residuals
C
C  alan penny               RAL               1991 July

      subroutine pr_mload ( tres, hres, tota, k, jxs, jys, rx,
     +                      ry, aljx, aljy, drmax, anumpt, anum )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real	tres(LXM,LYM)		!i/o: Mean residuals; fixed prof fit
      real	hres(LXM,LYM)		!i/o: Height of prof at pixels
      real	tota(LX,LY)		!i/o: Work space for image data
      integer   k			!i: Star number
      integer   jxs(TBYMAX)		!i: Old X posns of blh of boxes
      integer   jys(TBYMAX)		!i: Old Y posns of blh of boxes
      real	rx			!i: Star major axis radius
      real	ry			!i: Star minor axis radius
      real	aljx			!i: Pixel subdivision in X
      real	aljy			!i: Pixel subdivision in Y
      real	drmax			!i: Maximum radial distance
      real	anumpt			!i: Max number of radial points
      real	anum(200)		!i/o: Radial points
C--
      integer jj, kk, lrx, lry, kxs, kxe, kys, kye, nin, kd
      real    dx, dy, val, ah, dr
Cbegin


      if ( ST_FAILED ) return

      kxs = jxs(k)
      kxe = kxs + LX - 1
      kys = jys(k)
      kye = kys + LY - 1

      if ( IMTYPE.eq.'SHORT' ) then					!Get the residuals from this fit
         call copvsr ( %val(IPIM), NX, NY, tota, LX, LY, kxs, kxe,
     +                 kys, kye, BS, BZ, INVAL, RINVAL, nin, 0 )
      else
         call copvrr ( %val(IPIM), NX, NY, tota, LX, LY, kxs, kxe,
     +                 kys, kye, BS, BZ, RINVAL, nin, 0 )
      endif

      call pr_loadbl ( tota, LX, LY, COMA, kxs, kys, k )

      call pr_allsub ( tota, LX, LY, kxs, kys, 1, kxs, kys,
     +                 RESA, COMA, k )

      do kk = 1, LY							!Add this into the mean residuals
         do jj = 1, LX							! and mean profile
            dx = real(jj) - (RESA(1,k)-real(kxs)+1.0)
            dy = real(kk) - (RESA(2,k)-real(kys)+1.0)
            dr = sqrt((dx/rx)**2.0+(dy/ry)**2.0)
            val = tota(jj,kk)
            if ( val.ne.RINVAL ) then
               lrx = LXM/2 + dx*aljx
               lry = LYM/2 + dy*aljy
               if ( lrx.ge.1 .and. lrx.le.LXM .and.
     +              lry.ge.1 .and. lry.le.LYM ) then
                  ah = max(0.0001,RESA(9,k))
                  tres(lrx,lry) = tres(lrx,lry) + val
                  hres(lrx,lry) = hres(lrx,lry) + ah
               endif
               if ( dr.le.drmax ) then
                  kd = 1 + int((anumpt-1.0)*(dr/drmax))
                  PDIST(kd) = PDIST(kd) + dr*((rx+ry)/2.0)
                  anum(kd) = anum(kd) + 1.0
                  ah = max(0.0001,RESA(9,k))
                  PDATA(kd) = PDATA(kd) + val/ah
               endif
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_QPSTORE -- Put radial plot of profile + fit into storage file
C
C      a j penny                  stsci                   86-10-24

      subroutine pr_qpstore ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      character*20 theader(4)
      data theader/'DISTANCE','FIT','DATA','RESIDUALS'/
      integer k, ipo, istat
      real tdata
      character*30 text
Cbegin


      if ( ST_FAILED ) return

      call optabw ( 'OUTRADIAL', ipo, 4+5, NPTOT, .true., istat )
      if ( ST_FAILED ) return

      if ( istat.ne.0 ) return

      call get1c ( 'TITLE', text, 'PROFILE profile plot', .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUTRADIAL', 'TITLE', text )
      do k = 1, 4
         call pthead ( 'OUTRADIAL', k, theader(k), istat )
      enddo
      call ident ( %val(ipo), 9, NPTOT )
      do k = 1, NPTOT
         call cop1r ( PDIST(k), %val(ipo), 4+5, NPTOT, 1+5, k )
         call cop1r ( PFIT(k),  %val(ipo), 4+5, NPTOT, 2+5, k )
         tdata = PDATA(k) + PFIT(k)
         call cop1r ( tdata,    %val(ipo), 4+5, NPTOT, 3+5, k )
         call cop1r ( PDATA(k), %val(ipo), 4+5, NPTOT, 4+5, k )
      enddo
      call canpar ( 'OUTRADIAL' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     PROSUBD.FOR
C
C    This contains:-
C
C PR_WWFITLOR  Determine a Lorentz profile from an array with faint comps
C PR_FITLOR    Determine a Lorentz profile from an array with no faint comps
C PR_SORTANGLE Put angle (and major/minor axes) in standard way
C PR_PROFIT    Fits a variable Lorentz to an array
C PR_FITPAR    Calc normal eqn values for this star at this posn
C PR_CCSUB     Remove stars  from an array, using info loaded into CC



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WWFITLOR -- Determine a Lorentz profile from an array with faint comps
C This fits the Lorentz to a box containing a group of main stars to be
C solved together, and a group of faint or distant companions which are
C subtracted from the array as the fit progresses.
C
C   alan penny                   ral                    1991 July

      subroutine pr_wwfitlor ( data, kx, ky, dataw, cc, jfit, numm,
     +                         itertot, ccf, jfitf, numf, itslim, kp,
     +                         parfix, tfixangle, tfixp, ftlim, htlim,
     +                         damp, rinval, map, mx, my, magnif,
     +                         domap )

      implicit none

      integer   kx		!i: X size of data array
      integer   ky		!i: Y size of data array
      real      data(kx,ky)	!i: Data array of image with stars in it
      real      dataw(kx,ky)	!o: Work space for image data
      real      cc(36)		!i/o: Profile and posns for main+close
      integer   jfit(36)	!i/o: Control for fit for main+close
      integer   numm		!i: Number of (main + bright/close comp) stars
      integer   itertot		!o: Total number of iterations done
      real      ccf(36)		!i/o: Profile and posns for faint
      integer   jfitf(36)	!i/o: Control for fit for faint
      integer   numf		!i: Number of faint/distant comp stars
      integer   itslim		!i: Max no of iterations allowed
      integer	kp		!i: Print out intermediate fits results
				!   (1/0=y/n) ?
      logical   parfix		!i: All profile pars fixed (true) or var
      logical   tfixangle	!i: Profile angle fixed (true) or var
      logical   tfixp(5)	!i: Profile pars fixed (true) or var
      real      ftlim		!i: Fit params fractional error for convergence
      real      htlim		!i: Fit height fractional error for convergence
      real      damp		!i: Fit damping factor
      real      rinval		!i: INVALID pixel magic value
      integer   mx		!i: X size of profile map
      integer   my		!i: Y size of profile map
      real      map(mx,my,1)	!i: Profile map
      integer   magnif		!i: Magnification of profile map
      logical   domap		!i: Flag for using profile map
C--
      integer k, kk, iter, nloops, jfitall(36), numall
      real ccold(12), diff, fdiff, ccall(36), chisq, ochisq, dchi
      logical again, nearpos, nearprof
Cbegin


      call amovr ( cc(4), ccold(4), 9 ) 				!Set loop flags
      nearpos = .false.
      nearprof = .false.
      itertot = 0
      again = .true.
      nloops = 0

      do while ( again )						!Do the loop
         nloops = nloops + 1

         if ( kp.eq.1 ) then
            call pargi ( nloops )
            call printd ( 'Loop number: %d' )
         endif

         call azeror ( ccall, 36 )
         call amovr ( cc, ccall, 12+numm*3 )
         call azeroi ( jfitall, 36 )
         call amovi ( jfit, jfitall, 12+numm*3 )
         kk = 36 - (13+numm*3) + 1
         call amovr ( ccf(13), ccall(13+numm*3), kk )
         call amovi ( jfitf(13), jfitall(13+numm*3), kk )

         call amovr ( data, dataw, kx*ky )				!Take original area
         numall = min(8,(numm+numf))

         call pr_fitlor ( dataw, kx, ky, ccall, jfitall, numall, iter,	!Fit all stars with fixed profiles
     +                    4, kp, .true., tfixangle, tfixp, ftlim, htlim,
     +                    damp, rinval, map, mx, my, magnif, domap,
     +                    nearpos, nearprof, chisq )

         nearpos = .true.

         if ( numf.gt.0 ) then						!Subtract the faint companions
            call azeror ( ccf, 36 )
            call amovr ( ccall, ccf, 12 )
            call amovr ( ccall(13+numm*3), ccf(13), numf*3 )
            do k = 1, numf
               kk = 15 + 3*numm + 3*(k-1)
               if ( jfitall(kk).eq.0 ) ccf(kk) = 0.0
            enddo
            call pr_ccsub ( dataw, kx, ky, ccf, numf, map, mx, my,
     +                      magnif, domap, rinval )
            if ( kp.eq.1 ) then
               call pargi ( numf )
               call printd ( 'Subtracted %d faint or distant star(s)' )
            endif
         endif

         call azeror ( cc, 36 )
         call azeroi ( jfit, 36 )
         call amovr ( ccall, cc, 12+numm*3 )
         call amovi ( jfitall, jfit, 12+numm*3 )

         call pr_fitlor ( dataw, kx, ky, cc, jfit, numm, iter,		!Fit the profile for main
     +                    4, kp, parfix, tfixangle, tfixp,		! (+ bright close comps)
     +                    ftlim, htlim, damp, rinval, map, mx, my,
     +                    magnif, domap, nearpos, nearprof, chisq )
         call pr_sortangle ( cc )

         nearprof = .true.
         itertot = itertot + iter

         again = .true.							!See if need to repeat

         if ( nloops.gt.1 ) then					!After  2 loops is
            again = .false.

            do k = 4, 12						!profile still changing ?
               diff = abs(cc(k)-ccold(k))
               if ( k.eq.9 ) then
                  if ( diff.gt.0.002 ) again = .true.
               else
                  fdiff = diff/max(1.0e-10,ccold(k))
                  if ( fdiff.gt.ftlim ) again = .true.
               endif
            enddo

            dchi = (chisq-ochisq)/max(0.000001,ochisq)			!Is chi-squared converged?
            if ( abs(dchi).lt.0.0001 ) again = .false.			!Chi-sq change v small

         endif

         call amovr ( cc(4), ccold(4), 9 )
         ochisq = chisq

         if ( itertot.ge.itslim ) again = .false.

         if ( cc(15).lt.1.0e-3 ) again = .false.			!Main star failed

      enddo								!Loop back if doing again


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FITLOR -- Determine a Lorentz profile from an array with no faint comps
C This fits an array with a sloping background and up to 8 stars with
C the same Lorentz 2-D profile, whose shape parameters can be found at
C the same time.
C
C It does this by using the s/r PR_PROFIT (qv), but does it carefully,
C taking the approximate input parameters and solving for the easy ones
C roughly and then with refined parameters doing the real fit. This
C ensures that even if the input parameters are not too good, the fit
C will still converge.
C
C Even so, the input parameters must be quite good to be safe.
C
C For details of the fitting see a listing of PR_PROFIT.
C
C    alan penny                  ral                  1991 July

      subroutine pr_fitlor ( data, kx, ky, cc, jfit, nstar, iter,
     +                       itslim, kp, parfix, tfixangle, tfixp,
     +                       ftlim, htlim, damp, rinval, map,
     +                       mx, my, magnif, domap, nearpos,
     +                       nearprof, chisq )

      implicit none

      integer	kx		!i: X size of DATA
      integer	ky		!i: Y size of DATA
      real	data(kx,ky)	!i: data
      real	cc(36)		!i/o: Fit parameters
				!      background (1,2,3)
				!      profile (4,5,6,7,8,9,10,11,12)
				!     star posns, heights (13,14,...,36)
      integer	jfit(36)	!i: -1= par fixed;0=not used;1=variable
      integer	nstar		!i: No of stars
      integer	iter		!o: No of iterations taken
      integer	itslim		!i: Max no of iterations allowed
      integer	kp		!i: Printing flag
      logical	parfix		!i: all parameters fixed (true)
      logical	tfixangle	!i: profile angle fixed (true)
      logical	tfixp(5)	!i: profile pars fixed (true)
      real      ftlim		!i: Fit params fractional error for convergence
      real      htlim		!i: Fit height fractional error for convergence
      real      damp		!i: Fit damping factor
      real      rinval		!i: INVALID pixel magic value
      integer   mx		!i: X size of profile map
      integer   my		!i: Y size of profile map
      real      map(mx,my,1)	!i: Profile map
      integer   magnif		!i: Magnification of profile map
      logical   domap		!i: Flag for using profile map
      logical   nearpos		!i: Stars already near correct positions?
      logical   nearprof	!i: Profile already near correct shape?
      real      chisq		!o: Chi-squared of fit
C--
      integer jh(8), k
Cbegin


      call amovki ( -1, jfit(4), 9 )

      if ( .not.nearpos ) then
         do k = 1, nstar						!Get 1st approx to posns by
            jh(k) = jfit(12+k*3)					! keeping heights and
            if ( jh(k).ne.0 ) jfit(12+k*3) = -1				! profile fixed
         enddo
         call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 3, kp,
     +                    ftlim, htlim, damp, rinval, map, mx, my,
     +                    magnif, domap, chisq )
         do k = 1, nstar
            if ( jfit(12+k*3 ).ne.0 ) jfit(12+k*3) = jh(k)
         enddo
      endif

      if ( parfix ) then						!Do fixed/variable profile
         call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, itslim,	!Fixed profile
     +                    kp, ftlim, htlim, damp, rinval, map, mx, my,
     +                    magnif, domap, chisq )
      else
         if ( .not.nearprof ) then
            call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 2, 	!Variable profile: 1st get heights
     +                       kp, ftlim, htlim, damp, rinval, map, mx,
     +                       my, magnif, domap, chisq )
            if ( .not.tfixp(1) .or. .not.tfixp(2) ) then		!Then get approx radius
               if ( .not.tfixp(1) ) jfit(4) = 1
               if ( .not.tfixp(2) ) jfit(5) = 1
               call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 3,
     +                          kp, ftlim, htlim, damp, rinval, map,
     +                          mx, my, magnif, domap, chisq )
            endif
            if ( .not.tfixangle ) then					!Then get angle and fix
               jfit(9) = 1						! it, if doing angle
               call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 4,
     +                          kp, ftlim, htlim, damp, rinval, map,
     +                          mx, my, magnif, domap, chisq )
               jfit(9) = -1
            endif

            if ( .not.tfixp(4) .or. .not.tfixp(5) ) then		!Then let power radial change
               if ( .not.tfixp(4) ) jfit(7) = 1
               if ( .not.tfixp(5) ) jfit(8) = 1
               call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 3,
     +                          kp, ftlim, htlim, damp, rinval, map,
     +                          mx, my, magnif, domap, chisq )
            endif

            if ( .not.tfixangle ) then					!Then get angle and fix
               jfit(9) = 1						! it again, if doing angle
               jfit(7) = -1
               jfit(8) = -1
               call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, 4,
     +                          kp, ftlim, htlim, damp, rinval, map,
     +                          mx, my, magnif, domap, chisq )
               jfit(9) = -1
               if ( .not.tfixp(4) .or. .not.tfixp(5) ) then		!Then re-let power radial change
                  if ( .not.tfixp(4) ) jfit(7) = 1
                  if ( .not.tfixp(5) ) jfit(8) = 1
              endif
            endif

         endif

         if ( .not.tfixp(3) ) jfit(6) = 1				!Then do full fit including
         call pr_profit ( data, kx, ky, cc, jfit, nstar, iter, itslim,	! profile power, but angle  fixed
     +                    kp, ftlim, htlim, damp, rinval, map, mx,
     +                    my, magnif, domap, chisq )

         if ( .not.tfixangle ) call pr_sortangle ( cc )			!Put angle in standard way

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SORTANGLE -- Put angle (and major/minor axes) in standard way
C
C   alan penny                  ral          1991 July

      subroutine pr_sortangle ( cc )

      implicit none

      real	cc(9)		!i/o: Profile parameters
C--
      real rv, pi, pih
      parameter ( pi=3.14159265 )
      parameter ( pih=pi/2.0 )
Cbegin

      rv = cc(9)							!Get angle in one of the
      if ( rv.gt.pih .and. rv.le.pi ) then				! two right quadrants
         cc(9) = cc(9) - pi
      elseif ( rv.gt.pi .and. rv.le.(pi+pih) ) then
         cc(9) = cc(9) - pi
      elseif ( rv.gt.(pi+pih) ) then
         cc(9) = cc(9) - 2.0*pi
      elseif ( rv.lt.-1.0*pih .and. rv.ge.-1.0*pi ) then
         cc(9) = cc(9) + pi
      elseif ( rv.lt.-1.0*pi .and. rv.ge.-1.0*(pi+pih) ) then
         cc(9) = cc(9) + pi
      elseif ( rv.lt.-1.0*(pih+pi) ) then
         cc(9) = cc(9) + 2.0*pi
      endif

      if ( cc(4).lt.cc(5) ) then					!Get first parameter the major axis
         rv = cc(4)
         cc(4) = cc(5)
         cc(5) = rv
         rv = cc(7)
         cc(7) = cc(8)
         cc(8) = rv
         if ( cc(9).lt.0.0 ) then
            cc(9) = cc(9) + pih
         else
            cc(9) = cc(9) - pih
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PROFIT -- Fits a variable Lorentz to an array
C The data in an array are fitted by a model of a sloping background and up
C to 8 2-D Lorentzian profiled stars. There is also an empirical residuals
C map which is fitted.
C
C The variables are the star profile (RX,RY,P,PRX,PRY,THETA);
C the sloping background, and the star positions and heights.
C
C
C      The Lorentzian profile is :-
C
C
C                  I  =      1
C                       _______________
C                               P(1+d2)
C                          1 + d1
C
C
C                                  where d1 = sqrt(dmaj1*dmaj1 + dmin1*dmin1)
C
C                                    and d2 = sqrt(dmaj2*damj2 + dmin2*dmin2)
C
C
C
C                                    with dmaj1 = dmaj/RMAJ
C                                         dmin1 = dmin/RMIN
C
C                                     and dmaj2 = dmaj/PRMAJ
C                                         dmin2 = dmin/PRMIN
C
c                       with  dmaj =  (x-X0)cos(theta) + (y-Y0)sin(theta)
C                             dmin = -(x-X0)sin(theta) + (y-Y0)cos(theta)
C
C      The background is fitted as I = A.X + B.Y + C
C
C      The Y values are input as from bottom to top and output
C      the same.
C
C      The input profile and star heights and positions and the
C      background must be close to the real values for the fit to
C      work.
C
C    alan penny                  ral				1991 July

      subroutine pr_profit ( data, kx, ky, ccc, jjfit, nsfit, iter,
     +                       itslim, kp, ftlim, htlim, damp, rinval,
     +                       map, mx, my, magnif, domap, chisq )

      implicit none

      integer	kx		!i: X size of DATA
      integer	ky		!i: Y size of DATA
      real	data(kx,ky)	!i: data
      real	ccc(36)		!i/o: Fit parameters
				!      background (1,2,3)
				!      profile (4,5,6,7,8,9,10,11,12)
				!     star posns, heights (13,14,...,36)
      integer	jjfit(36)	!i: -1= par fixed;0=not used;1=variable
      integer	nsfit		!i: No of stars
      integer	iter		!o: No of iterations taken
      integer	itslim		!i: Max no of iterations allowed
      integer	kp		!i: Printing flag
      real      ftlim		!i: Fit params fractional error for convergence
      real      htlim		!i: Fit height fractional error for convergence
      real      damp		!i: Fit damping factor
      real      rinval		!i: INVALID pixel magic value
      integer   mx		!i: X size of profile map
      integer   my		!i: Y size of profile map
      real      map(mx,my,1)	!i: Profile map
      integer   magnif		!i: Magnification of profile map
      logical   domap		!i: Flag for using profile map
      real      chisq		!o: Chi-squared at end of fit
C--
      real cmat(1296)		!normal eqn matrix
      real cvec(36)		!normal eqn vector
      real rvec(36)		!normal eqn solution

      real cc(36), a, b, c, gx, gy, p, hx, hy, theta, qh, qr, qp
      equivalence (cc(1),a),  (cc(2),b),     (cc(3),c),
     +            (cc(4),gx), (cc(5),gy),    (cc(6),p),   (cc(7),hx),
     +            (cc(8),hy), (cc(9),theta), (cc(10),qh), (cc(11),qr),
     +            (cc(12),qp)

      integer jfit(36), jf1, jf2, jf3, jf4, jf5, jf6,
     +        jf7, jf8, jf9, jf10, jf11, jf12
      equivalence (jfit(1),jf1),   (jfit(2),jf2), (jfit(3),jf3),
     +            (jfit(4),jf4),   (jfit(5),jf5), (jfit(6),jf6),
     +            (jfit(7),jf7),   (jfit(8),jf8), (jfit(9),jf9),
     +            (jfit(10),jf10), (jfit(11),jf11),
     +            (jfit(12),jf12)

      real erf(36)							!Error functions
      real erfd(36), ccs(36), cclast(36), aht(8), axt(8), ayt(8)
      integer kdone(8)

      integer mstars							!Max poss no of stars
      integer nstars							!Actual no of stars
      integer nstar							!Current star
      integer npf							!No of free parameters

      real    ochisq			!previous chi-squared sum
      real    dchi			!fractional change in chi-squared
      real    var			!'varience' of a pixel
      real    psum			!No of pixels used

      integer j, k, kk, jk, kj, kjo, npfa, jfx, jfy, jfi, jx, jy, llx,
     +        lly, nac2, nac, nac0, n, kdo, imi, iminac, npsize, lxa,
     +        lya, npixel
      real rx, ry, co, si, sim, gx2, gy2, hx2, hy2, a0, alxly, x,
     +     y, z, zd, xdm, ydm, dx, dy, dxaa, dyaa, resval, dfdx, dfdy,
     +     afact, zfac(12), zv, vres, factor, diff, adiff, fdiff, rv,
     +     pout(9), ax, ay, adr, acc(3)
      logical loop, again, tdomap
      character*100 text
CX    integer jjk

      real rinter, trunc
      external rinter
      external trunc
      real pi
      parameter ( pi=3.14159265 )
Cbegin


      call amovr ( ccc, cc, 36 )					!Translate CCC and JJFIT from outside
      call amovi ( jjfit, jfit, 36 )

      cc(4) = max(0.01,abs(cc(4)))					!Check input profile parameters
      cc(5) = max(0.01,abs(cc(5)))					! are in possible range
      cc(7) = max(0.01,abs(cc(7)))
      cc(8) = max(0.01,abs(cc(8)))
      if ( abs(theta).lt.0.000005 ) theta = sign(0.000005,theta)
      if ( abs(theta).gt.2.0*pi ) theta =
     +                               sign(amod(abs(theta),2.0*pi),theta)
      cc(10) = abs(cc(10))
      cc(11) = abs(cc(11))
      cc(11) = max(0.1,cc(11))
      cc(12) = max(0.1,cc(12))

      do k = 1, nsfit							!Check input heights are all positive
         if ( jfit(12+k*3).eq.1 ) cc(12+k*3) = max(0.1,cc(12+k*3))
      enddo

      cc(4) = 1.0/cc(4)							!Invert the profile radii
      cc(5) = 1.0/cc(5)							! to match s/r convention
      cc(7) = 1.0/cc(7)
      cc(8) = 1.0/cc(8)
      cc(11) = 1.0/cc(11)

      call amovr ( cc, ccs, 36 )					!Store the input parameters
      call amovr ( cc, cclast, 36 )					!Store the initial parameters

      mstars = 8							!No of stars
      nstars = nsfit
      nstars = min(nstars,mstars)
      npsize = ky*kx							!Total no of pixels
      npfa = 3*mstars+12						!Max no of free parameters

      gx2 = gx*gx							!Squares of the 4 radii
      gy2 = gy*gy
      hx2 = hx*hx
      hy2 = hy*hy

      co = cos(theta)							!Angle parameters
      si = sin(theta)
      sim = -1.0*si

      if ( kp.eq.1 ) then						!Type out heights, posns if wanted
         do k = 1, nstars
            axt(k) = trunc(cc(13+(k-1)*3),5)
            ayt(k) = trunc(cc(14+(k-1)*3),5)
            aht(k) = trunc(cc(15+(k-1)*3),6)
         enddo
         acc(1) = trunc(cc(1),6)
         acc(2) = trunc(cc(2),6)
         acc(3) = trunc(cc(3),6)
         k = 1
         write ( text, '(1x,''Start '',i3,1x,f9.1,2f7.2,4x,f12.4,
     +                   2f8.4)')
     +           k, aht(1), axt(1), ayt(1), acc(1), acc(2), acc(3)
         call printo ( text )
         if ( nstars.ge.2 ) then
            do k = 2, nstars
               write ( text, '(1x,6x,i3,1x,f9.1,2f7.2)' ) k, aht(k),
     +                                               axt(k), ayt(k)
               call printo ( text )
            enddo
         endif
      endif

      npf = 0								!Count the number of free parameters
      do k = 1, 36
         if ( jfit(k).eq.1 ) npf = npf + 1
      enddo

      tdomap = domap							!See if the profile residuals
      if ( tdomap ) call rchzero ( map, mx, my, 1, 1, mx, my, tdomap )	! are to be used

      iter = 0								!End of initialization and
      loop = .true.							! data definition: start of
      do while ( loop )							! iteration loop

CX      if ( kp.eq.1 ) then
CX         write ( text, '(1x,''jj '',30i3)' ) (jfit(k),k=1,30)
CX         call printo ( text )
CX      endif

      call azeror ( cmat, 1296 )					!Zeroise the normal eqns
      call azeror ( cvec, 36 )

      chisq = 0.0							!Chi-squared start

      rx = 1.0/gx							!Loop through the pixels,
      ry = 1.0/gy							! setting up the simultaenous equations
      call subdiv ( rx, ry, lxa )
      lya = lxa
      alxly = real(lxa*lya)
      psum = 0.0
      do npixel = 1, npsize

         jy = 1 + (npixel-1)/kx						!Find pixel and if a valid one
         jx = npixel - (kx*(jy-1))
         if ( data(jx,jy).ne.rinval ) then
            psum = psum + 1.0
            x = jx
            y = jy
            nac = 0

            if ( jf1.eq.1 ) then					!Do A, B, and C
               nac = nac + 1
               erf(nac) = 1.0
            endif
            if ( jf2.eq.1 ) then
               nac = nac + 1
               erf(nac) = x
            endif
            if ( jf3.eq.1 ) then
               nac = nac + 1
               erf(nac) = y
            endif
									!Accumulate profile, error
									! functions over all stars

            call azeror ( erf(nac+1), 12-(nac+1)+1 )			!Clear the functions first

            z = a + b*x + c*y						!Accumulate the functions

            call azeror ( erfd(nac+1), 36-(nac+1)+1 )			!Do over the small boxes
            call azeroi ( kdone, nstars )
            zd = 0.0
            do lly = 1, lya
            do llx = 1, lxa
            xdm = -0.5 + ((0.5+real(llx)-1.0)/real(lxa))
            ydm = -0.5 + ((0.5+real(lly)-1.0)/real(lya))
            nac2 = 0
            do nstar = 1, nstars
               nac0 = nac
               n = 3*nstar + 10
               jfx = jfit(n)
               jfy = jfit(n+1)
               jfi = jfit(n+2)

               if ( jfx.ne.0 ) then					!If there is a star
                  dx = x - cc(n)
                  dy = y - cc(n+1)

                  if ( tdomap .and. kdone(nstar).eq.0 ) then
                     dxaa = mx/2 + dx*magnif
                     dyaa = my/2 + dy*magnif
                     resval = rinter ( map, mx, my, 1, 1, mx,
     +                                 my, dxaa, dyaa, dfdx, dfdy )
                     dfdx = dfdx*real(magnif)
                     dfdy = dfdy*real(magnif)
                  endif

                  if ( abs(dx).gt.4.0 .or. abs(dy).gt.4.0 ) then
                     if ( kdone(nstar).eq.0 ) then
                        kdo = 1
                        afact = alxly
                     else
                        kdo = 0
                        if ( nstar.eq.1 ) then
                           nac2 = nac
                           do k = 4, 12
                              nac2 = nac2 + max(0,jfit(k))
                           enddo
                        endif
                        if ( jfx.eq.1 ) nac2 = nac2 + 1
                        if ( jfy.eq.1 ) nac2 = nac2 + 1
                        if ( jfi.eq.1 ) nac2 = nac2 + 1
                     endif
                  else
                     kdo = 1
                     afact = 1.0
                     dx = x + xdm - cc(n)
                     dy = y + ydm - cc(n+1)
                  endif

                  kdone(nstar) = 1

                  if ( kdo.eq.1 ) then

                    if ( abs(dx).lt.0.0001 ) dx = sign(0.0001,dx)
                    if ( abs(dy).lt.0.0001 ) dy = sign(0.0001,dy)

                    a0 = cc(n+2)
                    call pr_fitpar ( a0, dx, dy, gx2, gy2, p, hx2,
     +                               hy2, hx, hy, gx, gy, co, si, sim,
     +                               qh, qr, qp, jfit, resval, dfdx,
     +                               dfdy, afact, tdomap, nstar, zfac,
     +                               zv )


CX         if ( kp.eq.1 ) then
CX            write (text,'(1x,''mm1 '',6g15.6)') a0,dx,dy,gx2,gy2,p
CX            call printo ( text )
CX            write (text,'(1x,''mm2 '',6g15.6)') hx2,hx,hy,gx,gy,co
CX            call printo ( text )
CX            write (text,'(1x,''mm3 '',6g15.6)') si,sim,qh,qr,qp,resval
CX            call printo ( text )
CX            write (text,'(1x,''mm4 '',2g15.6)') dfdx,dfdy
CX            call printo ( text )
CX            call azeror ( axt, 8 )
CX            do jk = 1, 8
CX               axt(jk) = trunc(zfac(jk),1)
CX            enddo
CX            write ( text, '(1x,''ma '',3i5,2x,8f9.6)' ) nstar,
CX     +                           nint(x), nint(y), (axt(jk),jk=1,8)
CX            call printo ( text )
CX            call azeror ( axt, 8 )
CX            do jk = 1, 4
CX               axt(jk) = trunc(zfac(jk+8),1)
CX            enddo
CX            write ( text, '(1x,''mb '',15x,2x,4f9.6)' )
CX     +                           (axt(jk),jk=1,4)
CX            call printo ( text )
CX         endif

                    if ( jf4.eq.1 ) then				!GX
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(1)
                    endif

                    if ( jf5.eq.1 ) then				!GY
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(2)
                    endif

                    if ( jf6.eq.1 ) then				!HX
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(3)
                    endif

                    if ( jf7.eq.1 ) then				!HY
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(4)
                    endif

                    if ( jf8.eq.1 ) then				!P
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(5)
                    endif

                    if ( jf9.eq.1 ) then				!Angle
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(6)
                    endif

                    if ( jf10.eq.1 ) then				!Q ratio
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(7)
                    endif

                    if ( jf11.eq.1 ) then		       		!Q radius
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(8)
                    endif

                    if ( jf12.eq.1 ) then				!Q power
                       nac0 = nac0 + 1
                       erfd(nac0) = erfd(nac0) + zfac(9)
                    endif

                    if ( nac2.eq.0 ) nac2 = nac0			!Error functions for position
                    if ( jfx.eq.1 ) then				! and intensity for each star
                       nac2 = nac2 + 1
                       erfd(nac2) = erfd(nac2) + zfac(10)
                    endif
                    if ( jfy.eq.1 ) then
                       nac2 = nac2 + 1
                       erfd(nac2) = erfd(nac2) + zfac(11)
                    endif
                    if ( jfi.eq.1 ) then
                       nac2 = nac2 + 1
                       erfd(nac2) = erfd(nac2) + zfac(12)
                    endif

                    zd = zd + zv					!Calc height of fit here

                  endif
               endif
            enddo

            enddo
            enddo

            do k = nac+1, 36						!Average the small squares at this point
               erf(k) = erfd(k)/alxly
            enddo
            z = z + (zd/alxly)

CX         if ( kp.eq.1 ) then
CX            call azeror ( axt, 8 )
CX            do jk = 1, min(8,npf)
CX               axt(jk) = trunc(erf(jk),4)
CX            enddo
CX            ayt(1) = trunc(z,4)
CX            write ( text, '(1x,''l '',i5,f10.3,2x,8f9.3)' )
CX     +                                npixel,ayt(1),(axt(jk),jk=1,8)
CX            call printo ( text )
CX         endif

            vres = data(jx,jy) - z					!Get residual from data
            vres = min(1.0e8,max(-1.0e8,vres))
            if ( abs(vres).lt.1.0e-30 ) vres = 0.0

            var = abs(data(jx,jy)) 					!Chi-squared
            if ( var.gt.1.0e-7 ) chisq = chisq + vres*vres/var

            if ( abs(vres).gt.1.0e10 ) vres = 1.0e10*sign(1.0,vres)	!Accumulate vector and matrix
            if ( abs(vres).lt.1.0e-10 ) vres = 1.0e-10*sign(1.0,vres)	! contributions at this pixel
            do k = 1, npf
             if (abs(erf(k)).gt.1.0e10) erf(k) = 1.0e10*sign(1.0,erf(k))
             if (abs(erf(k)).lt.1.0e-10) erf(k)=1.0e-10*sign(1.0,erf(k))
            enddo
            if ( npf.ge.1 ) then
               do k = 1, npf
                  cvec(k) = cvec(k) + erf(k)*vres
                  kjo = (k-1)*npf
                  do j = k, npf
                     kj = kjo+j
                     cmat(kj) = dble(cmat(kj))+dble(erf(j))*dble(erf(k))
                  enddo
               enddo
            endif

         endif
      enddo

      if ( npf.ne.0 ) then						!Complete matrix
         if ( npf.ge.2 ) then
            do k = 2, npf
               imi = k - 1
               iminac = imi*npf
               do j = 1, imi
                  kj = iminac + j
                  jk = (j-1)*npf + k
                  cmat(kj) = cmat(jk)
               enddo
            enddo
         endif

         factor = 1.0 + damp*damp					!Apply damping factor
         do k = 1, npf
            kk = (k-1)*npf + k
            cmat(kk) = cmat(kk)*factor
         enddo

CX         if ( kp.eq.1 ) then
CX            do k = 1, npf
CX               kk = (k-1)*npf
CX               call azeror ( axt, 8 )
CX               jjk = min(8,npf)
CX               do jk = 1, jjk
CX                  axt(jk) = trunc(cmat(kk+jk),5)
CX               enddo
CX               ayt(1) = trunc(cvec(k),6)
CX               write ( text, '(1x,''k '',f11.3,2x,8f10.3)' ) ayt(1),
CX     +                                            (axt(jk),jk=1,jjk)
CX               call printo ( text )
CX            enddo
CX         endif

         call simulx ( cvec, cmat, rvec, npf )				!Solve the normal equations

         nac = 0							!Update the solution
         do k = 1, npfa
            if ( jfit(k).eq.1 ) then
               nac = nac + 1
               rv = rvec(nac)
               if (k.ge.4 .and. k.le.8 .and. abs(cc(k)).gt.1.0e-8)then	! (Only let profile parameters
                  diff = rv/cc(k)					! change by < 20%
                  adiff = 0.2*abs(cc(k))
                  if ( abs(diff).gt.adiff ) rv = sign(adiff,rv)
               endif
               if ( k.gt.12 .and. mod((k-13),3).le.1 ) then		!Let posn change by < 0.2 pixels
                  if ( abs(rv).gt.0.2 ) rv = sign(0.2,rv)
               endif
               cc(k) = cc(k) + rv
            endif
         enddo
         cc(4) = abs(cc(4))
         cc(5) = abs(cc(5))
         cc(7) = abs(cc(7))
         cc(8) = abs(cc(8))
         cc(11) = abs(cc(11))

         gx2 = gx*gx							!Adjust dependent parameters
         gy2 = gy*gy
         hx2 = hx*hx
         hy2 = hy*hy
         si = sin(theta)
         co = cos(theta)
         sim = -1.0*si

      endif

      again=.false.							!Compare new variable parameters
      do k = 1, 36							! with the old ones.
         if ( jfit(k).eq.1 ) then					! Only do variables

            if ( cclast(k).lt.1.0e-10 ) cclast(k) = 1.0e-10		!Change
            diff = abs(cc(k)-cclast(k))
            fdiff = diff/cclast(k)

            if ( k.ge.4 .and. k.le.12 .and. k.ne.9 ) then		!Profile
               if ( fdiff.gt.ftlim ) again = .true.
            endif
            if ( k.eq.9 .and. diff.gt.0.002 ) again = .true.

            if ( k.gt.12 ) then						!Posn + Height
               kk = mod((k-13),3)
               if ( kk.eq.0 .and. diff.gt.0.01   ) again = .true.
               if ( kk.eq.1 .and. diff.gt.0.01   ) again = .true.
               if ( kk.eq.2 .and. fdiff.gt.htlim ) again = .true.
            endif
         endif

         cclast(k) = cc(k)
      enddo

      if (  cc(4).gt.10.0    .or.  cc(4).lt.0.01    .or.		!Stop if profile has
     +      cc(5).gt.10.0    .or.  cc(5).lt.0.01    .or.		! become impossible
     +      cc(6).gt.10.0    .or.  cc(6).lt.0.1     .or.
     +      cc(7).gt.10.0    .or.  cc(7).lt.0.001   .or.
     +      cc(8).gt.10.0    .or.  cc(8).lt.0.001   .or.
     +     cc(10).gt.1000.0  .or. cc(10).lt.-1000.0 .or.
     +     cc(11).gt.1000.0  .or. cc(11).lt.0.001   .or.
     +     cc(12).gt.10.0    .or. cc(12).lt.0.1      ) then
         again = .false.
         call amovr ( cclast(4), cc(4), 12 )
      endif

      chisq = chisq/max(1.0,(psum-real(npf)))
      if ( iter.ne.0 ) then						!Check chi-sq change
         dchi = (chisq-ochisq)/max(0.000001,ochisq)
         if ( abs(dchi).lt.0.0001 ) again = .false.			!Chi-sq change v small
      endif
      ochisq = chisq

      if ( kp.eq.1 ) then						!Type out heights, posns if wanted
         do k = 1, nstars
            axt(k) = trunc(cc(13+(k-1)*3),5)
            ayt(k) = trunc(cc(14+(k-1)*3),5)
            aht(k) = trunc(cc(15+(k-1)*3),6)
         enddo
         acc(1) = trunc(cc(1),6)
         acc(2) = trunc(cc(2),6)
         acc(3) = trunc(cc(3),6)
         k = iter + 1
         write ( text, '(1x,6x,i3,1x,f9.1,2f7.2,4x,f12.4,2f8.4)' )
     +         k, aht(1), axt(1), ayt(1), acc(1), acc(2), acc(3)
         call printo ( text )
         if ( nstars.ge.2 ) then
            do k = 2, nstars
               write ( text, '(1x,6x,i3,1x,f9.1,2f7.2)' ) k,aht(k),
     +                                                axt(k), ayt(k)
               call printo ( text )
            enddo
         endif
      endif

      if ( kp.eq.1 ) then						!Type out profile if wanted and varaible
         kk = 0
         do k = 4, 12
            if ( jfit(k).eq.1 ) kk = 1
         enddo
         if ( kk.eq.1 ) then
            call amovr ( cc(4), pout, 9 )
            pout(1) = 1.0/pout(1)
            pout(2) = 1.0/pout(2)
            pout(4) = 1.0/pout(4)
            pout(5) = 1.0/pout(5)
            pout(6) = pout(6)*(180.0/pi)
            pout(8) = 1.0/pout(8)
            pout(1) = trunc(pout(1),2)
            pout(2) = trunc(pout(2),2)
            pout(3) = trunc(pout(3),1)
            pout(4) = trunc(pout(4),3)
            pout(5) = trunc(pout(5),3)
            pout(6) = trunc(pout(6),4)
            pout(7) = trunc(pout(7),3)
            pout(8) = trunc(pout(8),4)
            pout(9) = trunc(pout(9),1)
            write ( text, '(1x,1x,f5.3,f7.2,f7.2,'' :'',
     +                      f6.3,f8.3,f7.3,f8.2,f8.2,f8.2)' )
     +        (pout(j),j=7,9), (pout(j),j=1,6)
            call printo ( text )
         endif
      endif

      adr = 0.5*((1.0/abs(cc(4)))+(1.0/abs(cc(5))))			!Exit if main star has
      if (  abs(cc(13)-ccs(13)).gt.2.0*adr .or.				! moved too far or is too small
     +      abs(cc(14)-ccs(14)).gt.2.0*adr .or.
     +      cc(15).le.0.1 ) then
         again = .false.
         call amovr ( cclast(4), cc(4), 12 )
      endif

      if ( nstars.ge.2 ) then						!Remove comp star if moved
         do k = 2, nstars						! too far or height too small
            j = 13 + (k-1)*3
            ax = cc(j) - ccs(j)
            ay = cc(j+1) - ccs(j+1)
            adr = 0.5*((1.0/abs(cc(4)))+(1.0/abs(cc(5))))
            if ( abs(ax).gt.2.0*adr .or. abs(ay).gt.2.0*adr .or.
     +           cc(j+2).lt.0.1  ) then
               if ( jfit(j).eq.1 )   npf = npf - 1
               if ( jfit(j+1).eq.1 ) npf = npf - 1
               if ( jfit(j+2).eq.1 ) npf = npf - 1
               cc(j) = ccs(j)
               cc(j+1) = ccs(j+1)
               cc(j+2) = 0.0
               jfit(j) = 0
               jfit(j+1) = 0
               jfit(j+2) = 0
            endif
         enddo
      endif

      if ( nstars.ge.2 ) then						!Fix comp star posn if too far
         do k = 2, nstars						! outside box
            j = 13 + (k-1)*3
            ax = cc(j)
            ay = cc(j+1)
            adr = 0.5*((1.0/abs(cc(4)))+(1.0/abs(cc(5))))
            if ( ax.lt.(1.0-adr) .or. ax.gt.(real(kx)+adr) .or.
     +           ay.lt.(1.0-adr) .or. ay.gt.(real(ky)+adr) ) then
               if ( jfit(j).eq.1 )   npf = npf - 1
               if ( jfit(j+1).eq.1 ) npf = npf - 1
               jfit(j) = -1
               jfit(j+1) = -1
            endif
         enddo
      endif

      iter = iter + 1							!Loop again if star heights
      if ( .not.again .or. iter.ge.itslim ) loop = .false.		! changing and done less than limit

      enddo

      call amovr ( cc, ccc, 36 )					!Put back to outside
      call amovi ( jfit, jjfit, 36 )

      ccc(4) = 1.0/ccc(4)						!Restore radii convention
      ccc(5) = 1.0/ccc(5)
      ccc(7) = 1.0/ccc(7)
      ccc(8) = 1.0/ccc(8)
      ccc(11) = 1.0/abs(ccc(11))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FITPAR -- Calc normal eqn values for this star at this posn
C
C   alan penny                 ral                      1991 July

      subroutine pr_fitpar ( a0, dx, dy, gx2, gy2, p, hx2, hy2,
     +                       hx, hy, gx, gy, co, si, sim,
     +                       qh, qr, qp, jfit, resval, dfdx, dfdy,
     +                       afact, tdomap, nstar, zfac, zv )

      implicit none

      real	a0		!i: Height
      real	dx		!i: Offset along X axis
      real	dy		!i: Offset along Y axis
      real	gx2		!i: 1/rx sq
      real	gy2		!i: 1/ry sq
      real	p		!i: power
      real	hx2		!i: 1/prx sq
      real	hy2		!i: 1/pry sq
      real	hx		!i: 1/rx
      real	hy		!i: 1/ry
      real	gx		!i: 1/prx
      real	gy		!i: 1/pry
      real	co		!i: Cosine of profile angle
      real	si		!i: Sine of profile angle
      real	sim		!i: -1*Sine of profile angle
      real	qr		!i: Gauss radius
      real	qh		!i: Gauss fractional height
      real	qp		!i: Gauss power
      integer	jfit(36)	!i: control for doing parameters
      real	resval		!i: profile map value
      real	dfdx		!i: profile map X slope
      real	dfdy		!i: profile map Y slope
      logical	tdomap		!i: Flag whether to use map
      real	afact		!i: pixel subdivision correction
      integer	nstar		!i: star being done
      real	zfac(12)	!o: normal eqn factors
      real	zv		!o: star height here
C--
      real gg, hh, h, hp1, alpha, aon2, alg2, alg, algam2,
     +     g2am2, g2a, f4, f4s, f9, qd, qdd, qa, qt, f6, ggalg, f7, f8,
     +     ddthg, ddthh, ggagph, ddth, ddxa, ddxb, ddxc, ddya, ddyb,
     +     ddyc, dmaj, dmin, dmaj2, dmin2
      integer jf45, jf78, jf6789, jfx, jfy, jfxy, jfi, n
Cbegin


      jf45 = max(jfit(4),jfit(5))					!Set control params
      jf78 = max(jfit(7),jfit(8))
      jf6789 = max(jfit(6),jf78,jfit(9))
      n = 3*nstar + 10
      jfx = jfit(n)
      jfy = jfit(n+1)
      jfi = jfit(n+2)
      jfxy = max(jfx,jfy)

      dmaj = dx*co + dy*si
      dmin = dx*sim + dy*co
      if ( abs(dmaj).lt.0.0001 ) dmaj = sign(0.0001,dmaj)
      if ( abs(dmin).lt.0.0001 ) dmin = sign(0.0001,dmin)
      dmaj2 = dmaj*dmaj							!Calc factors
      dmin2 = dmin*dmin
      gg = dmaj2*gx2 + dmin2*gy2
      hh = dmaj2*hx2 + dmin2*hy2
      h = sqrt(hh)
      hp1 = h + 1.0
      alpha = p*hp1
      aon2 = alpha/2.0
      alg2 = alog(gg)
      alg = alg2/2.0
      algam2 = alg2*(aon2-1.0)
      if ( abs(algam2).lt.60.0 ) then
         g2am2 = exp(algam2)
      else
         g2am2 = 0.0
      endif
      g2a = g2am2*gg
      if ( algam2.lt.60 ) then
         f4 = 1.0/(1.0+g2a)
         if (f4.lt.1.0e-10) f4 = 0.0
      else
         f4 = 0.0
      endif
      f4s = f4*f4
      f9  = a0*f4s*g2am2
      if ( qh.ne.0.0 ) then
         qd  = sqrt(dmaj2+dmin2)
         qdd = (qd*qr)**qp
         if ( qdd.gt.12.0 ) qdd = 12.0
         qa  = exp(-1.0*qdd)
         qt  = qp*qh*a0*qa*(qr**qp)*(qd**(qp-2.0))
      else
         qa = 0.0
         qt = 0.0
         qd = 1.0
         qdd = 1.0
      endif

      if ( jf45.eq.1 ) then						!Do GX and GY
         f6 = alpha*f9
         if ( jfit(4).eq.1 ) zfac(1) = -1.0*f6*dmaj2*gx*afact
         if ( jfit(5).eq.1 ) zfac(2) = -1.0*f6*dmin2*gy*afact
      endif

      if ( jf6789.eq.1.or.jfxy.eq.1 ) then				!Do HX, HY, and P
         ggalg = gg*alg
         f7 = f9*ggalg
         f8 = f7
         if ( jfit(6).eq.1 ) zfac(3) = -1.0*f8*hp1*afact
         if ( jf78.eq.1 ) then
            f7 = f7*p/h
            if ( jfit(7).eq.1 ) zfac(4) = -1.0*f7*dmaj2*hx*afact
            if ( jfit(8).eq.1 ) zfac(5) = -1.0*f7*dmin2*hy*afact
         endif
      endif

      if ( jfit(9).eq.1 ) then						!Do the Angle
         ddthg = dx*gx2*dmin - dy*gy2*dmaj
         ddthh = dx*hx2*dmin - dy*hy2*dmaj
         ggagph = ggalg*p/h
         ddth = -1.0*f9*(alpha*ddthg+ggagph*ddthh)
         zfac(6) = ddth*afact
      endif

      if ( jfit(10).eq.1 ) zfac(7) = qa*a0*afact 			!Do the Q ratio

      if ( jfit(11).eq.1 ) zfac(8) = -1.0*(qt*qd*qd/qr)*afact 		!Do the Q radius

      if ( jfit(12).eq.1 ) zfac(9) = qh*a0*qa*qdd*alog(qd*qr)*afact	!Do the Q power

      if ( jfxy.eq.1 ) then						!Error functions for position
         ggagph = ggalg*p/h						! and intensity for each star
         if ( jfx.eq.1 ) then
            ddxa = gx2*co*dmaj + gy2*sim*dmin
            ddxb = hx2*co*dmaj + hy2*sim*dmin
            ddxc = f9*(alpha*ddxa+ggagph*ddxb)
            ddxc = ddxc + qt*dx
            zfac(10) = ddxc*afact
            if ( tdomap ) zfac(10) = zfac(10) - a0*dfdx
         endif
         if ( jfy.eq.1 ) then
            ddya = gx2*si*dmaj + gy2*co*dmin
            ddyb = hx2*si*dmaj + hy2*co*dmin
            ddyc = f9*(alpha*ddya+ggagph*ddyb)
            ddyc = ddyc + qt*dy
            zfac(11) = ddyc*afact
            if ( tdomap ) zfac(11) = zfac(11) - a0*dfdy
         endif
      endif
      if ( jfi.eq.1 ) then
         zfac(12) = (f4+qa*qh)*afact
         if ( tdomap ) zfac(12) = zfac(12) + resval
      endif

      zv = a0*(f4+qa*qh)*afact						!Calc height of fit here
      if ( tdomap ) zv = zv + a0*resval


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CCSUB -- Remove stars from an array, using info loaded into CC
C
C   alan penny                   ral                    1991 July


      subroutine pr_ccsub ( data, kx, ky, cc, nst, map, mx, my,
     +                      magnif, domap, rinval )

      implicit none

      integer  kx		!i: X size of data array
      integer  ky		!i: Y size of data array
      real     data(kx,ky)	!i/o: Data array
      real     cc(36)		!i: Profile and posns
      integer  nst		!i: No of stars in CC
      integer   mx		!i: X size of profile map
      integer   my		!i: Y size of profile map
      real      map(mx,my,1)	!i: Profile map
      integer   magnif		!i: Magnification of profile map
      logical   domap		!i: Flag for using profile map
      real      rinval		!i: INVALID flag pixel magic value
C--
      integer k, kww(4), ierr
      real x, y, h, prof(9)
Cbegin


      if ( nst.lt.1 ) return

      call amovr ( cc(4), prof, 9 )

      do k = 1, min(8,nst)
         x = cc(10+3*k)
         y = cc(11+3*k)
         h = cc(12+3*k)
         call popsmr ( data, kx, ky, 1.0, rinval, x, y, h, prof,
     +                 map, mx, my, 1, 1, mx, my, magnif, domap,
     +                 ierr, kww )
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  PROSUBG.FOR
C
C  It contains:-
C PR_DFINTER      Interact with display for 'display' and 'fits'
C PRG_OPT_SETUP   Set up option choices
C PR_DISRESA      Display an array in a displaced position
C PR_PAINTBOX     Paint boxes round companion stars
C PR_BOXPOS       Calculate the screen position of a box
C PR_ABOXPOS      Calculate the BLH screen position (upper and lower) of box
C PR_PAINTCOMP    Paint central stars and companion stars
C PR_GTCOMPA      Get companions stars
C PR_PRINTVAL     Get area and prints the values or the residuals in the area
C PR_UNCOMPA      Get companions to remove and remove painted spots
C PR_GTBOXA       Get an ignore area in a box
C PR_UNBOX        Remove an ignore area from a box
C PR_MAPINTER     Interact with the display for 'map'
C PRGA_OPT_SETUP  Set up option choices
C PR_RESINTER     Interact with the display for 'residuals'
C PRGB_OPT_SETUP  Set up option choices
C PR_GTBOXB       Get by cursor a box in the residuals/map area
C PR_PRAREA       Put an area of a real array out to terminal, mult by 10000
C PR_BOXFRA       Smooth a real array by a rectangular box
C PR_BOXFR        Smooth a real array by a rectangular box
C PR_DISRESB      Display two arrays (residuals/flag) or one (profile map)
C PR_PREJECT      Paint a rectangle round rejected boxes on the display
C PR_PAINTLIT     Paint ticks around area displays, showing the calc areas
C PR_GRID         Paint a frame in the area displays, showing the calc areas
C PR_POLFILL      Use the cursor to define a set of polygonal  shapes
C PR_FILL         Fill an array defined by a polygon
C PR_XSECT        Find intersections of polygon with this line
C PR_INSIDE       (Function) See if inside the polygon
C PR_RANGLE       (Function) Find the rotation angle
C PR_POLDO        Take a masks and zeroes/smooths an area of an array
C PR_INFILL       Fill in empty pixels in the array RESID, where an empty
C PR_GTCEN        Enable you to move the posn of the centre stars
C PR_GTNBOX       Find out which box a cursor was in. If in upper box,
C PR_VALSTARS     Find pixel value due to main + companion stars
C PR_DSCLEAR      Clear display if open
C PR_DSOPEN       Open display if not already open
C PR_LOADRINVAL   Load a Real 'INVALID' value into the display common area


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DFINTER --  Interact with display for 'display' and 'fits'
C
C   a.j.penny                   stsci                      86-10-27

      subroutine pr_dfinter ( tres, tcom, klx, kly, klxs, klys,
     +                        klxd, klyd, klxsd, klysd )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_PANEL_INC'

      real	tres(TBX,TBY)		!i/o: Fit results array
      real	tcom(6,MAXTOT)		!i/o: comps/bad areas array
      integer   klx			!i: X size of fitting area
      integer   kly			!i: Y size of fitting area
      integer   klxs(TBY)		!i: X posns of blh of fitting areas
      integer   klys(TBY)		!i: Y posns of blh of fitting areas
      integer	klxd			!i: X size of display areas
      integer	klyd			!i: Y size of display areas
      integer	klxsd(TBY)		!i: X posns of blh of display areas
      integer	klysd(TBY)		!i: Y posns of blh of display areas
C--
      logical loop
      character*12 ktopt
Cbegin


      call prg_opt_setup ( ktopt, 2, .true. )
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )

         call prg_opt_setup ( ktopt, 2, .false. )
         call get_choice ( ktopt, 1 )					!Get choice
         if ( ST_FAILED ) return

         if ( ktopt.eq.'comp_br' ) call pr_gtcompa ( 1, MAXCL, klxsd,
     +                                               klysd, klxd, klyd )

         if ( ktopt.eq.'comp_fa' ) call pr_gtcompa ( 2, MAXFA, klxsd,
     +                                               klysd, klxd, klyd )

         if ( ktopt.eq.'blank' ) call pr_gtboxa ( tcom, klxd, klyd,
     +                                     klxsd, klysd, tres )

         if ( ktopt.eq.'uncomp_br' ) call pr_uncompa ( 1, tcom, klxd,
     +                                    klyd, tres, klxsd, klysd )

         if ( ktopt.eq.'uncomp_fa' ) call pr_uncompa ( 2, tcom, klxd,
     +                                      klyd, tres, klxsd, klysd )

         if ( ktopt.eq.'unblank' ) call pr_unbox ( tcom, klxd, klyd,
     +                                             tres, klxsd, klysd )


         if ( ktopt.eq.'info_star' ) call pr_listcomp			!Type out star and companions

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )          !Set zoom, pan to null

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .false., 0, 0 )

         if ( ktopt.eq.'im_values' ) then
                          if ( IMTYPE.eq.'SHORT' ) then
                             call pr_printvals ( %val(IPIM), klxd,
     +                               klyd, klxsd, klysd, 1, tres, tcom )
                          else
                             call pr_printvalr ( %val(IPIM), klxd,
     +                               klyd, klxsd, klysd, 1, tres, tcom )
                          endif
                          endif

         if ( ktopt.eq.'res_values' ) then
                           if ( IMTYPE.eq.'SHORT' ) then
                              call pr_printvals ( %val(IPIM), klxd,
     +                               klyd, klxsd, klysd, 2, tres, tcom )
                           else
                              call pr_printvalr ( %val(IPIM), klxd,
     +                               klyd, klxsd, klysd, 2, tres, tcom )
                           endif
                           endif

         if ( ktopt.eq.'paint_comp' ) then
                           call pr_paintcomp ( klxd, klyd, tres, tcom,
     +                                         klxsd, klysd, 0 )
                           call pr_paintbox  ( klxd, klyd, tcom, klxsd,
     +                                         klysd, 0 )
                           endif

         if ( ktopt.eq.'paint_box' ) call pr_grid ( klx, kly, klxs, 	!Paint grid
     +                        klys, klxd, klyd, klxsd, klysd, TBY, 0 )

         if ( ktopt.eq.'box' ) then
                                DOGRID = .not.DOGRID
                                if ( DOGRID ) then
                                   call printo ( '   Grid now shown' )
                                else
                                   call printo ('   Grid now not shown')
                                endif
                                endif

         if ( ktopt.eq.'shift' ) call pr_gtcen ( klxd, klyd, tres,
     +                                           klxsd, klysd )

         if ( ktopt.eq.'clear' ) call pr_dsclear			!Clear display


         if ( ktopt.eq.'disp_all' ) call pr_dispfit ( 0, 1, klx, kly,	!Display all fits/areas
     +                                             klxs, klys, klxd,
     +                                             klyd, klxsd, klysd )

         if ( ktopt.eq.'disp_one' ) call pr_dispfit ( 1, 1, klx, kly, 	!Display one fit/area
     +                                             klxs, klys, klxd,
     +                                             klyd, klxsd, klysd )

         if ( ktopt.eq.'close' ) call pr_setd				!Close display

         if ( ktopt.eq.'posntype' ) call pr_posntype 			!Set posn display type

         if ( ktopt.eq.'return' ) loop = .false.

         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRG_OPT_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 Jan

      subroutine prg_opt_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside           !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=21 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'blank', 'Remove an area from the calculations',
     + 'A rectangle in the image near a fit star can be ignored. It ',
     + 'is chosen using either the image or the residuals picture for',
     + 'a star. Place cursor on the b.l.h. corner of rectangle, press',
     + 'the l.h. button. Repeat for the t.r.h corner. This area is now',
     + 'blanked out and ignored. Repeat as desired. End by pressing',
     + 'another button. (These can be output via the -OUTCOMPS- file.)'/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'clear', 'Clear the display window',
     + 'Clear the display window. This only affects the window. It',
     + 'does not affect any of the image or residuals, or profile. It ',
     + 'is used solely for tidying up a window that has got too ',
     + 'cluttered, and you wish to display some new stuff.',
     + ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'comp_br', 'Add a bright or close companion' ,
     + 'Make the display window active. Place the cursor in a box at a',
     + 'position, press the left hand button. A purple marker is put ',
     + 'up and a -bright- or -close- companion is entered in for that',
     + 'position. Repeat as desired. Press another button to end.',
     + 'These companions take full part in fitting, so they must be ',
     + 'either bright and fit easy, or close so cannot be done else.'/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'comp_fa', 'Add a faint or distant companion',
     + 'Make the display window active. Place the cursor in a box at a',
     + 'position, press the left hand button. A purple marker is put ',
     + 'ip and a -faint- or -far- companion is entered in for that ',
     + 'position. Repeat as desired. Press another button to end.',
     + 'These companions take a secondary part in the fitting, being',
     + 'fitted with fixed profiles in steps, to ease the fitting.' /


      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'box', 'Paint fitting box when displaying fits',
     + 'If the areas around the stars have been displayed, then ',
     + 'shall a rectangle round the actual area used in the fitting be',
     + 'displayed? Toggles between -yes- and -no-, with -yes- being ',
     + 'the starting default. ',
     + '(The area displayed has sides twice the fitting box. For wing',
     + 'fitting, the fitting and display boxes are twice that size.)' /

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'shift', 'Reposition a main star',
     + 'You can change the position of one -main- star with this.',
     + 'Look at the display, choose which -central- star you want to',
     + 'move, place the cursor at the desired new position (in either',
     + 'the raw or residuals display box), press the left-hand button.',
     + 'The new position is marked up. If you wish to see the effect',
     + 'of this, you can do the -disp_one- option for that star. '/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'paint_comp', 'Paint up companions and bad areas',
     + ' ',
     + 'Take current companions and bad areas, and paint them up on',
     + 'the display, in both set of boxes ',
     + ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'posntype', 'Change posn markers between spot and crosses',
     + ' ',
     + 'Change the way the position markers for -main- stars and',
     + 'companion stars are painted up, when they are so painted up.',
     + 'The choice is toggled between spot and crosses.',
     + ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'return', 'Return to main program option list',
     + 'Return to main program option list.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'res_values', 'Type out residual values in cursor defined area' ,
     + 'Gives -residual- values after the stars have been subtracted.',
     + 'Place the cursor at a position in one of the boxes, and press',
     + 'the left-hand button. Move the cursor to another position in',
     + 'the same box, and press the button again. A ractangle is ',
     + 'painted with those two positions as blh and trh corners, and ',
     + 'the -residual- values of the fit in that area typed out.'/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'uncomp_br', 'Remove a bright or close companion',
     + 'Make the display window active. Place the cursor in a box at a',
     + 'position, press the left hand button. The closest such compan-',
     + 'ion is looked for, removed from the list and marked with a ',
     + 'yellow marker. Repeat as desired. Press another button to end.',
     + 'These companions take full part in fitting, so they must be ',
     + 'either bright and fit easy, or close so cannot be done else.' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'uncomp_fa', 'Remove a faint or distant companion',
     + 'Make the display window active. Place the cursor in a box at a',
     + 'position, press the left hand button. The closest such compan-',
     + 'ion is looked for, removed from the list and marked with a ',
     + 'yellow marker. Repeat as desired. Press another button to end.',
     + 'These companions take full part in fitting, so they must be ',
     + 'either bright and fit easy, or close so cannot be done else.' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'unblank', 'Restore an area to the calculations',
     + 'This removes rectangles in the image near a fit star set for',
     + 'being ignored. Place cursor in the rectangle, press',
     + 'the l.h. button. This area is now restored. Repeat as desired.',
     + 'End by pressing another button. (See -BLANK- for more details)',
     + ' ', ' ' /

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'im_values', 'Type values in main image in cursor given area',
     + 'Gives image pixel values.',
     + 'Place the cursor at a position in one of the boxes, and press',
     + 'the left-hand button. Move the cursor to another position in',
     + 'the same box, and press the button again. A ractangle is ',
     + 'painted with those two positions as blh and trh corners, and ',
     + 'the image pixel values in that area are typed out.'/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'disp_all',  'Display the areas + residuals to the fits',
     + 'Display the areas around each star and the same areas after',
     + 'the fitted stars have been subtraced. The contrast in each ',
     + 'area is maximised to show detail, so will be different for the',
     + '-cleaned- and -uncleaned- area for the same star, and between',
     + 'stars. The position of fitted and companion stars is marked',
     + 'and also -bad- areas you have determined. '/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'disp_one',  'Display the areas + residuals to a single fit',
     + 'Display the area around a star and the same area after',
     + 'the fitted stars have been subtraced. The contrast in each ',
     + 'area is maximised to show detail, so will be different for the',
     + '-cleaned- and -uncleaned- areas. The position of fitted and ',
     + 'companion stars is marked and also -bad- areas you have ',
     + 'determined. You are asked for the number of star to show.'/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'paint_box', 'Paint up fitting boxes',
     + ' ',
     + 'Paint up a purple rectangle on the displayed fits, to show',
     + 'the actual fits area',
     + ' ', ' ', ' '/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'info_star',  'List the fit, comps, bads of a star',
     + 'It asks the number of the desired star. You put it in.',
     + 'It types out fit details, details of close and of faint  ',
     + 'companions, and of the bad areas. ',
     + ' ', ' ', ' '/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Display and Display-fit',
     +                            'DOPTION', 2 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'zoom' /

      integer sect_num
      parameter ( sect_num=9 )
      character*10 sect_head(sect_num)
      data sect_head / 'LOOK', 'MAINS', 'COMPANIONS', 'BAD AREAS',
     +            'INSPECT', 'SETUP', 'INFORM', 'DISPLAY', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'disp_all:disp_one:paint_comp:paint_box' /
      data sect_text(2) / 'shift' /
      data sect_text(3) / 'comp_br:comp_fa:uncomp_br:uncomp_fa:'/
      data sect_text(4) / 'blank:unblank' /
      data sect_text(5) / 'res_values:im_values' /
      data sect_text(6) / 'box:posntype' /
      data sect_text(7) / 'info_star' /
      data sect_text(8) / 'clear:close:zoom:reset' /
      data sect_text(9) / 'return' /

      integer help_num
      parameter ( help_num=12 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + 'Enables you to pan and zoom at the display of the stars and ',
     + 'the residuals of the fits. You can also insert and remove ',
     + 'companion stars, faint companions, and bad areas; get the ',
     + 'values in the images or residuals; remove/replace the ',
     + 'coloured lines.',
     + '             ',
     + ' Buttons for Zoom work:  Button 1 twice        = zoom /2' ,
     + '                         Button 2 twice        = zoom x2' /
      data (help_text(k),k=11,help_num) /
     + '                         Button 1 and Button 2 = pan' ,
     + '                         Button 3 twice        = exit' /

Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DISRESA -- Display an array in a displaced position
C It is scaled to:- 1) +/- 2 std dev of the data in the area indicated
C                   2) +/- 2 std dev of the data to the left and right of
C                      the indicated area
C                   3) the input range
C
C The display is located in a grid pattern position.
C It can also put the display in one of two sets of grids.
C
C   a.j.penny                   stsci                    86-05-08

      subroutine pr_disresa ( rim, kx, ky, num, kxs, kxe, kys, kye,
     +                        kopt, bmin, bmax, kbl )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer	kx		!i: X Size of data box
      integer	ky		!i: Y size of data box
      real	rim(kx,ky)	!i: data of area in box round a star
      integer	num		!i: Number of star in the inpuit list
      integer	kxs		!i: X start in box of area to calc val range
      integer	kxe		!i: X   end in box of area to calc val range
      integer	kys		!i: Y start in box of area to calc val range
      integer	kye		!i: Y   end in box of area to calc val range
      integer	kopt		!i: Display value scale method (1=indicated
				!   area;2=not central;3= bmin/bmax)
      real	bmin		!i: Display value scale minimum (if used)
      real	bmax		!i: Display value scale maximum (if used)
      integer	kbl		!i: Display in Upper (raw)(2) or lower
				!   (fitted) sets (1)?
C--
      integer jx, jy, jya, ierr
      real a, b
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      if ( kopt.eq.0 ) then						!Get pixel values scale
         call ds_imgsclr ( rim, kx, ky, kxs, kxe, kys, kye )
      elseif ( kopt.eq.1 ) then
         call ds_imgsclr ( rim, kx, ky, 1, kxs, 1, ky )
         a = DSVMIN
         b = DSVMAX
         call ds_imgsclr ( rim, kx, ky, kxe, kx, 1, ky )
         DSVMIN = (DSVMIN+a)/2.0
         DSVMAX = (DSVMAX+b)/2.0
      else
         DSVMIN = bmax
         DSVMAX = bmin
      endif

      call pr_aboxpos ( num, TBY, kx, ky, jx, jy, jya )			!Get posn of blh in screen
      if ( kbl.eq.2 ) jy = jya

      call pr_loadrinval ( RINVAL )
      call ds_acimr ( rim, kx, ky, 1, kx, 1, ky, jx, jy, .false.)	!Display


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAINTBOX -- Paint boxes round companion stars
C
C     a.j.penny                  stsci                      86-05-08-1400

      subroutine pr_paintbox ( kx, ky, tcom, kxsd, kysd, kw )

      implicit none
      include 'profile.inc'

      integer	kx		!i: X size of display area
      integer	ky		!i: Y size of display area
      real	tcom(6,MAXTOT)	!i: comps/bad areas info array
      integer	kxsd(TBY)	!i: X posn of blh of display areas
      integer	kysd(TBY)	!i: Y posn of blh of display areas
      integer	kw		!i: 0=all boxes; not 0 = that box
C--
      real xs, xe, ys, ye, xaa, yaa, yab, ya, xa, xba, yba, ybb
      integer k, ka, ierr
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      do k = 1, MAXTOT							!Paint old boxes
         if ( nint(tcom(6,k)).eq.3 ) then
            xa = com(1,k)
            ya = com(2,k)
            ka = nint(tcom(5,k))
            if ( kw.eq.0 .or. ka.eq.kw ) then
               call pr_boxpos ( ka, TBY, kx, ky, kxsd, kysd,
     +                          xa, ya, xaa, yaa, yab )
               xs = xaa
               ys = yaa
               xa = com(3,k)
               ya = com(4,k)
               call pr_boxpos ( ka, TBY, kx, ky, kxsd, kysd,
     +                          xa, ya, xba, yba, ybb )
               xe = xba
               ye = yba
               call ds_box ( xs, xe, ys, ye, 4 )

               ys = ys + yab - yaa
               ye = ye + yab - yaa
               call ds_box ( xs, xe, ys, ye, 4 )

            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_BOXPOS -- Calculate the screen position of a box
C
C  a j penny                   stsci                 1987-02-28

      subroutine pr_boxpos ( k, tbya, kx, ky, kxsd, kysd,
     +                       x, y, xoa, yoa, yob )

      implicit none

      integer	k		!i: No of box in list
      integer	tbya		!i: List length
      integer	kx		!i: X box size
      integer	ky		!i: Y box size
      integer	kxsd(tbya)	!i: X posn of blh of display areas
      integer	kysd(tbya)	!i: Y posn of blh of display areas
      real	x		!i: X posn of star
      real	y		!i: Y posn of star
      real	xoa		!o: X screen posn of blh of lower box
      real	yoa		!o: Y screen posn of blh of lower box
      real	yob		!o: Y screen posn of blh of upper box
C--
      integer jxs, jys, jxe, jye, jysa
Cbegin


      call pr_aboxpos ( k, tbya, kx, ky, jxs, jys, jysa )

      jxe = jxs + kx - 1
      jye = jys + ky - 1
      xoa = x - kxsd(k) + jxs
      yoa = y - kysd(k) + jys
      xoa = max(jxs,min(jxe,nint(xoa)))
      yoa = max(jys,min(jye,nint(yoa)))
      yob = yoa + jysa - jys


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_ABOXPOS -- Calculate the BLH screen position (upper and lower) of box
C
C  a j penny                   stsci                 1987-02-28

      subroutine pr_aboxpos ( k, tbya, kx, ky, jxs, jys, jysa )

      implicit none
      include 'ST_DS_GEN_INC'

      integer	k		!i: No of box in list
      integer	tbya		!i: List length
      integer	kx		!i: X box size
      integer	ky		!i: Y box size
      integer 	jxs		!o: X blh screen position
      integer	jys		!o: Y blh screen position in lower array
      integer	jysa		!o: Y blh screen position in upper array
C--
      integer nsq, nsqy, jxa, jya
Cbegin


      nsq = 1 + sqrt(2.0*real(tbya))
      nsqy = 1 + ((tbya-1)/nsq)
      jya = (k-1)/nsq
      jxa = k - jya*nsq
      jxs = DSSNX/2 - (nsq*(kx+6))/2 + (jxa-1)*(kx+6)
      jys = DSSNY/2 - nsqy*(ky+6) + jya*(ky+6)
      jysa = jys + nsqy*(ky+6) + 4


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_POSNTYPE -- Change position display between spot and cross
C
C    a j penny                  ral                   1991 July

      subroutine pr_posntype ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer kopt, kdef
Cbegin


      kdef = 1
      if ( PAINTSPOT ) kdef = 2
      call get_job ( 'POSTYPE', 'spot:cross', kopt, kdef, ' ', 0 )
      if ( ST_FAILED ) return

      PAINTSPOT = .true.
      if ( kopt.eq.2 ) then
         PAINTSPOT = .false.
         call get1r ( 'CWIDTH', CWIDTH, CWIDTH, 1.0, 1.0e10 )
         if ( ST_FAILED ) return
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAINTCOMP -- Paint central stars and companion stars
C
C    a j penny                  stsci                     86-5-8

      subroutine pr_paintcomp ( kx, ky, tres, tcom, kxsd, kysd, kw )

      implicit none
      include 'profile.inc'

      integer	kx		!i: X size of display areas
      integer	ky		!i: Y size of display areas
      real	tres(TBX,TBY)	!i: Fits results array
      real	tcom(6,MAXTOT)	!i: Comps/bad areas info array
      integer	kxsd(TBY)	!i: X posns of blh of display areas
      integer	kysd(TBY)	!i: Y posns of blh of display areas
      integer	kw		!i: 0=all boxes; not 0 = that box
C--
      real xa, ya, xp, yp, ypa
      integer k, ka, ierr
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      do k = 1, TBY							!Mark central stars
         if ( kw.eq.0 .or. k.eq.kw ) then

            xa = tres(1,k)
            ya = tres(2,k)
            call pr_boxpos ( k, TBY, kx, ky, kxsd, kysd,
     +                       xa, ya, xp, yp, ypa )
            if ( PAINTSPOT ) then
               call ds_spot ( xp,  yp, 1 )
               call ds_spot ( xp, ypa, 1 )
            else
               call ds_cross ( xp,  yp, CWIDTH, 1 )
               call ds_cross ( xp, ypa, CWIDTH, 1 )
            endif

         endif
      enddo

      do k = 1, MAXTOT							!Paint existing faint companions
         if ( nint(tcom(6,k)).eq.2 ) then
            xa = tcom(1,k)
            ya = tcom(2,k)
            ka = nint(tcom(5,k))
            if ( kw.eq.0 .or. ka.eq.kw ) then
               call pr_boxpos ( ka, TBY, kx, ky, kxsd, kysd,
     +                          xa, ya, xp, yp, ypa )
               if ( PAINTSPOT ) then
                  call ds_spot ( xp,  yp, 3 )
                  call ds_spot ( xp, ypa, 3 )
               else
                  call ds_cross ( xp,  yp, CWIDTH, 3 )
                  call ds_cross ( xp, ypa, CWIDTH, 3 )
            endif
            endif
         endif
      enddo

      do k = 1, MAXTOT							!Paint existing close companions
         if ( nint(tcom(6,k)).eq.1 ) then
            xa = tcom(1,k)
            ya = tcom(2,k)
            ka = nint(tcom(5,k))
            if ( kw.eq.0 .or. ka.eq.kw ) then
               call pr_boxpos ( ka, TBY, kx, ky, kxsd, kysd,
     +                          xa, ya, xp, yp, ypa )
               if ( PAINTSPOT ) then
                  call ds_spot ( xp,  yp, 2 )
                  call ds_spot ( xp, ypa, 2 )
               else
                  call ds_cross ( xp,  yp, CWIDTH, 2 )
                  call ds_cross ( xp, ypa, CWIDTH, 2 )
               endif
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTCOMPA -- Get companions stars
C
C    a j penny                  ral               85-12-31

      subroutine pr_gtcompa ( kopt, maxfc, kxsd, kysd, kxd, kyd )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer	kopt			!i: 1=Get Close;2= Get Faint comps
      integer	maxfc			!i: Max no of these comps allowed
      integer	kxsd(TBY)		!i: X posns of blh of display areas
      integer	kysd(TBY)		!i: Y posns of blh of display areas
      integer	kxd			!i: X size of display area
      integer	kyd			!i: Y size of display area
C--
      integer ns, ix, iy, k, kdhy, nbox, jxa, jya,
     +        lmx, lmy, kbut, ierr, kc, kp
      real    rvo, x, y, ya
      logical loop, mloop, dothis
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      mloop = .true.
      do  while ( mloop )

         dothis = .true.

         loop = .true.
         kp = 0
         do while ( loop )
            kp = kp + 1
            if ( kp.eq.MAXTOT ) then
               call printo ( 'ERROR: Too many comps/boxes in all' )
               mloop = .false.
               loop = .false.
               dothis = .false.
            else
               if ( nint(COM(6,kp)).eq.0 ) loop = .false.
            endif
         enddo

         call ds_gcur ( .false., ix, iy, kbut, ierr )			!Get cursor posn

         if ( kbut.ne.1 ) then
            mloop = .false.
            dothis = .false.
         endif

         if ( dothis ) then
            call pr_gtnbox ( ix, iy, TBY, kxd, kyd, nbox, jxa,		!Find which box it is in
     +                       jya, kdhy )
            if ( nbox.eq.0 ) dothis = .false.
         endif

         if ( dothis ) then
            ns = 1							!See if too many companions
            do k = 1, MAXTOT
               if ( nint(COM(6,k)).eq.kopt .and.
     +              nint(COM(5,k)).eq.nbox ) ns = ns + 1
            enddo
            if ( ns.ge.maxfc ) then					!If not OK, return
               call printo ('ERROR: Not added, too many companions'//
     +                              ' of this type in total' )
               dothis = .false.
               mloop = .false.
            endif
         endif

         if ( dothis ) then
            lmx = ix - jxa + 1 + kxsd(nbox) - 1.0			!Comp position
            lmy = iy - jya + 1 + kysd(nbox) - 1.0
            if ( lmx.lt.1  .or. lmx.gt.NX .or. lmy.lt.1 .or.
     +           lmy.gt.NY ) then
                 call printo ( 'ERROR: Not added, outside image' )
               dothis = .false.
            endif
         endif

         if ( dothis ) then

            x = ix							!Paint new posn
            y = iy
            kc = 7
            ya = iy + kdhy
            if ( kopt.eq.1 ) kc = 5
            if ( kopt.eq.2 ) kc = 6
            if ( PAINTSPOT ) then
               call ds_spot ( x, y, kc )
               call ds_spot ( x, ya, kc )
            else
               call ds_cross ( x, y, CWIDTH, kc )
               call ds_cross ( x, ya, CWIDTH, kc )
            endif

            COM(1,kp) = lmx
            COM(2,kp) = lmy
            COM(5,kp) = nbox
            COM(6,kp) = kopt
            if ( IMTYPE.eq.'SHORT' ) then
               call pr_gtcompbs ( %val(IPIM), lmx, lmy, nbox, rvo )
            else
               call pr_gtcompbr ( %val(IPIM), lmx, lmy, nbox, rvo )
            endif
            COM(3,kp) = rvo
            RES(36,nbox) = 3.0

         endif

      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTCOMPBR -- Get val of present posn over local sky (realimage)
C
C    a j penny                  ral               85-12-31

      subroutine pr_gtcompbr ( im, jx, jy, nbox, rvo )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)		!i: Main image
      integer   jx			!i: X posn
      integer   jy			!i: Y posn
      integer   nbox			!i: Number of box being used
      real	rvo			!o: Value above local sky
C--
      integer j, k, kamin
      real    val, amin
Cbegin


      rvo = 1.0
      if ( im(jx,jy).eq.RINVAL ) return

      amin = 0.0
      kamin = 0
      do k = LYS(nbox), LYS(nbox)+LY
         do j = LXS(nbox), LXS(nbox)+LX
            if ( j.ge.1. and. j.le.NX .and. k.ge.1 .and. k.le.NY ) then
               if ( im(j,k).ne.RINVAL ) then
                  val = BS*im(j,k) + BZ
                  if ( kamin.eq.0 ) then
                     kamin = 1
                     amin = val
                  else
                     amin = min(amin,val)
                  endif
               endif
            endif
         enddo
      enddo
      val = BS*im(jx,jy) + BZ
      rvo = val - amin


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTCOMPBS -- Get val of present posn over local sky (int*2 image)
C
C    a j penny                  ral               85-12-31

      subroutine pr_gtcompbs ( im, jx, jy, nbox, rvo )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer*2 im(NX,NY)		!i: Main image
      integer   jx			!i: X posn
      integer   jy			!i: Y posn
      integer   nbox			!i: Number of box being used
      real	rvo			!o: Value above local sky
C--
      integer j, k, kamin
      real    val, amin
Cbegin


      rvo = 1.0
      if ( im(jx,jy).eq.INVAL ) return

      amin = 0.0
      kamin = 0
      do k = LYS(nbox), LYS(nbox)+LY
         do j = LXS(nbox), LXS(nbox)+LX
            if ( j.ge.1. and. j.le.NX .and. k.ge.1 .and. k.le.NY ) then
               if ( im(j,k).ne.INVAL ) then
                  val = BS*real(im(j,k)) + BZ
                  if ( kamin.eq.0 ) then
                     kamin = 1
                     amin = val
                  else
                     amin = min(amin,val)
                  endif
               endif
            endif
         enddo
      enddo
      val = BS*real(im(jx,jy)) + BZ
      rvo = val - amin


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PRINTVALR -- Get area, print values or residuals in it (real image)
C
C    a j penny                  ral               85-12-31

      subroutine pr_printvalr ( im, kxd, kyd, kxsd, kysd, kopt, tres,
     +                          tcom )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)		!i: Main image
      integer	kxd			!i: X size of display areas
      integer	kyd			!i: Y size of display areas
      integer	kxsd(TBY)		!i: X posns of blh of display areas
      integer	kysd(TBY)		!i: Y posns of blh of display areas
      integer	kopt			!i: Print option (1=values;2=resids)
      real	tres(TBX,TBY)		!i: Fit results array
      real	tcom(6,MAXTOT)		!i: Comps/bad areas info array
C--
      integer kx, ky, j, ja, k, nbox, jxa, jya, kbut, ierr,
     +        kv(9), ksp, nv, mxs, mys, mxe, mye, kdhy, nboxa
      real xs, xe, ys, ye, rv, x, y, sky
      character*72 text
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn

      call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nbox, jxa, jya, kdhy )	!Find which box it is in

      if ( nbox.eq.0 ) return						!If not OK, return

      xs = kx								!Note and paint position
      ys = ky
      call ds_spot ( xs, ys, 4 )
      mxs = kx - jxa + 1.0 + kxsd(nbox) - 1.0
      mys = ky - jya + 1.0 + kysd(nbox) - 1.0

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get 2nd posn

      call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nboxa, jxa, jya, kdhy )
      if ( nbox.ne.nboxa ) then
         call printo ( 'ERROR: Out of box - no action taken' )
         return
      endif

      xe = kx
      ye = ky
      mxe = kx - jxa + 1.0 + kxsd(nbox) - 1.0
      mye = ky - jya + 1.0 + kysd(nbox) - 1.0
      call cswopi ( mxs, mxe )
      call cswopi ( mys, mye )

      call cswopr ( xs, xe )						!Paint boxes
      call cswopr ( ys, ye )
      call ds_box ( xs, xe, ys, ye, 3 )
      ys = ys + kdhy
      ye = ye + kdhy
      call ds_box ( xs, xe, ys, ye, 3 )

      ksp = 1 + ((mxe-mxs)/9)
      nv = 1 + (mxe-mxs)/ksp

      if ( kopt.eq.1 ) then						!Print values
         do k = mye, mys, -1*ksp
            do j = 1, nv
               ja = mxs + (j-1)*ksp
               if ( im(ja,k).eq.RINVAL ) then
                  kv(j) = -999999
               else
                  rv = BS*im(ja,k) + BZ
                  kv(j) = min(999999.0,max(-999999.0,rv))
               endif
            enddo
            write ( text, '(1h ,i4,3x,9i7)' ) k, (kv(j),j=1,nv)
            call printo ( text )
         enddo
      endif

      if ( kopt.eq.2 ) then						!Print residuals
         do k = mye, mys, -1*ksp
            do j = 1, nv
               ja = mxs + (j-1)*ksp
               if ( im(ja,k).eq.RINVAL ) then
                  kv(j) = -999999
               else
                  call pr_valstars ( ja, k, rv, tres, nbox, tcom )
                  x = ja - kxsd(nbox) - kxd/4
                  y =  k - kysd(nbox) - kyd/4
                  sky = tres(10,nbox)+x*tres(32,nbox)+y*tres(33,nbox)
                  rv = BS*im(ja,k) + BZ - rv - sky
                  kv(j) = min(999999.0,max(-999999.0,rv))
               endif
            enddo
            write ( text, '(1h ,i4,3x,9i7)' ) k, (kv(j),j=1,nv)
            call printo ( text )
         enddo
      endif

      call printo ( ' ' )						!Print bottom line
      do j = 1, nv
         kv(j) = mxs + (j-1)*ksp
      enddo
      write ( text, '(1h ,7x,9i7)' ) (kv(j),j=1,nv)
      call printo ( text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PRINTVALS -- Get area, print values or residuals in it (int*2 image)
C
C    a j penny                  ral               85-12-31

      subroutine pr_printvals ( im, kxd, kyd, kxsd, kysd, kopt, tres,
     +                          tcom )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer*2 im(NX,NY)		!i: Main image
      integer	kxd			!i: X size of display areas
      integer	kyd			!i: Y size of display areas
      integer	kxsd(TBY)		!i: X posns of blh of display areas
      integer	kysd(TBY)		!i: Y posns of blh of display areas
      integer	kopt			!i: Print option (1=values;2=resids)
      real	tres(TBX,TBY)		!i: Fit results array
      real	tcom(6,MAXTOT)		!i: Comps/bad areas info array
C--
      integer kx, ky, j, ja, k, nbox, jxa, jya, kbut, ierr,
     +        kv(9), ksp, nv, mxs, mys, mxe, mye, kdhy, nboxa
      real xs, xe, ys, ye, rv, x, y, sky
      character*72 text
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn

      call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nbox, jxa, jya, kdhy )	!Find which box it is in

      if ( nbox.eq.0 ) return						!If not OK, return

      xs = kx								!Note and paint position
      ys = ky
      call ds_spot ( xs, ys, 4 )
      mxs = kx - jxa + 1.0 + kxsd(nbox) - 1.0
      mys = ky - jya + 1.0 + kysd(nbox) - 1.0

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get 2nd posn

      call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nboxa, jxa, jya, kdhy )
      if ( nbox.ne.nboxa ) then
         call printo ( 'ERROR: Out of box - no action taken' )
         return
      endif

      xe = kx
      ye = ky
      mxe = kx - jxa + 1.0 + kxsd(nbox) - 1.0
      mye = ky - jya + 1.0 + kysd(nbox) - 1.0
      call cswopi ( mxs, mxe )
      call cswopi ( mys, mye )

      call cswopr ( xs, xe )						!Paint boxes
      call cswopr ( ys, ye )
      call ds_box ( xs, xe, ys, ye, 3 )
      ys = ys + kdhy
      ye = ye + kdhy
      call ds_box ( xs, xe, ys, ye, 3 )

      ksp = 1 + ((mxe-mxs)/9)
      nv = 1 + (mxe-mxs)/ksp

      if ( kopt.eq.1 ) then						!Print values
         do k = mye, mys, -1*ksp
            do j = 1, nv
               ja = mxs + (j-1)*ksp
               if ( im(ja,k).eq.INVAL ) then
                  kv(j) = INVAL
               else
                  kv(j) = BS*im(ja,k) + BZ
               endif
            enddo
            write ( text, '(1h ,i4,3x,9i7)' ) k, (kv(j),j=1,nv)
            call printo ( text )
         enddo
      endif

      if ( kopt.eq.2 ) then						!Print residuals
         do k = mye, mys, -1*ksp
            do j = 1, nv
               ja = mxs + (j-1)*ksp
               if ( im(ja,k).eq.INVAL ) then
                  kv(j) = INVAL
               else
                  call pr_valstars ( ja, k, rv, tres, nbox, tcom )
                  x = ja - kxsd(nbox) - kxd/4
                  y =  k - kysd(nbox) - kyd/4
                  sky = tres(10,nbox)+x*tres(32,nbox)+y*tres(33,nbox)
                  kv(j) = BS*im(ja,k) + BZ - rv - sky
               endif
            enddo
            write ( text, '(1h ,i4,3x,9i7)' ) k, (kv(j),j=1,nv)
            call printo ( text )
         enddo
      endif

      call printo ( ' ' )						!Print bottom line
      do j = 1, nv
         kv(j) = mxs + (j-1)*ksp
      enddo
      write ( text, '(1h ,7x,9i7)' ) (kv(j),j=1,nv)
      call printo ( text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_UNCOMPA -- Get companions to remove and remove painted spots
C
C    a j penny                  ral               85-12-31

      subroutine pr_uncompa ( kopt, tcom, kxd, kyd, tres, kxsd, kysd )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      integer kopt		!i: Flag (1=do bright/close;
				!         2=do faint/distant comps)
      real    tcom(6,MAXTOT)	!i/o: comps/bad areas array
      integer kxd		!i: X size of display areas
      integer kyd		!i: Y size of display areas
      real    tres(TBX,TBY)	!i/o: Fit results array
      integer kxsd(TBY)		!i: X posns of blh of display areas
      integer kysd(TBY)		!i: Y posns of blh of display areas
C--
      integer kx, ky, kdhy, kbut, ierr, nbox, k, jxa, jya, kc, kit
      real dist, adist, amx, amy
      logical loop, dothis
Cbegin


      kit = 0
      do k = 1, MAXTOT
         if ( nint(tcom(6,k)).eq.kopt ) kit = k
      enddo
      if ( kit.eq.0 ) then
         call printo ( 'Nosuch companions to remove' )
         return
      endif

      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      kc = 2								!Set colour
      if ( kopt.eq.1 ) kc = 1
      if ( kopt.eq.2 ) kc = 2

      loop = .true.
      do while ( loop )
         dothis = .true.

         call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn
         if ( kbut.ne.1 ) then
            dothis = .false.
            loop = .false.
         endif

         if ( dothis ) then
            call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nbox, jxa, 		!Find which box it is in
     +                       jya, kdhy )
            if ( nbox.eq.0 ) dothis = .false.
         endif

         if ( dothis ) then
            kit = 0							!Remove nearest
            amx = kx - jxa + 1.0 + kxsd(nbox) - 1.0
            amy = ky - jya + 1.0 + kysd(nbox) - 1.0
            adist = NX + NY
            do k = 1, MAXTOT
               if ( nint(tcom(5,k)).eq.nbox .and.
     +              nint(tcom(6,k)).eq.kopt ) then
                  dist = sqrt( (amx-tcom(1,k))**2.0 +
     +                         (amy-tcom(2,k))**2.0 + 0.001 )
                  if ( dist.lt.adist ) then
                      kit = k
                     adist = dist
                  endif
               endif
            enddo
            if ( kit.ne.0 ) then
               call azeror ( tcom(1,kit), 6 )
               tres(36,nbox) = 3.0
               call pr_paintcomp ( kxd, kyd, tres, tcom, kxsd, kysd, 0 )
            else
               call printo ( 'No such companions to remove' )
            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTBOXA -- Get an ignore area in a box
C
C        a.j.penny           stsci                        86-05-08-1400

      subroutine pr_gtboxa ( tcom, kxd, kyd, kxsd, kysd, tres )

      implicit none
      include 'profile.inc'

      integer kxd		!i: X size of display areas
      integer kyd		!i: Y size of display areas
      integer kxsd(TBY)		!i: X posns of blh of display areas
      integer kysd(TBY)		!i: Y posns of blh of display areas
      real    tres(TBX,TBY)	!i/o: Fit results array
      real    tcom(6,MAXTOT)	!i/o: comps/bad areas array
C--
      integer kx, ky, ll, ns, mxa, mya, mxb, myb, nbox, jx, jy,
     +        nboxa, kdhy, kbut, kp, ierr
      real akl, xs, xe, ys, ye, x, y
      logical loop, mloop, dothis
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      mloop = .true.
      do while ( mloop )
         dothis = .true.

         loop = .true.
         kp = 0
         do while ( loop )
            kp = kp + 1
            if ( kp.gt.MAXTOT ) then
               call printo ( 'ERROR: Too many comps/boxes in all' )
               mloop = .false.
               loop = .false.
               dothis = .false.
            else
               if ( nint(tcom(6,kp)).eq.0 ) loop = .false.
            endif
         enddo

         if ( dothis ) then
            call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn
            if ( kbut.ne.1 ) then
               mloop = .false.
               dothis = .false.
            endif
         endif

         if ( dothis ) call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nbox, 	!Find which box it is in. return if bad.
     +                                  jx, jy, kdhy )
         if ( nbox.eq.0 ) dothis = .false.

         if ( dothis ) then
            ns = 1							!See if too many boxes. Return if bad.
            do ll = 1, MAXTOT
               if ( nint(tcom(6,ll)).eq.3 .and.
     +              nint(tcom(5,ll)).eq.nbox ) ns = ns + 1
            enddo
            if ( ns.ge.MAXBAD ) then
               call printo ( 'Not added, too many boxes' )
               mloop = .false.
               do this = .false.
            endif
         endif

         if ( dothis ) then

             x = kx							!Paint and note 1st posn
             y = ky
             call ds_spot ( x, y, 4 )
             y = ky + kdhy
             call ds_spot ( x, y, 4 )
             y = ky

             mxa = kx - jx + 1 + kxsd(nbox) - 1.0
             mya = ky - jy + 1 + kysd(nbox) - 1.0

             call ds_gcur ( .false., kx, ky, kbut, ierr ) 		!Get 2nd posn. return if bad.
             if ( kbut.ne.1 ) then
                mloop = .false.
                dothis = .false.
             endif

         endif

         if ( dothis ) then
            call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nboxa, 		!Find which box it is in. return if bad.
     +                                  jx, jy, kdhy )
            if ( nbox.ne.nboxa ) then
               call printo ( 'ERROR: Out of box - 1st point forgotten')
               dothis = .false.
            endif
         endif

         if ( dothis ) then
            mxb = kx - jx + 1 + kxsd(nbox) - 1.0
            myb = ky - jy + 1 + kysd(nbox) - 1.0

            xs = min(x,real(kx))					!Paint box
            xe = max(x,real(kx))
            ys = min(y,real(ky))
            ye = max(y,real(ky))
            call ds_box ( xs, xe, ys, ye, 4 )
            ys = ys + kdhy
            ye = ye + kdhy
            call ds_box ( xs, xe, ys, ye, 4 )

            tcom(1,kp) = mxa
            tcom(2,kp) = mya
            tcom(3,kp) = mxb
            tcom(4,kp) = myb
            tcom(5,kp) = nbox
            tcom(6,kp) = 3.0
            if ( nint(tcom(1,kp)).gt.nint(tcom(3,kp)) ) then
               akl = tcom(1,kp)
               tcom(1,kp) = tcom(3,kp)
               tcom(3,kp) = akl
            endif
            if ( nint(tcom(2,kp)).gt.nint(tcom(4,kp)) ) then
               akl = tcom(2,kp)
               tcom(2,kp) = tcom(4,kp)
               tcom(4,kp) = akl
            endif
            tres(36,nbox) = 3.0
            call pr_paintbox ( kxd, kyd, tcom, kxsd, kysd, 0 )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_UNBOX -- Remove an ignore area from a box
C
C     a.j.penny                  stsci                 86-10-28


      subroutine pr_unbox ( tcom, kxd, kyd, tres, kxsd, kysd )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer kxd		!i: X size of display areas
      integer kyd		!i: Y size of display areas
      integer kxsd(TBY)		!i: X posns of blh of display areas
      integer kysd(TBY)		!i: Y posns of blh of display areas
      real    tcom(6,MAXTOT)	!i/o: comps/bad areas array
      real    tres(TBX,TBY)	!i/o: Fit results array
C--
      integer kx, ky, kbut, ierr, nbox, j, k, jx, jy, lmx, lmy, kdhy,
     +        jkx(4), jky(4), kit
      real dist, adist
      data jkx/1,3,1,3/, jky/2,2,4,4/
      logical loop, dothis
Cbegin


      kit = 0
      do k = 1, MAXTOT
         if ( nint(tcom(6,k)).eq.3 ) kit = k
      enddo
      if ( kit.eq.0 ) then
         call printo ( 'No such boxes to remove' )
         return
      endif

      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      loop = .true.
      do while ( loop )
         dothis = .true.

         call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn
         if ( kbut.ne.1 ) then
            dothis = .false.
            loop = .false.
         endif

         if ( dothis ) call pr_gtnbox ( kx, ky, TBY, kxd, kyd, 		!Find which box it is in
     +                                  nbox, jx, jy, kdhy )
         if ( nbox.eq.0 ) dothis = .false.						!Remove nearest, if in box

         if ( dothis ) then
            lmx = kx - jx + 1
            lmy = ky - jy + 1
            lmx = lmx + kxsd(nbox) - 1.0
            lmy = lmy + kysd(nbox) - 1.0
            adist = NX + NY
            kit = 0
            do k = 1, MAXTOT
               if ( nint(tcom(6,k)).eq.3 .and.
     +              nint(tcom(5,k)).eq.nbox    ) then
                  do j = 1, 4
                     dist = sqrt( (lmx-tcom(jkx(j),k))**2.0+
     +                            (lmy-tcom(jky(j),k))**2.0 + 0.001 )
                     if ( dist.lt.adist ) then
                        kit = k
                        adist = dist
                     endif
                  enddo
               endif
            enddo
            if ( kit.ne.0 ) then
               call azeror ( tcom(1,kit), 6 )
               tres(36,nbox) = 3.0
               call pr_paintbox ( kxd, kyd, tcom, kxsd, kysd, 0 )
            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MAPINTER -- Interact with the display for 'map'
C
C    A.J.Penny          ral             1990-07-24

      subroutine pr_mapinter ( map, rima, rimb, omap )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_DS_PANEL_INC'

      real      map(MX,MY,1) 		!i/o: Map
      real      rima(MX,MY) 		!i/o: Map fill array
      real      rimb(MX,MY) 		!o: Work area
      real	omap(MX,MY,1)		!i: Original map
C--
      logical loop
      integer i, js, je, ks, ke, j, k
      character*12 ktopt
Cbegin


      call prga_opt_setup ( ktopt, 5, .true. )				!Set up options
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )

         call prga_opt_setup ( ktopt, 5, .false. )
         call get_choice ( ktopt, 1 )					!Get choice
         if ( ST_FAILED ) return

         if ( ktopt.eq.'values' ) call pr_prarea ( map, MX, MY )	!List residual values

         if ( ktopt.eq.'zero' ) then					!Zero map
                          call azeror ( map, MX*MY )
                          call pr_disresb ( map, rima, rimb, MX, MY, 1)
                          endif

         if ( ktopt.eq.'zrect' ) then					!Zero an area of the map
                          call pr_gtboxb ( MX, MY, js, je, ks, ke, i,2)
                          if ( i.eq.0 ) then
                             do k = ks, ke
                                do j = js, je
                                   map(j,k,1) = 0.0
                                enddo
                             enddo
                          call pr_disresb ( map, rima, rimb, MX, MY, 1)
                          endif
                          endif

         if ( ktopt.eq.'smooth' ) then					!Smooth map
                          call pr_boxfra ( map, MX, MY )
                          call pr_disresb ( map, rima, rimb, MX, MY, 1)
                          endif

         if ( ktopt.eq.'spolyin' ) call pr_dopol ( map, rima, rimb,	!Smooth area inside polygon
     +                                             MX, MY, 1, 1 )

         if ( ktopt.eq.'spolyout' ) call pr_dopol ( map, rima, rimb,	!Smooth area outside polygon
     +                                              MX, MY, 1, 2 )

         if ( ktopt.eq.'zpolyin' ) call pr_dopol ( map, rima, rimb, 	!Zero area inside polygon
     +                                             MX, MY, 1, 3 )

         if ( ktopt.eq.'zpolyout' ) call pr_dopol ( map, rima, rimb,	!Zero area outside polygon
     +                                              MX, MY, 1, 4 )

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )          !Set zoom, pan to null

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .false., 0, 0 )		!Pan and zoom look at them

         if ( ktopt.eq.'clear' ) call pr_dsclear			!Clear display

         if ( ktopt.eq.'display' ) call pr_disresb ( map, rima, rimb,	!Display map
     +                                               MX, MY, 1)

         if ( ktopt.eq.'original' ) call amovr ( omap, map, MX*MY )	!Load original map

         if ( ktopt.eq.'return' ) loop = .false.			!Exit

         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRGA_OPT_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 Jan

      subroutine prga_opt_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside           !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=14 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear the display window',
     + 'Clear the display window. This only affects the window. It',
     + 'does not affect any of the image or residuals, or profile. It',
     + 'is used solely for tidying up a window that has got too',
     + 'cluttered, and you wish to display some new stuff.',
     + ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'original', 'Load map as before entering this MAP option',
     + 'This loads the map array with the -original- values, ',
     + 'that is, those it had when you entered this panel of ',
     + '-MAP_INTERACT- options. It then displays it.',
     + 'This is of use when you have done various actions on the ',
     + 'array and want to erase their effect, without recalculating',
     + 'the map. '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'return', 'Return to main OPTION choice',
     + 'Return to main program option list.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'spolyin', 'Define a polygon with cursor, smooth area inside',
     + 'This smooths an area of the array. Place cursor on a vertex',
     + 'of a polygon inside which to smooth, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, asks what',
     + 'size of box to smooth with, smooths inside the polygon,',
     + 'displays the result. ' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'spolyout', 'Define a polygon with cursor, smooth area outside',
     + 'Smooths OUTSIDE a polygon defined in the array. Place cursor',
     + 'on vertex of a polygon, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, asks what',
     + 'size of box to smooth with, smooths outside the polygon,',
     + 'displays the result. ' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'smooth', 'Smooth profile map by >top hat> NxM box',
     + 'You input the size of a box (say XxY). The program then ',
     + 'smooths the WHOLE array by replacing each pixel with the ',
     + 'average of the XxY pixels around it. (Pixels too near edge ',
     + 'will be smoothed as if the array projected beyond its edges ',
     + 'with a refection of its edge pixels.) ',
     + ' ' /

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'values', 'Type out values for an area of the profile map',
     + 'You use the keyboard to define the X and Y range of a ',
     + 'rectangular box in the array. The values are then typed out.',
     + 'They are multiplied by 1000 for ease of display. The actual ',
     + 'values are the fraction of a star of unit height that this ',
     + 'empirical map represents the profile.',
     + ' '/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'zero', 'Zero all profile map',
     + 'Zeroes all the values in the empirical map. and displays it.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'zpolyin', 'Define a polygon with cursor, zero area inside',
     + 'This zeroes an area of the array. Place cursor on a vertex',
     + 'of a polygon inside which to smooth, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, and',
     + 'zeroes inside the polygon, displays the result. ',
     + ' ' /

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'zpolyout', 'Define a polygon with cursor, zero area outside',
     + 'Zeroes OUTSIDE a polygon defined in the array. Place cursor',
     + 'on vertex of a polygon, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, smooths ',
     + 'outside the polygon, displays the result.  ',
     + ' ' /

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zrect', 'Define a rectangle with cursor, zero area inside' ,
     + 'This zeroes a rectangle in the array. Place cursor on b.l.h.',
     + 'corner of desired area, press a button and repeat for the ',
     + 't.r.h. corner. The program completes the polygon, and',
     + 'zeroes inside the polygon, displays the result. ',
     + ' ', ' ' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + ' ',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' '/

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'display',  'Display the profile empirical map array',
     + ' ',
     + 'Display the profile empirical map array',
     + ' ', ' ', ' ', ' '/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Profile Map Array', 'MOPTION', 5 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'zoom'  /

      integer sect_num
      parameter ( sect_num=6 )
      character*10 sect_head(sect_num)
      data sect_head / 'SMOOTH', 'ZERO', 'RESTORE', 'INSPECT',
     +                 'DISPLAY', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'smooth:spolyin:spolyout' /
      data sect_text(2) / 'zero:zpolyin:zpolyout:zrect' /
      data sect_text(3) / 'original' /
      data sect_text(4) / 'values' /
      data sect_text(5) / 'clear:display:zoom:reset' /
      data sect_text(6) / 'return' /

      integer help_num
      parameter ( help_num=11 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + 'This enables you to look at the profile map closely and to',
     + 'change it to some extent. ',
     + '              ',
     + 'When defining the polygons, you use the cursor and the ' ,
     + 'buttons. Place the cursor at consecutive vertices of the ' ,
     + 'desired polygon, and press any button. To end the input of ' ,
     + 'vertices, do a repeat press, without moving the cursor, on ' ,
     + 'the last vertex. A line showing where the polygon lies is ' /
      data (help_text(k),k=11,help_num) /
     + 'output whilst you are doing this.' /

Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DOPOL -- Do the polygon smoothing and zeroing
C
C    A.J.Penny          ral             1990-07-24

      subroutine pr_dopol ( rim, rb, wim, kx, ky, km, kopt )

      implicit none

      integer	kx		!i: X size of arrays
      integer	ky		!i: Y size of arrays
      real	rim(kx,ky)	!i/o: Data array
      real	rb(kx,ky)	!i/o: Flag array for data
      integer*2 wim(kx,ky)	!o: Work array
      integer   km		!i: Flag (2=integer*2 input; 1= real input)
      integer   kopt		!i: Flag (1=smooth in;2=smooth out;3=zero in;
				!         4=zero out)
C--
      integer ipoly, i
Cbegin


      call gtwrks ( 'POLY', kx*ky, ipoly, i )

      call pr_polfill ( %val(ipoly), kx, ky, i )

      if ( km.eq.1 ) call amovkr ( 1.0, rb, kx*ky )

      if (i.eq.0) call pr_poldo ( %val(ipoly), kx, ky, rim, rb, kopt)

      call pr_disresb ( rim, rb, wim, kx, ky, km )

      call wrkcan ( 'POLY' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_RESINTER -- Interact with the display for 'residuals'
C
C    A.J.Penny          ral             1990-07-24

      subroutine pr_resinter ( kx, ky, rim, wim, rd, rdh, ord, ordh )

      implicit none
      include 'STARMAN_INC'
      include 'ST_DS_PANEL_INC'

      integer   kx		!i: X size of data array
      integer   ky		!i: Y size of data array
      real      rim(kx,ky)	!o: Work array
      integer*2 wim(kx,ky)	!o: Work array
      real      rd(kx,ky)	!i/o: Residuals array
      real      rdh(kx,ky)	!i/o: Residuals filling array
      real      ord(kx,ky)	!i: Input to this s/r residuals array
      real      ordh(kx,ky)	!i: Input to this s/r residuals fill array
C--
      logical loop
      integer i, js, je, ks, ke, j, k, ierr
      character*12 ktopt
Cbegin


      call prgb_opt_setup ( ktopt, 3, .true. )				!Set up options
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )

         call prgb_opt_setup ( ktopt, 3, .false. )
         call get_choice ( ktopt, 1 )					!Get option
         if ( ST_FAILED ) return

         if ( ktopt.eq.'calculate' ) call pr_mrescalc			!Calculate residuals and
									! put them in residuals map

         if ( ktopt.eq.'display' ) call pr_disresb ( rd, rdh, wim,	!Display residuals
     +                                               kx, ky, 2 )

         if ( ktopt.eq.'values' ) call pr_prarea ( rd, kx, ky )		!List residual values

         if ( ktopt.eq.'zero' ) then					!Zero all residuals
                          call azeror ( rd, kx*ky )
                          call azeror ( rdh, kx*ky )
                          call pr_disresb ( rd, rdh, wim, kx, ky, 2 )
                          endif

         if ( ktopt.eq.'zrect' ) then					!Zero an area of the residuals
                          call pr_gtboxb ( kx, ky, js, je, ks, ke, i,1)
                          if ( i.eq.0 ) then
                             do k = ks, ke
                                do j = js, je
                                   rd(j,k) = 0.0
                                   rdh(j,k) = 1.0
                                enddo
                             enddo
                             call pr_disresb ( rd, rdh, wim, kx, ky, 2 )
                          endif
                          endif

         if ( ktopt.eq.'smooth' ) then					!Smooth residuals
                          call pr_boxfr ( rd, rdh, rim, kx, ky, ierr )
                          if ( ierr.eq.0 ) call pr_disresb ( rd, rdh,
     +                                               wim, kx, ky, 2 )
                          endif

         if ( ktopt.eq.'spolyin' ) call pr_dopol ( rd, rdh, wim, 	!Smooth area inside polygon
     +                                             kx, ky, 2, 1 )

         if ( ktopt.eq.'spolyout' ) call pr_dopol ( rd, rdh, wim, 	!Smooth area outside polygon
     +                                             kx, ky, 2, 2 )

         if ( ktopt.eq.'zpolyin' ) call pr_dopol ( rd, rdh, wim, 	!Zero area inside polygon
     +                                             kx, ky, 2, 3 )

         if ( ktopt.eq.'zpolyout' ) call pr_dopol ( rd, rdh, wim,	!Zero area outside polygon
     +                                             kx, ky, 2, 4 )

         if ( ktopt.eq.'original' ) then				!Load original residuals
                          call amovr ( ord, rd, kx*ky )
                          call amovr ( ordh, rdh, kx*ky )
                          call pr_disresb ( rd, rdh, wim, kx, ky, 2 )
                          endif

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )          !Set zoom, pan to null

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .false., 0, 0 )		!Pan and zoom look at them

         if ( ktopt.eq.'fill' ) then					!Set empty areas to interpolated values
                              call pr_infill ( rd, rdh, kx, ky, .true.)
                              call pr_disresb ( rd, rdh, wim, kx,ky,2)
                                endif

         if ( ktopt.eq.'clear' ) call pr_dsclear			!Clear display

         if ( ktopt.eq.'return' ) loop = .false.			!Exit

         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRGB_OPT_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 Jan

      subroutine prgb_opt_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside           !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=16 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear the display window',
     + 'Clear the display window. This only affects the window. It',
     + 'does not affect any of the image or residuals, or profile. It ',
     + 'is used solely for tidying up a window that has got too',
     + 'cluttered, and you wish to display some new stuff.',
     + ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'original', 'Load original values from last fit calculations',
     + 'This loads the residuals array with the -original- values, ',
     + 'that is, those it had when you entered this panel of ',
     + '-RES_INTERACT- options. It then displays it.',
     + 'This is of use when you have done various actions on the ',
     + 'array and want to erase their effect, without recalculating',
     + 'the array from the fits. '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'return', 'Return to main OPTION choice',
     + 'Return to main program option list.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'spolyin', 'Define a polygon with cursor, smooth area inside',
     + 'This smooths an area of the array. Place cursor on a vertex',
     + 'of a polygon inside which to smooth, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, asks what',
     + 'size of box to smooth with, smooths inside the polygon,',
     + 'displays the result. Area must be -full- (see -FILL-) option. '/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'spolyout', 'Define a polygon with cursor, smooth area outside',
     + 'Smooths OUTSIDE a polygon defined in the array. Place cursor',
     + 'on vertex of a polygon, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, asks what',
     + 'size of box to smooth with, smooths outside the polygon,',
     + 'displays the result. Area must be -full- (see -FILL- option). '/

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'smooth', 'Smooth profile map by >top hat> NxM box',
     + 'You input the size of a box (say XxY). The program then ',
     + 'smooths the WHOLE array by replacing each pixel with the ',
     + 'average of the XxY pixels around it. (Pixels too near edge ',
     + 'will be smoothed as if the array projected beyond its edges ',
     + 'with a refection of its edge pixels.) ',
     + 'Only works if array is -full- (see -FILL- option).'/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'values', 'Type out values for an area of the residual array',
     + 'You use the keyboard to define the X and Y range of a ',
     + 'rectangular box in the array. The values are then typed out.',
     + 'They are multiplied by 1000 for ease of display. The actual ',
     + 'values are the fraction of a star of unit height that the ',
     + 'average stars pixels are bigger than the calculated fits.',
     + ' '/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'zero', 'Zero all residual array',
     + 'Zeroes all the values in the residual array. Also flags that ',
     + 'none of the values have had a value loaded into them, by',
     + 'clearing the loading array. (You can set them as loaded with',
     + 'the -FILL- option.)',
     + ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'zpolyin', 'Define a polygon with cursor, zero area inside',
     + 'This zeroes an area of the array. Place cursor on a vertex',
     + 'of a polygon inside which to smooth, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, and',
     + 'zeroes inside the polygon, displays the result. (Any pixel',
     + 'that has not been loaded will be zeroed and marked as loaded.)'/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'zpolyout', 'Define a polygon with cursor, zero area outside',
     + 'Zeroes OUTSIDE a polygon defined in the array. Place cursor',
     + 'on vertex of a polygon, press a button and ',
     + 'repeat for the other verticies. At last one, re-press (at the',
     + 'SAME position). The program completes the polygon, smooths ',
     + 'outside the polygon, displays the result.  (Any pixel',
     + 'that has not been loaded will be zeroed and marked as loaded.)'/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zrect', 'Define a rectangle with cursor, zero area inside' ,
     + 'This zeroes a rectangle in the array. Place cursor on b.l.h.',
     + 'corner of desired area, press a button and repeat for the ',
     + 't.r.h. corner. The program completes the polygon, and',
     + 'zeroes inside the polygon, displays the result. (Any pixel',
     + 'that has not been loaded will be zeroed and marked as loaded.)',
     + ' ' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'fill',  'Fill any -empty- pixels in the residuals array',
     + 'After the residuals array has been calculated some of the',
     + 'pixels may be empty, if they are subdivided, as there',
     + 'may not be a star with the right centre offset from the image',
     + 'pixel centres. Use this option to cure this by interpolating',
     + 'between neighbouring pixels. The infilled array is displayed,',
     + 'as is the check array showing that all pixels have values.' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'display',  'Display the mean fit residuals array',
     + 'After using the -CALCULATE- option to get the residuals image,',
     + 'this displays it, with, to its right side a picture of whether',
     + 'pixels actually have a residuals value (white if they have). ',
     + 'A pixel may be empty if they are subdivided, as there',
     + 'may not be a star with the right centre offset from the image',
     + 'pixel centres. Use -FILL- to cure this.'/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'calculate',  'Calculate the array of residuals',
     + 'The mean profile is fixed and fitted to all accepted stars. ',
     + 'The residuals from these fits are then scaled and averaged',
     + 'into a -residual- image. (The pixels may be sub-divided',
     + 'versions of the image pixels if the profile is sharp. In this',
     + 'case each residual from a star is put in its right sub-pixel)',
     + 'This residual image IS USED IN making the output -MAP- image. '/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Residuals Array', 'ROPTION', 3 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'zoom' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / 'LOAD', 'SMOOTH', 'ZERO', 'RESTORE', 'INSPECT',
     +                 'DISPLAY', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'fill:calculate' /
      data sect_text(2) / 'smooth:spolyin:spolyout' /
      data sect_text(3) / 'zero:zpolyin:zpolyout:zrect' /
      data sect_text(4) / 'original' /
      data sect_text(5) / 'values' /
      data sect_text(6) / 'clear:display:zoom:reset' /
      data sect_text(7) / 'return' /

      integer help_num
      parameter ( help_num=11 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + 'This enables you to look at the residuals map closely and to',
     + 'change it to some extent. ',
     + '              ',
     + 'When defining the polygons, you use the cursor and the ' ,
     + 'buttons. Place the cursor at consecutive vertices of the ' ,
     + 'desired polygon, and press any button. To end the input of ' ,
     + 'vertices, do a repeat press, without moving the cursor, on ' ,
     + 'the last vertex. A line showing where the polygon lies is ' /
      data (help_text(k),k=11,help_num) /
     + 'output whilst you are doing this.' /

Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTBOXB -- Get by cursor a box in the residuals/map area
C
C        a.j.penny           stsci                        86-05-08-1400

      subroutine pr_gtboxb ( lxa, lya, kxs, kxe, kys, kye, kit, kopt )

      implicit none
      include 'ST_DS_GEN_INC'

      integer lxa	!i: Screen X posn
      integer lya	!i: Screen Y posn
      integer kxs	!o: X start in residuals/map area
      integer kxe	!o: X   end in residuals/map area
      integer kys	!o: Y start in residuals/map area
      integer kye	!o: Y   end in residuals/map area
      integer kit	!o: 0=got box; 1= not got box
      integer kopt	!i: Choice Flag (1=residuals;2=map)
C--
      integer kx, ky, kbut, ierr, jxa, jya, jxb, jyb
      real x, y, xs, xe, ys, ye
Cbegin


      kit = 0								!Default

      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      jxa = DSSNX/2 - (lxa+5)						!Box position
      jya = DSSNY/2 - lya/2
      if ( kopt.eq.2 ) jya = jya - lya - 5
      jxb = jxa + lxa - 1
      jyb = jya + lya - 1

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn
      if ( kx.lt.jxa .or. kx.gt.jxb .or. ky.lt.jya .or. ky.gt.jyb )then	!See if in box
         kit = 1							!If not, return
         return
      endif
      kxs = kx - jxa + 1
      kys = ky - jya + 1

      x = kx								!paint and note 1st posn
      y = ky
      call ds_spot ( x, y, 3 )

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get 2nd posn
      if ( kx.lt.jxa .or. kx.gt.jxb .or. ky.lt.jya .or. ky.gt.jyb )then	!See if in box
         call printo ( 'ERROR: Out of box - no action taken' )
         kit = 1							!If not, return
         return
      endif
      kxe = kx - jxa + 1
      kye = ky - jya + 1

      call cswopi ( kxs, kxe )						!Store box
      call cswopi ( kys, kye )

      xs = kxs								!Paint box
      xe = kxe
      ys = kys
      ye = kye
      call ds_box ( xs, xe, ys, ye, 3 )



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PRAREA -- Put an area of a real array out to terminal, mult by 10000
C
C  a j penny              stsci   1988-03-20

      subroutine pr_prarea ( data, kx, ky )

      implicit none
      include 'STARMAN_INC'

      integer  kx				!i: Area X size
      integer  ky				!i: Area Y size
      real     data(kx,ky)			!i: Data in area
C--
      integer kxs, kxe, kys, kye, ka(10), j, k, ja, knx, kny, kflag,
     +        nxout
      character yflag, txt*69, text*72
Cbegin


      kxs = 1								!Get area to type out
      kxe = kx
      call get2i ( 'XAREA', kxs, kxe, .true., 1, kx )
      if ( ST_FAILED ) return
      call cswopi ( kxs, kxe )
      kys = 1
      kye = ky
      call get2i ( 'YAREA', kys, kye, .true., 1, ky )
      if ( ST_FAILED ) return
      call cswopi ( kys, kye )

      knx = 1 + (kxe-kxs)/10						!Calculate steps between values to be typed
      kny = 1 + (kye-kys)/10
      kny = -1.0*kny

      kflag = kye + kny*(((kys-kye)/kny)/2)				!Calc where to put 'Y' marker

      do k = kye, kys, kny						!Put out array
         if ( k.eq.kflag ) then
            yflag = 'Y'
         else
            yflag = ' '
         endif
         ja = 0
         do j = kxs, kxe, knx
            ja = ja + 1
            ka(ja) = min(30000,max(-30000,int(10000.0*data(j,k))))
         enddo
         write ( text, '(1x,a1,i5,3x,10i6)' ) yflag, k, (ka(j),j=1,ja)
         call printo ( text )
         call printo ( ' ' )
      enddo

      nxout = (kxe-kxs)/knx + 1						!Put out 'X' axis markers
      do k = 1, nxout
         ka(k) = kxs + knx*(k-1)
      enddo
      write ( text, '(1x,8x,10i6)' ) (ka(k),k=1,nxout)
      call printo ( text )
      txt(1:69) = ' '
      k = 11 + 6*(nxout-1)/2
      txt(k:k) = 'X'
      write ( text,'(1x,69a1)' ) (txt(k:k),k=1,69)
      call printo ( text )
      call printo ( 'The values are in units of 0.0001 of the '//
     +              'height of a star of unit height' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_BOXFR -- Smooth a real array by a rectangular box
C    It first checks that another array is full.
C
C      a j penny                       stsci      1987-01-22

      subroutine pr_boxfr ( data, adata, temp, kx, ky, ierr )

      implicit none
      include 'STARMAN_INC'

      integer	kx		!i: X size of data array
      integer	ky		!i: Y size of data array
      real	data(kx,ky)	!i/o: Data array
      real	adata(kx,ky)	!i: Check array
      real	temp(kx,ky)	!o: Temp work space
      integer   ierr		!o: Error flag (0=ok;1=bad)
C--
      logical full, again
      integer j, k, ix, iy
      integer*2 ii(150*150)						!Work space
      real    ist(150*150), nst(150*150), il(150), nl(150)
Cbegin


      ierr = 0								!Error flag

      if ( kx.gt.150 .or. ky.gt.150 ) then				!Check not too large
         call printo ( 'ERROR: Can only handle boxup to 150x150' )
         call printo ( '       in subroutine pr_boxfr' )
         ierr = 1
         return
      endif

      full = .true.							!Check to see if array is full
      do k = 1, ky
         do j = 1, kx
            if ( abs(adata(j,k)).lt.0.5 ) full = .false.
         enddo
      enddo
      if ( .not.full ) then
         call printo ( 'ERROR: Residuals array must be full' )
         ierr = 1
         return
      endif

C Get smoothing rectangle
C    Make rectangle size odd and set limits on iy to prevent the
C    number of pixels in the rectangle exceeding 32767 (this could cause
C    overflow in s/r SMOOTHBS)

      again = .true.
      do while ( again )
         ix = 3
         iy = 3
         call get2i ( 'SMSIZE', ix, iy, .true., 3, 32767 )
         if ( ST_FAILED ) return
         ix = 2*max(ix/2,0) + 1
         iy = 2*max(iy/2,0) + 1
         again = .false.
         if ( ix*iy.gt.32767 ) then
            call printo ( 'ERROR: X size times Y size must < 32767' )
            again = .true.
         endif
         if ( ix.gt.kx ) then
            call printo ( 'ERROR: X size larger than X size of array' )
            again = .true.
         endif
         if ( iy.gt.ky ) then
            call printo ( 'ERROR: Y size larger than Y size of array' )
            again = .true.
         endif
      enddo

      do k = 1, ky							!Copy into integer*2 array
         do j = 1, kx							! Scaling by x10000
           temp(j,k) = min(30000.0,max(-30000.0,10000.0*data(j,k)))
         enddo
      enddo
      call achtrs ( temp, ii, kx*ky )

      call smoothbs ( ii, kx, ky, -32767, ix, iy, 1, ist, nst, il, nl )	!apply the rectangular filter to the output image

      call achtsr ( ii, temp, kx*ky )					!Copy back to real
      call adivkr ( temp, 10000.0, data, kx*ky )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_BOXFRA -- Smooth a real array by a rectangular box
C
C      a j penny                       stsci      1987-01-22

      subroutine pr_boxfra ( data, kx, ky )

      implicit none
      include 'STARMAN_INC'

      integer	kx		!i: X size of data array
      integer	ky		!i: Y size of data array
      real	data(kx,ky)	!i/o: Data array
C--
      logical again
      integer ix, iy
      integer nst(150*150), nl(150)					!Work space
      real    rst(150*150), rl(150)
Cbegin


      if ( kx.gt.150 .or. ky.gt.150 ) then				!Check not too large
         call printo ( 'ERROR: Can only handle boxup to 150x150' )
         call printo ( '       in subroutine pr_boxfra' )
         return
      endif

C Get smoothing rectangle
C    Make rectangle size odd and set limits on iy to prevent the
C    number of pixels in the rectangle exceeding 32767 (this could cause
C    overflow in s/r SMOOTHR)

      again = .true.
      do while ( again )
         ix = 3
         iy = 3
         call get2i ( 'SMSIZE', ix, iy, .true., 3, 32767 )
         if ( ST_FAILED ) return
         ix = 2*max(ix/2,0) + 1
         iy = 2*max(iy/2,0) + 1
         again = .false.
         if ( ix*iy.gt.32767 ) then
            call printo ( 'ERROR: X size times Y size must < 32767' )
            again = .true.
         endif
         if ( ix.gt.kx ) then
            call printo ( 'ERROR: X size larger than X size of array' )
            again = .true.
         endif
         if ( iy.gt.ky ) then
            call printo ( 'ERROR: Y size larger than Y size of array' )
            again = .true.
         endif
      enddo

      call smoothr ( data, kx, ky, ix, iy, 1, rst, nst, rl, nl ) 	!apply the rectangular filter to the output image



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DISRESB -- Display two arrays (residuals/flag) or one (profile map)
C
C   a.j.penny                   ral                    86-01-02-1100

      subroutine pr_disresb ( resid, residh, wim, kx, ky, kopt )

      implicit none
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer   kx		!i: X size of array
      integer   ky		!i: Y size of array
      real      resid(kx,ky)	!i: Data array
      real      residh(kx,ky)	!i: Flag for data array (1=data;0=none)
      integer*2 wim(kx,ky)	!o: Work space
      integer   kopt		!i: Flag for array(s) to show (2=fill+data;
				!                              1=data only)
C--
      integer j, k, kxr(2), kyr(2), jpx, jpy, ierr
      real xs, xe, ys, ye, am, std, rv
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      if ( kopt.eq.2 ) then						!Display residuals filling array

         do k = 1, ky
            do j = 1, kx
               wim(j,k) = 2
               if ( residh(j,k).eq.RINVAL ) then
                  wim(j,k) = 2
               else
                  if ( residh(j,k).ne.0.0 ) wim(j,k) = 3
               endif
            enddo
         enddo
         DSVMAX = 3.0
         DSVMIN = 2.0
         jpx = DSSNX/2 + 5
         jpy = DSSNY/2 - ky/2
         call ds_acims ( wim, kx, ky, 1, kx, 1, ky, jpx, jpy, .false. )

         xs = jpx - 1							!Red ring round completeness box
         ys = jpy - 1
         xe = xs + kx + 1
         ye = ys + ky + 1
         call ds_box ( xs, xe, ys, ye, 1 )

      endif

      kxr(1) = 1							!Scale for residuals array
      kxr(2) = kx
      kyr(1) = 1
      kyr(2) = ky
      call ranger ( resid, kx, ky, kxr, kyr, RINVAL, am, std, ierr )
      DSVMIN = am + 3.0*std
      DSVMAX = am - 2.0*std

      jpx = DSSNX/2 - (kx+5)						!Display residuals array
      jpy = DSSNY/2 - ky/2
      rv = RINVAL
      call pr_loadrinval ( -1.0e20 )
      call ds_acimr ( resid, kx, ky, 1, kx, 1, ky, jpx, jpy, .false. )
      call pr_loadrinval ( rv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PREJECT -- Paint a rectangle round rejected boxes on the display
C
C     a j penny           stsci       1988-03-16

      subroutine pr_preject ( )

      implicit none
      include 'profile.inc'
C--
      real xs, xe, ys, ye
      integer jx, jya, jyb, k, ierr
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      do k = 1, TBY
         if ( RES(34,k).lt.0.5 ) then
            call pr_aboxpos ( k, TBY, LXD, LYD, jx, jya, jyb )
            xs = jx
            ys = jya
            xe = xs + LXD + 3
            ye = ys + LYD + 3
            call ds_box ( xs, xe, ys, ye, 1 )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAINTLIT -- Paint ticks around area displays, showing the calc areas
C
C         a j penny               stsci               86-11-19

      subroutine pr_paintlit ( kw )

      implicit none
      include 'profile.inc'

      integer	kw		!i: 0=all boxes; not 0 = that box
C--
      integer k, jxs, jys, jysa, ierr
      real xp, yp
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      do k = 1, TBY
         if ( kw.eq.0 .or. kw.eq.k ) then

            call pr_aboxpos ( k, TBY, LXD, LYD, jxs, jys, jysa )

            xp = jxs + LXS(k) - LXSD(k) - 1				!bottom line
            yp = jys - 2
            call ds_spot ( xp, yp, 5 )
            xp = xp + LX - 1 + 2
            call ds_spot ( xp, yp, 5 )

            yp = jys + LYD - 1 + 2					!Top line
            xp = jxs + LXS(k) - LXSD(k) - 1
            call ds_spot ( xp, yp, 5 )
            xp = xp + LX - 1 + 2
            call ds_spot ( xp, yp, 5 )

            xp = jxs - 2						!Left line
            yp = jys + LYS(k) - LYSD(k) - 1
            call ds_spot ( xp, yp, 5 )
            yp = yp + LY - 1 + 2
            call ds_spot ( xp, yp, 5 )

            xp = jxs + LXD - 1 + 2					!Right Half
            yp = jys + LYS(k) - LYSD(k) - 1
            call ds_spot ( xp, yp, 5 )
            yp = yp + LY - 1 + 2
            call ds_spot ( xp, yp, 5 )

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GRID -- Paint a frame in the area displays, showing the calc areas
C
C a j penny                     stsci                 1987-02-15

      subroutine pr_grid ( klx, kly, klxs, klys, klxd, klyd, klxsd,
     +                     klysd, tby, kw )

      implicit none

      integer   klx		!i: X size of fitting area
      integer   kly		!i: Y size of fitting area
      integer   tby		!i: Length of lists
      integer   klxs(tby)	!i: X posns of blh of fitting areas
      integer   klys(tby)	!i: Y posns of blh of fitting areas
      integer   klxd		!i: X size of display areas
      integer   klyd		!i: Y size of display areas
      integer   klxsd(tby)	!i: X posns of blh of display areas
      integer   klysd(tby)	!i: Y posns of blh of display areas
      integer	kw		!i: 0=all areas; not 0 = just (that number) area
C--
      integer k, jxs, jys, jysa, ierr
      real xs, xe, ys, ye
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      do k = 1, tby
         if ( kw.eq.0 .or. k.eq.kw ) then

            call pr_aboxpos ( k, tby, klxd, klyd, jxs, jys, jysa )
            xs = jxs + klxs(k) - klxsd(k) - 1
            ys = jys + klys(k) - klysd(k) - 1
            xe = xs + klx - 1 + 2
            ye = ys + kly -1 + 2
            call ds_box ( xs, xe, ys, ye, 5 )
            ys = ys + jysa - jys
            ye = ye + jysa - jys
            call ds_box ( xs, xe, ys, ye, 5 )

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_POLFILL -- Use the cursor to define a set of polygonal  shapes
C   The cursor is used on a displayed image. These shapes
C   are then stored as an array of 0s and 1s in an image of the
C   same size and shape as the image displayed.
C
C         a j penny               stsci                     1987-jan-25

      subroutine pr_polfill ( data, kx, ky, ierr )

      implicit none
      include 'ST_DS_GEN_INC'

      integer   kx		!i: X size of array
      integer   ky		!i: Y size of array
      integer*2 data(kx,ky)	!o: Array (0s and 1s)
      integer   ierr		!o: Error flag (0=ok;1=not)
C--
      integer ip, ix, iy, px(512), py(512), i, kbut, ierra, ixsub,
     +        iysub, ixa, iya
      real    apx, apy, bpx, bpy
      logical loop
Cbegin


      ierr = 0								!Clear error flag

      call pr_dsopen ( ierra )						!Open display, if not open
      if ( ierra.ne.0 ) then
         ierr = 1
         return
      endif

      ixsub = DSSNX/2 - (kx+5)						!Offset from screen to box
      iysub = DSSNY/2 - ky/2						! coords

      ixa = -100000
      iya = -100000
      ip = 0								!Get points
      loop = .true.							! and draw lines
      do while ( loop )
         ip = ip + 1

         call ds_gcur ( .false., ix, iy, kbut, i )			!Get point
         px(ip) = ix - ixsub
         py(ip) = iy - iysub
         px(ip) = max(1,min(px(ip),kx))
         py(ip) = max(1,min(py(ip),ky))

         if ( ip.eq.1 ) then						!Paint line
            apx = px(1) + ixsub
            apy = py(1) + iysub
            call ds_spot ( apx, apy, 2 )
         elseif ( ix.eq.ixa .and. iy.eq.iya ) then
            loop = .false.
         else
            apx = px(ip-1) + ixsub
            apy = py(ip-1) + iysub
            bpx = px(ip)   + ixsub
            bpy = py(ip)   + iysub
            call ds_line ( apx, apy, bpx, bpy, 2 )
         endif

         ixa = ix
         iya = iy
         if ( ip.ge.510 ) loop = .false.

      enddo

      apx = px(1) + ixsub						!Close up display polygon
      apy = py(1) + iysub
      bpx = px(ip) + ixsub
      bpy = py(ip) + iysub
      call ds_line ( apx, apy, bpx, bpy, 2 )

      px(ip) = px(1)							!Tidy up vertices
      py(ip) = py(1)

      call pr_fill ( px, py, ip, data, kx, ky )				!Load output with 0's outside polygon, 1's inside


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FILL -- Fill an array defined by a polygon
C
C  a j penny                  stsci             1987-jan-25

      subroutine pr_fill ( xv, yv, nv, data, kx, ky )

      implicit none

      integer   nv		!i: No of points in polygon
      integer   xv(nv)		!i: X posns of polygon corners
      integer   yv(nv)		!i: Y posns of polygon corners
      integer   kx		!i: X size of array
      integer   ky		!i: Y size of array
      integer*2 data(kx,ky)	!o: Array (0s outside polygon;1s inside)
C--
      integer j, k, nxsect, xsect(512), en, st, n, ix
      logical pr_inside
      external pr_inside
      integer*2 kone
      parameter ( kone=1 )
Cbegin


      call azeros ( data, kx*ky )					!Load array with 0's

      st = ky								!Find start line and end
      en = 1								! line for smallest box containing polygon
      do k = 1, nv
         en = max(en,yv(k))
         st = min(st,yv(k))
      enddo
      en = min(ky,en+1)
      st = max(1,st-1)

C  Find all the intersections of the line number 'k' with the
C  line segments of the polygon.
C  If regions between intersections are interior points fill
C  them in with 1's.

      do k = st, en
         call pr_xsect ( xv, yv, nv, k, xsect, nxsect )
         if ( nxsect.gt.1 ) then
            do j = 2, nxsect
               ix = (xsect(j-1) + xsect(j))/2
               if ( pr_inside(xv,yv,nv,ix,k) ) then
                  n = xsect(j) - xsect(j-1) + 1
                  call amovks ( kone, data(xsect(j-1),k), n )
               endif
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_XSECT -- Find intersections of polygon with a given horizontal line
C
C This finds the number of such intersections and the X positions of
C those intersections. Where the polygon line is horizontal, those
C intersections are ignored.
C
C  a j penny                     stsci           1987-01-25

      subroutine pr_xsect ( xv, yv, nv, kl, xsect, nxsect )

      implicit none

      integer nv		!i: No of lines in polygon
      integer xv(nv)		!i: X positions of vertices
      integer yv(nv)		!i: Y positions of vertices
      integer kl		!i: Y posn of horizontal line
      integer xsect(512)	!o: X positions of intersections
      integer nxsect		!o: No of intersections
C--
      integer x1, x2, y1, y2, i
Cbegin


      nxsect = 0

      do i = 2, nv
         y1 = yv(i-1)
         y2 = yv(i)
         if ( y1.ne.y2 .and. kl.ge.min0(y1,y2) .and.
     +        kl.le.max0(y1,y2) )  then
            nxsect = nxsect + 1
            x1 = xv(i-1)
            x2 = xv(i)
            xsect(nxsect) = 0.5 + real(x1) +
     +                       real(x2-x1)*real(kl-y1)/real(y2-y1)
         endif
      enddo

      if ( nxsect.gt.1 ) call sort1i ( xsect, nxsect )			!Sort them


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_INSIDE -- (Function) See if inside the polygon
C  Uses cauchy's integral theorem to test for inside. connect test
C  point to each vertex in turn and if the sum of the rotation angles
C  is zero then the point is exterior
C
C  a j penny                          stsci           1987-01-26

      logical function pr_inside ( xv, yv, nv, xpos, ypos )

      implicit none

      integer nv
      integer xv(nv)
      integer yv(nv)
      integer xpos
      integer ypos
C--
      real angsum
      integer i
      real pr_rangle
      external pr_rangle
Cbegin


      pr_inside = .false.
      if ( nv.ge.3 ) then
         angsum = 0.0
         do i = 2, nv
            angsum = angsum + pr_rangle(xpos,ypos,xv(i-1),
     +                                  yv(i-1),xv(i),yv(i))
         enddo
         if ( abs(angsum).gt.3.1416 )  pr_inside = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_RANGLE -- (Function) Find the rotation angle
C  Find the rotation angle between (x1,y1) and (x2,y2) relative
C  to the central point (x0,y0). It is calculated using the ratio of
C  the cross product(prop to sin(ang)) to the dot product(prop to cos)
C  the sense +/- is important.
C
C  a j penny                   stsci             1987-01-25

      real function pr_rangle ( x0, y0, x1, y1, x2, y2 )

      implicit none

      integer  x0
      integer  x1
      integer  x2
      integer  y0
      integer  y1
      integer  y2
C--
      real cross, dot, xx1, xx2, yy1, yy2
Cbegin


      xx1 = x1 - x0
      yy1 = y1 - y0
      xx2 = x2 - x0
      yy2 = y2 - y0

      cross = xx1*yy2 - xx2*yy1
      dot   = xx1*xx2 + yy1*yy2

      pr_rangle = 1.5708							!Check for right angle
      if ( abs(dot).gt.0.00001 )  then
         pr_rangle = atan(abs(cross/dot))
         if ( dot.lt.0.0 )  pr_rangle = 3.1416 - pr_rangle
      endif

      if ( cross.lt.0.0 )  pr_rangle = -pr_rangle


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_POLDO -- Take a mask and zeroes/smooths an area of an array
C
C  a j penny                      stsci           1987-01-26

      subroutine pr_poldo ( data, kx, ky, rd, rdh, kopt )

      implicit none

      integer   kx		!i: X size of mask array
      integer   ky		!i: Y size of mask array
      integer*2 data(kx,ky)	!i: Mask Array (1=in area;0=out of area)
      real      rd(kx,ky)	!i/o: Data to work on
      real      rdh(kx,ky)	!i/o: Flags for rd to have entry
      integer   kopt		!i: Flag (1=smooth in;2=smooth out;3=zero in;
				!         4=zero out)
C--
      integer j, k, ip, ipa, i, ierr
      real    rv
Cbegin


      if ( kopt.eq.1 .or. kopt.eq.2 ) then

         call gtwrkr ( 'KPOLYA', kx*ky, ip, i )
         call gtwrkr ( 'KPOLYB', kx*ky, ipa, i )
         call amovr ( rd, %val(ip), kx*ky )
         call pr_boxfr ( %val(ip), rdh, %val(ipa), kx, ky, ierr )
         if ( ierr.ne.0 ) then
            call wrkcan ( 'KPOLYA' )
            call wrkcan ( 'KPOLYB' )
            return
         endif

         if ( kopt.eq.1 ) then
            do k = 1, ky
               do j = 1, kx
                  if ( data(j,k).eq.1 ) then
                     call copr1 ( %val(ip), kx, ky, j, k, rv )
                     rd(j,k) = rv
                     rdh(j,k) = 1.0
                   endif
               enddo
            enddo
         elseif ( kopt.eq.2 ) then
            do k = 1, ky
               do j = 1, kx
                  if ( data(j,k).eq.0 ) then
                     call copr1 ( %val(ip), kx, ky, j, k, rv )
                     rd(j,k) = rv
                     rdh(j,k) = 1.0
                   endif
               enddo
            enddo
         endif
         call wrkcan ( 'KPOLYA' )
         call wrkcan ( 'KPOLYB' )
      elseif ( kopt.eq.3 ) then
         do k = 1, ky
            do j = 1, kx
               if ( data(j,k).eq.1 ) then
                  rd(j,k) = 0.0
                  rdh(j,k) = 1.0
                endif
            enddo
         enddo
      elseif ( kopt.eq.4 ) then
         do k = 1, ky
            do j = 1, kx
               if ( data(j,k).eq.0 ) then
                  rd(j,k) = 0.0
                  rdh(j,k) = 1.0
                endif
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_INFILL -- Fill in empty pixels in the array RD, where an empty
C   pixel is flagged by 0.0 in the matching RDH pixel. It does it
C   by interpolation.
C
C  a j penny                       stsci                    1987-02-16

      subroutine pr_infill ( rd, rdh, kx, ky, donecalc )

      implicit none

      integer kx		!i: X size of array
      integer ky		!i: Y size of array
      real    rd(kx,ky)		!i/o: Array
      real    rdh(kx,ky)	!i: Flagging array
      logical donecalc		!i: Calculated the values?
C--
      integer j, k, kky(2), kkx(2,2), ja, ka, jx, jy, jdx, jdy,
     +        kva, kvb, kvy, jjx(2)
      real vala, valb, val, diff, res(2), fk
      logical more, morea
Cbegin


      if ( .not.donecalc ) return

      do k = 1, ky
         do j = 1, kx
            if ( rdh(j,k).eq.0.0 ) then
               kky(1) = 0
               do jy = 1, 2
                  if ( (jy.eq.2.and.kky(1).ne.k) .or. jy.eq.1 ) then
                     jdy = 1
                     if ( jy.eq.2 ) jdy = -1
                     morea = .true.
                     ka = k - jdy
                     do while ( morea )
                        ka = ka + jdy
                        if ( ka.gt.ky .or. ka.lt.1 ) then
                           morea = .false.
                        else
                           jjx(1) = kx + 1
                           jjx(2) = 0
                           do jx = 1, 2
                              jdx = 1
                              if ( jx.eq.2 ) jdx = -1
                              more = .true.
                              ja = j - jdx
                              do while ( more )
                                 ja = ja + jdx
                                 if ( (jx.eq.1.and.ja.gt.kx) .or.
     +                                (jx.eq.2.and.ja.lt.1) ) then
                                    more = .false.
                                 else
                                    fk = rdh(ja,ka)
                                    if ( fk.ne.0.0 ) more = .false.
                                 endif
                              enddo
                              jjx(jx) = ja
                           enddo
                        endif
                        if ( jjx(1).le.kx .or. jjx(2).ge.1 ) then
                           morea = .false.
                        endif
                     enddo
                     kkx(1,jy) = jjx(1)
                     kkx(2,jy) = jjx(2)
                     kky(jy) = ka
                  endif
               enddo

               if ( kky(1).eq.k ) then
                  kky(2) = kky(1)
                  kkx(1,2) = kkx(1,1)
                  kkx(2,2) = kkx(2,1)
               endif
               if ( kky(1).gt.ky ) then
                  kky(1) = kky(2)
                  kkx(1,1) = kkx(1,2)
                  kkx(2,1) = kkx(2,2)
               elseif ( kky(2).lt.1 ) then
                  kky(2) = kky(1)
                  kkx(1,2) = kkx(1,1)
                  kkx(2,2) = kkx(2,1)
               endif

               do jy = 1, 2
                  res(jy) = 0.0
                  kva = kkx(1,jy)
                  kvb = kkx(2,jy)
                  kvy = kky(jy)
                  if ( kvy.lt.1 .or. kvy.gt.ky ) then
                     res(jy) = 0.0
                  elseif ( kvb.lt.1 ) then
                     if ( kva.ge.1 .and. kva.le.kx ) then
                        res(jy) = rd(kva,kvy)
                     endif
                  elseif ( kva.gt.kx ) then
                     if ( kvb.ge.1 .and. kvb.le.kx ) then
                        res(jy) = rd(kvb,kvy)
                     endif
                  else
                     if ( (kva.ge.1 .and. kva.le.kx) .and.
     +                    (kvb.ge.1 .and. kvb.le.kx) ) then
                        vala = rd(kva,kvy)
                        valb = rd(kvb,kvy)
                        if ( kva.ne.kvb ) then
                           val = (vala-valb)*real(j-kvb)
                           res(jy) = valb + (val/real(kva-kvb))
                        else
                           res(jy) = vala
                        endif
                     endif
                  endif
              enddo

              val = 0.0
              if ( kky(1).eq.kky(2) ) then
                 val = res(1)
              else
                 diff = real(kky(1)-k)/real(kky(1)-kky(2))
                 val = res(1) + (res(2)-res(1))*diff
              endif
              rd(j,k) = val

            endif

         enddo
      enddo

      call amovkr ( 1.0, rdh, kx*ky )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTCEN -- Enable you to move the posn of the centre stars
C
C  a j penny                       stsci                1987-02-27

      subroutine pr_gtcen ( kxd, kyd, tres, kxsd, kysd )

      implicit none
      include 'profile.inc'

      integer  kxd		!i: X size of display areas
      integer  kyd		!i: Y size of display areas
      real     tres(TBX,TBY)	!i/o: Fit results array
      integer  kxsd(TBY)	!i: X posns of blh of display areas
      integer  kysd(TBY)	!i: Y posns of blh of display areas
C--
      integer kx, ky, kbut, ierr, nbox, k, jx, jy, kdhy
      real x, ya, yb
Cbegin


      call pr_dsopen ( ierr )						!Open display, if not open
      if ( ierr.ne.0 ) return

      call ds_gcur ( .false., kx, ky, kbut, ierr )			!Get cursor posn

      call pr_gtnbox ( kx, ky, TBY, kxd, kyd, nbox, jx, jy, kdhy )	!Find which box it is in. Return if none.
      if ( nbox.eq.0 ) return

      tres(1,nbox) = kx - jx + 1 + kxsd(nbox) - 1			!Store posn
      tres(2,nbox) = ky - jy + 1 + kysd(nbox) - 1
      tres(36,nbox) = 3.0

      do k = 1, TBY							!Paint present positions
         call pr_boxpos ( k, TBY, kxd, kyd, kxsd, kysd, tres(1,k),
     +                    tres(2,k), x, ya, yb )
         if ( PAINTSPOT ) then
            call ds_spot ( x, ya, 1 )
            call ds_spot ( x, yb, 1 )
         else
            call ds_cross ( x, ya, CWIDTH, 1 )
            call ds_cross ( x, yb, CWIDTH, 1 )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GTNBOX -- Find out which box a display position is in. If in upper box,
C              posn is moved to lower.
C
C  a j penny                      stsci             1987-02-21

      subroutine pr_gtnbox ( kx, ky, tbya, kxd, kyd, nbox, jxa, jya,
     +                       kdhy )

      implicit none

      integer kx		!i: X position in display
      integer ky		!i/o: Y position in display (moved to lower in if upper)
      integer tbya		!i: No of boxes
      integer kxd		!i: X size of each box
      integer kyd		!i: Y size of each box
      integer nbox		!o: Which box (=0 if not in a box)
      integer jxa		!o: X position in that box
      integer jya		!o: Y position in that box
      integer kdhy		!o: Distance from lower to upper boxes
C--
      integer k, jxb, jyb, jyaa, jyba
Cbegin


      nbox = 0								!Go through all boxes,
      k = 1								! seeing if in
      do while ( k.le.tbya .and. nbox.eq.0 )

         call pr_aboxpos ( k, tbya, kxd, kyd, jxa, jya, jyaa )
         jxb = jxa + kxd - 1
         jyb = jya + kyd - 1
         if ( kx.ge.jxa .and. kx.le.jxb .and. ky.ge.jya .and.
     +        ky.le.jyb ) nbox = k

         jyba = jyaa + kyd - 1
         if ( kx.ge.jxa .and. kx.le.jxb .and. ky.ge.jyaa .and.
     +        ky.le.jyba ) then
            nbox = k
            ky = ky - (jyaa-jya)
         endif

         k = k + 1

      enddo

      kdhy = jyaa - jya


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_VALSTARS -- Find pixel value due to main + companion stars
C
C   a.j.penny                   stsCi                 1987-02-20

      subroutine pr_valstars ( j, k, val, tres, kstar, tcom )

      implicit none
      include 'profile.inc'

      integer  j		!i: X posn in main star profile
      integer  k		!i: Y posn in main star profile
      real     val		!o: Pixel value
      real     tres(TBX,TBY)	!i: Fit results array
      integer  kstar		!i: Star being done
      real     tcom(6,MAXTOT)	!i: comps/bad areas array
C--
      real hp, xp, yp, theta, co, si, sim, qh, qr, qp,
     +     vala, ap, gx, gy, hx, hy, hx2, hy2, gx2, gy2, prof(9)
      integer kx, ky, l
Cbegin


      val = 0.0								!Default

      if ( tres(9,kstar).eq.0.0 ) return				!Return if height zero

      call amovr ( tres(12,kstar), prof, 9 )				!Load profile

      if ( prof(1).le.0.0 .or. prof(2).le.0.0 .or. prof(4).le.0.0 .or.	!Check to see if profile radii all +ve
     +     prof(5).le.0.0 ) return
      if ( prof(7).ne.0.0 .and. prof(8).le.0.0 ) return

      gx = 1.0/prof(1)							!Load image parameters
      gy = 1.0/prof(2)
      hx = 1.0/prof(4)
      hy = 1.0/prof(5)
      theta = prof(6)
      co = cos(theta)
      si = sin(theta)
      sim = -1.0*si

      qh = prof(7)
      if ( qh.ne.0.0 ) qr = 1.0/prof(8)
      qp = prof(9)

      call subdiv ( prof(1), prof(2), kx )				!Preload more image parameters
      ky = kx
      ap = prof(3)/2.0
      gx2 = 1.0/(prof(1)*prof(1))
      gy2 = 1.0/(prof(2)*prof(2))
      hx2 = 1.0/(prof(4)*prof(4))
      hy2 = 1.0/(prof(5)*prof(5))

      xp = tres(1,kstar)						!Do Main star
      yp = tres(2,kstar)
      hp = tres(9,kstar)

      call profval ( vala, j, k, xp, yp, hp, kx, ky, co, si, sim, 	!Get the profile summed
     +               gx2, gy2, ap, hx2, hy2, qh, qr, qp, DOMAP,		! over the pixel
     +               %val(IPMAP), MX, MY, MZ, 1, MAPX, MAPY, MAGNIF )
      val = val + vala

      do l = 1, MAXTOT							!Do the companions

         if ( nint(tcom(6,l)).eq.1 .or. nint(tcom(6,l)).eq.2 ) then
            if ( nint(abs(tcom(5,l))).eq.kstar ) then
               xp = tcom(1,l)
               yp = tcom(2,l)
               hp = tcom(3,l)
               if (hp.ne.0.0) then
                  call profval ( vala, j, k, xp, yp, hp, kx, ky, co,
     +                           si, sim, gx2, gy2, ap, hx2, hy2, qh,
     +                           qr, qp, DOMAP, %val(IPMAP), MX, MY,
     +                           MZ, 1, MAPX, MAPY, MAGNIF )
                  val = val + vala
               endif
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DSCLEAR -- Clear display if open
C
C  alan penny               ral                     1990-05-24

      subroutine pr_dsclear ( )

      implicit none
      include 'profile.inc'
C--
Cbegin


      if ( PRDSOK ) call ds_erase


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DSOPEN -- Open display if not already open
C
C  alan penny               ral                     1990-05-24

      subroutine pr_dsopen ( ierr )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer   ierr		!o: Error flag (0=ok)
C--
      integer nsq, nsqy
Cbegin


      ierr = 0
      if ( .not.PRDSOK ) then						!Open display, if not already
         call ds_gtype ( ierr )
         if ( ierr.ne.0 ) return
         nsq = 1 + int(sqrt(2.0*real(TBY)))
         nsqy = 1 + ((TBY-1)/nsq)
         DSNXS = 1
         DSNXE = 60 + nsq*(LXD+6)
         DSNXE = 2*DSNXE
         DSNYS = 1
         DSNYE = 60 + 4 + 2*nsqy*(LYD+6)
         DSNYE = 2*DSNYE
         if ( IMTITLE.eq.' ' ) then
            call ds_init ( 'PROFILE display', 0, ierr )
         else
            call ds_init ( IMTITLE, 0, ierr )
         endif
         if ( ierr.ne.0 ) then
            call printo ( 'ERROR: Cannot open display' )
            return
         else
            PRDSOK = .true.
            call pr_dsclear
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_LOADRINVAL -- Load a Real 'INVALID' value into the image common area
C
C  alan penny               ral                     1990-05-24

      subroutine pr_loadrinval ( rv )

      implicit none
      include 'ST_IMAGE_INC'

      real    rv        !i: 'INVALID' pixel flag value
C--
Cbegin


      RINVAL = rv


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is PROSUBPAR_FAKE.FOR
C
C  It has subroutines with NO calls to prallel processors
C
C  Contains Parallel Processing s/rs:-
C
C PR_PARLOAD    Load parallel processors
C PR_PAROUT     Send data (image,map,params in common block) to par processors
C PR_PARIN      Take array from par processors, store results in common block
C PR_PARTOOBIG  Check if fit needs are too big for parallel processors
C PR_PAR_SII    Call PAR_SII
C PR_PAR_FEND   Call PAR_FEND



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PARLOAD -- Load parallel processors
C
C   a.j.penny                   ral                    1991 July

      subroutine pr_parload ( istat )

      implicit none
      include 'profile.inc'

      integer   istat		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      istat = 1

      call printo ( 'ERROR: s/r PR_PARLOAD called' )
      call printo ( '       Parallel processors not available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAROUT -- Send data (image,map,params in common block) to par processors
C
C    alan penny                ral              1989-08-14

      subroutine pr_parout ( datar, lxa, lya, map, tfixangle, tfixp,
     +                       kstar, kfix, cc, jfit, numm, ccf, jfitf,
     +                       numf, parfix, jxs, jys, nin, iterlim )

      implicit none

      include 'profile.inc'
      include 'ST_IMAGE_INC'

      integer   lxa			!i: X size of array
      integer   lya			!i: Y size of array
      real      datar(lxa*lya)		!i: Input array to fit
      real	map(MX*MY*1)		!i: profile map
      logical   tfixangle		!i: Profile angle fixed (Y) or var (N)
      logical   tfixp(5)		!i: Profile pars fixed (true) or var
      integer   kstar			!i: Number of star being done
      integer   kfix			!i: Profile fix option (0=no;1=yes)
      real      cc(36)			!i: Fit input parameters
      integer   jfit(36)		!i: Fit parameters controls
      integer   numm			!i: Number of bright/close companions
      real      ccf(36)			!i: Profile and posns for faint/distant comps
      integer   jfitf(36)		!i: Controls for profile and posns for faint/distant comps
      integer   numf			!i: Number of faint/far companions
      logical   parfix			!i: Fix profile flag
      integer   jxs			!i: X posn in image of blh of box
      integer   jys			!i: Y posn in image of blh of box
      integer   nin			!i: No of INVALID pixels in box
      integer   iterlim			!i: Max no of iterations allowed
C--
Cbegin


      call printo ( 'ERROR: s/r PR_PAROUT called' )
      call printo ( '       Parallel processors not available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PARIN -- Take array from par processors, store results in common block
C
C    alan penny                ral              1989-08-14

      subroutine pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin, lxa,
     +                      lya )

      implicit none

      integer   jstar			!o: Number of star done
      real      cc(36)			!o: Fit output parameters
      real      ccf(36)			!o: Profile and posns for faint/distant comps
      integer   jxs			!o: X posn in image of blh of box
      integer   jys			!o: Y posn in image of blh of box
      integer   iter			!o: No of iterations in fit
      integer   nin			!o: No of INVALID pixels in box
      integer   lxa			!o: X size of array
      integer   lya			!o: Y size of array
C--
      integer k
Cbegin


      do k = 1, 36
         cc(k) = 0.0
         ccf(k) = 0.0
      enddo
      jstar = 0
      jxs = 0
      jys = 0
      iter = 0
      nin = 0
      lxa = 0
      lya = 0

      call printo ( 'ERROR: s/r PR_PARIN called' )
      call printo ( '       Parallel processors not available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PARTOOBIG -- Check if fit needs are too big for parallel processors
C
C     a j penny                ral                  1990-05-07

      subroutine pr_partoobig ( ks, toobig )

      implicit none

      include 'profile.inc'

      integer	ks		!i: Star number
      logical	toobig		!o: True = too big for par procs, false = not
C--
Cbegin


      toobig = .false.

      if ( .not.DOPAR ) return

      call printo ( 'ERROR: s/r PR_PARTOOBIG called' )
      call printo ( '       Parallel processors not available' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAR_SII -- Call par_sii
C
C   a.j.penny                   ral                    1991 July

      subroutine pr_par_sii (  numpar, istat )

      implicit none
      include 'profile.inc'

      integer   numpar		!o: Number of parallel processors
      integer   istat		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      call printo ( 'ERROR: s/r PR_PAR_SII called' )
      call printo ( '       Parallel processors not available' )

      istat = 0

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_PAR_FEND -- Call PAR_FEND
C
C   a.j.penny                   ral                    1991 July

      subroutine pr_par_fend (  )

      implicit none
      include 'profile.inc'

C--
Cbegin


      call printo ( 'ERROR: s/r PR_PAR_FEND called' )
      call printo ( '       Parallel processors not available' )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PROSUBW.FOR  Software for the wings
C
C    Contains:-
C
C PR_WING       Find the profile wings
C PRW_OPT_SETUP Set up option choices
C PR_WFIT       Calculate the star fits for wing option
C PR_WINGCAL    Calculate wing fit
C PR_WGAUSS     Calculate wing fit from radial data
C PR_WFITL      Fit wing line data
C PR_GCUR       Get wing plot cursor positions
C PR_WPQLINE    Plots out a wing profile in 1-D
C PR_WTYPE      Type out fit params



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WING -- Find the profile wings
C
C alan penny                 ral                    90-05-09

      subroutine pr_wing ( data, store, tota, totb, amax, amin )

      implicit none
      include 'profile.inc'
      include 'profw.inc'
      include 'STARMAN_INC'
      include 'ST_DS_PANEL_INC'

      real       data(LXW,LYW)		!o: Work space for wing image
      real       store(LXW,LYWN)	!o: Work space for wing image
      real       tota(LX,LY)		!o: Work space for star image
      real       totb(LX,LY)		!o: Work space for star image
      real       amax			!i: Max display value
      real       amin			!i: Min display value
C--
      logical loop
      character*12 ktopt
Cbegin


      QH = SR(7)
      QR = SR(8)
      QP = SR(9)
      BASE = QBASE
      call azeroi ( JFF, 4 )
      FIRSTF = .true.
      FIRSTP = .true.
      WSTARCALC = .false.
      GOTXRANGE = .false.

      call amovr ( RES, RESA, TBX*TBY )					!Load working values
      call amovr ( COM, COMA, 6*MAXTOT )

      call prw_opt_setup ( ktopt, 4, .true. )				!Do desired options
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )

         call prw_opt_setup ( ktopt, 4, .false. )
         call get_choice ( ktopt, 1 )					!Get choice
         if ( ST_FAILED ) return

         if ( ktopt.eq.'display' ) call pr_dispfit ( 0, 1, LXW, LYW,	!Display residuals
     +                                        LXSW, LYSW, LXWD, LYWD,
     +                                        LXSWD, LYSWD )

         if ( ktopt.eq.'interact' ) call pr_dfinter ( RESA, COMA, 	!Display residuals and get more
     +                                           LXW, LYW, LXSW, LYSW, 	! comps and bad areas, possibly
     +                                      LXWD, LYWD, LXSWD, LYSWD )

         if ( ktopt.eq.'fit_stars' ) call pr_wfit ( data, store, tota,	!Calculate the star fits for wing
     +                                              totb )

         if ( ktopt.eq.'set_param' ) then				!Change result
                          call get1r ( 'HEIGHT', QH, QH, -1.e10, 1.e10)
                          call get1r ( 'POWER', QP, QP, 0.1, 100.0 )
                          call get1r ( 'RADIUS', QR, QR, 0.1, 1.0e5 )
                          call get1r ( 'BASE', BASE, BASE,-1.e10,1.e10)
                          if ( ST_FAILED ) return
                          endif

         if ( ktopt.eq.'fit_graw' ) call pr_wgauss ( 1 )		!

         if ( ktopt.eq.'fit_gcursor' ) call pr_wgauss ( 2 )		!

         if ( ktopt.eq.'gcursor' ) call pr_wgauss ( 3 )			!

         if ( ktopt.eq.'gplot' ) call pr_wgauss ( 4 )			!

         if ( ktopt.eq.'type_param' ) call pr_wtype			!Type parameters

         if ( ktopt.eq.'gshowfit' ) then				!Display fitted line
                           if ( GOTXRANGE ) call pr_wpqline
                           endif
         if ( ktopt.eq.'gclose' ) call gd_close 			!Close the graph device

         if ( ktopt.eq.'return' ) loop = .false.			!Exit

         if ( ST_FAILED ) return
      enddo

      SR(7) = QH							!Exit and store result
      SR(8) = QR
      SR(9) = QP
      QBASE = BASE


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRW_OPT_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 Jan

      subroutine prw_opt_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside           !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=12 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'display',  'Display the areas + residuals to the fits',
     + 'Display the areas around each star and the areas after the',
     + 'fitted stars have been subtraced. The contrast in each area is',
     + 'maximised to show detail, so will be different between stars. ',
     + 'The -fitted- area is the -normal- size and is half the size of',
     + 'the purple box, in the area seen, The box shows the area used ',
     + 'for the mean radial profile. Companions, bad areas are shown.'/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'fit_stars', 'Fit stars in the large areas round each star',
     + 'The program goes through the star list calculating, for the ',
     + 'large box, the residuals from the profile already fitted to ',
     + 'the -normal- sized box, and loading them into a mean radial',
     + 'radial profile of the residuals. Stars which have been ',
     + '-rejected- are not fitted. As this fitting follows a -normal-',
     + 'fixed profile fit, you are offered a chance to do it.' /

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'set_param', 'Insert your own wing profile values',
     + 'You can change the parameters of the wing profile.',
     + ' ',
     + 'The wing equation is:-',
     + '   Val = Base + [-main height- * ht * exp(-1.0*dd)]',
     + '   where dd = [(distance from star centre)/radius)]**power',
     + ' ' /

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'return', 'Return to main option',
     + ' ', 'Return to main program option list.' ,
     + ' ', ' ', ' ', ' ' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'type_Param', 'Type present wing parameters',
     + ' ',
     + 'Type out the present values of the wing parameters,',
     + 'and whether or not the parameters are fixed of can vary ',
     + 'in the fitting.',
     + ' ', ' ' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'gplot', 'Plot the wing points graphically',
     + 'This opens a graphics window and gives a graph of the average',
     + 'residuals in annular bins round the star centres. It is thus a',
     + '1-D plot of -residual vs distance from star-, where residuals',
     + 'are averaged between stars and in azimuth round the star',
     + 'centres. The -wing height- shown is residuals height in terms',
     + 'of a star of unit height. (Fit and cursor pts also shown.)' /

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'gcursor', 'Put -better- wing points in by cursor',
     + 'The -gplot- option plot of -residuals vs distance from star-  ',
     + 'may show a lot of scatter. You can put an alternative -smooth-',
     + 'curve, by marking points using a cursor. This option plots the',
     + 'graph. You then mark points by placing cursor and pressing the',
     + 'keyboard space bar. Stop by marking point to left of axis. ',
     +'(The option -fit_gcursor- can now be used to fit these points.)'/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'fit_gcursor', 'Fit to wing points input by cursor',
     + 'Fit the data, input via the cursor, with a simple modified ',
     + 'Gaussian. The data and present fit are plotted. The present',
     + 'parameter values are given, with the chance to modify them,',
     + 'and to select which parameters to hold fixed in the fitting. ',
     + 'The progress of the fit is shown and the results plotted and',
     + 'typed out. [See the -set_param- option for the fit equation.]' /

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'fit_graw', 'Fit to raw wing points as calculated',
     + 'Fit the raw data with a simple modified Gaussian.',
     + 'The data and present fit are plotted. The present',
     + 'parameter values are given, with the chance to modify them,',
     + 'and to select which parameters to hold fixed in the fitting. ',
     + 'The progress of the fit is shown and the results plotted and',
     + 'typed out. [See the -set_param- option for the fit equation.]' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'gshowfit', 'Plot fitted line',
     + ' ',
     + 'Plot the present fit on the graph of the radial data.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'interact',  'Interact with the display of fits',
     + 'Look in more detail at the fits and add companion stars, etc.',
     + ' ',
     + 'CHANGES: Change the companions; change bad areas; move the',
     + '         main stars.',
     + 'DISPLAY: Pan/zoom the fits display; type out the image or ',
     + '         residuals values; remove/replace coloured lines.' /

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'gclose', 'Close the graphics device',
     + ' ',
     + 'Close the graphics device',
     + ' ',
     + '(This is useful if you want to put out the graph on a ',
     + ' different graphics device.)',
     + ' ' /

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Profile - Wing', 'WOPTION', 4 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'fit_stars' /

      integer sect_num
      parameter ( sect_num=4 )
      character*10 sect_head(sect_num)
      data sect_head / 'FIT_STARS', 'PLOTTING', 'FITTING', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'fit_stars:display:interact' /
      data sect_text(2) / 'gplot:gcursor:gshowfit:gclose' /
      data sect_text(3) / 'fit_gcursor:fit_graw:set_param:type_param' /
      data sect_text(4) / 'return' /

      integer help_num
      parameter ( help_num=19 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + 'A normal course might be to:-',
     + ' -Display-     Display the x2 areas round the stars',
     + ' -Interact-    Blank off or mark up the extra stars in ',
     + '                the large wing areas',
     + ' -Fit_stars-   Fit the stars in the large wing area and calc ',
     + '                the distribution of wing data points' ,
     + ' -Gplot        Look at the wing data points' ,
     + ' -Gcursor      Put your own smoothly distributed wing data' /
      data (help_text(k),k=11,help_num) /
     + '                 points by using the graphics cursor' ,
     + ' -Fit_gcursor  Fit the wing parameters to your cursor input',
     + '              ',
     + 'After this you can loop round changing the cursor positions',
     + '  and/or refining the fit. The fit is usally very delicate',
     + '  and it is common to adjust the profile parameters by hand',
     + '  before a number of fits to get a good fit',
     + '               ' ,
     + 'Then -Return-  back to the main option.' /

Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WFIT -- Calculate the star fits for wing option
C
C   a j penny                 ral     1990-05-21

      subroutine pr_wfit ( data, store, tota, totb )

      implicit none
      include 'profile.inc'
      include 'profw.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real       data(LXW,LYW)		!o: Work space for wing image
      real       store(LXW,LYWN)	!o: Work space for wing image
      real       tota(LX,LY)		!o: Work space for image data
      real       totb(LX,LY)		!o: Work space for image data
C--
      integer k, lyas, jxs, jxe, jys, jye, jfit(36), jfitf(36), numpar,
     +        istat, ksent, lxa, lya, nin, iter, jstar, numm, numf
      real cc(36), ccf(36)
      logical kfixed(5), done, toobig, parfix
      data kfixed / .true., .true., .true., .true., .true. /
Cbegin


      numpar = 1

      call get1b ( 'WFITOK', done, .false. )
      if ( ST_FAILED ) return
      if ( .not.done ) then

         if ( DOPAR ) call pr_par_sii ( numpar, istat )			!Number of par processors

         ksent = 0
         do k = 1, TBY							!Do all the stars
            if ( RESA(34,k).gt.0.5 ) then

               call pr_partoobig ( k, toobig )
               if ( DOPAR .and. .not.toobig ) then			!Use par processors
                  if ( ksent.ge.numpar ) then
                     call pr_parin ( jstar, cc, ccf, jxs, jys, iter,
     +                               nin, lxa, lya )
                     call pr_dores ( jstar, RESA, TBX, TBYMAX, COMA,
     +                               MAXTOT, 0, cc, ccf, jxs, jys,
     +                               iter, nin, lxa, lya, MAXCL,
     +                               MAXFA )
                  endif
                  call pr_fitload ( totb, .true., .true., SR, k,
     +                              RESA, COMA, 1, cc, jfit, numm,
     +                              ccf, jfitf, numf, parfix, LX, LY,
     +                              jxs, jys, nin )
                  call pr_parout ( totb, LX, LY, %val(IPMAP), .true.,
     +                             .true., k, 1, cc, jfit, numm, ccf,
     +                             jfitf, numf, parfix, jxs, jys, nin,
     +                             20 )
                  ksent = ksent + 1
               else
                  call pr_solve ( tota, totb, .true., kfixed, SR, 20, 	!Solve for this star
     +                            k, RESA, COMA, 0, 1 )
               endif

            endif
         enddo

         if ( DOPAR ) then
            if ( ksent.eq.0 ) then
               call pr_par_fend
            else
               do k = 1, min(ksent,numpar)
                  if ( k.eq.1 .and. ksent.lt.numpar ) call pr_par_fend
                  call pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin,
     +                            lxa, lya )
                  call pr_dores ( jstar, RESA, TBX, TBYMAX, COMA,
     +                            MAXTOT, 0, cc, ccf, jxs, jys, iter,
     +                            nin, lxa, lya, MAXCL, MAXFA )
                  if ( k.eq.1 .and. ksent.ge.numpar ) call pr_par_fend
               enddo
            endif
         endif

      endif

      do k = 1, TBY
         if ( RESA(34,k).gt.0.5 ) then					!Do all good stars

            LXSW(k) = RESA(1,k) - real((LXW/2)-1)			!Load the residuals arrays
            LYSW(k) = RESA(2,k) - real((LYW/2)-1)			! into work area
            jxs = LXSW(k)
            jxe = jxs + LXW - 1
            jys = LYSW(k)
            jye = jys + LYW - 1
            if ( IMTYPE.eq.'SHORT' ) then
               call copvsr ( %val(IPIM), NX, NY, data, LXW, LYW, jxs,
     +                       jxe, jys, jye, BS, BZ, INVAL, RINVAL, nin,
     +                       0 )
            else
               call copvrr ( %val(IPIM), NX, NY, data, LXW, LYW, jxs,
     +                       jxe, jys, jye, BS, BZ, RINVAL, nin, 0 )
            endif
            call pr_allsub ( data, LXW, LYW, LXSW(k), LYSW(k), 0,
     +                       1, 1, RESA, COMA, k )
            call pr_loadbl ( data, LXW, LYW, COMA, jxs, jys, k )
            lyas = 1 + (k-1)*LYW
            call coprr ( data, LXW, LYW, 1, LXW, 1, LYW,
     +                   store, LXW, LYWN, 1, lyas )

         endif
      enddo

      WSTARCALC = .true.
      call pr_wingcal ( data, store )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WINGCAL -- Calculate wing fit
C
C   a j penny                 dao           1988-04-25

      subroutine pr_wingcal ( data, store )

      implicit none
      include 'profile.inc'
      include 'profw.inc'
      include 'ST_IMAGE_INC'

      real      data(LXW,LYW)		!o: Work space
      real      store(LXW,LYWN)		!i: Image wing areas for all stars
C--
      real xca, yca, ymax, dr, drmax, dx, dy, val, ab
      integer k, kgap, next, lyas, lyae, kd, j, ks, jxs, jys
      logical found
Cbegin


      if ( .not.WSTARCALC ) then					!Check star fits done for wing
         call printo (
     +        'ERROR: You must do the star fits for the wing first' )
         return
      endif

      xca = real(LXW)/2.0
      yca = real(LYW)/2.0
      xmax = max ( (xca-1.0), (real(LXW)-xca) )
      ymax = max ( (yca-1.0), (real(LYW)-yca) )
      drmax = sqrt(xmax*xmax+ymax*ymax)

      call azeror ( APNUM, NWMAX )					!Zero radial profile values and
      call azeror ( X, NWMAX )						! residuals and total height
      call azeror ( Y, NWMAX )

      height = 0.0							!Get residuals from each star
      do ks = 1, TBY
         if ( RESA(34,ks).gt.0.5 ) then

            lyas = 1 + (ks-1)*LYW					!Get area
            lyae = lyas + LYW - 1
            call coprr ( store, LXW, LYWN, 1, LXW, lyas, lyae,
     +                   data, LXW, LYW, 1, 1 )
            jxs = LXSW(ks)
            jys = LYSW(ks)
            call pr_loadbl ( data, LXW, LYW, COM, jxs, jys, ks )

            height = height + RESA(9,ks)				!Get profile points
            do k = 1, LYW
               do j = 1, LXW
                  dx = real(j) - real(LXW/2)
                  dy = real(k) - real(LYW/2)
                  dr = sqrt(dx*dx+dy*dy)
                  if ( dr.le.drmax .and.
     +                 data(j,k).ne.RINVAL ) then
                     kd = 1 + int(198.0*(dr/drmax))
                     X(kd) = X(kd) + dr
                     APNUM(kd) = APNUM(kd) + 1.0
                     ab = RESA(10,ks) + (j-1)*RESA(32,ks)
     +                    + (k-1)*RESA(33,ks)
                     val = data(j,k) - ab
                     Y(kd) = Y(kd) + val/RESA(9,ks)
                  endif
               enddo
            enddo

         endif
      enddo

      do k = 1, NWMAX-1							!Bunch up if any points in the
         if ( nint(APNUM(k)).eq.0 ) then				! radial profile have no data
            next = k
            found = .false.
            do j = k+1, NWMAX
               if ( .not.found .and. nint(APNUM(j)).ne.0 ) then
                  found = .true.
                  next = j
               endif
            enddo
            kgap = next - k
            APNUM(k)  = APNUM(k+kgap)
            X(k) = X(k+kgap)
            Y(k) = Y(k+kgap)
            APNUM(k+kgap) = 0.0
         endif
      enddo
      NTOT = 0
      do k = 1, NWMAX
         if ( nint(APNUM(k)).ne.0 ) NTOT = NTOT + 1
      enddo

      call adivr ( Y, APNUM, Y, NTOT )					!Divide by number of points added
      									! in each radial point to get mean value
      call adivr ( X, APNUM, X, NTOT )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WGAUSS -- Calculate wing fit from radial data
C
C        a j penny           stsci           1988-0318

      subroutine pr_wgauss ( kopt )

      implicit none
      include 'profw.inc'

      integer kopt		!i: Fit option number (1=fit data;2=fit
				!                  points;3=get points)
C--
      real xs, xe, ys, ye, ymin, ymax, xf(NWMAX), yf(NWMAX)
      integer k, istat
      character tx*15, ty*11, capt*9
      data tx, ty, capt / 'Radial Distance', 'Wing Height', 'Wing Plot'/
Cbegin


      if ( .not.WSTARCALC ) then					!Check star fits done for wing
         call printo (
     +        'ERROR: You must do the star fits for the wing first' )
         return
      endif

      call alimr ( X, NTOT, XMIN, XMAX ) 				!Get ranges of data
      call alimr ( Y, NTOT, ymin, ymax )
      xs = XMIN - 0.05*(XMAX-XMIN)
      xe = XMAX + 0.05*(XMAX-XMIN)
      ys = ymin - 0.05*(ymax-ymin)
      ye = ymax + 0.05*(ymax-ymin)
      GOTXRANGE = .true.

      if ( FIRSTP ) then
         call gd_open ( istat )						!Open graph device
         if ( istat.ne.0 ) return
         FIRSTP = .false.
         POINTSPUT = .false.
         FITTED = .false.
      endif

      call gd_dobox ( xs, xe, tx, ys, ye, ty, capt, 0 )			!Clear and Draw axes

      call gd_opts ( X, Y, Y, NTOT, .false., .false., 1 )		!Plot data

      if ( POINTSPUT ) then						!Plot points
         do k = 1, NUMC
            call pgpoint ( 1, XC(k), YC(k), 2 )
         enddo
      endif
      if ( FITTED ) call pr_wpqline ( XMIN, XMAX )			!Plot fit

      if ( kopt.eq.1 ) then						!Fit data
         call amovr ( X, xf, NTOT )
         call amovr ( Y, yf, NTOT )
         call pr_wfitl ( xf, yf, NTOT )
         call pr_wpqline
         FITTED = .true.
      elseif ( kopt.eq.2 ) then						!Fit points
         if ( POINTSPUT ) then
            call amovr ( XC, xf, NUMC )
            call amovr ( YC, yf, NUMC )
            call pr_wfitl ( xf, yf, NUMC )
            call pr_wpqline
            FITTED = .true.
         else
            call printo ( 'ERROR: No points put yet' )
         endif
      elseif ( kopt.eq.3 ) then						!Get points
         call pr_wgcur ( XC, YC, NWMAX, xs, NUMC )
         if ( NUMC.lt.4 .or. NUMC.gt.NTOT ) then
            call printo ( 'ERROR: Too few or too many points' )
            POINTSPUT = .false.
         else
            POINTSPUT = .true.
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WFITL -- Fit wing line data
C
C  a j penny                  stsci            1988-03-19


      subroutine pr_wfitl ( xa, ya, num )

      implicit none
      include 'profile.inc'
      include 'profw.inc'
      include 'STARMAN_INC'

      integer	num		!i: no of data points to plot
      real	xa(num)		!i: X values of data
      real	ya(num)		!i: Y values of data
C--
      real     pout(3), trunc, xo, ymin, ymax
      integer  jfit(5), iter, k
      character*70 text
      external trunc
Cbegin


      if ( FIRSTF ) then
         call alimr ( ya, num, ymin, ymax )				!Rough radius,base, height
         call sort2r ( xa, ya, num )
         BASE = 0.0
         do k = num-3, num
            BASE = BASE + ya(k)/4.0
         enddo
         QR = xa(num)/3.0
         QH = ymax - base
      endif
      FIRSTF = .false.

      call printo ( ' ' )
      call printo ( 'Input starting parameter values for the fits' )
      call printo (
     +     '   (The defaults are previous estimates by the program.)' )
      call get1r ( 'BASE',   BASE, BASE, -1.0e10, 1.0e10 )
      call get1r ( 'HEIGHT',   QH,   QH, -1.0e10, 1.0e10 )
      call get1r ( 'RADIUS',   QR,   QR,     0.1,  1.0e5 )
      call get1r ( 'POWER',    QP,   QP,     0.1,  100.0 )
      if ( ST_FAILED ) return
      call printo ( 'Fix (0) or Free (1) the values of:- '//
     +              'base, height, radius, power ?' )
      call get4i ( 'FIXEM', JFF(1), JFF(2), JFF(3), JFF(4), .true.,0,1)
      if ( ST_FAILED ) return

      call azeroi ( jfit, 5 )

      do k = 1, 2							!Two fit loops

         if ( JFF(1).eq.1 ) jfit(1) = 1					!Relase base
         call gauss1r ( ya, xa, num, .true., 1, num, BASE, QH, QR,
     +             QP, xo, jfit, WHTLIM, WFTLIM, 5, WDAMP, iter, 1 )

         if ( JFF(2).eq.1  ) jfit(2) = 1				! Relase ht
         call gauss1r ( ya, xa, num, .true., 1, num, BASE, QH, QR,
     +             QP, xo, jfit, WHTLIM, WFTLIM, 5, WDAMP, iter, 1 )

         if ( JFF(3).eq.1 ) jfit(3) = 1					! Release radius
         call gauss1r ( ya, xa, num, .true., 1, num, BASE, QH, QR,
     +             QP, xo, jfit, WHTLIM, WFTLIM, 20, WDAMP, iter, 1 )

         if ( k.eq.2 .and. JFF(4).eq.1 ) jfit(4) = 1			! Release power, unless
         call gauss1r ( ya, xa, num, .true., 1, num, BASE, QH, QR, 	! 1st loop or fixed
     +             QP, xo, jfit, WHTLIM, WFTLIM, 20, WDAMP, iter, 1 )

      enddo

      call printo ( '   Fitted parameters' )
      QR = min(max(QR,0.25),real(NWMAX))
      QP = min(max(QP,0.1),6.0)
      pout(1) = trunc(QH,4)
      pout(2) = trunc(QR,6)
      pout(3) = trunc(QP,5)
      write ( text, '(''    Height = '',f10.4,''  Radius = '',
     +            f9.2,''  Power = '',f9.3)' ) pout(1), pout(2), pout(3)
      call printo ( text )
      pout(1) = trunc(BASE,4)
      write ( text, '(''    Base = '',f10.4)' ) pout(1)
      call printo ( text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WGCUR -- Get wing plot cursor positions
C
C  a j penny                       stsci          1988-03-19

      subroutine pr_wgcur ( xa, ya, nf, xs, num )

      implicit none

      integer	nf		!i: Max no of points that can be stored
      real	xa(nf)		!o: X positions got
      real	ya(nf)		!o: Y positions got
      real	xs		!i: X axis start
      integer	num		!o: Number of positions got
C--
      logical loop
      real x, y
      character*1 ch
Cbegin


      call printo (
     +         'Enter cursor position by pressing any KEY ON KEYBOARD' )
      call printo (
     +         'End by inputting a position to left of axis or zero' )
      call printo ( ' ' )

      loop = .true.							!Get positions
      num = 0
      do while ( loop )
         call pgcurse ( x, y, ch )
         if ( x.ge.0.0 .and. x.ge.xs ) then
            if ( num.ge.nf ) then
               call pargi ( nf )
               call printd ( ' Over %d points - too many' )
            else
               num = num + 1
               xa(num) = x
               ya(num) = y
               call pgpoint ( 1, x, y, 2 )
               call pargi ( num )
               call pargr ( x )
               call pargr ( y )
               call printd ( '%d X = %f   Y = %f' )
            endif
         else
            loop = .false.
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WPQLINE -- Plots out a wing profile in 1-D
C
C    ajpenny                    rgo                83-1-1

      subroutine pr_wpqline ( )

      implicit none
      include 'profw.inc'
C--
      integer	k, num
      real	xa(100), ya(100), t, rv
Cbegin


      num = 0
      do k = 1, 100
         rv = XMIN + real(k)*(XMAX-XMIN)/100.0
         if ( rv.gt.0.0 ) then
            num = num + 1
            xa(num) = rv
            t = min(20.0,max(1.0e-5,((rv/QR)**QP)))
            ya(num) = BASE + QH*exp(-1.0*t)
         endif
      enddo
      if ( num.gt.0 ) call pgline ( num, xa, ya )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_WTYPE -- Type out fit params
C
C    ajpenny                    rgo                83-1-1

      subroutine pr_wtype ( )

      implicit none
      include 'profw.inc'

C--
      real trunc, pout(3)
      integer k
      character*70 text*70, ach*3
      external trunc
Cbegin


      call printo ( ' ' )
      call printo ( '  Present Wing Profile:-' )
      pout(1) = trunc(QH,4)
      pout(2) = trunc(QR,6)
      pout(3) = trunc(QP,5)
      write ( text, '(''   Height = '',f10.4,''  Radius = '',
     +            f9.2,''  Power = '',f9.3)' ) pout(1), pout(2), pout(3)
      call printo ( text )

      pout(1) = trunc(BASE,4)
      write ( text, '(''   Base = '',f10.4)' ) pout(1)
      call printo ( text )

      call printo ( '  Parameters fixed in fitting?' )
      do k = 1, 4
         ach = 'Yes'
         if ( JFF(k).eq.1 ) ach = 'No'
         call pargc ( ach )
      enddo
      call printd ('   Height - %c :Radius - %c :Power - %c :Base - %c')
      call printo ( ' ' )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_PROFILE.F
C
C   Contains:-
C
C T_PROFILE      Determine profiles of stars
C PR_SET         Set up defaults
C PR_SETC        Clear arrays; Set up box size
C PR_SETD        Set up display defaults
C PR_MGSTARS     Manually get stars
C PR_FITSETUP    Sets up for fit
C PR_AUTO        Do the automatic estimating
C PR_MAN         Do the interactive profile estimating
C PR_OPT_SETUP   Set up option choices
C PR_IFPARALLEL  Toggle between use/not use of parallel processors
C PR_CHFITPR     Change printing out of intermediate fit steps
C PR_NEWSTARS    Get new star list
C PR_NEWIM       Get new image
C PR_CHUSEMAP    Change if to use profile map
C PR_DOSCPMAP    Scale the profile map
C PR_MRESCALC    Calc residual and put in residual array
C PR_STORERAD    Store radial plot of profile
C PR_CALCVOL     Calculate profile volume
C PR_IFITDISP    Interact with fit dispay ( e.g. change companions, etc)
C PR_ANGCHE      Get new profile angle; check if demands new box size
C PR_TRIAL       Do the trial fit
C PR_OPWRK       Open the work space
C PR_DOWING      Calculate the gaussian wing
C PR_FXPROF      Change the fixed/variable flags on a profile
C PR_FIXXY       Changes stars fixed positions flag, positions
C PR_CHPROF      Change profile
C PR_MEAN        Calc mean and get good stars
C PR_OSTORE      Store results
C PR_CHMAP       Change map size. Remap map.
C PR_CHBOX       Change box size. remap residuals, change posns of
C PR_UPMAP       Adjust map for new size or magnification
C PR_GCOMP       Get new companion lists
C PR_GPROF       Get new profile file
C PR_FIT         Do the fits
C PR_DISPFIT     Display the areas and the fits
C PR_CHFITPAR    Change the fit calculation control parameters
C PR_DORES       Store the result of a fit
C PR_FITLOAD     Load data for fitting
C PR_SOLVE       Fit a profile to a star with poss companions, bad areas
C PR_COMPSET     Set CCA/JFITA from COM for removing faint/distant stars
C PR_FIFILL      Load profile and star (x,y,height) for PR_FITLOR


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_PROFILE -- Determine profile of stars
C
C   a j penny                 dao           1988-04-25

      subroutine t_profile

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer istat, kauto
Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!Get the image data array. Return if bad
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,istat)
      if ( ST_FAILED ) return
      if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL

      call pr_set							!Set up defaults
      call pr_setc

      call pr_mgstars 							!Get star list

CX      if ( .not.ST_FAILED ) then
CX         call get1b ( 'DOPAR', DOPAR, .false. )				!Get if use parallel
CX         if ( DOPAR ) call pr_parload ( istat )				!Load par processors
CX         if ( istat.ne.0 ) DOPAR = .false.
CX      endif

      call pr_setd

      call pr_fitsetup 							!Set up manual

      call pr_opwrk							!Open work space

      call pr_listfit							!Type out present fit

      if ( .not.ST_FAILED ) call get_job ( 'USE', 'auto:inter', 	!Get if use manual or auto approach
     +                                      kauto, 2, ' ', 0 )
      if ( ST_FAILED ) return
      if ( kauto.eq.1 ) then
         call pr_auto							!Do the automatic work
      else
         call pr_man							!Do the manual work
      endif

      call ds_close ( istat )
      call ds_p_close ( istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SET -- Set up defaults
C
C   a.j.penny                   ral                    86-01-02-1100

      subroutine pr_set ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      KFITPR = 0

      call ds_sdef ( 3, 21 )						!Set display default
      PRDSOK = .false.

      DAMP = 0.5							!Set fit calculation control parameters
      FTLIM = 0.001
      HTLIM = 0.001
      WDAMP = 0.5
      WFTLIM = 0.001
      WHTLIM = 0.001
      FIXANGLE = .false.
      do k = 1, 5
         FIXPROF(k) = .false.
      enddo
      DOPAR = .false.
      PARLOADED = .false.

      DOGRID = .true.
      PAINTSPOT = .false.
      CWIDTH = 3.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SETC -- Clear arrays; Set up box size
C
C   a.j.penny                   ral                    86-01-02-1100

      subroutine pr_setc ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call azeror ( RES,   TBX*TBYMAX )					!Zero the arrays
      call azeror ( RESA,  TBX*TBYMAX )
      call azeror ( COM,   6*MAXTOT )
      call azeror ( COMA,  6*MAXTOT )
      call azeror ( RUSE,  TBYMAX )
      call azeror ( XNFIX, TBYMAX )
      call azeror ( YNFIX, TBYMAX )

      call azeroi ( XYNFIX,  TBYMAX )
      call azeroi ( LXS,     TBYMAX )
      call azeroi ( LYS,     TBYMAX )
      call azeroi ( LXSD,    TBYMAX )
      call azeroi ( LYSD,    TBYMAX )
      call azeroi ( LXSW,    TBYMAX )
      call azeroi ( LYSW,    TBYMAX )
      call azeroi ( NUSE,    TBYMAX )

      LX = 20
      LY = 20
      LXD = 2*LX
      LYD = 2*LY
      LXW = 2*LX
      LYW = 2*LY
      LXWD = 2*LXW
      LYWD = 2*LYW

      MX = LX
      MY = LY
      MZ = 1
      MAGNIF = 1
      MAPX = MX
      MAPY = MY


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SETD -- Set up display defaults
C
C   a.j.penny                   ral                    86-01-02-1100

      subroutine pr_setd ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_DS_GEN_INC'
C--
      integer nsq, nsqy, ierr
Cbegin


      if ( ST_FAILED ) return

      if ( PRDSOK ) call ds_close ( ierr )				!Close display
      PRDSOK = .false.

      nsq = 1 + int(sqrt(2.0*real(TBY)))				!Set display default
      nsqy = 1 + ((TBY-1)/nsq)
      DSNXS = 1
      DSNXE = 60 + nsq*(LXD+6)
      DSNYS = 1
      DSNYE = 60 + 4 + 2*nsqy*(LYD+6)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MGSTARS -- Manually get stars
C
C   a j penny                 dao           1988-04-25

      subroutine pr_mgstars ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      logical loop, found
      integer tbxva, tbya, ipxy, ka, kb, j, k, ncol, istat, kal, kbl
      character*30 ahead, bhead
      character*70 text
      integer lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'INSTARS', ipxy, tbxva, tbya, .false., istat )
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      TBY = min(TBYMAX,tbya)

      if ( tbya.gt.TBYMAX ) then					!Warning if input star list long
         write ( text, '(''   Warning: Only the first '',i5,'' stars'',
     +                   '' will be used.'')' ) TBYMAX
         call printo ( text )
         call printo ( '   Thats all the program can take.' )
         call printo ( '   To do more - use the program again.' )
      endif

      call azeror ( RES, TBX*TBY )					!Clear results array

      ka = 0								!Load input star data columns
      kb = 0								! into their proper columns, but
      do j = 1, tbxva - 5						! if havent found 'X' or 'Y',
         call gthead ( 'INSTARS', j, ahead, istat )			! assume they are in 1st and 2nd cols.
         if ( istat.eq.0 ) then
            kal = lens(ahead)
            loop = .true.
            k = 1
            found = .false.
            do while ( loop )
               bhead = HEADER(k)
               kbl = lens(bhead)
               if ( ahead(1:kal).eq.bhead(1:kbl) ) then
                  loop = .false.
                  found = .true.
                  if ( k.eq.1 ) ka = 1
                  if ( k.eq.2 ) kb = 1
               else
                 k = k + 1
                 if ( k.gt.TBX ) loop = .false.
               endif
            enddo
            if ( found ) then
               ncol = j + 5
               call coprr ( %val(ipxy), tbxva, tbya, ncol, ncol, 1, TBY,
     +                      RES, TBX, TBYMAX, k, 1 )
            endif
         endif
      enddo
      if ( ka.eq.0 ) call coprr ( %val(ipxy), tbxva, tbya, 6, 6, 1, TBY,
     +                            RES, TBX, TBYMAX, 1, 1 )
      if ( kb.eq.0 ) call coprr ( %val(ipxy), tbxva, tbya, 7, 7, 1, TBY,
     +                            RES, TBX, TBYMAX, 2, 1 )

      if ( IMTYPE.eq.'SHORT' ) then					!If heights all zero, then find from image
         call pr_dotops ( %val(IPIM) )
      else
         call pr_dotopr ( %val(IPIM) )
      endif

      INPROF = .false.							!Set up flags for type of star list input
      if ( tbxva.eq.(TBXV) ) INPROF = .true.
      INMEAS = .false.
      if ( tbxva.eq.(TBXV-3) ) INMEAS = .true.

      call coprr ( RES, TBX, TBYMAX, 1, 2, 1, TBY, 			!Store input positions
     +             INXY, 2, TBYMAX, 1, 1 )

      call canpar ( 'INSTARS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_AUTO -- Do the automatic profile estimating
C
C   a.j.penny                   stsCi + ral              87-05-22

      subroutine pr_auto ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer nst(150*150), nl(150)                  !Work space
      real    rst(150*150), rl(150)
      logical ok
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ( '    INPUT COMPANION STARS AND BAD AREAS' )
      call printo ( ' ' )

      call pr_gcomp							!Get companion stars

      call printo ( ' ' )
      call printo ( '    SHIFT EVERYTHING IN X AND Y' )
      call printo ( ' ' )

      call pr_shiftxy

      call printo ( ' ' )
      call printo ( '    FIND ROUGH STAR POSNS AND RADII' )
      call printo ( ' ' )

      call pr_rough 							!Get rough posns, heights, radii

      call printo ( ' ' )
      call printo ( '    SET UP FITTING BOX' )
      call printo ( ' ' )

      call pr_chbox ( 2 )						!Change box size


      call printo ( ' ' )
      call printo ( '    FIT STARS WITH FULL PROFILE' )
      call printo ( ' ' )

      call pr_fit ( 2, 2 )						!Calculate fit

      call printo ( ' ' )
      call printo ( '    CALCULATE MEAN PROFILE' )
      call printo ( ' ' )

      call pr_mean ( 2 )						!Calculate mean

      ok = .true.
      if ( SR(1).lt.0.01 .or. SR(1).gt.100.0 )  ok = .false.
      if ( SR(2).lt.0.01 .or. SR(2).gt.100.0 )  ok = .false.
      if ( SR(3).lt.0.01 .or. SR(3).gt.100.0 )  ok = .false.
      if ( SR(4).lt.0.01 .or. SR(4).gt.400.0 )  ok = .false.
      if ( SR(5).lt.0.01 .or. SR(5).gt.400.0 )  ok = .false.
      if ( .not.ok ) then
         call printo ( 'ERROR: Mean profile not feasible' )
         return
      endif

      call printo ( ' ' )
      call printo ( '    USE MEAN PROFILE TO GET COMMON FIT TO STARS' )
      call printo ( ' ' )

      call pr_fit ( 2, 3 )						!Calculate fit

      call printo ( ' ' )
      call printo ( '    CALCULATE EMPIRICAL PROFILE' )
      call printo ( ' ' )

      call pr_mrescalc 							!Calculate residuals and put them in residuals map

      call pr_infill ( %val(IPTR), %val(IPTH), LXM, LYM, DONERES )	!Fill in any gaps in residual map

      if ( LXM.gt.150 .or. LYM.gt.150 ) then
         call pargi ( LXM )
         call pargi ( LYM )
         call printd (
     +           'ERROR: Map sides are %d x %d - must be < 150x150')
         return
      endif

      call smoothr ( %val(IPTR), LXM, LYM, 3, 3, 1, rst, nst,rl,nl)	!Smooth residuals map 3x3

      call azeror ( %val(IPMAP), MX*MY )				!Zero Map

      call pr_addmap ( %val(IPTR), %val(IPTH), %val(IPMAP) )		!Add to profile map from residuals

      call pr_calcvol ( 2 )						!Calculate fit volume

      call printo ( ' ' )
      call printo ( '    STORE PROFILE' )
      call printo ( ' ' )

      call pr_ostore							!Store fit


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MAN -- Do the interactive profile estimating
C
C   a.j.penny                   stsCi + ral              87-05-22

      subroutine pr_man ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
      include 'ST_DS_PANEL_INC'
C--
      real rworka(MAXWORK), rworkb(MAXWORK)
      integer istat, ipmwa
      logical loop, ok, change
      character*12 ktopt
Cbegin


      if ( ST_FAILED ) return						!Check failure flag

      call type_hchoice

      call pr_opt_setup ( ktopt, 1, .true. )
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )							!Loop doing the work

         call pr_opt_setup ( ktopt, 1, .false. )			!Get option
         call get_choice ( ktopt, 1 )
         if ( ST_FAILED ) return

         if ( ktopt.eq.'interact_fit' ) call pr_ifitdisp 		!Interact with fit dispay ( e.g. change companions, etc)

         if ( ktopt.eq.'controls_fit' ) call pr_chfitpar		!Change fit calculation control parameters

         if ( ktopt.eq.'fit_changed' ) call pr_fit ( 1, 1 )		!Fit changed stars

         if ( ktopt.eq.'fit_all' ) call pr_fit ( 2, 1 )			!Fit all accepted stars

         if ( ktopt.eq.'fit_one' ) call pr_fit ( 3, 1 )			!Fit one star

         if ( ktopt.eq.'disp_fit_all' ) call pr_dispfit ( 0, 1, LX, LY,	!Display all fits/areas
     +                                 LXS, LYS, LXD, LYD, LXSD, LYSD )

         if ( ktopt.eq.'disp_fit_one' ) call pr_dispfit ( 1, 1, LX, LY,	!Display one fit/area
     +                                 LXS, LYS, LXD, LYD, LXSD, LYSD )

         if ( ktopt.eq.'mean' ) call pr_mean ( 1 )			!Ring rejects on screenChoose best stars and calculate mean values

         if ( ktopt.eq.'reject' ) call pr_starrk ( 2, change )		!Get change of stars to be used

         if ( ktopt.eq.'flip' ) call pr_starrk ( 1, change )		!Get change of stars to be used

         if ( ktopt.eq.'res_calc' ) call pr_mrescalc 			!Calculate residuals and put them in residuals map

         if ( ktopt.eq.'res_fill' ) call pr_infill ( %val(IPTR),	!Fill any empty pixels in residuals array
     +                             %val(IPTH), LXM, LYM, DONERES )

         if ( ktopt.eq.'res_disp' ) call pr_disresb ( %val(IPTR), 	!Display residuals
     +                             %val(IPTH), rworka, LXM, LYM, 2 )

         if ( ktopt.eq.'res_interact' ) call pr_resinter ( LXM, LYM, 	!Look at and change residuals
     +                                     rworka, rworkb, %val(IPTR),
     +                                      %val(IPTH), %val(IPOCC),
     +                                      %val(IPOCD) )

         if ( ktopt.eq.'map_disp' ) call pr_disresb ( %val(IPMAP), 	!Display the profile map
     +                                   rworka, rworkb, MX, MY, 1 )

         if ( ktopt.eq.'map_size' ) call pr_chmap			!Change Map size

         if ( ktopt.eq.'map_zero' ) call azeror ( %val(IPMAP), MX*MY )	!Zero Map

         if ( ktopt.eq.'map_add' ) call pr_addmap ( %val(IPTR),		!Add to profile map from residuals
     +                                   %val(IPTH), %val(IPMAP) )

         if ( ktopt.eq.'map_scale' ) call pr_doscpmap			!Scale the profile map

         if ( ktopt.eq.'map_use' ) call pr_chusemap			!Change if to use map

         if ( ktopt.eq.'map_interact' ) then				!Look at and change profile map
                        call gtwrkr ( 'TMAPA', MX*MY, ipmwa, istat )
                        call amovr ( %val(IPMAP), %val(ipmwa), MX*MY )
                        call pr_mapinter ( %val(IPMAP), rworka, rworkb,
     +                                     %val(ipmwa) )
                        call wrkcan ( 'TMAPA' )
                        endif

         if ( ktopt.eq.'wing' ) call pr_dowing 				!Solve for a wide wing

         if ( ktopt.eq.'radial_plot' ) call pr_storerad 		!Store profile plot

         if ( ktopt.eq.'prof_change' ) call pr_chprof 			!Change profile values

         if ( ktopt.eq.'volume' ) call pr_calcvol ( 1 )			!Calculate fit volume

         if ( ktopt.eq.'fit_trial' ) call pr_trial 			!Do the trial fit

         if ( ktopt.eq.'box_size' ) call pr_chbox ( 1 ) 		!Change box size

         if ( ktopt.eq.'info_fits' ) call pr_listfit 			!Type out the fits

         if ( ktopt.eq.'info_fitting' ) call pr_chfitpr			!Get if to print out intermediate
                        						! fitting step results

         if ( ktopt.eq.'info_star' ) call pr_listcomp 			!Type out the details of a star and its comps, bad areas

         if ( ktopt.eq.'shift_all' ) call pr_shiftxy 			!Shift all positions by a given amount

         if ( ktopt.eq.'angle' ) call pr_angche				!Check on angle

         if ( ktopt.eq.'comp_zero' ) call pr_compzero 			!Clear companions

         if ( ktopt.eq.'fix_set_xy' ) call pr_fixxy			!Change position fixed/variable flag and positions

         if ( ktopt.eq.'prof_fix' ) call pr_fxprof 			!Change profile fixed/variable flags

         if ( ktopt.eq.'show_reject' ) call pr_preject	 		!Ring rejects on screen

         if ( ktopt.eq.'fit_rough' ) call pr_rough			!Get rough posns, heights, radii

         if ( ktopt.eq.'new_cm_pr' ) then 				!Get input companion list and input profile
                                      call pr_gcomp
                                      call pr_gprof
                                   endif

         if ( ktopt.eq.'new_image' ) call pr_newim			!Get new image

         if ( ktopt.eq.'new_stars' ) call pr_newstars 			!Get new star list

         if ( ktopt.eq.'store' ) call pr_ostore 			!Store profile, stars, comps

         if ( ktopt.eq.'display' ) call pr_dispfit ( 2, 0, LX, LY, 	!Display all areas
     +                                 LXS, LYS, LXD, LYD, LXSD, LYSD )

         if ( ktopt.eq.'clear' ) call pr_dsclear			!Clear display

         if ( ktopt.eq.'close' ) call pr_setd				!Close display

CX         if ( ktopt.eq.'parallel' ) call pr_ifparallel		!Toggle between parallel andnot

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Change panel/keyboard input choice

         if ( ktopt.eq.'exit' ) loop = .false.				!Exit

         if ( .not.DONESTOR .and. .not.loop ) then			!Exit if really want to, otherwise loop
            call printo ( ' ' )
            call printo ( '  You have not stored the result since '//
     +                    'the last profile or box change.' )
            call printo ( '  Is it really ok to exit?' )
            call get1b ( 'OK', ok, .false. )
            if ( ST_FAILED ) return
            if ( .not.ok ) loop = .true.
         endif

         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_OPT_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 Jan

      subroutine pr_opt_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside           !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=46 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

C DISPLAY

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear the display window',
     + 'Clear the display window. This only affects the window. It',
     + 'does not affect any of the image or residuals, or maps. It is',
     + 'used solely for tidying up a window that has got too cluttered',
     + 'and you wish to display some new stuff.',
     + ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

C  CONTROL

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(45),opt_head(45),(opt_help(j,45),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

C  FITS

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'interact_fit',  'Interact with the display of fits',
     + 'Look in more detail at the fits and add companion stars, etc.',
     + ' ',
     + 'CHANGES: Change the companions; change bad areas; move the',
     + '         main stars.',
     + 'DISPLAY: Pan/zoom the fits display; type out the image or ',
     + '         residuals values; remove/replace coloured lines.' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'fit_all',  'Fit all the accepted stars',
     + 'The program goes through the star list fitting the stars.',
     + ' ',
     + 'The fits are under the control of many aspects, which have all',
     + 'been defaulted, unless you have changed them. Stars which have',
     + 'been -rejected- are not fitted. Before the fitting you are ',
     + 'asked for whether to fix the profile and how to start the fit.'/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'disp_fit_all',  'Display the areas + residuals to the fits',
     + 'Display the areas around each star and the same areas after',
     + 'the fitted stars have been subtraced. The contrast in each ',
     + 'area is maximised to show detail, so will be different for the',
     + '-cleaned- and -uncleaned- area for the same star, and between',
     + 'stars. The position of fitted and companion stars is marked',
     + 'and also -bad- areas you have determined. '/

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'fit_changed', 'Fit only stars with changed circumstances',
     + 'It goes through the star list fitting the stars. It only does',
     + 'those with new circumstances (a new companion,bad area,etc.)',
     + 'The fits are under the control of many aspects, which have all',
     + 'been defaulted, unless you have changed them. Stars which have',
     + 'been -rejected- are not fitted. Before the fitting you are ',
     + 'asked for whether to fix the profile and how to start the fit.'/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'fit_one',  'Fit a certain star',
     + 'Fits a profile to one of the stars in star list. You input the',
     + 'number of the star in the list you want to do.',
     + 'The fit is under the control of many aspects, which have all',
     + 'been defaulted, unless you have changed them. A star which has',
     + 'been -rejected- is even so fitted. Before the fitting you are ',
     + 'asked for whether to fix the profile and how to start the fit.'/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'disp_fit_one',  'Display the areas + residuals to a single fit',
     + 'Display the area around a star and the same area after',
     + 'the fitted stars have been subtraced. The contrast in each ',
     + 'area is maximised to show detail, so will be different for the',
     + '-cleaned- and -uncleaned- areas. The position of fitted and ',
     + 'companion stars is marked and also -bad- areas you have ',
     + 'determined. You are asked for the number of star to show.'/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'controls_fit', 'Change internal controls of fit calculating',
     + 'The inner mechanics of the iterative linearised least-squares',
     + 'calculations can be altered if the fits are not proceeding',
     + 'as desired. The parameters asked for are: the damping factor -',
     + 'which controls the sensitivity of the fit; the accuracy of the',
     + 'end of the fit, when it should stop, for the height and the ',
     + 'profile. These are askedfor the main and wing profile fit.'/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'mean',  'Decide which stars to use; find mean weighted profile',
     + 'From the fits, calculate a mean profile. The parameters are',
     + 'weighted by star -heights-. If a star has any of its profile',
     + 'parameters differing from mean by more than 3 std devs, that',
     + 'star is rejected. After a mean is made, you can repeatedly ',
     + 'flip profiles, and reject or accept stars to get new mean.',
     + 'If profile parameters change, RESIDUALS ARRAY WILL BE ZEROED.'/

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'reject',  'Change which stars are accepted/rejected',
     + 'On taking the mean, the program will reject certain stars as',
     + 'having profile parameters too far from their means. You may ',
     + 'correct its choice by changing the status of any star as ',
     + 'rejected (+ve number) or accepted (-ve number). [As always, ',
     + 'this status stays until you change it, or until the program',
     + 'decides to reject an accepted star.] '/

      data opt_text(46),opt_head(46),(opt_help(j,46),j=1,6) /
     + 'flip',  'Rotate profile ellipse of stars by 180 degrees',
     + 'Sometimes the fit will produce an angle more than +/- 90 ',
     + 'degrees away from the X axis. To make the averaging of the',
     + 'angles to work out, these should be corrected by rotating by',
     + '+/- 180 degrees (which does not change the profile w.r.t. the',
     + 'image) until the angle lies within that +/-90 degree range. ',
     + ' '/

C RESIDUALS

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'res_interact',  'Interact with the display of residuals array',
     + 'Look in detail at the residuals array, and change if needed.',
     + ' ' ,
     + 'CHANGES: Fill in -empty- pixels by interpolating from neigh-',
     + '         bours; smooth; smooth areas; zero areas; zero.',
     + 'DISPLAY: Pan/zoom the display; look at values; clear.',
     + 'RESTORE: Reload calculated values '/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'res_calc',  'Calculate the array of residuals',
     + 'The mean profile is fixed and fitted to all accepted stars. ',
     + 'The residuals from these fits are then scaled and averaged',
     + 'into a -residual- image. (The pixels may be sub-divided',
     + 'versions of the image pixels if the profile is sharp. In this',
     + 'case each residual from a star is put in its right sub-pixel)',
     + 'This residual image IS USED IN making the output -MAP- image. '/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'res_disp',  'Display the mean fit residuals array',
     + 'After using the -RESCALC- option to get the residuals image,',
     + 'this displays it, with, to its right side a picture of whether',
     + 'pixels actually have a residuals value (white if they have). ',
     + 'A pixel may be empty if they are subdivided, as there',
     + 'may not be a star with the right centre offset from the image',
     + 'pixel centres. Use -RESFILL- or -RESINTER- to cure this.'/

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'res_fill',  'Fill any -empty- pixels in the residuals array',
     + 'After the residuals array has been calculated some of the',
     + 'pixels may be empty, if they are subdivided, as there',
     + 'may not be a star with the right centre offset from the image',
     + 'pixel centres. Use this option or -RESINTER- to cure this',
     + 'by interpolating between neighbouring pixels. ',
     + 'Using -RESINTER- lets you see the amount of underfilling.' /

C  MAP

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'map_interact',  'Interact with the display of Profile Map',
     + 'Look in detail at the Profile Map, and change if needed.',
     + ' ' ,
     + 'CHANGES: smooth; smooth areas; zero areas; zero.',
     + 'DISPLAY: Pan/zoom the display; look at values; clear.',
     + 'RESTORE: Reload calculated values ',
     + ' ' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'map_add',  'Add the Residuals array onto the Profile Map',
     + 'This is means whereby the residuals array, calculated by',
     + '-RESCALC- after doing fits is transferred to the Profile Map',
     + 'ready for storing in the output profile image.',
     + 'It adds the residuals array to the present values in the ',
     + 'Profile Map. The Map is used in the fits, so a subsequent add',
     + 'will add in the new residuals correctly.  '/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'map_disp',  'Display the profile map',
     + 'The profile map is displayed at the centre of the display',
     + 'window. It may have -more- pixels than the area of the image,',
     + 'if the pixels have been sub-divided because of the narrowness',
     + 'of the profile. ',
     + ' ', ' '/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'map_scale',  'Change Profile Map scale (mult/div its values)',
     + 'In use, the Profile Map has its pixel values multiplied by a ',
     + 'scale factor. This factor is defaulted at the beginning to 1.',
     + 'This option can change this scale factor to anything desired.',
     + 'Its purpose is somewhat obscure, but may be useful at odd ',
     + 'times. The author would be grateful if you would let him know',
     + 'if you have found a use for it. '/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'map_size',  'Change Profile Map size',
     + 'The size of the Profile Map may be changed. If it becomes ',
     + 'smaller than it was, the outside bits are lost. If it becomes',
     + 'larger, then the new bits at the edges are filled with zeroes.',
     + ' ', ' ', ' '/

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'map_use',  'Change whether to apply the Profile Map or not',
     + 'The default state is that the Profile Map is used in a fit. ',
     + 'This may be used to switch it -off-, so that it is not used',
     + 'in the fitting or making of the residuals.  ',
     + ' ',
     + 'If used again, it switches it back -on- again, and so on.',
     + ' ' /

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'map_zero',  'Zero the Profile Map',
     + 'Replace all the contents of the Profile Map with zeroes.',
     + 'This may be used when an unwanted residuals have been added, ',
     + 'or when the Profile Map has been modified wrongly, and it is ',
     + 'desired to start over again. ',
     + ' ', ' '/

C  WING

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'wing',  'Fit a broad Gaussian wing - zero broad wing start',
     + 'After the profile with its empirical map has been subtracted',
     + 'from a star, there is sometimes a very wide wing left, which',
     + 'is too wide to fit with a normal boxsize. After doing a normal',
     + 'fit with a normal boxsize, this option then uses a box size',
     + 'twice the normal size, takes the residuals, and makes a mean',
     + 'radial profile. This is then fit with a modified Gaussian.'/

C  ACTIONS

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'radial_plot', 'Put a radial plot of profile and fit into file',
     + 'Perform fit to the accepted stars with mean fit, if not',
     + 'already done so. Then calculate the mean radial profile of',
     + 'the mean profile, and of the data. Calculate the difference',
     + 'between the two. Store these plots out to a file as a table.',
     + 'with the columns (distance:fit:data:residuals). This table',
     + 'can then be plotted out with the program TBPLOT.'/

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'prof_change',  'Change the mean profile parameters',
     + 'The profile has nine analytical parameters. For the mean',
     + 'profile, which is used at a number of points in this program,',
     + 'these can be set with this option. The -VALUE- parameter is ',
     + 'used. You are told nine times which of the paramters to change',
     + 'and offered the present value as a default. Afterwards, the ',
     + 'new profile is used to suggest a new fitting box size.'/

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'fit_rough',  'Make rough estimate of posns, heights, radii',
     + 'This calculates the mean profile, using a fast, but rough ',
     + 'method. It takes the present profile radius (start default ',
     + 'radius = 2.0) and fits a Gaussian to the active stars. The  ',
     + 'fitted radii are then used to make a crude mean profile. ',
     + 'It types the fit details, so you can look for anomalies,',
     + 'and it changes the star positions to their fitted positions.'/

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'fit_trial',  'Calculate the mean profile from one star',
     + 'The mean profile is calculated by doing a proper fit to one',
     + 'star and putting the fit parameters found into the mean ',
     + 'profile. ',
     + 'The program looks for the first not -rejected- star in the ',
     + 'list, and suggests it. You choose any one, even a -rejected-',
     + 'one. It fits, notes, and types out the fit. '/

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'volume',  'Calculate the profile volume',
     + 'Takes the mean profile, asks you how far out from the centre',
     + 'to go, and integrates the -volume- under the profile of a star',
     + 'of unit height. This is in units of (square pixels)*height.',
     + 'This is typed out, remembered and stored with the profile when',
     + 'it is stored. It is used in the transformation from height to',
     + 'magnitude for stars of different profiles.'/

C  SETUPS

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'angle',  'Change Fix/Vary angle and/or angle',
     + 'The angle profile parameter (angle in degrees between X-axis',
     + 'and the profile major axis) can be allowed to vary during',
     + 'fitting for stars, or it can be fixed at the starting value. ',
     + ' ',
     + 'Also the angle of the mean profile can be can be set to a ',
     + 'specified value. ' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'box_size',  'Change size of box round stars used for fitting',
     + 'Analysis of stars is done in a rectangular box round the stars',
     + 'This tells you the old size, calculates what a good size would',
     + 'be from the current mean profile, and asks for a new XY size.',
     + 'It also gives the chance to change the size of the empirical',
     + 'map of the profile. Usually, the two boxes should be the same ',
     + 'size, but there is no obligation for them to be so.'/

      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'comp_zero',  'Delete all comp/faint comp/bad area list entries',
     + 'Each star can have companions, faint companions, and bad areas',
     + 'which are used in fitting. These are loaded from the ',
     + '-new_cm_pr- option, or via the -interact_fit- option.',
     + ' ',
     + 'This deletes any ones which are presently noted. ', ' '/

      data opt_text(34),opt_head(34),(opt_help(j,34),j=1,6) /
     + 'fix_set_xy',  'Set fixed/varying positions and/or the posns',
     + 'A star can be fixed at its starting position during a fit or ',
     + 'it can be allowed to move. This lets you fix the positions of ',
     + 'some of the stars. (The companions to the fit can still move.)',
     + 'It also lets you change the positions of stars. (The positions',
     + 'of the companions are unchanged so they will move relative ',
     + 'to the main stars. '/

      data opt_text(35),opt_head(35),(opt_help(j,35),j=1,6) /
     + 'info_fits',  'List the present fits',
     + 'Types out:- ',
     + '            the parameters of the fit stars',
     + '            the mean profile',
     + '            whether the profile empirical map is used',
     + '            the -seeing- (Full Width at Half Maximum)',
     + '            the fitting box size and suggested size' /

      data opt_text(36),opt_head(36),(opt_help(j,36),j=1,6) /
     + 'info_star',  'List the fit, comps, bads of a star',
     + 'It asks the number of the desired star. You put it in.',
     + 'It types out fit details, details of close and of faint  ',
     + 'companions, and of the bad areas. ',
     + ' ', ' ', ' '/

      data opt_text(37),opt_head(37),(opt_help(j,37),j=1,6) /
     + 'shift_all', 'Shift XY positions of all the stars and comps',
     + 'Apply a shift in X and Y to all the stars, and their ',
     + 'companions (both close and distant/faint). Also shift the',
     + 'bad areas.',
     + ' ', ' ', ' '/

      data opt_text(38),opt_head(38),(opt_help(j,38),j=1,6) /
     + 'info_fitting',  'Type intermediate step results in the fitting',
     + 'This toggles the switch for the option of typing out the',
     + 'intermediate steps in a fit calculation. What gets typed out,',
     + 'if the option is -on-, is for each star, at each step in the',
     + 'iteration, the star and iteration number, height, x, y ',
     + '(in box); for 1st star, the base level, X slope, Y slope. ',
     + 'The full profile at each step is typed (3 wing params first).' /

      data opt_text(39),opt_head(39),(opt_help(j,39),j=1,6) /
     + 'prof_fix',  'Change fixed/variable control on profile params',
     + 'The 5 main parameters of the analytical profile  can be ',
     + 'allowed to vary or to be fixed during a fit. This option lets',
     + 'you set whether they are. They are the:- the major and minor',
     + 'axes radii, the power, and the major and minor power radii.',
     + 'The angle parameter is controlled separately, as are the',
     + 'Gaussian wing parameters.' /

      data opt_text(40),opt_head(40),(opt_help(j,40),j=1,6) /
     + 'show_reject', 'Put red border for rejected stars on display',
     + 'This enables you to see which of the displayed stars have been',
     + 'rejected (either by the -mean- option, or by you. It paints',
     + 'up a red border round the displayed box area in the ',
     + 'image display for the rejected stars.',
     + ' ', ' '/

CX      data opt_text(47),opt_head(47),(opt_help(j,47),j=1,6) /
CX     + 'parallel',  'Switch between parallel processors/not choice',
CX     + ' ',
CX     + 'This option is not available. The software exists, but no',
CX     + 'as there is no generally available parallel processor ',
CX     + 'system in use, the particular implementation that would',
CX     + 'be needed cannot yet be supllied.',
CX     + ' '/


C  FILES

      data opt_text(41),opt_head(41),(opt_help(j,41),j=1,6) /
     + 'new_cm_pr',  'Load some previous stuff (Comps, Bad areas, Fit)',
     + 'The program can output full details of the results of its',
     + 'These can be input, via two calls:- ',
     + 'INCOMPS inputs the list of companions, faint companions and',
     + 'bad areas - as fitted previously. INPROF inputs a profile,',
     + 'via an image containing the profile (parameters are headers,',
     + 'profile map is stored as the image values). '/

      data opt_text(42),opt_head(42),(opt_help(j,42),j=1,6) /
     + 'new_image',  'Input new image to replace the present one ',
     + 'Replace present image by new one. All other details will be ',
     + 'unchanged. You have the option to make a rough calculation',
     + 'of the star heights by looking at the 3x3 pixels at the',
     + 'positions, taking the highest, subtractinga rough sky from',
     + 'the mean in the entire image and calling that the height.',
     + ' '/

      data opt_text(43),opt_head(43),(opt_help(j,43),j=1,6) /
     + 'new_stars',  'Input list of star posns to replace present ones',
     + 'This is like the original -INSTARS- at the start of the ',
     + 'program. It wipes out ALL the previous stars, companions, ',
     + 'bad areas, profile, fits. ',
     + ' ', ' ', ' '/

      data opt_text(44),opt_head(44),(opt_help(j,44),j=1,6) /
     + 'store',  'Save the profile and fits',
     + 'Puts results into files:-',
     + '    OUT      is the image containing a profile (parameters are',
     + '             headers, profile map is the picture)',
     + '    OUTSTARS is the file of stars with posns and heights',
     + '    OUTCOMPS is (if there are any) the companions, bad areas',
     + '             associated with the stars' /

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Profile - Main', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=7 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'fit_rough', 'box_size', 'fit_all',
     +                'disp_fit_all', 'panel', 'interact_fit' /

      integer sect_num
      parameter ( sect_num=10 )
      character*10 sect_head(sect_num)
      data sect_head / 'FILES', 'AREAS', 'POSITIONS', 'FIT', 'PROFILE',
     +                 'RESIDUALS', 'MAP', 'WING', 'DISPLAY',
     +                 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'new_image:new_stars:new_cm_pr:store' /
      data sect_text(2) / 'box_size:display' /
      data sect_text(3) / 'fix_set_xy:shift_all:info_star' /
      data sect_text(4) / 'fit_rough:fit_trial:fit_all:fit_one:
     + fit_changed:disp_fit_all:disp_fit_one:interact_fit:comp_zero:
     + controls_fit:info_fits:info_fitting:show_reject' /
      data sect_text(5) / 'mean:reject:flip:angle:prof_change:
     +                     prof_fix:volume:radial_plot' /
      data sect_text(6) / 'res_calc:res_fill:res_disp:res_interact' /
      data sect_text(7) / 'map_add:map_disp:map_interact:map_scale:
     +                     map_size:map_use:map_zero' /
      data sect_text(8) / 'wing' /
      data sect_text(9) / 'clear:close' /
      data sect_text(10) / 'panel:exit' /

      integer help_num
      parameter ( help_num=2 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,help_num) /
     + '             ',
     + '             ' /

Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_IFPARALLEL -- Toggle between use/not use of parallel processors
C
C     a j penny                 ral             1990-05-10

      subroutine pr_ifparallel ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( DOPAR ) then

         DOPAR = .false.
         call printo ( 'Parallel processors were'//
     +                 ' used. They are not now.' )

      else

         if ( .not.PARLOADED ) then

            call pr_parload ( istat )
            if ( istat.ne.0 ) then
               call printo ( 'Parallel processors '//
     +                       'were not used. They are still not.' )
            else
               DOPAR = .true.
               call printo ( 'Parallel processors '//
     +                       'were not used. They are now.' )
            endif

         else

            DOPAR = .true.
            call printo ( 'Parallel processors '//
     +                    'were not used. They are now.' )

         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHFITPR -- Change printing out of intermediate fit steps
C
C     a j penny                 ral             1990-05-10

      subroutine pr_chfitpr ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      if ( KFITPR.eq.0 ) then
         call printo ( 'Intermediate steps in the fitting were not'//
     +                 ' output' )
         call printo ( 'They are now' )
         KFITPR = 1
      else
         call printo ( 'Intermediate steps in the fitting were output' )
         call printo ( 'They are not now' )
         KFITPR = 0
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_NEWSTARS -- Get new star list
C
C     a j penny                 ral             1990-05-10

      subroutine pr_newstars ( )

      implicit none
      include 'profile.inc'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'
C--
      integer nsq, nsqy
Cbegin


      if ( ST_FAILED ) return

      call pr_setc
      call pr_mgstars
      call pr_fitsetup

      nsq = 1 + int(sqrt(2.0*real(TBY)))				!Set display default
      nsqy = 1 + ((TBY-1)/nsq)
      DSNXS = 1
      DSNXE = 60 + nsq*(LXD+6)
      DSNYS = 1
      DSNYE = 60 + 4 + 2*nsqy*(LYD+6)

      call wrkcan ( 'CALCA' )
      call wrkcan ( 'CALCB' )
      call wrkcan ( 'DISP' )
      call wrkcan ( 'MAPMAP' )
      call wrkcan ( 'RESMAP' )
      call wrkcan ( 'RESH' )
      call wrkcan ( 'DISC' )
      call wrkcan ( 'DISD' )
      call pr_opwrk
      call pr_listfit


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_NEWIM -- Get new image
C
C     a j penny                 ral             1990-05-10

      subroutine pr_newim ( )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer k, istat
      logical ok
      character*72 text
Cbegin


      if ( ST_FAILED ) return

      call canpar ( 'IN' )
      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!Get the image data array. Return if bad
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) return
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, istat)
      if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL

      call printo ( 'Recalculate rough heights?' )
      call get1b ( 'OK', ok, .true. )
      if ( ST_FAILED ) return
      if ( ok ) then

         if ( IMTYPE.eq.'SHORT' ) then
            call pr_dotops ( %val(IPIM) )
         else
            call pr_dotopr ( %val(IPIM) )
         endif

         call printo ( ' Num       Height' )
         do k = 1, TBY
            write ( text, '(1x, i5, 2x, f12.4)' ) k, RES(9,k)
            call printo ( text )
         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHUSEMAP -- Change if to use profile map
C
C     a j penny                 ral             1990-05-10

      subroutine pr_chusemap ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( DOMAP ) then
         call printo (
     +   'At present the Profile Map is used in the fitting' )
      else
         call printo (
     +   'At present the Profile Map is not used in the fitting')
      endif
      call get1b ( 'USEMAP', DOMAP, DOMAP )
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DOSCPMAP -- Scale the profile map
C
C     a j penny                 ral             1990-05-10

      subroutine pr_doscpmap ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      real rv, diff
Cbegin


      rv = SCALEMAP
      call get1r ( 'SCALE', SCALEMAP, SCALEMAP, 1.0e-8, 1.0e8 )
      if ( ST_FAILED ) return
      diff = SCALEMAP/rv
      call arrsc ( %val(IPMAP), MX, MY, diff, 0.0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MRESCALC -- Calc residual and put in residual array
C
C     a j penny                 ral             1990-05-10

      subroutine pr_mrescalc ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      logical ok
Cbegin


      if ( ST_FAILED ) return

      ok = .true.
      if ( .not.DONEMEAN ) then
         call printo ( 'You have not calculated the mean profile'//
     +                 '. Is this OK?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
      endif
      if ( .not.ok ) return

      call pr_rescalc ( %val(IPTR), %val(IPTH),
     +                  %val(IPDATA), %val(IPDATAR) )
      call amovr ( %val(IPTR), %val(IPOCC), LXM*LYM )
      call amovr ( %val(IPTH), %val(IPOCD), LXM*LYM )

      DONERES = .true.
      DONEMAP = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_STORERAD -- Store radial plot of profile
C
C     a j penny                 ral             1990-05-10

      subroutine pr_storerad ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      logical ok
Cbegin


      if ( ST_FAILED ) return

      ok = .true.
      if ( .not.DONEMEAN ) then
         call printo ( 'You have not calculated the mean profile'//
     +                 '. Is this OK?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
      endif
      if ( .not.ok ) return

      if ( .not.DONEVOL ) then
         call printo ( 'You have not calculated the profile'//
     +                    ' volume yet. Is this OK?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
      endif
      if ( .not.ok ) return

      if ( .not.DONERES ) then						!If not calculated mean residuals and mean radial residuals, do so
         call printo ( 'You have not calculated the residuals yet' )
         call printo ( 'Will do so now' )
         call pr_rescalc ( %val(IPTR), %val(IPTH),
     +                     %val(IPDATA), %val(IPDATAR) )
         call amovr ( %val(IPTR), %val(IPOCC), LXM*LYM )
         call amovr ( %val(IPTH), %val(IPOCD), LXM*LYM )
         DONERES = .true.
         DONEMAP = .false.
      endif

      call pr_qpstore 							!Store plot in file


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CALCVOL -- Calculate profile volume
C
C     a j penny                 ral             1990-05-10

      subroutine pr_calcvol ( kopt )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: Involve user, or not? (1:2)
C--
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 ) call get1r ( 'VOLRAD', VOLRAD, VOLRAD, 1.0,1.0e6)
      if ( ST_FAILED ) return

      call cvolume ( SR, DOMAP, %val(IPMAP), MX, MY, 1,
     +                  1, MX, MY, MAGNIF, VOLRAD, VOL )

      call pargr ( VOL )
      call printd (
     + '   Volume out to that radius of unit height star = %f' )
      call printo (
     + '   Where volume is in units of: pixel.pixel.height' )
      call printo ( ' ' )

      DONEVOL = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_IFITDISP -- Interact with fit dispay ( e.g. change companions, etc)
C
C     a j penny                 ral             1990-05-10

      subroutine pr_ifitdisp ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k, istat
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         RES(36,k) = 0.0
      enddo

      call pr_dsopen ( istat ) 						!Open display if not already open
      if ( istat.ne.0 ) return

      call pr_dfinter ( RES, COM, LX, LY, LXS, LYS, LXD, LYD,
     +                  LXSD, LYSD )

      DONERES = .false.
      DONEMAP = .false.
      DONEFIX = .false.
      DONESTOR = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_ANGCHE -- Get new profile angle; check if demands new box size
C
C     a j penny                 ral             1990-05-10

      subroutine pr_angche ( )

      implicit none
      include 'profile.inc'
C--
      logical flag
      real alx, aly
      integer lxa, lya, k
      character*72 text
Cbegin


      call pr_getangle  ( FIXANGLE, SR(6), flag )

      if ( .not.flag ) return

      DONEMEAN = .false.
      DONEFIX = .false.
      DONERES = .false.
      DONEMAP = .false.
      DONESTOR = .false.
      DONEVOL = .false.
      do k = 1, TBY
         RES(36,k) = 3.0
      enddo
      call boxeli ( SR(1), SR(2), SR(6), alx, aly )
      lxa = 10.0*alx
      lya = 10.0*aly
      if ( LX.ne.lxa .or. LY.ne.lya ) then
         write ( text, '(''Present box size = '',2i4)' ) LX, LY
         call printo ( text )
         write ( text, '(''Suggested new box size = '',2i4)' ) lxa, lya
         call printo ( text )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_TRIAL -- Do the trial fit
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_trial ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k, ka, kb
      logical flag
      real asr(6)
Cbegin


      ka = 0								!Get star to use
      k = 1
      do kb = 1,TBY
         if ( ka.eq.0 .and. RES(34,kb).gt.0.5 ) then
            ka = 1
            k = kb
         endif
      enddo
      call amovr ( SR, asr, 6 )
      call get1i ( 'NUMBER', k, 1, 1, TBY )
      if ( ST_FAILED ) return

      if ( RES(34,k).lt.0.5 ) then
         call printo (
     +    '  This star has been -rejected-. Fitting will still occur' )
      endif

      call printo ( '   No   Height Its     Rmaj    Rmin'//
     +              '    P       PRmaj   PRmin  Theta  ' )
      LXS(k) = RES(1,k) - real((LX/2)-1)
      LYS(k) = RES(2,k) - real((LY/2)-1)
      call pr_solve ( %val(IPDATA), %val(IPDATAR), FIXANGLE, FIXPROF,
     +                SR, 10, k, RES, COM, 1, 0 )
      call amovr ( RES(12,k), SR, 6 )

      flag = .false.
      do k = 1, 6
         if ( asr(k).ne.SR(k) ) flag = .true.
      enddo
      if ( flag ) then
         DONEMEAN = .false.
         DONERES = .false.
         DONEMAP = .false.
         DONESTOR = .false.
         DONEVOL = .false.
         DONEFIX = .false.
      endif

      do k = 1, TBY
         RES(36,k) = 3.0
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_OPWRK -- Open the work space
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_opwrk ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr ( 'CALCA',  LX*LY, IPDATA,  istat )			!Get temp work space for data,fit,residuals,,,.
      call gtwrkr ( 'CALCB',  LX*LY, IPDATAR, istat )

      call gtwrkr ( 'DISP',  LXWD*LYWD, IPDISP,  istat )		!Open display area

      call gtwrkr ( 'MAPMAP', MX*MY, IPMAP, istat )			!Open Profile Map array
      call azeror ( %val(IPMAP), MX*MY )

      call gtwrkr ( 'RESMAP', LXM*LYM, IPTR, istat )			!Open Residuals array
      call azeror ( %val(IPTR),  LXM*LYM )
      call gtwrkr ( 'RESH',   LXM*LYM, IPTH, istat )
      call azeror ( %val(IPTH),  LXM*LYM )
      call gtwrkr ( 'DISC',   LXM*LYM, IPOCC, istat )
      call azeror ( %val(IPOCC), LXM*LYM )
      call gtwrkr ( 'DISD',   LXM*LYM, IPOCD, istat )
      call azeror ( %val(IPOCD), LXM*LYM )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DOWING -- Calculate the gaussian wing
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_dowing ( )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer ipwa, ipwb, ipwc, ipwd, kxc(2), kyc(2), istat
      real amax, amin, astd, aam
      logical ok
Cbegin


      if ( ST_FAILED ) return

      if ( .not.DONEMEAN ) then
         ok = .true.
         call printo ( 'You have not calculated the mean profile'//
     +                 '. Is this OK?' )
         call get1b ( 'OK', ok, .true. )
         if ( ST_FAILED ) return
         if ( .not.ok ) return
      endif

      if ( .not.DONERANG ) then
         kxc(1) = 1
         kxc(2) = NX
         kyc(1) = 1
         kyc(2) = NY
         if ( IMTYPE.eq.'SHORT' ) then
            call ranges ( %val(IPIM), NX, NY, kxc, kyc, INVAL, aam,
     +                    astd, istat )
         else
            call ranger ( %val(IPIM), NX, NY, kxc, kyc, RINVAL, aam,
     +                    astd, istat )
         endif
         amax = (aam+3.0*astd)*BS + BZ
         amin = (aam-3.0*astd)*BS + BZ
         DONERANG = .true.
      endif

      LXW = 2*LX
      LYW = 2*LY
      LXWD = 2*LXW
      LYWD = 2*LYW
      LYWN = LYW*TBY
      call gtwrkr ( 'WINGA', LXW*LYW,  ipwa, istat )
      call gtwrkr ( 'WINGB', LXW*LYWN, ipwb, istat )
      call gtwrkr ( 'WINGC', LX*LY,  ipwc, istat )
      call gtwrkr ( 'WINGD', LX*LY,  ipwd, istat )
      call pr_wing ( %val(ipwa), %val(ipwb), %val(ipwc), %val(ipwd),
     +               amax, amin )
      call wrkcan ( 'WINGA' )
      call wrkcan ( 'WINGB' )
      call wrkcan ( 'WINGC' )
      call wrkcan ( 'WINGD' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FXPROF -- Change the fixed/variable flags on a profile
C
C    a j penny              ral              1989-04-11

      subroutine pr_fxprof ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k
      logical oldfix, newfix
      character*70 text
      character*5 pname(5)
      data pname / 'Rmaj', 'Rmin', 'Power', 'PRmaj', 'PRmin' /
Cbegin

      do k = 1, 5
         write ( text, '('' Profile parameter '',a5)' ) pname(k)
         call printo ( text )
         oldfix = FIXPROF(k)
         call get1b ( 'FIXPROF', newfix, oldfix )
         if ( ST_FAILED ) return
         FIXPROF(k) = newfix
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FIXXY -- Changes stars fixed positions flag, positions
C
C  alan penny                ral               nov 89

      subroutine pr_fixxy ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      real rva, rvb
      integer iv
      logical more
Cbegin


      if ( ST_FAILED ) return						!Check failure flag

      call printo ( ' ' )
      call printo ( 'Entry of 0 will stop looping' )
      call printo ( 'Entry of +ve number will fix that star' )
      call printo ( 'Entry of -ve number will unfix that +ve star' )
      iv = 1
      more = .true.							!Get posn
      do while ( more )
         call get1i ( 'NUMBER', iv, iv, -1*TBY, TBY )
         if ( ST_FAILED ) return
         if ( iv.gt.0 .and. iv.le.TBY  ) then
            XYNFIX(iv) = 1
            rva = RES(1,iv)
            rvb = RES(2,iv)
            call get2r ( 'POSN', rva, rvb, .false., -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            XNFIX(iv) = rva
            YNFIX(iv) = rvb
         elseif ( iv.lt.0 .and. iv.ge. -1*TBY ) then
            XYNFIX(iv) = 0
         elseif ( iv.eq.0 ) then
            more = .false.
         endif
      enddo

      call printo ( 'Reminder - What about companions?' )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHPROF -- Change profile
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_chprof ( )

      implicit none
      include 'profile.inc'
C--
      integer lxa, lya, magnifo, istat
      real alx, aly
      logical flag
      character*70 text
Cbegin


      call pr_chprofpar ( flag )					!Change the parameters
      if ( .not.flag ) return						! Any actually changed?

      DONERES = .false.
      DONEMAP = .false.
      DONEFIX = .false.
      DONESTOR = .false.
      DONEVOL = .false.

      magnifo = MAGNIF
      call subdiv ( SR(1), SR(2), MAGNIF )
      if ( magnifo.ne.MAGNIF ) then
         LXM = LX*MAGNIF
         LYM = LY*MAGNIF
         call wrkcan ( 'RESMAP' )
         call gtwrkr ( 'RESMAP', LXM*LYM, IPTR,  istat )
         call wrkcan ( 'RESH' )
         call gtwrkr ( 'RESH',   LXM*LYM, IPTH,  istat )
         call wrkcan ( 'DISC' )
         call gtwrkr ( 'DISC',   LXM*LYM, IPOCC, istat )
         call wrkcan ( 'DISD' )
         call gtwrkr ( 'DISD',   LXM*LYM, IPOCD, istat )
      endif

      call azeror ( %val(IPTR), LXM*LYM )
      call azeror ( %val(IPTH), LXM*LYM )
      call printo ( 'Had to zero residuals - profile changed' )
      if ( MAGNIF.ne.magnifo ) call pr_upmap ( magnifo, LXR, LYR )

      call boxeli ( SR(1), SR(2), SR(6), alx, aly )
      lxa = 10.0*alx
      lya = 10.0*aly
      if ( LX.ne.lxa .or. LY.ne.lya ) then
         write ( text, '(''Present box size = '',2i4)' ) lx, ly
         call printo ( text )
         write ( text, '(''Suggested new box size = '',2i4)' ) lxa, lya
         call printo ( text )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_MEAN -- Calc mean and get good stars
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_mean ( kopt )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: Involve user, or not? (1:2)
C--
      logical flag
      integer magnifo, lxa, lya, istat
      real alx, aly
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      call pr_areject ( flag, kopt )					!Get new mean profile
      DONEMEAN = .true.
      if ( .not.flag ) return						! Profile actually changed?

      DONERES = .false.							!If mean profile changed,
      DONEMAP = .false.							! look at consequnces
      DONEFIX = .false.
      DONESTOR = .false.
      DONEVOL = .false.

      magnifo = MAGNIF							!Change in pixel subdivision
      call subdiv ( SR(1), SR(2), MAGNIF )
      if ( MAGNIF.ne.magnifo ) then
         LXM = LX*MAGNIF
         LYM = LY*MAGNIF
         call wrkcan ( 'RESMAP' )
         call gtwrkr ( 'RESMAP', LXM*LYM, IPTR,  istat )
         call wrkcan ( 'RESH' )
         call gtwrkr ( 'RESH',   LXM*LYM, IPTH,  istat )
         call wrkcan ( 'DISC' )
         call gtwrkr ( 'DISC',   LXM*LYM, IPOCC, istat )
         call wrkcan ( 'DISD' )
         call gtwrkr ( 'DISD',   LXM*LYM, IPOCD, istat )
      endif

      call azeror ( %val(IPTR), LXM*LYM )
      call azeror ( %val(IPTH), LXM*LYM )
      call printo ( 'Had to zero residuals - profile changed' )
      if ( MAGNIF.ne.magnifo ) call pr_upmap ( magnifo, LXR, LYR )

      if ( kopt.eq.2 ) then
         call pr_chbox ( 2 )
      else
         call boxeli ( SR(1), SR(2), SR(6), alx, aly )
         lxa = 10.0*alx
         lya = 10.0*aly
         if ( LX.ne.lxa .or. LY.ne.lya ) then
            write ( text, '(''Present box size = '',2i4)' ) LX, LY
            call printo ( text )
            write ( text, '(''Suggested new box size = '',2i4)' )lxa,lya
            call printo ( text )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_OSTORE -- Store results
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_ostore ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k, ipo, ipmapt, maxout, ipxyp, ipxyo, istat
      real    angle, robs, obscale, obzero
      logical ok
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      ok = .true.							!Check if ok to do. If not return
      if ( .not.DONEMAP ) then
         call printo ( 'You have not calculated the Profile Map'//
     +                 ' at all' )
         call printo ( ' - or - since the last change. Is this OK?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
      elseif ( .not.DONEMEAN ) then
         call printo ( 'You have not calculated the mean profile'//
     +                 ' at all' )
         call printo ( ' - or - since the last change. Is this OK?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
      endif
      if ( .not.ok ) return

      call printo ( 'Image to put profile parameters into' )		!Store the data
      call opimsw ( 'OUT', ipo, MX, MY, .true., istat )
      if ( ST_FAILED ) return
      if ( istat.eq.0 ) then
         call get1c ( 'TITLE', text, 'Profile from PROFILE', .true. )
         if ( ST_FAILED ) return
         obscale = 0.0001
         obzero = 0.0
         call ptdesr ( 'OUT', 'BSCALE', obscale )
         call ptdesr ( 'OUT', 'BZERO', obzero )
         call ptdesc ( 'OUT', 'TITLE', text )
         call ptdesi ( 'OUT', 'MAGNIF', MAGNIF )
         call ptdesr ( 'OUT', 'RX',  SR(1) )
         call ptdesr ( 'OUT', 'RY',  SR(2) )
         call ptdesr ( 'OUT', 'P',   SR(3) )
         call ptdesr ( 'OUT', 'PRX', SR(4) )
         call ptdesr ( 'OUT', 'PRY', SR(5) )
         angle = SR(6)*HOPI
         call ptdesr ( 'OUT', 'THETA', angle )
         call ptdesr ( 'OUT', 'QH', SR(7) )
         call ptdesr ( 'OUT', 'QR', SR(8) )
         call ptdesr ( 'OUT', 'QP', SR(9) )
         call ptdesr ( 'OUT', 'QBASE', QBASE )
         call ptdesr ( 'OUT', 'VOL', VOL )
         call ptdesr ( 'OUT', 'VOLRAD', VOLRAD )
         call ptdesi ( 'OUT', 'MAPMAX', MZ )
         call gtwrkr ( 'MAPTEMP', MX*MY, ipmapt, istat )
         call amovr  ( %val(IPMAP), %val(ipmapt), MX*MY )
         robs = 1.0/obscale
         call arrsc ( %val(ipmapt), MX, MY, robs, obzero )
         call achtrs ( %val(ipmapt), %val(ipo), MX*MY )
         call wrkcan ( 'MAPTEMP' )
         call canpar ( 'OUT' )
      endif

      call printo ( 'File to put stars details into' )			!Get the output list of
      call optabw ( 'OUTSTARS', ipxyo, TBXV, TBY, .true., istat )	! results of main fits
      if ( ST_FAILED ) return
      if ( istat.eq.0 ) then
         call get1c ( 'TITLE', text, text, .true. )
         if ( ST_FAILED ) return
         call ptdesc ( 'OUTSTARS', 'TITLE', text )
         do k = 1, TBX
            call pthead ( 'OUTSTARS', k, HEADER(k), istat )
         enddo
         call coprr ( RES, TBX, TBY, 1, TBX, 1, TBY,
     +                %val(ipxyo), TBXV, TBY, 6, 1 )
         call ident ( %val(ipxyo), TBXV, TBY )
         call canpar ( 'OUTSTARS' )
      endif

      maxout = 0							!Load the output of the possible
      do k = 1, MAXTOT							! companion stars + bads
         if ( COM(6,k).gt.0.5 ) maxout = maxout + 1
      enddo
      if ( maxout.ne.0 ) then
         call printo ( 'File to put companions details into' )
         call optabw ( 'OUTCOMPS', ipxyp, 11, maxout, .true., istat )
         if ( ST_FAILED ) return
         if ( istat.eq.0 ) then
            call get1c ( 'TITLE', text, text, .true. )
            if ( ST_FAILED ) return
            call ptdesc ( 'OUTCOMPS', 'TITLE', text )
            do k = 1, 6
               call pthead ( 'OUTCOMPS', k, WHEADER(k), istat )
            enddo
            call pr_loadcom ( COM, MAXTOT, %val(ipxyp), 11, maxout )
            call ident ( %val(ipxyp), 11, maxout )
            call canpar ( 'OUTCOMPS' )
         endif
      endif

      DONESTOR = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHMAP -- Change map size. Remap map.
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_chmap ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      real alx, aly
      integer lxa, lya, lxrn, lyrn, magnifo
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      call boxeli ( SR(1), SR(2), SR(6), alx, aly )
      lxa = 10.0*alx
      if ( lxa.gt.200 ) lxa = 200
      lya = 10.0*aly
      if ( lya.gt.200 ) lya = 200
      write ( text, '(1h ,'' Old X Y sides of Profile Map'',2i5)' )
     +               LXR, LYR
      call printo ( text )
      write ( text, '(1h ,'' Suggested new sides of Profile Map '',
     +               2i5)' ) lxa, lya
      call printo ( text )
      lxrn = lxa
      lyrn = lya
      call get2i ( 'SIZE', lxrn, lyrn, .true., 1, 200 )
      if ( ST_FAILED ) return
      if ( LXR.ne.lxrn .or. LYR.ne.lyrn ) then
         magnifo = MAGNIF
         call subdiv ( SR(1), SR(2), MAGNIF )
         call pr_upmap ( magnifo, lxrn, lyrn )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHBOX -- Change box size. remap residuals, change posns of
C             boxes + display boxes
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_chbox ( kopt )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer    kopt		!i: Involve user, or not? (1:2)
C--
      real alx, aly
      integer k, ipmapt, lxa, lya, lxrn, lyrn, mxo, myo, istat
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      call boxeli ( SR(1), SR(2), SR(6), alx, aly )			!Calc suggested box size
      lxa = 10.0*alx
      if ( lxa.gt.200 ) lxa = 200
      lya = 10.0*aly
      if ( lya.gt.200 ) lya = 200

      write ( text, '(1h ,'' Old X Y sides of box'',2i5)' ) LX, LY	!Get new measuring box size
      call printo ( text )
      write (text,'(1h ,'' Suggested X Y sides of box'',2i5)')lxa,lya
      call printo ( text )
      if ( kopt.eq.1 ) call get2i ( 'SIZE', lxa, lya, .true., 1, 200 )
      if ( ST_FAILED ) return

      if ( LX.ne.lxa .or. LY.ne.lya ) then
         do k = 1, TBY
            LXS(k) = LXS(k) + real((LX/2)-1) - real((lxa/2)-1)
            LYS(k) = LYS(k) + real((LX/2)-1) - real((lxa/2)-1)
         enddo
         LX = lxa
         LY = lya
         LXM = LX*MAGNIF
         LYM = LY*MAGNIF
         call wrkcan ( 'RESMAP' )
         call gtwrkr ( 'RESMAP', LXM*LYM, IPTR, istat )
         call azeror ( %val(IPTR), LXM*LYM )
         call wrkcan ( 'RESH' )
         call gtwrkr ( 'RESH',   LXM*LYM, IPTH, istat )
         call azeror ( %val(IPTH), LXM*LYM )
         call printo ( 'Residuals zeroed' )

         call wrkcan ( 'DISC' )
         call gtwrkr ( 'DISC', LXM*LYM, IPOCC, istat )
         call wrkcan ( 'DISD' )
         call gtwrkr ( 'DISD', LXM*LYM, IPOCD, istat )

         DONERES = .false.
         DONEFIX = .false.
         DONESTOR = .false.

         call wrkcan ( 'CALCA' )
         call wrkcan ( 'CALCB' )
         call gtwrkr ( 'CALCA', LX*LY, IPDATA,  istat )
         call gtwrkr ( 'CALCB', LX*LY, IPDATAR, istat )
         call wrkcan ( 'DISP' )
         LXD = 2*LX
         LYD = 2*LY
         LXW = 2*LX
         LYW = 2*LY
         LXWD = 2*LXW
         LYWD = 2*LYW
         call gtwrkr ( 'DISP',  LXWD*LYWD, IPDISP,  istat )

      endif

      call printo ( ' ' )
      call printo ( 'You may also wish to change the size of the box' )
      Call printo ( 'for the profile map to match the measuring box' )
      write ( text, '(1h ,'' Old X Y sides of Profile Map'',2i5)' )	!Get new profile map box size
     +        LXR, LYR
      call printo ( text )
      write ( text, '(1h ,'' Suggested new sides of Profile Map '',
     +               2i5)' ) lxa, lya
      call printo ( text )
      lxrn = lxa
      lyrn = lya
      if ( kopt.eq.1 ) call get2i ( 'SIZE', lxrn, lyrn, .true.,1,200)
      if ( ST_FAILED ) return
      if ( LXR.ne.lxrn .or. LYR.ne.lyrn ) then
         mxo = MX
         myo = MY
         call gtwrkr ( 'MAPTEMP', mxo*myo, ipmapt, istat )
         call amovr ( %val(IPMAP), %val(ipmapt), MX*MY )
         LXR = lxrn
         LYR = lyrn
         MX = LXR*MAGNIF
         MY = LYR*MAGNIF
         MAPX = MX
         MAPY = MY
         call wrkcan ( 'MAPMAP' )
         call gtwrkr ( 'MAPMAP', MX*MY, IPMAP, istat )
         call azeror ( %val(ipmap), MX*MY )
         call pr_copmap ( %val(ipmapt), mxo, myo, MAGNIF,
     +                    %val(IPMAP), MX, MY, MAGNIF )
         call wrkcan ( 'MAPTEMP' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_UPMAP -- Adjust map for new size or magnification
C             New values of LXR, LYR, MX, MY set
C
C  alan penny              ral                 1988-12-20

      subroutine pr_upmap ( magnifo, lxrn, lyrn )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer   magnifo         !i: Old magnification
      integer   lxrn            !i: New map X size
      integer   lyrn            !i: New map Y size
C--
      integer mxo, myo, ipmapt, istat
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr ( 'MAPTEMP', MX*MY, ipmapt, istat )			!Make Temp copy of map
      call amovr ( %val(IPMAP), %val(ipmapt), MX*MY )
      mxo = MX
      myo = MY

      LXR = lxrn							!Create new map area
      LYR = lyrn
      MX = LXR*MAGNIF
      MY = LYR*MAGNIF
      MAPX = MX
      MAPY = MY
      call wrkcan ( 'MAPMAP' )
      call gtwrkr ( 'MAPMAP', MX*MY, IPMAP, istat )
      call azeror ( %val(IPMAP), MX*MY )

      call pr_copmap ( %val(ipmapt), mxo, myo, magnifo,			!Move old map from temporary
     +                 %val(IPMAP),  MX,  MY,  MAGNIF )			! area into new map array


      call wrkcan ( 'MAPTEMP' )					!Delete temp area


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GCOMP -- Get new companion lists
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_gcomp ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      logical valid
      integer k, tbxe, tbye, ipecom, istat
Cbegin


      if ( ST_FAILED ) return

      valid = .false.							!Get the position list of
									! the possible companions and bads
      do while ( .not.valid )
         call optabr ( 'INCOMPS', ipecom, tbxe, tbye, .true., istat )
         if ( ST_FAILED ) return
         if ( istat.eq.2 ) then
            valid = .true.
         else
            if ( istat.ne.0 ) then
               call printo ( 'ERROR: Not a valid table' )
            elseif ( tbxe.ne.(6+5) ) then
               call printo ( 'ERROR: Not a companions table' )
            else
               valid = .true.

               call azeror ( COM, 6*MAXTOT )
               call coprr ( %val(ipecom), tbxe, tbye, 6, tbxe, 1, tbye,
     +                      COM, 6, MAXTOT, 1, 1 )
               do k = 1, MAXTOT
                  if ( (nint(COM(5,k)).gt.TBY) .or.
     +                 (nint(COM(5,k)).le.0)           ) then
                     COM(6,k) = -1.0*abs(COM(6,k))
                  endif
               enddo

            endif
            call canpar ( 'INCOMPS' )
         endif
      enddo

      DONEMEAN = .false.
      DONERES = .false.
      DONEMAP = .false.
      DONEFIX = .false.
      DONESTOR = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_GPROF -- Get new profile file
C
C     a j penny                 stsci            1988-03-24

      subroutine pr_gprof ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      logical lv, flag
      integer magnifo, k, ierr, mxa, mya, mza, ipinr, ipinra, istat
      real    pbs, pbz, asr(6)
Cbegin


      if ( ST_FAILED ) return

      DONEMEAN = .false.
      DONERES = .false.
      DONEMAP = .false.
      DONEFIX = .false.
      DONESTOR = .false.

      call amovr ( SR, asr, 6 )						!Note old profile
      magnifo = MAGNIF

      call get_mprof ( 'INPROF', ipinr, SR, mxa, mya, mza, MAGNIF, 	!Get new profile
     +                 MAPX, MAPY, pbs, pbz, QBASE, VOL, VOLRAD, ierr )
      if ( ST_FAILED ) return

      if ( ierr.ne.0 ) then
         if ( ierr.eq.1 ) call printo ( 'ERROR: Profile file wrong' )
         if ( ierr.eq.2 ) call printo ( 'ERROR: Profile unusable' )
         return
      endif

      if ( mza.ne.1 ) then
         call printo('ERROR: Can only deal with single maps at present')
         return
      endif

      call pr_getangle  ( FIXANGLE, SR(6), flag )
      if ( ST_FAILED ) return
      if ( DOMAP ) then
         call printo ( 'Profile Map is now used' )
      else
         call printo ( 'Profile Map is not now used' )
      endif
      call get1b ( 'USEMAP', DOMAP, .true. )
      if ( ST_FAILED ) return

      do k = 1, 6							!See if profile has changed
         if ( asr(k).ne.SR(k) ) flag = .true.
      enddo
      call gtwrkr ( 'INPROFT', mxa*mya*mza, ipinra, istat )
      call copsr ( %val(ipinr),  mxa, mya*mza, 1, mxa, 1, mya,
     +             %val(ipinra), mxa, mya*mza, 1, 1 )
      call arrsc ( %val(ipinra), mxa, mya, pbs, pbz )
      call canpar ( 'INPROF' )
      if ( MX.ne.mxa .or. MY.ne.mya ) then
         flag = .true.
      else
         call adiffr ( %val(ipinra), %val(IPMAP), MX*MY, lv )
         if ( lv ) flag = .true.
      endif

      DONEMEAN = .true.
      if ( flag ) DONEVOL = .false.

      MX = mxa
      MY = mya
      MZ = mza
      MAPX = MX
      MAPY = MY
      LXR = MX/MAGNIF
      LYR = MY/MAGNIF
      call wrkcan ( 'MAPMAP' )
      call gtwrkr ( 'MAPMAP', MX*MY, IPMAP, istat )
      call coprr ( %val(ipinra), mxa, mya*mza, 1, MX, 1, MY,
     +             %val(IPMAP), MX, MY, 1, 1 )
      call wrkcan ( 'INPROFT' )

      if ( magnifo.ne.MAGNIF ) then
         LXM = LX*MAGNIF
         LYM = LY*MAGNIF
         call wrkcan ( 'RESMAP' )
         call gtwrkr ( 'RESMAP', LXM*LYM, IPTR,  istat )
         call wrkcan ( 'RESH' )
         call gtwrkr ( 'RESH',   LXM*LYM, IPTH,  istat )
         call wrkcan ( 'DISC' )
         call gtwrkr ( 'DISC',   LXM*LYM, IPOCC, istat )
         call wrkcan ( 'DISD' )
         call gtwrkr ( 'DISD',   LXM*LYM, IPOCD, istat )
      endif

      if ( flag ) then
         call azeror ( %val(IPTR), LXM*LYM )
         call azeror ( %val(IPTH), LXM*LYM )
         call printo ( 'Residuals zeroed' )
      endif

      call pr_listfit



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FIT -- Do the fits
C
C  a j penny             stsci              1988-03-15

      subroutine pr_fit ( kopt, kuser )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'

      integer	kopt			!i: Fit option (1=changed stars;2=all accepted stars;
					!               3=one star)
      integer   kuser			!i: Involve user, or not, or not and fixmean? (1:2:3)
C--
      integer nfix, kfixm, kfitm, knum, k, numpar, istat, ksent, jstar,
     +        lxa, lya, jfit(36), jfitf(36), numm, numf, jxs, jys, iter,
     +        nin
      real    asr(6), bsr(9), cc(36), ccf(36)
      logical doneone, toobig, parfix
      character*70 text

      character*1000 topt
      data topt /
     +    'update:old:fixmean:fixold:mean:
     +     none' /
      integer nthelp
      parameter ( nthelp=13 )
      character*68 thelp(nthelp)
      data thelp /
     + 'Method of inputing START values to the iterations, either to',
     + 'speed things up, or  get better values. This CAN ALSO FIX the',
     + 'profile.',
     + 'Option   Function',
     + '------   --------',
     + 'Fixmean  Fix at the last calculated mean profile (no profile ',
     + '          fitting)',
     + 'Fixold   Fix profile at last fit of each star (no profile ',
     + '          fitting)',
     + 'Mean     Take last calculated mean profile ',
     + 'None     Do not do this fitting',
     + 'Old      Take last fit for each star ',
     + 'Update   Take mean of fits made of previous stars in this list'/
Cbegin


      if ( ST_FAILED ) return

      numpar = 1

      if ( kopt.eq.3 ) call get1i ( 'NUMBER', knum, 1, 1, TBY )
      if ( ST_FAILED ) return

      if ( kuser.eq.1 ) then						!Get type of fit
         call get_job ( 'FITTYPE', topt, kfitm, 2, thelp, nthelp )
         if ( ST_FAILED ) return
         if ( kfitm.eq.6 ) return
      else
         kfitm = 2
         if ( kuser.eq.3 ) kfitm = 3
      endif

      if ( kfitm.ne.3 ) DONEMEAN = .false.				!Calculating new profile values

      if ( kopt.eq.2 ) then						!Do for all the stars or only those changed, or just one
         do k = 1, TBY
            RES(36,k) = 3.0
         enddo
      endif

      call printo ( ' ' )						!Write heading
      write ( text, '('' Box  X length = '',i4,''  Y length = '',i4)' )
     +        LX, LY
      call printo ( text )
      call printo ( '   No   Height Its     Rmaj    Rmin'//
     +              '    P       PRmaj   PRmin  Theta  ' )

      if ( DOPAR ) call pr_par_sii ( numpar, istat )			!Number of par processors

      ksent = 0
      doneone = .false.							!Solve for each star
      do k = 1, TBY
         if (
     +       ( kopt.eq.3 .and. k.eq.knum ) .or.
     +       ( (RES(34,k).gt.0.5) .and.
     +         ((kopt.eq.2).or.((kopt.eq.1).and.(RES(36,k).ge.2.0))) )
     +                                                         ) then

            LXS(k) = RES(1,k) - real((LX/2)-1)
            LYS(k) = RES(2,k) - real((LY/2)-1)

            nfix = 6
            if ( FIXANGLE ) nfix = 5
            asr(6) = SR(6)
            if ( kfitm.eq.1 ) then
               if ( .not.doneone ) then
                  call amovr ( RES(12,k), asr, nfix )
               else
                  call pr_updatefit ( k-1 )
               endif
               kfixm = 0
            elseif ( kfitm.eq.2 ) then
               call amovr ( RES(12,k), asr, nfix )
               kfixm = 0
            elseif ( kfitm.eq.3 ) then
               call amovr ( SR, asr, nfix )
               kfixm = 1
            elseif ( kfitm.eq.4 ) then
               call amovr ( RES(12,k), asr, nfix )
               kfixm = 1
            elseif ( kfitm.eq.5 ) then
               call amovr ( SR, asr, nfix )
               kfixm = 0
            endif

            call amovr ( asr, bsr, 6 )
            call amovr ( SR(7), bsr(7), 3 )

            call pr_partoobig ( k, toobig )

            if ( DOPAR .and. .not.toobig ) then				!Use par processors
               if ( ksent.ge.numpar ) then
                  call pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin,
     +                            lxa, lya )
                  call pr_dores ( jstar, RES, TBX, TBYMAX, COM, MAXTOT,
     +                            1, cc, ccf, jxs, jys, iter, nin,
     +                            lxa, lya, MAXCL, MAXFA )
               endif
               call pr_fitload ( %val(IPDATAR), FIXANGLE, FIXPROF, bsr,
     +                           k, RES, COM, kfixm, cc, jfit, numm,
     +                           ccf, jfitf, numf, parfix, LX, LY, jxs,
     +                           jys, nin )
               call pr_parout ( %val(IPDATAR), LX, LY, %val(IPMAP),
     +                          FIXANGLE, FIXPROF, k, kfixm, cc,
     +                          jfit, numm, ccf, jfitf, numf, parfix,
     +                          jxs, jys, nin, ITSLIM )
               ksent = ksent + 1
            else
               call pr_solve ( %val(IPDATA), %val(IPDATAR),
     +                         FIXANGLE, FIXPROF, bsr, ITSLIM, k, RES,
     +                         COM, 1, kfixm )
            endif

            if ( RES(36,k).eq.3.0 ) then
               RES(36,k) = 1.0
            elseif ( RES(36,k).eq.2.0 ) then
               RES(36,k) = 0.0
            endif

            doneone = .true.

         endif

      enddo

      if ( DOPAR ) then
         if ( ksent.eq.0 ) then
            call pr_par_fend
         else
            do k = 1, min(ksent,numpar)
               if ( k.eq.1 .and. ksent.lt.numpar ) call pr_par_fend
               call pr_parin ( jstar, cc, ccf, jxs, jys, iter, nin, lxa,
     +                         lya )
               call pr_dores ( jstar, RES, TBX, TBYMAX, COM, MAXTOT,
     +                         1, cc, ccf, jxs, jys, iter, nin,
     +                         lxa, lya, MAXCL, MAXFA )
               if ( k.eq.1 .and. ksent.ge.numpar ) call pr_par_fend
            enddo
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DISPFIT -- Display the areas and the fits
C
C  a j penny               stsci              1988-03-19

      subroutine pr_dispfit ( kopt, koptb, klx, kly, klxs, klys,
     +                        klxd, klyd, klxsd, klysd )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer   kopt		!i: Flag (0=all areas/fits;1=one area/fit;
				!         2=all areas/no fits)
      integer   koptb		!i: Flag for other overlay lines (1=yes/0=no)

      integer   klx             !i: X size of fitting area
      integer   kly             !i: Y size of fitting area
      integer   klxs(TBY)       !i: X posns of blh of fitting areas
      integer   klys(TBY)       !i: Y posns of blh of fitting areas
      integer   klxd            !i: X size of display areas
      integer   klyd            !i: Y size of display areas
      integer   klxsd(TBY)      !i: X posns of blh of display areas
      integer   klysd(TBY)      !i: Y posns of blh of display areas
C--
      integer knum, kxc(2), kyc(2), jx1, jy1, jx2, jy2, k, nin, ierr,
     +        kw, i
      real am, std, amax, amin, rv, rva
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 ) call get1i ( 'NUMBER', knum, 1, 1, TBY )		!If just one star, get it
      if ( ST_FAILED ) return

      call pr_dsopen ( ierr ) 						!Open display if not already open
      if ( ierr.ne.0 ) return

      if ( .not.DONERANG ) then						!Display star areas
         kxc(1) = 1
         kxc(2) = NX
         kyc(1) = 1
         kyc(2) = NY
         if ( IMTYPE.eq.'SHORT' ) then
            call ranges ( %val(IPIM), NX, NY, kxc, kyc, INVAL, am,
     +                    std, i )
         else
            call ranger ( %val(IPIM), NX, NY, kxc, kyc, RINVAL, am,
     +                    std, i )
         endif
         amax = (am+3.0*std)*BS + BZ
         amin = (am-3.0*std)*BS + BZ
         DONERANG = .true.
      endif

      do k = 1, TBY							!Top part - areas
         if ( kopt.eq.0 .or. kopt.eq.2 .or.
     +        (kopt.eq.1.and.k.eq.knum) ) then
            if ( RES(36,k).eq.3.0 ) then
               RES(36,k) = 2.0
            elseif ( RES(36,k).eq.1.0 ) then
               RES(36,k) = 0.0
            endif
            klxsd(k) = RES(1,k) - real((klxd/2)-1)
            klysd(k) = RES(2,k) - real((klyd/2)-1)
            jx2 = klxsd(k) + klxd - 1
            jy2 = klysd(k) + klyd - 1
            if ( IMTYPE.eq.'SHORT' ) then
               call copvsr ( %val(IPIM), NX, NY, %val(IPDISP), klxd,
     +                       klyd, klxsd(k), jx2, klysd(k), jy2, BS,
     +                       BZ, INVAL, RINVAL, nin, 0 )
            else
               call copvrr ( %val(IPIM), NX, NY, %val(IPDISP), klxd,
     +                       klyd, klxsd(k), jx2, klysd(k), jy2, BS,
     +                       BZ, RINVAL, nin, 0 )
            endif
            jx1 = 1 + real((klxd/2)-1) - real((klx/2)-1)
            jx2 = jx1 + klx - 1
            jy1 = 1 + real((klyd/2)-1) - real((kly/2)-1)
            jy2 = jy1 + kly - 1
            call pr_disresa ( %val(IPDISP), klxd, klyd, k, jx1, jx2,
     +                        jy1, jy2, 2, amin, amax, 2 )
         endif
      enddo

      if ( kopt.eq.0 .or. kopt.eq.1 ) then				!Bottom part - areas less fits
         do k = 1, TBY
            if ( kopt.eq.0 .or. (kopt.eq.1.and.k.eq.knum) ) then
               if ( RES(36,k).eq.3.0 ) then
                  RES(36,k) = 2.0
               elseif ( RES(36,k).eq.1.0 ) then
                  RES(36,k) = 0.0
               endif
               klxsd(k) = RES(1,k) - real((klxd/2)-1)
               klysd(k) = RES(2,k) - real((klyd/2)-1)
               jx2 = klxsd(k) + klxd - 1
               jy2 = klysd(k) + klyd - 1
               if ( IMTYPE.eq.'SHORT' ) then
                  call copvsr ( %val(IPIM), NX, NY, %val(IPDISP), klxd,
     +                          klyd, klxsd(k), jx2, klysd(k), jy2, BS,
     +                          BZ, INVAL, RINVAL, nin, 0 )
               else
                  call copvrr ( %val(IPIM), NX, NY, %val(IPDISP), klxd,
     +                          klyd, klxsd(k), jx2, klysd(k), jy2, BS,
     +                          BZ, RINVAL, nin, 0 )
               endif
               jx1 = 1 + real((klxd/2)-1) - real((klx/2)-1)
               jx2 = jx1 + klx - 1
               jy1 = 1 + real((klyd/2)-1) - real((kly/2)-1)
               jy2 = jy1 + kly - 1
               call pr_allsub ( %val(IPDISP), klxd, klyd, klxsd(k),
     +                          klysd(k), 1, klxs(k), klys(k), RES,
     +                          COM, k )
               call pr_loadbl ( %val(IPDISP), klxd, klyd, COM,
     +                          klxsd(k), klysd(k), k )
               call pr_disresa ( %val(IPDISP), klxd, klyd, k, jx1, jx2,
     +                           jy1, jy2, 0, rv, rva, 1 )
            endif
         enddo
      endif

      kw = 0
      if ( kopt.eq.1 ) kw = knum

      if ( DOGRID ) call pr_grid ( klx, kly, klxs, klys, klxd,		!Paint frame for calc areas
     +                             klyd, klxsd, klysd, TBY, kw )

      if ( koptb.eq.1 ) then						!Paint markers
                        call pr_paintlit ( kw )
                        call pr_paintcomp ( klxd, klyd, RES, COM,
     +                                      klxsd, klysd, kw )
                        call pr_paintbox ( klxd, klyd, COM, klxsd,
     +                                     klysd, kw )
                        endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FITSETUP -- Sets up for fit
C
C  a j penny             stsci                  1988-03-15

      subroutine pr_fitsetup ( )

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
      integer k
      real alx, aly
Cbegin


      if ( ST_FAILED ) return

      SR(1) = 2.0							!Put Lorentz profile
      SR(2) = 2.0
      SR(3) = 2.2
      SR(4) = SR(1)*5.0
      SR(5) = SR(2)*5.0
      SR(6) = 0.0
      SR(7) = 0.0
      SR(8) = 10.0
      SR(9) = 2.0
      QBASE = 0.0
      VOL = 22.31404
      VOLRAD = 30.0

C  Load default profile fits, if stars list was not MEASURE or
C  PROFILE output

      if ( .not.INMEAS .and. .not.INPROF ) then
         do k = 1, TBY
            RES(11,k) = 1.0
            call amovr ( SR, RES(12,k), 9 )
         enddo
      endif

      call boxeli ( SR(1), SR(2), SR(6), alx, aly )			!Box round star
      LX = 10.0*alx
      if ( LX.gt.200 ) LX = 200
      LY = 10.0*aly
      if ( LY.gt.200 ) LY = 200

      LXD = 2*LX							!Display size
      LYD = 2*LY

      call subdiv ( SR(1), SR(2), MAGNIF )				!Fit area
      LXM = LX*MAGNIF
      LYM = LY*MAGNIF
      LXR = LX
      LYR = LY

      MX = LX*MAGNIF							!Map details
      MY = LY*MAGNIF
      DOMAP = .true.
      MZ = 1
      MAPX = MX
      MAPY = MY
      SCALEMAP = 1.0

      do k = 1, TBY							!Set default box positions
         LXS(k)  = RES(1,k) - real((LX/2)-1)
         LYS(k)  = RES(2,k) - real((LY/2)-1)
         LXSD(k) = RES(1,k) - real((LXD/2)-1)
         LYSD(k) = RES(2,k) - real((LYD/2)-1)
      enddo

									!Set flags for:-
      DONEMEAN = .false.	! calculated mean profile
      DONEVOL = .true.		! calculated profile volume
      DONERES = .true.		! prof map filled with current prof fit resids
      DONEMAP = .true.		! calculated map
      DONESTOR = .false.	! results stored with current fit and box size
      DONEFIX = .true.		! solved all with fixed profile
      DONERANG = .false.	! calculated display range

C  Set all as ok and all as changed, if stars list is not PROFILE output

      if ( .not.INPROF ) then
         do k = 1, TBY
            RES(34,k) = 1.0
            RES(35,k) = 1.0
            RES(36,k) = 3.0
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_CHFITPAR -- Change the fit calculation control parameters
C
C  a j penny                          stsci                   1987-05-24

      subroutine pr_chfitpar ()

      implicit none
      include 'profile.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call get1r ( 'DAMP', DAMP, DAMP, 0.0, 100.0 )
      call get1r ( 'FITLIM', FTLIM, FTLIM, 0.0, 100.0 )
      call get1r ( 'HTLIM', HTLIM, HTLIM, 0.0, 100.0 )
      call get1r ( 'WDAMP', WDAMP, WDAMP, 0.0, 100.0 )
      call get1r ( 'WFITLIM', WFTLIM, WFTLIM, 0.0, 100.0 )
      call get1r ( 'WHTLIM', WHTLIM, WHTLIM, 0.0, 100.0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_DORES -- Store the result of a fit
C
C   a.j.penny                   ral                    1991 April

      subroutine pr_dores ( kstar, tres, tbx, tbymax, tcom, maxtot,
     +                      ktype, cc, ccf, jxs, jys, iter, nin, lx,
     +                      ly, maxcl, maxfa )

      implicit none
      include 'STARMAN_INC'

      integer   kstar			!i: Number of star being done
      integer   tbx			!i: No of columns in results table
      integer   tbymax			!i: No of rows in results table
      real      tres(tbx,tbymax)	!i/o: Results table
      integer   maxtot			!i: No of rows in companions table
      real      tcom(6,maxtot)		!i/o: Companions table
      integer   ktype			!i: Print out result (1=yes)
      real	cc(36)			!i: Results of fit
      real	ccf(36)			!i: Input to fit
      integer   jxs			!i: X position of blh corner of fit box
      integer   jys			!i: Y position of blh corner of fit box
      integer   iter			!i: No of iterations taken
      integer   nin			!i: No of invalid pixels
      integer   lx			!i: X size of fit box
      integer   ly			!i: Y size of fit box
      integer   maxcl			!i: Max no of close/bright companions
      integer   maxfa			!i: Max no of far/faint companions
C--
      integer k, nlistc, nlistf
      real    ah, oldx, oldy, bh, armaj, armin, ap, aprmaj, aprmin,
     +        angle
      character*72 text
      real pi
      parameter ( pi=3.1415926536 )
      external trunc
      real trunc
Cbegin


      if ( ST_FAILED ) return

      if ( ktype.eq.1 ) then						!Type out fit
         ah = trunc(cc(15),5)
         armaj  = trunc(cc(4),2)
         armin  = trunc(cc(5),2)
         ap     = trunc(cc(6),1)
         aprmaj = trunc(cc(7),3)
         aprmin = trunc(cc(8),3)
         angle  = cc(9)*180.0/pi
         angle  = trunc(angle,4)
         write ( text,
     +           '(1x,i4,f9.1,i4,f10.3,f8.3,f7.3,f8.2,f8.2,f8.2)' )
     +        kstar, ah, iter, armaj, armin, ap, aprmaj, aprmin, angle
         call printo ( text )
      endif

      oldx = tres(1,kstar)						!Note fit. Remove comps gone too small
      oldy = tres(2,kstar)
      tres(1,kstar) = cc(13) + jxs - 1.0
      tres(2,kstar) = cc(14) + jys - 1.0
      bh = cc(15)
      tres(3,kstar) = 30 - 2.5*alog10(max(0.00001,bh))
      if ( bh.lt.0.00001 .or. bh.gt.1.0e10 ) tres(3,kstar) = 50.0
      tres(4,kstar) = tres(1,kstar) - oldx
      tres(5,kstar) = tres(2,kstar) - oldy
      tres(6,kstar) = iter
      tres(7,kstar) = 0.0
      tres(8,kstar) = nin
      tres(9,kstar) = cc(15)
      tres(10,kstar) = cc(1)
      call amovr ( cc(4), tres(12,kstar), 9 )
      tres(29,kstar) = lx
      tres(30,kstar) = ly
      tres(35,kstar) = 1.0
      tres(32,kstar) = cc(2)
      tres(33,kstar) = cc(3)
      tres(37,kstar) = jxs
      tres(38,kstar) = jys

      nlistc = 0
      nlistf = 0
      do k = 1, maxtot
         if ( tcom(6,k).gt.0.5 .and. nint(tcom(5,k)).eq.kstar ) then
            if ( nint(tcom(6,k)).eq.1 ) then
               nlistc = min(maxcl,(nlistc+1))
               tcom(1,k) = cc(10+3*(nlistc+1)) + jxs - 1
               tcom(2,k) = cc(11+3*(nlistc+1)) + jys - 1
               tcom(3,k) = cc(12+3*(nlistc+1))
               if ( tcom(3,k).lt.0.1 ) tcom(6,k) = 0.0
            endif
            if ( nint(tcom(6,k)).eq.2 ) then
               nlistf = min(maxfa,(nlistf+1))
               tcom(1,k) = ccf(10+3*nlistf) + jxs - 1
               tcom(2,k) = ccf(11+3*nlistf) + jys - 1
               tcom(3,k) = ccf(12+3*nlistf)
               if ( tcom(3,k).lt.0.1 ) tcom(6,k) = 0.0
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FITLOAD -- Load data for fitting
C
C alan penny                     ral                   1991 April

      subroutine pr_fitload ( datar, tfixangle, tfixp, tsr, kstar, tres,
     +                        tcom, kfix, cc, jfit, numm, ccf, jfitf,
     +                        numf, parfix, lxa, lya, jxs, jys, nin )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer   lxa			!i: X size of area to fit
      integer   lya			!i: Y size of area to fit
      real      datar(lx,ly)		!o: Work space for image data
      logical   tfixangle		!i: Profile angle fixed (true) or var
      logical   tfixp(5)		!i: Profile pars fixed (true) or var
      real      tsr(9)			!i: Profile
      integer   kstar			!i: Number of star being done
      real      tres(TBX,TBYMAX)	!i/o: Results table
      real      tcom(6,MAXTOT)		!i/o: Companions table
      integer   kfix			!i: Profile fix option (0=no;1=yes)
      real      cc(36)			!o: Fit input parameters
      integer   jfit(36)		!o: Fit parameters controls
      integer   numm			!o: Number of (main and bright/close companions) stars
      real      ccf(36)			!o: Profile and posns for faint/distant comps
      integer   jfitf(36)		!o: Controls for profile and posns for faint/distant comps
      integer   numf			!o: Number of faint/far companions
      logical   parfix			!o: Fix profile flag
      integer   jxs			!o: X posn in image of blh of box
      integer   jys			!o: Y posn in image of blh of box
      integer   nin			!o: No of INVALID pixels in box
C--
      real prof(9), xp(8), yp(8), aht(8), xds(8), yds(8)
      integer xyf(8)
      integer jxe, jye, k
      real    rmaj, rmin, p, prmaj, prmin, theta, qp, qr, qh
      logical kfixed(5)
      data kfixed / .true., .true., .true., .true., .true. /
Cbegin


      if ( ST_FAILED ) return

      rmaj  = tsr(1)				!Load profile
      rmin  = tsr(2)
      p     = tsr(3)
      prmaj = tsr(4)
      prmin = tsr(5)
      theta = tsr(6)
      qh    = tsr(7)
      qr    = tsr(8)
      qp    = tsr(9)

      jxs = tres(1,kstar) - real((lxa/2)-1)				!Get the area into an array
      jxe = jxs + lxa - 1
      jys = tres(2,kstar) - real((lya/2)-1)
      jye = jys + lya - 1
      if ( IMTYPE.eq.'SHORT' ) then
         call copvsr ( %val(IPIM), NX, NY, datar, lxa, lya, jxs, jxe,
     +                 jys, jye, BS, BZ, INVAL, RINVAL, nin, 0 )
      else
         call copvrr ( %val(IPIM), NX, NY, datar, lxa, lya, jxs, jxe,
     +                 jys, jye, BS, BZ, RINVAL, nin, 0 )
      endif

      call pr_loadbl ( datar, lxa, lya, tcom, jxs, jys, kstar )		!Mask out bad areas

      prof(1) = rmaj							!Set up profile
      prof(2) = rmin
      prof(3) = p
      prof(4) = prmaj
      prof(5) = prmin
      prof(6) = theta
      prof(7) = qh
      prof(8) = qr
      prof(9) = qp
      cc(10)  = qh
      cc(11)  = qr
      cc(12)  = qp
      parfix  = .false.
      if ( kfix.eq.1 ) parfix = .true.

      xp(1)  = tres(1,kstar) - jxs + 1.0				!Set the positions of the main
      yp(1)  = tres(2,kstar) - jys + 1.0				! and close or bright companions
      aht(1) = tres(9,kstar)
      xds(1) = XNFIX(kstar) - jxs + 1.0
      yds(1) = YNFIX(kstar) - jys + 1.0
      xyf(1) = XYNFIX(kstar)

      numm = 1
      do k = 1, MAXTOT
         if ( nint(tcom(6,k)).eq.1 .and. nint(tcom(5,k)).eq.kstar ) then
            numm = min((MAXCL+1),(numm+1))
            xp(numm)  = tcom(1,k) - jxs + 1.0
            yp(numm)  = tcom(2,k) - jys + 1.0
            aht(numm) = tcom(3,k)
            xds(numm) = 1.0
            yds(numm) = 1.0
            xyf(numm) = 0
         endif
      enddo

      if ( parfix ) then						!Load the fitting input
         call pr_fifill ( datar, lxa, lya, parfix, .true., kfixed,
     +                    prof, xp, yp, aht, xds, yds, xyf, numm, cc,
     +                    jfit, RINVAL )
      else
         call pr_fifill ( datar, lxa, lya, parfix, tfixangle, tfixp,
     +                    prof, xp, yp, aht, xds, yds, xyf, numm, cc,
     +                    jfit, RINVAL )
      endif

      call pr_compset ( tcom, kstar, jxs, jys, cc, ccf, jfitf, numf )	!See if any faint companions,
									! and if so load arrays

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_SOLVE -- Fit a profile to a star with poss companions, bad areas
C     Takes an area, with a star in the middle, and up to 7 other stars
C     round it, and solves for the profile. It can also deal with 8
C     more faint or distant stars.
C
C     It needs rough input positions and a rough star radii.
C
C     It outputs the good profile and the positions and heights
C     of the main star and of the companions.
C
C   a.j.penny                   ral                    1991 April


      subroutine pr_solve ( data, dataw, tfixangle, tfixp, tsr,
     +                      titslim, kstar, tres, tcom, ktype, kfix )

      implicit none
      include 'profile.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      data(LX,LY)		!o: Work space for image data
      real      dataw(LX,LY)		!o: Work space for image data
      logical   tfixangle		!i: Profile angle fixed (true) or var
      logical   tfixp(5)		!i: Profile pars fixed (true) or var
      real      tsr(9)			!i: Profile
      integer   titslim			!i: Max no of iterations allowed
      integer   kstar			!i: Number of star being done
      real      tres(TBX,TBYMAX)	!i/o: Results table
      real      tcom(6,MAXTOT)		!i/o: Companions table
      integer   ktype			!i: Print out result (1=yes)
      integer   kfix			!i: Profile fix option (0=no;1=yes)
C--
      real cc(36), ccf(36), chisq
      integer jfit(36), jfitf(36), jxs, jys, numm, nin, numf, iter
      logical parfix
Cbegin


      if ( ST_FAILED ) return

      call pr_fitload ( data, tfixangle, tfixp, tsr, kstar,		!Load fit
     +                  tres, tcom, kfix, cc, jfit, numm, ccf, jfitf,
     +                  numf, parfix, LX, LY, jxs, jys, nin )

      if ( numf.gt.0 ) then						!Solve the profile
         call pr_wwfitlor ( data, LX, LY, dataw, cc, jfit, numm, iter,
     +                      ccf, jfitf, numf, titslim, KFITPR, parfix,
     +                      tfixangle, tfixp, FTLIM, HTLIM, DAMP,
     +                      RINVAL, %val(IPMAP), MX, MY, MAGNIF, DOMAP)
      else
         call pr_fitlor ( data, LX, LY, cc, jfit, numm, iter, titslim,
     +                    KFITPR, parfix, tfixangle, tfixp, FTLIM,
     +                    HTLIM, DAMP, RINVAL, %val(IPMAP), MX, MY,
     +                    MAGNIF, DOMAP, .false., .false., chisq )

      endif

      call pr_dores ( kstar, tres, TBX, TBYMAX, tcom, MAXTOT, ktype,	!Store and type result
     +                cc, ccf, jxs, jys, iter, nin, LX, LY, MAXCL,
     +                MAXFA )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_COMPSET -- Set CCA/JFITA from COM for removing faint/distant stars
C
C   a.j.penny                   ral                    85-12-31

      subroutine pr_compset ( tcom, kstar, jx, jy, cc, ccf, jfitf, numf)

      implicit none
      include 'profile.inc'

      real     tcom(6,MAXTOT)	!i: Array of companions info
      integer  kstar		!i: Main star number
      integer  jx		!i: X start of data array to be fitted
      integer  jy 		!i: Y start of data array to be fitted
      real     cc(36)		!i: Profile
      real     ccf(36)		!o: Profile and posns for faint/distant
      integer  jfitf(36)	!o: Fit controls for faint/distant
      integer  numf		!o: Number of faint/distant stars
C--
      real alx, aly
      integer j, k, ka
Cbegin


      numf = 0								!Find number of faint companions
      do k = 1, MAXTOT							! return if none
         if ( nint(tcom(6,k)).eq.2 .and. nint(tcom(5,k)).eq.kstar ) then
            numf = min(MAXFA,(numf+1))
         endif
      enddo
      if ( numf.eq.0 ) return

      call azeror ( ccf, 36 ) 						!Clear ccf
      call amovr ( cc, ccf, 12 )					!Load profile

      ka = 0								!Load star posns and heights
      do k = 1, MAXTOT
         if ( nint(tcom(6,k)).eq.2 .and. nint(tcom(5,k)).eq.kstar ) then
            ka = min(MAXFA,(ka+1))
            j = (ka-1)*3
            ccf(13+j) = tcom(1,k) - jx + 1.0
            ccf(14+j) = tcom(2,k) - jy + 1.0
            ccf(15+j) = tcom(3,k)
         endif
      enddo

      call azeroi (  jfitf, 36 )					!Load fit controls
      call amovki (  1, jfitf,  3 )
      call amovki ( -1, jfitf(4), 9 )
      alx = LX
      aly = LY
      do k = 13, 12+numf*3, 3
         jfitf(k) = 1
         jfitf(k+1) = 1
         if ( ccf(k).lt.-1.0 .or. ccf(k).gt.(alx+2.0) .or.
     +        ccf(k+1).lt.-1.0 .or. ccf(k+1).gt.(aly+2.0) ) then
            jfitf(k) = -1
            jfitf(k+1) = -1
         endif
         jfitf(k+2) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PR_FIFILL -- Load profile and star (x,y,height) for PR_FITLOR
C  This takes the profile parameters and and star (x,y,height) parameters
C  and loads them into CC and JFIT for use in the Lorentz 2-D profile
C  fitting subroutines.
C
C   A.J.Penny           RAL                  1990-05-09

      subroutine pr_fifill ( data, nx, ny, parfix, tfixangle, tfixp,
     +                       prof, x, y, aht, xds, yds, xyf, nstar,
     +                       cc, jfit, rinval )

      implicit none

      integer	nx			!i: X size of DATA
      integer	ny			!i: Y size of DATA
      real	data(nx,ny)		!i: data
      logical	parfix			!i: All profile pars fixed (true) or
					!   var
      logical	tfixangle		!i: Profile angle fixed (true) or var
      logical   tfixp(5)        	!i: Profile pars fixed (true) or var
      real	prof(9)			!i: Profile parameters
      real	x(8)			!i: X coords of stars
      real	y(8)			!i: Y coords of stars
      real	aht(8)			!
      real	xds(8)			!i: X coords if fixing star posns
      real	yds(8)			!i: X coords if fixing star posns
      integer   xyf(8)			!i: Flag for fixing star posns
      integer	nstar			!i/o: No of stars (1 to 8); if
					!   outside that a single central
					!   star is assumed and NSTAR is
					!   returned as 1
      real	cc(36)			!o: Fit starting parameters:-
					!   background (1,2,3)
					!   profile (4,5,6,7,8,9,10,11,12)
					!   star posns, heights (13,...,36)
      integer	jfit(36)		!o: -1= par fixed;0=not used;1=var
      real	rinval			!i: Invalid flag value of data
C--
      real amin, amax, top, base
      integer j, k, kk, kd, kx, ky
Cbegin


      amin = 1.1e10							!Estimate the background parameters from
      do k = 1, ny							! the minimum of the array
         do j = 1, nx
           if ( data(j,k).ne.rinval ) amin = min(amin,data(j,k))
         enddo
      enddo
      if ( amin.gt.1.0e10 ) amin = 0.0
      cc(1) = amin
      cc(2) = 0.0
      cc(3) = 0.0
      jfit(1) = 1
      jfit(2) = 1
      jfit(3) = 1

      call amovr ( prof, cc(4), 6 )					!Set the profile parameters to be fixed.
      call amovki ( -1, jfit(4), 9 )					! Then see if not fixed.
      if ( .not.parfix ) then						! If variable set at input starting values
         call amovki ( 1, jfit(4), 6 )
         do k = 1, 5
            if ( tfixp(k) ) jfit(k+3) = -1
         enddo
         if ( tfixangle ) jfit(9) = -1
         call amovki ( -1, jfit(10), 3 )
      endif

      if ( nstar.lt.1 .or. nstar.gt.8 ) then				!If NSTAR outside range 1 to 8,
         nstar = 1							! assume a single star at centre
         x(1) = real(nx)/2.0 + 0.5
         y(1) = real(ny)/2.0 + 0.5
      endif

C  Put in star positions and rough heights and flag these to be fitted
C  Calculate heights from height above base of highest point in 3x3 box
C  round the position, if there is no input height.

      do kk = 1, nstar
         kd = kk*3
         cc(10+kd) = x(kk)
         cc(11+kd) = y(kk)
         if ( xyf(kk).eq.1 ) then
            cc(10+kd) = xds(kk)
            cc(11+kd) = yds(kk)
         endif
         if ( aht(kk).ne.0.0 ) then
            cc(12+kd) = aht(kk)
         else
            amax = 0.0
            do j = 1, 3
               do k = 1, 3
                  kx = x(kk) - 2 + k
                  ky = y(kk) - 2 + j
                  if ( kx.ge.1 .and. kx.le.nx .and. ky.ge.1 .and.
     +                 ky.le.ny ) then
                     base = cc(1) + cc(2)*real(kx) + cc(3)*real(ky)
                     top = data(kx,ky) - base
                     if ( top.gt.amax ) amax = top
                  endif
               enddo
            enddo
            cc(12+kd) = amax
         endif
         jfit(10+kd) = 1
         jfit(11+kd) = 1
         if ( xyf(kk).eq.1 ) then
            jfit(10+kd) = -1
            jfit(11+kd) = -1
         endif
         jfit(12+kd) = 1
      enddo

      if ( nstar.lt.8 ) then						!Set other stars to be not done
         call azeror ( cc(13+nstar*3), 36-(13+nstar*3)+1 )
         call azeroi ( jfit(13+nstar*3), 36-(13+nstar*3)+1 )
      endif


      end
