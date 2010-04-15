CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AUTOMAG.F -- This program does the AUTOMAG program
C
C             For a decription of this program, see AUTOMAG.HLP
C
C   A.J.Penny                 STScI                    87-02-2013:31

      subroutine automag (ierradam)

      implicit none
      integer   ierradam
C--
Cbegin

      call starman_start

      call t_automag

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_AUTOMAG.F
C
C  Contains:-
C
C T_AUTOMAG   Perform the AUTOMAG program
C AU_SETUP    Set up details
C AU_GCLA     Get command line information (1st go)
C AU_GCLB     Get command line information (2nd go)
C AU_GETSCOL  Get file standards columns from filter names
C AU_STRCON3  Convert string into 3 reals
C AU_IMGET    Get the image
C AU_GSTAR    Get the star posns
C AU_FLIM(RS) Set flag vector for vector elements above threshold
C AU_MAGS     Measure the stars
C AU_PHOT(RS) Do aperture photometry on an area
C AU_AVER     Average sky values and make mean mag
C AU_CORRECT  Correct the magnitudes
C AU_PRINT    Print to terminal and file
C AU_IMDET    Get image and exposure details
C AU_SECZMAG  Calc secZ of exposure and std mag
C AU_END      End process?


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_AUTOMAG -- Perform the AUTOMAG program
C
C   alan penny                   ral                 1990-03-09


      subroutine t_automag

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer istat
Cbegin


      call au_setup							!Set up details

      call au_gcla							!Get support input
      call au_gclb

      do while ( IMORE )						!Loop over a number of images

         call au_imget							!Get image

         if ( .not.ST_FAILED ) call gtwrks ( 'WORK', NX*NY, IPWK,istat)

         call au_gstar   ( %val(IPWK) )					!Calc posns of stars in image

         call au_mags    						!Calc mags

         call au_correct						!Corrections to mags

         call au_end							!End?

         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_SETUP -- Set up details
C
C alan penny                     ral            1990-03-09

      subroutine au_setup ()

      implicit none
      include 'automag.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      BADIM = .false.
      IMORE = .true.
      NTOT = 0
      NLOOP = 0

      TBY = 1								!Standards dummy size
      TBVX = 1
      NX = 1								!Image dummy size
      NY = 1
      call gtwrkr ( 'DTEMPA', 1, IPSTD, istat )				!Dummy work space
      call gtwrks ( 'DTEMPB', 1, IPWK, istat )				!Dummy work space
      call gtwrkr ( 'DTEMPC', 1, IPIM, istat )				!Dummy work space


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_GCLA -- Get command line information (1st go)
C
C alan penny                     ral            1990-03-09

      subroutine au_gcla ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer k, ka
      real rv
      character cvb*7, cvc*1, cva*40, text*79

      real dxa(4), dya(4)
      data dxa / 1.0,  0.0, -1.0,   0.0 /
      data dya /  0.0, 1.0,   0.0, -1.0 /
Cbegin


      if ( ST_FAILED ) return						!Failure flag

      call printo ( ' ' )
      call printo ( '  Select which options to do:-')
      call get1b ( 'DOMULT', MULTIPLE, .false. )			!Many input files?
      call get1b ( 'DOCENTRE', CENTRE, .true. )				!Gauss centre on stars?
      call get1b ( 'DOFILE', DOFILE, .false. )				!Output to file?
      call get1b ( 'DOSECZ', DOSECZ, .false. )				!Get exposure RA, Dec?
      DOFILT = .false.							!Correct for extinction?
      if ( DOSECZ ) call get1b ( 'DOFILT', DOFILT, .false. )
      DOSTDS = .false.							!Use standard mags?
      if ( DOFILT ) call get1b ( 'DOSTDS', DOSTDS, .false. )
      if ( ST_FAILED ) return
      call printo ( ' ' )

      call printo ( ' ' )
      call printo ( '  Select aperture setups:-' )
      call get1r ( 'STARDIA', STARDIA, 40.0, 1.0, 1.0e8 )		!Size of star aperture
      call get1i ( 'NSKY', NAPER, 4, 1, 4 )				!No of sky apertures to use
      if ( ST_FAILED ) return
      do k = 1, NAPER							!Positions of apertures
         write ( cvc, '(i1)' ) k
         cvb = 'OFFSET'//cvc
         DXSKY(k) = dxa(k)*STARDIA
         DYSKY(k) = dya(k)*STARDIA
         call get2r ( cvb, DXSKY(k), DYSKY(k), .true., -1.0e8, 1.0e8 )
         if ( ST_FAILED ) return
      enddo
      rv = STARDIA/2.0
      call get1r ( 'SKYDIA', SKYDIA, rv, 1.0, 1.0e8 )			!Size of sky apertures

      call get1r ( 'THRESH', THRESH, 500.0, 0.0, 1.0e8 )		!Search threshold

      call get1r ( 'AVLIM', AVLIM, 0.1, 0.0, 1.0e8 )			!Averaging rejection threshold

      ka = STARDIA/2.0
      call get1i ( 'BOX', IBOX, ka, 1, 10000 )				!Max star size allowed
      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ( '  Input other set-ups:-' )
      call get1r ( 'GAIN',   ZGAIN, 1.0, 1.0e-8, 1.0e8 )		!Get poisson values of
      call get1r ( 'NOISE', ZNOISE, 0.0,    0.0, 1.0e8 )		! input image

      call get1c ( 'EXPNAME', EXPNAME, 'EXPOSED', .true. )		!Image exposure time
      if ( ST_FAILED ) return
      call lowcase ( EXPNAME, cva )
      if ( cva.eq.'none' ) then
         NOEXPNAME = .true.
      else
         NOEXPNAME = .false.
         LOCEXP(1) = 1
         LOCEXP(2) = 79
         call get2i ( 'EXPLOC', LOCEXP(1), LOCEXP(2), .true., 1, 256 )	!Place in descriptor of time
         call get1r ( 'EXPOFF', EXPOFF, 0.0, -1.0e8, 1.0e8 )		!Exposure time offset
      endif
      if ( ST_FAILED ) return

      call get1c ( 'OBJNAME', OBJNAME, 'OBJECT', .true. )		!Image name
      if ( ST_FAILED ) return
      call lowcase ( OBJNAME, cva )
      if ( cva.eq.'none' ) then
         NOOBJNAME = .true.
      else
         NOOBJNAME = .false.
         LOCOBJ(1) = 1
         LOCOBJ(2) = 79
         call get2i ( 'OBJLOC', LOCOBJ(1), LOCOBJ(2), .true., 1, 256 )	!Place in descriptor of name
      endif
      if ( ST_FAILED ) return

      call printo ( ' ' )

      if ( DOFILE ) then
         call get1c ( 'OUT', text, ' ', .true. )			!Open results output file
         if ( ST_FAILED ) return
         open ( unit=1, file=text, status='new' )
         call get1c ( 'HEADER', text, ' ', .true. )			!With comment at start
         if ( ST_FAILED ) return
         write ( 1, '('' '',a79)' ) text
         write ( 1, '(''  '')' )
         call printo ( ' ' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_GCLB -- Get command line information (2nd go)
C
C alan penny                     ral            1990-03-09

      subroutine au_gclb ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
C--
      logical more, amore
      real r1, r2, r3
      integer j, k, iy, im, id, ilen, istat, iv
      double precision dmjd
      character cva*79, cvb*8, cvc*1, text*79

      double precision sla_epj
      external sla_epj

      real extvala(9)
      data extvala / 0.68, 0.28, 0.15, 0.10, 0.07, 0.0,0.0,0.0,0.0/

      integer nthelp
      parameter ( nthelp=5 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,nthelp) /
     + ' Option    Function' ,
     + ' ------    --------' ,
     + ' Degrees   Data in degrees' ,
     + ' Radians   Data in radians' ,
     + ' String    Data in a character string' /
Cbegin


      if ( ST_FAILED ) return						!Failure flag

      if ( DOSECZ ) then						!Those things to do with Sec Z

         call get1c ( 'LATITUDE', cva, '+00 00 00', .true. )		!Latitude
         if ( ST_FAILED ) return
         iv = 1
         call sla_dafin ( cva, iv, DLAT, istat )
         if ( istat.ne.0 ) then
            call printo ( 'ERROR: Latitude in wrong format' )
            ST_FAILED = .true.
            return
         endif

         ISPOSN = .true.

         call get1c ( 'RANAME', RANAME, 'RA', .true. )			!Image RA name
         if ( ST_FAILED ) return
         text = RANAME							!See if 'none'
         call lbgone ( text )
         call charln ( text, iv )
         do k = 1, iv
            j = ichar(text(k:k))
            if ( j.ge.65 .and.  j.le.90 ) text(k:k) = char(j+32)
         enddo
         if ( text(1:iv).eq.'none' ) ISPOSN = .false.

         call get1c ( 'DECNAME', DECNAME, 'DEC', .true. )		!Image Dec name
         if ( ST_FAILED ) return
         text = DECNAME							!See if 'none'
         call lbgone ( text )
         call charln ( text, iv )
         do k = 1, iv
            j = ichar(text(k:k))
            if ( j.ge.65 .and.  j.le.90 ) text(k:k) = char(j+32)
         enddo
         if ( text(1:iv).eq.'none' ) ISPOSN = .false.

         if ( ISPOSN ) call get_job ( 'POSNTYPE', 			!Posn input type
     +      'degrees:radians:string', POSNTYPE,1, thelp, nthelp )
         if ( ST_FAILED ) return


         call get1c ( 'SIDTNAME', SIDTNAME, 'STSTART', .true. )		!Image Sid T name
         if ( ST_FAILED ) return
         LOCSIDT(1) = 1
         LOCSIDT(2) = 79
         call get2i ( 'SIDTLOC', LOCSIDT(1), LOCSIDT(2), .true., 1,256)
         if ( ST_FAILED ) return

         call printo ( ' ' )
      endif


      if ( DOFILT ) then						!Extinction things

         call get1c ( 'FILTNAME', FILTNAME, 'FILTER', .true. )		!Get image filter name location
         if ( ST_FAILED ) return
         LOCFILT(1) = 1
         LOCFILT(2) = 79
         call get2i ( 'FILTLOC', LOCFILT(1), LOCFILT(2), .true., 1,256)
         if ( ST_FAILED ) return

         k = 0								!get filter names in image
         more = .true.							! and in stds file
         do while ( k.lt.9 .and. more )					! and extinctions
            k = k + 1
            write ( cvc, '(i1)' ) k
            cvb = 'FNAME'//cvc
            call get1c ( cvb, FNAME(k), ' ', .true. )
            if ( ST_FAILED ) return
            if ( FNAME(k).eq.' ' ) then
               more = .false.
               TOTFILT = k - 1
            else
               call lbgone ( FNAME(k) )
               if ( DOSTDS ) then
                  write ( cvc, '(i1)' ) k
                  cvb = 'FSNAME'//cvc
                  amore = .true.
                  do while ( amore )
                     call get1c ( cvb, FSNAME(k), FNAME(k), .true. )	!Name in stds file of filter?
                     if ( ST_FAILED ) return
                     call lbgone ( FSNAME(k) )				! Default is name in image
                     if ( FSNAME(k).eq.' ' ) then			!Check if null
                       call printo('ERROR: That name null - Try again')
                     else
                        amore = .false.
                     endif
                  enddo
               endif
               cvb = 'EXTINC'//cvc
               call get1r ( cvb, EXTVAL(k), extvala(k), 0.0, 1.0e8 )
               if ( ST_FAILED ) return
            endif
         enddo

         call printo ( ' ' )
      endif

      if ( DOSTDS ) then

         call optabr ( 'STDS', IPSTD, TBVX, TBY, .true., istat )	!Standards file
         if ( ST_FAILED ) return
         if ( istat.ne.0 .or. TBVX.lt.9 ) then
            call printo ( 'ERROR: File not a proper standards file' )
            ST_FAILED = .true.
            return
         endif
         if ( DOSECZ .and. ISPOSN ) call get1b ( 'STDMAST', STDMAST, 	!Std or image master?
     +                                           .true. )

         call get1c ( 'DATE', cva, '2000/01/01', .true. )		!Date of observation
         if ( ST_FAILED ) return

         call charln ( cva, ilen )					!Convert to yr, mo, dy
         if ( ilen.lt.10 ) then
            call printo ( 'WARNING: Image date incorrect format - '//
     +                    'Date taken as 2000/01/01' )
            DATE = '2000 01 01'
         else
            DATE = cva
         endif
         call au_strcon3 ( DATE, r1, r2, r3, istat )
         iy = r1
         im = r2
         id = r3
         if ( istat.ne.0 ) then
            call printo ( 'ERROR: Date in wrong format' )
            ST_FAILED = .true.
            return
         else
            write ( text, '(1x,3x,''Date is: '',i2.2,''/'',i2.2,''/'',
     +                      i2.2)' ) iy, im, id
            call printo ( text )
         endif

         call sla_caldj ( iy, im, id, dmjd, istat )			!Convert to Julian epoch
         if ( istat.ne.0 ) then
            call printo ( 'ERROR: Date is impossible' )
            ST_FAILED = .true.
            return
         endif
         DDATE = sla_epj ( dmjd )

         call au_getscol						!Get filter names from file

         call printo ( ' ' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_GETSCOL -- Get file standards columns from filter names
C
C alan penny                     ral            1990-03-09

      subroutine au_getscol ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
C--
      character tname*80, tiname*80
      integer j, k, jlen, klen, ierr
      logical more
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TOTFILT
         tiname = FSNAME(k)
         call charln ( tiname, jlen )
         j = 3
         more = .true.
         do while ( more .and. j.lt.(TBVX-5) )
            j = j + 1
            call gthead ( 'STDS', j, tname, ierr )
            if ( ierr.eq.0 ) then
               call lbgone ( tname )
               call charln ( tname, klen )
               if ( jlen.gt.0 .and. klen.gt.0 ) then
                  if ( tname(1:klen).eq.tiname(1:jlen) ) then
                     NCOL(k) = j					!This is column number in file
                     more = .false.					! for that filter
                  endif
               endif
            endif
         enddo
         if ( more ) then
            call pargc ( tiname(1:jlen) )
            call printd ( 'ERROR: Filter name - %c - not in '//
     +                               'Standards file' )
            ST_FAILED = .true.
            return
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_STRCON3 -- Convert string into 3 reals
C
C alan penny                     ral            1990-03-09

      subroutine au_strcon3 ( str, r1, r2, r3, istat )

      implicit none
      include 'STARMAN_INC'

      character*(*)  str	!i: Character string
      real           r1		!o: 1st real
      real           r2		!o: 2nd real
      real           r3		!o: 3rd real
      integer        istat	!o: Error flag (0=ok, not 0=fail)
C--
      logical more, is
      integer j, ja, k, kk(6), ilen, istata, istatb, istatc
      character*1 wht(13)
      data wht / '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.',
     +           '+', '-' /
Cbegin


      if ( ST_FAILED ) return

      call charln ( str, ilen )						!Get start and end locations
      more = .true.							! of the three numbers
      k = 1								! in the character string
      ja = 1
      do while ( more )
         is = .false.
         do j = 1, 13
            if ( str(k:k).eq.wht(j) ) is = .true.
         enddo
         if ( (ja.eq.1.or.ja.eq.3.or.ja.eq.5) .and. is ) then		!Start location
             kk(ja) = k
             ja = ja + 1
         elseif ( (ja.eq.2.or.ja.eq.4.or.ja.eq.6) .and. .not.is ) then	!End location
             kk(ja) = k - 1
             ja = ja + 1
         endif
         k = k + 1
         if ( k.gt.ilen .or. ja.eq.7 ) more = .false.
      enddo
      if ( ja.eq.6 ) then						!Last number may end at end of string
          kk(6) = ilen
          ja = 7
      endif

      r1 = 0.0								!Failure default
      r2 = 0.0
      r3 = 0.0
      if ( ja.ne.7 ) then						!Not found 3 numbers
         istat = 1
      else
         call chartor ( str(kk(1):kk(2)), r1, istata )			!Turn strings to numbers
         call chartor ( str(kk(3):kk(4)), r2, istatb )
         call chartor ( str(kk(5):kk(6)), r3, istatc )
         istat = max(istat,istata,istatb,istatc)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_IMGET -- Get the image
C
C alan penny                     ral            1990-03-09

      subroutine au_imget ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ka, iv, istat
      real rv
      character*79 cva
Cbegin


      if ( ST_FAILED ) return

      NLOOP = NLOOP + 1
      if ( NLOOP.ne.1 ) call printo ( ' ' )

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .true., istat )		!Get image
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         BADIM = .true.
         if ( istat.eq.2 ) IMORE = .false.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,istat)	!Get image details

      rv = 1.0
      if ( .not.NOEXPNAME ) then
         call gtdesc ( 'IN', EXPNAME, cva, ' ', iv, istat )		!Exposure time
         if ( istat.ne.0 ) then
            call printo ('WARNING: Exposure time not found: Set = 1.0')
         else
            cva = cva(LOCEXP(1):LOCEXP(2))
            ka = index(cva,'/')
            if ( ka.ne.0 ) cva = cva(1:ka-1)
            call chartor ( cva, rv, istat )
            if ( istat.ne.0 ) rv = 1.0
         endif
      endif
      if ( rv.eq.0.0 ) then
         call printo ( 'WARNING: Exposure time = 0.0; set to 1.0' )
         rv = 1.0
      endif
      call get1r ( 'TIME', EXPT, rv, 0.0001, 1.0e8 )			!Chance to correct the time
      if ( ST_FAILED ) return

      OBJECT = ' '
      if ( .not.NOOBJNAME ) then
         call gtdesc ( 'IN', OBJNAME, cva, ' ', iv, istat )		!Name of 'star' in image
         OBJECT = cva(LOCOBJ(1):LOCOBJ(2))
         call lbgone ( OBJECT )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_GSTAR -- Get the star posns
C
C alan penny                     ral            1990-03-09

      subroutine au_gstar ( wc )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2 wc(NX,NY)		!o: Work space
C--
      integer j, k, kxc(2), kyc(2), np, jj, kk, iter, nin, kl, jb, kb,
     +        jba, jbb, kba, kbb, ierr, jbc, jbd, kbc, kbd
      real    std, rx, ry, am, ah, ab, dx, dy, xa, ya, arx, ary, arms,
     +        athresh, px, py, pnum, rv
      logical more, amore, bmore

      integer kf(NMAX)
Cbegin


      if ( ST_FAILED .or. BADIM ) return				!Failure flag

      kxc(1) = 1							!Get rough sky for whole image
      kxc(2) = NX
      kyc(1) = 1
      kyc(2) = NY
      if ( IMTYPE.eq.'SHORT' ) then
         call ranges ( %val(IPIM), NX, NY, kxc, kyc, INVAL,rv,std,ierr)
      else
         call ranger ( %val(IPIM), NX, NY, kxc, kyc,RINVAL,rv,std,ierr)
      endif
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Image data weird' )
         BADIM = .true.
         return
      endif
      athresh = rv + THRESH/BS						!Set limit above sky

      call azeros ( wc, NX*NY )
      if ( IMTYPE.eq.'SHORT' ) then					!Mark pixels above threshold
         call au_flims ( %val(IPIM), INVAL, athresh, wc, NX*NY )
      else
         call au_flimr ( %val(IPIM), RINVAL, athresh, wc, NX*NY )
      endif

      np = 0								!Look for pixel above threshold (1)
      more = .true.							! but not already done (2)
      do while ( more )

         kk = 0								!Find 1st such one
         kl = 1
         do while ( kl.eq.1 .and. kk.lt.NX*NY )
            kk = kk + 1
            kb = 1 + (kk-1)/NX
            jb = kk - (kb-1)*NX
            if ( wc(jb,kb).eq.1 ) kl = 0
         enddo
         if ( kl.eq.1 ) more = .false.

         if ( more ) then

            wc(jb,kb) = 3						!Flag with a 3 all points in contact with that one
            kba = kb							! within +/- half box size
            kbb = kb
            jba = jb
            jbb = jb
            kbc = kb
            kbd = min(NY,(kb+IBOX))
            jbc = max(1,(jb-(IBOX/2)))
            jbd = min(NX,(jb+(IBOX/2)))
            amore = .true.
            do while ( amore )
               amore = .false.
               k = kbc - 1
               bmore = .true.
               do while ( bmore .and. k.lt.kbd )
                  k = k + 1
                  bmore = .false.
                  do j = jbc, jbd
                     if ( wc(j,k).eq.3 ) bmore = .true.
                     if ( wc(j,k).eq.1 ) then
                        do kk = max(1,k-1), min(NY,k+1)
                           do jj = max(1,j-1), min(NX,j+1)
                              if ( (jj.ne.j.or.kk.ne.k) .and.
     +                              wc(jj,kk).eq.3 ) then
                                 wc(j,k) = 3
                                 kbb = max(k,kbb)
                                 jba = min(j,jba)
                                 jbb = max(j,jbb)
                                 amore = .true.
                                 bmore = .true.
                              endif
                           enddo
                        enddo
                     endif
                  enddo
               enddo

            enddo

            px = 0.0							!Get centroid of pixels
            py = 0.0							! and note pixels as done
            pnum = 0
            do k = kba, kbb
               do j = jba, jbb
                  if ( wc(j,k).eq.3 ) then
                     wc(j,k) = 2
                     pnum = pnum + 1.0
                     px = px + real(j)
                     py = py + real(k)
                  endif
               enddo
            enddo
            px = px/pnum
            py = py/pnum

            np = np + 1							!Store location
            if ( np.gt.NMAX ) then
               np = NMAX
               call printo ( 'WARNING: Max number of stars found'//
     +                          '- BEWARE VALIDITY OF OUTPUT' )
               more = .false.
            else
               XP(np) = px
               YP(np) = py
            endif

         endif

      enddo

      call amovki ( 1, kf, np )						!Flag stars as ok

      if ( CENTRE ) then
         do j = 1, np							!Centre on the images.
            if ( IMTYPE.eq.'SHORT' ) then
               call gauss2sa ( %val(IPIM), NX, NY, XP(j),YP(j), 20, 20, ! If Gaussian height less
     +                         0, rx, ry, INVAL, 20, am, ah, ab, dx, 	! than 100 or a radius
     +                         dy, xa, ya, arx, ary, arms, iter, nin )	! less than 0.6, reject.
            else
               call gauss2ra ( %val(IPIM), NX, NY, XP(j),YP(j), 20, 20,
     +                         0, rx, ry, RINVAL, 20, am, ah, ab, dx,
     +                         dy, xa, ya, arx, ary, arms, iter, nin )
            endif
            XP(j) = xa
            YP(j) = ya
            if (ah.lt.THRESH/BS .or. arx.lt.0.6 .or. ary.lt.0.6)kf(j)=0
         enddo
      endif

      do j = 2, np							!Reject all 'stars' too close to others
         do k = 1, j-1
            if ( kf(k).eq.1 ) then
               if ( abs(XP(k)-XP(j)).lt.8.0 .and.
     +              abs(YP(k)-YP(j)).lt.8.0 ) kf(j) = 0
            endif
         enddo
      enddo

      NTOT = 0								!Only use 'good' stars
      do j = 1, np
         if ( kf(j).eq.1 ) then
            NTOT = NTOT + 1
            XP(NTOT) = XP(j)
            YP(NTOT) = YP(j)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_FLIMR -- Set flag vector for real vector elements above threshold
C
C  alan penny                  ral               1990-03-10

      subroutine au_flimr ( im, rinval, athresh, wc, n )

      implicit none
      include 'STARMAN_INC'

      integer      n			!i: No of elements
      real         im(n)		!i: Input vector
      real         rinval		!i: Invalid flag
      real         athresh		!i: Threshold
      integer*2    wc(n)		!i/o: Flag vector
C--
      integer k
      real    val
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         val = im(k)
         if ( val.ne.rinval .and. val.gt.athresh ) wc(k) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_FLIMS -- Set flag vector for int*2 vector elements above threshold
C
C  alan penny                  ral               1990-03-10

      subroutine au_flims ( im, inval, athresh, wc, n )

      implicit none
      include 'STARMAN_INC'

      integer      n			!i: No of elements
      integer*2    im(n)		!i: Input vector
      integer      inval		!i: Invalid flag
      real         athresh		!i: Threshold
      integer*2    wc(n)		!i/o: Flag vector
C--
      integer k, ival
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         ival = im(k)
         if ( ival.ne.inval .and. ival.gt.athresh ) wc(k) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_MAGS -- Measure the stars
C
C  alan penny                  ral               1990-03-10

      subroutine au_mags ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

C--
      integer  j, ja, kl, nsky(4), iserr(4), ierr, ninval, nstar,
     +         jerr, isneg
      real     amag(4), sky(4), am, astop, snum, sum, ama,
     +         x, y, rms, flux, star, slevel, atop, top, alevel(4)
      character text*79, texta*24
      real     trunc
      external trunc
Cbegin


      if ( ST_FAILED .or. BADIM ) return				!Failure flag

      call au_print ( DOFILE, ' ' )					!Info output
      text = 'Image name is :-  '//OBJECT
      call au_print ( DOFILE, text )
      text = 'Image Title is :-  '//IMTITLE
      call au_print ( DOFILE, text )

      if ( NTOT.eq.0 ) then						!Star found?
         call au_print ( DOFILE, 'No star found' )
         BADIM = .true.
         return
      endif

      write ( text,'('' Star'',5x,''X'',6x,''Y'',''        Top '',
     +       ''    Mag'',''   Rms'',''    Sk1'',''   Sk2'',
     +       ''   Sk3'',''   Sk4'')' )
      call au_print ( DOFILE, text )					!Stars found info header

      do kl = 1, NTOT							!For each star
         if ( IMTYPE.eq.'SHORT' ) then					!Do photometry for star
            call au_phots ( %val(IPIM), XP(kl), YP(kl), STARDIA, star,
     +                      slevel, astop, nstar, ninval, jerr, ierr )
         else
            call au_photr ( %val(IPIM), XP(kl), YP(kl), STARDIA, star,
     +                      slevel, astop, nstar, ninval, jerr, ierr )
         endif

         if ( ierr.ne.0 .or. jerr.ne.0 ) then

            atop = trunc(astop,5)
            write ( text, '('' '',i3,2x,2f7.1,2x,f8.1,
     +                      ''   0.000   - too near edge'')' )
     +                      kl, XP(kl), YP(kl), atop
            call au_print ( DOFILE, text )
            PMAG(kl) = 0.0

         else

            do j = 1, NAPER						!Do photometry for skies
               x = XP(kl) + DXSKY(j)
               y = YP(kl) + DYSKY(j)
               if ( IMTYPE.eq.'SHORT' ) then
                  call au_phots ( %val(IPIM), x, y, SKYDIA, sky(j),
     +            alevel(j), top, nsky(j), ninval, jerr, iserr(j) )
               else
                  call au_photr ( %val(IPIM), x, y, SKYDIA, sky(j),
     +            alevel(j), top, nsky(j), ninval, jerr, iserr(j) )
               endif
            enddo

            call au_aver ( star, nstar, sky, nsky, flux, am, amag, 	!Make mean mag
     +                     rms, isneg )

            if ( isneg.eq.1 ) then

               atop = trunc(astop,5)
               write ( text, '('' '',i3,2x,2f7.1,2x,f8.1,
     +                         ''   0.000   - -ve flux'')' )
     +                      kl, XP(kl), YP(kl), atop
               call au_print ( DOFILE, text )
               PMAG(kl) = 0.0

            else

               am = am + 2.5*alog10(max(1.0e-10,(EXPT+EXPOFF)))		!Apply exposure time corrn
               PMAG(kl) = am						!Store

               sum = 0.0						!Get max level above sky
               snum = 0.0
               do j = 1, NAPER
                  if ( nsky(j).ne.0 ) then
                     sum = sum + alevel(j)
                     snum = snum + 1.0
                  endif
               enddo
               if ( snum.gt.0.5 ) astop = astop - (sum/snum)
               astop = BS*astop

               atop = trunc(astop,5)					!Output result
               texta = '  '
               do j = 1, 4
                  if ( nsky(j).ne.0 ) then
                     ama = max(-0.999,min(0.999,amag(j)))
                     ja = 1 + (j-1)*6
                     write ( texta(ja:ja+5),'(f6.3)') ama
                     if ( ama.ge.0.0 ) then
                        write ( texta(ja:ja+1), '(''  '')')
                     else
                        write ( texta(ja:ja+1), '('' -'')')
                     endif
                  endif
               enddo

               write ( text, '('' '',i3,2x,2f7.1,2x,f8.1,1x,f7.3,
     +         f6.3,1x,a24)' ) kl, XP(kl), YP(kl), atop, am, rms,
     +                         texta
               call au_print ( DOFILE, text )

            endif

         endif

      enddo

      call au_print ( DOFILE, ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_PHOTR -- Do aperture photometry on an area in a real image
C
C  alan penny                      ral              1990-03-09

      subroutine au_photr ( rim, x, y, dia, flux, level, top, nsp,
     +                      ninval, jerr, ierr )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      rim(NX,NY)	!i: Image
      real      x		!i: Aperture X position
      real      y		!i: Aperture Y position
      real      dia		!i: Aperture diameter
      real      flux		!o: Flux
      real      level		!o: Level
      real      top		!o: Top value
      integer   nsp		!o: No of good pixels in aperture
      integer   ninval		!o: No of 'INVALID' pixels in aperture
      integer   jerr		!o: Diameter all in image (0=yes; 1=no)
      integer   ierr		!o: Error flag (0=ok; 1=not in image; 2=all invalid pixels
C--
      double precision sum
      real saprad, dx, dy, rv
      integer kxs, kxe, kys, kye, j, k
Cbegin


      if ( ST_FAILED ) return

      flux = 0.0							!Defaults
      level = 0.0
      top = 0.0
      nsp = 0
      ninval = 0
      ierr = 0
      jerr = 0

      kxs = x - dia/2.0							!Aperture in image?
      kxe = kxs + dia
      kys = y - dia/2.0
      kye = kys + dia
      if ( kxs.gt.NX .or. kxe.lt.1 .or. kys.gt.NY .or. kye.lt.1 ) then
         ierr = 1
         return
      endif

      jerr = 0
      if(kxs.lt.1 .or. kxe.gt.NX .or. kys.lt.1 .or. kye.gt.NY ) jerr=1

      kxs = min(NX,max(1,kxs))
      kxe = min(NX,max(1,kxe))
      kys = min(NY,max(1,kys))
      kye = min(NY,max(1,kye))

      saprad = (dia/2.0)**2.0						!Flux in aperture and
      sum = 0.0d0							! bad pixels and
      nsp = 0								! max value
      ninval = 0
      top = 0.0
      do k = kys, kye
         do j = kxs, kxe
            dx = abs(x-real(j))
            dy = abs(y-real(k))
            if ( (dx*dx+dy*dy).le.saprad ) then
               if ( rim(j,k).eq.RINVAL ) then
                  ninval = ninval + 1
               else
                  nsp = nsp + 1
                  rv = BS*rim(j,k) + BZ
                  top = max(top,rv)
                  sum = sum + dble(rv)
               endif
            endif
         enddo
      enddo

      flux = sngl(sum)							!Mean level if any good pixels
      if ( nsp.ne.0 ) then
         level = flux/real(nsp)
      else
         ierr = 2
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_PHOTS -- Do aperture photometry on an area in an int*2 image
C
C  alan penny                      ral              1990-03-09

      subroutine au_phots ( im, x, y, dia, flux, level, top, nsp,
     +                      ninval, jerr, ierr )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2 im(NX,NY)	!i: Image
      real      x		!i: Aperture X position
      real      y		!i: Aperture Y position
      real      dia		!i: Aperture diameter
      real      flux		!o: Flux
      real      level		!o: Level
      real      top		!o: Top value
      integer   nsp		!o: No of good pixels in aperture
      integer   ninval		!o: No of 'INVALID' pixels in aperture
      integer   jerr		!o: Diameter all in image (0=yes; 1=no)
      integer   ierr		!o: Error flag
C--
      double precision sum
      real saprad, dx, dy, rv
      integer kxs, kxe, kys, kye, j, k
Cbegin


      if ( ST_FAILED ) return

      flux = 0.0							!Defaults
      level = 0.0
      top = 0.0
      nsp = 1
      ninval = 0
      ierr = 0
      jerr = 0

      kxs = x - dia/2.0							!Aperture in image?
      kxe = kxs + dia
      kys = y - dia/2.0
      kye = kys + dia
      if ( kxs.lt.1 .or. kxe.gt.NX .or. kys.lt.1 .or. kye.gt.NY ) then
         ierr = 1
         return
      endif

      jerr = 0
      if(kxs.lt.1 .or. kxe.gt.NX .or. kys.lt.1 .or. kye.gt.NY ) jerr=1

      kxs = min(NX,max(1,kxs))
      kxe = min(NX,max(1,kxe))
      kys = min(NY,max(1,kys))
      kye = min(NY,max(1,kye))

      saprad = (dia/2.0)**2.0						!Flux in aperture and
      sum = 0.0d0							! bad pixels and
      nsp = 0								! max value
      ninval = 0
      top = 0.0
      do k = kys, kye
         do j = kxs, kxe
            dx = abs(x-real(j))
            dy = abs(y-real(k))
            if ( (dx*dx+dy*dy).le.saprad ) then
               if ( im(j,k).eq.INVAL ) then
                  ninval = ninval + 1
               else
                  nsp = nsp + 1
                  rv = BS*real(im(j,k)) + BZ
                  top = max(top,rv)
                  sum = sum + dble(rv)
               endif
            endif
         enddo
      enddo

      flux = sngl(sum)							!Mean level if any good pixels
      if ( nsp.ne.0 ) then
         level = flux/real(nsp)
      else
         ierr = 1
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_AVER -- Average sky values and make mean mag
C
C alan penny             ral                 1990-03-09

      subroutine au_aver ( star, nstar, sky, nsky, flux, am, amag,
     +                     rms, isneg )

      implicit none
      include 'automag.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real    star		!i: Star flux
      integer nstar		!i: No of star pixels
      real    sky(4)		!i: Sky fluxes
      integer nsky(4)		!i: No of sky pixels
      real    flux		!o: Mean flux
      real    am		!o: Mean magnitude
      real    amag(4)		!o: Individual magn corrns (mag-mean)
      real    rms		!o: Star flux error
      integer isneg		!o: Star flux is -ve =1; +ve = 0
C--
      integer j, k, ka, kb, nsum, ngood, nnsky(4), knum(4)
      real    aflux(4), slevel(4), afl, afla, dmin, dmina, ama,
     +        var, varsk, err, ssky(4), diff, totsf, totsn
Cbegin


      if ( ST_FAILED ) return

      flux = 0.0							!Default values
      am = 0.0
      call azeror ( amag, 4 )
      rms = 0.0
      isneg = 0
      if ( nstar.le.0 ) return

      ngood = 0								!Bunch skies up into only good ones
      do k = 1, NAPER
         if ( nsky(k).ne.0 ) then
            ngood = ngood + 1
            ssky(ngood) = sky(k)
            nnsky(ngood) = nsky(k)
         endif
      enddo
      if ( ngood.eq.0 ) then
         flux = star
         if ( flux.gt.1.0e-8 ) am = 30.0 - 2.5*alog10(flux)
         return
      endif

      do k = 1, ngood
         slevel(k) = ssky(k)/real(nnsky(k))				!Get level and star flux
         aflux(k) = star - slevel(k)*nstar				! over sky for each star
      enddo

      if ( ngood.eq.0 ) then						!Do for the cases of 1, 2, 3, or 4
         return								! sky apertures with any good pixels
      elseif ( ngood.eq.1 ) then
         flux = aflux(1)						!if = 1, take it
      elseif ( ngood.eq.2 ) then
         flux = (aflux(1)+aflux(2))/2.0					!if = 2, take mean
      else
									!If 3 or 4, take close skies

         ka = 1								!Get 2 closest and make mean
         kb = 1
         dmin = 999.0
         do j = 1, ngood-1
            do k = j+1, ngood
               dmina = abs(aflux(j)-aflux(k))
               if ( dmina.lt.dmin ) then
                   ka = j
                   kb = k
                   dmin = dmina
               endif
            enddo
         enddo
         afla = (aflux(ka)+aflux(kb))/2.0

         afl = 0.0							!Take mean or
         nsum = 0							! all those within AVLIM mags of mean
         call azeroi ( knum, 4 )
         do k = 1, ngood
            diff = abs((aflux(k)-afla)/afla)
            if ( diff.lt.AVLIM ) then
               afl = afl + aflux(k)
               nsum = nsum + 1
               knum(k) = 1
            endif
         enddo
         if ( nsum.ne.0 ) then
            flux = afl/real(nsum)
         else
            knum(ka) = 1
            knum(kb) = 1
            flux = afla
         endif

      endif

      flux = flux*BS							!Apply BSCALE corrn
      if ( flux.gt.1.0e-8 ) then					!Make magnitude
         am = 30.0 - 2.5*alog10(flux)
         isneg = 0
      else
         am = 0.0
         isneg = 1
         return
      endif

      call azeror ( amag, 4 )						!Difference in mag if had used
      do k = 1, NAPER							! the skies individually
         if ( nsky(k).ne.0 ) then
            afl = BS*(star-slevel(k)*nstar)
            ama = 0.0
            if ( afl.gt.1.0e-8 ) ama = 30.0 - 2.5*alog10(afl)
            amag(k) = ama - am
         endif
      enddo

      totsf = 0.0							!Good total sky flux and sky pixels
      totsn = 0.0
      do k = 1, ngood
         if ( (ngood.le.2 .and. k.le.2) .or.
     +        (ngood.gt.2 .and. knum(k).eq.1)     ) then
            totsf = totsf + ssky(k)
            totsn = totsn + nnsky(k)
         endif
      enddo

      varsk = totsf*ZGAIN + totsn*ZNOISE*ZNOISE			!Sky variance
      varsk = varsk/totsn

      var = star*ZGAIN + nstar*ZNOISE*ZNOISE + nstar*varsk	!Star error
      err = sqrt(max(1.0e-8,var))/(star*ZGAIN)
      rms = 2.5*alog10(1.0+err)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_CORRECT -- Correct the magnitudes
C
C  alan penny                  ral               1990-03-10

      subroutine au_correct ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
C--
      integer  k, ii(4), iv
      real     am1, secz, stdmag, diff, time
      character text*79, c1*9, c2*9, c3*9, c4*1
Cbegin


      if ( ST_FAILED .or. BADIM ) return				!Failure flag

      if ( .not.DOSECZ ) return

      call au_imdet							!Get image and header info
      if ( ST_FAILED .or. BADIM ) return

      call au_seczmag ( %val(IPSTD), secz, stdmag )			!Calc secZ, get mags
      secz = max(0.0,min(9.999,secz))

      time = max(0.0,min(99999.999,(EXPT+EXPOFF)))			!Output filter, sec z, exposure time
      if ( DOFILT ) then
         call charln ( FILTER, iv )
         iv = min(10,iv)
         write ( text, '(1x,''Filter= '',a10,'' :Sec Z= '',f5.3,
     +            ''  :Exp time= '',f9.3)' ) FILTER(1:iv), secz, time
         call lbgone ( text(47:) )
         call lbgone ( text(10:) )
      else
         write ( text, '(1x,''Sec Z= '',f5.3,''  :Exp time= '',
     +                   f9.3)' ) secz, time
         call lbgone ( text(27:) )
      endif
      call au_print ( DOFILE, text )

      call sla_dr2tf ( 0, DERA, c4, ii )				!Output Image RA and Dec
      write ( c1, '(3(i2,1x))' ) ii(1), ii(2), ii(3)
      call sla_dr2af ( 0, DEDEC, c4, ii )
      write ( c2, '(3(i2,1x))' ) ii(1), ii(2), ii(3)
      call sla_dr2tf ( 0, DSIDT, c4, ii )
      write ( c3, '(3(i2,1x))' ) ii(1), ii(2), ii(3)
      write ( text, '(1x,''Image:-  '',''  RA= '',a9,''  :Dec= '',
     +                a1,a9,''  :Sid T= '',a9)' ) c1, c4, c2, c3
      call au_print ( DOFILE, text )

      if ( DOSTDS ) then						!Output stds file RA and Dec,
         call sla_dr2tf ( 0, DSRA, c4, ii )				! precessed to date of obsn
         write ( c1, '(3(i2,1x))' ) ii(1), ii(2), ii(3)
         call sla_dr2af ( 0, DSDEC, c4, ii )
         write ( c2, '(3(i2,1x))' ) ii(1), ii(2), ii(3)
         write ( text, '(1x,'' File:-  '',''  RA= '',a9,''  :Dec= '',
     +                   a1,a9)' ) c1, c4, c2
         call au_print ( DOFILE, text )
      endif
      call au_print ( DOFILE, ' ' )

      if ( NTOT.ne.0 .and. DOFILT ) then				!Output extinction and standard mag diff

        if ( DOSTDS ) then
            write ( text, '(1x,''Raw Mag    Sec Z Corrected     Std'',
     +                      ''     Mag-Std'')' )
         else
            write ( text, '(1x,''Raw Mag    Sec Z Corrected'')' )
         endif
         call au_print ( DOFILE, text )

         do k = 1, NTOT
            am1 = PMAG(k) - EXTINC*(secz-1.0)
            diff = am1 - stdmag
            if ( DOSTDS .and. SFOUND ) then
               write ( text, '(1x,2f10.3,6x,f10.3,f8.3)' ) PMAG(k),
     +                                            am1, stdmag, diff
            else
               write ( text, '(1x,2f10.3)' ) PMAG(k), am1
            endif
            call au_print ( DOFILE, text )
         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_PRINT -- Print to terminal and file
C
C  alan penny                ral                1990-03-09

      subroutine au_print ( dofile, ctext )

      implicit none
      include 'STARMAN_INC'

      logical           dofile		!i: Print to file as well?
      character*(*)     ctext		!i: Text to print
C--
      integer i
      character*79 cctext
Cbegin


      if ( ST_FAILED ) return

      call charln ( ctext, i )
      i = min(79,i)
      if ( i.lt.1 ) then
         call printo ( ' ' )
         if ( dofile ) write ( 1, '('' '')' )
      else
         call printo ( ctext(1:i) )
         cctext = ctext(1:i)//' '
         if ( dofile ) write ( 1, '(1x,a79)' ) cctext
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_IMDET -- Get image and exposure details
C
C  alan penny                ral                1990-03-09

      subroutine au_imdet ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      character*79 cva
      real rh, rm, rs, rv
      integer jlen, k, iv, istat
Cbegin


      if ( ST_FAILED .or. BADIM ) return				!Failure flag

      if ( DOFILT ) then

         call gtdesc ( 'IN', FILTNAME, cva, ' ', iv, istat )		!Get image filter and match
         FILTER = cva(LOCFILT(1):LOCFILT(2))				! with set of filter names
         call lbgone ( FILTER )
         call charln ( FILTER, jlen )
         NUMFILT = 0
         if ( jlen.gt.0 ) then
            do k = 1, 9
              if ( FILTER(1:jlen).eq.FNAME(k) ) NUMFILT = k
            enddo
         endif

         EXTINC = 0.0							!Load extinction
         if ( NUMFILT.ne.0 ) EXTINC = EXTVAL(NUMFILT)

      endif

      if ( DOSECZ ) then

         ERA = 0.0							!Image RA and Dec
         DERA = ERA
         EDEC = 0.0
         DEDEC = EDEC
         if ( ISPOSN ) then

            call gtdesc ( 'IN', RANAME, ESTRA, ' ', iv, istat )		!RA
            call lbgone ( ESTRA )					!Convert to radians
            if ( POSNTYPE.eq.1 ) then
               call chartor ( ESTRA, ERA, istat )
               if ( istat.eq.0 ) then
                  ERA = PI*ERA/180.0
               else
                  ERA = 0.0
               endif
            elseif ( POSNTYPE.eq.2 ) then
               call chartor ( ESTRA, ERA, istat )
               if ( istat.ne.0 ) ERA = 0.0
            else
               iv = 1
               call sla_afin ( ESTRA, iv, ERA, istat )
               if ( istat.ne.0 ) then
                  call printo ( 'ERROR: Image RA in wrong format' )
                  BADIM = .true.
                  return
               endif
            endif
            DERA = ERA

            call gtdesc ( 'IN', DECNAME, ESTDEC, ' ', iv, istat )	!Dec
            call lbgone ( ESTDEC )					!Convert to radians
            if ( POSNTYPE.eq.1 ) then
               call chartor ( ESTDEC, EDEC, istat )
               if ( istat.eq.0 ) then
                  EDEC = PI*EDEC/180.0
                else
                  EDEC = 0.0
                endif
            elseif ( POSNTYPE.eq.2 ) then
               call chartor ( ESTDEC, EDEC, istat )
               if ( istat.ne.0 ) EDEC = 0.0
            else
               iv = 1
               call sla_afin ( ESTDEC, iv, EDEC, istat )
               if ( istat.ne.0 ) then
                  call printo ( 'ERROR: Image Dec in wrong format' )
                  BADIM = .true.
                  return
               endif
            endif
            DEDEC =EDEC

         endif

         call gtdesc ( 'IN', SIDTNAME, cva, ' ', iv, istat )		!Image Sid T
         if ( cva.ne.' ' ) then
            ESTSIDT = cva(LOCSIDT(1):LOCSIDT(2))				! with set of filter names
            call lbgone ( ESTSIDT )
            call au_strcon3 ( ESTSIDT, rh, rm, rs, istat )
            rv = rs + rm*60.0 + rh*3600.0
            DSIDT = rv*PI/(12.0*60.0*60.0)
         else
            ESTSIDT = '0.0'
            DSIDT = 0.0
         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_SECZMAG -- Calc secZ of exposure and std mag
C
C alan penny             ral                 1990-03-09

      subroutine au_seczmag ( stds, secz, stdmag )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'

      real	stds(TBVX,TBY)	!i: Standards list
      real      secz		!o: Sec Z
      real      stdmag		!o: Standard mag
C--
      character*20 name
      integer k, len, lena, iv
      double precision dequ
Cbegin


      if ( ST_FAILED ) return

      SFOUND = .false.
      stdmag = 0.0
      if ( DOSTDS ) then						!Get stds file RA and Dec
         call charln ( OBJECT, len )
         if ( len.gt.0 ) then
            k = 0
            do while ( k.lt.TBY .and. .not.SFOUND )
               k = k + 1
               call namegt ( stds, TBVX, TBY, k, name )
               call charln ( name, lena )
               if ( len.eq.lena ) then
                  if ( name(1:len).eq.OBJECT(1:len) ) then
                     SFOUND = .true.
                     DSRA = stds(6,k)
                     DSDEC = stds(7,k)
                     dequ = stds(8,k)
                     call sla_preces ( 'FK5', dequ, DDATE, DSRA, DSDEC)	! and precess them
                     iv = 5 + NCOL(NUMFILT)
                     stdmag = stds(iv,k)
                  endif
               endif
            enddo
         endif
         if ( .not.SFOUND ) call printo ( 'WARNING: Star not found '//
     +                                    'in file' )
      endif

      DRA = DERA							!Choose dominant RA/Dec
      if ( DOSTDS .and. SFOUND .and. (STDMAST .or. .not.ISPOSN) ) 	! - image or stds?
     +   DRA = DSRA
      DDEC = DEDEC
      if ( DOSTDS .and. SFOUND .and. (STDMAST .or. .not.ISPOSN) )
     +   DDEC = DSDEC

      DAH = dabs(DRA-DSIDT)						!Calc Sec Z
      secz = 1.0/(dsin(DLAT)*dsin(DDEC)+dcos(DLAT)*dcos(DDEC)*dcos(DAH))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AU_END -- End process?
C
C  alan penny                ral                1990-03-09

      subroutine au_end ( )

      implicit none
      include 'automag.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
Cbegin


      if ( ST_FAILED ) IMORE = .false.

      if ( .not.MULTIPLE ) IMORE = .false.

      if ( IMORE ) then
         BADIM = .false.
         call canpar ( 'IN' )
         call wrkcan ( 'WORK' )
      else
         call wrkcan ( ' ' )
         if ( DOFILE ) close ( 1 )
      endif


      end
