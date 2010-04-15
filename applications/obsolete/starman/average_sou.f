CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AVERAGE --  Averages together MEASURE star magnitudes
C        For details of this program see AVERAGE.HLP
C
C         a.j.penny                rgo                             84-7-31

      subroutine average ( ierradam )

      implicit none

      integer    ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_average

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  AVERSUBG.FOR
C
C  Contains graphic s/rs for AVERAGE:-
C
C AV_PLERR -- Plot calculated error vs found error
C AV_PLDIFF -- Plot differences between stars in file K and file 1.
C AV_PLHIS -- Plot histogram and get new diff limit with cursor

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PLERR -- Plot calculated error vs found error
C
C   a j penny                 dao           1988-04-25

      subroutine av_plerr ( ax, ay, kfile )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      real ax(12,KNUMF)		!i: Theoretical errors for files
      real ay(12,KNUMF)		!i: Actual errors for files
      integer kfile		!i: File whose run of T vs A to plot
C--
      logical bv(1)
      real xmax, ymax, rv
      integer k, istat
Cbegin


      if ( ST_FAILED ) return

      xmax = 0.0
      ymax = 0.0
      do k = 1, 12
         rv = ay(k,kfile)
         if ( rv.gt.1.0e-8 ) then
            xmax = max(xmax,ax(k,kfile))
            ymax = max(ymax,rv)
         endif
      enddo
      xmax = 1.1*xmax
      ymax = 1.1*ymax

      if ( xmax.le.0.0 .or. ymax.le.0.0 ) then
         call printo ( 'WARNING: No plot is possible of calc vs '//
     +                 'found errors' )
         return
      endif

      call gd_open ( istat )						!Open device
      if ( istat.ne.0 ) return

      call pgbbuf

      call gd_dobox ( 0.0, xmax, 'Calculated errors', 0.0, ymax,	!Plot errors
     +              'Found errors', ' ', 0 )
      call gd_opts ( ax(1,kfile), ay(1,kfile), bv, 12, .false.,
     +               .false., 2 )

      call pgmove ( 0.0, 0.0 )						!Draw lines at unity slope
      rv = min(xmax,ymax)
      call pgdraw ( rv, rv )

      call pgebuf

      call printo ( 'See the HELP AVERAGE METHOD MAKING_MEANS '//
     +              'MEANS_REJECTING on how to use this graph' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PLDIFF -- Plot differences between stars in file K and file 1.
C Then use cursor to refine mean.
C Magnitudes and differences from GOODMG and RDIFF
C
C   a j penny                 dao           1988-04-25

      subroutine av_pldiff ( ax, ay, am, num )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      real    ax(2)		!i: X plot limts
      real    ay(2)		!i: Y plot limits
      real    am		!i/o: estimated mean on input; refined on out
      integer num		!i: no of points
C--
      real x, y
      integer k, pgcurse, istat
      character  text*72
      logical bv(1)
      external pgcurse
Cbegin


      if ( ST_FAILED ) return

      call gd_open ( istat )						!Open device
      if ( istat.ne.0 ) return

      call pgbbuf

      call gd_dobox ( ax(1), ax(2), 'Mag', ay(1), ay(2), 'Diff',' ',0 )	!Plot residuals from mean
      call asubkr ( RDIFF, am, RDIFF, num )
      call gd_opts ( GOODMG, RDIFF, bv, num, .false., .false., 1 )
      call aaddkr ( RDIFF, am, RDIFF, num )

      call pgmove ( ax(1), 0.0 )					!Draw line at zero residual from mean
      call pgdraw ( ax(2), 0.0 )

      call pgebuf

      call pgqinf ( 'CURSOR', text, k )
      if ( text(1:1).eq.'Y' ) then					!Use cursor to  get a better mean
         call printo ( 'Use cursor to refine the mean difference' )
         x = (ax(1)+ax(2))/2.0
         y = 0.0
         k = pgcurse ( x, y, text )
         am = am + y
         write ( text, '(1x, ''New mean = '',f8.4)' ) am
         call printo ( text )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PLHIS -- Plot histogram and get new diff limit with cursor
C  (Histogram values from NDIFL.)
C
C   a j penny                 dao           1988-04-25

      subroutine av_plhis ( ax, ay, dlim, kf )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      real     ax(2)		!i: X range of data to plot
      real     ay(2)		!i: Y range of data to plot
      real     dlim		!i/o: Difference limit from mean for acceptability
      integer  kf		!i: Magnitude range start
C--
      real x, y
      integer k, istat
      character texta*30, text*72
      integer pgcurse
      external pgcurse
Cbegin


      if ( ST_FAILED ) return

      call gd_open ( istat )						!Open device
      if ( istat.ne.0 ) return

      write ( texta, '(1x,''Magnitude range:'',i3,'' to'',i3)' ) kf,	!Plot Box
     +                                                          (kf+1)
      call gd_dobox ( ax(1), ax(2), 'Magnitude difference',
     +                ay(1), ay(2), 'Number of star pairs', texta, 0 )

      call pgbbuf
      call pgmove ( 0.0, 0.0 )						!Plot histogram
      x = 0.0
      do k = 1, KNUMD
         y = NDIFL(kf,k)
         call pgdraw ( x, y )
         x = x + 0.5/real(KNUMD)
         call pgdraw ( x, y )
      enddo
      call pgdraw ( x, 0.0 )
      call pgebuf

      call pgqinf ( 'CURSOR', text, k )					!Use cursor to get a
      if ( text(1:1).eq.'Y' ) then					! better limit
         call printo ( 'Use cursor to refine limit' )
         x = dlim
         y = (ay(1)+ay(2))/2.0
         k = pgcurse ( x, y, text )
         dlim = max(abs(x),0.0001)
         write ( text, '(1x, ''New limit = '',f8.4)' ) dlim
         call printo ( text )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     AVERAGE.FOR
C
C  Contains:-
C
C T_AVERAGE      Perform the 'AVERAGE' program function
C AV_SETUP       Setup and get bad star conditions
C AV_GTIN        Get input MEASURE files
C AV_GTCOLUMN    Get columns where data is in input file
C AV_NAMES       Get locations of stars in files
C AV_LOADN1      Get number of output rows, load posns and output names
C AV_LOADN2      Load positions of names for same order files
C AV_FILAVS      Calc for each file mean mag difference from file 1
C AV_SREFINE     Interactive cursor/keyboard estimate of 2 lists mean mag diff
C AV_GTDATA      Move wanted data from input table row to storage areas
C AV_CHOK        Check if a star is ok
C AV_PTDATA      Load integer into a defined position of character string
C AV_AVSTAR      Calc the best mean mag and other params for a star
C AV_CALDIFLIM   Calc difference limits for magnitudes from mean
C AV_ACCLIM      Get acceptance limits for errors
C AV_GTERRFAC    Get:- ratio true/calculated error; acceptance limit
C AV_HISTCALC    Calculate histograms of the magnitude differences
C AV_AUTODIFLIM  Automatic acceptance limits from difference histograms
C AV_GTDIFLIM    Interactive acceptance limits from difference histograms
C AV_DOSTD       Calc std dev at each mag level
C AV_GTOUT       Get output file
C AV_PFOPEN      Open printing file
C AV_PFWRHEAD    Print out the header information to the Print file.
C AV_DOIT        Average and output to output and print
C AV_WRLINE      Put out a line of results for a star to output and Print file
C AV_PFWRLINE    Puts out to the 'print file' a line (or lines) for a result



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_AVERAGE -- Runs the 'AVERAGE' program
C
C   a j penny                 ral              1991 March

      subroutine t_average ( )

      implicit none
C--
Cbegin


      call av_setup							!Setup and get conditions for taking a
									! star from a file

      call av_gtin							!Get the input MEASURE files

      call av_names							!Get locations of stars in files

      call av_gtout 							!Open output file

      call av_filavs							!Mean differences between files

      call av_acclim							!Deal with the acceptance limits

      call av_pfopen 							!Open printing file

      call av_pfwrhead							!Write out the header info to the Print file

      call av_doit							!Do the averaging: load output and print files


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_SETUP -- Setup and get bad star conditions
C
C   a j penny                      ral              1991 March

      subroutine av_setup ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      integer k, ka

      integer nthelp
      parameter ( nthelp=15 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,10) /
     + 'There are three ways of getting the acceptance limits',
     + 'of how far off the mean a measure can be and still',
     + 'be accepted. The default way is to use 0.5mag. If the',
     + 'default limit is not used, then both the other options',
     + 'involve some estimation on your part',
     + '                   ' ,
     + 'A fuller help is given in the program HELP.',
     + '                    ',
     + ' Option     Function' ,
     + ' ------     --------' /
      data (thelp(k),k=11,nthelp) /
     + ' Default    Use the 0.5mag default ' ,
     + ' Error      Take the input error estimates and accept only' ,
     + '            mags whose difference from the mean is less than' ,
     + '            some multiple of this error.' ,
     + ' Mag        Use a mean value at each magnitude level ' /
Cbegin


      if ( ST_FAILED ) return

      MAXR = 20000
      call azeroi ( NUMGO, KTOT+1 )

      call amovkr ( 1.0, ERRFAC, KNUMF )				!Default values
      call amovkr ( 0.1, ERRLIM, KNUMF )
      FIXDIFLIM = .true.
      call amovkr ( 0.5, DIFLIM, KNX )

      call get_job ( 'USE', 'auto:inter', ka, 2, ' ', 0 )
      MODE = 'auto'
      if ( ka.eq.2 ) MODE = 'inter'

      call printo ( ' ' )
      call get1i ( 'NUMINV', NUMINVLIM, 0, 0, 1000000 )			! Max allowed no of invalid pixels
      call get1i ( 'NUMITS', NUMITSLIM, 29, 0, 100 )			! Max allowed no of iterations
      call get1r ( 'OFFCEN', DCEN, 2.0, 0.0, 1000.0 )			!Get user controls of star measure rejection
      call get1b ( 'CENACC', CENACC, .false. )				! If only fault off centre, accept star?
      call get1r ( 'SIGMALIM', SIGMALIM, 0.5, 0.0, 100.0 )		! Max allowed input sigma error

      call get1b ( 'USESIGMA', USESIGMA, .false. )			!Use Input errors to weight data?
      SIGMAMIN = 0.01
      if ( USESIGMA ) call get1r ( 'SIGMAMIN', SIGMAMIN, 0.01, 1.0e-5,
     +                             100.0 )

      call get1b ( 'USECHI', USECHI, .true. )				!Use the chi-sq values to reject?

      if ( MODE.eq.'auto' ) then
         if ( .not.USESIGMA ) call get1r ( 'DIFMUL', DIFMUL, 2.5, 0.0, 	!Star has to be 'DIFMUL' std devs out
     +                                     1.0e9 )
         call get_job ( 'ASCATTER', 'mag:error:default', KSCATREJ, 1,
     +                             thelp, nthelp )
      endif

      call printo ( ' ' )
      call get1b ( 'NAMES', NAMES, .false. )

      if ( ST_FAILED ) return

      call printo ( ' ' )
      call get1c ( 'FPRINT', FPNAME, ' ', .true. )			!Output controls
      FPRINTING = .false.
      if ( FPNAME.ne.' ' ) then
         call get1r ( 'CHIPRIN', CHIPRIN, 1.0, 0.0, 1.0e10 )
         FPRINTING = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTIN -- Get input MEASURE files
C
C  a j penny              ral                  1991 March

      subroutine av_gtin ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      integer i, iv, k, ierr, tbvxchi, tbychi, ipchi
      logical loop, loopa, ok
      character tin*4, tinchi*7
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      k = 0
      loop = .true.
      do while ( loop )							!Loop getting
         k = k + 1
         ok = .true.

         call pargi ( k )						!Get next file
         call printd ( 'Input name of file no: %d ' )
         if ( k.ge.10 ) then
            write ( tin,    '(''IN'',i2)' )    k
            write ( tinchi, '(''INCHI'',i2)' ) k
         else
            write ( tin,    '(''IN'',i1)' )    k
            write ( tinchi, '(''INCHI'',i1)' ) k
         endif
         call optabr ( tin, IPI(k), TBVXI(k), TBYI(k), .true., ierr )
         if ( ST_FAILED ) return
         if ( ierr.eq.2 ) then						!ok?
            k = k - 1
            ok = .false.
            if ( k.ne.0 ) then						!Must have 1 file
               loop = .false.
            else
               call printo ( 'ERROR: Must have at least one file' )
            endif
         elseif ( ierr.eq.1 ) then
            ST_FAILED = .true.
            return
         endif

         if ( ok ) then							!all same length
            if ( .not.NAMES .and. TBYI(k).ne.TBYI(1) ) then
               call printo ( 'ERROR: File wrong length - try again' )
               ok = .false.
               k = k - 1
               call canpar ( tin )
            endif
         endif

         if ( ok ) call av_gtcolumn ( tin, k )				!Look at column order in file

         if ( ok .and. COLPOS(7,k).ne.0 .and. USECHI ) then		!Get CHI data from any
            loopa = .true. 						! wanted CHI file
            do while ( loopa )
               call optabr ( tinchi, ipchi, tbvxchi, tbychi, .true.,i)
               if ( ST_FAILED ) return
               if ( tbvxchi.lt.7 ) then
                  call printo ( 'ERROR: Too few columns - try again' )
                  call canpar ( tinchi )
               else
                  loopa = .false.
               endif
            enddo
            call copr1  ( %val(ipchi), tbvxchi, tbychi, 6, 1, ALCHI(k) )
            call copfrr ( %val(ipchi), tbvxchi, tbychi, 7, 1,
     +                    CHIL(1,k), tbychi )
            if ( tbychi.lt.KMAXCHI ) call amovkr ( CHIL(tbychi,k),
     +                             CHIL(tbychi+1,k), (KMAXCHI-tbychi) )
         endif

         if ( ok .and. k.eq.KNUMF ) then				!Not too many
            call printo ( 'No more input files allowed' )
            loop = .false.
         endif

         if ( ok ) call gtdesc  ( tin, 'TITLE', TITLEI(k), ' ', iv, i)	!Get title

      enddo

      KTOT = k								!Total no of input tables


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTCOLUMN -- Get columns where data is in input file
C  Either use default, or look for right headers to find
C  the columns. Result (posns of columns) stored in COLPOS
C
C   a j penny                 ral          1991 March

      subroutine av_gtcolumn ( file, kf )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      character*(*)  file	!i:Name of program parameter being accessed
      integer        kf		!i: File number being accessed
C--
      integer j, jj, ierr
      logical more
      character*20 header(NUMCOLP), ahead
      data header / 'X', 'Y', 'MAG', 'DX', 'DY',
     +              'ITERATIONS', 'CHI', 'NUMINVAL',
     +              'IMPORTANCE OF COMPS', 'ERROR',
     +              'FAINTCORR', 'RX', 'RY' /
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( COLPOS(1,kf), NUMCOLP )				!Set up default.

      more = .true.
      do jj = 1, TBVXI(kf)-5						!Find columns for data
         if ( more ) then

            call gthead ( file, jj, ahead, ierr )

            if ( ierr.eq.0 ) then
               do j = 1, NUMCOLP
                  if ( COLPOS(j,kf).eq.0 ) then
                     if ( ahead.eq.header(j) ) COLPOS(j,kf) = jj
                  endif
               enddo
            endif

            more = .false.
            do j = 1, NUMCOLP
               if ( COLPOS(j,kf).eq.0 ) more = .true.
            enddo

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_NAMES -- Get locations of stars in files
C
C   a j penny                      ral              1991 March

      subroutine av_names ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      integer istat, k
Cbegin


      if ( ST_FAILED ) return

      call gtwrki ( 'WORKN', KTOT*MAXR, IPNUMS, istat )			!Open array with file row posns
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call azeroi ( %val(IPNUMS), KTOT*MAXR )				!Set default to not found for all

      call printo ( ' ' )
      if ( NAMES ) then							!Load names from file 1 and
         call printo ('Making sorting tables for files to match names')	! add any extra names from other files
         TBYO = 0
         do k = 1, KTOT
            call av_loadn1 ( %val(IPI(k)), TBVXI(k), TBYI(k),
     +                       %val(IPNUMS), k )
         enddo
      else
         TBYO = TBYI(1)
         call coprr ( %val(IPI(1)), TBVXI(1), TBYI(1), 1, 5, 1,
     +                TBYI(1), TBNAMES, 5, MAXR, 1, 1 )
         call av_loadn2 ( %val(IPNUMS) )
         call pargi ( TBYO )
         call printd ( 'Loaded star names using file 1 - %d stars' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_LOADN1 -- Get number of output rows and load output names
C
C   a j penny                      ral              1991 March

      subroutine av_loadn1 ( tb, tbvx, tby, nums, kf )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'

      integer   tbvx			!i: Input file no of cols
      integer   tby			!i: Input file no of rows
      real	tb(tbvx,tby)		!i: Input file
      integer   nums(KTOT,MAXR)		!o: Positions array
      integer   kf			!i: File number
C--
      integer k, kk, kd
      logical found
Cbegin


      if ( ST_FAILED ) return

      k = 1
      do while ( k.le.tby .and. .not.ST_FAILED )

         kk = 0
         found = .false.
         do while ( kk.lt.TBYO .and. .not.found )
            kk = kk + 1
            call namechr ( TBNAMES(1,kk), tb(1,k), kd )
            if ( kd.eq.0 ) found = .true.
         enddo

         if ( found ) then
            nums(kf,kk) = k
         else
            if ( TBYO.eq.MAXR ) then
               call pargi ( TBYO )
               call printd (
     +              'ERROR: Too many star names: there are over %d' )
               ST_FAILED  = .true.
            else
               TBYO = TBYO + 1
               call amovr ( tb(1,k), TBNAMES(1,TBYO), 5 )
               nums(kf,TBYO) = k
            endif
         endif

         k = k + 1

      enddo

      if ( ST_FAILED ) return

      call pargi ( kf )
      call pargi ( tby )
      call printd ( 'Input file %d - %d stars' )
      call pargi ( TBYO )
      if ( kf.eq.1 ) then
         call printd ( '  Loaded output names 1 - %d stars' )
      else
         call printd ( '  Added extra output names - total stars = %d' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_LOADN2 -- Load positions of names for same order files
C
C   a j penny                      ral              1991 March

      subroutine av_loadn2 ( nums )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      integer    nums(KTOT,MAXR)	!o: Positions array
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBYO
         do j = 1, KTOT
            nums(j,k) = k
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_FILAVS -- Calc for each file mean mag difference from file 1
C
C  Go through each file calculating for good stars:-
C    1) the star mag in file 1
C    2) the difference from the star in File 1 (if star in file 1 is also good)
C    3) the error of that difference (calc from the input errors)
C  Store these in temporary arrays GODDMG, RDIFF and RVAR.
C
C  Calc mean difference and use interactive cursor refine it. Store mean
C  differences in AVDIFF.
C
C  The distribution of good differences for each file is put into LDIFF and
C  the total no of good differences for each file into NUMG.
C
C  Also calc mean X and Y offset of files from first into XDIFFM and YDIFFM
C
C   a j penny                      ral              1991 March

      subroutine av_filavs ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      logical ok, tok
      integer j, k, ngood, kd
      real asig, bsig, sd, rv
      logical tuse(KMAXR)
      real    wt(KMAXR), xdiff(KMAXR), ydiff(KMAXR)
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( LDIFF,  20*KNUMF )					!Clear output LDIFF,
      call azeror ( AVDIFF, KNUMF )					! NUMG and AVDIFF
      call azeroi ( NUMG,   KNUMF )
      call azeror ( XDIFFM, KNUMF )
      call azeror ( YDIFFM, KNUMF )

      do k = 1, TBYO							!Set first file as reference
         call av_gtdata ( %val(IPI(1)), TBVXI(1), TBYI(1), 		! level and store the number
     +                    %val(IPNUMS), 1, k )
         call av_chok ( 1, ok )						! of good stars in it and set
         if ( ok ) NUMG(1) = NUMG(1) + 1				! difference distribution all
      enddo								! at zero. Only allow max allowed
      NUMG(1) = min(NUMG(1),MAXR)
      LDIFF(10,1) = NUMG(1)
      AVDIFF(1) = 0.0

      if ( KTOT.eq.1 ) return						!If only one file, return

      do k = 2, KTOT							!Calc output for the other files

         ngood = 0							!Go through all the stars, noting
         do j = 1, TBYO							! good stars with good star in
									! file 1

            call av_gtdata ( %val(IPI(1)), TBVXI(1), TBYI(1), 		!See if this is a good pair
     +                       %val(IPNUMS), 1, j )
            call av_chok ( 1, ok )
            if ( ok ) then
               call av_gtdata ( %val(IPI(k)), TBVXI(k), TBYI(k),
     +                          %val(IPNUMS), k, j )
               call av_chok ( k, tok )
               if ( .not.tok ) ok = .false.
            endif

            if ( ok .and. ngood.lt.MAXR ) then				!If OK, store that away, noting
               ngood = ngood + 1					! file 1 mag and magdiff
               RDIFF(ngood) = AMAG(k) - AMAG(1)
               wt(ngood) = 1.0
               if ( USESIGMA ) then
                  asig = max(SIGMA(1),SIGMAMIN)
                  bsig = max(SIGMA(k),SIGMAMIN)
                  wt(ngood) = 1.0/(asig*asig + bsig*bsig)
               endif
               GOODMG(ngood) = AMAG(1)
               xdiff(ngood) = PX(k) - PX(1)
               ydiff(ngood) = PY(k) - PY(1)
            endif

         enddo
         NUMG(k) = ngood

         if ( ngood.eq.0 ) then
            call pargi ( k )
            call printd ( 'ERROR: No good matches for file no: %d ' )
         else

            call amovkb ( .true., tuse, ngood )				!Calculate raw mean
            call meanstdr ( RDIFF, wt, tuse, ngood, .false., 0.0,
     +                      .true., 0.2, AVDIFF(k), sd )
            if ( MODE.ne.'auto' ) call av_srefine ( ngood, k )				!Display, use cursor to refine mean
         endif

         if ( ngood.ge.1 ) then						!Store the distribution of
            do j = 1, ngood						! good stars around mean
               kd = (RDIFF(j)-AVDIFF(k))*1000.0
               kd = (kd+525)/50
               kd = min(20,max(kd,1))
               LDIFF(kd,k) = LDIFF(kd,k) + 1
            enddo
         endif

         if ( ngood.ne.0 ) then						!Calc mean X and Y offsets
           call meanstdr ( xdiff, wt, tuse, ngood, .false., 0.0,
     +                      .true., 1.0e10, XDIFFM(k), sd )
            call meanstdr ( ydiff, wt, tuse, ngood, .false., 0.0,
     +                      .true., 1.0e10, YDIFFM(k), sd )
         endif

      enddo

      call asumi ( NUMG(2), (KTOT-1), rv )				!Get total number of good matches
      if ( nint(rv).eq.0 ) then
         call printo ( 'ERROR: No good matches at all' )
         ST_FAILED = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_SREFINE --  Interactive cursor/keyboard estimate of 2 lists mean mag diff
C
C  Plots on graphics screen for all stars in a file:- the difference in
C  magnitude from the mag of the star in file 1. Does this only for stars
C  which have good magnitudes in both files. Plots the diff against the
C  mag. Plots estimated mean difference of stars in file from first file.
C  User can use cursor and keyboard to refine this difference.
C  Result output to AVDIFF for this file.
C
C  a j penny                ral                 1991 March

      subroutine av_srefine ( ngood, kf )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      integer 	ngood		!i: No of matched good mag pairs
      integer 	kf		!i: File being done
C--
      logical  loop
      real am, xmax, xmin, ymax, ymin, ax(2), ay(2), rva, rvb
      character text*72
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      am = AVDIFF(kf) 							!Default result

      call printo ( ' ' )
      write ( text, '('' Mean diff from file 1 for file '', i3,
     +              '' is '', f8.4)' ) kf, am
      call printo ( text )

      if ( ngood.le.1 ) then						!Return if difficult
         call printo ( 'No good pairs' )
         return
      endif

      call printo ( ' ' )						!Start display
      call printo ( 'Display differences between files' )

      call alimr ( GOODMG, ngood, xmin, xmax )				!Get ranges of values
      call alimr ( RDIFF,  ngood, ymin, ymax )
      ymax = ymax - am
      ymin = ymin - am

      ax(1) = real(int(xmin*2.0))/2.0 - 0.5				!Set default desired limits
      ax(2) = real(int(xmax*2.0))/2.0 + 1.0
      ay(1) = real(int(ymin*20.0))/20.0 - 0.05
      ay(2) = real(int(ymax*20.0))/20.0 + 0.10

      loop = .true.							!Refine
      do while ( loop )

         call get2r ( 'PLOTXLIM', ax(1), ax(2), .true., -1.0e8, 1.0e8 )	!Get desired limits
         if ( ST_FAILED ) return
         rva = trunc(ay(1),4)
         rvb = trunc(ay(2),4)
         write ( text, '(''  Mag Differences range from '', f8.3,
     +                   '' to '',f8.3)' ) rva, rvb
         call printo ( text )
         ay(1) = -0.3
         ay(2) = 0.3
         call get2r ( 'PLOTYLIM', ay(1), ay(2), .true., -1.0e8, 1.0e8 )
         if ( ST_FAILED ) return

         call av_pldiff ( ax, ay, am, ngood )				!Plot

         call get1r ( 'NEWMEAN', am, am, -1.0e10, 1.0e10 )		!Refine by keyboard
         if ( ST_FAILED ) return

         call get1b ( 'AGAIN', loop, .false. )				!Again?
         if ( ST_FAILED ) return

      enddo

      AVDIFF(kf) = am


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTDATA -- Move wanted data from input table row to storage areas
C
C     a j penny               ral                1991 March

      subroutine av_gtdata ( tb, tbvx, tby, nums, kf, kina )

      implicit none

      include 'average.inc'
      include 'STARMAN_INC'

      integer   tbvx				!i: Input file no of cols
      integer   tby				!i: Input file no of rows
      real	tb(tbvx,tby)			!i: Input file
      integer   nums(KTOT,MAXR)			!i: Place in file of star
      integer	kf				!i: Number of input file
      integer	kina				!i: Number of star in file
C--
      integer kk, kin
      real rv
Cbegin


      if ( ST_FAILED ) return

      kin = nums(kf,kina)					!Get location in file

      PX(kf) = 0.0						!Default
      PY(kf) = 0.0
      AMAG(kf) = 50.0
      DX(kf) = 0.0
      DY(kf) = 0.0
      RRX(kf) = 0.0
      RRY(kf) = 0.0
      ITER(kf) = 0
      CHI(kf) = 0.0
      NINVAL(kf) = 0
      COMP(kf) = 0.0
      SIGMA(kf) = 0.0
      FAINTCORR(kf) = 0.0

      if ( kin.eq.0 ) return					!No data?

      kk = COLPOS(1,kf) + 5					!Load data
      if ( kk.ne.5 ) PX(kf) = tb(kk,kin)

      kk = COLPOS(2,kf) + 5
      if ( kk.ne.5 ) PY(kf) = tb(kk,kin)

      kk = COLPOS(3,kf) + 5
      if ( kk.ne.5 ) AMAG(kf) = tb(kk,kin)

      kk = COLPOS(4,kf) + 5
      if ( kk.ne.5 ) DX(kf) = tb(kk,kin)

      kk = COLPOS(5,kf) + 5
      if ( kk.ne.5 ) DY(kf) = tb(kk,kin)

      kk = COLPOS(6,kf) + 5
      if ( kk.ne.5 ) ITER(kf) = nint(tb(kk,kin))

      kk = COLPOS(7,kf) + 5
      if ( kk.ne.5 ) CHI(kf) = tb(kk,kin)

      kk = COLPOS(8,kf) + 5
      rv = max(-1.0e6,min(1.0e6,(tb(kk,kin))))
      if ( kk.ne.5 ) NINVAL(kf)=nint(rv)

      kk = COLPOS(9,kf) + 5
      if ( kk.ne.5 ) COMP(kf) = tb(kk,kin)

      kk = COLPOS(10,kf) + 5
      if ( kk.ne.5 ) SIGMA(kf) = tb(kk,kin)

      kk = COLPOS(11,kf) + 5
      if ( kk.ne.5 ) FAINTCORR(kf) = tb(kk,kin)

      kk = COLPOS(12,kf) + 5
      if ( kk.ne.12 ) RRX(kf) = tb(kk,kin)

      kk = COLPOS(13,kf) + 5
      if ( kk.ne.12 ) RRY(kf) = tb(kk,kin)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_CHOK -- Checks if a star is ok
C  Looks at data read in from the files for the star, and checks the
C  data for one file, to see if there is a reason for saying that
C  file's measure of the star is bad.
C
C  a j penny             ral            1991 March

      subroutine av_chok ( knum, ok )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      integer knum		!i: Input table number
      logical ok		!o: Star is ok or not
C--
      integer km, nkm
      real akm, ch, dxa, dya, rx, ry, dist
Cbegin


      if ( ST_FAILED ) return

      ok = .true.

      if ( AMAG(knum).gt.49.9 .or. AMAG(knum).lt.0.01 ) ok = .false.

      if ( ITER(knum).gt.NUMITSLIM ) ok = .false.

      if ( USECHI ) then
         akm = (ALCHI(knum)-AMAG(knum))/0.2 + 1.0
         km = akm
         akm = akm - real(km)
         km = min(KMAXCHI,max(1,km))
         nkm = min(KMAXCHI,max(1,(1+km)))
         ch = CHIL(km,knum) + akm*(CHIL(nkm,knum)-CHIL(km,knum))
         if ( CHI(knum).gt.ch ) ok = .false.
      endif

      if ( .not.CENACC ) then
         rx = RRX(knum)
         ry = RRY(knum)
         dxa = DX(knum)
         dya = DY(knum)
         dist = sqrt((dxa*dxa+dya*dya)/(rx*rx+ry*ry))
         if ( dist.gt.DCEN ) ok = .false.
      endif

      if ( NINVAL(knum).gt.NUMINVLIM ) ok = .false.

      if ( SIGMA(knum).gt.SIGMALIM ) ok = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PTDATA -- Load integer into a defined position of character string
C The position starts at LC and ends at LC+NW-1.
C On output from the s/r, LC is put equal LC+NW-1, the
C end position of the integer.
C
C    a j penny             ral              1991 March

      subroutine av_ptdata ( a, lc, k, nw, kblank )

      implicit none
      include 'STARMAN_INC'

      character*132 a		!i/o: 132a1 character string
      integer lc		!i/o: Start posn of integer/end posn
      integer k			!i: Integer to put in
      integer nw		!i: No of chars to put integer into
      integer kblank		!i: Load ' ' if input is zero (0=yes;1=no)?
C--
      integer nwa, kw, l, j, la, lca, lblank
Cbegin


      if ( ST_FAILED ) return

      if ( k.lt.0 ) then
         l = -1*k
         nwa = nw - 1
         lc = lc + 1
      else
         l = k
         nwa = nw
      endif

      kw = 1
      if ( nwa.ne.1 ) kw = 10**(nwa-1)
      if ( l.ge.10**nwa ) then
         do j = 1,nwa
            lca = lc + j
            a(lca:lca) = '*'
         enddo
         if ( k.lt.0 ) a(lc+1:lc+1) = '-'
      else
         lblank = 0
         do j = 1, nwa
            la = l/kw
            if ( la.eq.0 ) then
               lca = lc + j
               a(lca:lca) = ' '
               if ( lblank.ne.0 ) a(lca:lca) = '0'
            else
               lca = lc + j
               if ( lblank.eq.0 ) lblank = j
               if ( la.eq.1 ) a(lca:lca)= '1'
               if ( la.eq.2 ) a(lca:lca)= '2'
               if ( la.eq.3 ) a(lca:lca)= '3'
               if ( la.eq.4 ) a(lca:lca)= '4'
               if ( la.eq.5 ) a(lca:lca)= '5'
               if ( la.eq.6 ) a(lca:lca)= '6'
               if ( la.eq.7 ) a(lca:lca)= '7'
               if ( la.eq.8 ) a(lca:lca)= '8'
               if ( la.eq.9 ) a(lca:lca)= '9'
            endif
            l = l - la*kw
            kw = kw/10
         enddo
         lca = lc + nwa
         if ( k.eq.0 .and. kblank.eq.1 ) a(lca:lca) = '0'
      endif

      if ( k.lt.0 ) a(lc+lblank-1:lc+lblank-1) = '-'
      lc = lc + nwa


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_AVSTAR -- Calc the best mean mag and other params for a star
C  Can have up to KNUMF estimates, uses various tests to reject
C  some of the estimates.
C
C    a j penny                  ral                1991 March

      subroutine av_avstar ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      logical tuse(KNUMF)
      real    wt(KNUMF), tdiflim(KNUMF)
      logical loop
      integer k, numfar, num
      real    adiff, rdiflim, rdifmax, am, sd, rv
      double precision dam, dvar, dsvar, dsm, dsn, dss, dsv, dm
Cbegin


      if ( ST_FAILED ) return

      do k = 1, KTOT							!See which files have a
         call av_chok ( k, FUSE(k) )					! good magnitude measure
      enddo

      call amovb ( FUSE, tuse, KTOT )					!Set up local use flags

      num = 0								!Average the mag and
      dsm = 0.0d0							! position of the good
      dsvar = 0.0d0							! measures for this star
      do k = 1, KTOT
         if ( tuse(k) )  then
            dvar = 1.0d0
            if ( USESIGMA ) then
               rv = max(SIGMA(k),SIGMAMIN)
               dvar = 1.0d0/(dble(rv)*dble(rv))
            endif
            dsm = dsm + dble(AMAG(k)-AVDIFF(k))*dvar
            dsvar = dsvar + dvar
            num = num + 1
         endif
      enddo
      dam = 0.0
      if ( dsvar.gt.1.0d-20 ) dam = dsm/dsvar
      am = dam

      if ( num.gt.1 ) then						!Throw out mags too far
         loop = .true.							! from mean. Start with
         do while ( loop )						! furthest, then recalc
									! again, until none
									! outside limits

            call av_caldiflim ( tuse, am, tdiflim )			!Calc difference limits
									! for magnitudes from mean

            numfar = 0							!Find furthest out good star
            rdifmax = 0.0
            do k = 1, KTOT
               if ( tuse(k) ) then
                  adiff = abs(am-(AMAG(k)-AVDIFF(k)))
                  if ( adiff.gt.tdiflim(k) ) then
                     rdiflim = adiff/tdiflim(k)
                     if ( rdiflim.gt.rdifmax ) then
                        rdifmax = rdiflim
                        numfar = k
                     endif
                  endif
               endif
            enddo

            loop = .false.
            if ( numfar.ne.0 ) then					!See if no more thrown out
               if ( num.le.2 ) then					! or not enough left
                  num = 0
               else
                  loop = .true.						!if another go, recalc
                  num = num - 1						! new limits and flag the
                  dvar = 1.0d0						! ejected star as no good
                  if ( USESIGMA ) then
                     rv = max(SIGMA(k),SIGMAMIN)
                     dvar = 1.0d0/(dble(rv)*dble(rv))
                  endif
                  dam = dam*dsvar - (AMAG(numfar)-AVDIFF(numfar))*dvar
                  dsvar = dsvar - dvar
                  dam = dam/dsvar
                  am = dam
                  tuse(numfar) = .false.
               endif
            endif

         enddo
      endif

      AVM = am								!Set mean
      AVNUM = num							!Set no of good measures

      if ( AVNUM.eq.0 ) then

         AVM = 50.0
         AVDX = 0.0
         AVDY = 0.0
         AVCHI = 0.0
         AVCOMP = 0.0
         AVITER = 0
         AVSIGMA = 0.0
         AVNUMINV = 0
         AVFAINTCORR = 0.0
         AVSIGMAD = 0.0

         AVPX = 0.0
         AVPY = 0.0
         rv = 0.0
         do k = 1, KTOT
            if ( abs(PX(k)).gt.0.1 .and. abs(PX(k)).gt.0.1 ) then
               AVPX = AVPX + PX(k) - XDIFFM(k)
               AVPY = AVPY + PY(k) - YDIFFM(k)
               rv = rv + 1.0
            endif
         enddo
         if ( rv.gt.0.5 ) then
            AVPX = AVPX/rv
            AVPY = AVPY/rv
         endif

      else

         call amovkr ( 1.0, wt, KTOT )					!Calc means of other
         if ( USESIGMA ) then						! paramaters
            do k = 1, KTOT
               rv = max(SIGMA(k),SIGMAMIN)
               wt(k) = 1.0/(rv*rv)
            enddo
         endif

         call meanstdr (     DX, wt, tuse, KTOT, .false., 0.0, .false.,	!Get means of parameters
     +                      0.0,    AVDX, sd )
         call meanstdr (     DY, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,    AVDY, sd )
         call meanstdr (    CHI, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,   AVCHI, sd )
         call meanstdr (   COMP, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,  AVCOMP, sd )
         call meanstdi (   ITER, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,  AVITER, sd )
         call meanstdr (  SIGMA, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0, AVSIGMA, sd)
         call meanstdi ( NUMINV, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0, AVNUMINV,sd )
         call meanstdr ( FAINTCORR, wt, tuse, KTOT, .false., 0.0,
     +                      .false., 0.0, AVFAINTCORR, sd )

         call asubkr ( PX, XDIFFM(k), PX, KTOT )			!Get mean X,Y position
         call asubkr ( PY, YDIFFM(k), PY, KTOT )

         call meanstdr (     PX, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,    AVPX, sd )
         call meanstdr (     PY, wt, tuse, KTOT, .false., 0.0, .false.,
     +                      0.0,    AVPY, sd )

         AVSIGMAD = 0.0							!Calculate mean
         dss = 0.0d0							! theoretical error
         dsn = 0.0d0							! from scatter in output
         dsv = 0.0d0							! mags
         do k = 1, KTOT
            if ( tuse(k) ) then
               dvar = 1.0d0
               if ( USESIGMA ) then
                  rv = max(SIGMA(k),SIGMAMIN)
                  dvar = 1.0d0/(dble(rv)*dble(rv))
               endif
               dm = dble(AMAG(k)-AVDIFF(k)-AVM)
               dss = dss + dm*dm*dvar
               dsn = dsn + 1.0d0
               dsv = dsv + dvar
            endif
            if ( dsn.gt.1.5d0 ) AVSIGMAD = sqrt(dss/((dsn-1.0d0)*dsv))
         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_CALDIFLIM -- Calc difference limits for magnitudes from mean
C This calculates the limit to the value of the difference from the mean,
C for each estimate, that triggers the rejection of the estimate. Thus if the
C difference is less than this value, the estimate is accepted, if greater
C rejected.
C
C   a j penny                 ral          1991 March

      subroutine av_caldiflim ( tuse, am, tdiflim )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      logical tuse(KNUMF)		!i: Flags for which estimates are good
      real    am			!i: Mean magnitude of star
      real    tdiflim(KNUMF)		!o: Difference limits for acceptability
					!    for each estimate
C--
      real sm, rva, errmean, rv
      integer k, num, kam
Cbegin


      if ( ST_FAILED ) return

      if ( .not.FIXDIFLIM ) then					!Calc mean sigma-squared
         sm = 0.0							! for error in mean
         num = 0
         do k = 1, KTOT
            if ( tuse(k) ) then
               rv = 0.1
               if ( USESIGMA ) rv = max(SIGMA(k),SIGMAMIN)
               sm = sm + rv*rv
               num = num + 1
            endif
         enddo
         errmean = sm/real(num)
      endif

      kam = min(49,max(1,int(am)))					!Find for each table for,
      do k = 1, KTOT							! this star, the difference
									! limits
         tdiflim(k) = 0.0
         if ( tuse(k) ) then
            if ( FIXDIFLIM ) then
               tdiflim(k) = max(abs(DIFLIM(kam)),0.0001)
            else
               rva = 0.1
               if ( USESIGMA ) rva = max(SIGMA(k),SIGMAMIN)
               rv = (ERRFAC(k)*sqrt((rva*rva)+errmean))
               tdiflim(k) = max(ERRLIM(k),rv)
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_ACCLIM -- Get acceptance limits for errors
C
C   a j penny                 ral          1991 March

      subroutine av_acclim ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      integer k, kopt, kdef
      logical loop

      integer nthelp
      parameter ( nthelp=16 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,10) /
     + 'There are three ways of getting the acceptance limits',
     + 'of how far off the mean a measure can be and still',
     + 'be accepted. The default way is to use 0.5mag. If the',
     + 'default limit is not used, then both the other options',
     + 'involve some estimation on your part',
     + '                   ' ,
     + 'A fuller help is given in the program HELP.',
     + '                    ',
     + ' Option     Function' ,
     + ' ------     --------' /
      data (thelp(k),k=11,nthelp) /
     + ' Default    Use the 0.5mag default ' ,
     + ' Error      Take the input error estimates and accept only' ,
     + '            mags whose difference from the mean is less than' ,
     + '            some multiple of this error.' ,
     + ' Mag        Use a mean value at each magnitude level ' ,
     + ' Ok         Defined the limits ok - continue to next step' /
Cbegin


      if ( ST_FAILED ) return

      call av_histcalc							!Calc differencs histograms

      call amovkr ( 0.5, DIFLIM, KNX )

      if ( MODE.eq.'auto' ) then
         call av_autodiflim
      else

         kopt = 1
         loop = .true.
         do while ( loop )
            call printo ( ' ' )
            kdef = kopt
            call get_job ( 'SCATTER', 'mag:error:default:ok', kopt,
     +                                 kdef, thelp, nthelp )
            if ( ST_FAILED ) return
            if ( kopt.eq.1 ) call av_gtdiflim
            if ( kopt.eq.2 ) then
               if ( USESIGMA ) then
                  call av_gterrfac
               else
                  call printo ( 'You are not using the input errors' )
               endif
            endif
            if ( kopt.eq.3 ) call amovkr ( 0.5, DIFLIM, KNX )
            if ( kopt.eq.4 ) loop = .false.
            kopt = 4
         enddo
      endif

      call av_dostd							!Calc std dev at each mag


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTERRFAC -- Get:- ratio true/calculated error; acceptance limit
C
C   a j penny                 ral          1991 March

      subroutine av_gterrfac ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      integer j, k, ks, kk, kka, nerr
      real    rv
      real ax(12,KNUMF), ay(12,KNUMF)
      double precision dsum, dnum, damerr, dyy, dyym, dadif, dv
      double precision dnn(12,KNUMF),   dmerr(12,KNUMF),
     +                 derr(12,KNUMF),  dssadif(12,KNUMF),
     +                 dnsum(12,KNUMF), dnssum(12,KNUMF),
     +                 dsadif(12,KNUMF)
Cbegin


      if ( ST_FAILED ) return

      call amovkr ( 1.0, ERRFAC, KTOT )					!Default values
      call amovkr ( 0.1, ERRLIM, KTOT )

      FIXDIFLIM = .true.						!Set up
      call amovkr ( 0.5, DIFLIM, KNX )
      call azeroi ( NDIFL,   KNX*KNUMD )
      call azerod ( dnsum,   12*KNUMF )
      call azerod ( dnssum,  12*KNUMF )
      call azerod ( dsadif,  12*KNUMF )
      call azerod ( dssadif, 12*KNUMF )
      call azerod ( dmerr,   12*KNUMF )
      call azerod ( derr,    12*KNUMF )
      call azerod ( dnn,     12*KNUMF )

      do ks = 1, TBYO							!Go through star list

         do k = 1, KTOT							!Get the data on this
             call av_gtdata ( %val(IPI(k)), TBVXI(k), TBYI(k), 		! star from all the files
     +                        %val(IPNUMS), k, ks )
         enddo

         call av_avstar 						!Calc best mean values

         dnum = 0.0d0							!Store the errors
         dsum = 0.0d0
         do k = 1, KTOT
            if ( FUSE(k) ) then
               dnum = dnum + 1.0d0
               rv = max(SIGMA(k),SIGMAMIN)
               dsum = dsum + dble(rv)*dble(rv)
            endif
         enddo
         if ( dnum.gt.1.5d0 ) then
            damerr = dsqrt(dsum)/dnum
            do k = 1, KTOT
               if ( FUSE(k) .and. SIGMA(k).lt.0.3 ) then
                  nerr = 1 + int(40.0*SIGMA(k))
                  dadif = dble(AMAG(k)-AVDIFF(k)-AVM)
                  dsadif(nerr,k) = dsadif(nerr,k) + dadif
                  dssadif(nerr,k) = dssadif(nerr,k) + dadif*dadif
                  derr(nerr,k) = derr(nerr,k) + max(SIGMA(k),SIGMAMIN)
                  dmerr(nerr,k) = dmerr(nerr,k) + damerr
                  dnn(nerr,k) = dnn(nerr,k) + 1.0d0
               endif
            enddo
         endif

      enddo

      do k = 1, KTOT							!Calc mean values
         do j = 1, 12
            ay(j,k) = 0.0
            if ( dnn(j,k).lt.0.5d0 ) then
               ax(j,k) = 0.0125 + 0.025*real(j-1)
            else
               ax(j,k) = derr(j,k)/dnn(j,k)
               dyy = (dssadif(j,k)-dnn(j,k)*dsadif(j,k))/dnn(j,k)
               dyym = dmerr(j,k)/dnn(j,k)
               dv = dyy - dyym*dyym
               if ( dv.gt.0.0d0 ) ay(j,k) = sqrt(dv)
            endif
         enddo
      enddo

      kk = 1								!Plot calc errors vs actual errors
      do while ( kk.ge.1 )
         kka = kk
         if ( kka.gt.KTOT ) kka = 0
         call get1i ( 'FILENUM', kk, kka, 0, KTOT )
         if ( ST_FAILED ) return
         if ( kk.ge.1 ) then
            call av_plerr ( ax, ay, kk )
            call get1r ( 'ERRFAC', ERRFAC(kk), 1.0, 0.0, 1.0e9 )
            call get1r ( 'ERRLIM', ERRLIM(kk), 0.01, 0.0, 1.0e9 )
            if ( ST_FAILED ) return
            kk = kk + 1
         endif
      enddo

      call get1r ( 'ERRMUL', ERRMUL, 2.5, 0.0, 1.0e9 )			!Star has to be 'ERRMUL' std devs out
      if ( ST_FAILED ) return
									! to be rejected

      FIXDIFLIM = .false.						!Set real type of difference test


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_HISTCALC -- Calculate histograms of the magnitude differences
C
C   a j penny                 ral               1991 March

      subroutine av_histcalc ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      integer ks, k, kam, kdone, kmdif
Cbegin


      if ( ST_FAILED ) return

      FIXDIFLIM = .true.						!Set up default
      call amovkr ( 0.5, DIFLIM, KNX )
      call azeroi ( NDIFL, KNX*KNUMD )

      do ks = 1, TBYO							!Get run of differences with magnitude,
									! and set new acceptance limits.

         do k = 1, KTOT							!Get the data on this star
             call av_gtdata ( %val(IPI(k)), TBVXI(k), TBYI(k),
     +                        %val(IPNUMS), k, ks )
         enddo

         call av_avstar 						!Calc best mean mag and posn and
									! number of good estimates

         kam = min(49,max(1,int(AVM)))					!Store the differences
         kdone = 0
         do k = 1, ktot
            if ( FUSE(k) ) kdone = kdone + 1
         enddo
         if ( kdone.gt.1 ) then
            do k = 1, KTOT
               if ( FUSE(k) ) then
                  kmdif = iabs(int(40.0*(AMAG(k)-AVDIFF(k)-AVM))) + 1
                  if ( kmdif.gt.KNUMD ) kmdif = KNUMD
                  NDIFL(kam,kmdif) = NDIFL(kam,kmdif) + 1
               endif
            enddo
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_AUTODIFLIM -- Automatic acceptance limits from difference histograms
C
C    a j penny              ral              1991 March

      subroutine av_autodiflim ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      integer j, k, ka, lnum
      real  rv
      double precision dsd, dnum, dv
      character text*72
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      if ( KSCATREJ.eq.3 ) then

         call amovkr ( 0.5, DIFLIM, KNX )

      elseif ( KSCATREJ.eq.2 ) then

         if ( USESIGMA ) then
            call av_gterrfac
         else
            call printo ( 'ERROR: not using the input errors' )
            ST_FAILED = .true.
         endif

      else

         call amovkr ( 0.5, DIFLIM, KNX )
         call asumi ( NDIFL, KNX*KNUMD, rv )				!Check there are any
         if ( nint(rv).eq.0 ) then
            call printo ( ' ' )
            call printo ( '  No scatter between files' )
            return
         endif

         call printo ( ' ' )
         call printo ( '  Mag range   No of good     Difference '//
     +                 'from mean value a magn in one' )
         call printo ( '              diffs          file can have '//
     +                 'and still be accepted' )

         do k = 1, KNX-2

            lnum = 0							!Points in it?
            do j = 1, KNUMD
               lnum = lnum + NDIFL(k,j)
            enddo

            if ( lnum.eq.0 ) then
               rv = 0.5
               if ( k.ne.1 ) rv = DIFLIM(k-1)
            else
               rv = 0.5
               if ( lnum.gt.1 ) then
                  dnum = dble(lnum-1)
                  dsd = 0.0d0
                  do j = 1, KNUMD
                     dv = real(j)*0.025
                     dsd = dsd + dble(NDIFL(k,j))*(dv*dv)
                  enddo
                  dsd = dsqrt(dsd/dnum)
                  rv = DIFMUL*real(dsd)
               endif
               ka = k + 1
               rv = trunc(rv,3)
               write ( text, '(3x,i2,'' - '',i2,4x,i7,7x,f6.3)' )
     +                       k, ka, lnum, rv
               call printo ( text )
            endif
            call amovkr ( rv, DIFLIM(k), KNX-k+1 )

         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTDIFLIM -- Interactive acceptance limits from difference histograms
C  Uses cursor interaction with plots of the histogram of magnitude
C  differences at each magnitude level.
C
C    a j penny              ral              1991 March

      subroutine av_gtdiflim ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      logical loop
      integer k, j, lnum
      real dlim, ax(2), ay(2), ymin, ymax, akdone
Cbegin


      if ( ST_FAILED ) return

      call asumi ( NDIFL, KNX*KNUMD, akdone )				!Check there are any
      if ( nint(akdone).eq.0 ) then
         call printo ( 'ERROR: No differences' )
         return
      endif

      call printo ( ' ' )						!Do for each mag range with
      call printo ( 'Display and get difference limits' )		! points in it

      do k = 1, KNX-2

         lnum = 0							!Points in it?
         do j = 1, KNUMD
            lnum = lnum + NDIFL(k,j)
         enddo

         loop = .true.							!Points and got good value?
         do while ( lnum.ne.0 .and. loop )

            call printo ( ' ' )
            call pargi ( k )
            call pargi ( (k+1) )
            call printd ( 'Mag range: %d to %d ')

            ax(1)  = 0.0
            ax(2) = 0.5
            call get2r ( 'PLHISTX', ax(1), ax(2), .true., 0.0, 1.0e8 )	!Get ranges of X plot values
            if ( ST_FAILED ) return

            ymin = 0.0							!Get ranges of Y plot values
            ymax = 0.0
            do j = 1, KNUMD
               ymax = max(ymax,real(NDIFL(k,j)))
            enddo
            ymax = 1.2*ymax
            ay(1) = ymin
            ay(2) = ymax
            call get2r ( 'PLHISTY', ay(1), ay(2), .true., 0.0, 1.0e8 )
            if ( ST_FAILED ) return

            dlim = ax(1) + 0.2*(ax(2)-ax(1))				!Plot histogram of differences
            if ( k.ne.1 ) dlim = DIFLIM(k-1)				! and use cursor to set
            call av_plhis ( ax, ay, dlim, k )				! difference limit
            call amovkr ( dlim, DIFLIM(k), KNX-k+1 )

            call get1b ( 'AGAIN', loop, .false. )
            if ( ST_FAILED ) return
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_DOSTD -- Calc std dev at each mag level
C
C   a j penny                 ral          1991 March

      subroutine av_dostd ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'
C--
      integer k, ks, kam, kdone
      real dif, s, ss, sn, std
      real difss(KNX,3)
Cbegin


      if ( ST_FAILED ) return

      call azeror ( DIFSTD, KNX )					! Default

      call azeror ( difss, KNX*3 )
      do ks = 1, TBYO

         do k = 1, KTOT							!Get the data on this star
             call av_gtdata ( %val(IPI(k)), TBVXI(k), TBYI(k),
     +                        %val(IPNUMS), k, ks )
         enddo

         call av_avstar 						!Calc best mean mag and posn and number of good estimates

         kam = min(49,max(1,int(AVM)))					!Look at the differences
         kdone = 0
         do k = 1, KTOT
            if ( FUSE(k) ) kdone = kdone + 1
         enddo

         if ( kdone.gt.1 ) then
             do k = 1, KTOT
              if ( FUSE(k) ) then
                  dif = AMAG(k) - AVDIFF(k) - AVM
                  if ( abs(dif).le.DIFLIM(kam) ) then
                     dif = dif*sqrt(real(kdone)/real(kdone-1))
                     difss(kam,1) = difss(kam,1) + dif
                     difss(kam,2) = difss(kam,2) + dif*dif
                     difss(kam,3) = difss(kam,3) + 1.0
                  endif
               endif
            enddo
         endif

      enddo

      do k = 1, KNX
         if ( difss(k,3).gt.1.5 ) then
            s = difss(k,1)
            ss = difss(k,2)
            sn = difss(k,3)
            std = sqrt((ss-(s*s/sn))/(sn-1.0))
            DIFSTD(k) = std
         else
            DIFSTD(k) = 0.0
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_GTOUT -- Get output file
C
C   a j penny                      ral              1991 March

      subroutine av_gtout ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      integer k, istat
      character*20 header(TBXO)
      data header / 'X', 'Y', 'Mag', 'Number', 'Dx',
     +              'Dy', 'Iterations', 'Chi', 'Numinval',
     +              'Importance of comps',
     +              'Sigmatheory', 'Sigmadiff', 'Faintcorr' /
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )						!Open output file
      call optabw ( 'OUT', IPO, TBVXO, TBYO, .false., istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1c ( 'TITLE', TITLE, 'Output from Average', .true. )	!Put output file headers
      if ( ST_FAILED ) return

      call ptdesc ( 'OUT', 'TITLE', TITLE )
      do k = 1, TBXO
         call pthead ( 'OUT', k, header(k), istat )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PFOPEN -- Open printing file
C
C  a j penny                ral          1991 March

      subroutine av_pfopen ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( FPRINTING ) open ( unit=1, file=FPNAME, status='NEW' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PFWRHEAD -- Prints out the header information to the Print file.
C
C  a j penny                ral          1991 March

      subroutine av_pfwrhead ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      character*1000 aout
      character*1 kifchi, kcen
      integer j, k, lnum
      real bchil(KMAXCHI)
Cbegin


      if ( ST_FAILED ) return

      if ( .not.FPRINTING ) return

      write ( 1, '(''Output file title is   '',a72)' ) TITLE		!Output file title

      kifchi = 'N'							!Checks
      if ( USECHI ) kifchi = 'Y'
      kcen = 'N'
      if ( CENACC ) kcen = 'Y'
      write ( 1,
     +       '(  '' '', ''Centre distance allowed = '', f6.1,
     +        /'' '', ''Check for chi errors (Y/N)   '', a1,
     +        /'' '', ''Accept if centre off only fault (Y/N) '', a1,
     +        /'' '', ''Invalid points allowed = '', i5,
     +        /'' '', ''Iterations allowed = '', i5,
     +        /'' '', ''Sigma error allowed = '', f7.3,
     +        /'' '', '' ''/)' )
     +        DCEN, kifchi, kcen, NUMINVLIM, NUMITSLIM, SIGMALIM
									!Scatter of mags
      write ( 1,
     +        '( '' '', ''Run of mag differences with magnitude''/
     +           '' Mag   Limit   Std dev  No   .025 .050 .075 .100'',
     +           '' .125 .150 .175 .200 .225 .250 .275 .300 .325 .350'',
     +           '' .375 .400 .425 .450 .475 .500'')' )

      do k = 1, KNX
         lnum = 0
         do j = 1, KNUMD
            lnum = lnum + NDIFL(k,j)
         enddo
         if ( lnum.ne.0 ) write ( 1,
     +      '(1x,i3,f8.2,f7.3,2x,i5,2x,20i5)' ) k, DIFLIM(k),
     +                        DIFSTD(k), lnum, (NDIFL(k,j),j=1,KNUMD)
      enddo
      write ( 1, '(1x,1x/)' )

      do k = 1, KTOT							!Do input files

									!Input file titles
         write ( 1, '('' Input file no '',i3,'' title is   '',
     +         a50 )' ) k, TITLEI(k)

         if ( USECHI ) then						!Chi limits
            do j = 1, 60, 5
               bchil(j) = min(CHIL(j,k),9999.0)
            enddo
            write ( 1, '( '' Chi mag start = '',f6.2,/
     +             '' Chi limits at 1 mag steps''/
     +             1x,6F7.1/ 1x,7F7.1)' ) ALCHI(k), (bchil(j),j=1,60,5)
         endif
         write ( 1, '(1x,1x)' )

      enddo

      write ( 1, '(1x,1x)' )						!Matches with file 1
      do k = 1, KTOT
         write ( 1, '('' Mean diff from file 1 for file '',i3,
     +           '' is '', f10.4)' ) k, AVDIFF(k)
         write ( 1, '('' No of good close matches used for diff ='',
     +           i6 )' ) NUMG(k)
         write ( 1, '('' Good diffs from file 1 (in 0.05mag '',
     +           ''groups) = ''/ 1x, 10i5/1x, 10i5/ )' )
     +                   (LDIFF(j,k),j=1,20)
      enddo

									!1st Caption to results
      write ( 1, '(1x, ///10x,''Chi values have been multiplied '',
     +               ''by '',f10.2/)' ) CHIPRIN

      aout = ' '							!2nd Caption to results
      j = 1
      aout(j:j+3) = 'Name'
      j = j + 8
      aout(j:j) = 'X'
      j = j + 5
      aout(j:j) = 'Y'
      j = j + 4
      aout(j:j+2) = 'Mag'
      j = j + 7
      aout(j:j) = 'N'
      j = j + 5
      aout(j:j+3) = 'Difs'
      j = j + 1 + 4*KTOT
      aout(j:j+2) = 'Its'
      j = j + 2 + 3*KTOT
      aout(j:j+2) = 'Chi'
      j = j + 2 + 3*KTOT
      aout(j:j+1) = 'Dx'
      j = j + 2 + 3*KTOT
      aout(j:j+1) = 'Dy'
      j = j + 2 + 3*KTOT
      aout(j:j+6) = 'Invalid'
      write ( 1, '(1x,132a1)' ) (aout(k:k),k=1,min(132,(j+6)))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_DOIT -- Do the averaging and output to output and print
C            files
C
C   a j penny                 ral          1991 March

      subroutine av_doit ( )

      implicit none
      include 'STARMAN_INC'
      include 'average.inc'
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do j = 1, TBYO							!Go through stars making the best
									! estimate of mag and position

         do k = 1, KTOT							!Get data on this star
             call av_gtdata ( %val(IPI(k)), TBVXI(k), TBYI(k), 		! from all the files
     +                        %val(IPNUMS), k, j )
         enddo

         call av_avstar 						!Calc best mean mag, posn,
									! number of good estimates

         call av_wrline ( %val(IPO), j )				!Store this result and the
									! parameters and print

      enddo

      call av_printsum


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_WRLINE -- Put out a line of results for a star to output and Print file
C
C  a j penny              ral          1991 March

      subroutine av_wrline ( tbo, ks )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      real	tbo(TBVXO,TBYO)			!o: Output tbale
      integer	ks				!i: Output table row being done
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      call av_pfwrline ( ks, %val(IPNUMS) )				!Write to print file

      call amovr ( TBNAMES(1,ks), tbo(1,ks), 5 )			!Store in Output file
      call cop1r ( AVPX,        tbo, TBVXO, TBYO,  6, ks )
      call cop1r ( AVPY,        tbo, TBVXO, TBYO,  7, ks )
      call cop1r ( AVM,         tbo, TBVXO, TBYO,  8, ks )
      call cop1r ( AVNUM,       tbo, TBVXO, TBYO,  9, ks )
      call cop1r ( AVDX,        tbo, TBVXO, TBYO, 10, ks )
      call cop1r ( AVDY,        tbo, TBVXO, TBYO, 11, ks )
      call cop1r ( AVITER,      tbo, TBVXO, TBYO, 12, ks )
      call cop1r ( AVCHI,       tbo, TBVXO, TBYO, 13, ks )
      call cop1r ( AVNUMINV,    tbo, TBVXO, TBYO, 14, ks )
      call cop1r ( AVCOMP,      tbo, TBVXO, TBYO, 15, ks )
      call cop1r ( AVSIGMA,     tbo, TBVXO, TBYO, 16, ks )
      call cop1r ( AVSIGMAD,    tbo, TBVXO, TBYO, 17, ks )
      call cop1r ( AVFAINTCORR, tbo, TBVXO, TBYO, 18, ks )

      k = nint(AVNUM) + 1
      NUMGO(k) = NUMGO(k) + 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PRINTSUM -- Print summary of no of good output
C
C  a j penny              ral          1994 Dec

      subroutine av_printsum ( )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

C--
      integer j, ja, jb, k, ka
      character text*72
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call printo ( '  Number of stars output with measures' )
      call printo ( '  ------------------------------------' )
      k = min(KTOT,5)
      text = '  Number of measures:'
      do j = 0, k
         ja = 27 + j*6
         jb = ja + 1
         write ( text(ja:jb), '(i2)' ) j
      enddo
      if ( KTOT.gt.5 ) text(ja+6:) = 'Over'
      call printo ( text )

      text = '  Number of stars:'
      do j = 0, k
         ja = 24 + j*6
         jb = ja + 5
         ka = min(99999,NUMGO(j+1))
         write ( text(ja:jb), '(i5)' ) ka
      enddo
      if ( KTOT.gt.5 ) then
         ka = 0
         do j = 5, KTOT
            ka = ka + NUMGO(j+1)
         enddo
         ka = min(99999,ka)
         write ( text(ja+6:), '(i5)' ) ka
      endif
      call printo ( text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AV_PFWRLINE -- Puts out to the 'print file' a line (or lines) for a result
C
C  a j penny                ral          1991 March

      subroutine av_pfwrline ( kl, nums )

      implicit none
      include 'average.inc'
      include 'STARMAN_INC'

      integer	kl			!i: Row of table to deal with
      integer   nums(KTOT,MAXR)		!i: Place in file of row
C--
      character*1000 aout
      byte asc(20)
      integer iv, k, nchar, lc, lca, iva, kk, ks, ke, klines,
     +        lcst, kin
Cbegin


      if ( ST_FAILED ) return

      if ( .not.FPRINTING ) return					!Check making print file

      aout = ' '							!Make an ASCII line of the results

      call amovr ( TBNAMES(1,kl), asc, 5 ) 				!Name
      do k = 1, 5
         nchar = asc(k)
         aout(k:k) = char(nchar)
      enddo

      iv = min(1.0e6,max(-1.0e6,AVPX))					!X
      lc = 5
      call av_ptdata ( aout, lc, iv, 4, 1 )

      iv = min(1.0e6,max(-1.0e6,AVPY))					!Y
      lc = lc + 1
      call av_ptdata ( aout, lc, iv, 4, 1 )

      iv = min(1.0e3,max(-1.0e3,AVM))					!Mag
      lc = lc + 3
      call av_ptdata ( aout, lc, iv, 2, 1 )
      if ( AVM.lt.0.0 .and. AVM.gt.-1.0 ) then
         lca = lc - 1
         aout(lca:lca) = '-'
      endif
      lc = lc + 1
      aout(lc:lc) = '.'
      iva = abs((AVM-real(iv))*10.0)
      call av_ptdata ( aout, lc, iva, 1, 1 )
      iva = abs((AVM-real(iv)-(real(iva)/10.0))*100.0)
      call av_ptdata ( aout, lc, iva, 1, 1 )

      lc = lc + 2							!Number of good measures
      iv = nint(AVNUM)
      call av_ptdata ( aout, lc, iv, 1, 1 )

      lcst = lc								!Block of (N>=1) output lines
      klines = 1 + (KTOT-1)/5
      do kk = 1, klines
         ks = 1 + (kk-1)*5
         ke = min(KTOT,(ks+4))
         lc = lcst + 2
         if ( kk.ne.1 ) aout = ' '

         do k = ks, ke							!Mag diffs

            kin = nums(k,kl)						!Get location in file
            if ( kin.eq.0 ) then
               lc = lc + 4
            else
               if ( FUSE(k) ) then
                  iv = nint(100.0*(AMAG(k)-AVDIFF(k)-AVM))
                  iv = min(999,max(-999,iv))
                  call av_ptdata ( aout, lc, iv, 4, 1 )
               else
                  aout(lc+1:lc+4) = '    '
                  lc = lc + 4
               endif
            endif
         enddo

         lc = lc + 2							!Iter
         if ( kk.ne.1 .and. kk.eq.klines ) lc = lc + (4-ke+ks)*4
         do k = ks, ke
            call av_ptdata ( aout, lc, ITER(k), 3, 1 )
         enddo

         lc = lc + 2							!Chi
         if ( kk.ne.1 .and. kk.eq.klines ) lc = lc + (4-ke+ks)*3
         do k = ks, ke
            iv = nint(CHIPRIN*CHI(k))
            call av_ptdata ( aout, lc, iv, 3, 1 )
         enddo

         lc = lc + 2							!Dx
         if ( kk.ne.1 .and. kk.eq.klines ) lc = lc + (4-ke+ks)*3
         do k = ks, ke
            iv = nint(DX(k))
            call av_ptdata ( aout, lc, iv, 3, 1 )
         enddo

         lc = lc + 2							!Dy
         if ( kk.ne.1 .and. kk.eq.klines ) lc = lc + (4-ke+ks)*3
         do k = ks, ke
            iv = nint(DY(k))
            call av_ptdata ( aout, lc, iv, 3, 1 )
         enddo

         lc = lc + 2							!Number invalid
         if ( kk.ne.1 .and. kk.eq.klines ) lc = lc + (4-ke+ks)*3
         do k = ks, ke
            call av_ptdata ( aout, lc, NINVAL(k), 3, 0 )
         enddo

         write ( 1, '(1x,132a1)' ) (aout(k:k),k=1,min(132,lc))		!Write line to Print file

      enddo


      end

