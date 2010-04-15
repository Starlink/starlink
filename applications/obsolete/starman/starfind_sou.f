CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C STARFIND -- Find stars in an image
C          For details see the 'STARFIND' help
C
C  a j penny               stsci             86-11-26

      subroutine starfind ( ierradam )

      implicit none

      integer    ierradam       !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_starfind

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   T_STARFIND.F
C
C   Contains:-
C
C T_STARFIND   Find stars in an image (Main s/r)
C FI_GCL       Get CL input and input image
C FI_GWSA      Open work space for tables
C FI_GWSB      Open work space for tables
C FI_ARRSET    Load BS and BZ into array and remove minimum
C FI_STATIM    Make Mann_Whitney probability image
C FI_SKY       Make the sky image
C FI_FINDP     Find the peaks in a confidence map
C FI_LCHECK    See what levels are present
C FI_FLAG      Flag all pixels above threshold not already marked
C FI_STAR      Calc position of all new isolated sets
C FI_TSTORE    Put result into local store and report
C FI_ELLIPSE   Find elliptical images
C FI_NIPPLE    Find faint companions to stars (as U-test long projections)
C FI_XNIPPLE   Find faint companions of a star
C FI_ISCOL     Is this pixel in a 'col'?
C FI_RLOAD     Load results from this program into results table
C FI_WEEDP     Weed out close pairs in a list
C FI_WEEDI     Weed out invalid peaked stars with multiple fits
C FI_OSTORE    Open the output list and store results table into it
C FI_SMEAR     Remove doubles that are the result of image smear
C FI_BOX       Look for significant excess in pair angles, separations


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_STARFIND -- Find stars in an image (Main s/r)
C
C   a j penny                 dao           1988-04-25

      subroutine t_starfind ( )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ipima, istat
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      call fi_gcl 							!Get CL input and input image

      call fi_gwsa 							!Open tables work space

      call fi_gwsb 							!Open images work spaces

      if ( ST_FAILED ) return						!Check for failure

      call gtwrkr ( 'WORKIMA', NX*NY, ipima, istat )			!Make real working image
      if ( IMTYPE.eq.'SHORT' ) then					! and release input image
         call azchtsr ( %val(IPIM), INVAL, %val(ipima), RINVAL, NX*NY)
         RINVAL = INVAL
      else
          call amovr ( %val(IPIM), %val(ipima), NX*NY )
      endif
      call canpar ( 'IN' )

      call fi_arrset ( %val(ipima) )					!Allow for BS,BZ, and remove minimum

      call coprrr ( %val(ipima), NX, NY, 1.0, 0.0,RINVAL,%val(IPIMU))	!Make uninvalided image

      call fi_statim ( %val(IPIMU), %val(IPWB), 1 )			!Make the U-test image

      call fi_sky ( %val(ipima), %val(IPWD), %val(IPWE) )		!Make the sky and (sky+noise)

      call fi_findp ( %val(IPIMU), %val(IPWB), %val(IPWC), %val(IPXP), 	!Find the stars
     +                %val(IPYP), %val(IPFP), %val(IPWD), %val(IPWE) )

      call fi_rload ( %val(IPXY), %val(IPFP), %val(IPWE), %val(IPIMU), 	!Put into results table
     +                %val(IPXP), %val(IPYP), %val(IPWP), %val(IPZP),
     +                %val(IPVP) )

      call fi_weedp ( %val(IPXY) )					!Reject close pairs

      call fi_weedi ( %val(IPXY), %val(ipima), %val(IPWD) )		!Reject multiple marks of invalid stars

      call fi_smear ( %val(IPXY), %val(IPVP) )				!Reject pairs caused by smearing

      call fi_ostore ( %val(IPXY) )					!Store in output file


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_GCL -- Get CL input and input image
C
C       a j penny                  stsci          1988-05-02

      subroutine fi_gcl ( )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      character ktopt*12
      integer   istat
Cbegin

      if ( ST_FAILED ) return						!Check for failure

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!Get the image
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      if ( NX.lt.10. or. NY.lt.10 ) then
         call printo ( 'ERROR: The image sides must be 10 or more' )
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,istat)

      call get1r ( 'RADIUS', RADIUS, 2.0, 0.2, 20.0 )			!Rough star radius

      call get1r ( 'PLIMIT', PLIMIT, 0.001, 1.0e-5, 1.0 )  		!probable limit

      call get1r ( 'WLIMIT', WLIMIT, 0.05, 1.0e-5, 1.0 )   		!wing probable limit

      call get1b ( 'ELLIPSE', ELLIP, .true. )				!Look at elliptical stars

      call get1b ( 'FAINT', FAINT, .true. )				!Look at companions to stars

      call get1b ( 'SMEAR', SMEAR, .true. )				!do de-smearing?

      NXSKY = 1								!Get number and size of sky
      NYSKY = 1								! estimating boxes
      call get_option ( 'SKYTYPE', 'local:mean:minimum', 1, ktopt,
     +                  'local', ' ', 0 )
      DOSKY = 1
      if ( ktopt.eq.'mean' ) DOSKY = 2
      if ( ktopt.eq.'minimum' ) DOSKY = 3
      if ( DOSKY.EQ.1 ) then
         NXSD = min(64,(NX/2))
         call get1i ( 'XSKYBOX', NXSD, NXSD, 10, NX/2 )
         NYSD = min(64,(NY/2))
         call get1i ( 'YSKYBOX', NYSD, NYSD, 10, NY/2 )
         NXSKY = NX/NXSD						!Size of image holding estimates of mean
         NYSKY = NY/NYSD						! and std dev of sky values
      endif								! in the boxes

      call get1i ( 'MAXNUM', NUMTOT, 10000, 1, 1000000 )		!Get max poss no of stars (hidden from user )

      call get_option ( 'INFORM', 'none:some:lots', 1, ktopt, 'some',
     +                  ' ', 0 )
      INFORM = 0
      if ( ktopt.eq.'some' ) INFORM = 1
      if ( ktopt.eq.'lots' ) INFORM = 2

      call get1b ( 'USAVE', USAVE, .false. )				!Save U-test image?
      if ( USAVE ) then
         call opimsw ( 'OUTIM', IPOUTU, NX, NY, .true., istat )
         if ( istat.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
         call icopdes ( 'IN', 'OUTIM' , istat )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_GWSA -- Open work space for tables
C
C   a j penny                 dao           1988-04-25

      subroutine fi_gwsa ( )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return						!Check failure

      call gtwrkr ( 'WXY', NUMXY*NUMTOT, IPXY, istat )			!Open space
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WXP', NUMTOT, IPXP, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WYP', NUMTOT, IPYP, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WZP', NUMTOT, IPZP, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WWP', NUMTOT, IPWP, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WVP', NUMTOT, IPVP, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if (.not.ST_FAILED) call gtwrkr ( 'WFP', NELLI*NUMTOT, IPFP,istat)
      if ( istat.ne.0 ) ST_FAILED = .true.

      if ( ST_FAILED ) then
          call printo ( 'ERROR: Cannot open enough storage work space' )
      else
         if ( INFORM.gt.0 ) call printo ( 'Opened storage work spaces' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_GWSB -- Open work space for images
C
C    a j penny                      dao              1988-05-02

      subroutine fi_gwsb ( )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return						!Check failure

      call gtwrkr ( 'WIN', NX*NY, IPIMU, istat )			!Open work spaces
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrks ( 'WB', NX*NY, IPWB, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrki ( 'WC', NX*NY, IPWC, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'WD', NX*NY, IPWD, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'WE', NX*NY, IPWE, istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'WF', NXSKY*NYSKY, IPWF,istat)
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( .not.ST_FAILED ) call gtwrkr ( 'WG', NXSKY*NYSKY, IPWG,istat)
      if ( istat.ne.0 ) ST_FAILED = .true.

      if  ( ST_FAILED ) then
          call printo ( 'ERROR: Cannot open enough work space' )
      else
         if ( INFORM.gt.0 ) call printo ( 'Opened work spaces' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_ARRSET -- Load BS and BZ into array and remove minimum
C
C       a j penny                  ral          1994 May

      subroutine fi_arrset ( ima )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real  ima(NX*NY)		!i/o: Array
C--
      integer j
      real rmin
      logical some
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      some = .false.							!Check for not all INVALID
      j = 0
      do while ( .not.some .and. j.lt.NX*NY )
         j = j + 1
         if ( ima(j).ne.RINVAL ) some = .true.
      enddo
      if ( .not.some ) then
         call printo ( 'ERROR:  No valid pixels' )
         ST_FAILED = .true.
         return
      endif

      do j = 1, NX*NY							!Descale
         if ( ima(j).ne.RINVAL ) ima(j) = BS*ima(j) + BZ
      enddo

      rmin = 1.0e12							!Subtract minimum
      do j = 1, NX*NY
         if ( ima(j).ne.RINVAL ) rmin = min(ima(j),rmin)
      enddo
      do j = 1, NX*NY
         if ( ima(j).ne.RINVAL ) ima(j) = ima(j) - rmin
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_STATIM -- Make Mann_Whitney probability image
C
C    This is an image, the same size as the input image, where each
C    pixel value has a probability that a box around that pixel
C    has a median significantly higher than that in an annulus around
C    it.
C
C    A box of size NBOX is compared (one-tailed Mann-Whitney) with a
C    square annulus starting one pixel away from box, and width to give
C    roughly equal no of pixels. Also with an annulus immeadiately
C    adjacent to the box. The comparision that gives the most striking
C    discord is taken, unless either has a prob of less than 0.025 when
C    that level is taken.
C
C    Optionally, a further test against the areas in strips:-
C        1) above and below the star
C        2) to the left and right of the star
C    is made.
C
C    Output pixel has probability (*100 000, limited to 10 000) that
C    inside box median is NOT higher than outside median. Thus 0 means
C    almost certainly a bump, 10000 means only 90% chance that it is a
C    real bump.
C
C   a j penny               ral                    1988-07-06

      subroutine fi_statim ( imu, map, kopt )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real      imu(NX,NY)	!i: Input image (unINVAILDed)
      integer*2	map(NX,NY)	!o: Output Mann-Whitney image
      integer   kopt		!i: 1=test only vs annulli; 2=also against strips
C--
      integer nbox, j, ii, jj, ii1, ii2, jj1, jj2, nbox2, idelta,
     +        igap, icol, kl, npts, npts1, npts2, npts3, npts4,
     +        is, ie, js, je, igs, ige, jgs, jge, irs, ire, jrs, jre,
     +        ics, ice, jcs, jce, ist, iv, i
      real usig, delta, usig1, usig2, usig3, usig4
      logical tbig1, tbig2, tbig3, tbig4
      real x(600), xcom1(600), xcom2(600), xcom3(600), xcom4(600)
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( INFORM.gt.0 ) call printo ( 'Started making U test image' )

      if ( RADIUS.lt.1.75 ) then					!Calculate box size
         nbox = 3
      elseif ( RADIUS.lt.2.5 ) then
         nbox = 5
      elseif ( RADIUS.lt.5.5 ) then
         nbox = 7
      else
         nbox = 1 + 2*int(1.0+(RADIUS-1.5)/2.0)
      endif
      nbox = min(20,nbox)

      nbox2 = nbox/2							!Get border round box
      delta = (1.4142-1)*(float(nbox)/2.0)
      idelta = delta+1
      if ( nbox.eq.3 ) idelta = 1
      if ( nbox.eq.5 ) idelta = 1
      if ( nbox.eq.7 ) idelta = 2
      igap = 1
      icol = nbox/2

      do j = 1, NY							!Do the calcs
         do i = 1, NX

            is = i - nbox2						!Load arrays at this pixel
            ie = i + nbox2
            js = j - nbox2
            je = j + nbox2
            igs = i - nbox2 - igap
            ige = i + nbox2 + igap
            jgs = j - nbox2 - igap
            jge = j + nbox2 + igap
            irs = is - idelta
            ire = ie + idelta
            jrs = js - idelta
            jre = je + idelta
            ics = is - icol
            ice = ie + icol
            jcs = js - icol
            jce = je + icol
            jj1 = max(1,j-nbox2-idelta-igap)
            jj2 = min(ny,j+nbox2+idelta+igap)
            ii1 = max(1,i-nbox2-idelta-igap)
            ii2 = min(nx,i+nbox2+idelta+igap)
            npts = 0
            npts1 = 0
            npts2 = 0
            npts3 = 0
            npts4 = 0
            do jj = jj1, jj2
               do ii = ii1, ii2
                  if ( jj.ge.j-nbox2.and.jj.le.j+nbox2.and.
     +                 ii.ge.i-nbox2.and.ii.le.i+nbox2)then
                     npts = npts+1
                     x(npts) = imu(ii,jj)
                  else
                     if ( ii.le.igs .or. ii.ge.ige .or. jj.le.jgs
     +                    .or. jj.ge.jge ) then
                        npts1 = npts1 + 1
                        xcom1(npts1) = imu(ii,jj)
                     endif
                     if ( ii.le.ire .or. ii.ge.irs .or. jj.le.jre
     +                    .or. jj.ge.jrs ) then
                        npts2 = npts2 + 1
                        xcom2(npts2) = imu(ii,jj)
                     endif
                     if ( kopt.eq.2 ) then
                        if ( ii.ge.is .and. ii.le.ie .and. jj.ge.jcs
     +                       .and. jj.le.jce ) then
                           npts3 = npts3 + 1
                           xcom3(npts3) = imu(ii,jj)
                        endif
                        if ( jj.ge.js .and. jj.le.je .and. ii.ge.ics
     +                       .and. ii.le.ice ) then
                           npts4 = npts4 + 1
                           xcom4(npts4) = imu(ii,jj)
                        endif
                     endif
                  endif
               enddo
            enddo

            usig = 1.0							!Calc prob of bump
            call mannwhitr ( xcom1, npts1, x, npts, usig1, tbig1, ist )
            if ( tbig1 ) usig = usig1
            if ( usig.gt.0.025 ) then
               call mannwhitr ( xcom2, npts2, x, npts, usig2, tbig2,
     +                          ist )
               if ( tbig2 ) usig = min(usig,usig2)
            endif
            if ( kopt.eq.2 .and. usig.gt.0.025 ) then
               call mannwhitr ( xcom3, npts3, x, npts, usig3, tbig3,
     +                          ist )
               if ( tbig3 ) usig = min(usig,usig3)
            endif
            if ( kopt.eq.2 .and. usig.gt.0.025 ) then
               call mannwhitr ( xcom4, npts4, x, npts, usig4, tbig4,
     +                          ist )
               if ( tbig4 ) usig = min(usig,usig4)
            endif

            map(i,j) = min(10000,int(100000.0*usig))			!Load output

         enddo

         kl = NY/10
         if ( INFORM.gt.1 .and. ((j/kl)*kl).eq.j ) then
            call pargi ( j )
            call printd ( ' Made Utest image line no %d' )
         endif

      enddo

      if ( USAVE ) then							!Save U-test image?
         call amovs ( map, %val(IPOUTU), NX*NY )
         call ptdesc ( 'OUTIM', 'TITLE', 'U-test image' )
         iv = INT_INVALSI
         call ptdesi ( 'OUTIM', 'INVAL', iv )
         call ptdesr ( 'OUTIM', 'BSCALE', 1.0 )
         call ptdesr ( 'OUTIM', 'BZERO', 0.0 )
         call wrkcan ( 'OUTIM' )
      endif

      if ( INFORM.gt.0 ) call printo ( 'Made the U test image' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_SKY -- Make the sky image
C  DOSKY=1 => sky = sky from box areas, each with own mean sky
C  DOSKY=2 => sky = mean sky
C  DOSKY=3 => sky = minimum pixel
C
C   a j penny                 ral          1991 Jan

      subroutine fi_sky ( im, msky, asky )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real          im(NX,NY)		!i: Input image
      real          msky(NX,NY)		!o: Sky image
      real          asky(NX,NY)		!o: (Sky+std dev) image
C--
      real  skylev, skystd, vmin
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      if ( INFORM.gt.0 ) call printo ( 'Started making sky image' )

      if ( DOSKY.eq.1 ) then
         call sky_1r ( im, NX, NY, RINVAL, msky, asky, NXSKY, NYSKY,
     +                 %val(IPWF), %val(IPWG), NXSD, NYSD, ST_FAILED )
      elseif ( DOSKY.eq.2 ) then
         print*,'B1  ', INVAL, RINVAL
         call sky_0 ( im, 'REAL', NX, NY, INVAL, RINVAL, skylev, skystd)
         vmin = skylev
         call amovkr ( vmin, msky, NX*NY )
         vmin = skylev + skystd
         call amovkr ( vmin, asky, NX*NY )
      else
         vmin = INT_MAXRR
         do k = 1, NY
            do j = 1, NX
               if ( im(j,k).ne.RINVAL ) vmin = min(vmin,im(j,k))
            enddo
         enddo
         call amovkr ( vmin, msky, NX*NY )
         print*,'B2  ', INVAL, RINVAL
         call sky_0 ( im, 'REAL', NX, NY, INVAL, RINVAL, skylev, skystd)
         vmin = vmin + skystd
         call amovkr ( vmin, asky, NX*NY )
      endif

      if ( INFORM.gt.0 ) call printo ( 'Made the sky image' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_FINDP -- Find the peaks in a confidence map
C
C     a j penny                stsci                  86-10-27

      subroutine fi_findp ( imu, map, wc, xp, yp, fp, msky, asky )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      imu(NX,NY)		!i: Un-invalid image
      integer*2	map(NX,NY)		!i: U-test image
      integer	wc(NX,NY)		!o: peak locations image
      real	xp(NUMTOT)		!o: X posns
      real	yp(NUMTOT)		!o: Y posns
      real	fp(NELLI,NUMTOT)	!o: Elliptical stars info
      real      msky(NX,NY)		!i: (sky) image
      real      asky(NX,NY)		!i: (sky+std) image
C--
      integer lev, ktop, kbot, kstep, kklev, klim, kj
      logical new, doit
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( INFORM.gt.0 ) call printo ( 'Started finding main stars' )	!Inform flag

      if ( INFORM.gt.1 ) call printo ( '  Star       X       Y    '//	!Detailed stars output header
     +                                 'Height   U-map  Type  Elli' )

      call azeroi ( wc, NX*NY )						!Zero pixel flag image

      NUMF = 0 								!Zero - the number of found stars
      call azeror ( xp, NUMTOT )					! - the star x,y positions
      call azeror ( yp, NUMTOT )
      call azeror ( fp, NUMTOT*NELLI )					! - the ellipse markers

      SMALIM = 0.67*RADIUS						!Set elliptical detection limits
      ELLIPLIM = 0.35

      ktop = 0								!Set loop limits
      kbot = nint(100000.0*PLIMIT)
      kstep = 5
      kklev = nint(100000.0*WLIMIT)

      call fi_lcheck ( map, kbot, ktop, kstep )				!See which levels present

      klim = min(10001,(1+((kbot-ktop)/kstep)))
      do lev = ktop, kbot, kstep					!Loop through, lowering the threshold each time

        if ( NUMF.lt.NUMTOT ) then

            kj = 1 + (lev-ktop+kstep-1)/kstep				!See if new level pixels to look at
            doit = .true.
            if ( kj.ge.1 .and. kj.le.klim ) then
               if ( LCHECK(kj).eq.0 ) doit = .false.
            endif
            if ( doit ) then
               call fi_flag ( imu, map, wc, asky, NX, NY, lev, kklev,	!Look for points not previously flagged
     +                        new, INFORM )				! as now above level threshold
               if ( new ) then
                  call fi_star ( imu, map, wc, xp, yp, msky, asky,	!Calculate mean position of all isolated
     +                           lev, kklev )				! new groups (all '1' points) in 'peak locataion'
									! image and set as old ('1')
                  call fi_ellipse ( imu, map, wc, xp, yp, fp, msky ) 	!Search for elliptical images
               endif
            endif

	 endif

      enddo

      if ( INFORM.gt.0 ) then						!Inform
         if ( ELLIP ) then
            call printo ( 'Done finding main and ellipse stars' )
         else
            call printo ( 'Done finding main stars' )
         endif
      endif

      call fi_nipple ( map, imu, wc, xp, yp, asky, msky )		!Search for long extensions to images


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_LCHECK -- See what levels are present
C
C   a j penny                     RAL            1991 Feb

      subroutine fi_lcheck ( map, kbot, ktop, kstep )

      implicit none
      include 'starfind.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2 map(NX,NY)	!i: U-test image
      integer   kbot		!i: Bottom of used levels (0=high,10000=low)
      integer   ktop		!i: Top of used levels (0=high,10000=low)
      integer   kstep		!i: Step in used levels
C--
      integer j, k, lev, jj, klim
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      call azeroi ( LCHECK, 10001 )
      klim = min(10001,(1+((kbot-ktop)/kstep)))
      do k = 1, NY
         do j = 1, NX
            lev = map(j,k)
            jj = 1 + (lev-ktop+kstep-1)/kstep
            if ( jj.ge.1 .and. jj.le.klim ) LCHECK(jj) = 1
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_FLAG -- Flag all pixels above threshold not already marked
C   Put 1 or 2 in them (1 if isolated pixel or set of pixels,
C   2 if touching another previously flagged pixel).
C
C   Any pixel flags set at 1 on entry, which are touching new
C   flagged pixels, are set at 2
C
C   There are two cases for the threshold level use:-
C    (case 1) when it is above a set limit, when the pixel
C             flag is set if the probability image is above the
C             the probability threshold.
C    (case 2) when it is below that limit, when a pixel is only
C             flagged if it is not only above the threshold (as
C             for case 1) but also the image pixel value is some
C             distance above the sky level.
C
C   a j penny                     dao                 1988-05-02

      subroutine fi_flag ( imu, map, wc, asky, nx, ny, lev, kklev,
     +                     new, inform )

      implicit none
      include 'STARMAN_INC'

      integer   nx			!i: X size of image
      integer   ny			!i: Y size of image
      real      imu(nx,ny)		!i: Image (INVALIDs replaced)
      integer*2 map(nx,ny)		!i: U-test image
      integer   wc(nx,ny)		!i/o: Peak locations image
      real      asky(nx,ny)		!i: (sky+std) image
      integer   lev			!i: Probability level being used
      integer   kklev			!i: Threshold case 1/case 2 divide level
      logical   new			!o: Flag for new isolated pixels found (true/false)
      integer   inform			!i: Print out progess remarks? 0=no;1=yes;2=full
C--
      integer j, k, jj, kk, knum
      logical tnew, done
Cbegin


      if ( ST_FAILED ) return 						!Check failure

      knum = 0								!Look for points not previously
      if ( lev.le.kklev ) then						! flagged as now above level threshold
         do k = 1, ny
            do j = 1, nx
               if ( map(j,k).le.lev .and. wc(j,k).eq.0 ) then
                  wc(j,k) = 1
                  knum = knum + 1
               endif
            enddo
         enddo
      else
         do k = 1, ny
            do j = 1, nx
               if ( map(j,k).le.lev .and. wc(j,k).eq.0 ) then
                  if ( imu(j,k).gt.asky(j,k) ) then
                     wc(j,k) = 1
                     knum = knum + 1
                  endif
               endif
            enddo
         enddo
      endif

      if ( knum.eq.0 ) then						!Flag as old (with a 2) all
         new = .false.							! new points touching old
      else								! points. Flag as 1, any new
         new = .false.							! isolated ones
         done = .false.
         do while ( .not.done )
            done = .true.
            do k = 1, ny
               do j = 1, nx
                  if ( wc(j,k).eq.1 ) then
                     tnew = .true.
                     do kk = max(1,k-1),min(ny,k+1)
                        do jj = max(1,j-1),min(nx,j+1)
                           if ( (jj.ne.j.or.kk.ne.k) .and.
     +                          wc(jj,kk).eq.2 ) then
                              wc(j,k) = 2
                              done = .false.
                              tnew = .false.
                           endif
                        enddo
                     enddo
                     if ( tnew ) new = .true.
                  endif
               enddo
            enddo
         enddo

      endif

      if ( inform.gt.1 ) then
         call pargi ( lev )
         if ( new ) then
            call printd ( ' Level %d flagged - new isolated pixels '//
     +                    'marked' )
         else
            call printd ( ' Level %d flagged - no new isolated '//
     +                    'pixels marked' )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C FI_STAR -- Calc position of all new isolated groups
C  Calculate the mean position of all isolated new groups, that is
C  all groups of '1' pixels in the 'peak location' image. Then put
C  these pixels to 'old', that is to '3'.
C
C  Only 'significant' groups are stored.
C
C    alan penny                    ral              1990 dec

      subroutine fi_star ( imu, map, wc, xp, yp, msky, asky,
     +                     lev, kklev )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      imu(NX,NY)		!i: Image (un INVALID ed)
      integer*2 map(NX,NY)		!i: U-test image
      integer   wc(NX,NY)		!i/o: Peak location image
      real      xp(NUMTOT)		! X posns
      real      yp(NUMTOT)		! Y posns
      real      msky(NX,NY)		!i: (sky) image
      real      asky(NX,NY)		!i: (sky+std) image
      integer   lev			!i: Present test level of U-test image
      integer   kklev			!i: Level of U-test image below which the level is very significant
C--
      integer j, k, jj, kk, kl, nxy, jb, kb, kxp, kyp, jba, jbb, kba,
     +        kbb, numold, ierr, jbc, jbd, kbd
      real px, py, pnum, axp, ayp
      logical more, amore, bmore, got
Cbegin


      if ( ST_FAILED ) return 						!Check failure

      numold = NUMF							!Note number of stars

      got = .false.							!Look for all areas
      more = .true.
      do while ( more )

         kk = 0								!Find 1st such one
         kl = 1
         nxy = nx*ny
         do while ( kl.eq.1 .and. kk.lt.nxy )
            kk = kk + 1
            kb = 1 + (kk-1)/nx
            jb = kk - (kb-1)*nx
            if ( wc(jb,kb).eq.1 ) kl = 0
         enddo
         if ( kl.eq.1 ) then
            more = .false.
         else

            wc(jb,kb) = 3						!Flag with a 3 all points
            kba = kb							! in contact with that one
            kbb = kb
            jba = jb
            jbb = jb
            jbc = max(1,(jb-1))
            jbd = min(nx,(jb+1))
            kbd = min(ny,(kb+1))
            amore = .true.
            do while ( amore )
               amore = .false.
               bmore = .true.
               k = kb - 1
               do while ( bmore .and. k.lt.kbd )
                  k = k + 1
                  bmore = .false.
                  do j = jbc, jbd
                     if ( wc(j,k).eq.3 ) bmore = .true.
                     if ( wc(j,k).eq.1 ) then
                        do kk = max(1,k-1),min(ny,k+1)
                           do jj = max(1,j-1),min(nx,j+1)
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
                  jbc = max(1,(jba-1),(jb-50))
                  jbd = min(nx,(jbb+1),(jb+50))
               enddo
               kbd = min(ny,(kbd+1),(kb+100))
            enddo


            px = 0.0							!Find mean psoition
            py = 0.0
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
            axp = px/pnum
            ayp = py/pnum
            kxp = axp
            kyp = ayp

            if ( (lev.le.kklev). or.					!Store if significant
     +           ( (imu(kxp,kyp)-msky(kxp,kyp)).gt.
     +           (1.5*(asky(kxp,kyp)-msky(kxp,kyp))) ) )then
                 got = .true.
                 call fi_tstore ( axp, ayp, 0, imu, map, msky,
     +                %val(IPXP), %val(IPYP),
     +                %val(IPWP), %val(IPZP), %val(IPVP), ierr )
                if ( ierr.ne.0 ) return
            endif

         endif

      enddo

      if ( INFORM.gt.0 ) then
         if ( got ) then
            k = NUMF - numold
            call pargi ( k )
            call pargi ( lev )
            call pargi ( NUMF )
            if ( k.eq.1 ) then
               call printd (
     +   '    - %d new star found at level %d - total %d' )
            else
               call printd (
     +   '    - %d new stars found at level %d - total %d' )
            endif
         else
            call pargi ( lev )
            call pargi ( NUMF )
            call printd (
     +   '    - no new stars found at level %d - total %d' )
         endif
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C FI_TSTORE -- Put result into local store and report
C
C    alan penny                    ral              1990 dec

      subroutine fi_tstore ( axp, ayp, kopt, imu, map, msky, xp, yp,
     +                       wp, zp, vp, ierr )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      axp			!i: X position of star
      real      ayp			!i: Y position of star
      integer   kopt			!i: 1=ordinary star; 2=nipple star
      real      imu(NX,NY)		!i: Image (un INVALID ed)
      integer*2 map(NX,NY)		!i: U-test image
      real      msky(NX,NY)		!i: (sky) image
      real      xp(NUMTOT)		!i/o: X posns results
      real      yp(NUMTOT)		!i/o: Y posns results
      real      zp(NUMTOT)		!i/o: U maps  results
      real      wp(NUMTOT)		!i/o: Fluxs results
      real      vp(NUMTOT)		!i/o: Types results
      integer   ierr			!o: Error (0=ok)
C--
      integer kxp, kyp
      real rv
      character text*72
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      ierr = 0
      NUMF = NUMF + 1
      if ( NUMF.gt.NUMTOT ) then
         NUMF = NUMTOT
         call printo ( 'Max number of stars found'//
     +                 '- BEWARE VALIDITY OF OUTPUT' )
         ierr = 1
         return
      endif

      kxp = int(axp)
      kyp = int(ayp)
      xp(NUMF) = axp
      yp(NUMF) = ayp
      zp(NUMF) = map(kxp,kyp)
      wp(NUMF) = imu(kxp,kyp)
      vp(NUMF) = real(kopt)

      if ( INFORM.gt.1 ) then
         if ( NUMF.eq.(20*(NUMF/20)) ) call printo (
     +                                 '  Star       X       Y    '//
     +                                 '  Height   U-map  Type Elli' )
         rv = imu(kxp,kyp) - msky(kxp,kyp)
         write ( text,
     +           '(1x,i5,2x,2f8.2,2x,2f8.1,3x,i1,4x,''0'')' )
     +           NUMF, xp(NUMF), yp(NUMF), rv, zp(NUMF),  kopt
         call printo ( text )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_ELLIPSE -- Find elliptical images
C  This looks at stars and sees if the U-test image shape of the
C  star is elliptical. For a star to qualify as elliptical, the
C  group of flagged pixels that make up the star (that is the number
C  of contiguous pixels flagged as being below the present
C  significance level) have to satisfy certain criteria:-
C
C     - the ellipticity has to be greater than a given limit
C     - the semi-major axis has to be larger than a given limit
C         (in terms of the star image profile radius)
C     - the area of the star has to be more than seven pixels
C     - the area of the star must not touch the image edge
C     - the area of the star does not contain other stars
C
C  This s/r is only called when new stars are found in the main
C  program. On these occasions, all the stars are checked for
C  ellipticity. Now some of these will have already been found
C  to be elliptical when working at a higher contour level
C  significance. Such stars are rechecked regardless, as the lower
C  significance, larger, image may show up a better ellipticity. This
C  rechecking of 'existing' doubles is however only done five times.
C
C  The output of the s/r is into an array 'fp', which is in parallel
C  to the main results arrays of 'xp', 'yp', 'zp', 'vp'
C
C   a j penny                 stsci             86-12-03

      subroutine fi_ellipse ( imu, map, wc, xp, yp, fp, msky )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      imu(NX,NY)		!i: Image (UnINVALIDed)
      integer*2	map(NX,NY)		!i: U-test image
      integer   wc(NX,NY)		!i/o: Peak locations image
      real	xp(NUMTOT)		!i/o: X posns results
      real	yp(NUMTOT)		!i/o: Y posns results
      real	fp(NELLI,NUMTOT)	!i/o: Elliptical stars info results
      real      msky(NX,NY)		!i: (sky) image - only used when 'INFORM'
C--
      integer j, k, kk, kb, jb, jj, km, kma, jbo, kbo, kbt,
     +        nn, numap, jdx, jdy, kba, jba, jbt, nsixr,
     +        js, je, ks, ke, numnew, numold
      real px, py, pxx, pyy, pxy, pnum, axp, ayp, axx, ayy, axy,
     +     rr, sma, ell, xr, yr
      logical amore, got
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( .not.ELLIP .or. NUMF.eq.0 ) return				!Should do?

      numnew = 0							!New star counter
      numold = 0							!Old star counter

      do kk = 1, NUMF							!Mark posns of all
         j = max(1,min(NX,nint(xp(kk))))				! found stars
         k = max(1,min(NY,nint(yp(kk))))
         if ( wc(j,k).ne.0 ) wc(j,k) = 4
      enddo

      got = .false.							!Search all stars
      do nn = 1, NUMF
         if ( fp(10,nn).lt.4.5 ) then					!Only do if not done at least 5
									! times before and not (=6) at edge

            nsixr = 6.0*RADIUS						!Flag with a 3 all points
            kma = 0							! in contact with that one
            kb = NY							!Search area within +/- 6
            kba = 1							! star radius
            jb = NX
            jba = 1
            jbo = max(1,min(NX,nint(xp(nn))))
            kbo = max(1,min(NY,nint(yp(nn))))
            wc(jbo,kbo) = 3
            js = max(1,(jbo-nsixr))
            je = min(NX,(jbo+nsixr))
            ks = max(1,(kbo-nsixr))
            ke = min(NY,(kbo+nsixr))
            amore = .true.
            do while ( amore )
               amore = .false.						!For each pixel in search area
               do k = ks, ke						! see if marked as star
                  do j = js, je						! then mark all contiguous pixels
                     if ( wc(j,k).eq.2 .or. wc(j,k).eq.4 ) then		! to that pixel
                        do kk = max(1,k-1), min(NY,k+1)
                           do jj = max(1,j-1), min(NX,j+1)
                              if ( (jj.ne.j.or.kk.ne.k) .and.
     +                           wc(jj,kk).eq.3 ) then

                                 if ( wc(j,k).eq.4 ) then		!Image contains another star
                                    fp(10,nn) = 6.0			! so quit, note location of
                                    do km = 1, NUMF			! other star. get its number
                                       jbt = max(1,min(nx,nint(xp(km))))
                                       kbt = max(1,min(ny,nint(yp(km))))
                                       if( j.eq.jbt .and. k.eq.kbt )then
                                          kma = km
                                          jdx = j
                                          jdy = k
                                       endif
                                    enddo
                                    goto 1
                                 endif

                                 if ( j.eq.1 .or. j.eq.nx .or. k.eq.1.	!Quit if reached edge
     +                                .or. k.eq.ny ) then		! of image. Note
                                    fp(10,nn) = 6.0
                                    goto 1
                                 endif

                                 wc(j,k) = 3				!If still contiguous,
                                 kb = min(k,kb)				! note edge of star
                                 kba = max(k,kba)			! and mark with a 3
                                 jb = min(j,jb)
                                 jba = max(j,jba)
                                 amore = .true.

                              endif
                           enddo
                        enddo
                     endif
                  enddo
               enddo
            enddo
    1       continue

            if ( fp(10,nn).lt.4.5 ) then				!Only do if not done at least 5
									! times before and not (=6) at edge

               numap = 0						!Is star image larger than
               do k = kb, kba						! last time and larger than 7 ?
                  do j = jb, jba
                     if ( wc(j,k).eq.3 ) numap = numap + 1
                  enddo
               enddo
               if ( numap.ne.nint(fp(11,nn)) .and. numap.gt.7 ) then

                  px = 0.0						!Calculate ellipticity and
                  py = 0.0						! semi-major axis by looking
                  pxx = 0.0						! in determined rectangle
                  pyy = 0.0						! and noting all '3' pixels
                  pxy = 0.0
                  do k = kb, kba
                     do j = jb, jba
                        if ( wc(j,k).eq.3 ) then
                           xr = j - jb
                           yr = k - kb
                           px = px + xr
                           py = py + yr
                           pxx = pxx + xr*xr
                           pyy = pyy + yr*yr
                           pxy = pxy + xr*yr
                        endif
                     enddo
                  enddo
                  pnum = real(numap)
                  axp = px/pnum
                  ayp = py/pnum
                  axx = (pxx - pnum*axp*axp)
                  ayy = (pyy - pnum*ayp*ayp)
                  axy = (pxy - pnum*axp*ayp)
                  rr = max(0.0001,(axx+ayy))
                  ell = sqrt((axx-ayy)*(axx-ayy)+4.0*axy*axy)/rr
                  if ( ell.ge.ELLIPLIM ) sma = sqrt(rr/pnum)

                  if ( ell.ge.ELLIPLIM .and. sma.ge.SMALIM ) then	!If really elliptical
                     got = .true.					! note, store result in
                     if ( fp(10,nn).lt.0.5 ) then			! 'fp', output to terminal
                        numnew = numnew + 1
                     else
                        numold = numold + 1
                     endif
                     call fi_ellnote ( ell, sma, rr, axy, ayy, axp,
     +                                 ayp, jb, kb, numap, nn, fp,
     +                                 imu, map, msky )
                  endif

               endif

            endif

            do k = kb, kba
               do j = jb, jba
                  if ( wc(j,k).eq.3 ) wc(j,k) = 2
               enddo
            enddo

            wc(jbo,kbo) = 4						!If found another star
            if ( kma.ne.0 ) then					! in area, note for other
               fp(10,kma) = 6.0						! that it in turn has a star
               wc(jdx,jdy) = 4						! in it
            endif

         endif
      enddo

      do k = 1, NY							!Set back to 2's
         do j = 1, NX
            if ( wc(j,k).ne.0 ) wc(j,k) = 2
         enddo
      enddo

      if ( INFORM.gt.0 ) then						!Inform progress
         if ( got ) then
            call pargi ( numnew )
            if ( numnew.ne.1 ) call pargc ( 's' )
            if ( numnew.eq.1 ) call pargc ( ' ' )
            call pargi ( numold )
            if ( numold.ne.1 ) call pargc ( 's' )
            if ( numold.eq.1 ) call pargc ( ' ' )
            call printd ( '      - ellipse doubles  - %d new '//
     +                    'double%c  -  %d existing double%c' )
         else
            call printd ( '      - ellipse doubles  - no new stars ' )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_ELLNOTE -- Note an elliptical star
C  Calculate positions of components and store all details in 'fp'
C
C    a j penny                       ral           1991 Jan

      subroutine fi_ellnote ( ell, sma, rr, axy, ayy, axp, ayp,
     +                        jb, kb, numap, nn, fp, imu, map, msky )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      ell		!i: Ellipticity of star
      real      sma		!i: Semi-major axis of star
      real      rr		!i:
      real      axy		!i:
      real      ayy		!i:
      real      axp		!i:
      real      ayp		!i:
      integer   jb		!i:
      integer   kb		!i:
      integer   numap		!i: No of pixels in image
      integer   nn		!i: Star working on
      real      fp(NELLI,NUMTOT)!i/o: Star ellipticity results table
      real      imu(NX,NY)	!i: Image (UnINVALIDed)
      integer*2	map(NX,NY)	!i: U-test image
      real      msky(NX,NY)	!i: (sky) image
C--
      real temp, dx, dy, fact, bxp, byp, aval, bval
      integer kxp, kyp
      character text*72
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      temp = 0.5*(1.0+ell)*rr-ayy
      if ( temp.eq.0.0 ) then
         dx = 0.0
         dy = sma
      elseif ( axy.eq.0.0 ) then
         dx = sma
         dy = 0.0
      else
         temp = axy/temp
         fact = sign(1.0,temp)
         dx = 0.8*sma/sqrt(1.0+temp*temp)
         dy = 0.8*fact*sma/sqrt(1.0+1.0/(temp*temp))
      endif

      bxp = axp - dx + real(jb)
      bxp = max(1.0,min(real(NX),bxp))
      byp = ayp - dy + real(kb)
      byp = max(1.0,min(real(NY),byp))
      kxp = bxp
      kyp = byp
      fp(1,nn) = 1.0
      fp(2,nn) = bxp
      fp(3,nn) = byp
      fp(4,nn) = map(kxp,kyp)
      fp(5,nn) = imu(kxp,kyp)

      if ( INFORM.gt.1 ) then
         aval = imu(kxp,kyp) - msky(kxp,kyp)
         bval = map(kxp,kyp)
         write ( text, '(1x,i5,2x,2f8.2,2x,2f8.1,3x,''3'',i5)' )
     +                   nn, bxp, byp, aval, bval, nn
         call printo ( text )
      endif

      bxp = axp + dx + real(jb)
      bxp = max(1.0,min(real(NX),bxp))
      byp = ayp + dy + real(kb)
      byp = max(1.0,min(real(NY),byp))
      fp(6,nn) = bxp
      fp(7,nn) = byp
      kxp = nint(bxp)
      kyp = nint(byp)
      fp(8,nn) = map(kxp,kyp)
      fp(9,nn) = imu(kxp,kyp)
      fp(10,nn) = fp(10,nn) + 1.0

      if ( INFORM.gt.1 ) then
         aval = imu(kxp,kyp) - msky(kxp,kyp)
         bval = map(kxp,kyp)
         write ( text, '(1x,i5,2x,2f8.2,2x,2f8.1,3x,''3'',i5)' )
     +                   nn, bxp, byp, aval, bval, nn
         call printo ( text )
      endif

      fp(11,nn) = real(numap)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_NIPPLE -- Find faint companions to bright stars
C  Look for stars which do not show up as isolated peaks in U-test image, by
C  looking for long projections at the edges of stars in the U-test image.
C  Only look at edges of very certain (v low U-test value) stars or stars
C  which are reasonably certain and also bright.
C  (Takes a 100x100 box round each of these stars)
C
C    a j penny                       stsci            86-12-02

      subroutine fi_nipple ( map, imu, wc, xp, yp, msky, asky )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2 map(NX,NY)		!i: U test image
      real      imu(NX,NY)		!i: Image (un INVALID ed)
      integer   wc(NX,NY)		!o: Work space for area round each star
      real	xp(NUMTOT)		!i/o: X posns
      real	yp(NUMTOT)		!i/o: Y posns
      real      msky(NX,NY)		!i: (sky) image
      real      asky(NX,NY)		!i: (sky+std) image
C--
      integer   kleva			!Look at all stars with U-test values below this
      parameter ( kleva=1000)
      integer   klevb			!Look at all stars with U-test values below this
      parameter ( klevb=9500)		! as long as real star is above (sky+std) value

      integer j, k, kk, l, jj, jb, kb, numin, kxa, kxb,
     +        kya, kyb, nx1, nx2, ny1, ny2, ierr
      logical amore

      integer numinm
      parameter ( numinm=300 )
      real xpa(numinm), ypa(numinm)		!Locations of known stars and nipples round a star
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( .not.FAINT .or. NUMF.eq.0 .or. NUMF.ge.NUMTOT ) return	!Should do?

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +                   'Started looking for stars near big ones' )

      l = 0								!Do for each star in turn
      do while ( l.lt.NUMF )
         l = l + 1
         jb = xp(l)							!Only do if star definitely found
         kb = yp(l)							! or if probably found and is brightish
         if ( (map(jb,kb).le.kleva) .or.
     +      (map(jb,kb).le.klevb.and.imu(jb,kb).gt.asky(jb,kb)) ) then

            call azeroi ( wc, NX*NY )					!Clear work space

            wc(jb,kb) = 3						!Flag star pixel with a 3
            kxa = max(1,(jb-50))					!Work withion 50 pixels
            kxb = min(NX,(jb+50))
            kya = max(1,(kb-50))
            kyb = min(NY,(kb+50))

            amore = .true.						!Flag with a 3, all points
            do while ( amore )						! in contact with star
               amore = .false.
               do k = kya, kyb
                  do j = kxa, kxb
                     if (
     +                   (wc(j,k).ne.3)  .and.
     +                   ((map(j,k).le.kleva).or.
     +                   (map(j,k).le.klevb.and.imu(j,k).gt.asky(j,k)))
     +                                        )then
                        do kk = max(1,k-1),min(ny,k+1)
                           do jj = max(1,j-1),min(nx,j+1)
                              if ((jj.ne.j.or.kk.ne.k).and.
     +                           wc(jj,kk).eq.3) then
                                 wc(j,k) = 3
                                 amore = .true.
                              endif
                           enddo
                        enddo
                     endif
                  enddo
               enddo
            enddo

            nx1 = kxb							!Find limits of star
            nx2 = kxa							! inside the 100x100 box
            ny1 = kyb
            ny2 = kya
            do k = kya, kyb
               do j = kxa, kxb
                  if ( wc(j,k).eq.3 ) then
                     if ( j.lt.nx1 ) nx1 = j
                     if ( j.gt.nx2 ) nx2 = j
                     if ( k.lt.ny1 ) ny1 = k
                     if ( k.gt.ny2 ) ny2 = k
                  endif
               enddo
            enddo

            numin = 0							!Load stars marked already
            do j = 1, NUMF
               if ( xp(j).ge.nx1 .and. xp(j).le.nx2 .and.
     +              yp(j).ge.ny1 .and. yp(j).le.ny2 ) then
                  numin = min(numinm,(numin+1))
                  xpa(numin) = xp(j)
                  ypa(numin) = yp(j)
               endif
            enddo

            call fi_xnipple ( nx1, nx2, ny1, ny2, xpa, ypa, numinm,	!Find nipple points round this star
     +                        numin, wc, map, imu, msky, xp, yp, ierr )
            if ( ierr.ne.0 ) return

         endif

         if ( INFORM.gt.1 ) then
            call pargi ( l )
            call printd ( 'Looked at star %d for faint companions' )
         endif

      enddo

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +                        'Ended extra looking near big stars')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_XNIPPLE -- Find faint companions of a star
C  Take the 100x100 box in the U-map, and look at the marked image round star, that is
C  pixels marked by a '3', and not at edge of box.
C
C  Then look for long extensions at edge of marked area. These extensions
C  are sort of inverse shoulders. These have to be 4 pixels long. The
C  shoulder is traced along as it twists and turns.
C
C    a j penny                   ral             91-1-10

      subroutine fi_xnipple ( nx1, nx2, ny1, ny2, xpa, ypa, numpa,
     +                        numin, wc, map, imu, msky, xp, yp, ierr )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer   nx1		!i: X start of area to cover
      integer   nx2		!i: X   end of area to cover
      integer   ny1		!i: Y start of area to cover
      integer   ny2		!i: Y   end of area to cover
      integer   numpa		!i: max no of posns
      real      xpa(numpa)	!i/o: Star X posns
      real      ypa(numpa)	!i/o: Star Y posns
      integer   numin		!i/o: Number of stars
      integer   wc(NX,NY)	!i/o: Work space for area round each star
      integer*2 map(NX,NY)	!i: U test image
      real      imu(NX,NY)	!i: Image (un INVALID ed)
      real      msky(NX,NY)	!i: (sky) image
      real	xp(NUMTOT)	!i/o: X posns
      real	yp(NUMTOT)	!i/o: Y posns
      integer   ierr		!o: Error flag (0=ok)
C--
      integer j, k, jxn, jyn, jxl(2), jyl(2), jxd(2), jyd(2),
     +        kk, knum, ja, jb, ka, kb, kka, kkb, numpath
      real    aj, ak
      logical loop, iscol

      integer numinm
      parameter ( numinm=5 )
      real xpb(numinm), ypb(numinm)		!Locations of nipple ridge
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      ierr = 0

      do k = ny1, ny2							!Look at each 'good' pixel in area
         do j = nx1, nx2						! Does this pixel lead to the end of a nipple?
            if ( wc(j,k).eq.3 .and. j.ne.1 .and. j.ne.NX .and.
     +                k.ne.1 .and. k.ne.NY ) then

               knum = 0
               jxn = j
               jyn = k
               jxl(1) = 0
               jyl(1) = 0
               jxl(2) = 0
               jyl(2) = 0

               loop = .true.
               do while ( loop )

                  if ( loop ) then
                     do kk = 1, numin					!Near existing star?
                        if ( (abs(xpa(kk)-real(jxn)).le.2.0) .and.
     +                  (abs(ypa(kk)-real(jyn)).le.2.0) ) loop = .false.
                     enddo
                  endif

                  if ( map(jxn,jyn).le.500 ) loop = .false.		!Pixel not definitely a star

                  if ( loop ) then					!Along a nipple?
                     call fi_iscol ( jxn, jyn, wc, map, jxl, jyl, jxd,
     +                               jyd, numpath, iscol )
                     if ( .not.iscol ) loop = .false.
                  endif

                  if ( loop ) then					!If along a nipple

                     knum = knum + 1					!No of steps along nipple
                     xpb(knum) = jxn					!Note path posn
                     ypb(knum) = jyn
                     jxl(1) = jxn					!Old path posns for next loop
                     jyl(1) = jyn

                     if ( numpath.eq.1 ) then				!If one new pixel, then path is along it
                        jxn = jxd(1)
                        jyn = jyd(1)
                        jxl(2) = 0
                        jyl(2) = 0
                     endif

                     if ( numpath.eq.2 ) then				!If two new pixels,
                        ja = jxd(1)					! then choose as path,
                        ka = jyd(1)					! that with the lowest U-map value
                        jb = jxd(2)
                        kb = jyd(2)
                        if ( map(ja,ka).lt.map(jb,kb) ) then
                           jxn = ja
                           jyn = ka
                           jxl(2) = jb
                           jyl(2) = kb
                        else
                           jxn = jb
                           jyn = kb
                           jxl(2) = ja
                           jyl(2) = ka
                        endif
                     endif

                     if ( knum.eq.4 ) then				!If there are 4 pixels in path , then this is a nipple

                        kka = min(numinm,(numin+1))			!Load nipple path as 'done' pixels
                        kkb = min(numinm,(numin+4))			! so not redone
                        do kk = 1, 4
                           xpa(kk+kka-1) = xpb(kk)
                           ypa(kk+kka-1) = ypb(kk)
                        enddo
                        numin = min(numinm,(numin+4))

                        aj = real(j)					!Store start of nipple as star position
                        ak = real(k)
                        call fi_tstore ( aj, ak, 2, imu, map, msky,
     +                       %val(IPXP), %val(IPYP),
     +                       %val(IPWP), %val(IPZP), %val(IPVP), ierr )
                        if ( ierr.ne.0 ) return

                        loop = .false.					!Done for this pixel

                     endif

                  endif

               enddo

            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_ISCOL -- Is this pixel in a 'col'?
C  Take a 3x3 box round position (allow for image edge). See if it is
C  in a 'col', that is a negative shoulder. This is tested by looking
C  at the adjacent pixels. If they are lower and have not been flagged by
C  a previous pass through this s/r as being along the 'col' path that has
C  already been traced, then they are noted.
C
C  A 'col' will have either 1 or 2 such lower pixels. So if the output
C  'numpath' is 1 or 2, then the 'iscol' is set to true. 2 adjacent pixels
C  means that the 'col' is turning, and thus these 2 pixels have to
C  be adjacent.
C
C  a j penny                          stsci              1986 nov

      subroutine fi_iscol ( jxn, jyn, wc, map, jxl, jyl, jxd, jyd,
     +                      numpath, iscol )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer   jxn		!i: X position
      integer   jyn		!i: Y position
      integer   wc(NX,NY)	!i: Work space for arae round each star
      integer*2 map(NX,NY)	!i: U test image
      integer   jxl(2)		!i: X positions of pixels of 'col' path come from
      integer   jyl(2)		!i: Y positions of pixels of 'col' path come from
      integer   jxd(2)		!o: X positions of new pixels of 'col' path found
      integer   jyd(2)		!o: Y positions of new pixels of 'col' path found
      integer   numpath		!o: Number of pixels of new 'col' path found
      logical   iscol		!o: Is a 'col', that is part of a nipple?
C--
      integer ja, jb, ka, kb, j, k
      real dx, dy
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      iscol = .true.							!Default

      ja = max(1,jxn-1)							!3x3 area to work in
      jb = min(nx,jxn+1)
      ka = max(1,jyn-1)
      kb = min(ny,jyn+1)

      numpath = 0							!Find number of 'good' adjacent
      do k = ka, kb							! pixels
         do j = ja, jb

            if ( (j.ne.jxn.or.k.ne.jyn)       .and.			!Not central pixel
     +           wc(jxn,jyn).eq.3             .and.			!Is a '3' pixel
     +           map(j,k).le.map(jxn,jyn)     .and.			!Has U-map value lower than central
     +           (j.ne.jxl(1).or.k.ne.jyl(1)) .and.			!Not flagged from previous pass
     +           (j.ne.jxl(2).or.k.ne.jyl(2))       ) then		!Not flagged from previous pass

               numpath = numpath + 1
               if ( numpath.ge.3 ) then					!Not a 'col'
                  iscol = .false.
               else
                  jxd(numpath) = j					!Note new 'col' posns
                  jyd(numpath) = k
               endif

            endif

         enddo
      enddo

      if ( numpath.eq.0 ) iscol = .false.				!Not a 'col'

      if ( numpath.eq.2 ) then						!If 2 new pixels, check they are adjacent
         dx = real(jxd(2)-jxd(1))
         dy = real(jyd(2)-jyd(1))
         if ( (dx*dx+dy*dy).gt.1.1 ) iscol = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_RLOAD -- Load results from this program into results table
C
C  alan penny                   ral                       1990-07-27

      subroutine fi_rload ( xy, fp, msky, im, xp, yp, wp, zp, vp )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real	xy(NUMXY,NUMTOT)	!i/o: Results table
      real	fp(NELLI,NUMTOT)	!i/o: Elliptical stars info
      real      msky(NX,NY)		!i:   Sky image
      real      im(NX,NY)		!i:   Image
      real	xp(NUMTOT)		!i: X posns results
      real	yp(NUMTOT)		!i: Y posns results
      real	wp(NUMTOT)		!i: Flux values results
      real	zp(NUMTOT)		!i: U map values results
      real	vp(NUMTOT)		!i: Types results
C--
      integer ja, j, kxa, kya, ierr
      real    amin, amax
Cbegin


      if ( ST_FAILED ) return						!Check failure

      call azlimr ( im, NX*NY, RINVAL, amin, amax, ierr )		!Find maximum of image, to set
      									! stars with invalid central points
      									! to this value

      ja = 0								!Load output
      do j = 1, NUMF

         ja = ja + 1

         if ( ja.gt.NUMTOT ) then
            call printo ( 'Max no of stars loaded'//
     +                    '- BEWARE VALIDITY OF OUTPUT' )
            ja = NUMTOT
            NUMFG = NUMTOT
            return
         endif

         if ( fp(1,j).gt.0.5 ) then       				!ellipse

            xy(1,ja) = fp(2,j)          				!1st star
            xy(2,ja) = fp(3,j)
            xy(4,ja) = fp(4,j)
            kxa = xy(1,ja)
            kya = xy(2,ja)
            kxa = max(1,min(kxa,NX))
            kya = max(1,min(kya,NY))
            if ( fp(5,j).eq.RINVAL ) then
               xy(3,ja) = BS*(amax-msky(kxa,kya))
               xy(5,ja) = 13.0
            else
               xy(3,ja) = BS*(fp(5,j)-msky(kxa,kya))
               xy(5,ja) = 3.0
            endif
            xy(6,ja) = min(NUMTOT,(ja+1))

            ja = ja + 1  		          			!2nd star

            if ( ja.gt.NUMTOT ) then
               call printo ( 'Max no of stars loaded'//
     +                       '- BEWARE VALIDITY OF OUTPUT' )
               ja = NUMTOT
               NUMFG = NUMTOT
               return
            endif

            xy(1,ja) = fp(6,j)
            xy(2,ja) = fp(7,j)
            xy(4,ja) = fp(8,j)
            kxa = xy(1,ja)
            kya = xy(2,ja)
            kxa = max(1,min(kxa,NX))
            kya = max(1,min(kya,NY))
            if ( fp(9,j).eq.RINVAL ) then
               xy(3,ja) = BS*(amax-msky(kxa,kya))
               xy(5,ja) = 13.0
            else
               xy(3,ja) = BS*(fp(9,j)-msky(kxa,kya))
               xy(5,ja) = 3.0
            endif
            xy(6,ja) = ja - 1
         else
            xy(1,ja) = xp(j)         					!single stars
            xy(2,ja) = yp(j)
            xy(4,ja) = zp(j)
            xy(5,ja) = vp(j)
            kxa = xy(1,ja)
            kya = xy(2,ja)
            kxa = max(1,min(kxa,NX))
            kya = max(1,min(kya,NY))
            if ( wp(j).eq.RINVAL ) then
               xy(3,ja) = BS*(amax-msky(kxa,kya))
               xy(5,ja) = 10.0 + xy(5,ja)
            else
               xy(3,ja) = BS*(wp(j)-msky(kxa,kya))
            endif
            xy(6,ja) = 0.0
         endif
      enddo

      NUMFG = ja


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_WEEDP -- Weed out close pairs in a list
C
C  a j penny                         stsci             86-12-03

      subroutine fi_weedp ( xy )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'

      real xy(NUMXY,NUMTOT)		!i/o: Results table
C--
      integer k, j, jj, jd, ja, kk
      real rlim, rstep, rnow, dx, dy, dd
      logical more, first
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +                              'Started weeding out close pairs' )

      rlim = 0.66*RADIUS						!Weed out close pairs
      rstep = rlim/4.0
      do k = 1, 4
         rnow = real(k)*rstep
         more = .true.
         do while (more)
            more = .false.
            do j = 1, NUMFG
               if (xy(5,j).gt.-0.5) then
                  do jj = 1, NUMFG
                     if ( jj.ne.j .and. xy(5,jj).gt.-0.5 ) then
                        dx = xy(1,jj) - xy(1,j)
                        dy = xy(2,jj) - xy(2,j)
                        dd = dx*dx + dy*dy
                        if ( dd.lt.rnow*rnow ) then
                           more = .true.
                           xy(1,j) = (xy(1,j)+xy(1,jj))/2.0
                           xy(2,j) = (xy(2,j)+xy(2,jj))/2.0
                           xy(3,j) = min(xy(3,j),xy(3,jj))
                           xy(4,j) = max(xy(4,j),xy(4,jj))
                           xy(5,j) = min(xy(5,j),xy(5,jj))
                           xy(5,jj) = -1.0
                           if ( INFORM.gt.1 ) then
                              call pargi ( jj )
                              call printd ( 'Weeded star no %d' )
                           endif
                        endif
                     endif
                  enddo
               endif
            enddo
         enddo
      enddo

      NUMFP = 0								!See how many left
      do j = 1, NUMFG
         if ( xy(5,j).gt.-0.5 ) NUMFP = NUMFP + 1
      enddo

      do j = 1, NUMFG							!Bunch up
         if ( xy(5,j).lt.-0.5 ) then
            first = .true.
            do k = j, NUMFG
               if ( xy(5,k).gt.-0.5 .and. first ) then
                  ja = k
                  first = .false.
               endif
            enddo
            if ( .not.first ) then
              jd = ja - j
              do k = j, NUMFG-jd
                  do kk = 1, NUMXY
                     xy(kk,k) = xy(kk,k+jd)
                  enddo
               enddo
            endif
         endif
      enddo

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +                            'Finished weeding out close pairs' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_WEEDI -- Weed out invalid peaked stars with multiple fits
C
C  a j penny                         stsci             86-12-03

      subroutine fi_weedi ( xy, im, wd )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      xy(NUMXY,NUMTOT)	!i/o: Results table
      real      im(NX,NY)		!i: Input image
      integer	wd(NX,NY)		!o: Temorary workspace
C--
      logical more, first, amore
      integer nxy, kl, jb, kb, j, k, jj, kk, ja, jd, nump, joa, job,
     +        jp, kp, kcount
CX    integer jba, jbb, kba, kbb
      real ax, ay, an, dx, dy, d, fact
      parameter ( fact=7.0 )
      parameter ( nump=100 )
      integer knp(nump), lnp(nump)
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +             'Started weeding out multiple invalid peaks' )

      call azeroi ( wd, NX*NY )						!Map all invalid areas
      do k = 1, NY
         do j = 1, NX
            if ( im(j,k).eq.RINVAL ) wd(j,k) = 1
         enddo
      enddo

      kcount = 0							!Search for all isolated sets
      more = .true.
      do while ( more )

         kk = 0								!Find next such one
         kl = 1
         nxy = NX*NY
         do while ( kl.eq.1 .and. kk.lt.nxy )
            kk = kk + 1
            kb = 1 + (kk-1)/NX
            jb = kk - (kb-1)*NX
            if ( wd(jb,kb).eq.1 ) kl = 0
         enddo
         if ( kl.eq.1 ) then
            more = .false.
         else

            kcount = kcount + 1						!Flag with a 3 all points
            wd(jb,kb) = 3						! in contact with that one
CX            kba = kb
CX            kbb = kb
CX            jba = jb
CX            jbb = jb
            amore = .true.
            do while ( amore )
               amore = .false.
               do k = kb, min(ny,(kb+100))
                  do j = max(1,(jb-50)), min(nx,(jb+50))
                     if ( wd(j,k).eq.1 ) then
                        do kk = max(1,k-1),min(ny,k+1)
                           do jj = max(1,j-1),min(nx,j+1)
                              if ( (jj.ne.j.or.kk.ne.k) .and.
     +                             wd(jj,kk).eq.3 ) then
                                 wd(j,k) = 3
                                 amore = .true.
CX                                 kbb = max(k,kbb)
CX                                 jba = min(j,jba)
CX                                 jbb = max(j,jbb)
                              endif
                           enddo
                        enddo
                     endif
                  enddo
               enddo
            enddo

            jj = 0							!Look for all posns in that area
            do j = 1, NUMFP
               jp = xy(1,j)
               kp = xy(2,j)
               first  = .true.
               do k = max(1,(kp-1)),min(ny,(kp+1))
                  do kk = max(1,(jp-1)), min(nx,(jp+1))
                     if ( wd(kk,k).eq.3 .and. first ) then
                        first = .false.
                        jj = min(nump,(jj+1))
                        knp(jj) = j
                     endif
                  enddo
               enddo
            enddo

            if ( jj.gt.1 ) then						!Weed them if each nearer than n*radii
               call amovki ( 1, lnp, jj )
               do j = 1, jj-1
                  if ( lnp(j).ge.1 ) then
                     joa = knp(j)
                     ax = xy(1,joa)
                     ay = xy(2,joa)
                     an = 1.0
                     do ja = 1, jj
                        if ( j.ne.ja .and. lnp(ja).ge.1 ) then
                           job = knp(ja)
                           dx = xy(1,job) - ax
                           dy = xy(2,job) - ay
                           d = sqrt(max(0.00001,(dx*dx+dy*dy)))
                           if ( d.lt.(fact*RADIUS) ) then
                              ax = ax + xy(1,job)*real(lnp(ja))
                              ay = ay + xy(2,job)*real(lnp(ja))
                              an = an + real(lnp(ja))
                              lnp(ja) = 0
                              xy(5,job) = -1.0
                              if ( INFORM.gt.1 ) then
                                 call pargi ( job )
                                 call printd ( 'Weeded star no %d' )
                              endif
                           endif
                        endif
                     enddo
                     if ( an.gt.1.1 ) then
                        xy(1,joa) = ax/an
                        xy(2,joa) = ay/an
                        xy(3,joa) = inval
                        xy(5,joa) = 4
                        lnp(j) = nint(an)
                     endif
                  endif
               enddo
            endif

            do k = 1, ny						!Remove that area from the list
               do j = 1, nx
                  if ( wd(j,k).eq.3 ) wd(j,k) = 4
               enddo
            enddo

         endif
      enddo


      NUMOUT = 0							!See how many left
      do j = 1, NUMFP
         if ( xy(5,j).gt.-0.5 ) NUMOUT = NUMOUT + 1
      enddo

      do j = 1, NUMFP							!Bunch up
         if ( xy(5,j).lt.-0.5 ) then
            first = .true.
            do k = j, NUMFP
               if ( xy(5,k).gt.-0.5 .and. first ) then
                  ja = k
                  first = .false.
               endif
            enddo
            if ( .not.first ) then
              jd = ja - j
              do k = j, NUMFP-jd
                  do kk = 1, NUMXY
                     xy(kk,k) = xy(kk,k+jd)
                  enddo
               enddo
            endif
         endif
      enddo

      if ( INFORM.gt.0 ) call printo ( 					!Inform
     +                   'Finished weeding out multiple invalid peaks' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
C FI_OSTORE -- Open the output list and store results table into it
C
C    a j penny             stsci             1988-05-02

      subroutine fi_ostore ( xy )

      implicit none

      include 'starfind.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real xy(NUMXY,NUMTOT)		!i: Results table
C--
      integer numcol, ipout, k, istat
      character    text*72
      character*20 header(NUMXY)
      data header / 'X', 'Y', 'HEIGHT', 'UTEST', 'TYPE', 'ECOMPANION' /
Cbegin


      if ( ST_FAILED ) return						!Check failure

      if ( NUMOUT.eq.0 ) then
         call printo ( 'No stars found - No output file opened' )
         ST_FAILED  = .true.
         return
      endif

      numcol = NUMXY + 5						!Open file, set up
      call optabw ( 'OUT', ipout, numcol, NUMOUT, .false., istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get1c ( 'TITLE', text, IMTITLE, .true. )
      call ptdesc ( 'OUT', 'TITLE', text )
      do k = 1, NUMXY
         call pthead ( 'OUT', k, header(k), istat )
      enddo
      call ident ( %val(ipout), numcol, NUMOUT )

      call coprr ( xy, NUMXY, NUMTOT, 1, NUMXY, 1, NUMOUT,		!Store results
     +             %val(ipout), numcol, NUMOUT, 6, 1)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_SMEAR -- Remove doubles that are the result of image smear
C   Looks for a significant number of doubles with the same orientation
C   and separation. If it finds them, assumes they are caused by image
C   smearing and replaces each double by one star in the centre.
C
C   The test for significance is to make an array of bins, of 10 in X
C   and +/- 10 in Y (where the bins have sides of 0.5*star radius). Then
C   for each star look at all the other stars and see if their separation
C   from the first star would land them in one of the bins. If so, add one
C   to the appropriate bin. When this is done for all stars, look at the
C   contrast between the mean count per bin and the bin with the maximum
C   number of counts. Is there a Poissonly significant (prob>0.999) chance
C   that that bin has too many counts for chance? If so, then there is
C   smearing.
C
C    a j penny                  ral                           1988-07-09

      subroutine fi_smear ( xy, kp )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'

      real	xy(NUMXY,NUMTOT)	!i/o: Results table
      integer	kp(NUMTOT)		!o: Work space
C--
      integer j, k, jj, jx, jy, jxm, jym, nweed
      real	ratm, ratio, ox, oy, oxm, oym, ax, ay, axms, axme,
     +          ayms, ayme
      logical found
Cbegin


      if ( ST_FAILED .or. .not.SMEAR ) return				!Failure or not do check

      if ( INFORM.gt.0 ) call printo ( 'Started looking for smear' )

      nweed = 0
      ratm = -0.5

      do k = 1, 10							!Find peakest box offset
         do j = 1, 10							! and size, posn of peak
            ox = (real(j)-1.0)/10.0
            oy = (real(k)-1.0)/10.0
            call fi_box ( xy, ox, oy, jx, jy, ratio )
            if ( ratio.gt.ratm ) then
               ratm = ratio
               jxm = jx
               jym = jy
               oxm = ox
               oym = oy
            endif
         enddo
      enddo

      if ( ratm.gt.0.0 ) then						!If significant peak
         call azeroi ( kp, NUMOUT ) 					! found, mark those there
         axms = real(jxm) - 0.5		        			! and average posns
         axme = real(jxm) + 1.5
         ayms = real(jym) - 0.5
         ayme = real(jym) + 1.5
         do k = 1, NUMOUT - 1
            if ( kp(k).eq.0 ) then
               j = k + 1
               found = .false.
               do while ( .not.found .and. j.le.NUMOUT )
                  if ( kp(j).eq.0 ) then
                     ax = 2.0*(xy(1,j)-xy(1,k))*RADIUS + oxm
                     if ( ax.ge.axms .and. ax.le.axme ) then
                        ay = 2.0*(xy(2,j)-xy(2,k))*RADIUS + oym + 10
                        if ( ay.ge.ayms .and. ay.le.ayme ) then
                           xy(1,k) = (xy(1,k)+xy(1,j))/2.0
                           xy(2,k) = (xy(2,k)+xy(2,j))/2.0
                           found = .true.
                           kp(j) = 2
                           kp(k) = 1
                        endif
                     endif
                  endif
                  j = j + 1
                enddo
            endif
         enddo

         k = 1								!Weed comps
         do while ( k.lt.(NUMOUT-nweed) )
            if ( kp(k).eq.2 ) then
               do j = k, NUMOUT-nweed-1
                  do jj = 1, NUMXY
                     xy(jj,j) = xy(jj,j+1)
                  enddo
                  kp(j) = kp(j+1)
               enddo
               nweed = nweed + 1
            endif
            k = k + 1
         enddo
         if ( kp(NUMOUT-nweed).eq.2 ) nweed = nweed + 1
         NUMOUT = NUMOUT - nweed

      endif

      if ( INFORM.gt.0 ) then
         if ( nweed.ne.0 ) then
            call pargi ( nweed )
            call printd ( 'Smearing found - %d stars removed' )
         else
            call printo ( 'No smeared stars found' )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FI_BOX -- Look for significant excess in pair angles, separations
C   This loads bins with number of doubles at that angle and separation,
C   and looks for a bin with a real excess.
C
C     a j penny                   ral                     1988-07-09

      subroutine fi_box ( xy, ox, oy, jxm, jym, ratio )

      implicit none
      include 'starfind.inc'
      include 'STARMAN_INC'

      real	xy(NUMXY,NUMTOT)	!i/o: Results table
      real	ox			!i: Fractional X offset for bins
      real	oy			!i: Fractional X offset for bins
      integer	jxm			!o: X of bin with significant excess counts
      integer	jym			!o: X of bin with significant excess counts
      real	ratio			!o: Ratio of max bin to biggest touching one
					!   Set to -1.0 if no significant bin found
C--
      integer j, k, nsum, nmax, nmaxl, kpois(21), kam,
     +        n(10,20), kx, ky
      real am
      logical found
      data kpois / 2,  3,  3,  4,  4,  4,  5,  5,  6,  6,
     +             8, 10, 12, 14, 16, 17, 18, 19, 22, 29, 36/
Cbegin


      if ( ST_FAILED ) return						!Check for failure

      call azeroi ( n, 200 )						!Clear bins

      nsum = 0								!Load bins
      do k = 1, NUMOUT-1
         do j = k+1, NUMOUT
            kx = 2.0*(xy(1,j)-xy(1,k))*RADIUS + ox
            if ( kx.ge.1 .and. kx.le.10 ) then
               ky = 2.0*(xy(2,j)-xy(2,k))*RADIUS + oy + 10
               if ( ky.ge.1 .and. ky.le.20 ) then
                  n(kx,ky) = n(kx,ky) + 1
                  nsum = nsum + 1
               endif
            endif
         enddo
      enddo

      nmax = 0								!Posn, value of max
      jxm = 1
      jym = 1
      do k = 1, 20
         do j = 1, 10
            if ( n(j,k).gt.nmax ) then
               jxm = j
               jym = k
               nmax = n(j,k)
            endif
         enddo
      enddo

      found = .false.							!Is max significant?
      if ( nmax.ne.0 ) then						! If so, make 'found' true
         am = real(nsum)/200.0
         if ( am.lt.1.0 ) then
            kam = am*10.0 + 1.0
         elseif ( am.ge.15 ) then
            kam = 21
         elseif ( am.ge.10 ) then
            kam = 20
         else
            kam = 10 + am
         endif
         if ( nmax.gt.kpois(kam) ) found = .true.
      endif

      ratio = -1.0							!Load ratio
      if ( found ) then
         nmaxl = 0
         do k = jym-1, jym+1
            do j = jxm-1, jxm+1
               if ( .not.(j.eq.jxm.and.k.eq.jym) .and.
     +              j.ge.1 .and. j.le.10 .and. k.ge.1 .and.
     +              k.le.20 ) then
                  if ( n(j,k).gt.nmaxl ) nmaxl = n(j,k)
               endif
            enddo
         enddo
         ratio = 1000.0
         if ( nmaxl.ne.0 ) ratio = real(nmax)/real(nmaxl)
      endif


      end
