CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADDSTARS -- This program adds/subtracts stars to/from an image
C
C             For a decription of this program, see ADDSTARS.HLP
C
C   A.J.Penny                 STScI                    87-02-2013:31

      subroutine addstars (ierradam)

      implicit none
      integer   ierradam
C--
Cbegin

      call starman_start

      call t_addstars

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_ADDSTARS.F
C
C T_ADDSTARS     Add/subtract stars to/from an image
C AD_GCL         Get some CL parameters
C AD_DOIT        Add the stars
C AD_RIMGADDR    Add a constant with (scaled poisson noise) to real array.
C AD_RIMGNOISE   Add scaled poisson noise to a real array
C AD_RIMGADDN    Add a poisson noise (of a fixed size, mean zero)
C AD_ADDM(SR)IR  Add a scaled integer*2/real (magic values) image to a real image
C AD_RCOR        Correct a real array for BS, BZ


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_ADDSTARS -- Add/subtract stars to/from an image
C
C   A.J.Penny                 RAL                         1994 Feb

      subroutine t_addstars ()

      implicit none
      include 'addstars.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ipd, ipmap, istat
Cbegin


      call ad_gcl							!Get some CL params and
      if ( ST_FAILED ) return						! open some files

      call gtwrkr ( 'WORK', NX*NY, ipd, istat )				!Open working image
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      if ( RINVAL.eq.0.0 ) then						! (uses real work image)
         call amovkr ( 1.0e-5, %val(ipd), NX*NY )
      else
         call azeror ( %val(ipd), NX*NY )
      endif

      call gtwrkr ( 'WORKFIT', MX*MY, ipmap, istat )			!Copy profile map to working area
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call achtsr ( %val(IPTMAP), %val(ipmap), MX*MY )
      call arrsc  ( %val(ipmap), MX, MY, BSM, BZM )

      call ad_doit ( %val(ipd), %val(ipmap) )				!Add stars

      call ad_rimgnoise ( %val(ipd), STARPOIS, STARGAIN, NRAN )		!Apply noise to stars

      call ad_rimgaddr ( %val(ipd), SKY, SKYPOIS, SKYGAIN, NRAN )	!Add the sky

      call ad_rimgaddn ( %val(ipd), PIXNOISE, SKYGAIN, NRAN )		!Put the extra sky noise in

      if ( ADDIM ) then							!Add old image
         if ( IMTYPE.eq.'SHORT' ) then
            call ad_addmsir ( %val(IPIM), %val(ipd) )
         else
            call ad_addmrir ( %val(IPIM), %val(ipd) )
         endif
      endif

      call ad_rcor ( %val(ipd), NX*NY )			 		!Scale back to input
      if ( ST_FAILED ) return

      if ( OUTTYPE.eq.'SHORT' ) then					!Work image to output image
         call achtrs ( %val(ipd), %val(IPO), NX*NY )
      else
         call amovr ( %val(ipd), %val(IPO), NX*NY )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_GCL -- Get some CL parameters
C
C   a j penny                 dao           1988-04-25

      subroutine ad_gcl ( )

      implicit none
      include 'addstars.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
C--
      logical gotran
      integer iv, istat
      character*72 title
      real rv, rva, qbase
      character*68 thelp1(4)
      data thelp1 /
     + 'Option    Function',
     + '------    --------',
     + 'Add       Add stars to image',
     + 'Sub       Subtract stars from image' /
      character*68 th2(4)
      data th2 /
     + 'Option    Function',
     + '------    --------',
     + 'Real      Output image to be 32-bit real type',
     + 'Short     Output image to be 16-bit integer type' /
Cbegin


      if ( ST_FAILED ) return

      gotran = .false.							!Default

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .true., istat )		!Get the input image data
      if ( ST_FAILED ) return
      if ( istat.eq.2 ) then						! array or new image size
         ADDIM = .false.
         BS = 1.0
         BZ = 0.0
         INVAL = INT_INVALSI
         RINVAL = INT_INVALR
         NX = 100
         NY = 100
         call get2i ( 'SIZE', NX, NY, .true., 1, 4096 )
         if ( ST_FAILED ) return
         call get_job ( 'OUTTYPE', 'real:short', iv, 2, th2, 4 )
         if ( ST_FAILED ) return
         OUTTYPE = 'REAL'
         if ( iv.eq.2 ) OUTTYPE = 'SHORT'
      elseif ( istat.eq.1 .or. istat.eq.3 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: File wrong' )
         return
      else
         ADDIM = .true.
         OUTTYPE = IMTYPE
         call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, title, iv)
         if ( IMTYPE.eq.'SHORT' ) RINVAL = INVAL
      endif

      call optabr ( 'INSTARS', IPXY, TBX5, TBY, .false., istat )	!Now seek the list of positions
      if ( istat.ne.0 ) then						! and magnitudes
         ST_FAILED = .true.
         call printo ( 'ERROR: Star list wrong' )
      elseif ( TBX5.lt.8 ) then
         ST_FAILED = .true.
         call printo (
     +         'ERROR: Star list must have at least three columns' )
      endif
      if ( ST_FAILED ) return

      call get_mprof ( 'PROFILE', IPTMAP, PROF, MX, MY, MZ,		!Get profile
     +                 MAGNIFK, MAPXK, MAPYK, BSM, BZM, qbase,
     +                 rv, rva, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Profile wrong' )
         return
      endif

      call get_job ( 'ADD', 'add:subtract', iv, 1, thelp1, 4 )		!Get if to add or subtract
      if ( ST_FAILED ) return
      ADD = 'SUB'
      if ( iv.eq.1 ) ADD = 'ADD'

      call get1b ( 'DOMAP', DOMAP, .true. )				!Get if to use profile map

      call get1b ( 'PYES', PBOSS, .true. )				!Get if profile paramaters
									! to overide star list profile

      rv = 100.0							!Get the sky level
      if ( ADDIM ) rv = 0.0
      call get1r ( 'SKY', SKY, rv, -1.0e9, 1.0e9 )

      STARGAIN = 1.0
      call get1b ( 'STARPOIS', STARPOIS, .true. )			!Star Poisson noise?
      if ( ST_FAILED ) return
      if ( STARPOIS ) then
         if ( ADD.ne.'ADD' .and. STARPOIS ) then
            call printo ( 'Noise will be added as though stars +ve' )
         endif
         call get1r ( 'STARGAIN', STARGAIN, 1.0, 0.0, 1.0e8 )
         if ( .not.gotran .and. STARGAIN.ne.0.0 ) then
            call get1i ( 'SEED', NRAN, 1234567891, 1200000001,
     +                1400000001 )
            if ( ST_FAILED ) return
            gotran = .true.
         endif
      endif

      SKYPOIS = .false.
      SKYGAIN = 1.0							!Get if sky image Poisson noisy
      if ( SKY.ne.0.0 ) then
         call get1b ( 'SKYPOIS', SKYPOIS, .true. )
         if ( ST_FAILED ) return
         if ( SKY.lt.0.0 .and. SKYPOIS ) then
            call printo ( 'Noise will be added as though sky +ve' )
         endif
         if ( SKYPOIS ) then
            call get1r ( 'SKYGAIN', SKYGAIN, 1.0, 0.0, 1.0e8 )
            if ( ST_FAILED ) return
            if ( .not.gotran .and. SKYGAIN.ne.0.0 ) then
               call get1i ( 'SEED', NRAN, 1234567891, 1200000001,
     +                      1400000001 )
               gotran = .true.
            endif
         endif
      endif

      call get1r ( 'PIXNOISE', PIXNOISE, 0.0, 0.0, 1.0e8 )		!Get if sky image have extra noise
      if ( ST_FAILED ) return
      if ( .not.gotran .and. PIXNOISE.ne.0.0 ) then
         call get1i ( 'SEED', NRAN, 1234567891, 1200000001,
     +                1400000001 )
         if ( ST_FAILED ) return
         gotran = .true.
      endif

      call opimzw ( 'OUT', OUTTYPE, IPO, NX, NY, .false., istat )	!Get output image
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Output file wrong' )
         return
      endif
      if ( .not.ADDIM ) title = 'Output from ADDSTARS'
      call get1c ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      if ( ADDIM ) call icopdes ( 'IN', 'OUT', istat )
      call ptdesc ( 'OUT', 'TITLE', title )
      call ptdesr ( 'OUT', 'BSCALE', BS )
      call ptdesr ( 'OUT', 'BZERO', BZ )
      if ( OUTTYPE.eq.'SHORT' ) then
         call ptdesi ( 'OUT', 'INVAL', INVAL )
      else
         call ptdesr ( 'OUT', 'INVAL', RINVAL )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_DOIT -- Add the stars
C
C alan penny                 ral                     1991 Dec

      subroutine ad_doit ( rim, map )

      implicit none
      include 'addstars.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real     rim(NX,NY)		!i/o: Working image
      real     map(MX,MY,MZ)		!i: Star profiles map
C--
      real       aprof(9), x, y, h
      integer    khead(16), mapx, mapy, mapnum, magnif, k, ierr, kxw(4)
      character  ahead*20, thead*20
Cbegin


      if ( ST_FAILED ) return

      call gettheh ( 'INSTARS', TBX5, khead )				!Get input stars list column order

      if ( khead(1).eq.0 ) then
         do k = TBX5-5, 1, -1
            call gthead ( 'INSTARS', k, ahead, ierr )
            if ( ierr.eq.0 ) then
               if ( ahead.eq.'x' ) khead(1) = k
            endif
         enddo
         if ( khead(1).eq.0 ) then
            khead(1) = 1
            call printo ( ' WARNING: Header for X positions not found')
            call printo ( '          Must be x or X')
            call printo ( '          Assumed to be in column 1')
            call gthead ( 'INSTARS', 1, ahead, ierr )
            call pargc ( ahead )
            call printd ( '          Header in column 1 is %c' )
         endif
      endif
      if ( khead(2).eq.0 ) then
         do k = TBX5-5, 1, -1
            call gthead ( 'INSTARS', k, ahead, ierr )
            if ( ierr.eq.0 ) then
               if ( ahead.eq.'y' ) khead(2) = k
            endif
         enddo
         if ( khead(2).eq.0 ) then
            khead(2) = 2
            call printo ( ' WARNING: Header for Y positions not found')
            call printo ( '          Must be y or Y')
            call printo ( '          Assumed to be in column 2')
            call gthead ( 'INSTARS', 2, ahead, ierr )
            call pargc ( ahead )
            call printd ( '          Header in column 2 is %c' )
         endif
      endif
      if ( khead(3).eq.0 ) then
         do k = TBX5-5, 1, -1
            call gthead ( 'INSTARS', k, thead, ierr )
            if ( ierr.eq.0 ) then
               call lowcase ( thead, ahead )
               if ( ahead.eq.'ht' .or. ahead.eq.'hts' .or.
     +              ahead.eq.'height' .or. ahead.eq.'heights' )
     +              khead(3) = k
            endif
         enddo
         if ( khead(3).eq.0 ) then
            khead(3) = 3
            call printo ( ' WARNING: Header for heights not found')
            call printo ( '          Must be ht/hts/height/heights')
            call printo ( '          (Case not important)')
            call printo ( '          Assumed to be in column 3')
            call gthead ( 'INSTARS', 3, ahead, ierr )
            call pargc ( ahead )
            call printd ( '          Header in column 3 is %c' )
         endif
      endif

      do k = 1, TBY							!Add or Remove stars

         call getd ( %val(IPXY), TBX5, TBY, khead, k, x, y, h, 3, PROF,
     +               aprof, mapnum, mapx, mapy, magnif, MAGNIFK, MAPXK,
     +               MAPYK, 1, PBOSS )

         if ( DOMAP ) call rchzero ( map, MX, MY, MZ, mapnum, mapx,
     +                               mapy, DOMAP )

         if ( ADD.eq.'ADD' ) then
            call popamr ( rim, NX, NY, 1.0, RINVAL, x, y, h, aprof,
     +                    map, MX, MY, MZ, mapnum, mapx, mapy, magnif,
     +                    DOMAP, ierr, kxw )
         else
            call popsmr ( rim, NX, NY, 1.0, RINVAL, x, y, h, aprof,
     +                    map, MX, MY, MZ, mapnum, mapx, mapy, magnif,
     +                    DOMAP, ierr, kxw )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_RIMGADDR -- Add a constant with (scaled +ve poisson noise) to real array.
C
C     a.j. penny          stsci                19

      subroutine ad_rimgaddr ( ra, rval, donoise, scale, nran )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real       ra(NX,NY)          !i/o: image array
      real       rval               !i:   mean value to be added
      logical    donoise	    !i:   Add noise?
      real       scale              !i:   noise scale
      integer    nran               !i/o: random seed
C--
      integer j, k
      real    sval, ss
Cbegin


      if ( ST_FAILED ) return

      if ( rval.eq.0 ) return

      if ( donoise .and. scale.ne.0.0 ) then
         sval = rval*scale
         if ( sval.lt.0.0 ) sval = -1.0*sval
         do k = 1, NY
   	    do j = 1, NX
               call poidev ( sval, ss, nran )
               ra(j,k) = ra(j,k) + rval + (ss-sval)/scale
            enddo
         enddo
      else
         call aaddkr ( ra, rval, ra, NX*NY )
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_RIMGNOISE -- Add scaled poisson noise to a real array
C    The noise is added with zero mean, and size is the poisson
C    expected noise from the pixel value. If pixel value is -ve,
C    the noise is calculated as if +ve.
C
C     a.j. penny            stsci                1988-04-22

      subroutine ad_rimgnoise ( ra, donoise, scale, nran )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real       ra(NX,NY) 	    !i/o: image array
      logical    donoise	    !i:   Add noise?
      real       scale              !i:   noise scale
      integer    nran               !i/o: random seed
C--
      integer j, k
      real    ss, sval
Cbegin


      if ( ST_FAILED ) return

      if ( .not.donoise .or.scale.eq.0.0 ) return

      do k = 1, NY
         do j = 1, NX
            sval = scale*ra(j,k)
            if ( sval.lt.0.0 ) sval = -1.0*sval
            call poidev ( sval, ss, nran )
            ra(j,k) = ra(j,k) + (ss-sval)/scale
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_RIMGADDN -- Add a poisson noise to a real array.
C
C     a.j. penny                  stsci                1988-04-22

      subroutine ad_rimgaddn ( ra, rval, scale, nran )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real       ra(NX,NY)		!i/o: image array
      real       rval			!i:   noise
      real       scale			!i:   Scale (electrons/adu)
      integer    nran			!i/o: random seed
C--
      integer j, k
      real    reff, rr
Cbegin


      if ( ST_FAILED ) return

      if ( rval.eq.0.0 .or. scale.eq.0.0 ) return

      reff = rval*rval
      do k = 1, NY
         do j = 1, NX
            call poidev ( reff, rr, nran )
            ra(j,k) = ra(j,k) + rr - reff/scale
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_ADDMRIR -- Add a scaled real (magic values) image to a real image
C          Scaling is performed. Magic value left untouched.
C
C     a.j. penny                stsci                1988-04-22

      subroutine ad_addmrir ( ia, rim )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real         ia(NX,NY) 	        !i:   image array
      real	   rim(NX,NY)		!i/o: added image
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, NY
	 do j = 1, NX
            if ( ia(j,k).eq.RINVAL ) then
               rim(j,k) = RINVAL
            else
               rim(j,k) = rim(j,k) + BS*ia(j,k) + BZ
            endif
         enddo
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_ADDMSIR -- Add a scaled integer*2 (magic values) image to a real image
C          Scaling is performed. Magic value left untouched.
C
C     a.j. penny                stsci                1988-04-22

      subroutine ad_addmsir ( ia, rim )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2    ia(NX,NY) 	        !i:   image array
      real	   rim(NX,NY)		!i/o: added image
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, NY
	 do j = 1, NX
            if ( ia(j,k).eq.INVAL ) then
               rim(j,k) = INVAL
            else
               rim(j,k) = rim(j,k) + BS*real(ia(j,k)) + BZ
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AD_RCOR -- Correct a real array for BS, BZ
C
C   a j penny                 dao           1988-04-25

      subroutine ad_rcor ( rim, n )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer	n		!i: Array size
      real	rim(n)		!i/o: Array
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( BZ.eq.0.0 .and. BS.eq.1.0 ) return

      if ( BS.eq.0.0 ) then
         call printo ( 'ERROR: s/r AD_RCOR divide by zero wanted' )
         return
      endif

      do k = 1, n
         if ( rim(k).ne.RINVAL ) rim(k) = (rim(k)-BZ)/BS
      enddo


      end
