CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   AIRSUB.FOR
C
C   Contains:-
C
C AIRGKTIME       Get use of keyboard for exposure time?
C AIRDESCR        Use image descriptors for air mass and exp time?
C AIRTIME         Get image and exposure details
C AIR_FIRSTSTR    Puts blanks after 1st contiguous characters in a character string
C AIR_STRCON3     Convert string into 3 reals



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIRGKTIME -- Get use of keyboard for exposure time?
C
C   a j penny                 ral               1992 Oct

      subroutine airgktime ( )

      implicit none
      include 'air.inc'
      include 'STARMAN_INC'
C--
      real rv
Cbegin


      call get1b ( 'KEYTIME', DOKTIME, DOKTIME )
      if ( ST_FAILED ) return
      if ( DOKTIME ) then
         DOHTIME = .false.
         call get1r ( 'EXPTIM', EXPTIM, EXPTIM, 1.0e-8, 1.0e8 )
         if ( ST_FAILED ) return
         rv = 2.5*alog10(EXPTIM)
         call pargr ( rv )
         call printd ( 'Time zero point correction = %f ' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIRDESCR -- Use image descriptors for exp time and air mass?
C
C alan penny             ral                 1992 Oct

      subroutine airdescr ( kopt )

      implicit none
      include 'air.inc'
      include 'STARMAN_INC'

      integer  kopt		!i: 1=ask for exp time; 2=ask for air mass
C--
      logical more
      integer k, iv, istat
      character*70 cv, cva
      real extvala(9)
      data extvala / 0.68, 0.28, 0.15, 0.10, 0.07, 0.0,0.0,0.0,0.0/

      integer nthelp1
      parameter ( nthelp1=5 )
      character*68 thelp1(nthelp1)
      data (thelp1(k),k=1,nthelp1) /
     + ' Option    Function' ,
     + ' ------    --------' ,
     + ' Airamss   Look for airmass in an airmass descriptor',
     + ' None      Do not look for airamass',
     + ' Position  Look for airmass via sky position descriptors' /

      integer nthelp2
      parameter ( nthelp2=5 )
      character*68 thelp2(nthelp2)
      data (thelp2(k),k=1,nthelp2) /
     + ' Option    Function' ,
     + ' ------    --------' ,
     + ' Degrees   Data in degrees' ,
     + ' Radians   Data in radians' ,
     + ' String    Data in a character string (h:m:s or d:m:s)' /
Cbegin


      if ( kopt.eq.1 ) then						!Get image exposure time details
         call get1b ( 'DESCRTIME', DOHTIME, DOHTIME )
         if ( ST_FAILED ) return
         if ( DOHTIME ) then
            call get1c ( 'EXPNAME', EXPNAME, 'TIMEXPOS', .true. )
            if ( ST_FAILED ) return
            DOKTIME = .false.
         endif
      endif

      if ( kopt.ne.2 ) return

      call get_job ( 'DESCRAIR', 'none:airmass:position', k, 2,		!Get what input type
     +                thelp1, nthelp1 )
      if ( ST_FAILED ) return
      DOHAIR = .false.
      if ( k.eq.1 ) return
      DOHAIR = .true.

      if ( k.eq.2 ) then
         call get1c ( 'AIRNAME',AIRNAME, 'AIRMASS',.true.)		!Get image airmass location
         if ( ST_FAILED ) return
      else
         AIRNAME = ' '
         call get1c ( 'LATITUDE', cv, '+00 00 00', .true. )		!Latitude
         if ( ST_FAILED ) return
         iv = 1
         call sla_dafin ( cv, iv, DLAT, istat )
         if ( istat.ne.0 ) call printo (
     +                             'ERROR: Latitude in wrong format' )
         call get1c (  'RANAME',   RANAME,   'RA', .true. )		!Get image RA location
         call get1c ( 'DECNAME',  DECNAME,  'DEC', .true. )		!Get image Dec location
         call get_job ( 'COORDTYPE', 'degrees:radians:string', 		!Posn input type
     +                      COORDTYPE, 2 , thelp2, nthelp2 )
         call get1c ( 'SIDTNAME', SIDTNAME, 'SIDT', .true. )		!Get image Sid T location
         if ( ST_FAILED ) return
      endif

      call get1b ( 'DESCRFILT', DOHFILT, DOHFILT )
      if ( ST_FAILED ) return

      if ( DOHFILT ) then
         call get1c ( 'FILTNAME', FILTNAME, 'FILTER', .true. )		!Get image filter name location
         if ( ST_FAILED ) return
         LOCFILT(1) = 1
         LOCFILT(2) = 79
         call get2i ( 'FILTLOC', LOCFILT(1), LOCFILT(2), .true., 1,256)
         if ( ST_FAILED ) return
         k = 0								!get filter names in image
         more = .true.							! and extinctions
         NUMFILT = 0
         do while ( k.lt.9 .and. more )
            k = k + 1
            write ( cv, '(i1)' ) k
            cva = 'FNAME'//cv
            call get1c ( cva, FNAME(k), ' ', .true. )
            if ( ST_FAILED ) return
            if ( FNAME(k).eq.' ' ) then
               more = .false.
               NUMFILT = k - 1
            else
               call lbgone ( FNAME(k) )
               cva = 'EXT'//cv
               call get1r ( cva, EXTVAL(k), extvala(k), 0.0, 1.0e8 )
               if ( ST_FAILED ) return
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIRTIME -- Get image and exposure details
C
C  alan penny                ral                1990-03-09

      subroutine airtime ( htime, airm, extinc, nfilt )

      implicit none
      include 'air.inc'
      include 'STARMAN_INC'

      real         htime	!o: Exposure time
      real         airm		!o: Air mass
      real         extinc	!o: Extinction
      character*4  nfilt	!o: Filter name
C--
      character*90 text, filter
      real rh, rm, rs, rv, ra, dec, sidt
      integer jlen, k, iv, istat, numf
      double precision dha, dzd, ddec, sla_zd, sla_airmas
      external sla_zd, sla_airmas
Cbegin


      if ( ST_FAILED ) return

      htime = 1.0
      airm = 1.0
      extinc = 0.0
      nfilt = ' '

      if ( DOHTIME ) then
         call gtdesc ( 'IN', EXPNAME, text, ' ', iv, istat )             !Exposure time
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.ne.0 ) then
            call printo ('ERROR: No Exposure Time in image descriptors')
         else
            call lbgone ( text )
            call air_firststr ( text )
            call chartor ( text, htime, istat )
         endif
       endif

      if ( DOHAIR .and. AIRNAME.ne.' ' ) then
         call gtdesc ( 'IN', AIRNAME, text, ' ', iv, istat )		!Airmass
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.eq.0 ) then
            call printo ( 'ERROR: No Air Mass in image descriptors' )
         else
            call lbgone ( text )
            call air_firststr ( text )
            call chartor ( text, airm, istat )
         endif
      endif

      if ( DOHAIR .and. AIRNAME.eq.' ' ) then

         call gtdesc ( 'IN', RANAME, text, ' ', iv, istat )		!Image RA
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.ne.0 ) then
            call printo ( 'ERROR: No RA in image descriptors' )
         else
            call lbgone ( text )
            call air_firststr ( text )
            if ( COORDTYPE.eq.1 ) then
               call chartor ( text, ra, istat )
               ra = PI*ra/180.0
            elseif ( COORDTYPE.eq.2 ) then
               call chartor ( text, ra, istat )
            else
               iv = 1
               call sla_afin ( text, iv, ra, istat )
               if ( istat.ne.0 ) then
                  call printo ( 'ERROR: Image RA in wrong format' )
                  return
               endif
            endif
         endif

         call gtdesc ( 'IN', DECNAME, text, ' ', iv, istat )		!Image Dec
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.ne.0 ) then
            call printo ( 'ERROR: No Dec in image descriptors' )
         else
            call lbgone ( text )
            call air_firststr ( text )
            if ( COORDTYPE.eq.1 ) then
               call chartor ( text, dec, istat )
               dec = PI*dec/180.0
            elseif ( COORDTYPE.eq.2 ) then
               call chartor ( text, dec, istat )
            else
               iv = 1
               call sla_afin ( text, iv, dec, istat )
               if ( istat.ne.0 ) then
                  call printo ( 'ERROR: Dec in wrong format' )
                  return
               endif
            endif
         endif

         call gtdesc ( 'IN', SIDTNAME, text, ' ', iv, istat )		!Image Sid T
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.ne.0 ) then
            call printo ( 'ERROR: No Sid Time in image descriptors' )
         else
            call lbgone ( text )
            call air_firststr ( text )
            call air_strcon3 ( text, rh, rm, rs, istat )
            rv = rs + rm*60.0 + rh*3600.0
            sidt = rv*PI/(12.0*60.0*60.0)

            dha = dabs(dble(ra)-dble(sidt))				!Calc Air Mass
            ddec = dble(dec)
            dzd = sla_zd ( dha, ddec, dlat )
            airm = sla_airmas ( dzd )
         endif

      endif

      if ( DOHFILT ) then						!Extinction
         call gtdesc ( 'IN', FILTNAME, text, ' ', iv, istat )		!Get image filter and match
         if ( ST_FAILED ) return
         if ( text.eq.' ' .or. istat.ne.0 ) then
            call printo ( 'ERROR: No Filter in image descriptors' )
         else
            filter = text(LOCFILT(1):LOCFILT(2))			! with set of filter names
            jlen = LOCFILT(2) - LOCFILT(1) + 1
            numf = 0
            k = 1
            do while ( k.le.NUMFILT )
               if ( filter(1:jlen).eq.FNAME(k)(1:jlen) ) numf = k
               k = k + 1
            enddo
            if ( numf.ne.0 ) then					!Load extinction
               extinc = EXTVAL(numf)
               nfilt = filter(1:4)
            else
               call printo ( 'ERROR: Filter name not recognised' )
               call pargc ( text )
               call printd ( '       Filter descriptor: %c')
            endif
            extinc = extinc*(airm-1.0)
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIR_FIRSTSTR -- Puts blanks after 1st contiguous characters in a character string
C
C  alan penny                ral                1992 Oct

      subroutine air_firststr ( text )

      implicit none

      character*(*)     text		!i/o: String to strip later parts off
C--
      integer k, klen, jlen
Cbegin


      klen = len(text)
      jlen = 0
      k = 1
      do while ( k.le.klen .and. jlen.eq.0 )
         if ( text(k:k).eq.' ' ) jlen = k
         k = k + 1
      enddo
      if ( jlen.ne.0 ) text(jlen:klen) = ' '


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AIR_STRCON3 -- Convert string into 3 reals
C
C alan penny                     ral            1990-03-09

      subroutine air_strcon3 ( str, r1, r2, r3, istat )

      implicit none

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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    ALTER.F
C
C    Contains:-
C
C T_ALTER        Perform the ALTER program function
C ALT_OPTION_SETUP  Set up options choice
C ALT_MAINGET    Translate main prog params to 'alter' ones
C ALT_SRMAINGET  Get main prog params to temp ones
C ALT_MAINPUT    Translate 'alter' params to main prog ones
C ALT_SRINSGET   Get 'alter' s/r params to temp ones
C ALT_OPDISP	 Open display
C ALT_LOADIM     Load image into work array
C ALT_IMLOAD(RS) Load real:short image into work array
C ALT_BLANK      Put areas to invalid by cursor or key
C ALT_CUT        Cut an image out by cursor or key
C ALT_VALUE      Set pixel value
C ALT_FLIP       Flip image
C ALT_TRANS      Transform image in x and y
C ALT_MAGNIFY    Magnify image
C ALT_ROTATE     Rotate image
C ALT_ROT_A      Rotate image by 90,180,270
C ALT_ROT_B      Do rotate image by 90,180,270
C ALT_ATRANS     Transform image in x and y
C ALT_DOTRAN     Do the transform image in x and y
C ALT_SMOOTH     Smooth image
C ALT_GAUSSR     Smooth a real flagged array with gaussian
C ALT_MEDIANR    Smooth a real flagged array with boxed median
C ALT_SETVR      Get a replacement real value for an INVALID pixel
C ALT_COMPRESS   Compress image
C ALT_DOCOMP     Do the binning of an image
C ALT_STORE      Store image on disk
C ALT_UNSHARP    Unsharp mask image


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_ALTER -- Perform the ALTER program function
C
C   a j penny                 ral      1994 Oct

      subroutine t_alter ( )

      implicit none
      include 'alter.inc'
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ierr, kx, ky, kb, ipw
      logical loop
      character*12 ktopt
Cbegin


      if ( ST_FAILED ) return

      MAIN_SELECT = ' '

      call alt_mainget				  			!Bring in new info

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got yet' )
         return
      endif

      call alt_loadim ( ierr )						!Load image to work array
      if ( ierr.ne.0 ) return

      call alt_option_setup ( ktopt, 6, .true. )
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )							!Loop thru options

         call alt_option_setup ( ktopt, 6, .false. )
         call get_choice ( ktopt, 1 ) 					!Get choice

         if ( ktopt.eq.'display' .or. ktopt.eq.'flash' .or.		!Check display open
     +        ktopt.eq.'zoom' .or. ktopt.eq.'reset' .or.
     +        ktopt.eq.'cvalues' .or. ktopt.eq.'clear' )
     +      call alt_opdisp ( ierr )

         if ( ktopt.eq.'blank_c' ) call alt_blank ( %val(IPIM), 1 )	!Put areas to invalid by cursor

         if ( ktopt.eq.'blank_k' ) call alt_blank ( %val(IPIM), 2 )	!Put areas to invalid by keyboard

         if ( ktopt.eq.'calculate' ) call alt_calc			!Calculate new image

         if ( ktopt.eq.'cut_c' ) call alt_cut ( %val(IPIM), 1 )		!Cut an image out by cursor

         if ( ktopt.eq.'cut_k' ) call alt_cut ( %val(IPIM), 2 )		!Cut an image out by keyboard

         if ( ktopt.eq.'flipx' ) then					!Flip image in x direction
                                    call gtwrkr ( 'IPWK', NX, ipw,ierr)
                                    if ( ierr.ne.0 ) return
                                    call alt_flip ( %val(IPIM),
     +                                              %val(ipw), 0 )
                                    call wrkcan ( 'IPWK' )
                                    call ds_erase
                                    call ds_acimr ( %val(IPIM), NX,
     +                              NY, DSNXS, DSNXE, DSNYS, DSNYE,
     +                              DSIXS, DSIYS, DSWRAP )
                                 endif

         if ( ktopt.eq.'flipy' ) then 					!Flip image in y direction
                                    call gtwrkr ( 'IPWK', NY, ipw,ierr)
                                    if ( ierr.ne.0 ) return
                                    call alt_flip ( %val(IPIM),
     +                                              %val(ipw), 1 )
                                    call wrkcan ( 'IPWK' )
                                    call ds_erase
                                    call ds_acimr ( %val(IPIM), NX,
     +                              NY, DSNXS, DSNXE, DSNYS, DSNYE,
     +                              DSIXS, DSIYS, DSWRAP )
                                 endif

        if ( ktopt.eq.'transform' ) call alt_trans			!Transform image in x and y

        if ( ktopt.eq.'rotate' ) call alt_rotate			!Rotate image

        if ( ktopt.eq.'unsharp' ) call alt_unsharp			!Unsharp mask image

        if ( ktopt.eq.'value_k' ) call alt_value ( %val(IPIM) )		!Set pixel value

        if ( ktopt.eq.'magnify' ) call alt_magnify			!Magnify image

        if ( ktopt.eq.'compress' ) call alt_compress			!Compress image

        if ( ktopt.eq.'weed' ) call alt_weed ( %val(IPIM) )		!Set range of values to INVALID

        if ( ktopt.eq.'smooth' ) then 		 			!Smooth image
                                 call alt_smooth ( 2 )
                                 call ds_acimr ( %val(IPIM), NX, NY,
     +                           DSNXS, DSNXE, DSNYS, DSNYE, DSIXS,
     +                           DSIYS, DSWRAP )
                                 endif

        if ( ktopt.eq.'store' ) call alt_store 				!Store present image

        if ( ktopt.eq.'drange' ) then					!Get display range
                                  call ds_imgscl ( %val(IPIM), NX, NY,
     +                            IMTYPE, 1, NX, 1, NY )
                                  DSKVRANGE = 1
                                  endif

        if ( ktopt.eq.'image' ) then                 			!Get ne$
                                   call in_newim ( ierr )
                                   call alt_loadim ( ierr )
                                   if ( ierr.ne.0 ) return
                                endif

        if ( ktopt.eq.'im_get_flash' ) then				!Input $
                                         call in_newim ( ierr )
                                         if ( .not.ST_FAILED .and.
     +                                        ierr.eq.0 ) then
                                         call ds_doflash ( %val(IPIM),
     +                                   NX, NY, IMTYPE, DSKVRANGE,
     +                                   IMTITLE )
                                         DISPLAYED = .true.
                                         call alt_loadim ( ierr )
                                         if ( ierr.ne.0 ) return
                                         endif
                                      endif

         if ( ktopt.eq.'display' ) then					!Display image
                       call ds_dodisp ( %val(IPIM), NX, NY, IMTYPE,
     +                                  DSKVRANGE, IMTITLE )
                       DISPLAYED = .true.
                       endif

         if ( ktopt.eq.'flash' ) then					!Flash image
                       call ds_doflash ( %val(IPIM), NX, NY, IMTYPE,
     +                                   DSKVRANGE, IMTITLE )
                       DISPLAYED = .true.
                       endif

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom/pan display

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Reset display zoom/pan

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb, 	!Get images values by cursor
     +                                             ierr )

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice

         if ( ktopt.eq.'clear' ) call ds_erase 				!Clear screen

         if ( ktopt.eq.'close' ) then                                   !Close display screen
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )                 !Open display screen

         if ( ktopt.eq.'main' .or. 					!Return to main program
     +        ktopt.eq.'aperture' .or. ktopt.eq.'colour' .or.
     +        ktopt.eq.'inspect' .or. ktopt.eq.'fit_magns' .or.
     +        ktopt.eq.'positions' .or. ktopt.eq.'scrutiny' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo

      call alt_mainput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_OPTION_SETUP -- Set up options choice
C
C   alan penny                        ral              1990-01-31

      subroutine alt_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'

      character*12   ktopt		!i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside		!i: Is this called from outside loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=36 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return.' /

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'im_get_flash', 'Input new image and display it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, without displaying an image.',
     + 'The screen may be any size you want, through keyboard entry',
     + 'via the SCREEN parameter.',
     + ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from program. Do not access main option list first.',
     + ' ', ' ', ' ', ' '/

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'flipx', 'Flip image along x direction',
     + ' ',
     + 'Image is reversed along x (horizontal) direction and ',
     + 'displayed.',
     + ' ', ' ', ' '/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'flipy', 'Flip image along y direction',
     + ' ',
     + 'Image is reversed along y (vertical) direction and ',
     + 'displayed.',
     + ' ', ' ', ' '/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'transform', 'Transform image in X and Y',
     + 'The user inputs the parameters of the transform equations ',
     + 'x(new) = a1 + a2*x(old) + a3*y(old) ',
     + 'y(new) = a4 + a5*x(old) + a6*y(old) ',
     + 'The image is transformed by giving each new pixel the value',
     + 'of the nearest original pixel. Image shifted to lie against',
     + 'X and Y axes. Image displayed (in larger window if needed).'/

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'smooth', 'Smooth image',
     + 'The image is smoothed and displayed.',
     + ' ',
     + 'Three smoothing techniques are available:- ',
     + 'Box    - Run box over image, replacing pixels by box average',
     + 'Gauss  - Run a 2-D Gaussian (in finite box) over image',
     + 'Median - Same as Box, but median instead of average'/

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'rotate', 'Rotate image',
     + 'Image is rotated, counter-clockwise, by given number of',
     + 'degrees. ',
     + 'The image is transformed by giving each new pixel the value',
     + 'of the nearest original pixel. Image shifted to lie against',
     + 'X and Y axes. ',
     + 'Image displayed (in larger window if needed).'/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'compress', 'Compress image in X and Y by integer factors',
     + 'Compression technique is one of three options:-',
     + ' Average - Take average of all pixels',
     + ' Highest - Take highest pixel',
     + ' Sample  - Take value of bottom left hand pixel in box',
     + 'This should be used before transformations which involve',
     + 'a lot of compression.'/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'magnify', 'Magnify image in X and Y by real factors',
     + 'This allows a magnification by  factors in X and in Y.',
     + ' ',
     + 'These factors must lie between 1.0e-10 and 1.0e20. This is ',
     + 'really designed for magnification. If the factor is less than ',
     + 'about 0.8, then use the -compress- option.',
     + ' '/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'drange', 'Change pixel value display range',
     + 'The present working image is displayed over a certain pixel',
     + 'value range. ',
     + 'This enables you to choose a different range, and the image ',
     + 'is then displayed over that range.',
     + ' ', ' '/

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'store', 'Store present image to disk',
     + 'Take the present working image and copy it to disk. ',
     + ' ',
     + 'This does not remove it from the present working array,',
     + 'but enables you to keep it, separate from future -alter- ',
     + 'actions. ',
     + ' '/

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'unsharp', 'Unsharp mask the image',
     + 'Unsharp masking removes large scale structure to show fine ',
     + 'detail where a high background drowns out the detail. This is',
     + 'done smoothing the image, and subtracting from the original.',
     + 'Deciding on how much smoothing and subtraction is an art. ',
     + 'One can smooth at two scales and subtract different amounts. ',
     + 'A single median smooth and 80% subtraction is set as default.'/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'blank_c', 'Put areas of image to invalid with cursor',
     + 'Place the cursor within the image. Press left or centre',
     + 'button. Move cursor and repeat. The rectangle defined by',
     + 'those two points is set to INVALID.',
     + ' ',
     + 'Repeat as needed. When finished press right button to return',
     + 'to panel choices.' /

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'blank_k', 'Put areas of image to invalid with keyboard',
     + 'Input on the keyboard one line with the X,Y coords of one ',
     + 'point followed by the X,Y coords of another. The rectangle ',
     + 'defined by those two points is set to INVALID.',
     + ' ',
     + 'Repeat as needed. When finished enter a line with a -ve posn',
     + 'to return to panel choices.' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'weed', 'Set range of pixel values to INVALID',
     + 'The user inputs a range of pixel values and these are',
     + 'set to the INVALID flag value. Many Starman programs',
     + 'will then ignore those pixels. The display will show them',
     + 'as white.',
     + ' ', ' ' /

      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'cut_c', 'Use cursor to cut out new rectangular image',
     + 'Use the cursor to define one point on the image. Then',
     + 'use it again to mark another point. The rectangle defined',
     + 'by those two points is then made into a new image and',
     + 'displayed.',
     + ' ', ' ' /

      data opt_text(34),opt_head(34),(opt_help(j,34),j=1,6) /
     + 'cut_k', 'Use keyboard to cut out new rectangular image',
     + 'Input on the keyboard one line with the X,Y coords of one ',
     + 'point followed by the X,Y coords of another. The rectangle',
     + 'defined by those two points is then made into a new image ',
     + 'and displayed.',
     + ' ', ' ' /

      data opt_text(35),opt_head(35),(opt_help(j,35),j=1,6) /
     + 'value_k', 'Set value of pixel by the keyboard',
     + 'Enter the X,Y position and pixel value for a pixel. The',
     + 'pixel is set to that value and the pixel displayed.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(36),opt_head(36),(opt_help(j,36),j=1,6) /
     + 'calculate', 'Calculate new image with Fortan-like equation',
     + 'The present image, or no image, may be combined with other',
     + 'images, constants, variables, etc., to make a new image',
     + 'which is displayed.',
     + ' ', ' ', ' ' /

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact - Alter', 'ALOPTION', 6 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'value_k' /

      integer sect_num
      parameter ( sect_num=5 )
      character*10 sect_head(sect_num)
      data sect_head /'GENERAL', 'IMAGE',
     +                'DISPLAY', 'FUNCTIONS', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'blank_c:blank_k:calculate:compress:cut_c:
     +                     cut_k:flipx:flipy:magnify:rotate:
     +                     smooth:transform:unsharp:value_k:weed' /
      data sect_text(2) / 'drange:image:im_get_flash:store' /
      data sect_text(3) / 'clear:close:cvalues:display:
     +                     flash:open:reset:zoom' /
      data sect_text(4)/  'aperture:colour:fit_magns:
     +                     inspect:main:positions:scrutiny' /
      data sect_text(5) / 'panel:exit' /

      integer help_num
      parameter ( help_num=33 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ' ,
     + 'For workstation use, the Cursor must be in image window area',
     + 'for the cursor buttons to work when in -WORKING- mode. The ' ,
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' ',
     + ' Buttons for Blink:  Left Button   = slow blink x2' ,
     + '                     Centre Button = speed blink x2' ,
     + '                     Right Button  = switch to hand blink' /
      data (help_text(k),k=11,20) /
     + '     Hand blink operates by keyboard' ,
     + ' ',
     + ' Buttons for Radius: Left Button    = Variable radius' ,
     + '                     Centre Button  = Fixed radius' ,
     + '                     Right Button   = return' ,
     + ' ' ,
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' ,
     + '     Centre Button twice            = zoom x2' ,
     + '     Left Button then Centre Button = pan' /
      data (help_text(k),k=21,30) /
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Zoom means zoom around present position of cursor ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' ,
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' /
      data (help_text(k),k=31,help_num) /
     + ' X windows       YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits ' /

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
C ALT_MAINGET -- Get main prog params to 'alter' ones
C
C   a j penny                 ral               1990-06-09

      subroutine alt_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call alt_srmainget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine alt_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_MAINPUT -- Put 'alter' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine alt_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call alt_srinsget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SRINSGET -- Get 'alter' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine alt_srinsget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'

C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_OPDISP -- Open display
C
C alan penny                    ral                  1990-06-16

      subroutine alt_opdisp ( ierr )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( ST_FAILED ) return

      if ( OPDISP ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return

      call ds_init ( IMTITLE, 0, ierr )					!Open display
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return
      OPDISP = .true.

      call ds_gtcomf ( 1 )						!Get image display size compression


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_LOADIM -- Load image into work array
C
C alan penny                    ral                  1990-06-16

      subroutine alt_loadim ( ierr )

      implicit none
      include 'alter.inc'
      include 'interact.inc'
      include 'ST_DS_GEN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
      integer ipa, ipc, istat
Cbegin


      if ( ST_FAILED ) return
      ierr = 0

      call gtwrkr ( 'WORKC', NX*NY, ipc, istat )
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Cant open ALTER workspace WORKC' )
         call printo ( '       Returning to main menu' )
         ierr = 1
         return
      endif

      if ( IMTYPE.eq.'REAL' ) then
         call alt_imloadr ( %val(IPIM), %val(ipc), NX, NY )
      else
         call alt_imloads ( %val(IPIM), %val(ipc), NX, NY )
      endif

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )

      call gtwrkr ( 'WORKALT', NX*NY, ipa, istat )
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Cant open ALTER workspace WORKALT' )
         call printo ( '       Returning to main menu' )
         ierr = 1
         return
      endif
      WORK_LOAD = .true.

      call amovr ( %val(ipc), %val(ipa), NX*NY )

      call wrkcan ( 'WORKC' )

      DSVMIN = BS*DSVMIN + BZ
      DSVMAX = BS*DSVMAX + BZ

      IPIM = ipa
      RINVAL = INT_INVALR
      IMTYPE = 'REAL'
      BS = 1.0
      BZ = 0.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_IMLOADS -- Load short image into work array
C
C alan penny                    ral                  1990-06-16

      subroutine alt_imloads ( in, out, nxa, nya )

      implicit none
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'

      integer*2 in(NX,NY)	!i: Input image
      integer   nxa		!i: X size of output image
      integer   nya		!i: Y size of output image
      real      out(nxa,nya)	!o: Output image
C--
      integer j, k, ja, ka
Cbegin


      if ( ST_FAILED ) return

      if ( BS.eq.1.0 .and. BZ.eq.0.0 ) then
         do k = 1, nya
            do j = 1, nxa
               ja = j + DSNXS - 1
               ka = k + DSNYS - 1
               if ( in(ja,ka).eq.INVAL ) then
                  out(j,k) = INT_INVALR
               else
                  out(j,k) = BS*real(in(ja,ka)) + BZ
               endif
            enddo
         enddo
      else
         do k = 1, nya
            do j = 1, nxa
               ja = j + DSNXS - 1
               ka = k + DSNYS - 1
               if ( in(ja,ka).eq.RINVAL ) then
                  out(j,k) = INT_INVALR
               else
                  out(j,k) = real(in(ja,ka))
               endif
            enddo
         enddo
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_IMLOADR -- Load real image into work array
C
C alan penny                    ral                  1990-06-16

      subroutine alt_imloadr ( in, out, nxa, nya )

      implicit none
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'

      real      in(NX,NY)	!i: Input image
      integer   nxa		!i: X size of output image
      integer   nya		!i: Y size of output image
      real      out(nxa,nya)	!o: Output image
C--
      integer j, k, ja, ka
Cbegin


      if ( ST_FAILED ) return

      if ( BS.eq.1.0 .and. BZ.eq.0.0 ) then
         do k = 1, nya
            do j = 1, nxa
               ja = j + DSNXS - 1
               ka = k + DSNYS - 1
               if ( in(ja,ka).eq.RINVAL ) then
                  out(j,k) = INT_INVALR
               else
                  out(j,k) = BS*in(ja,ka) + BZ
               endif
            enddo
         enddo
      else
         do k = 1, nya
            do j = 1, nxa
               ja = j + DSNXS - 1
               ka = k + DSNYS - 1
               if ( in(ja,ka).eq.RINVAL ) then
                  out(j,k) = INT_INVALR
               else
                  out(j,k) = in(ja,ka)
               endif
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_FLIP -- Flip image
C
C alan penny                    ral                  1990-06-16

      subroutine alt_flip ( in, wk, kdir )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      in(NX,NY)	!i/o: Input/output image
      real      wk(*)		!o: Work space
      integer   kdir		!i: 0 = x direction; 1 = y direction
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      if ( kdir.eq.0 ) then
         do k = 1, NY
            do j = 1, NX
               wk(j) = in((NX-j+1),k)
            enddo
            do j = 1, NX
               in(j,k) = wk(j)
            enddo
         enddo
      else
         do k = 1, NX
            do j = 1, NY
               wk(j) = in(k,(NY-j+1))
            enddo
            do j = 1, NY
               in(k,j) = wk(j)
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_BLANK -- Set areas to invalid by cursor or keyboard
C
C alan penny                    ral                  1995 July

      subroutine alt_blank ( im, kopt )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real       im(NX,NY)      !i/o: image
      integer    kopt		!i: Method (1=cursor;2=keyboard)
C--
      integer lxs, lxe, lys, lye, kb, j, k, ix, iy, ierr,
     +        jxs, jxe, jys, jye, jx, jy, kcc
      logical more
      real    x, y, acs
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 .and. .not.DISPLAYED ) then
         call printo ( 'ERROR: Not displayed' )
         return
      endif

      lxs = 1
      lxe = 1
      lys = 1
      lye = 1
      more = .true.
      do while ( more )

         if ( kopt.eq.1 ) then						!Get area
            call ds_gcur ( .true., lxs, lys, kb, ierr )
            if ( ierr.ne.0 ) call printo ( 'ERROR: Invalid entry' )
            if ( ierr.ne.0 .or. kb.eq.3 ) then
               more = .false.
            else
               x = lxs
               y = lys
               acs = 4
               kcc = 4
               call ds_cross ( x, y, acs, kcc )
               call ds_gcur ( .true., lxe, lye, kb, ierr )
               if ( ierr.ne.0 ) call printo ( 'ERROR: Invalid entry' )
               if ( ierr.ne.0 .or. kb.eq.3 ) more = .false.
            endif
         else
            call get4i ( 'AREA', lxs, lys, lxe, lye, .true.,
     +                   -1000000, 1000000 )
            if ( lxs.lt.1 .or.lxe.lt.1 .or. lys.lt.1 .or.
     +           lye.lt.1 ) more = .false.
         endif

         if ( more ) then

            if ( kopt.eq.1 ) then
               jxs = lxs - acs - 2
               jxe = jxs + 2.0*acs + 4
               jys = lys - acs - 2
               jye = jys + 2.0*acs + 4
               call ds_tiv ( jxs, jys, jx, jy )
               call ds_acim ( im, NX, NY, 'REAL', jxs, jxe,
     +                        jys, jye, jx, jy, .false. )
            endif

            call cswopi ( lxs, lxe )
            call cswopi ( lys, lye )
            lxs = max(1,min(lxs,NX))
            lxe = max(1,min(lxe,NX))
            lys = max(1,min(lys,NY))
            lye = max(1,min(lye,NY))

            do k = lys, lye
               do j = lxs, lxe
                  im(j,k) = RINVAL
               enddo
            enddo

            if ( DISPLAYED ) then
               call ds_tiv ( lxs, lys, ix, iy )
               call ds_acim ( im, NX, NY, 'REAL', lxs, lxe, lys, lye,
     +                        ix, iy, .false. )
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_CUT -- Cut an image out by cursor or keyboard
C
C alan penny                    ral                  1995 July

      subroutine alt_cut ( im, kopt )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real       im(NX,NY)      !i/o: image
      integer    kopt		!i: Method (1=cursor;2=keyboard)
C--
      integer lxs, lxe, lys, lye, kb, nxa, nya, ierr, kcc, ipa,
     +        iptr
      real    x, y, acs
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 .and. .not.DISPLAYED ) then
         call printo ( 'ERROR: Not displayed' )
         return
      endif

      lxs = 1
      lxe = 1
      lys = 1
      lye = 1

      if ( kopt.eq.1 ) then						!Get area

         call ds_gcur ( .true., lxs, lys, kb, ierr )
         if ( ierr.ne.0 ) call printo ( 'ERROR: Invalid entry' )
         if ( ierr.ne.0 .or. kb.eq.3 ) then
            return
         else
            x = lxs
            y = lys
            acs = 4
            kcc = 4
            call ds_cross ( x, y, acs, kcc )
            call ds_gcur ( .true., lxe, lye, kb, ierr )
            if ( ierr.ne.0 ) call printo ( 'ERROR: Invalid entry' )
            if ( ierr.ne.0 .or. kb.eq.3 ) return
         endif

      else
         call get4i ( 'AREA', lxs, lys, lxe, lye, .true.,
     +                -1000000, 1000000 )
         if ( lxs.lt.1 .or.lxe.lt.1 .or. lys.lt.1 .or.
     +        lye.lt.1 ) then
            call printo ( 'ERROR: Not in image' )
            return
         endif

      endif


      call cswopi ( lxs, lxe )
      call cswopi ( lys, lye )
      lxs = max(1,min(lxs,NX))
      lxe = max(1,min(lxe,NX))
      lys = max(1,min(lys,NY))
      lye = max(1,min(lye,NY))

      nxa = lxe - lxs + 1
      nya = lye - lys + 1

      call pargi ( nxa )
      call pargi ( nya )
      call printd ( '  New image is a %dx%d image ' )

      call gtwrkr ( 'IMTR', nxa*nya, iptr, ierr )
      if ( ierr.ne.0 ) return

      call coprr ( %val(IPIM), NX, NY, lxs, lxe, lys, lye,
     +             %val(iptr), nxa, nya, 1, 1 )

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )
      call gtwrkr ( 'WORKALT', nxa*nya, ipa, ierr )
      call amovr ( %val(iptr), %val(ipa), nxa*nya )
      call wrkcan ( 'IMTR' )

      NX = nxa
      NY = nya
      DSNXE = NX
      DSNYE = NY
      IPIM = ipa

      call ds_erase
      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE,
     +                DSNYS, DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_VALUE -- Set pixel value
C
C alan penny                    ral                  1995 July

      subroutine alt_value ( im )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real       im(NX,NY)      !i/o: image
C--
      integer kx, ky, ix, iy
      real    ax, ay, val
Cbegin


      if ( ST_FAILED ) return

      if ( .not.DISPLAYED ) then
         call printo ( 'ERROR: Not displayed' )
         return
      endif

      ax = 1.0
      ay = 1.0
      val = 1.0
      call get3r ( 'PIXVAL', ax, ay, val, .true., -1.0e20, 1.0e20 )

      if ( abs(ax).gt.1.0e5 .or. abs(ay).gt.1.0e5 ) then
         call printo ( 'ERROR: Pixel position invalid' )
         return
      endif

      kx = nint(ax)
      ky = nint(ay)
      if ( kx.lt.1. or. kx.gt.NX .or. ky.lt.1. or. ky.gt.NY ) then
         call printo ( 'ERROR: Pixel position outside image' )
         return
      endif

      im(kx,ky) = val

      call ds_erase
      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE,
     +                DSNYS, DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_TRANS -- Transform image in x and y
C
C alan penny                    ral                  1994 Oct

      subroutine alt_trans ( )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
C--
      real trc(6)
Cbegin


      if ( ST_FAILED ) return

      trc(1) = 0.0
      trc(2) = 1.0
      trc(3) = 0.0
      trc(4) = 0.0
      trc(5) = 0.0
      trc(6) = 1.0

      call get3r ( 'XCOEFF', trc(1), trc(2), trc(3), .true.,
     +             -1.0e8, 1.0e8 )
      call get3r ( 'YCOEFF', trc(4), trc(5), trc(6), .true.,
     +             -1.0e8, 1.0e8 )

      call alt_atrans ( trc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_MAGNIFY -- Magnify image
C
C alan penny                    ral                  1994 Oct

      subroutine alt_magnify ( )

      implicit none
      include 'STARMAN_INC'
C--
      real trc(6), xmag, ymag
Cbegin


      if ( ST_FAILED ) return

      call get1r ( 'XMAGNIFY', xmag, 1.0, 1.0, 1.0e20 )			!Get magnification

      call get1r ( 'YMAGNIFY', ymag, 1.0, 1.0, 1.0e20 )			!Get magnification

      if ( ST_FAILED ) return

      trc(1) = 0.0
      trc(2) = xmag
      trc(3) = 0.0
      trc(4) = 0.0
      trc(5) = 0.0
      trc(6) = ymag

      call alt_atrans ( trc )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_ROTATE -- Rotate image
C
C alan penny                    ral                  1994 Oct

      subroutine alt_rotate ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      real trc(6), rot, cenx, ceny, cs, sn
Cbegin


      if ( ST_FAILED ) return

      call get1r ( 'ROTATE', rot, 0.0, -1.0e20, 1.0e20 )		!Get rotation
      if ( ST_FAILED ) return

      if ( abs(rot).gt.360.0 ) then
         rot = mod(rot,360.0)
      elseif ( rot.eq.0.0 .or. rot.eq.360.0 .or. rot.eq.-360.0) then
         return
      endif

      if ( rot.eq.-90.0 ) rot = 270.0
      if ( rot.eq.-180.0 ) rot = 180.0
      if ( rot.eq.-270.0 ) rot = 90.0

      if ( rot.eq.90.0 .or. rot.eq.180.0 .or. rot.eq.270.0 ) then
         call alt_rot_a ( rot )
         return
      endif

      rot = rot*3.1415926536/180.0

      cenx = 1 + NX/2
      ceny = 1 + NY/2

      cs = cos(rot)							!Calc transformation
      sn = sin(rot)
      trc(1) = cenx - cenx*cs + ceny*sn
      trc(2) = cs
      trc(3) = -1.0*sn
      trc(4) = ceny - cenx*sn - ceny*cs
      trc(5) = sn
      trc(6) = cs

      call alt_atrans ( trc )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_ROT_A -- Rotate image by 90, 180, 270
C
C alan penny                    ral                  1994 Oct

      subroutine alt_rot_a ( rot )

      implicit none
      include 'alter.inc'
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real   rot	!i: angle to rotate (must be 90,180,270)
C--
      integer pnx, pny, ipa, iptr, ierr
Cbegin


      if ( ST_FAILED ) return

      if ( rot.ne.90.0 .and. rot.ne.180.0 .and. rot.ne.270.0 ) return

      if ( rot.eq.180.0 ) then
         pnx = NX
         pny = NY
      else
         pnx = NY
         pny = NX
      endif

      call pargi ( pnx )
      call pargi ( pny )
      call printd ( '  Image put into %dx%d image ' )

      call gtwrkr ( 'IMTR', pnx*pny, iptr, ierr )
      if ( ierr.ne.0 ) return

      call alt_rot_b ( %val(IPIM), %val(iptr), pnx, pny, rot )

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )
      call gtwrkr ( 'WORKALT', pnx*pny, ipa, ierr )
      call amovr ( %val(iptr), %val(ipa), pnx*pny )
      call wrkcan ( 'IMTR' )

      NX = pnx
      NY = pny
      DSNXS = 1
      DSNYS = 1
      DSNXE = NX
      DSNYE = NY
      IPIM = ipa
      call ds_erase

      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_ATRANS -- Transform image in x and y
C
C alan penny                    ral                  1994 Oct

      subroutine alt_atrans ( trc )

      implicit none
      include 'alter.inc'
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real trc(6)		!i: Transformation eqns parameters
C--
      integer px(4), py(4), pxs, pys, pnx, pny, ierr, ipa, iptr
      real rv
Cbegin


      if ( ST_FAILED ) return

      px(1) = trc(1) + 1.0*trc(2) + 1.0*trc(3)
      px(2) = trc(1) + NX*trc(2) + 1.0*trc(3)
      px(3) = trc(1) + 1.0*trc(2) + NY*trc(3)
      px(4) = trc(1) + NY*trc(2) + NY*trc(3)
      py(1) = trc(4) + 1.0*trc(5) + 1.0*trc(6)
      py(2) = trc(4) + NX*trc(5) + 1.0*trc(6)
      py(3) = trc(4) + 1.0*trc(5) + NY*trc(6)
      py(4) = trc(4) + NX*trc(5) + NY*trc(6)
      pxs = min(px(1),px(2),px(3),px(4))
      pys = min(py(1),py(2),py(3),py(4))
      pnx = max(px(1),px(2),px(3),px(4)) - pxs + 1
      pny = max(py(1),py(2),py(3),py(4)) - pys + 1

      trc(1) = trc(1) - pxs + 1
      trc(4) = trc(4) - pys + 1

      rv = trc(5)*trc(3) - trc(2)*trc(6)
      if ( abs(rv).lt.1.0e-20 ) then
         call printd ( 'ERROR: Transform Eqn is bad' )
         return
      endif

      call pargi ( pnx )
      call pargi ( pny )
      call printd ( '  Image put into %dx%d image ' )

      call gtwrkr ( 'IMTR', pnx*pny, iptr, ierr )
      if ( ierr.ne.0 ) return

      call alt_dotran ( %val(IPIM), %val(iptr), pnx, pny, trc,
     +                  pxs, pys )

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )
      call gtwrkr ( 'WORKALT', pnx*pny, ipa, ierr )
      call amovr ( %val(iptr), %val(ipa), pnx*pny )
      call wrkcan ( 'IMTR' )

      NX = pnx
      NY = pny
      DSNXE = NX
      DSNYE = NY
      IPIM = ipa
      call ds_erase

      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_ROT_B -- Do the rotation by 90, 180, 270
C
C alan penny                    ral                  1994 Oct

      subroutine alt_rot_b ( in, out, nxa, nya, rot )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real    in(NX,NY)		!i: Input image
      integer nxa		!i: X size of output image
      integer nya		!i: Y size of output image
      real    out(nxa,nya)	!o: Output image
      real    rot		!i: 90,180,270 rotation angle
C--
      integer j, k, ja, ka
Cbegin


      if ( ST_FAILED ) return

      if ( rot.eq.90.0 ) then
         do k = 1, nya
            do j = 1, nxa
               ja = nxa - j + 1
               out(j,k) = in(k,ja)
            enddo
         enddo
      elseif ( rot.eq.180.0 ) then
         do k = 1, nya
            ka = nya - k + 1
            do j = 1, nxa
               ja = nxa - j + 1
               out(j,k) = in(ja,ka)
            enddo
         enddo
      elseif ( rot.eq.270.0 ) then
         do k = 1, nya
            ka = nya - k + 1
            do j = 1, nxa
               out(j,k) = in(ka,j)
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_DOTRAN -- Do the transform image in x and y
C
C alan penny                    ral                  1994 Oct

      subroutine alt_dotran ( in, out, nxa, nya, trc, pxs, pys )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real    in(NX,NY)		!i: Input image
      integer nxa		!i: X size of output image
      integer nya		!i: Y size of output image
      real    out(nxa,nya)	!o: Output image
      real    trc(6)		!i: Transforms
      integer pxs		!i: X start of transform
      integer pys		!i: Y start of transform
C--
      integer j, k, ja, ka
      real atrc(6), rv
Cbegin


      if ( ST_FAILED ) return

      rv = trc(5)*trc(3) - trc(2)*trc(6)
      if ( abs(rv).lt.1.0e-20 ) then
         call printd ( 'ERROR: Transform Eqn is bad' )
         return
      endif

      atrc(1) = (trc(1)*trc(6)-trc(3)*trc(4))/rv			!Invert transform
      atrc(2) = -1.0*trc(6)/rv
      atrc(3) = trc(3)/rv
      atrc(4) = (trc(2)*trc(4)-trc(5)*trc(1))/rv
      atrc(5) = trc(5)/rv
      atrc(6) = -1.0*trc(2)/rv

      do k = 1, nya
         do j = 1, nxa
            ja = atrc(1) + atrc(2)*j + atrc(3)*k
            ka = atrc(4) + atrc(5)*j + atrc(6)*k
            if(ja.lt.1 .or. ja.gt.NX .or. ka.lt.1 .or. ka.gt.NY) then
               out(j,k) = INT_INVALR
            else
               out(j,k) = in(ja,ka)
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SMOOTH -- Smooth image
C
C alan penny                    ral                  1994 Oct

      subroutine alt_smooth ( kdef )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      integer   kdef		!i: Default smoothing option (1=box;2=gauss;3=median)
C--
      integer nxa, nxb, nya, nyb, iv, ipwa, ipwb, ipwc, ipwd,
     +        ipwe, kmeth, ierr
      real rad
Cbegin


      if ( ST_FAILED ) return

      call get_job ( 'SMETHOD', 'gauss:box:median', kmeth, kdef, ' ',
     +               0 )

      if ( kmeth.eq.1 ) then
         call get1r ( 'GRADIUS', rad, 2.0, 1.0e-5, 1.0e10 )
         nxb = rad*10.0
         nyb = rad*10.0
         iv = max(NX,NY)
         nxb = min(nxb,(NX-1))
         nyb = min(nyb,(NY-1))
         if ( nxb.eq.(2*(nxb/2)) ) nxb = nxb + 1
         if ( nyb.eq.(2*(nyb/2)) ) nyb = nyb + 1
         call get2i ( 'GBOX', nxb, nyb, .true., 1, iv )
         if ( ST_FAILED ) return
         if ( nxb.eq.(2*(nxb/2)) ) then
            call printo ( 'WRONG: X Size of box must be odd' )
            return
         endif
         if ( nyb.eq.(2*(nyb/2)) ) then
            call printo ( 'WRONG: Y Size of box must be odd' )
            return
         endif
      else
         nxb = 3
         nyb = 3
         iv = max(NX,NY)
         call get2i ( 'BOX', nxb, nyb, .true., 1, iv )
         if ( ST_FAILED ) return
      endif

      if ( kmeth.eq.1 ) then
         nxa = NX + 2*(nxb/2)
         nya = NY + 2*(nyb/2)
         call gtwrkr ( 'WA', NX*NY, ipwa, ierr )
         call gtwrkr ( 'WB', nxb*nyb, ipwb, ierr )
         call gtwrkr ( 'WC', nxa*nya, ipwc, ierr )
         call alt_gaussr ( %val(IPIM), NX, NY, RINVAL, rad,
     +                     %val(ipwc), nxa, nya,
     +                     %val(ipwa), %val(ipwb), nxb, nyb )
         call amovr ( %val(ipwa), %val(IPIM), NX*NY )
         call wrkcan ( 'WA' )
         call wrkcan ( 'WB' )
         call wrkcan ( 'WC' )
      elseif ( kmeth.eq.2 ) then
         call gtwrkr ( 'WB', NX*NY, ipwb, ierr )
         call gtwrkr ( 'WC', NX*NY, ipwc, ierr )
         call gtwrkr ( 'WD', NX,    ipwd, ierr )
         call gtwrkr ( 'WE', NX,    ipwe, ierr )
         call smoothbr ( %val(IPIM), NX, NY, RINVAL, nxb, nyb,
     +                   1, %val(ipwb), %val(ipwc), %val(ipwd),
     +                   %val(ipwe) )
         call wrkcan ( 'WB' )
         call wrkcan ( 'WC' )
         call wrkcan ( 'WD' )
         call wrkcan ( 'WE' )
      elseif ( kmeth.eq.3 ) then
         call gtwrkr ( 'WB', NX*NY, ipwb, ierr )
         call gtwrkr ( 'WC', nxb*nyb, ipwc, ierr )
         call alt_medianr ( %val(IPIM), %val(ipwb), %val(ipwc),
     +                      nxb, nyb )
         call amovr ( %val(ipwb), %val(IPIM), NX*NY )
         call wrkcan ( 'WB' )
         call wrkcan ( 'WC' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_GAUSSR -- Smooth a real flagged array with gaussian
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine alt_gaussr ( rin, nx, ny, rinval, rad,
     +                        ria, nxa, nya, rio, ee, nxb, nyb )

      implicit none
      include 'STARMAN_INC'

      integer   nx                      !i: X size of input array
      integer   ny                      !i: Y size of input array
      real      rin(nx,ny)              !i: Input array
      real      rinval                  !i: Invalid pixel magic value flag
      real      rad			!i: Gaussian radius
      integer   nxa                     !i: X size of work array
      integer   nya                     !i: Y size of work array
      real      ria(nxa,nya)            !o: Work array
      real      rio(nx,ny)              !o: Output array
      integer   nxb                     !i: X size of area to smooth
      integer   nyb                     !i: Y size of area to smooth
      real      ee(nxb,nyb)             !o: Smmoth work array
C--
      integer j, ja, jb, jx, jy, jdx, k, ka, kb, kdy
      real ex, ey, esum, rv
Cbegin


      if ( ST_FAILED ) return

      call azeror ( ria, nxa*nya )
      call azeror ( rio, nx*ny )

      jb = nxb/2
      kb = nyb/2

      do k = 1, ny
         do j = 1, nx
            ja = j + jb
            ka = k + kb
            ria(ja,ka) = rin(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa
            if ( j.le.jb .or. j.gt.(nxa-jb) .or.
     +           k.le.kb .or. k.gt.(nya-kb) ) then
               ja = j - jb
               if ( j.le.jb ) then
                  ja = 1
               elseif ( j.gt.(nxa-jb) ) then
                  ja = nx
               endif
               ka = k - kb
               if ( k.le.kb ) then
                  ka = 1
               elseif ( k.gt.(nya-kb) ) then
                  ka = ny
               endif
               ria(j,k) = rin(ja,ka)
            endif
         enddo
      enddo


      esum = 0.0
      do k = 1, nyb
         kdy = k - kb
         ey = 1.0
         if ( kdy.ne.0 ) ey = exp(-1.0*((real(kdy)/rad)**2.0))
         do j = 1, nxb
            jdx = j - jb
            ex = 1.0
            if ( jdx.ne.0 ) ex = exp(-1.0*((real(jdx)/rad)**2.0))
            ee(j,k) = ex*ey
            esum = esum + ee(j,k)
         enddo
      enddo
      rv = 1.0/esum
      do k = 1, nyb
         do j = 1, nxb
            ee(j,k) = rv*ee(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa

            rv = ria(j,k)
            if(rv.eq.rinval)call alt_setvr(ria,nxa,nya,rinval,j,k,rv)
            do ka = 1, nyb
               jy = k - kb + ka - kb
               if ( jy.ge.1 .and. jy.le.ny ) then
                  kdy = ka - kb
                  do ja = 1, nxb
                     jx = j - jb + ja - jb
                     if ( jx.ge.1 .and. jx.le.nx ) rio(jx,jy) =
     +                                    rio(jx,jy) + rv*ee(ja,ka)
                  enddo
               endif
            enddo

         enddo
      enddo

      do k = 1, ny
         do j = 1, nx
            if ( ria(j,k).eq.rinval ) rio(j,k) = rinval
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_MEDIANR -- Smooth a real flagged array with boxed median
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine alt_medianr ( in, out, box, nxb, nyb )
      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      in(NX,NY)              !i: Input array
      real      out(NX,NY)             !o: Output array
      integer   nxb                    !i: X size of box
      integer   nyb                    !i: Y size of box
      real      box(nxb,nyb)           !o: work array
C--
      integer kx(2), ky(2), ierr, kxl, kyl, j, k
      real rm
Cbegin


      if ( ST_FAILED ) return

      kxl = (nxb-1)/2
      kyl = (nyb-1)/2
      do k = 1, NY
         ky(1) = k - kyl
         ky(2) = k + kyl
         ky(1) = min(NY,max(1,ky(1)))
         ky(2) = min(NY,max(1,ky(2)))
         do j = 1, NX
            kx(1) = j - kxl
            kx(2) = j + kxl
            kx(1) = min(NX,max(1,kx(1)))
            kx(2) = min(NX,max(1,kx(2)))
            call medianr ( in, NX, NY, box, kx, ky, RINVAL,
     +                     rm, ierr )
            out(j,k) = rm
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SETVR -- Get a replacement real value for an INVALID pixel
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine alt_setvr ( ra, nx, ny, rinval, ja, ka, rv )

      implicit none

      integer   nx                      !i: X size of array
      integer   ny                      !i: Y size of array
      real      ra(nx,ny)               !i: Input array
      real      rinval                  !i: Invalid pixel magic value flag
      integer   ja			!i: X Position
      integer   ka			!i: Y Position
      real      rv			!o: replacement value
C--
      integer j, k, kn, kna, nsum
      real sum
      logical found, some
Cbegin


      found = .false.
      some = .true.

      kn = 1
      do while ( .not.found .and. some )
         kn = kn + 2
         kna = kn/2
         some = .false.
         sum = 0.0
         nsum = 0
         do k = ka-kna, ka+kna
            if ( k.ge.1 .and. k.le.ny ) then
               do j = ja-kna, ja+kna
                  if ( j.ge.1 .and. j.le.nx ) then
                     some = .true.
                     if ( ra(j,k).ne.rinval ) then
                        found = .true.
                        sum = sum + ra(j,k)
                        nsum = nsum + 1
                     endif
                  endif
               enddo
            endif
         enddo

         if ( .not.some ) then
            rv = 0.0
         elseif ( found ) then
            rv = sum/real(nsum)
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_COMPRESS -- Compress image
C
C alan penny                    ral                  1994 Oct

      subroutine alt_compress ( )

      implicit none
      include 'alter.inc'
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ierr, ipo, nxst, nyst, nxo, nyo, lim, k, ipa
      logical bv, ignore
      character*7 sample

      integer nth
      parameter ( nth=6 )
      character*68 th(nth)
      data th /
     + 'Method of extracting output value from binning box',
     + 'Option   Choice',
     + '------   --------',
     + 'Average  Take average of all pixels',
     + 'Highest  Take highest pixel',
     + 'Sample   Take value of bottom left hand pixel in box'/
Cbegin


      if ( ST_FAILED ) return

      nxst = 1
      nyst = 1
      lim = max(NX,NY)
      call get2i ( 'BIN', nxst, nyst, .true., 1, lim )
      if ( ST_FAILED ) return

      if ( nxst.eq.1 .and. nyst.eq.1 ) return

      nxst = min(nxst,NX)
      nyst = min(nyst,NY)

      nxo = NX/nxst
      nyo = NY/nyst

      sample = 'sample '
      call get_job ('SAMPLE', 'sample:highest:average', k, 2, th,nth)
      if ( ST_FAILED ) return
      sample = 'sample '
      if ( k.eq.2 ) sample = 'highest'
      if ( k.eq.3 ) sample = 'average'
      if ( sample.eq.'average' ) then
         call get1b ( 'CHECK', bv, .false. )
         if ( ST_FAILED ) return
         ignore = .not.bv
      endif

      if ( ST_FAILED ) return

      call pargi ( nxo )
      call pargi ( nyo )
      call printd ( '  Image put into %dx%d image ' )

      call gtwrkr ( 'IMTR', nxo*nyo, ipo, ierr )
      if ( ierr.ne.0 ) return

      call alt_docomp ( %val(IPIM), %val(ipo), nxo, nyo, nxst,
     +                  nyst, sample, ignore )

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )
      call gtwrkr ( 'WORKALT', nxo*nyo, ipa, ierr )
      call amovr ( %val(ipo), %val(ipa), nxo*nyo )
      call wrkcan ( 'IMTR' )

      NX = nxo
      NY = nyo
      DSNXE = NX
      DSNYE = NY
      IPIM = ipa
      call ds_erase

      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_WEED -- Set range of valuesto invalid
C
C alan penny                    ral                  1994 Oct

      subroutine alt_weed ( im )

      implicit none
      include 'alter.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real im(NX*NY)		!i/o: image
C--
      real bot, top, rv
      integer j
Cbegin


      if ( ST_FAILED ) return

      bot = 0.0
      top = 0.0
      call get2r ( 'RANGE', bot, top, .true., -1.0e20, 1.0e20 )
      call cswopr ( bot, top )

      do j = 1, NX*NY
         rv = im(j)
         if ( rv.ne.RINVAL ) then
            if ( rv.ge.bot .and. rv.le.top ) im(j) = RINVAL
         endif
      enddo

      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_DOCOMP -- Do the binning of an image
C
C   alan penny                     ral           1994 Oct

      subroutine alt_docomp ( in, out, nxo, nyo, nxst, nyst,
     +                        sample, ignore )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      real     in(NX,NY)	!i: Input image
      integer  nxo		!i: X size of output image
      integer  nyo		!i: X size of output image
      real     out(nxo,nyo)	!o: Output image
      integer  nxst		!i: X size of bin
      integer  nyst		!i: X size of bin
      character*7 sample	!i: Sampling method
      logical  ignore		!i: Ignore bad pixels when averaging
C--
      integer j, jj, ja, jja, jb, k, kk, ka, kka, kb
      logical set
      real high, rv, rvs
Cbegin


      if ( SAMPLE.eq.'sample ' ) then

         ka = 0
         do kka = 1, nyo
            k = 1 + (kka-1)*nyst
            ka = ka + 1
            ja = 0
            do jja = 1, nxo
               j = 1 + (jja-1)*nxst
               ja = ja + 1
               out(ja,ka) = in(j,k)
            enddo
         enddo

      elseif ( SAMPLE.eq.'highest' ) then

         ka = 0
         do kka = 1, nyo
            k = 1 + (kka-1)*nyst
            ka = ka + 1
            ja = 0
            do jja = 1, nxo
               j = 1 + (jja-1)*nxst
               ja = ja + 1
               out(ja,ka) = RINVAL
               set = .false.
               do kk = 1, iabs(nyst)
                  kb = k + (kk-1)*nyst/iabs(nyst)
                  do jj = 1, iabs(nxst)
                     jb = j + (jj-1)*nxst/iabs(nxst)
                     if ( in(jb,kb).ne.RINVAL ) then
                        if ( .not.set ) then
                           high = in(jb,kb)
                           set = .true.
                        else
                           if(in(jb,kb).gt.high)high=in(jb,kb)
                        endif
                     endif
                  enddo
               enddo
               if ( set ) out(ja,ka) = high
             enddo
         enddo
      elseif ( SAMPLE.eq.'average' ) then
         ka = 0
         do kka = 1, nyo
            k = 1 + (kka-1)*nyst
            ka = ka + 1
            ja = 0
            do jja = 1, nxo
               j = 1 + (jja-1)*nxst
               ja = ja + 1

               rv = 0.0
               rvs = 0.0
               set = .false.
               do kk = 1, iabs(nyst)
                  kb = k + (kk-1)*nyst/iabs(nyst)
                  do jj = 1, iabs(nxst)
                     jb = j + (jj-1)*nxst/iabs(nxst)
                     if ( in(jb,kb).ne.RINVAL ) then
                        rv = rv + in(jb,kb)
                        rvs = rvs + 1.0
                     else
                        if ( .not.ignore ) set = .true.
                     endif
                  enddo
               enddo

               if ( rvs.lt.0.5 .or.set ) then
                  out(ja,ka) = RINVAL
               else
                  out(ja,ka) = rv/rvs
               endif
            enddo
         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_STORE -- Store image on disk
C
C alan penny                    ral                  1994 Oct

      subroutine alt_store ( )

      implicit none
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'STARMAN_INC'
C--
      integer ierr, ipo, num
      character*50 title, cv
Cbegin


      if ( ST_FAILED ) return

      call opimzw ( 'OUTIM', IMTYPE, ipo, NX, NY, .false., ierr )
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Image not stored' )
         return
      endif

      call amovr ( %val(IPIM), %val(ipo), NX*NY )

      call icopdes ( 'IN', 'OUTIM', ierr )
      call gtdesc ( 'IN', 'TITLE', cv, ' ', num, ierr )
      call get1c ( 'TITLEIM', title, cv, .true. )
      call ptdesc ( 'OUTIM', 'TITLE', title )

      call ptdesr ( 'OUTIM', 'BSCALE', BS )
      call ptdesr ( 'OUTIM', 'BZERO',  BZ )
      call ptdesr ( 'OUTIM', 'INVAL', RINVAL )

      call canpar ( 'OUTIM' )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_UNSHARP -- Unsharp mask image
C
C alan penny                    ral                  1994 Oct

      subroutine alt_unsharp ( )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ipma, ipmb, ipmc, ierr
      real frac
      logical doit, more
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr ( 'MASTA', NX*NY, ipma, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'MASTB', NX*NY, ipmb, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'MASTC', NX*NY, ipmc, ierr )
      if ( ierr.ne.0 ) return

      call amovr ( %val(IPIM), %val(ipma), NX*NY )
      call amovr ( %val(IPIM), %val(ipmb), NX*NY )

      call printo ( '  ' )
      call printo (
     + 'You must first smooth the image - default method is MEDIAN' )
      call printo ( '  ' )

      call alt_smooth ( 3 )

      call printo ( '  ' )
      call printo (
     +       'You can now do a second smooth at a different scale' )
      call printo ( ' - default is not to do so.' )
      call printo ( '  ' )
      call get1b ( 'DOIT', doit, .false. )

      if ( doit ) call alt_smooth ( 3 )

      call printo ( '  ' )
      call printo (
     + 'You now try subtracting various fractions of this from the' )
      call printo ( '  original image.  Default is 80%.' )
      call printo ( '  ' )

      call amovr ( %val(IPIM), %val(ipmc), NX*NY )

      more = .true.
      do while ( more )

         call get1r ( 'FRACTION', frac, 0.8, 0.0, 1.0e20 )
         call azmulkr ( %val(IPIM), RINVAL, frac, %val(ipmb), NX*NY )
         call azsubr ( %val(ipma), RINVAL, %val(ipmb), RINVAL,
     +                 %val(IPIM), NX*NY )
         call ds_imgscl ( %val(IPIM), NX, NY, IMTYPE, 1, NX, 1, NY )
         DSKVRANGE = 1
         call pargr ( DSVMIN )
         call pargr ( DSVMAX )
         call printd ( ' Display range: Min = %f  Max = %f ' )
         call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                   DSNYE, DSIXS, DSIYS, DSWRAP )

         call get1b ( 'MORE', more, .true. )
         if ( more ) call amovr ( %val(ipmc), %val(IPIM), NX*NY )

      enddo

      call wrkcan ( 'MASTA' )
      call wrkcan ( 'MASTB' )
      call wrkcan ( 'MASTC' )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALTER_CALC.F
C
C  Contains:-
C
C ALT_CALC     Do image maths
C ALT_OUTLOAD  Put calculated image to work array and display
C ALT_SDEF     Set up defaults
C ALT_GCL      Get input and output info from the command line
C ALT_SPACE    Get work space for line stack and line
C ALT_POLISH   Decode equation and sort into reverse Polish
C ALT_EQNERR   Put out equation error
C ALT_CDOIT    Combine the lines from the stack of images into one line
C ALT_IMDIFF   Get number of diff input images and point to stack
C ALT_VARDIFF  Get number of diff input variables and point to stack
C ALT_LOAD(RS) Copy image into a 3-D stack
C ALT_OUTLR    Load a line to the output image
C ALT_SORT     Sort identifiers to alphabetical order


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_CALC -- Do image calculations
C
C  alan penny                  ral              1991 Dec

      subroutine alt_calc ()

      implicit none
      include 'alter_calc.inc'
      include 'STARMAN_INC'
C--
      integer k, kk, ierr
Cbegin


      call alt_sdef							!Set up defaults

      call alt_gcl							!Get input/output info
      if ( ST_FAILED ) return

      call alt_space							!Get work space
      if ( ST_FAILED ) return

      do k = 1, NYO

         if ( ANYIN ) then						!Load input images into stack
            do kk = 1, NZ
               if ( IMTY(kk).eq.'SHORT' ) then
                  call alt_loads ( %val(IPC(kk)), NXC(kk), NYC(kk),
     +                              BSC(kk), BZC(kk), INVALI(kk), k,
     +                              kk, %val(IPSTK), NXO, NZ )
               else
                  call alt_loadr ( %val(IPC(kk)), NXC(kk), NYC(kk),
     +                              BSC(kk), BZC(kk), RINVALI(kk), k,
     +                              kk, %val(IPSTK), NXO, NZ )
               endif
            enddo
         endif

         call alt_cdoit ( %val(IPSTK), NXO, NZ, K, OPCODE, NOPCODE, 	!Do the calculations
     +                    IMP, IMPV, VAR, CON, %val(ipl), KSEED, ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif

         call alt_outlr ( %val(ipl), %val(IPCO), NXO, NYO, BSO,
     +                    BZO, RINVALO, k )

      enddo

      call alt_outload


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_OUTLOAD -- Put calculated image to work array and display
C
C  alan penny                         RAL                1991 Dec

      subroutine alt_outload ( )

      implicit none
      include 'interact.inc'
      include 'alter_calc.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ipa, istat
Cbegin


      if ( ST_FAILED ) then
         if ( WORK_CALC ) call wrkcan ( 'CALC' )
         WORK_CALC = .false.
         return
      endif

      if ( WORK_LOAD ) call wrkcan ( 'WORKALT' )
      call gtwrkr ( 'WORKALT', NXO*NYO, ipa, istat )
      WORK_LOAD = .true.
      call amovr ( %val(IPCO), %val(ipa), NXO*NYO )
      call wrkcan ( 'CALC' )
      WORK_CALC = .false.
      IPIM = ipa

      NX = NXO
      NY = NYO
      RINVAL = RINVALO
      IMTYPE = 'REAL'
      DSNXS = 1
      DSNXE = NX
      DSNYS = 1
      DSNYE = NY
      BS = 1.0
      BZ = 0.0

      call ds_imgscl ( %val(IPIM), NX, NY, IMTYPE, 1, NX, 1, NY )
      DSKVRANGE = 1
      call pargr ( DSVMIN )
      call pargr ( DSVMAX )
      call printd ( ' Display range: Min = %f  Max = %f ' )

      call ds_erase
      DSIXS = DSSNX/2 - (((DSNXE-DSNXS+1)-1)/DSCOMFX)/2
      DSIYS = DSSNY/2 - (((DSNYE-DSNYS+1)-1)/DSCOMFY)/2
      call ds_acimr ( %val(IPIM), NX, NY, DSNXS, DSNXE, DSNYS,
     +                DSNYE, DSIXS, DSIYS, DSWRAP )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SDEF -- Set up defaults
C
C  alan penny                         RAL                1991 Dec

      subroutine alt_sdef ( )

      implicit none
      include 'alter_calc.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
C--
Cbegin


      NXIM = NX
      NYIM = NY
      RINVALIM = RINVAL
      WORK_CALC = .false.
      WORK_STACK = .false.

      IPL = 1
      IPSTK = 1

      NXO = 1
      NYO = 1

      ANYIN = .false.
      NZ = 1
      BSO = 1.0
      BZO = 0.0
      INVALO = INT_INVALSI
      RINVALO = INT_INVALR

      call azeroi ( IMP, 70 )						!Zero locators for images and varaibles
      call azeroi ( IMPV, 70 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_GCL -- Get input and output info from the command line
C
C  alan penny                         RAL                1991 Dec

      subroutine alt_gcl ( )

      implicit none
      include 'alter_calc.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer k, ierr
      character title*50, texta*132, inim*3
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'EQN', texta, ' ', .false. )
      call lbgone ( texta )
      call charln ( texta, k )
      if ( k.eq.0 ) then
         call printo ( 'ERROR: Must have an equation' )
         return
      endif
      EXPRS = texta

      call alt_polish ( EXPRS, OPCODE, NOPCODE, IMID, NIM,		!Translate it into reverse polish notation
     +                   IMP, VARID, NVAR, IMPV, CON, NCON, ierr )
      if ( ierr.ne.0 ) then
         return
      endif

      call alt_imdiff							!How many diff images?

      if ( .not.ANYIN ) then						!Get images
         NXO = 1
         NYO = 1
         call get2i ( 'NXY', NXO, NYO, .true., 1, 100000 )
      else
         do k = 1, NZ

            if ( IMTOT(k)(2:2).eq.'A' ) then
               IPC(k) = IPIM
               NXC(k) = NXIM
               NYC(k) = NYIM
               IMTY(k) = 'REAL'
               BSC(k) = 1.0
               BZC(k) = 0.0
               texta = 'Main'
            else
               inim = 'IN'//IMTOT(k)(2:2)
               call opimzr ( inim, IPC(k), NXC(k), NYC(k), IMTY(k),
     +                       .false., ierr )
               if ( ierr.ne.0 ) then
                  ST_FAILED = .true.
                  return
               endif
               call gtimzd ( inim, IMTY(k), BSC(k), BZC(k), INVALI(k),
     +                       RINVALI(k), texta, ierr)
               if ( ierr.ne.0 ) then
                  ST_FAILED = .true.
                  return
               endif
            endif

            if ( k.eq.1 ) then
               NXO = NXC(k)
               NYO = NYC(k)
               title = texta(1:50)
            else
               NXO = min(NXO,NXC(k))
               NYO = min(NYO,NYC(k))
            endif
         enddo
      endif

      call alt_vardiff							!How many diff variables?

      if ( NVARTOT.gt.0 ) then						!Get input variables
         do k = 1, NVARTOT
            call get1r ( VARTOT(k), VAR(k), 0.0, -1.0e37, +1.0e37 )
         enddo
      endif

      KSEED = 1								!Random generator seed
      call uppcase ( EXPRS, texta )
      if ( index(texta,'RAN').ne.0 .or. index(texta,'GAUSS').ne.0 )
     +   then
         call get1i ( 'SEED', KSEED, 1234567891, 1200000001,
     +                        1400000001 )
         call ajseed ( KSEED )
      endif

      OUTTYPE = 'REAL'							!Output image type and scale
      BSO = 1.0
      BZO = 0.0
      RINVALO = RINVALIM

      call gtwrkr ( 'CALC', NXO*NYO, IPCO, ierr )			!Get temp output image
      if ( ierr.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      WORK_CALC = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SPACE -- Get work space for line stack and line
C
C  alan penny                RAL                1991 Dec

      subroutine alt_space ( )

      implicit none
      include 'alter_calc.inc'
      include 'STARMAN_INC'
C--
      integer ierr
Cbegin


      if ( WORK_STACK ) call wrkcan ( 'STACK' )
      WORK_STACK = .false.
      if ( .not.ANYIN ) then						!Work space for stack
         call gtwrkr ( 'STACK', 1, IPSTK, ierr )
         call azeror ( %val(IPSTK), 1 )
      else
         call gtwrkr ( 'STACK', NXO*NZ, IPSTK, ierr )
         call azeror ( %val(IPSTK), NXO*NZ )
      endif
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      WORK_STACK = .true.

      call gtwrkr ( 'LINE', NXO, IPL, ierr )				!Work space for stack
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_POLISH -- Decode equation and sort into reverse Polish
C
C  alan penny                RAL                1991 Dec

      subroutine alt_polish ( exprs, opcode, noper, imid,
     +                         nim, imp, varid, nvar, impv, con,
     +                         ncon, ierr)

      implicit none

      character*132    exprs		!i: Equation to decode
      character*6      opcode(70)	!o: Polish stack of commands
      integer          noper		!o: No of operations
      character*2      imid(26)		!o: images identifiers
      integer          nim		!o: Number of different images
      integer          imp(70)		!o: Pointers to images
      character*1      varid(70)	!o: Variables identifiers
      integer          nvar		!o: Number of variable
      integer          impv(70)		!o: Pointers to variables
      real             con(70)		!o: Constants
      integer          ncon		!o: Number of constants
      integer          ierr		!o: Error flag (0=ok;bad 2=;3=;4=)
C--

      character in*133, numchr*132, test*7, testa*7,
     +          output*7, fin*2, fina*3
      integer j, k, tos, stk(0:132), symb(132), ll, ncin, nnum,
     +        nsymb, iok, isymb, ndiff, istat, jj
      real    const
      logical opnext, found, ischar, isnumb, issign, lmore, atend,
     +        thisok, isfile
      external ischar, isnumb, issign

      integer maxsym
      parameter ( maxsym=38 )

      integer numendoper
      parameter ( numendoper=7 )
      character*1 endoper(numendoper)

      character*7 oper(-3:maxsym), opsymb(-3:maxsym)
      integer opl(-3:maxsym), opr(-3:maxsym), l(-3:maxsym),
     +        prl(-3:maxsym), prr(-3:maxsym)

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),		!Set up image of operators,
     +        prr(j),j=-3,10 ) /					! symbols and their priorities
     +  '       ', 'LDCON  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDVAR  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDIM   ', 2, 0, 0, 10, 10,
     +  '=      ', '=      ', 1, 1, 1,  0,  0,
     +  ')      ', ')      ', 1, 1, 0,  2, 10,
     +  '(      ', '(      ', 1, 0, 1, 10,  1,
     +  '-      ', '-      ', 1, 1, 1,  4,  4,
     +  '+      ', '+      ', 1, 1, 1,  4,  4,
     +  '**     ', '**     ', 2, 1, 1,  9,  6,
     +  '*      ', '*      ', 1, 1, 1,  5,  5,
     +  '/      ', '/      ', 1, 1, 1,  5,  5,
     +  ':      ', ':      ', 1, 1, 1,  2,  2,
     +  '-      ', 'NEG    ', 1, 0, 1,  8,  7,
     +  '+      ', 'POS    ', 1, 0, 1,  8,  7 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=11,20 ) /
     +  'SQRT(  ', 'SQRT(  ', 5, 0, 1, 10,  1,
     +  'EXP(   ', 'EXP(   ', 4, 0, 1, 10,  1,
     +  'LOG10( ', 'LOG10( ', 6, 0, 1, 10,  1,
     +  'LOG(   ', 'LOG(   ', 4, 0, 1, 10,  1,
     +  'SIN(   ', 'SIN(   ', 4, 0, 1, 10,  1,
     +  'COS(   ', 'COS(   ', 4, 0, 1, 10,  1,
     +  'TAN(   ', 'TAN(   ', 4, 0, 1, 10,  1,
     +  'ASIN(  ', 'ASIN(  ', 5, 0, 1, 10,  1,
     +  'ACOS(  ', 'ACOS(  ', 5, 0, 1, 10,  1,
     +  'ATAN(  ', 'ATAN(  ', 5, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=21,30 ) /
     +  'ATAN2( ', 'ATAN2( ', 6, 0, 1, 10,  1,
     +  'SINH(  ', 'SINH(  ', 5, 0, 1, 10,  1,
     +  'COSH(  ', 'COSH(  ', 5, 0, 1, 10,  1,
     +  'TANH(  ', 'TANH(  ', 5, 0, 1, 10,  1,
     +  'ABS(   ', 'ABS(   ', 4, 0, 1, 10,  1,
     +  'AINT(  ', 'AINT(  ', 5, 0, 1, 10,  1,
     +  'ANINT( ', 'ANINT( ', 6, 0, 1, 10,  1,
     +  'MOD(   ', 'MOD(   ', 4, 0, 1, 10,  1,
     +  'SIGN(  ', 'SIGN(  ', 5, 0, 1, 10,  1,
     +  'DIM(   ', 'DIM(   ', 4, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +       prr(j),j=31,maxsym ) /
     +  'MIN(   ', 'MIN(   ', 4, 0, 1, 10,  1,
     +  'MAX(   ', 'MAX(   ', 4, 0, 1, 10,  1,
     +  'XX     ', 'XX     ', 2, 0, 0, 10, 10,
     +  'YY     ', 'YY     ', 2, 0, 0, 10, 10,
     +  'CLIP(  ', 'CLIP(  ', 5, 0, 1, 10,  1,
     +  'GAUSS( ', 'GAUSS( ', 6, 0, 1, 10,  1,
     +  'RAN(   ', 'RAN(   ', 4, 0, 1, 10,  1,
     +  '%      ', '/      ', 1, 1, 1,  5,  5 /

      data endoper / '+', '-', '/', '*', ')', '=', ' ' /
Cbegin


      ierr = 0
      ncin = 0

      do k = 1, len(exprs)						!Remove embedded blanks
         if ( exprs(k:k).ne.' ' .and. ncin.lt.132  ) then		!Count no of characters
            ncin = ncin + 1
            in(ncin:ncin) = exprs(k:k)
         endif
      enddo

      exprs = in(1:ncin)						!Return the expression with blanks removed

      ncin = ncin + 1							!Append an '= ' operator to terminate the expression
      in(ncin:ncin) = '='

      nim = 0								!Initiallise counters
      nvar = 0
      ncon = 0
      k = 1
      j = 0

      opnext = .false.							!Indicates if an operator is expected next
									! first entity must not look like an operator

      lmore = .true.							!Loop thru OPLs until '='found
      do while ( lmore )
         found = .false.						!Search through the list of symbols to
         nsymb = -1							! identify which comes next
         do while ( nsymb.lt.maxsym .and. .not.found )
            nsymb = nsymb + 1
            if ( opnext.eqv.(opl(nsymb).eq.1) ) then			!Symbol is only valid if it looks like
               test = in(k:min(k+l(nsymb)-1,81))			! an operator or operand from the left, as appropriate
               call uppcase ( test, testa )
               if ( testa.eq.oper(nsymb) ) found = .true.
            endif
         enddo

         isfile = .false.
         if ( .not.found ) then						!If symbol was not found: -

            if ( opnext ) then						!Error if an operator expected
               ierr = 3
               call printo ( ' ' )
               call printo ( 'ERROR: Cant understand Equation - ' )
               call pargi ( k )
               call printd (
     +              'ERROR:   An Operator missing, at character: %d' )
               call printo (
     +              'ERROR:   - that is a plus, or a minus, etc')
               call alt_eqnerr ( exprs, k )
               return
            elseif ( in(k:k).eq.'!' ) then				!Is it marked as filename?

               nnum = 0
               numchr = ' '						!extract contiguous characters

               ll = index(in(k+1:),'!')					!File name would end with an !
               if ( ll.eq.0 ) then
                  ierr = 3
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation - ' )
                  call pargi ( k )
                  call printd (
     +              'ERROR:   An ! at character: %d marks start of' )
                  call printo (
     +              'ERROR:   filename - but there is no end !')
                  call alt_eqnerr ( exprs, k )
                  return
               elseif ( ll.eq.1 ) then
                  ierr = 3
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation - ' )
                  call pargi ( k )
                  call printd (
     +              'ERROR:   An ! at character: %d marks start of' )
                  call printo (
     +              'ERROR:   filename - but only another ! follows')
                  call alt_eqnerr ( exprs, k )
                  return
               endif
               nnum = ll + 1
               numchr = in(k+1:k+ll-1)

               call file_is ( numchr(:nnum-2), isfile )			!Try to read this as a file
               if ( isfile ) then
                  thisok = .true.
                  nsymb = -1
                  nim = nim + 1						!If it is a image, add name to image stack
                  fin(1:1) = 'I'
                  fin(2:2) = char(64+nim)
                  imid(nim) = fin
                  fina(1:2) = 'IN'
                  fina(3:3) = char(64+nim)
                  call file_par ( numchr(:nnum-2), fina, istat )
               endif

            else if ( (in(k:k).eq.'I' .or. in(k:k).eq.'i')		!If an operand was expected, it may be
     +                 .and. ischar(in(k+1:k+1)) ) then
               nsymb = -1						! a image, variable or const.
               nim = nim + 1						!If it is a image, add name to image stack
               imid(nim) = in(k:k+1)
            else if ( ischar(in(k:k)) ) then
               nsymb = -2						!If it is a variable, add name to variable stack
               nvar = nvar + 1
               varid(nvar) = in(k:k)
            else

               thisok = .false.

               nnum = 0							!Otherwise it may be a constant...
               numchr = ' '						! extract contiguous numerical characters

               atend = .false.						!Character may be part of a numerical constant
               ll = k - 1						! if it is 0..9 or '.'
               do while ( ll.lt.ncin .and. .not.atend )			! or if it is an 'e' following one of the above
                  ll = ll + 1						! or if it is a sign following an 'e'
                  if ( isnumb(in(ll:ll)) .or. (in(ll:ll).eq.'E') .or.
     +                 (in(ll:ll).eq.'E') .or.
     +                 (issign(in(ll:ll)).and.
     +               ((in(ll-1:ll-1).eq.'E').or.(in(ll-1:ll-1).eq.'E'))
     +               ) ) then
                     nnum = nnum + 1
                     numchr(nnum:nnum) = in(ll:ll)
                  else
                     atend = .true.					!End of number as soon as one of the above tests fails
                  endif
               enddo
               call chartor ( numchr(:nnum), const, iok )

               if ( iok.eq.0 .and. nnum.ne.0 ) then			!If successful, add constant to stack
                  thisok = .true.
                  ncon = ncon + 1
                  con(ncon) = const
                  nsymb = -3
                  l(nsymb) = nnum
               endif

               if ( .not.thisok ) then					!May be filename

                  nnum = 0						!Otherwise it may be a filename..
                  numchr = ' '						! extract contiguous characters

                  atend = .false.					!File name would end with an operator
                  ll = k - 1
                  do while ( ll.lt.ncin .and. .not.atend )
                     ll = ll + 1
                     do jj = 1, numendoper
                        if ( in(ll:ll).eq.endoper(jj) ) atend = .true.
                     enddo
                     if ( .not.atend ) then
                        nnum = nnum + 1
                        numchr(nnum:nnum) = in(ll:ll)
                     endif
                  enddo

                  call file_is ( numchr(:nnum), isfile )			!Try to read this as a file
                  if ( isfile .and. nnum.ne.0 ) then
                     thisok = .true.
                     nsymb = -1						!
                     nim = nim + 1						!If it is a image, add name to image stack
                     fin(1:1) = 'I'
                     fin(2:2) = char(64+nim)
                     imid(nim) = fin
                     fina(1:2) = 'IN'
                     fina(3:3) = char(64+nim)
                     call file_par ( numchr(:nnum), fina, istat )
                  endif

               endif

               if ( .not.thisok ) then
                  ierr = 2						!Otherwise there is a bad operand error
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation -' )
                  call pargi ( k )
                  call printd (
     +            'ERROR:   Operand missing or bad, at character: %d' )
                  call printo ( 'ERROR:   - that is a file, '//
     +                      'a parameter, a constant, or a variable' )
                  call printo (
     +            'ERROR:   - If file name, the file may not exist' )
                  call alt_eqnerr ( exprs, k )
                  return
               endif

            endif
         endif

         j = j + 1							!Put the identified symbol into the output
         symb(j) = nsymb						! array and move the input pointer to the
         if ( isfile ) then
            k = k + nnum
         else
            k = k + l(nsymb)						! next symbol
         endif

         opnext = opr(nsymb).ne.1					!Decide whether an operator or operand follows
         if ( opsymb(nsymb).eq.'=' ) lmore = .false.
      enddo

      call azeroi ( stk(0), j+1 )					!Zero operator stack for converting to reverse polish
      tos = 0
      isymb = 1
      noper = 0

      lmore = .true.
      do while ( lmore )
         lmore = .false.

         if ( index(oper(stk(tos)),'(').ne.0 .and.			!If the top of stack and input stream have
     +        oper(symb(isymb)).eq.')' ) then				! matching parentheses, cancel them

            if ( oper(stk(tos)).ne.'(' ) then				!If there is a function asssociated with the
 									! opening parenthesis then send it to the output stream

               output = opsymb(stk(tos))				!Remove the enclosed '(' from functions first
               output(index(output,'('):index(output,'(')) = ' '
               tos = tos - 1
               isymb = isymb + 1
             else
               tos = tos - 1
               isymb = isymb + 1
               lmore = .true.
             endif

         else if ( prr(stk(tos)).ge.prl(symb(isymb)) ) then		!If the symbol on the top of the stack has
            output = opsymb(stk(tos))					! a high enough priority, transfer it to
            tos = tos - 1						! the output stream
         else
            tos = tos + 1						!Otherwise, transfer the next symbol to the stack
            stk(tos) = symb(isymb)
            isymb = isymb + 1
            lmore = .true. 						!Return for next test
         endif

         if ( .not.lmore ) then

            if ( index(output,'(').ne.0 ) then				!If a bracket appears in the output,
               ierr = 4							! it results from unpaired parentheses
               call printo ( ' ' )
               call printo ( 'ERROR: ) missing from Equation -' )	! in the input expression...quit qith error
               call printo ( 'ERROR: '//exprs(1:70) )
               return
            elseif ( index(output,')').ne.0 ) then
               ierr = 4
               call printo ( 'ERROR: (  missing from Equation -' )
               call printo ( 'ERROR: '//exprs(1:70) )
               call printo ( ' ' )
               return
            endif

            if ( output.ne.'POS'.and.output.ne.':' ) then		!If there is some output, disregard it
               noper = noper + 1					! if it is unary + or a comma
               opcode(noper) = output
            endif

            if ( output.ne.'=' ) lmore = .true.				!Return for next symbol if not the end

         endif

      enddo

      if ( nim.ge.1 ) then						!If images or variables are referenced, sort
         call alt_sort ( imid, nim, imp, ndiff, symb )			! their names into alphabetical order and obtain
         nim = ndiff							! pointers to allow them to be accessed in their
      endif								! original order
      if ( nvar.ge.1 ) then
         call alt_sort ( varid, nvar, impv, ndiff, symb )
         nvar = ndiff
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_EQNERR -- Put out equation error
C
C  alan penny                     RAL            1994 Jul

      subroutine alt_eqnerr ( exprs, k )

      implicit none

      character*(*)     exprs	!i: Equation
      integer           k	!i: Character where error is
C--
      character texto*90
Cbegin


      call printo ( 'ERROR:   Eqn: '//exprs(1:70) )
      texto(1:90) = 'ERROR:   Err: '
      if ( k.le.14 ) then
         texto(14+k:14+k+14) = '^ Here is error'
         call printo ( texto )
      elseif ( k.le.70 ) then
         texto(14+k-14:14+k) = 'Error is here ^'
         call printo ( texto )
      endif
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_CDOIT -- Combine the lines from the stack of images into one line
C
C  alan penny                     RAL            1991 Dec

      subroutine alt_cdoit ( riml, nx, nz, ky, opcode, nopcode, imp,
     +                       impv, var, con, rimo, kseed, ierr )

      implicit none
      include 'ST_LIMITS_INC'

      integer      nx			!i: X size of input lines
      integer      nz			!i: Z size of input lines
      real         riml(nx,nz)		!i: Input lines
      integer      ky			!i: Y line in output image being done
      integer      nopcode		!i: Number of operations
      character*6  opcode(70)		!i: Code for operations
      integer      imp(70)		!i: Number of image in (i)th posn
      integer      impv(70)		!i: Number variable in (i)th posn
      real         var(26)		!i: Variables
      real         con(70)		!i: Constants
      real         rimo(nx)		!o: Output line
      integer      kseed		!i/o: Random number seed
      integer      ierr			!o: Error flag (0=ok;1=bad)
C--
      logical found, bad, more
      integer j, k, tos, numiim, nvar, ncon, ib, nop
      real    rv
      double precision s(0:132), a, b, c, dib, dvmin, dvmax, dv
      integer opwhich(70)

      integer nsymb
      parameter ( nsymb=37 )

      character opsymb(nsymb)*6						!Recognised operations
      data opsymb /
     + 'LDCON ', 'LDVAR ', 'LDIM  ', '=     ',
     + '-     ', '+     ', '**    ', '*     ', '/     ', 'NEG   ',
     + 'SQRT  ', 'EXP   ', 'LOG10 ', 'LOG   ', 'SIN   ', 'COS   ',
     + 'TAN   ', 'ASIN  ', 'ACOS  ', 'ATAN  ', 'ATAN2 ', 'SINH  ',
     + 'COSH  ', 'TANH  ', 'ABS   ', 'AINT  ', 'ANINT ', 'MOD   ',
     + 'SIGN  ', 'DIM   ', 'MIN   ', 'MAX   ', 'XX    ', 'YY    ',
     + 'CLIP  ', 'GAUSS ', 'RAN   ' /
Cbegin


      ierr = 0								!Failure flag

      dvmax = INT_MAXRR							!Set (dble precision) max and min vals
      dvmin = INT_MINRR

      do k = 1, nopcode							!Find `opcode' for each input operator
         found = .false.
         j = 0
         do while ( j.lt.nsymb .and. .not.found )
            j = j + 1
            if ( opcode(k).eq.opsymb(j) ) then
               opwhich(k) = j
               found = .true.
            endif
         enddo
         if ( .not.found ) then
            ierr = 1
            return
         endif
      enddo

      do j = 1, nx							!Do for all pixels in line

         tos = 0							!Initialise the arith, image,
         numiim = 0							! variable, constant stack pointers
         nvar = 0
         ncon = 0

         nop = 0							!Do the operations
         more = .true.
         bad = .false.

         do while ( nop.lt.nopcode .and. more )
            nop = nop + 1

            go to ( 101,102,103,104,105,106,107,108,109,110,111,112,
     +              113,114,115,116,117,118,119,120,121,122,123,124,
     +              125,126,127,128,129,130,131,132,133,134,135,136,
     +              137 ),
     +            opwhich(nop)

 101        continue
               tos = tos + 1						!Load constant on to stack
               ncon = ncon + 1
               s(tos) = con(ncon)
            go to 100
 102        continue
               tos = tos + 1						!Load variable onto stack
               nvar = nvar + 1
               s(tos) = var(impv(nvar))
            go to 100
 103        continue
               tos = tos + 1						!Load image pixel on to stack
               numiim = numiim + 1
               rv = riml(j,imp(numiim))
               if ( rv.ne.INT_INVALR ) then
                  s(tos) = rv
               else
                  bad = .true.
               endif
            go to 100
 104        continue
               dv = s(tos)						!=  : end of calculation
               if ( dv.ge.dvmax .or. dv.le.dvmin ) then
                  rimo(j) = INT_INVALR
               else
                  rimo(j) = dv
               endif
               more = .false.
            go to 100
 105        continue
               s(tos-1) = s(tos-1) - s(tos)				!- : subtract tos
               tos = tos - 1
            go to 100
 106        continue
               s(tos-1) = s(tos-1) + s(tos)				!+ : add tos
               tos = tos - 1
            go to 100
 107        continue
               a = s(tos-1)						!** : raise to power
               b = s(tos)
               if ( a.ge.0.0 ) then
                  c = a**b
               else
                  ib = nint(b)
                  dib = ib
                  if ( abs(dib-b).le.1.0d-7 ) then
                     c = a**ib
                  else
                     bad = .true.
                  endif
               endif
               tos = tos - 1
               s(tos) = c
            go to 100
 108        continue
               s(tos-1) = s(tos-1)*s(tos)				!* : multiply tos
               tos = tos - 1
            go to 100
 109        continue
               a = s(tos)						!/ : Divide tos
               if ( a.ne.0.0d0 ) then
                  s(tos-1) = s(tos-1)/a
                  tos = tos - 1
               else
                  bad = .true.
               endif
            go to 100
 110        continue
               s(tos) = -s(tos)						!NEG : Negate tos
            go to 100
 111        continue
              a = s(tos)						!SQRT : Square root tos
               if ( a.ge.0.0d0 ) then
                  s(tos) = sqrt(a)
               else
                  bad = .true.
               endif
            go to 100
 112        continue
               a = s(tos)						!EXP : E to power tos
               if ( a.le.100.0d0 ) then
                  s(tos) = exp(a)
               else
                  bad = .true.
               endif
            go to 100
 113        continue
               a = s(tos)						!LOG10 : Log tos base 10
               if ( a.gt.0.0d0 ) then
                  s(tos) = log10(a)
               else
                  bad = .true.
               endif
            go to 100
 114        continue
               a = s(tos)						!LOG : Natural log of tos
               if ( a.gt.0.0d0 ) then
                  s(tos) = log(a)
               else
                  bad = .true.
               endif
            go to 100
 115        continue
               s(tos) = sin(s(tos))					!SIN : Sine of tos
            go to 100
 116        continue
               s(tos) = cos(s(tos))					!COS : Cosine of tos
            go to 100
 117        continue
               a = s(tos)						!TAN : Tangent of tos
               s(tos) = tan(a)
            go to 100
 118        continue
            a = s(tos)							!ASIN : Arcsine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = asin(a)
               else
                  bad = .true.
               endif
            go to 100
 119        continue
               a = s(tos)						!ACOS : Arccosine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = acos(a)
               else
                  bad = .true.
               endif
            go to 100
 120        continue
               s(tos) = atan(s(tos))					!ATAN : Arctangent of tos
            go to 100
 121        continue
               a = s(tos-1)						!ATAN2 : Arctangent of tos ratio
               b = s(tos)
               if ( a.ne.0.0d0 .or. b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = atan2(a,b)
               else
                  bad = .true.
               endif
            go to 100
 122        continue
               a = s(tos)						!SINH : Hyperbolic sine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = sinh(a)
               else
                  bad = .true.
               endif
            go to 100
 123        continue
               a = s(tos)						!COSH : Hyperbolic cosine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = cosh(a)
               else
                  bad = .true.
               endif
            go to 100
 124        continue
               s(tos) = tanh(s(tos))					!TANH : Hyperbolic tangent of tos
            go to 100
 125        continue
               s(tos) = abs(s(tos)) 					!ABS : Absolute value of tos
            go to 100
 126        continue
               s(tos) = aint(s(tos))					!AINT : Truncate tos
            go to 100
 127        continue
               s(tos) = anint(s(tos))					!ANINT : Nearest integer to tos
            go to 100
 128        continue
               a = s(tos-1)						!MOD : Modulo on two tos entries
               b = s(tos)
               if ( b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = mod(a,b)
               else
                  bad = .true.
               endif
            go to 100
 129        continue
               s(tos-1) = sign(s(tos-1),s(tos))				!SIGN : Transfer of sign between two tos entries
               tos = tos - 1
            go to 100
 130        continue
               s(tos-1) = dim(s(tos-1),s(tos))				!DIM : Positive difference of two tos entries
               tos = tos - 1
            go to 100
 131        continue
              s(tos-1) = min(s(tos-1),s(tos))				!MIN : Minimum of two tos entries
               tos = tos - 1
            go to 100
 132        continue
               s(tos-1) = max(s(tos-1),s(tos))				!MAX : Maximum of two tos entries
               tos = tos - 1
            go to 100
 133        continue
               tos = tos + 1						!XX : X position in output image
               s(tos) = j
            go to 100
 134        continue
               tos = tos + 1						!YY : Y position in output image
               s(tos) = ky
            go to 100
 135        continue
               a = s(tos-2)						!CLIP : Abort if tos lies outside window
               b = s(tos-1)
               c = s(tos)
               if ( a.le.b .and. b.le.c ) then
                  tos = tos - 2
                  s(tos) = b
               else
                  bad = .true.
               endif
            go to 100
 136        continue
               call gasdev ( rv, kseed )				!GAUSS : Put gaussian noise on tos
               s(tos) = s(tos)*dble(rv)
            go to 100
 137        continue
               call rano ( rv, kseed )					!RAN : Uniform random noise
               s(tos) = s(tos)*dble(rv)
            go to 100
 100        continue

            if ( bad ) then						!Invalid arithmetic operation done
               rimo(j) = INT_INVALR
               more = .false.
            endif

         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_IMDIFF -- Get number of diff input images and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine alt_imdiff ( )

      implicit none
      include 'alter_calc.inc'
C--
      integer j, kk
      character ch*2
      logical found
Cbegin


      NZ = 0

      if ( NIM.ne.0 ) then
         do j = 1, NIM
            if ( NZ.eq.0 ) then
               NZ = 1
               IMTOT(NZ) = IMID(j)(1:2)
            else
               found = .false.
               ch = IMID(j)(1:2)
               do kk = 1, NZ
                  if ( ch.eq.IMTOT(kk) ) found = .true.
               enddo
               if ( .not.found ) then
                  NZ = NZ + 1
                  IMTOT(NZ) = IMID(j)(1:2)
               endif
            endif
         enddo
      endif

      if ( NZ.ne.0 ) then
         ANYIN = .true.
      else
         ANYIN = .false.
         NZ = 1
      endif

      do j = 1, 70
         if ( IMP(j).ne.0 ) then
            ch = IMID(IMP(j))(1:2)
            do kk = 1, NZ
               if ( ch.eq.IMTOT(kk) ) IMP(j) = kk
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_VARDIFF -- Get number of diff input variables and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine alt_vardiff ( )

      implicit none
      include 'alter_calc.inc'
C--
      integer j, kk
      character ch*1
      logical found
Cbegin


      NVARTOT = 0

      if ( NVAR.ne.0 ) then
         do j = 1, NVAR
            if ( NVARTOT.eq.0 ) then
               NVARTOT = 1
               VARTOT(NVARTOT) = VARID(j)(1:1)
            else
               found = .false.
               ch = VARID(j)(1:1)
               do kk = 1, NVARTOT
                  if ( ch.eq.VARTOT(kk) ) found = .true.
               enddo
               if ( .not.found ) then
                  NVARTOT = NVARTOT + 1
                  VARTOT(NVARTOT) = VARID(j)(1:1)
               endif
            endif
         enddo
      endif

      do j = 1, 70
         if ( IMPV(j).ne.0 ) then
            ch = VARID(IMPV(j))(1:1)
            do kk = 1, NVARTOT
               if ( ch.eq.VARTOT(kk) ) IMPV(j) = kk
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_LOADR -- Copy real image into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine alt_loadr ( im, nx, ny, bs, bz, rinvali, jn, jzn,
     +                        rim, nxo, nz )

      implicit none
      include 'ST_LIMITS_INC'

      integer     nx			!i: X size of input image
      integer     ny			!i: Y size of input image
      real        im(nx,ny)		!i: Input image
      real        bs			!i: Scale of input image
      real        bz			!i: Zero of input image
      real        rinvali		!i: Invalid flag of input image
      integer     jn			!i: Y line of input image to take
      integer     jzn			!i: Z line to put it in
      integer     nxo			!i: X size of stack line
      integer     nz			!i: Z size of stack line
      real        rim(nxo,nz)		!i/o: stack image of a line
C--
      integer k
      real    rv
Cbegin


      do k = 1, nxo
         rv = im(k,jn)
         if ( rv.eq.rinvali ) then
            rim(k,jzn) = INT_INVALR
         else
            rim(k,jzn) = bs*rv + bz
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_LOADS -- Copy int*2 image into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine alt_loads ( im, nx, ny, bs, bz, invali, jn, jzn,
     +                        rim, nxo, nz )

      implicit none
      include 'ST_LIMITS_INC'

      integer     nx			!i: X size of input image
      integer     ny			!i: Y size of input image
      integer*2   im(nx,ny)		!i: Input image
      real        bs			!i: Scale of input image
      real        bz			!i: Zero of input image
      integer     invali		!i: Invalid flag of input image
      integer     jn			!i: Y line of input image to take
      integer     jzn			!i: Z line to put it in
      integer     nxo			!i: X size of stack line
      integer     nz			!i: Z size of stack line
      real        rim(nxo,nz)		!i/o: stack image of a line
C--
      integer k, kv
Cbegin


      do k = 1, nxo
         kv = im(k,jn)
         if ( kv.eq.invali ) then
            rim(k,jzn) = INT_INVALR
         else
            rim(k,jzn) = bs*real(kv) + bz
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_OUTLR -- Load a line to the real output image
C
C  alan penny            ral                     1991 Dec

      subroutine alt_outlr  ( rimi, rimo, nx, ny, bs, bz, rinval, kl )

      implicit none
      include 'ST_LIMITS_INC'

      integer    nx		!i: length of line (= X size of output)
      integer    ny		!i: Y Size of output
      real       rimi(nx)	!i: Line
      real       rimo(nx,ny)	!o: Output image
      real       bs             !i: Image scale
      real       bz             !i: Image zero
      real       rinval         !i: Image invalid flag
      integer    kl             !i: Y Line to load
C--
      integer j
      real rv
Cbegin


      do j = 1, nx
         rv = rimi(j)
         if ( rv.eq.INT_INVALR ) then
            rimo(j,kl) = rinval
         else
            rv = (rv-bz)/bs
            if ( rv.lt.INT_MINRR .or. rv.gt.INT_MAXRR ) then
               rimo(j,kl) = rinval
            else
               rimo(j,kl) = rv
            endif
         endif
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALT_SORT -- Sort identifiers to alphabetical order
C
C  alan penny            ral                     1991 Dec

      subroutine alt_sort ( c, nc, imp, ndiff, iw )

      implicit none

      integer        nc		!i: Number of identifiers
      character*(*)  c(nc)	!i/o: Identiers
      integer        imp(nc)	!o: pointer to identifier place in stack
      integer        ndiff	!o: Number of different identifiers
      integer        iw(nc)	!o: Work space
C--
      integer k, nn, it
      character*132 text
      logical repeat
Cbegin


      ndiff = 1								!Only 1?
      iw(1) = 1
      imp(1) = 1
      if ( nc.le.1 ) return

      nn = min(132,len(c(1)))						!Length of input strings

      do k = 1, nc							!Initiallise pointers in workspace
         iw(k) = k
      enddo

      repeat = .true.							!Perform a bubble sort to put
      do while ( repeat ) 						! character strings into alphabetical order
         repeat = .false.
         do k = 1, nc-1
            if ( c(k+1).lt.c(k) ) then
               repeat = .true.
               text(:nn) = c(k+1)					!Swap pairs which are in the wrong order
               c(k+1) = c(k)
               c(k) = text(:nn)
               it = iw(k+1)						!Permute the pointers in the same way
               iw(k+1) = iw(k)
               iw(k) = it
            endif
         enddo
      enddo

      ndiff = 1								!Pointers now point to original positions. scan list to
      imp(iw(1)) = 1							! remove repeated entries and reverse the pointing direction
      do k = 2, nc
         if ( c(k).ne.c(ndiff) ) then					!If a different character string is found, count it and
            ndiff = ndiff + 1						! put it in the correct place in the list
            c(ndiff) = c(k)
         endif
         imp(iw(k)) = ndiff						!Set the appropriate output pointer to its new location
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    APERTURE.F
C
C    Contains:-
C
C T_APERTUE    Aperture photometry using the display
C AP_OPTION_SETUP  Put out help panel help on chosen option
C AP_SDEF      Set up aperture photometry values and starting values
C AP_MEAS      Use cursor to measure stars
C AP_MEASA     Measure stars - single aperture
C AP_MEASB     Measure stars - annulus
C AP_LOVAL     List oval(s) as placed on display
C AP_CHCENTRE  Toggle method of placing centre of aperture
C AP_CEN(RS)   Get centre in oval area
C AP_PMAX(RS)  Get position of max pixel in oval area
C AP_DOIT(RS)  Calc flux through aperture (star/sky/annulus)
C AP_MAG       Calc, store and type out star measure
C AP_OUT       Put measures out to file
C AP_REM       Remove the ovals near a point in display
C AP_LIST      List the measures so far
C AP_GKSIZE    Get aperture radii by keyboard
C AP_GCSIZE    Get aperture radii by cursor
C AP_OSIZE     Get max X and Y size of oval
C AP_CALCELL   Calc coords of 180 points on an ellipse
C AP_MAINGET   Get main prog params to 'aperture' ones
C AP_SRMAINGET Get main prog params to temp params
C AP_MAINPUT   Put 'aperture' params to main prog ones
C AP_SRAPGET   Get 'aperture' s/r params to temp ones


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_APERTURE -- Aperture photometry using the display
C
C   alan penny                   ral                  1990-01-31

      subroutine t_aperture ( )

      implicit none
      include 'aper.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'
C--
      integer km, ierr, kma
      logical loop
      character ktopt*12
Cbegin


      if ( ST_FAILED ) return

      call ap_mainget

      call ap_option_setup ( ktopt, 5, .true. )
      if ( ST_FAILED ) return
      loop = .true.							!Loop through options
      do while ( loop )

         call ap_option_setup ( ktopt, 5, .false. )
         call get_choice ( ktopt, 1 )					!Get choice

         if ( ktopt.eq.'measure' .or. ktopt.eq.'remove' .or. 		!Check display open for
     +        ktopt.eq.'zoom' .or. ktopt.eq.'reset' .or.		! options that need it
     +        ktopt.eq.'flash' .or. ktopt.eq.'im_get_flash' .or.
     +        ktopt.eq.'display' .or. ktopt.eq.'clear' .or.
     +        ktopt.eq.'size_star'  .or. ktopt.eq.'size_sky' ) then
            call in_opdisp ( ierr )
         endif

         if ( .not.GOTIMAGE .and. (ktopt.eq.'measure' .or. 		!Check got image
     +        ktopt.eq.'flash' .or. ktopt.eq.'display'  .or.
     +        ktopt.eq.'recalc') ) then
            call printo ( 'No displayed image yet' )
            ktopt = ' '
         endif

         if ( ktopt.eq.'measure' ) call ap_meas 			!Measure

         if ( ktopt.eq.'remove' ) call ap_rem 				!Remove ovals

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom and pan

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Reset zoom and pan

         if ( ktopt.eq.'flash' ) call ds_doflash ( %val(IPIM), NX, NY, 	!Flash up image
     +                                   IMTYPE, DSKVRANGE, IMTITLE )

         if ( ktopt.eq.'list' ) call ap_list 				!List measures

         if ( ktopt.eq.'size_key' ) call ap_gksize 			!Get aperture sizes

         if ( ktopt.eq.'shape' ) then					!Get aperture angle and ellipticity
                                    call get1r ( 'APELL', ELLIPSE,
     +                                           ELLIPSE, 0.0, 1.0 )
                                    call get1r ( 'APANG', APANG,
     +                                           APANG, -90.0, 90.0 )
                                 endif

         if ( ktopt.eq.'exptim_k' ) call airgktime 			!Change Zero point by
									! keyboard exposure time?

         if ( ktopt.eq.'recalc' ) call ap_mag 				!Calc Magnitude from last star and sky

         if ( ktopt.eq.'poisson' ) call get1r ( 'POISS', POISV, POISV,	!Change RMS correction value
     +                                          1.e-8, 1.e16 )

         if ( ktopt.eq.'noise' ) call get1r ( 'NOISE', NOISE, NOISE, 	!Change extra noise value
     +                                        0.0, 1.e16 )

         if ( ktopt.eq.'store' ) call ap_store 				!Write results to an output file

         if ( ktopt.eq.'method' ) then					!Choose sky method
                        kma = 1
                        if ( .not.ANNULUS ) kma = 2
                        call get_job ( 'METHOD', 'annulus:separate',
     +                                 km, kma, ' ', 0 )
                        ANNULUS = .true.
                        if ( km.eq.2 ) ANNULUS = .false.
                        endif

         if ( ktopt.eq.'centre' ) call ap_chcentre			!Toggle method of finding centre

         if ( ktopt.eq.'level' ) then 					!Toggle using median/mean for sky
                        DOMEDIAN = .not.DOMEDIAN
                        if ( DOMEDIAN ) then
                           call printo (
     +                     'Median used when measuring sky level' )
                        else
                           call printo (
     +                     'Mean used when measuring sky level')
                        endif
                        endif

         if ( ktopt.eq.'size_star' ) call ap_gcsize ( 1 )		!Get star aperture size

         if ( ktopt.eq.'size_sky' ) call ap_gcsize ( 2 )		!Get sky aperture size(s)

         if ( ktopt.eq.'zero_pt' ) call get1r ( 'ZEROP', ZP, ZP, 	!Change Zero point
     +                                          -1.0e8, 1.0e8 )

         if ( ktopt.eq.'show_ap' ) then					!Toggle show apertures when getting?
                        APSHOW = .not.APSHOW
                        if ( APSHOW ) then
                           call printo (
     +                        'Apertures shown whilst getting stars' )
                        else
                           call printo (
     +                     'Apertures not shown whilst getting stars' )
                        endif
                        endif

         if ( ktopt.eq.'image' ) then					!Input new image
                                    call in_newim ( ierr )
                                    NSTLAST = NST
                                 endif

         if ( ktopt.eq.'im_get_flash' ) then				!Input new image and display
                                          call in_newim ( ierr )
                                          NSTLAST = NST
                                          if ( .not.ST_FAILED .and.
     +                                         ierr.eq.0 ) then
                                          call ds_doflash ( %val(IPIM),
     +                                    NX, NY, IMTYPE, DSKVRANGE,
     +                                    IMTITLE )
                                          DISPLAYED = .true.
                                          endif
                                       endif

         if ( ktopt.eq.'exptim_im' ) call airdescr ( 1 )		!Use exp time image descriptors?

         if ( ktopt.eq.'airmass' ) call airdescr ( 2 )			!Use air mass image descriptors?

         if ( ktopt.eq.'display' ) call ds_dodisp ( %val(IPIM), NX, NY, !Display
     +                                   IMTYPE, DSKVRANGE, IMTITLE )

         if ( ktopt.eq.'clear' ) then					!Clear screen
                        call ds_erase
                        DISPLAYED = .false.
                        endif

         if ( ktopt.eq.'close' ) then                                  !Close display screen
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )			!Open display screen

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice


         if ( ktopt.eq.'main' .or. ktopt.eq.'alter' .or. 		!Return to main program
     +        ktopt.eq.'colour' .or.
     +        ktopt.eq.'inspect' .or. ktopt.eq.'fit_magns' .or.
     +        ktopt.eq.'positions' .or. ktopt.eq.'scrutiny' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo

      if ( NST.ne.0 ) then						!Flag not all stoed
         call printo ( ' ' )
         call printo ( 'WARNING: You have not stored all the results' )
         if ( ktopt.ne.'exit' ) call printo ( '         You might '//
     +            'want to go back into APERTURE to store them')
            call printo ( ' ' )
      endif

      call ap_mainput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_OPTION_SETUP -- Put out help panel help on chosen option
C
C   alan penny                        ral              1990-01-31

      subroutine ap_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt		!i: Chosen option
      integer        set_num		!i: Code for set of options
      logical        koutside		!i: Is this called from outside loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=37 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'airmass', 'Correct for Airmass from image descriptor',
     + 'Do you want to know the observation airmass? If so, what are ',
     + 'the names of the Airmass or Position [RA, Dec, type of coord,',
     + 'Sid Time] descriptor(s) in the image. Are magnitudes to be',
     + 'corrected for the airmass? If so, what is the Filter descrip-',
     + 'tor name, where in it is the Filter code, what codes are to be',
     + 'looked for, and what are the extinctions of those filters?' /

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'zero_pt', 'Change the -base- magnitude zero point',
     + 'Input (via the keyboard) the -zero point- correction to be' ,
     + 'added to measured magnitude, before it is reported or stored.',
     + ' ',
     + ' (This is in addition to any -exposure time- zero point used.)',
     + ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'centre', 'Toggle centre => cursor/max pixel/centroid/Gaussian',
     + 'Each time this option is chosen, the method of determining the',
     + 'position to be used for the aperture centre changes. They are:',
     + '(1) The raw cursor position  (2) The maximum pixel within the',
     + 'aperture  (3) The centroid of the pixels within the aperture',
     + '(4) The location of a 2-D Gaussian fitted to the -star- in the',
     + 'aperture (choose either fixed or variable Gaussian radii).'/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'exptim_k', 'Change zeropoint by using keyboard exposure time',
     + 'Do you want (KEYTIME) to add a correction to the measured mags',
     + 'corresponding to an -exposure time- correction? If you do, you',
     + 'are then asked for this time (EXPTIM). Subsequent magnitudes',
     + 'then have a delta mag [= 2.5*log(EXPTIM)] SUBTRACTED.' ,
     + ' ',
     + ' (This is in addition to any -base- zero point used.)'/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'Exptim_im', 'Correct for exposure time from image descriptor' ,
     + 'Do you want (DESCRTIME) to correct the measured magnitudes ',
     + 'automatically from the exposure time stored in the image',
     + 'descriptors? If so, what (EXPNAME) is the name of the',
     + 'descriptor with the exposure time? Subsequent magnitudes will',
     + 'then have a delta mag [= 2.5*log(time)] SUBTRACTED.' ,
     + ' (This is in addition to any -base- zero point used.)' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'level', 'Toggle mean/median method for sky level' ,
     + 'When the sky level is measured, either in the annulus or in' ,
     + 'the sky aperture, the level is determined either by taking',
     + 'the mean or the median. They both ignore -invalid- pixels. The',
     + 'mean method ignores pixels. The error in the sky level for the',
     + 'mean uses trhe scatter in the pixel values, whilst the median',
     + 'uses Poisson (and base noise) theoretical statistics.' /

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'list', 'List the measures so far' ,
     + 'List the star measures done so far. This has a condensed ' ,
     + 'result for each measure. The full results may be obtained by ',
     + 'the -store- option, which puts the results into a table in a ',
     + 'data file.',
     + '(-removed- star measures are marked as such. Optionally these',
     + 'can be ommitted from the listing.)' /

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'recalc', 'A fresh calc for the last star and last sky' ,
     + 'Take the deatils of the last sky measurement and the last',
     + 'star measurement, and calculate the star magnitude. This can',
     + 'be used when a star has been measured once, but a new sky',
     + 'measurement has been done. The star does not then have to be',
     + 'remeasured.' ,
     + 'The result is remembered for later storage. '/

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'measure', 'Measure star/sky flux via cursor and buttons',
     + 'In -annulus- mode, the left or centre mouse buttons make the',
     + 'star aperture and sky annulus measure at that position. In',
     + '-separate- mode, left button chooses star aperture measure, ',
     + 'centre one sky aperture measure. In both, right button ends.',
     + 'The result is remembered for later storage. ',
     + '  Magn = 30 + [Corrns] - 2.5*log10[(star-sky level) flux]'  /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'method', 'Sky determined from annulus or from separate area',
     + 'Choose between the -annulus- and -separate- methods. ',
     + 'In the -annulus- method of star measurement, the sky level is' ,
     + 'determined at the same time as the star measurement from an ',
     + 'annulus around the star. In the -separate- method either the',
     + 'star flux or the sky level can be determined. If the star flux',
     + 'is being found, the last measured sky level is used.' /

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'noise', 'Level of extra noise/pixel in pixel counts' ,
     + 'Input the amount of -extra- noise that is actually present in',
     + 'each pixel, above the amount the programme thinks is there',
     + 'from the Poisson noise of the counts. [An example could be the',
     + 'bias -read-out- noise of a CCD, which is subtracted in the',
     + 'pre-processing.] Input the standard deviation per pixel of any',
     + 'such extra noise.' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'store', 'Store results in output file',
     + 'Put the star measures done so far into a -Starman- table in a',
     + 'data file. (Optionally ommitting any -removed- star measures.)',
     + 'A stars -name- will be its number and the first 14 digits',
     + 'of the title of the image it was measured in.  ',
     + 'After storing in a file, the measurements are lost and the ',
     + 'recording starts anew.'/

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'poisson', 'Change the Poisson scaling factor for Error',
     + 'The scaling that would have to be applied to pixel values so',
     + 'that their values would have a Poissonian distribution.',
     + 'This is used to calculate the error on a flux level.',
     + 'The error used (squared) = Flux*(-Poiss- value) + ',
     + '       (no of pixels)*(-Noise- value)*(-Noise- value)',
     + '(This enables the output error estimates to be more correct.)'/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'remove', 'Remove a measure from memory and the display',
     + 'By positioning the cursor inside one of the measure ovals',
     + 'in the image display and pressing the left or centre mouse',
     + 'buttons, the displayed oval(s) is removed. The measurement ',
     + '(if a star measurement) is flagged for possible ommission',
     + 'in listing or storing in a file. The right hand button ends.',
     + ' '/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'show_ap', 'Show apertures when getting positions',
     + 'Toggle between showing the cursor as a small cross and showing',
     + 'it as the oval(s) that the photometry will use. (the ovals',
     + 'will always be painted at the location of an actual',
     + 'measurement.',
     + ' ', ' '/

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'size_key', 'Set via keyboard: X, Y size of the oval apertures',
     + 'Set X,Y diameters of the four apertures via keyboard.',
     + 'The apertures can thus be vertical or horizontal ovals.',
     + 'The four are (1) the star aperture diameter; (2) the ',
     + '-annulus- sky aperture inner diameter; (3) the -annulus- ',
     + 'sky aperture outer diameter; (4) the -separate- sky aperture',
     + 'diameter. '/

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'size_sky', 'Cursor change the size of the sky aperture(s)',
     + 'Change the size of the sky aperture(s). The left hand button',
     + 'on the mouse decreases the diameter by one pixel, the centre',
     + 'button increases it by one. The right hand button ends.If the',
     + 'sky measurement is in -annulus- mode, you first set the inner',
     + 'diameter, and then the outer one. The change in the aperture',
     + 'is shown on the image display and the end result typed out.' /

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'size_star', 'Cursor change the size of the star aperture' ,
     + 'Change the size of the star measuring aperture. Press the left',
     + 'hand mouse button to decrease the diameter by one pixel, and ',
     + 'the middle button to increase it by one pixel. Press the right',
     + 'button to end. The change in the aperture is shown on the ',
     + 'image display and the end result typed out.' ,
     + ' '/

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'im_get_flash', 'Input new image and display it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, without displaying an image.',
     + 'The screen may be any size you want, through keyboard entry',
     + 'via the SCREEN parameter.',
     + ' ', ' ', ' '/

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from main program, do not access main option list',
     + ' ', ' ', ' ', ' '/

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'shape', 'Set via keyboard: aperture ellipticity, angle',
     + 'The apertures are oval, and may be inclined to the X-axis.',
     + 'This option allows you to set this ellipticity and angle, via',
     + 'the keyboard. The ellipticity is ((a-b)/a) where a = major ',
     + 'axis diameter and b = minor axis. a is the diameter you input',
     + 'for size. Thus a round aperture has ellipticity = 0.0, a line',
     + 'has = 1.0. The angle is in degrees, +/- from the X-axis.'/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(34),opt_head(34),(opt_help(j,34),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(35),opt_head(35),(opt_help(j,35),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(36),opt_head(36),(opt_help(j,36),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /

      data opt_text(37),opt_head(37),(opt_help(j,37),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /


      character*50 title, option
      integer ncode
      data title, option, ncode / 'Aperture Photometry', 'AOPTION', 5 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'measure' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / 'ACTIONS', 'APERTURE', 'SETUPS', 'IMAGE',
     +                 'DISPLAY', 'FUNCTIONS', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'measure:list:recalc:remove:store' /
      data sect_text(2) / 'size_key:size_star:size_sky:shape:
     +                     show_ap' /
      data sect_text(3) / 'airmass:centre:exptim_k:exptim_im:level:
     +                     method:noise:poisson:zero_pt' /
      data sect_text(4) / 'image:im_get_flash' /
      data sect_text(5) / 'clear:close:display:flash:open:
     +                     reset:zoom' /
      data sect_text(6) / 'alter:colour:fit_magns:
     +                     inspect:main:positions:scrutiny' /
      data sect_text(7) / 'panel:exit' /

      integer help_num
      parameter ( help_num=15 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + 'Zoom means zoom around present position of cursor ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' ,
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' ,
     + 'VWS VWS windows  YES         all buttons exit ' ,
     + 'and Decwindows    NO         Buttons Left,Centre give values'/
      data (help_text(k),k=11,help_num) /
     + '                               Right Button 3 exits ' ,
     + 'IKON device      YES         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits' ,
     + '                  NO         Buttons Left,Centre give values',
     + '                               Right Button 3 exits ' /

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
C AP_SDEF -- Set up aperture photometry values and starting values
C
C   alan penny                        ral              1990-01-31

      subroutine ap_sdef ()

      implicit none
      include 'aper.inc'
      include 'air.inc'
C--
Cbegin


      NST = 0
      NSTLAST = 0

      STAREA = 0.0
      NSTBAD = 0
      STFL   = 0.0
      STRAW  = 0.0
      STFERR = 0.0
      STMERR = 0.0
      STMAG  = 0.0
      STTOP  = 0.0
      STAPRX  = 30.0
      ELLIPSE = 0.0
      APANG   = 0.0
      XST    = 0.0
      YST    = 0.0

      SKAREA    = 0.0
      NSKBAD    = 0
      SKFL      = 0.0
      SKLEV     = 0.0
      SKAPRX    = 20.0
      ANAPRX(1) = 50.0
      ANAPRX(2) = 60.0
      XSK       = 0.0
      YSK       = 0.0

      POISV  = 1.0
      ZP     = 0.0
      NOISE  = 0.0
      EXPTIM = 1.0

      APSHOW    = .true.
      ANNULUS   = .true.
      ADOCENTRE = 1
      FIXGRAD   = .false.
      AGRX      = 2.0
      AGRY      = 2.0
      DOMEDIAN  = .false.

      DOHAIR  = .false.
      DOHFILT = .false.
      DOHTIME = .false.
      DOKTIME = .false.

      NCIR = 0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MEAS -- Use cursor to measure stars
C
C alan penny                  ral            1990-01-31

      subroutine ap_meas ( )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
C--
      integer kx, ky, kbut, ierr
      logical loop
      real    xn(2), ax, ay
Cbegin


      if ( ANNULUS .and. (ANAPRX(1).eq.ANAPRX(2)) ) then		!Check apertures right
         call printo (
     +  'ERROR: Sky annulus zero width - will not try to measure')
         return
      endif

      xn(1) = 0.0
      xn(2) = 0.0
      ierr = 0

      call printo ( ' Name      X      Y     Mag   Error    Peak   '//	!Type header
     +              '     Flux       Sky   Bad    Time  Airm '//
     +              'Filt Corrn' )

      loop = .true.
      do while ( loop )							!Loop getting stars

         if ( APSHOW ) then
            if ( ANNULUS ) then						!Get position
               call ds_ovcus ( .true., STAPRX, 2, ANAPRX, 1,
     +                         APANG, ELLIPSE, 0, kx, ky, kbut )
            else
               call ds_ovcus ( .true., STAPRX, 2, xn, 0,
     +                         APANG, ELLIPSE, 0, kx, ky, kbut )
            endif
         else
            call ds_gcur ( .true., kx, ky, kbut, ierr )
         endif
         ax = kx
         ay = ky

         if ( ierr.ne.0 .or. kbut.eq.3 ) then
            loop = .false.						!Rt hd button =  end
         elseif ( kbut.eq.2 .and. .not.ANNULUS ) then
            call ap_measa ( ax, ay )
         else
            call ap_measb ( ax, ay )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MEASA -- Measure stars - single aperture
C
C alan penny                  ral            1994 Dec

      subroutine ap_measa ( ax, ay )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real   ax		!i: X position
      real   ay		!i: Y position
C--
      integer kvx, kvy, ipwkakv, ierr, ngood
      real    xsize, ysize, xxsize, yysize, rv
      character*70 text
Cbegin


      call ap_osize ( SKAPRX, APANG, ELLIPSE, xsize, ysize )
      xxsize = xsize/2.0
      yysize = ysize/2.0

      if ( (ax+xxsize).lt.1.0 .or. (ax-xxsize).gt.NX .or.		!Check if any in image
     +     (ay+yysize).lt.1.0 .or. (ay-yysize).gt.NY ) return

      XSK = ax								!Get sky
      YSK = ay
      call ds_oval ( ax, ay, SKAPRX, APANG, ELLIPSE, 1 )
      NCIR = NCIR + 1
      call ap_loval ( ax, ay, 1 )
      kvx = xsize + 4.0
      kvy = ysize + 4.0
      if ( IMTYPE.eq.'SHORT' ) then
         call gtwrki ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
         call ap_doits ( %val(IPIM), ax, ay, DOMEDIAN, 0.0,
     +                   SKAPRX, %val(ipwkakv), SKFL, SKLEV,
     +                   SKAREA, SKERR, NSKBAD, ngood, rv )
      else
         call gtwrkr ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
         call ap_doitr ( %val(IPIM), ax, ay, DOMEDIAN, 0.0,
     +                   SKAPRX, %val(ipwkakv), SKFL, SKLEV,
     +                   SKAREA, SKERR, NSKBAD, ngood, rv )
      endif
      call wrkcan ( 'WORKAKV' )

      if ( ngood.eq.0 ) call printo ( 'WARNING: No good '//
     +                  'pixels in sky aperture - sky set at zero' )
      write ( text, '(1x,6x,2f7.1,24x,11x,f10.2)' ) XSK, YSK, SKLEV 	!Type result
      call printo ( text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MEASB -- Measure stars - annulus
C
C alan penny                  ral            1994 Dec

      subroutine ap_measb ( ax, ay )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real   ax		!i: X position
      real   ay		!i: Y position
C--
      integer kvx, kvy, ipwkakv, ierr, ngood, iter, ninval, lxg, lyg,
     +        kw
      real    xsize, ysize, xxsize, yysize, rv, rva, amag, height,
     +        base, dx, dy, anx, any, rx, ry, rms, aamag, arx, ary
      character*70 text
      real trunc
      external trunc
Cbegin


      call ap_osize ( STAPRX, APANG, ELLIPSE, xsize, ysize )
      xxsize = xsize/2.0
      yysize = ysize/2.0

      if ( (ax+xxsize).lt.1.0 .or. (ax-xxsize).gt.NX .or.		!Check if any in image
     +     (ay+yysize).lt.1.0 .or. (ay-yysize).gt.NY ) return

      call ap_osize ( STAPRX, APANG, ELLIPSE, xsize, ysize )

      if ( ADOCENTRE.eq.2 ) then					!Get star position

         if ( IMTYPE.eq.'SHORT' ) then
            call ap_pmaxs ( %val(IPIM), NX, NY, INVAL, ax, ay,
     +                      xsize/1.414, ysize/1.414, BS )
         else
            call ap_pmaxr ( %val(IPIM), NX, NY, RINVAL, ax, ay,
     +                      xsize/1.414, ysize/1.414, BS )
         endif
         call ds_oval ( ax, ay, STAPRX/2.0, APANG, ELLIPSE,3)

      elseif ( ADOCENTRE.eq.3 ) then

         if ( IMTYPE.eq.'SHORT' ) then
            call ap_cens ( %val(IPIM), NX, NY, INVAL, ax, ay,
     +                     xsize/1.414, ysize/1.414 )
            call ap_cens ( %val(IPIM), NX, NY, INVAL, ax, ay,
     +                     xsize/1.414, ysize/1.414 )
         else
            call ap_cenr ( %val(IPIM), NX, NY, RINVAL, ax, ay,
     +                     xsize/1.414, ysize/1.414 )
            call ap_cenr ( %val(IPIM), NX, NY, RINVAL, ax, ay,
     +                     xsize/1.414, ysize/1.414 )
         endif
         call ds_oval ( ax, ay, STAPRX/2.0, APANG, ELLIPSE, 3)

      elseif ( ADOCENTRE.eq.4 ) then

         call ap_osize ( SKAPRX, APANG, ELLIPSE,xsize, ysize )
         lxg = min(100,int(xsize))
         lyg = min(100,int(ysize))
         kw = 0
         if ( FIXGRAD ) kw = 1
         if ( IMTYPE.eq.'SHORT' ) then
            call gauss2sa ( %val(IPIM), NX, NY, ax, ay, lxg,
     +                      lyg, kw, AGRX, AGRY, INVAL, 20,
     +                      amag, height, base, dx, dy, anx,
     +                      any, rx, ry, rms, iter, ninval )
         else
            call gauss2ra ( %val(IPIM), NX, NY, ax, ay, lxg,
     +                      lyg, kw, AGRX, AGRY, RINVAL, 20,
     +                      amag, height, base, dx, dy, anx,
     +                      any, rx, ry, rms, iter, ninval )
         endif
         if ( amag.gt.49.0 .or. rx.lt.0.1 .or. ry.lt.0.1 .or.
     +        rx.gt.100.0 .or. ry.gt.100.0 ) then
            call printo ( 'ERROR: Cannot fit Gaussian' )
         else
            ax = anx
            ay = any
            aamag = trunc(amag,3)
            arx = trunc(rx,3)
            ary = trunc(ry,3)
            write ( text, '(1x,'' Gauss mag = '',f7.3,'//
     +              ' '' ; Rx '', ''= '',f6.2,'' ; Ry = '',f6.2)' )
     +               aamag, arx, ary
            call printo ( text )
            call ds_oval ( ax, ay, STAPRX/2.0,APANG,ELLIPSE,3)
         endif

      endif
      call ds_oval ( ax, ay, STAPRX, APANG, ELLIPSE, 2 )

      if ( ANNULUS ) then						!Get sky
         call ds_oval ( ax, ay, ANAPRX(1), APANG, ELLIPSE, 1 )
         call ds_oval ( ax, ay, ANAPRX(2), APANG, ELLIPSE, 1 )
         call ap_osize ( ANAPRX(2), APANG, ELLIPSE, xsize, ysize )
         kvx = xsize + 4.0
         kvy = ysize + 4.0
         if ( IMTYPE.eq.'SHORT' ) then
            call gtwrki ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
            call ap_doits ( %val(IPIM), ax, ay, DOMEDIAN, ANAPRX(1),
     +                      ANAPRX(2), %val(ipwkakv), SKFL, SKLEV,
     +                      SKAREA, SKERR, NSKBAD, ngood, rv )
         else
            call gtwrkr ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
            call ap_doitr ( %val(IPIM), ax, ay, DOMEDIAN, ANAPRX(1),
     +                      ANAPRX(2), %val(ipwkakv), SKFL, SKLEV,
     +                      SKAREA, SKERR, NSKBAD, ngood, rv )
         endif
         call wrkcan ( 'WORKAKV' )
         if ( ngood.eq.0 ) call printo ( 'WARNING: No good '//
     +                       'pixels in sky aperture - set to zero')
      endif

      call ap_loval ( ax, ay, 2 )					!List ovals done

      call ap_osize ( STAPRX, APANG, ELLIPSE, xsize, ysize )
      kvx = xsize + 4.0
      kvy = ysize + 4.0
      if ( IMTYPE.eq.'SHORT' ) then
         call gtwrki ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
         call ap_doits ( %val(IPIM), ax, ay, .false., 0.0,  		!Get star
     +                   STAPRX, %val(ipwkakv), STRAW, rv,
     +                   STAREA, rva, NSTBAD, ngood, STTOP )
      else
         call gtwrkr ( 'WORKAKV', kvx*kvy, ipwkakv, ierr )
         call ap_doitr ( %val(IPIM), ax, ay, .false., 0.0,
     +                   STAPRX, %val(ipwkakv), STRAW, rv,
     +                   STAREA, rva, NSTBAD, ngood, STTOP )
      endif
      call wrkcan ( 'WORKAKV' )

      if ( ngood.eq.0 ) call printo (
     +                  'WARNING: No good pixels in star aperture' )

      XST = ax
      YST = ay
      call ap_mag							!Store and type result


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_LOVAL -- List an oval as placed on display
C
C alan penny             ral            1990-02-10

      subroutine ap_loval ( x, y, kopt )

      implicit none
      include 'aper.inc'

      real	x		!i: X position
      real      y		!i: Y position
      integer   kopt		!i: 1= sky aperture; 2=star aperture (+annulus?)
C--
Cbegin

      NCIR = NCIR + 1

      XCIR(NCIR) = x
      YCIR(NCIR) = y
      REMCIR(NCIR) = .false.
      RCIR(5,NCIR) = APANG
      RCIR(6,NCIR) = ELLIPSE

      if ( kopt.eq.1 ) then						!Sky oval
         RCIR(1,NCIR) = -1.0
         RCIR(2,NCIR) = SKAPRX
         RCIR(3,NCIR) = -1.0
         RCIR(4,NCIR) = -1.0
      else								!Star/sky/centred ovals
         RCIR(1,NCIR) = STAPRX
         if ( ADOCENTRE.ne.1 ) then
            RCIR(2,NCIR) = STAPRX/2.0
         else
            RCIR(2,NCIR)  = -1.0
         endif
         if ( ANNULUS ) then
            RCIR(3,NCIR) = ANAPRX(1)
            RCIR(4,NCIR) = ANAPRX(2)
         else
            RCIR(3,NCIR)  = -1.0
            RCIR(4,NCIR)  = -1.0
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_CHCENTRE -- Toggle method of placing centre of aperture
C
C alan penny             ral            1993 Sep

      subroutine ap_chcentre ( )

      implicit none
      include 'aper.inc'
C--
Cbegin


      call printo ( ' ' )
      ADOCENTRE = ADOCENTRE + 1
      if ( ADOCENTRE.eq.5 ) ADOCENTRE = 1
      if ( ADOCENTRE.eq.1 ) then
         call printo ( 'No centering done now        - '//
     +                 'Gaussian centering was being done' )
      elseif ( ADOCENTRE.eq.2 ) then
         call printo ( 'Max pixel centering done now - '//
     +                 'No centering was being done' )
      elseif ( ADOCENTRE.eq.3 ) then
         call printo ( 'Centroid centering done now - ' //
     +                 'Max pixel centering was being done' )
      elseif ( ADOCENTRE.eq.4 ) then
         call printo ( 'Gaussian centering done now  - '//
     +                 'Centroid centering was being done' )
         call get1b ( 'FIXGRAD', FIXGRAD, .false. )
         if ( FIXGRAD ) call get2r ( 'GRADII', AGRX, AGRY, .true.,
     +                               0.1, 100.0 )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_CENR -- Get centroid in oval real area
C
C alan penny             ral            1990-02-10

      subroutine ap_cenr ( im, nx, ny, rinval, x, y, radx, rady )

      integer   nx		!i:   Image X size
      integer   ny		!i:   Image Y size
      real      im(nx,ny)	!i:   Image
      real      rinval		!i:   Bad pixel flag value
      real	x		!i/o: X position
      real      y		!i/o: Y position
      real      radx            !i:   Y radius in which to work
      real      rady            !i:   X radius in which to work
C--
      integer j, k, kxs, kxe, kys, kye, ns
      real d, tx, ty, tv, vmin, vmax, xd, yd, rv
Cbegin


      kxs = x - radx - 1.0				!Search area
      kxe = x + radx + 1.0
      kys = y - rady - 1.0
      kye = y + rady + 1.0
      kxs = max(1,min(nx,kxs))
      kxe = max(1,min(nx,kxe))
      kys = max(1,min(ny,kys))
      kye = max(1,min(ny,kye))

      ns = 0						!Get local minimum
      do k = kys, kye
         yd = real(k) - y
         do j = kxs, kxe
            xd = real(j) - x
            d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
            if ( d.le.1.0 ) then
               rv = im(j,k)
               if ( rv.ne.rinval ) then
                  ns = ns + 1
                  if ( ns.eq.1 ) then
                     vmin = rv
                     vmax = rv
                  else
                     vmin = min(vmin,rv)
                     vmax = max(vmax,rv)
                  endif
               endif
            endif
         enddo
      enddo

      if ( ns.eq.0 .or. vmin.eq.vmax ) then			!Get centroid
         call printo ( 'ERROR: Cannot Centroid Star Position' )
      else
         tv = 0.0d0
         tx = 0.0d0
         ty = 0.0d0
         do k = kys, kye
            yd = real(k) - y
            do j = kxs, kxe
               xd = real(j) - x
               d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
               if ( d.le.1.0 ) then
                  rv = im(j,k)
                  if ( rv.ne.rinval ) then
                     rv = rv - vmin
                     tx = tx + dble(rv*real(j))
                     ty = ty + dble(rv*real(k))
                     tv = tv + dble(rv)
                  endif
               endif
            enddo
         enddo
         x = tx/tv
         y = ty/tv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_CENS -- Get centroid in oval int*2 area
C
C alan penny             ral            1990-02-10

      subroutine ap_cens ( im, nx, ny, inval, x, y, radx, rady )

      implicit none

      integer   nx		!i:   Image X size
      integer   ny		!i:   Image Y size
      integer*2 im(nx,ny)	!i:   Image
      integer   inval		!i:   Bad pixel flag value
      real	x		!i/o: X position
      real      y		!i/o: Y position
      real      radx            !i:   X radius in which to work
      real      rady            !i:   Y radius in which to work
C--
      integer j, k, kxs, kxe, kys, kye, ns, kv
      real    d, tx, ty, tv, vmin, vmax, xd, yd, rv
Cbegin


      kxs = x - radx - 1.0				!Search area
      kxe = x + radx + 1.0
      kys = y - rady - 1.0
      kye = y + rady + 1.0
      kxs = max(1,min(nx,kxs))
      kxe = max(1,min(nx,kxe))
      kys = max(1,min(ny,kys))
      kye = max(1,min(ny,kye))

      ns = 0							!Get local minimum
      do k = kys, kye
         yd = real(k) - y
         do j = kxs, kxe
            xd = real(j) - x
            d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
            if ( d.le.1.0 ) then
               kv = im(j,k)
               if ( kv.ne.inval ) then
                  ns = ns + 1
                  if ( ns.eq.1 ) then
                     vmin = kv
                     vmax = kv
                  else
                     vmin = min(vmin,real(kv))
                     vmax = max(vmax,real(kv))
                  endif
               endif
            endif
         enddo
      enddo

      if ( ns.eq.0 .or. vmin.eq.vmax ) then			!Get centroid
         call printo ( 'ERROR: Cannot Centroid Star Position' )
      else
         tv = 0.0d0
         tx = 0.0d0
         ty = 0.0d0
         do k = kys, kye
            yd = real(k) - y
            do j = kxs, kxe
               xd = real(j) - x
               d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
               if ( d.le.1.0 ) then
                  kv = im(j,k)
                  if ( kv.ne.inval ) then
                     rv = real(kv) - vmin
                     tx = tx + dble(rv*real(j))
                     ty = ty + dble(rv*real(k))
                     tv = tv + dble(rv)
                  endif
               endif
            enddo
         enddo
         x = tx/tv
         y = ty/tv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_PMAXR -- Get position of max pixel in oval real area
C
C alan penny             ral            1990-02-10

      subroutine ap_pmaxr ( im, nx, ny, rinval, x, y, radx, rady, bs )

      implicit none

      integer   nx		!i:   Image X size
      integer   ny		!i:   Image Y size
      real      im(nx,ny)	!i:   Image
      real      rinval		!i:   Bad pixel flag value
      real	x		!i/o: X position
      real      y		!i/o: Y position
      real      radx            !i:   X radius in which to work
      real      rady            !i:   Y radius in which to work
      real      bs		!i:   Scale of image pixel values
C--
      integer j, k, kxs, kxe, kys, kye, ns, kvx, kvy
      real d, xd, yd, rv, rkw, vmax
Cbegin


      kxs = x - radx - 1.0						!Search area
      kxe = x + radx + 1.0
      kys = y - rady - 1.0
      kye = y + rady + 1.0
      kxs = max(1,min(nx,kxs))
      kxe = max(1,min(nx,kxe))
      kys = max(1,min(ny,kys))
      kye = max(1,min(ny,kye))

      rkw = 1.0
      if ( bs.lt.0.0 ) rkw = -1.0
      kvx = (kxe+kxs)/2
      kvy = (kye+kys)/2
      rv = im(kvx,kvy)
      if ( rv.ne.rinval ) then
         vmax = rkw*rv
      else
         vmax = rinval
      endif

      ns = 0
      do k = kys, kye
         yd = real(k) - y
         do j = kxs, kxe
            xd = real(j) - x
            d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
            if ( d.le.1.0 ) then
               rv = im(j,k)
               if ( rv.ne.rinval ) then
                  if ( (rkw*rv).gt.vmax ) then
                     vmax = rkw*rv
                     kvx = j
                     kvy = k
                     ns = ns + 1
                  endif
               endif
            endif
         enddo
      enddo
      x = kvx
      y = kvy

      if ( ns.eq.0 ) call printo ( 'ERROR: Cannot get Maximum Pixel' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_PMAXS -- Get position of max pixel in oval int*2 area
C
C alan penny             ral            1990-02-10

      subroutine ap_pmaxs ( im, nx, ny, inval, x, y, radx, rady, bs )

      implicit none

      integer   nx		!i:   Image X size
      integer   ny		!i:   Image Y size
      integer*2 im(nx,ny)	!i:   Image
      integer   inval		!i:   Bad pixel flag value
      real	x		!i/o: X position
      real      y		!i/o: Y position
      real      radx            !i:   X radius in which to work
      real      rady            !i:   Y radius in which to work
      real      bs		!i:   Scale of image pixel values
C--
      integer j, k, kxs, kxe, kys, kye, ns, kv, kw, kvmax, kvx, kvy
      real d, xd, yd
Cbegin


      kxs = x - radx - 1.0						!Search area
      kxe = x + radx + 1.0
      kys = y - rady - 1.0
      kye = y + rady + 1.0
      kxs = max(1,min(nx,kxs))
      kxe = max(1,min(nx,kxe))
      kys = max(1,min(ny,kys))
      kye = max(1,min(ny,kye))

      kw = 1
      if ( bs.lt.0.0 ) kw = -1
      kvx = (kxe+kxs)/2
      kvy = (kye+kys)/2
      kv = im(kvx,kvy)
      if ( kv.ne.inval ) then
         kvmax = kw*kv
      else
         kvmax = -32768
      endif

      ns = 0
      do k = kys, kye
         yd = real(k) - y
         do j = kxs, kxe
            xd = real(j) - x
            d = (xd*xd)/(radx*radx) + (yd*yd)/(rady*rady)
            if ( d.le.1.0 ) then
               kv = im(j,k)
               if ( kv.ne.inval ) then
                  if ( (kw*kv).gt.kvmax ) then
                     kvmax = kw*kv
                     kvx = j
                     kvy = k
                     ns = ns + 1
                  endif
               endif
            endif
         enddo
      enddo
      x = kvx
      y = kvy

      if ( ns.eq.0 ) call printo ( 'ERROR: Cannot get Maximum Pixel' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_DOITR -- Calc flux through aperture (star/sky/annulus)
C
C  alan penny          ral          1990-02-01

      subroutine ap_doitr ( im, ax, ay, dome, xradi, xrado,
     +                      rva, flux, av, area, err, nbad,
     +                      ngood, top )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)	!i: Image
      real      ax              !i: X position
      real      ay              !i: Y position
      logical   dome		!i: Do median averaging
      real      xradi		!i: Inner X radius
      real      xrado		!i: Outer X radius
      real      rva(*)		!i: Work space
      real      flux		!o: Total flux
      real      av		!o: Average in area
      real	area		!o: Area used
      real      err		!o: Std dev of average
      integer   nbad		!o: No of bad pixels
      integer   ngood		!o: No of good pixels
      real      top		!o: Highest value
C--
      double precision tsum, tsumsq, tarea, esum, earea
      integer j, k, kxs, kxe, kys, kye, km
      real xda, yda, rvxi, rvxo, rvyi, rvyo, dd, xd, yd, rv, sav, cav,
     +     xsize, ysize
      logical inner, outer
Cbegin


      flux = 0.0
      av = 0.0
      area = 0.0
      err = 0.0
      nbad = 0
      ngood = 0
      top = 0.0

      if ( ELLIPSE.lt.0.0 .or. ELLIPSE.ge.1.0 ) return

      dd = max(xradi,xrado)
      call ap_osize ( dd, APANG, ELLIPSE, xsize, ysize )
      kxs = ax - xsize/2.0 - 1.0
      kxe = ax + xsize/2.0 + 1.0
      kys = ay - ysize/2.0 - 1.0
      kye = ay + ysize/2.0 + 1.0

      if ( kxs.gt.NX .or. kxe.lt.1 .or. kys.gt.NY .or. kye.lt.1 ) return

      kxs = max(1,min(NX,kxs))
      kxe = max(1,min(NX,kxe))
      kys = max(1,min(NY,kys))
      kye = max(1,min(NY,kye))

      sav = sin(APANG*3.14159/180.0)
      cav = cos(APANG*3.14159/180.0)

      km = 0
      top = 0.0
      nbad = 0
      ngood = 0
      tsum = 0.0d0
      tsumsq = 0.0d0
      tarea = 0.0d0
      esum = 0.0d0
      earea = 0.0d0
      rvxi = max(xradi,1.0e-6)
      rvyi = rvxi*(1.0-ELLIPSE)
      rvxo = max(xrado,1.0e-6)
      rvyo = rvxo*(1.0-ELLIPSE)
      do k = kys, kye
         yd = real(k) - ay
         do j = kxs, kxe
            xd = real(j) - ax
            xda = cav*xd + sav*yd
            yda = -1.0*sav*xd + cav*yd

            dd = (xda*xda)/(rvxi*rvxi) + (yda*yda)/(rvyi*rvyi)
            inner = .false.
            if ( dd.ge.1.0 ) inner = .true.
            if ( xradi.eq.0.0 ) inner = .true.

            dd = (xda*xda)/(rvxo*rvxo) + (yda*yda)/(rvyo*rvyo)
            outer = .false.
            if ( dd.le.1.0 ) outer = .true.
            if ( xrado.eq.0.0 ) outer = .false.

            if ( inner .and. outer ) then				!In annulus?
               rv = im(j,k)
               if ( rv.eq.RINVAL ) then
                  nbad = nbad + 1
               else
                  ngood = ngood + 1
                  km = km + 1
                  rva(km) = rv
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
         err = flux*POISV + tarea*NOISE*NOISE
         err = max(0.0,err)
         if ( err.gt.0.0 ) err = sqrt(err)/POISV

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
C AP_DOITS -- Calc flux through aperture (star/sky/annulus)
C
C  alan penny          ral          1990-02-01

      subroutine ap_doits ( im, ax, ay, dome, xradi, xrado,
     +                      kva, flux, av, area, err, nbad,
     +                      ngood, top )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'

      integer*2 im(NX,NY)	!i: Image
      real      ax              !i: X position
      real      ay              !i: Y position
      logical   dome		!i: Do median averaging
      real      xradi		!i: Inner X radius
      real      xrado		!i: Outer X radius
      integer   kva(*)		!i: Work space
      real      flux		!o: Total flux
      real      av		!o: Average in area
      real	area		!o: Area used
      real      err		!o: Std dev of average
      integer   nbad		!o: No of bad pixels
      integer   ngood		!o: No of good pixels
      real      top		!o: Highest value
C--
      double precision tsum, tsumsq, tarea, esum, earea
      integer j, k, kxs, kxe, kys, kye, km, kv
      real xda, yda, rvxi, rvxo, rvyi, rvyo, dd, xd, yd, rv, sav, cav,
     +     xsize, ysize
      logical inner, outer
Cbegin


      flux = 0.0
      av = 0.0
      area = 0.0
      err = 0.0
      nbad = 0
      ngood = 0
      top = 0.0

      if ( ELLIPSE.lt.0.0 .or. ELLIPSE.ge.1.0 ) return

      dd = max(xradi,xrado)
      call ap_osize ( dd, APANG, ELLIPSE, xsize, ysize )
      kxs = ax - xsize/2.0 - 1.0
      kxe = ax + xsize/2.0 + 1.0
      kys = ay - ysize/2.0 - 1.0
      kye = ay + ysize/2.0 + 1.0

      if ( kxs.gt.NX .or. kxe.lt.1 .or. kys.gt.NY .or. kye.lt.1 ) return

      kxs = max(1,min(NX,kxs))
      kxe = max(1,min(NX,kxe))
      kys = max(1,min(NY,kys))
      kye = max(1,min(NY,kye))

      sav = sin(APANG*3.14159/180.0)
      cav = cos(APANG*3.14159/180.0)

      km = 0
      top = 0.0
      nbad = 0
      ngood = 0
      tsum = 0.0d0
      tsumsq = 0.0d0
      tarea = 0.0d0
      esum = 0.0d0
      earea = 0.0d0
      rvxi = max(xradi,1.0e-6)
      rvyi = rvxi*(1.0-ELLIPSE)
      rvxo = max(xrado,1.0e-6)
      rvyo = rvxo*(1.0-ELLIPSE)
      do k = kys, kye
         yd = real(k) - ay
         do j = kxs, kxe
            xd = real(j) - ax
            xda = cav*xd + sav*yd
            yda = -1.0*sav*xd + cav*yd

            dd = (xda*xda)/(rvxi*rvxi) + (yda*yda)/(rvyi*rvyi)
            inner = .false.
            if ( dd.ge.1.0 ) inner = .true.
            if ( xradi.eq.0.0 ) inner = .true.

            dd = (xda*xda)/(rvxo*rvxo) + (yda*yda)/(rvyo*rvyo)
            outer = .false.
            if ( dd.le.1.0 ) outer = .true.
            if ( xrado.eq.0.0 ) outer = .false.

            if ( inner .and. outer ) then				!In annulus?
               kv = im(j,k)
               if ( kv.eq.INVAL ) then
                  nbad = nbad + 1
               else
                  rv = kv
                  ngood = ngood + 1
                  km = km + 1
                  kva(km) = rv
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
               av = (kva(j)+kva(j+1))/2.0
            endif
         endif
         av = BS*av + BZ
         err = flux*POISV + tarea*NOISE*NOISE
         err = max(0.0,err)
         if ( err.gt.0.0 ) err = sqrt(err)/POISV

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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MAG -- Calc, store and type out star measure
C
C  alan penny              ral               1990-01-31

      subroutine ap_mag ( )

      implicit none
      include 'aper.inc'
      include 'air.inc'
      include 'ST_IMAGE_INC'
C--
      character text*97, nfilt*4, id*20
      real rv, rva, rvb, trunc, htime, airm, extinc
      integer knst
      external trunc
Cbegin


      call airtime ( htime, airm, extinc, nfilt )			!Get airmass and exp time?

      STFL = STRAW - SKLEV*STAREA
      STMAG = 50.0

      if ( STFL.gt.0.0 ) then						!Magnitude
         STMAG = 30.0 - 2.5*alog10(STFL) + ZP
         if ( DOKTIME ) STMAG = STMAG + 2.5*alog10(EXPTIM)
         if ( DOHTIME ) STMAG = STMAG + 2.5*alog10(htime)
         if ( DOHAIR )  STMAG = STMAG - extinc
      endif

      STFERR = 0.0
      rv = (SKERR+NOISE)*STAREA + STRAW/POISV
      if ( rv.gt.0.0 ) STFERR = sqrt(rv)
      STMERR = 0.0
      if ( STFL.gt.0.0 ) STMERR = 2.5*alog10(1.0+abs(STFERR/STFL))

      NST = NST + 1

      id = ' '								!Name
      id(1:1) = '#'
      knst = NST - NSTLAST
      knst = min(999,knst)
      write ( id(2:4), '(i3.3)' ) knst
      id(5:6) = ': '
      id(7:20) = IMTITLE(1:14)
      AP_NAMES(NST) = id

      rva = trunc(STTOP,6)						!Type result
      rvb = trunc(STFL,7)
      knst = min(999,NST)
      write ( text,'(1x,i3,3x,2f7.1,14x,f10.2,f11.2,f10.2,i5)') knst,
     +             XST, YST, rva, rvb, SKLEV, NSTBAD
      if ( STFL.gt.0.0 ) then
         write ( text(22:35), '(f8.3,f6.3)' ) STMAG, STMERR
      else
         write ( text(22:35), '(''     none     '')' )
      endif
      if ( DOKTIME )  write ( text(73:80), '(f8.3)' ) EXPTIM
      if ( DOHTIME )  write ( text(73:80), '(f8.3)' ) htime
      if ( DOHAIR )  write ( text(82:97), '(f5.3,1x,a4,f6.3)' ) airm,
     +                                                 nfilt, extinc
      call printo ( text )

      if ( NST.ge.NYRS ) then						!Store result
         call pargi ( NYRS )
         call printd ( 'ERROR: Can only store %d  entries' )
         return
      endif

      DATAS(1,NST)  = XST
      DATAS(2,NST)  = YST
      DATAS(3,NST)  = STMAG
      DATAS(4,NST)  = STMERR
      DATAS(5,NST)  = STFL
      DATAS(6,NST)  = STFERR
      DATAS(7,NST)  = STTOP
      DATAS(8,NST)  = STAREA
      DATAS(9,NST)  = real(NSTBAD)
      DATAS(10,NST) = STRAW
      DATAS(11,NST) = SKLEV
      DATAS(12,NST) = SKFL
      DATAS(13,NST) = real(NSKBAD)
      DATAS(14,NST) = SKAREA
      DATAS(15,NST) = XSK
      DATAS(16,NST) = YSK
      DATAS(17,NST) = 2.0*STAPRX
      DATAS(18,NST) = 2.0*SKAPRX
      DATAS(19,NST) = 2.0*ANAPRX(1)
      DATAS(20,NST) = 2.0*ANAPRX(2)
      DATAS(21,NST) = APANG
      DATAS(22,NST) = ELLIPSE
      DATAS(23,NST) = 0.0
      if ( ANNULUS ) DATAS(23,NST) = 1.0
      DATAS(24,NST) = ADOCENTRE
      DATAS(25,NST) = 0.0
      if ( DOMEDIAN ) DATAS(25,NST) = 1.0
      DATAS(26,NST) = NOISE
      DATAS(27,NST) = POISV
      DATAS(28,NST) = 0.0
      if ( DOKTIME ) DATAS(28,NST) = EXPTIM
      DATAS(29,NST) = 0.0
      if ( DOHTIME ) DATAS(29,NST) = htime
      DATAS(30,NST) = ZP
      DATAS(31,NST) = airm
      DATAS(32,NST) = extinc


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_STORE -- Put measures out to file
C
C  alan penny              ral               1990-02-10

      subroutine ap_store ( )

      implicit none
      include 'aper.inc'
      include 'STARMAN_INC'
C--
      character*50 cv
      integer ip, j, k, ka, kout, istat, kt, ko, nrem
      real rid(5)
      logical keep
      character*20 ahead(NXRS)
      data ahead / 'X', 'Y', 'Magnitude', 'Error', 'Flux',
     +             'Flux Error', 'Max Value', 'Area', 'Num Bad Pixels',
     +                'Total Flux',
     +             'Sky Level', 'Sky Flux', 'Num Sky Bad Pixels',
     +                'Sky Area', 'Sky X',
     +             'Sky Y', 'Star Ap Maj Dia', 'Sky Ap Maj Dia',
     +             'Sky Inner Ann Maj Dia', 'Sky Outer Ann Maj Dia',
     +                'Aperture Angle', 'Aperture Ellipticity',
     +             'Annulus Flag', 'Centering Flag', 'Median Flag',
     +                'Extra noise', 'Events/pixel Values',
     +             'Exp Time - Keyboard', 'Exp Time - Image',
     +                'Air Mass', 'Air Corrn', 'Zero Point' /
      character*68 thelp(4)
      data thelp /
     + 'Option   Function',
     + '------   --------',
     + 'Full     Store full (32 column) table',
     + 'Short    Store short (3 column -just x, y, mag details) table'/
Cbegin


      if ( NST.eq.0 ) then
         call printo ( 'ERROR: None to output' )
         return
      endif

      nrem = 0								!Check for removed measures
      if ( NCIR.ne.0 ) then
         do k = 1, NCIR
            if ( RCIR(1,k).gt.0.0 .and. REMCIR(k) ) nrem = nrem + 1
         enddo
         if ( nrem.ne.0 ) then
            call printo ( 'Some measures removed. Keep them?' )
            call get1b ( 'KEEP', keep, .false. )
            if ( ST_FAILED ) return
         endif
      endif

      if ( nrem.eq.NST ) then						!See if any left after removal
         call printo ( 'No measures left after removal' )
         return
      endif

      if ( .not.keep .and. nrem.ne.0 ) then				!Remove 'removed' stars from list
         kout = NST
         ka = 1
         do k = 1, NST
            do while ( RCIR(1,ka).lt.0.0 )
               ka = ka + 1
            enddo
            ka = ka + 1
            if ( REMCIR(ka) ) then
               kout = kout - 1
               do j = ka, kout
                  call amovr ( DATAS(1,j+1), DATAS(1,j), NXRS )
                  AP_NAMES(j) = AP_NAMES(j+1)
               enddo
            endif
         enddo
         NST = kout
      endif

      call get_job ( 'OUTTYPE', 'short:full', kt, 2, thelp, 4 )		!Type of output
      if ( ST_FAILED ) return

      ko = NXRS
      if ( kt.eq.1 ) ko = 3
      call optabw ( 'OUTMAGS', ip, ko+5, NST, .true., istat )		!Output results
      if ( ST_FAILED ) return
      if ( istat.eq.2 ) then
         call printo ( 'No file written' )
      elseif ( istat.eq.0 ) then
         do k = 1, NST
            call nametr ( AP_NAMES(k), rid )
            call coprr ( rid, 5, 1, 1, 5, 1, 1, %val(ip), ko+5, NST,
     +                   1, k )
         enddo
         call coprr ( DATAS, NXRS, NYRS, 1, ko, 1, NST,
     +                %val(ip), ko+5, NST, 6, 1 )
         do k = 1, ko
            call pthead ( 'OUTMAGS', k, ahead(k), istat )
         enddo
         call get1c  ( 'TITLE', cv, 'Results from Interact:Aperture',
     +                 .true. )
         if ( ST_FAILED ) return
         call ptdesc ( 'OUTMAGS', 'TITLE', cv )
         NST = 0
         NSTLAST = 0
         NCIR = 0
         call canpar ( 'OUTMAGS' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_REM -- Remove the ovals near a point in display
C
C  alan penny              ral               1990-02-10

      subroutine ap_rem ( )

      implicit none
      include 'aper.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

C--
      integer jx, jy, kbut, ierr, k, kmin, nxs, nxe, nys, nye, ivx, ivy
      real    dx, dy, d, dd, ddmin, rad, dr, aap, aelli, radx,
     +        xsize, ysize
      logical more
Cbegin


      if ( NCIR.eq.0 ) then						!Error check
         call printo ( 'ERROR: No ovals yet done - or list'//
     +                 ' cleared by output' )
         return
      endif

      call ds_pscur ( DSCURPOSX, DSCURPOSY )				!Place cursor in old posn

      more = .true.  							!Loop removing ovals
      do while ( more )

         call ds_gcur ( .true., jx, jy, kbut, ierr )			!Get position

         if ( ierr.ne.0 ) then

            call printo ( 'ERROR: Cant use that position' )

         elseif ( kbut.eq.3 ) then

            more = .false.

         else

            ddmin = 1.0e12						!Find nearest oval
            kmin = 0							! and log as removed
            do k = 1, NCIR
               if ( .not.REMCIR(k) ) then
                  dx = jx - XCIR(k)
                  dy = jy - YCIR(k)
                  radx = max(RCIR(1,k),RCIR(2,k),RCIR(3,k),RCIR(4,k))
                  call ap_osize ( radx, RCIR(5,k), RCIR(6,k),
     +                            xsize, ysize )
                  dd = (dx*dx)/(xsize*xsize/4.0) +
     +                 (dy*dy)/(ysize*ysize/4.0)
                  if ( dd.lt.1.0 .and. dd.lt.ddmin ) then
                     ddmin = dd
                     kmin = k
                  endif
               endif
            enddo
            if ( kmin.ne.0 ) REMCIR(kmin) = .true.

            if ( kmin.eq.0 ) then					!Return if none found
               call printo ( 'ERROR: Not inside an oval' )
            else

               radx = max(RCIR(1,kmin),RCIR(2,kmin),RCIR(3,kmin),	!Paint image over
     +                    RCIR(4,kmin))
               call ap_osize ( radx, RCIR(5,kmin), RCIR(6,kmin),
     +                         xsize, ysize )
               nxs = XCIR(kmin) - xsize/2.0 - 2.0
               nxe = nxs + xsize + 4.0
               nys = YCIR(kmin) - ysize/2.0 - 2.0
               nye = nys + ysize + 4.0
               nxs = max(1,min(NX,nxs))
               nxe = max(1,min(NX,nxe))
               nys = max(1,min(NY,nys))
               nye = max(1,min(NY,nye))
               call ds_tiv ( nxs, nys, ivx, ivy )
               call ds_acim ( %val(IPIM), NX, NY, IMTYPE, nxs, nxe,
     +                        nys, nye, ivx, ivy, DSWRAP )

               rad = 1.5*(rad+2.0)					!Redraw close ovals
               do k = 1, NCIR
                  if ( .not.REMCIR(k) )then
                     dx = jx - XCIR(k)
                     dy = jy - YCIR(k)
                     d = sqrt(dx*dx + dy*dy)
                     dr = max(RCIR(1,k),RCIR(2,k),RCIR(3,k),RCIR(4,k))
                     if ( d.le.(dr+rad) ) then
                        aap = RCIR(5,k)
                        aelli = RCIR(6,k)
                        if ( RCIR(1,k).gt.0.0 ) call ds_oval (
     +               XCIR(k), YCIR(k), RCIR(1,k), aap, aelli, 2 )
                        if ( RCIR(2,k).gt.0.0 ) call ds_oval (
     +               XCIR(k), YCIR(k), RCIR(2,k), aap, aelli, 2 )
                        if ( RCIR(3,k).gt.0.0 ) call ds_oval (
     +               XCIR(k), YCIR(k), RCIR(3,k), aap, aelli, 2 )
                        if ( RCIR(4,k).gt.0.0 ) call ds_oval (
     +               XCIR(k), YCIR(k), RCIR(4,k), aap, aelli, 2 )
                     endif
                  endif
               enddo
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_LIST -- List the measures so far
C
C  alan penny              ral               1990-02-10

      subroutine ap_list ( )

      implicit none
      include 'aper.inc'
      include 'STARMAN_INC'
C--
      integer k, ka, kb, nbad, nrem
      real x, y, amag, amerr, afl, atop, asklev
      character text*70, tr*1
      logical dolist, doit
Cbegin


      if ( NCIR.lt.1 .or. NST.lt.1 ) then
         call printo ( 'No measures made' )
         return
      endif

      nrem = 0								!Check for removed measures
      dolist = .true.
      if ( NCIR.ne.0 ) then
         do k = 1, NCIR
            if ( RCIR(1,k).gt.0.0 .and. REMCIR(k) ) nrem = nrem + 1
         enddo
         if ( nrem.ne.0 ) then
            call printo ( 'Some measures removed. List them?' )
            call get1b ( 'LIST', dolist, .false. )
            if ( ST_FAILED ) return
         endif
      endif

      call printo ( ' Num     X      Y     Mag   Error    Peak   '//	!Type header
     +              '     Flux       Sky   Bad    Time  Airmass - '//
     +              'Corrn' )

      ka = 0
      do k = 1, NST							!Type stars

         doit = .true.
         if ( nrem.ne.0 ) then
            ka = ka + 1
            do while ( ka.lt.NCIR .and. RCIR(1,ka).lt.0.0 )
               ka = ka + 1
            enddo
            doit = .not.REMCIR(ka)
         endif

         if ( doit .or. dolist ) then

            x = DATAS(1,k)
            y = DATAS(2,k)
            amag = DATAS(3,k)
            amerr = DATAS(4,k)
            afl   = DATAS(5,k)
            atop  = DATAS(7,k)
            nbad  = nint(DATAS(9,k))
            asklev = DATAS(11,k)
            kb = min(999,k)
            tr = ' '
            if ( .not.doit ) tr = 'R'

            if ( afl.gt.0.0 ) then
               write ( text,
     +               '(1x,a1,i3,2f7.1,f8.3,f6.3,f10.2,f11.2,f10.2,i5)')
     +             tr, kb, x, y, amag, amerr, atop, afl, asklev, nbad
            else
               write ( text,
     +               '(1x,a1,i3,2f7.1,14x,f10.2,f11.2,f10.2,i5)')
     +             tr, kb, x, y, atop, afl, asklev, nbad
            endif
            call printo ( text )

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_GKSIZE -- Get aperture radii by keyboard
C
C  alan penny              ral               1990-02-10

      subroutine ap_gksize ( )

      implicit none
      include 'aper.inc'
      include 'STARMAN_INC'
C--
      real rv, rvx
Cbegin


      if ( ST_FAILED ) return

      rvx = 2.0*STAPRX                                            !Star aperture
      call printo ( '   The main star - aperture ' )
      call get1r ( 'APDIAM', rvx, rvx, 1.0, 1.0e8 )
      if ( ST_FAILED ) return
      STAPRX = rvx/2.0

      rvx = 2.0*ANAPRX(1)                                         !Sky annulus inner aperture
      call printo ( '   The annular sky - inner aperture' )
      call get1r ( 'APDIAM', rvx, rvx, 1.0, 1.0e8 )
      if ( ST_FAILED ) return
      ANAPRX(1) = rvx/2.0

      rvx = 2.0*ANAPRX(2)                                         !Sky annulus outer aperture
      call printo ( '  The annular sky - outer aperture' )
      call get1r ( 'APDIAM', rvx, rvx, 1.0, 1.0e8 )
      if ( ST_FAILED ) return
      ANAPRX(2) = rvx/2.0

      rvx = 2.0*SKAPRX                                            !Sky aperture
      call printo ( '  The single sky - aperture' )
      call get1r ( 'APDIAM', rvx, rvx, 1.0, 1.0e8 )
      if ( ST_FAILED ) return
      SKAPRX = rvx/2.0

      if ( ANAPRX(1).gt.ANAPRX(2) ) then
         rv = ANAPRX(2)
         ANAPRX(2) = ANAPRX(1)
         ANAPRX(1) = rv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_GCSIZE -- Get aperture radii by cursor
C
C  alan penny              ral               1990-02-10

      subroutine ap_gcsize ( kopt )

      implicit none
      include 'aper.inc'
      include 'STARMAN_INC'

      integer    kopt		!i: Star (1) or sky (2) apertures option
C--
      integer kx, ky, kbut, kc2
      real    rx1, rx2(2), rv
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 ) then
         call printo ( 'Set star aperture size' )			!Get new star size
         call printo (
     +   '       Buttons: LH smaller; Centre Larger; RH - Quit' )
         rx1 = STAPRX
         rx2(1) = ANAPRX(1)
         rx2(2) = ANAPRX(2)
         kc2 = 2
         if ( .not. ANNULUS ) then
            rx1 = STAPRX
            rx2(1) = 0.0
            rx2(2) = 0.0
            kc2 = -1
         endif
         call ds_ovcus ( .true., rx1, 1, rx2, kc2,
     +                   APANG, ELLIPSE, 1, kx, ky, kbut )
         STAPRX = rx1
         rv = 2.0*STAPRX
         call pargr ( rv )
         call printd ( 'Star aperture diameter = %f ' )
      endif

      if ( kopt.eq.2 ) then

         if ( ANNULUS ) then						!Get new sky size
            call printo ( 'Set sky annulus inner aperture size' )
            call printo (
     +      '       Buttons: LH smaller; Centre Larger; RH - Quit' )
            rx1 = STAPRX
            rx2(1) = ANAPRX(1)
            rx2(2) = ANAPRX(2)
            call ds_ovcus ( .true., rx1, 2, rx2, 1,
     +                      APANG, ELLIPSE, 2, kx, ky, kbut )
            ANAPRX(1) = rx2(1)
            rv = 2.0*ANAPRX(1)
            call pargr ( rv )
            call printd ( 'Sky annulus inner diameter = %f ' )

            call printo ( 'Set sky annulus outer aperture size' )	!Get new sky outer annular size
            call printo (
     +      '       Buttons: LH smaller; Centre Larger; RH - Quit' )
            rx1 = STAPRX
            rx2(1) = ANAPRX(1)
            rx2(2) = ANAPRX(2)
            call ds_ovcus ( .true., rx1, 2, rx2, 1, 		!Get new sky aperture size
     +                      APANG, ELLIPSE, 3, kx, ky, kbut)
            ANAPRX(2) = rx2(2)
            rv = 2.0*ANAPRX(2)
            call pargr ( rv )
            call printd ( 'Sky annulus outer diameter = %f ' )

            if ( ANAPRX(1).gt.ANAPRX(2) ) then
               rv = ANAPRX(2)
               ANAPRX(2) = ANAPRX(1)
               ANAPRX(1) = rv
            endif

         else
            call printo ( 'Set sky aperture size' )
            call printo (
     +      '       Buttons: LH smaller; Centre Larger; RH - Quit' )
            rx1 = SKAPRX
            rx2(1) = 0.0
            rx2(2) = 0.0
            call ds_ovcus ( .true., rx1, 1, rx2, -1,
     +                      APANG, ELLIPSE, 1, kx, ky, kbut )
            SKAPRX = rx1
            rv = 2.0*SKAPRX
            call pargr ( rv )
            call printd ( 'Sky aperture diameter = %f ' )

         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_OSIZE -- Get max X and Y size of oval
C
C   a j penny                 ral               1994

      subroutine ap_osize ( rad, rang, elli, xsize, ysize )

      implicit none

      real    rad	!i: Major axis
      real    rang	!i: Angle to X-axis in degrees
      real    elli	!i: Ellipticity
      real    xsize	!o: Size of X range covered
      real    ysize	!o: Size of Y range covered
C--
      integer kx(180), ky(180), kxmax, kymax, k
      real ang
Cbegin


      ang = abs(rang)
      ang = float(int(ang/180.0))
      if ( ang.gt.90.0 ) ang = ang - 180.0
      if ( rang.lt.0.0 ) ang = -1.0*ang
      ang = abs(ang)

      call ap_calcell ( ang, elli, rad, kx, ky )
      kxmax = 0
      kymax = 0
      do k = 1, 180
         kxmax = max( kxmax,kx(k))
         kymax = max( kymax,ky(k))
      enddo
      xsize = 2*kxmax
      ysize = 2*kymax


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_CALCELL -- Calc coords of 180 points on an ellipse
C
C    alan penny           ral                       1990-02-01

      subroutine ap_calcell ( ang, elli, rmaj, kcx, kcy )

      implicit none

      real    ang		!i: Angle in degrees
      real    elli		!i: Ellipticity
      real    rmaj		!i: Ellipse major axis
      integer kcx(180)		!o: X coords of ellipse
      integer kcy(180)		!o: Y coords of ellipse

C--
      integer k, ka, kk
      real  rv, trv, ttrv, sav, cav, dx, dy, rmin, rrmin,
     +      sdx(4), sdy(4), dax(45), day(45), f1, f2, f3, f4
      data sdx / 1.0, 1.0, -1.0, -1.0 /
      data sdy / 1.0, -1.0, -1.0, 1.0 /
Cbegin


      sav = -1.0*sin(ang*3.14159/180.0)
      cav = cos(ang*3.14159/180.0)

      rmin = 1.0 - elli
      rmin = min(1.0,max(0.0,rmin))
      rrmin = 1.0e12
      if ( rmin.gt.1.0e-6 ) rrmin = 1.0/(rmin*rmin)
      do k = 1, 45

         rv = real(k-1)*2*3.14159/180.0
         trv = tan(rv)
         ttrv = trv*trv

         if ( abs(trv).lt.1.0e-5 ) then
            dax(k) = 0.0
            day(k) = rmin
         elseif ( trv.gt.1.0e5 .or. trv.lt.-1.0e5 ) then
            dax(k) = 1.0
            day(k) = 0.0
         else
            dax(k) = sqrt(1.0/(1.0+(rrmin/ttrv)))
            day(k) = sqrt(1.0/(rrmin+(1.0*ttrv)))
         endif
      enddo

      do kk = 1, 4
         ka = (kk-1)*45
         f1 = cav*rmaj*sdx(kk)
         f2 = -1.0*sav*rmaj*sdy(kk)
         f3 = sav*rmaj*sdx(kk)
         f4 = cav*rmaj*sdy(kk)
         do k = 1, 45
            if ( kk.eq.2 .or. kk.eq.4 ) then
               dx = f1*dax(46-k) + f2*day(46-k)
               dy = f3*dax(46-k) + f4*day(46-k)
            else
               dx = f1*dax(k) + f2*day(k)
               dy = f3*dax(k) + f4*day(k)
            endif
            kcx(k+ka) = dx
            kcy(k+ka) = dy
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MAINGET -- Get main prog params to 'aperture' ones
C
C   a j penny                 ral               1990-06-09

      subroutine ap_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ap_srmainget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine ap_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_MAINPUT -- Put 'aperture' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine ap_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ap_srapget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AP_SRAPGET -- Get 'aperture' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine ap_srapget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'

C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    COLOUR.F
C
C    Contains:-
C
C T_COLOUR      Change colour look-up table
C CO_OPTION_SETUP   Set up option choices
C CO_MAINGET    Get main prog params to 'colour' ones
C CO_SRMAINGET  Get main prog params to temp params
C CO_MAINPUT    Put 'colour' params to main prog ones
C CO_SRAPGET    Get 'colour' s/r params to temp ones


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_COLOUR -- Change colour look-up table
C
C   alan penny                   ral                  1990-01-31

      subroutine t_colour ( )

      implicit none
      include 'interact.inc'
      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer ierr, kl, kx, ky, kb
      logical loop
      character*12 ktopt, ktoptc
Cbegin


      if ( ST_FAILED ) return

      call co_mainget

      if ( .not.GOTIMAGE .or. .not.DISPLAYED ) then
         call printo ( 'No displayed image yet' )
         return
      endif

      call in_opdisp ( ierr )						!Check display open

      call ds_lutbar ( 1 )						!Show LUT bar

      call co_option_setup ( ktopt, 6, .true. )
      if ( ST_FAILED ) return
      loop = .true.							!Loop through options
       do while ( loop )

         if ( ktopt.eq.'false' ) ktopt = ktoptc

         call co_option_setup ( ktopt, 6, .false. )
         call get_choice ( ktopt, 1 )					!Get choice

         if ( ktopt.eq.'flash' .or. ktopt.eq.'display' ) then		!Check image there if needed
             if ( .not.GOTIMAGE ) then
                call printo ( 'No image yet' )
                ktoptc = ktopt
                ktopt = 'false'
             endif
         endif

         if ( ktopt.eq.'rotate' ) call ds_lutrot 			!Rotate

         if ( ktopt.eq.'scale' ) call ds_lutsca 			!Squeeze

         if ( ktopt.eq.'bar_clear' ) then				!Clear display of LUT bar
                       call ds_lutbar ( 0 )
                       endif

         if ( ktopt.eq.'bar_show' ) then				!Clear display of LUT bar
                       call ds_lutbar ( 1 )
                       endif

         if ( ktopt.eq.'load' ) then					!Load LUT
                       call get1i ( 'LUTNUM', kl, 1, 1, 15 )
                       if ( ST_FAILED ) return
                       call ds_lutcol ( kl )
                       endif

         if ( ktopt.eq.'wrap' ) then					!Toggle annulus or separate
                        DSWRAP = .not.DSWRAP
                        if ( DSWRAP ) call printo (
     +                                 'Default: Values wrapped round' )
                        if ( .not.DSWRAP ) call printo (
     +                            'Default: Values not wrapped round' )
                        endif

         if ( ktopt.eq.'file_get' ) call ds_lutread ( 1 )

         if ( ktopt.eq.'file_sget' ) call ds_lutread ( 2 )

         if ( ktopt.eq.'file_put' ) call ds_lutwrite

         if ( ktopt.eq.'ends_bw' ) call ds_lutends ( 1 )		!Ends

         if ( ktopt.eq.'ends_wb' ) call ds_lutends ( 2 )		!Ends

         if ( ktopt.eq.'ends_cc' ) call ds_lutends ( 3 )		!Ends

         if ( ktopt.eq.'ends_wrap' ) call ds_lutends ( 4 )		!Ends

         if ( ktopt.eq.'flip' ) call ds_lutflip 			!Flip

         if ( ktopt.eq.'paint' ) call ds_lutpai		 		!Paint

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom/pan

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Reset

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb, 	!Look at posn/pixel vals
     +                                             ierr )

         if ( ktopt.eq.'flash' ) then                                   !Flash image
                          call in_opdisp ( ierr )
                          call ds_doflash ( %val(IPIM), NX, NY, IMTYPE,
     +                                      DSKVRANGE, IMTITLE )
                          DISPLAYED = .true.
                          endif

         if ( ktopt.eq.'display' ) then                                 !Display
                          call in_opdisp ( ierr )
                          call ds_dodisp ( %val(IPIM), NX, NY, IMTYPE,
     +                                     DSKVRANGE, IMTITLE )
                          DISPLAYED = .true.
                          endif

         if ( ktopt.eq.'area' ) then                                    !Change area to show
                                call in_imgsiz ( NX, NY, 1 )
                                endif

         if ( ktopt.eq.'close' ) then                                   !Close display
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'clear' .and. OPDISP ) then                      !Erase display
                                              call ds_erase
                                              DISPLAYED = .false.
                                              endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )                 !Open display

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice

         if ( ktopt.eq.'main' .or. ktopt.eq.'alter' .or. 		!Return to main program
     +        ktopt.eq.'aperture' .or.
     +        ktopt.eq.'inspect' .or. ktopt.eq.'fit_magns' .or.
     +        ktopt.eq.'positions' .or. ktopt.eq.'scrutiny' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo								!Loop round again ?

      call co_mainput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CO_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1990-01-31

      subroutine co_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'

      character*12   ktopt		!i: Chosen option
      integer set_num                   !i: Code for set of options
      logical  koutside                 !i: Is this called from loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=33 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return.' /

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'bar_clear', 'Remove display of LUT table bar',
     + 'The -LUT table bar- is displayed in a delineated area at the',
     + 'bottom of the image. It shows the section of actual pixel ',
     + 'values that when scaled and shifted to the 0-255 display ',
     + 'range. Thus one can see the colours associated with the LUT.',
     + ' ',
     + 'This option -removes- the bar from the display. ' /

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'bar_show', 'Display LUT bar',
     + 'The -LUT table bar- is displayed in a delineated area at the',
     + 'bottom of the image. It shows the section of actual pixel ',
     + 'values that when scaled and shifted to the 0-255 display ',
     + 'range. Thus one can see the colours associated with the LUT.',
     + ' ',
     + 'This option -places- the bar in the display. ' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'ends_bw', 'Values below/above LUT ends as black/white',
     + 'When the LUT has been -rotated- or -shifted- so that it does ',
     + 'not fill the 0-255 range that the image pixel values have been',
     + 'mapped into for display, there are -empty- values in the LUT.',
     + ' ',
     + 'For these -empty- values, act as though the LUT colours below',
     + 'the bottom are black and those above the top, white. ' /

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'ends_wb', 'Values below/above LUT ends as white/black',
     + 'When the LUT has been -rotated- or -shifted- so that it does ',
     + 'not fill the 0-255 range that the image pixel values have been',
     + 'mapped into for display, there are -empty- values in the LUT.',
     + ' ',
     + 'For these -empty- values, act as though the LUT colours below',
     + 'the bottom are white and those above the top, black. ' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'ends_cc', 'Values below/above LUT ends as col/col',
     + 'When the LUT has been -rotated- or -shifted- so that it does ',
     + 'not fill the 0-255 range that the image pixel values have been',
     + 'mapped into for display, there are -empty- values in the LUT.',
     + ' ',
     + 'For these -empty- values, act as though the LUT colours at the',
     + 'ends are extended into them.' /

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'ends_wrap', 'Values below/above LUT ends as wrapped',
     + 'When the LUT has been -rotated- or -shifted- so that it does ',
     + 'not fill the 0-255 range that the image pixel values have been',
     + 'mapped into for display, there are -empty- values in the LUT.',
     + ' ',
     + 'For these -empty- values, act as though the LUT is duplicated',
     + 'below and above its end.' /

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'flip', 'Flip LUT',
     + 'Reverse the Look-Up Table, so that pixels painted with the',
     + 'colours appropriate to the LUT values 0 to 255 are now ',
     + 'painted with the colours for 255 to 0. ',
     + 'Thus, for example, if the LUT was a simple grey scale, a ',
     + 'black pixel will now be white and a white one black.',
     + ' ' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'file_get', 'Read a full Look-Up Table from a file',
     + 'A Look-Up Table is read in from a file.',
     + 'This reads in such a -Starman- table file, in the -LONG- ',
     + 'format of a file produced by the -store- option in this list. ',
     + 'This stores the red/green/blue components of each colour.',
     + ' ',
     + 'See the manual for further information.' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'file_sget', 'Read a short Look-Up Table from a file',
     + 'A Look-Up Table is read from a file using the -SHORT- format.',
     + 'The format is one whereby the red/green/blue component values',
     + 'are stored at a number of positions in the LUT, and the ',
     + 'program interpolates between them when loading the LUT',
     + ' ',
     + 'See the manual for further information.' /

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'load', 'Load LUT from store',
     + 'The program has eight Look-Up Tables kept in its insides.',
     + 'This option loads one onto the image display.',
     + 'The default one (black at bottom, white at top) is number one.',
     + 'The others are:- 2)purple-black; 3)yellow-red; 4)various;',
     + '5)black-yellow; 6)blue-purple;7)stepped various;8)white-black',
     + ' ' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'paint', 'Pick a colour from LUT bar and overwrite range of LUT',
     + 'Select a single colour from those displayed on the LUT bar, ',
     + 'and overwrite any other range of the LUT with that colour. ',
     + 'Place cursor on desired colour, press and release left-hand',
     + 'Move cursor to start of desired range of LUT bar, press LH',
     + 'button, move cursor to end of range, release button. Repeat as',
     + 'required. Press the right-hand button to leave this option.'/

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'rotate', 'Rotate LUT by moving cursor',
     + 'Place cursor in image, hold left hand or centre mouse button',
     + 'down, and move cursor across image. -Horizontal- movement will',
     + 'rotate the LUT. -Vertical- movement will not affect the LUT.',
     + 'Action stops when you move outside the image. Release the ',
     + 'button, replace, repress and restart. To end, release and ',
     + 'press the right hand button.' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'scale', 'Shift and scale LUT by moving cursor',
     + 'Place cursor in image, hold left hand or centre mouse button',
     + 'down, and move cursor across image. -Horizontal- movement will',
     + 'rotate the LUT, -up- movement will expand it, -down- will',
     + 'compress it. Action stops when you move outside the image. ',
     + 'Release the button, replace, repress and restart. To end, ',
     + 'release and press the right hand button.' /

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'file_put', 'Write the present Look-Up Table to file',
     + 'Write the presently employed Look-Up Table to a file. ',
     + 'The file produced is a -Starman- table file with each of the ',
     + '(up to) 256 colours stored as a red/green/blue vector.',
     + ' ',
     + 'This file may be read back in with the -Input- option.',
     + ' ' /

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'wrap', 'Toggle how image values above/below limits shown',
     + 'This option toggles the default suggestion as to which of the',
     + 'the two methods of how to show image values which are above ',
     + 'or below the limits of the Look-Up Table. This is either to',
     + 'give them at the LUT end colours, or to -wrap- the image pixel',
     + 'values, modulo the range of the LUT. ',
     + 'This choice is put into operation in the -DISPLAY- option. ' /

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'area', 'Select area of image to display',
     + 'Only use that part of the image defined by the XRANGE and',
     + 'YRANGE keyboard parameters, which give the start and end',
     + 'pixels in X and Y to use.',
     + ' ', ' ', ' '/

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from program. Do not access main option list first.',
     + ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /

      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /


      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact - Colours', 'COPTION', 6 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'load' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / 'LOAD', 'ALTER', 'ENDS', 'BAR', 'DISPLAY',
     +                 'FUNCTIONS', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'load:file_put:file_get:file_sget' /
      data sect_text(2) / 'flip:paint:rotate:scale:wrap' /
      data sect_text(3) / 'ends_bw:ends_wb:ends_cc:ends_wrap' /
      data sect_text(4) / 'bar_clear:bar_show' /
      data sect_text(5) / 'area:clear:close:cvalues:display:
     +                     flash:open:reset:zoom' /
      data sect_text(6) / 'alter:aperture:fit_magns:
     +                     inspect:main:positions:scrutiny' /
      data sect_text(7) / 'panel:exit' /

      integer help_num
      parameter ( help_num=53 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '                                 ' ,
     + '****  This colour system uses a Look-Up Table (LUT)  ****' ,
     + 'The actual pixel values in the image are already displayed',
     + 'by scaling and shifting so that a range of those values lies ',
     + 'between 0 and 255. To each of these 256 -values-, a colour',
     + 'is now ascribed, by means of a Look-Up Table (LUT).',
     + 'These options allow you to change the colours ascribed to',
     + 'to each -value-, in a number of ways. Actual pixel values ',
     + 'which fell outside the original range, can never have a range',
     + 'of colours, as they are all set at the end value. To change '/
      data (help_text(k),k=11,20) /
     + 'that, redisplay the image. Likewise for fine gradations.' ,
     + '               ' ,
     + 'Paint Function Use:-' ,
     + '         Place cursor on displayed LUT bar' ,
     + '         Press left or centre button' ,
     + '         Release' ,
     + '         Place cursor on displayed LUT bar' ,
     + '         Press left or centre button' ,
     + '         Move cursor along LUT bar' ,
     + '         Release button' /
      data (help_text(k),k=21,30) /
     + '         Repeat or press right button to exit' ,
     + ' ',
     + ' Buttons from left:-    (1)         (2)            (3) ',
     + ' -----------------------------------------------------' ,
     + ' Rotate LUT          Operate when held down       End' ,
     + '                       Move cursor left/right'  ,
     + ' Scale  LUT          Operate when held down       End',
     + '                       Move cursor left/right' ,
     + '                           and up/down',
     + ' ' /
      data (help_text(k),k=31,40) /
     + 'For workstation use, the Cursor must be in image window area',
     + 'for the cursor buttons to work when in -WORKING- mode. The ' ,
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' ' ,
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' ,
     + '     Centre Button twice            = zoom x2' ,
     + '     Left Button then Centre Button = pan' /
      data (help_text(k),k=41,50) /
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Zoom means zoom around present position of cursor ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-'  ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' ,
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- '  /
      data (help_text(k),k=51,help_num) /
     + 'X Windows        YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits ' /

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
C CO_MAINGET -- Get main prog params to 'colour' ones
C
C   a j penny                 ral               1990-06-09

      subroutine co_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ap_srmainget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CO_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine co_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CO_MAINPUT -- Put 'colour' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine co_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ap_srapget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CO_SRAPGET -- Get 'colour' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine co_srapget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    INSPECT.FOR
C
C    Contains:-
C
C T_INSPECT     Perform the INSPECT program function
C INS_OPTION_SETUP  Set up options choice
C INS_MAINGET   Translate main prog params to 'inspect' ones
C INS_SRMAINGET Get main prog params to temp ones
C INS_MAINPUT   Translate 'inspect' params to main prog ones
C INS_SRINSGET  Get 'inspect' s/r params to temp ones
C INS_SDEF      Set up default parameters into common
C INS_STYPE     Type out setup
C INS_DETAILS   Type out image details
C INS_GDINFO    Access 'display' parameters
C INS_DESCR     Access descriptors of a file
C INS_GTAREA    Get the area of interest as 'xs,xe,xstep'  'ys,ye,ystep'
C INS_GTSTEP    Get the steps in the area of interest
C INS_GCURSE    Get the area of interest via cursor; Get step size
C INS_FORMAT    Get display parameters
C INS_VTYPE     Type out the values on the CL
C INS_RTOC      Make real number character string
C INS_PVTYPE    Type out the values as a 'picture' on the CL
C INS_LOLINE    Print out single spaced x-coords posn line
C INS_CHINT     Convert an integer into a series of integers
C INS_GSOLID    Put out a "3-D" graph of the values on the graph window
C INS_ASTATS    Area statistics
C INS_HISTO     Put out histogram
C INS_RADIUS    Get radius, height of star
C INS_SLICE     Plot out slice through image
C INS_SLICEA    Plot out slice through image (part 2)
C INS_GCONTOUR  Put out contour plot to device
C INS_LOADX     Load X values for histogram
C INS_GDISPLAY  PGPLOT picture of image area to device (hardcopy maybe)
C INS_OPDISP	Open display


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_INSPECT -- Perform the INSPECT program function
C
C   a j penny                 ral      1990-06-15

      subroutine t_inspect ( )

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_PANEL_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ierr, kx, ky, kb, iva(2)
      logical loop
      real    rvb, rvc
      character*12 ktopt
Cbegin


      if ( ST_FAILED ) return

      call ins_mainget				  			!Bring in new info

      if ( .not.GOTIMAGE ) then
         call printo ( 'No image got yet' )
         return
      endif

      call ins_option_setup ( ktopt, 3, .true. )
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )							!Loop thru options

         call ins_option_setup ( ktopt, 3, .false. )
         call get_choice ( ktopt, 1) 					!Get choice

         if ( ktopt.eq.'area_c' .or. ktopt.eq.'display' .or.
     +        ktopt.eq.'flash' .or. ktopt.eq.'zoom' .or.
     +        ktopt.eq.'reset' .or. ktopt.eq.'blink' .or.
     +        ktopt.eq.'display' .or. ktopt.eq.'im_get_flash' )
     +      call ins_opdisp ( ierr )

         if ( ktopt.eq.'area_c' ) call ins_gcurse			!Get area via cursor

         if ( ktopt.eq.'display' ) then					!Display image
                       call ds_dodisp ( %val(IPIM), NX, NY, IMTYPE,
     +                                  DSKVRANGE, IMTITLE )
                       DISPLAYED = .true.
                       endif

         if ( ktopt.eq.'flash' ) then					!Flash image
                       call ds_doflash ( %val(IPIM), NX, NY, IMTYPE,
     +                                   DSKVRANGE, IMTITLE )
                       DISPLAYED = .true.
                       endif

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom/pan display

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Reset display zoom/pan

         if ( ktopt.eq.'area_k' ) call ins_gtarea			!Get display area

         if ( ktopt.eq.'typeform' ) call ins_format			!Get values format

         if ( ktopt.eq.'descriptor' ) call ins_descr ( GOTIMAGE ) 	!Get keyword value

         if ( ktopt.eq.'details' ) call ins_details			!Type out image details

         if ( ktopt.eq.'values' ) call ins_vtype 			!Type out values

         if ( ktopt.eq.'pvalues' ) call ins_pvtype 			!Type out values as 'picture'

         if ( ktopt.eq.'solid_w' ) call ins_gsolid ( %val(IPIM), 	!Display 3-D graph of values
     +                                               %val(IPIM), 1 )

         if ( ktopt.eq.'solid_tw' ) call ins_gsolid ( %val(IPIM), 	!Display 3-D graph (hidden
     +                                                %val(IPIM), 2 )	! lines) of values

         if ( ktopt.eq.'controls' ) call ins_stype			!Type out setup

         if ( ktopt.eq.'stats' ) call ins_astats ( GOTIMAGE ) 		!Get area statistics

         if ( ktopt.eq.'histogram' ) call ins_histo ( %val(IPIM), 	!Calculate histogram
     +                                                %val(IPIM)  )

         if ( ktopt.eq.'gclose' ) call gd_close				!Close graphwindow

         if ( ktopt.eq.'contour' ) call ins_gcontour 			!Contour plot

         if ( ktopt.eq.'steps' ) call ins_gtstep			!Get display area sampling step

         if ( ktopt.eq.'slice' ) call ins_slice 			!Plot image slice

         if ( ktopt.eq.'gdisplay' ) call ins_gdisplay 			!Pgplot copy to a device

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb, 	!Get images values by cursor
     +                                             ierr )

         if ( ktopt.eq.'image' ) call in_newim ( ierr )			!Get new image

        if ( ktopt.eq.'im_get_flash' ) then				!Input new image and display
                                         call in_newim ( ierr )
                                         if ( .not.ST_FAILED .and.
     +                                        ierr.eq.0 ) then
                                         call ds_doflash ( %val(IPIM),
     +                                   NX, NY, IMTYPE, DSKVRANGE,
     +                                   IMTITLE )
                                         DISPLAYED = .true.
                                         endif
                                      endif

         if ( ktopt.eq.'area' ) then                                    !Change area to show
                                call in_imgsiz ( NX, NY, 1 )
                                endif

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice

         if ( ktopt.eq.'blink' ) call ds_blink ( iva, rvb, rvc )	!Blink image

         if ( ktopt.eq.'radius' ) call ins_radius 			!Get radius of star

         if ( ktopt.eq.'clear' ) call ds_erase 				!Clear screen

         if ( ktopt.eq.'close' ) then                                   !Close display screen
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )                 !Open display screen

         if ( ktopt.eq.'main' .or. ktopt.eq.'alter' .or. 		!Return to main program
     +        ktopt.eq.'aperture' .or. ktopt.eq.'colour' .or.
     +        ktopt.eq.'fit_magns' .or.
     +        ktopt.eq.'positions' .or. ktopt.eq.'scrutiny' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo

      call ins_mainput


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_OPTION_SETUP -- Set up options choice
C
C   alan penny                        ral              1990-01-31

      subroutine ins_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'

      character*12   ktopt		!i: Chosen option
      integer        set_num            !i: Code for set of options
      logical        koutside		!i: Is this called from outside loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=39 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/


      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return.' /

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'im_get_flash', 'Input new image and display it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, without displaying an image.',
     + 'The screen may be any size you want, through keyboard entry',
     + 'via the SCREEN parameter.',
     + ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'area', 'Select area of image to display',
     + 'If the image is too large for the display screen, one can ',
     + 'select an area to display. ',
     + 'This is done through the keyboard parameters, XRANGE and',
     + 'YRANGE, which delimit the X and Y areas to work with.',
     + ' ', ' ' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'area_c', 'Choose (by cursor) new area for inspection ',
     + 'Some of the actions in this panel operate on a sub-area of the',
     + 'image (default whole image). A sub-area is chosen by putting',
     + 'the cursor on a location in the displayed image and pressing',
     + 'a mouse button, twice. This area is then painted with a ',
     + 'pale blue rectangle. The STEPS option can then define internal',
     + 'stepping in this area. NOT ALL ACTIONS USE THIS STEPPING. '/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'area_k', 'Choose (by keyboard) new area for inspection ',
     + 'Some of the actions in this panel operate on a sub-area of the',
     + 'image. This is selected by the keyboard parameters XAREA and',
     + 'YAREA (default whole image). These define not only the',
     + 'rectangular area, but the grid of pixels inside it to use. So',
     + '-1,7,2- will mean that only pixels at steps 1,3,5,7 are used.',
     + '**** Not all actions use this internal stepping. ****'/

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'blink', 'Blink present display',
     + 'By alternately putting the window -above- and -below- any ',
     + 'other window present, -blinking- is simulated. To use, open',
     + 'another window, display an image you want blinked this one,',
     + 'and align it with this one. The BLINK_CH keyboard parameter',
     + 'chooses between end:auto:undr:over modes. For auto, ',
     + 'the LH button slows x2 the blink, centre speeds x2, RH exits.' /

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'controls', 'Type out present control parameters',
     + 'This give some general information about the image, and then',
     + 'tells you:- (1) Inspection area; (2) Typing out format for',
     + 'values in that area; (3) Image display parameters; (4) Image',
     + 'selected area. ',
     + ' ', ' ' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'descriptor', 'Type out a selected image descriptor',
     + 'Get value of a descriptor in the Starman image extension.',
     + '(A null DESNAME input will give all descriptors in extension.)',
     + 'Remember Starman programs only use the Starman extension. To',
     + 'look at other extensions, use KAPPA programs ',
     + 'NDFTRACE/FITSLIST/FITSIMP. Starman IMPORT can load from these',
     + 'extensions to the Starman extension.'/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'gclose', 'Close graphical device ',
     + 'Options with picture or graphical output (contour,gdisplay',
     + ',solid_w,solid_tw,histogram,slice) may need to -close-',
     + 'the PGPLOT -device-. One might want to swop between screen ',
     + '(xwindows) and printer, or one might want to get the ',
     + 'output from printer [THIS FILE ONLY BECOMES AVAILABLE WHEN',
     + 'THE DEVICE IS -CLOSED-]. All options open device if closed.'/

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'contour', 'Contour (PGPLOT) image area to device (maybe hard)',
     + 'Take the selected (area_c,area_k, or default) area and plot',
     + 'out a contour map of it in a graphical device.',
     + 'You choose (through parameter CONTOUR) the lowest and highest',
     + 'contour levels and cotour intervals and what device to ',
     + 'plot it on. [The screen name is -xwindows-.] ',
     + '[IF THIS MAKES A FILE, PRESS GCLOSE BEFORE USING FILE.]' /

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'gdisplay', 'Display (PGPLOT) image area to device (maybe hard)',
     + 'Take the selected (area_c,area_k, or default) area and put',
     + 'a greyscale rendering picture of it in a graphical device.',
     + 'You choose (through parameter VPRANGE) the lowest and highest',
     + 'pixel values to scale between. (This can be used to put a',
     + 'greyscale out to a printer.) [The screen name is -xwindows-.]',
     + '[IF THIS MAKES A FILE, PRESS GCLOSE BEFORE USING FILE.]' /

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'solid_w', 'Display (PGPLOT) solid body plot - hidden lines',
     + 'Put to graphical device a -solid body- plot (lines between',
     + 'pixel values) of a selected (area_c,area_k, or default) area.',
     + 'Choose (through parameter ORIENT) -side- to -view- image from',
     + '(in steps of 30 degrees from -front-), and (through VPRANGE) ',
     + 'pixel values to go between. Lines -behind- others are hidden.',
     + 'Screen name is -xwindows-. PRESS GCLOSE BEFORE USING A FILE.'/

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'solid_tw', 'Display (PGPLOT) solid body plot - transparent',
     + 'Put to graphical device a -solid body- plot (lines between',
     + 'pixel values) of a selected (area_c,area_k, or default) area.',
     + 'Choose (through parameter ORIENT) -side- to -view- image from',
     + '(in steps of 30 degrees from -front-), and (through VPRANGE) ',
     + 'pixel values to go between. Lines -behind- others are shown.',
     + 'Screen name is -xwindows-. PRESS GCLOSE BEFORE USING A FILE.'/

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'histogram', 'Display area histogram',
     + 'Take a selected (area_c,area_k, or default) area and put ',
     + 'the histogram of the pixel values in a graphical device.',
     + 'You choose (through parameter HRANGE) the lowest and highest',
     + 'pixel values to take notice of, and the size of pixel value',
     + 'bins to use. [The screen name is -xwindows-.] ',
     + '[IF THIS MAKES A FILE, PRESS GCLOSE BEFORE USING FILE.]' /

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'pvalues', 'Type out the look area values as a picture',
     + 'Code pixel values into single digits and type the ',
     + 'selected (area_c,area_k, or default) area, as a kind of ',
     + 'simple picture. The values are scaled between lower and ',
     + 'upper limits of -3/+2 std devs of the mean value.',
     + 'The values go between 1 and 9, with values below the limits',
     + 'are shown as a -blank-, those above as an -asterisk-. ' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'radius', 'Get radius, height of star',
     + 'Determine the Gaussian radii of a series of star-like objects.',
     + 'First define (IGBOX) the X,Y box size. Then use cursor repeat-',
     + 'edly to select positions, and a 2-D Gaussian is fitted to a',
     + '-star- in a box round them (shown). With fixed radii, these',
     + 'are asked for (IGRADII). The fit fails if star too far away.',
     + '|Buttons: Left=Fit-fit radii /Centre=Fit-fix radii /Right=End|'/

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'slice', 'Plot a slice across the image',
     + 'Plot to a graphical device, a -slice- through a stretch of the',
     + 'Use cursor to define start and end points of line, at any',
     + 'angle. The values along this line are then plotted out between',
     + 'limits input by you (through VPRANGE).',
     + '[The screen graphical device name is -xwindows-.] ',
     + '[IF THIS MAKES A FILE, PRESS GCLOSE BEFORE USING FILE.]' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'stats', 'Calculate areas mean  std dev max min',
     + 'Take the selected sub-area (ignoring any internal stepping) ',
     + 'and calculate the pixel value statistics. It calculates the ',
     + 'mean and standard deviation, the median, the minimum and ',
     + 'maximum, and the number of pixels flagged as -bad- values ',
     + ' ', ' ' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'steps', 'Change sampling steps in areas',
     + 'In the options which deal with a sub-area of the image, SOME',
     + 'of them only look at a -grid- of pixels in the sub-area. This',
     + 'option, through the STEPS parameter, lets you determine that',
     + 'stepping in X and Y. ',
     + ' ', ' ' /

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'typeform', 'Change format of typing out of values in area',
     + 'The pixel values typed out in the VALUES option are formatted',
     + 'in this option. The WINDOW parameter gives the lower and upper',
     + 'pixel values to be typed out as numbers, The SCALE and BASE',
     + 'parameters reset values [typed out = (value-BASE)/SCALE]. The',
     + 'DECPL parameter gives the number of decimal places to put out.',
     + 'The WIDTH parameter gives the space for each number.' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'values', 'Type out the values in the area',
     + 'Type out pixel values, in a user defined format, for the ',
     + 'selected (area_c,area_k, or default) area. The format is',
     + 'set by the TYPEFORM option. ',
     + ' ', ' ', ' ' /


      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'details', 'Type out the details of the image',
     + 'Type out size, image title, ',
     + 'pixel type (REAL - 32-bit real/SHORT - 16-bit/integer),',
     + 'disk storage factors (bscale, bzero, inval parameters).',
     + ' ', ' ', ' ' /

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from program. Do not access main option list first.',
     + ' ', ' ', ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(34),opt_head(34),(opt_help(j,34),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      data opt_text(35),opt_head(35),(opt_help(j,35),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(36),opt_head(36),(opt_help(j,36),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(37),opt_head(37),(opt_help(j,37),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(38),opt_head(38),(opt_help(j,38),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /

      data opt_text(39),opt_head(39),(opt_help(j,39),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /


      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact - Inspect', 'IOPTION', 3 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'area_c' /

      integer sect_num
      parameter ( sect_num=8 )
      character*10 sect_head(sect_num)
      data sect_head /'AREA', 'GENERAL', 'VALUES', 'GRAPHICS', 'IMAGE',
     +                'DISPLAY', 'FUNCTIONS', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'area_c:area_k:steps' /
      data sect_text(2) / 'details:descriptor:stats:controls:radius:
     +                     blink'/
      data sect_text(3) / 'cvalues:values:pvalues:typeform' /
      data sect_text(4) / 'contour:gclose:gdisplay:histogram:slice:
     +                     solid_w:solid_tw' /
      data sect_text(5) / 'image:im_get_flash' /
      data sect_text(6) / 'area:clear:close:display:
     +                     flash:open:reset:zoom' /
      data sect_text(7) / 'alter:aperture:colour:fit_magns:
     +                     main:positions:scrutiny' /
      data sect_text(8) / 'panel:exit' /

      integer help_num
      parameter ( help_num=33 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ' ,
     + 'For workstation use, the Cursor must be in image window area',
     + 'for the cursor buttons to work when in -WORKING- mode. The ' ,
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' ',
     + ' Buttons for Blink:  Left Button   = slow blink x2' ,
     + '                     Centre Button = speed blink x2' ,
     + '                     Right Button  = switch to hand blink' /
      data (help_text(k),k=11,20) /
     + '     Hand blink operates by keyboard' ,
     + ' ',
     + ' Buttons for Radius: Left Button    = Variable radius' ,
     + '                     Centre Button  = Fixed radius' ,
     + '                     Right Button   = return' ,
     + ' ' ,
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' ,
     + '     Centre Button twice            = zoom x2' ,
     + '     Left Button then Centre Button = pan' /
      data (help_text(k),k=21,30) /
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Zoom means zoom around present position of cursor ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' ,
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' /
      data (help_text(k),k=31,help_num) /
     + ' X windows       YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits ' /

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
C INS_MAINGET -- Get main prog params to 'inspect' ones
C
C   a j penny                 ral               1990-06-09

      subroutine ins_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ins_srmainget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine ins_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_MAINPUT -- Put 'inspect' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine ins_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call ins_srinsget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SRINSGET -- Get 'inspect' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine ins_srinsget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'

C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SDEF -- Set up default parameters into common
C
C alan penny                  ral             1990-06-16

      subroutine ins_sdef ()

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
C--
Cbegin


      NXS = 1	                                                       !X positions for typing
      NXE = NX
      NXSTEP = 1
      NYS = 1    	                                               !Y positions for typing
      NYE = NY
      NYSTEP = 1
      BOT = -1.0e20                             			!Limits on typed values
      TOP = 1.0e20
      DECPL = 1                              				!Typed value decimal width
      WIDTH = 8                              				!"       "    no of decimal places
      BASE = 0.0 	                             			! Scaling of typed value
      SCALE = 1.0
      GOTIMAGE = .false.                              			! Image accessed flag
      ORIENT = 30
      GOTHRANGE = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_DETAILS -- Type out image details
C
C   alan penny                    ral               1994 Oct

      subroutine ins_details ()

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call printo ( '  ' )
      if ( .not.GOTIMAGE ) then
         call printo ( '  No image yet' )
      else
         if ( IMTITLE.eq.' ' ) then
            call printo ( 'Image has no title' )
         else
            call pargc ( IMTITLE )
            call printd ( ' Image title is: %c ' )
         endif
      endif

      call pargi ( NX )
      call pargi ( NY )
      call pargc ( IMTYPE )
      call printd ( ' Size =  %dx%d  :  Type = %c' )

      call pargr ( BS )
      call pargr ( BZ )
      if ( IMTYPE.eq.'SHORT' ) then
         call pargi ( INVAL )
         call printd ( ' Bscale = %f  Bzero = %f  Inval = %d' )
      else
         call pargr ( RINVAL )
         call printd ( ' Bscale = %f  Bzero = %f  Rinval = %f' )
      endif
      call printo ( '  ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_STYPE -- Type out setup
C
C   alan penny                    ral               1990-06-15

      subroutine ins_stype ()

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer dsnxs, dsnxe, dscomfx, dscomfy, dsnys, dsnye, dssnx,
     +        dssny
      logical dswrap, dsopen
      real dsvmin, dsvmax
Cbegin


      if ( ST_FAILED ) return

      call printo ( '  ' )
      if ( GOTIMAGE ) then
         if ( IMTITLE.eq.' ' ) then
            call printo ( 'Image has no title' )
         else
            call pargc ( IMTITLE )
            call printd ( '  Image title is: %c ' )
         endif
         call pargc ( IMTYPE )
         call printd ( ' Image type is: %c ' )
      else
         call printo ( '  No image yet' )
      endif

      call pargi ( NX )
      call pargi ( NY )
      call printd ( '  X size =  %d  Y size = %d ' )

      call pargr ( BS )
      call pargr ( BZ )
      if ( IMTYPE.eq.'SHORT' ) then
         call pargi ( INVAL )
         call printd ( ' BSCALE = %f  BZERO = %f  INVAL = %d' )
      else
         call pargr ( RINVAL )
         call printd ( ' BSCALE = %f  BZERO = %f  INVAL = %f' )
      endif
      call printo ( '  ' )

      call printo ( '  Inspection Area ' )
      call pargi ( NXS )
      call pargi ( NXE )
      call pargi ( NXSTEP )
      call printd ( '    X start = %d  X end = %d  X step = %d ' )
      call pargi ( NYS )
      call pargi ( NYE )
      call pargi ( NYSTEP )
      call printd ( '    Y start = %d  Y end = %d  Y step = %d ' )

      call printo ( '  Inspection area values typing out format ' )
      call pargr ( BOT )
      call pargr ( TOP )
      call printd ( '    Bottom Limit = %f Top limit = %f' )
      call pargr ( SCALE )
      call pargr ( BASE )
      call printd ( '    Numbers scale = %f Base = %f ' )
      call pargi ( WIDTH )
      call pargi ( DECPL )
      call printd ( '    Number width = %d No after decimal = %d' )

      call ins_gdinfo ( dsnxs, dsnxe, dscomfx, dscomfy, dsnys, dsnye,
     +                  dsopen, dsvmin, dsvmax, dswrap, dssnx, dssny )

      call printo ( '  Display Parameters ' )
      call pargl ( dsopen )
      call printd ( '     Display open = %l ' )
      call pargi ( dssnx )
      call pargi ( dssny )
      call printd ( '     Display X size = %d Display Y size = %d' )
      dsvmin = BS*dsvmin + BZ
      dsvmax = BS*dsvmax + BZ
      call pargr ( dsvmin )
      call pargr ( dsvmax )
      call printd ( '     Display low = %f  Display high = %f ' )
      call pargl ( dswrap )
      call printd ( '     Wrap of display = %l ' )
      call printo ( '  Display Area : ' )
      call pargi ( dsnxs )
      call pargi ( dsnxe )
      call pargi ( dscomfx )
      call printd ( '     X start = %d  X end = %d  X step = %d' )
      call pargi ( dsnys )
      call pargi ( dsnye )
      call pargi ( dscomfy )
      call printd ( '     Y start = %d  Y end = %d  Y step = %d' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GDINFO -- Access 'display' parameters
C
C alan penny                   ral          1990-09-15

      subroutine ins_gdinfo ( adsnxs, adsnxe, adscomfx, adscomfy,
     +                        adsnys, adsnye, adsopen, adsvmin,
     +                        adsvmax, adswrap, adssnx, adssny )

      implicit none
      include 'ST_DS_GEN_INC'

      integer adsnxs            !o: Displayed image blh pixel X true image pixel number
      integer adsnxe            !o: Displayed image trh pixel X true image pixel number
      integer adscomfx		!o: Displayed X image compression factor to virtual screen
      integer adscomfy		!o: Displayed Y image compression factor to virtual screen
      integer adsnys            !o: Displayed image blh pixel Y true image pixel number
      integer adsnye            !o: Displayed image trh pixel X true image pixel number
      integer adssnx		!o: Screen (and virtual screen) X size
      integer adssny		!o: Screen (and virtual screen) Y size
      logical adswrap		!o: Displayed image wrap display values around min/max (t/f)?
      logical adsopen		!o: Display open?
      real    adsvmin		!o: Displayed image unscaled display min value
      real    adsvmax		!o: Displayed image unscaled display max value
C--
Cbegin


      adsnxs  = DSNXS
      adsnxe  = DSNXE
      adscomfx = DSCOMFX
      adscomfy = DSCOMFY
      adsnys  = DSNYS
      adsnye  = DSNYE
      adssnx  = DSSNX
      adssny  = DSSNY
      adswrap = DSWRAP
      adsopen = DSOPEN
      adsvmin = DSVMIN
      adsvmax = DSVMAX


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_DESCR -- Access descriptors of a file
C
C alan penny                   ral          1990-09-15

      subroutine ins_descr ( gotimage )

      implicit none
      include 'STARMAN_INC'

      logical	gotimage	!i: Flag for image accessed
C--
      logical more
      integer j, kt, istat, count, nval
      character*80 ext, descr, text(16384)
Cbegin


      if ( ST_FAILED ) return

      if ( .not.gotimage ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      call get1c ( 'EXTNAME', ext, 'STARMAN', .true. )
      if ( ST_FAILED ) return
      call lbgone ( ext )
      call charln ( ext, kt )

      call get1c ( 'DESNAME', descr, ' ', .true. )
      if ( ST_FAILED ) return

      if ( descr.eq.' ' ) then
         more = .true.
         count = 0
         nval = 1
         do while ( more )
            count = count + nval
            call gtdesn_gen ( 'IN', count, ext(1:kt), descr, istat )
            if ( istat.ne.0 ) then
               more = .false.
            else
               call gtdesc_gen ( 'IN', ext(1:kt), descr, text, ' ',
     +                           nval, istat )
               call printo ( descr(1:8)//' '//text(1)(1:68) )
               if ( nval.gt.1 ) then
                  do j = 2, nval
                     call printo ( '        '//' '//text(j)(1:68) )
                  enddo
               endif
            endif
         enddo
         if ( count.eq.1 ) call printo ( 'No descriptors' )
      else
         call gtdesc_gen ( 'IN', ext(1:8), descr, text, ' ',nval,istat)
         if ( istat.ne.0 ) then
            call printo ( 'No descriptor of that name' )
         else
            call printo ( descr(1:8)//' '//text(1)(1:68) )
            if ( nval.gt.1 ) then
               do j = 2, nval
                  call printo ( '        '//' '//text(j)(1:68) )
               enddo
            endif
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GTAREA -- Get the area of interest as 'xs,xe,xstep'  'ys,ye,ystep'
C   The steps are optional.
C
C  alan penny                      ral            1990-06-16

      subroutine ins_gtarea ()

      implicit none
      include 'inspect.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call get3i ( 'XAREA', NXS, NXE, NXSTEP, .true., 1, NX )
      call cswopi ( NXS, NXE )
      NXSTEP = iabs(NXSTEP)

      call get3i ( 'YAREA', NYS, NYE, NYSTEP, .true., 1, NY )
      call cswopi ( NYS, NYE )
      NYSTEP = iabs(NYSTEP)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GTSTEP -- Get the steps in the area of interest
C
C  alan penny                      ral            1990-06-16

      subroutine ins_gtstep()

      implicit none
      include 'inspect.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer maxi
Cbegin


      if ( ST_FAILED ) return

      maxi = max(NX,NY) - 1
      call get2i ( 'STEPS', NXSTEP, NYSTEP, .true., 1, maxi )
      NXSTEP = iabs(NXSTEP)
      NYSTEP = iabs(NYSTEP)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GCURSE -- Get the area of interest via cursor; Get step size
C
C  alan penny                      ral            1990-06-16

      subroutine ins_gcurse ( )

      implicit none
      include 'inspect.inc'
      include 'STARMAN_INC'
C--
      integer kbut, ierr
      real xa, ya, xb, yb
Cbegin


      if ( ST_FAILED ) return

      call ds_gcur ( .true., NXS, NYS, kbut, ierr )
      call pargi ( NXS )
      call pargi ( NYS )
      call printd ( ' X = %d  Y = %d' )
      xa = NXS
      ya = NYS
      call ds_cross ( xa, ya, 2.0, 4 )

      call ds_gcur ( .true., NXE, NYE, kbut, ierr )
      call pargi ( NXE )
      call pargi ( NYE )
      call printd ( ' X = %d  Y = %d' )
      xb = NXE
      yb = NYE

      call ds_line ( xa, ya, xa, yb, 4 )
      call ds_line ( xa, yb, xb, yb, 4 )
      call ds_line ( xb, yb, xb, ya, 4 )
      call ds_line ( xb, ya, xa, ya, 4 )

      call cswopi ( NXS, NXE )
      call cswopi ( NYS, NYE )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_FORMAT -- Get display parameters
C  These are max and min allowed, scaling to do for display, size of decimal places to allow.
C
C  alan penny                       ral                   1990-06-15

      subroutine ins_format ()

      implicit none
      include 'inspect.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call get2r ( 'WINDOW', BOT, TOP, .true., -1.0e20, 1.0e20 )	!Max ,min allowed

      call get1r ( 'SCALE', SCALE, SCALE, -1.0e20, 1.0e20 )		!Scaling to apply
      call get1r ( 'BASE', BASE, BASE, -1.0e20, 1.0e20 )

      call get1i ( 'DECPL', DECPL, DECPL, 0, 10000 )			!Display widths
      call get1i ( 'WIDTH', WIDTH, WIDTH, 1, 10000 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_VTYPE -- Type out the values on the CL
C
C alan penny                  ral             1990-06-16

      subroutine ins_vtype ( )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer j, kl, iv, mxs, mxe, mys, mye
      real rv
      character text*72, ostr*72
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( (NXS.lt.1 .and. NXE.lt.1) .or. 				!Check area is inside
     +     (NXS.gt.NX .and. NXE.gt.NX) .or.
     +     (NYS.lt.1 .and. NYE.lt.1) .or.
     +     (NYS.gt.NY .and. NYE.gt.NY) ) then
	 call printo ( 'Area is outside image' )
         return
      endif

      mxs = min(NX,max(1,NXS))
      mxe = min(NX,max(1,NXE))
      mys = min(NY,max(1,NYS))
      mye = min(NY,max(1,NYE))

      if ( IMTYPE.eq.'SHORT' ) then
         call ins_vtypes ( %val(IPIM), mxs, mxe, mys, mye )
      else
         call ins_vtyper ( %val(IPIM), mxs, mxe, mys, mye )
      endif
      call printo ( ' ' )

      text = ' '	 		                 		!Print x-coords
      kl = 12 - WIDTH				                    	! at bottom
      do j = mxs, mxe, NXSTEP
         kl = kl + WIDTH
         if ( (kl+WIDTH).lt.72 ) then
            if ( WIDTH.eq.1  .and.  DECPL.eq.0 ) then
               iv = j - (j/10)*10
               write ( text(kl:kl), '(i1)' ) iv
            else
               rv = j
               call ins_rtoc ( rv, 0, WIDTH, ostr )
               text(kl:kl+WIDTH-1) = ostr(1:WIDTH)
            endif
         endif
      enddo
      call printo ( text )

      if ( WIDTH.eq.1  .and.  DECPL.eq.0 ) call ins_loline ( mxs, mxe,	!X coord line
     +                                                      NXSTEP )

      call pargr ( SCALE )
      call pargr ( BASE )
      call printd ( '  True values = (these values)*%f + %f')

      call pargr ( TOP )
      call pargr ( BOT )
      call printd('  Data above %f / below %f are put as: * / blank')

      call printo (
     +    '  Data too wide to fit are put as: -999.9/9999.9' )
      call printo ( '  Invalid pixels are put as: IIIII' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_VTYPER -- Type out the values on the CL - part 2 short
C
C alan penny                  ral             1990-06-16

      subroutine ins_vtyper ( im, mxs, mxe, mys, mye )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real         im(NX,NY)		!i: Image
      integer      mxs			!i: Start of X area
      integer      mxe			!i: End of X area
      integer      mys			!i: Start of Y area
      integer      mye			!i: End of Y area
C--
      integer j, k, ka, kl, iv
      real rv, rva, rvb
      character*72 text, ostr
Cbegin


      if ( ST_FAILED ) return

      do k = mye, mys, -1*NYSTEP	                               	!Do line by line, backwards
         write ( text, '(1x,i4)' ) k					!line number
         kl = 12 - WIDTH
         do j = mxs, mxe, NXSTEP	                              	!In a line, do point by point
            kl = kl + WIDTH
            if ( (kl+WIDTH).lt.72 ) then
               rvb = im(j,k)
               rv = BS*rvb + BZ						!Get value
               rva = (rv-BASE)/SCALE	                              	!Scale
               if ( rvb.eq.RINVAL ) then
                  text(kl:kl) = ' '
                  do ka = kl+1, kl+WIDTH
                     text(ka:ka) = 'I'
                  enddo
               elseif ( rv.gt.TOP ) then
                  text(kl:kl) = ' '
                  do ka = kl+1, kl+WIDTH
                     text(ka:ka) = '*'
                  enddo
               elseif ( rv.lt.BOT ) then
                  do ka = kl, kl+WIDTH
                     text(ka:ka) = ' '
                  enddo
               elseif ( WIDTH.eq.1 .and. DECPL.eq.0 ) then		!If 1 character width, then
                  iv = rva - (int(rva)/10)*10 				!do special outputting of
                  write ( text(kl:kl), '(i1)' ) iv			!last digit before dec point
               else
                  call ins_rtoc ( rva, DECPL, WIDTH, ostr ) 		!Normal, but truncate if
                  text(kl:kl+WIDTH-1) = ostr(1:WIDTH)			!too wide
               endif
            endif
         enddo
         call printo ( text )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_VTYPES -- Type out the values on the CL - part 2 short
C
C alan penny                  ral             1990-06-16

      subroutine ins_vtypes ( im, mxs, mxe, mys, mye )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2    im(NX,NY)		!i: Image
      integer      mxs			!i: Start of X area
      integer      mxe			!i: End of X area
      integer      mys			!i: Start of Y area
      integer      mye			!i: End of Y area
C--
      integer j, k, ka, kl, iv
      real rv, rva
      character*72 text, ostr
Cbegin


      if ( ST_FAILED ) return

      do k = mye, mys, -1*NYSTEP	                               	!Do line by line, backwards
         write ( text, '(1x,i4)' ) k					!line number
         kl = 12 - WIDTH
         do j = mxs, mxe, NXSTEP	                              	!In a line, do point by point
            kl = kl + WIDTH
            if ( (kl+WIDTH).lt.72 ) then
               iv = im(j,k)
               rv = BS*real(iv) + BZ					!Get value
               rva = (rv-BASE)/SCALE	                              	!Scale
               if ( iv.eq.INVAL ) then
                  text(kl:kl) = ' '
                  do ka = kl+1, kl+WIDTH
                     text(ka:ka) = 'I'
                  enddo
               elseif ( rv.gt.TOP ) then
                  text(kl:kl) = ' '
                  do ka = kl+1, kl+WIDTH
                     text(ka:ka) = '*'
                  enddo
               elseif ( rv.lt.BOT ) then
                  do ka = kl, kl+WIDTH
                     text(ka:ka) = ' '
                  enddo
               elseif ( WIDTH.eq.1 .and. DECPL.eq.0 ) then		!If 1 character width, then
                  iv = rva - (int(rva)/10)*10 				!do special outputting of
                  write ( text(kl:kl), '(i1)' ) iv			!last digit before dec point
               else
                  call ins_rtoc ( rva, DECPL, WIDTH, ostr ) 		!Normal, but truncate if
                  text(kl:kl+WIDTH-1) = ostr(1:WIDTH)			!too wide
               endif
            endif
         enddo
         call printo ( text )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_RTOC -- Make real number character string
C
C alan penny                  ral             1990-06-16

      subroutine ins_rtoc ( rv, decpl, width, ostr )

      implicit none

      real	rv		!i: Input number
      integer	decpl		!i: No of decimal places
      integer	width		!i: Total output width
      character*(*) ostr	!o: Character string
C--
      real trunc, rva, dv
      external trunc
      integer k, kl, j, ja, jb, jc
      character*72 text
Cbegin


      ostr(1:width) = ' '

      dv = 0.5							!Round last figure
      k = decpl
      do while ( k.gt.0 )
         dv = dv/10.0
         k = k - 1
      enddo
      rva = rv + dv

      kl = width - decpl - 1
      if ( kl.ge.1 ) rva = trunc(rva,kl)
      write ( text, '(f50.20)' ) rva
      call lbgone ( text )
      j = index(text,'.')
      ja = max(1,(j-kl))
      if ( decpl.eq.0 ) then
         jb = j - 1
      else
         jb = min((ja+width-1),(j+decpl))
      endif
      jc = width - (jb-ja)
      ostr(jc:width) = text(ja:jb)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_PVTYPE -- Type out the values as a 'picture' on the CL
C
C alan penny                  ral             1990-06-16

      subroutine ins_pvtype ( )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

C--
      integer mxs, mxe, mys, mye, kx(2),
     +        ky(2), ierr
      real    am, std, amax, amin
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( (NXS.lt.1 .and. NXE.lt.1) .or. 				!Check area is inside
     +     (NXS.gt.NX .and. NXE.gt.NX) .or.
     +     (NYS.lt.1 .and. NYE.lt.1) .or.
     +     (NYS.gt.NY .and. NYE.gt.NY) ) then
	 call printo ( 'Area is outside image' )
         return
      endif

      mxs = min(NX,max(1,NXS))
      mxe = min(NX,max(1,NXE))
      mys = min(NY,max(1,NYS))
      mye = min(NY,max(1,NYE))

      kx(1) = mxs
      kx(2) = mxe
      ky(1) = mys
      ky(2) = mye
      if ( IMTYPE.eq.'SHORT' ) then
         call ranges ( %val(IPIM), NX, NY, kx, ky, INVAL, am, std, ierr)
      else
         call ranger ( %val(IPIM), NX, NY, kx, ky, RINVAL, am, std,ierr)
      endif
      if ( std.lt.1.0e-8 ) std = 1.0

      if ( IMTYPE.eq.'SHORT' ) then
         call ins_pvtypes ( %val(IPIM), mxs, mxe, mys, mye, am, std )
      else
         call ins_pvtyper ( %val(IPIM), mxs, mxe, mys, mye, am, std )
      endif

      call ins_loline ( mxs, mxe, NXSTEP )                              !X coord line

      amin = BZ + BS*am - BS*2.0*std
      amax = BZ + BS*am + BS*3.0*std
      call pargr ( amin )
      call pargr ( amax )
      call printd ( ' Values range from %f to %f ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_PVTYPER -- Type out the values as a 'picture' on the CL - part 2 short
C
C alan penny                  ral             1990-06-16

      subroutine ins_pvtyper ( im, mxs, mxe, mys, mye, am, std )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real         im(NX,NY)		!i: Image
      integer      mxs			!i: Start of X area
      integer      mxe			!i: End of X area
      integer      mys			!i: Start of Y area
      integer      mye			!i: End of Y area
      real         am			!i: Mean
      real         std			!i: Std dev
C--
      integer j, k, kl, iv
      real rv
      character*72 text
Cbegin


      if ( ST_FAILED ) return

      do k = mye, mys, -1*NYSTEP                              	 	!Do line by line, backwards
         write ( text, '(1x,i4,8x)' ) k                      		!Type line number
         kl = 12
         do j = mxs, mxe, NXSTEP 					!In a line, do point by point
            kl = kl + 1
            if ( kl.le.72 ) then
               rv = im(j,k)						!Get value
               if ( rv.eq.RINVAL ) then
                  write ( text(kl:kl), '(''I'')' )
               else
                  rv = (rv-(am-2.0*std))/(5.0*std)			!Scale value
                  iv = 10.0*rv
                  if ( iv.gt.9 ) then                              	!Print value
                     write ( text(kl:kl), '(''*'')' )
                  elseif (iv.lt.0) then
                     write ( text(kl:kl), '('' '')' )
                  else
                     write ( text(kl:kl), '(i1)' ) iv
                  endif
              endif
            endif
         enddo
         call printo ( text )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_PVTYPES -- Type out the values as a 'picture' on the CL - part 2 short
C
C alan penny                  ral             1990-06-16

      subroutine ins_pvtypes ( im, mxs, mxe, mys, mye, am, std )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2    im(NX,NY)		!i: Image
      integer      mxs			!i: Start of X area
      integer      mxe			!i: End of X area
      integer      mys			!i: Start of Y area
      integer      mye			!i: End of Y area
      real         am			!i: Mean
      real         std			!i: Std dev
C--
      integer j, k, kl, iv
      real rv
      character*72 text
Cbegin


      if ( ST_FAILED ) return

      do k = mye, mys, -1*NYSTEP                              	 	!Do line by line, backwards
         write ( text, '(1x,i4,8x)' ) k                      		!Type line number
         kl = 12
         do j = mxs, mxe, NXSTEP 					!In a line, do point by point
            kl = kl + 1
            if ( kl.le.72 ) then
               if ( im(j,k).eq.INVAL ) then
                  write ( text(kl:kl), '(''I'')' )
               else
                  rv = im(j,k)						!Get value
                  rv = (rv-(am-2.0*std))/(5.0*std)			!Scale value
                  iv = 10.0*rv
                  if ( iv.gt.9 ) then                              	!Print value
                     write ( text(kl:kl), '(''*'')' )
                  elseif (iv.lt.0) then
                     write ( text(kl:kl), '('' '')' )
                  else
                     write ( text(kl:kl), '(i1)' ) iv
                  endif
               endif
            endif
         enddo
         call printo ( text )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_LOLINE -- Print out single spaced x-coords posn line
C
C  alan penny                ral                      1990-06-15

      subroutine ins_loline ( mxs, mxe, mxstep )

      implicit none

      integer	mxs		!i: X start
      integer	mxe		!i: X end
      integer	mxstep		!i: X step
C--
      integer j, iv, ivals(1000), num, numa, kl, jj, jl, nout
      character*132 text
Cbegin


      text = ' '							!Lowest characters of numbers
      kl = 8
      do j = mxs, mxe, mxstep
         iv = j - (j/10)*10
         kl = kl + 1
         if ( kl.le.132 ) write ( text(kl:kl), '(i1)' ) iv
      enddo
      kl = min(132,kl)
      call printo ( text(1:kl) )

      text = ' '
      kl = 8
      call ins_chint ( mxe, numa, ivals )
      nout = ((mxe-mxs+1)/mxstep)/(numa+1)
      do jj = 1, nout
         jl = mxs + (jj-1)*mxstep*(numa+1)
         call ins_chint ( jl, num, ivals )
         kl = 8 + (jj-1)*(numa+1)
         do j = 1, num
            kl = kl + 1
            if ( kl.le.132 ) write ( text(kl:kl), '(i1)' ) ivals(j)
         enddo
      enddo
      kl = min(132,kl)
      call printo ( text(1:kl) )

      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_CHINT -- Convert an integer into a series of integers
C
C  alan penny                ral                      1990-06-15

      subroutine ins_chint ( iv, num, ival )

      implicit none

      integer 	iv		!i: Input number
      integer 	num		!o: No of characters in it
      integer 	ival(1)		!o: Array of single digit integers, making input
C--
      integer j, k
Cbegin

      if ( iv.eq.0 ) then
         num = 1
         ival(1) = 0
         return
      endif

      k = iabs(iv)
      num = 0
      do while ( k.gt.0 )
          k = k/10
          num = num + 1
      enddo

      k = iabs(iv)
      do j = 1, num
         ival(num-j+1) = k - 10*(k/10)
         k = k/10
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GSOLID -- Put out a "3-D" graph of the values on the graph window
C
C  alan penny                ral                      1990-06-15

      subroutine ins_gsolid ( im, rim, kopt )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2	im(NX,NY)	!i: Image (SHORT)
      real      rim(NX,NY)	!i: Image (REAL)
      integer	kopt		!i: Flag (1=transparent;2=hiden lines)
C--
      integer j, k, l, iv, mxs, mxe, mys, mye, kstart, kx1, kx2, kc,
     +        kco(12), kdo(12), lxs, lxe, lxst, lys, lye, lyst,
     +        keo(12), ngood, nbad, ja, ka, jno, kno, lx, ly, kp(4),
     +        istat
      logical start, last, over
      real rv, rvm, rvmax, rvmin, rvr(1000), rvd(1000),
     +     x1, y1, x2, y2, xscale, zscale, xoff, yoff, ysize, akx,
     +     xfoff, xsoff, xdoff, dk
      data kco / 1, 1,  2, 2, 2,  3, 3, 3,  4, 4, 4,  1 /
      data kdo / 0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1, -1 /
      data keo / 0, 0,  1, 0, 0,  1, 0, 0,  1, 0, 0,  1 /
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( (NXS.lt.1 .and. NXE.lt.1) .or. 				!Check area is inside
     +     (NXS.gt.NX .and. NXE.gt.NX) .or.
     +     (NYS.lt.1 .and. NYE.lt.1) .or.
     +     (NYS.gt.NY .and. NYE.gt.NY) ) then
	 call printo ( 'Area is outside image' )
         return
      endif

      if ( NXE.le.(NXS+2) .or. NYE.le.(NYS+2) ) then
         call printo ( 'ERROR: Area too small - cant do plot' )
         return
      endif

      mxs = min(NX,max(1,NXS))						!Make area regular
      mxe = min(NX,max(1,NXE))
      mys = min(NY,max(1,NYS))
      mye = min(NY,max(1,NYE))
      call cswopi ( mxs, mxe )
      call cswopi ( mys, mye )

      lx = (1+mxe-mxs)/NXSTEP						!Check area is large enough
      ly = (1+mye-mys)/NYSTEP
      if ( lx.lt.3 .or. ly.lt.3 ) then
         call printo ( 'Area is too small' )
         return
      endif

      call get1i ( 'ORIENT', ORIENT, ORIENT, 0, 360 )			!get orientation
      if ( ST_FAILED ) return
      ORIENT = mod(30*(nint(real(ORIENT)/30.0)),360)
      if ( ORIENT.eq.360 ) ORIENT = 0
      k = ORIENT/30 + 1
      kc = kco(k)
      xdoff = kdo(k)
      xsoff = keo(k)

      xscale = 1.0
      if ( kc.eq.1 ) then
         xscale = 0.7/real(mxe-mxs)
         if ( kdo(k).eq.0 ) xscale = xscale*0.9/0.7
         ysize = real(mye-mys) + 0.0000001
         lxs = mxs
         lxe = mxe
         lxst = NXSTEP
         lys = mys
         lye = mye
         lyst = NYSTEP
      elseif ( kc.eq.2 ) then
         xscale = 0.7/real(mye-mys)
         if ( kdo(k).eq.0 ) xscale = xscale*0.9/0.7
         ysize = real(mxe-mxs) + 0.0000001
         lxs = mys
         lxe = mye
         lxst = NYSTEP
         lys = mxe
         lye = mxs
         lyst = -1*NXSTEP
      elseif ( kc.eq.3 ) then
         xscale = 0.7/real(mxe-mxs)
         if ( kdo(k).eq.0 ) xscale = xscale*0.9/0.7
         ysize = real(mye-mys) + 0.0000001
         lxs = mxe
         lxe = mxs
         lxst = -1*NXSTEP
         lys = mye
         lye = mys
         lyst = -1*NYSTEP
      elseif ( kc.eq.4 ) then
         xscale = 0.7/real(mye-mys)
         if ( kdo(k).eq.0 ) xscale = xscale*0.9/0.7
         ysize = real(mxe-mxs) + 0.0000001
         lxs = mye
         lxe = mys
         lxst = -1*NYSTEP
         lys = mxs
         lye = mxe
         lyst = NXSTEP
      endif

      jno = 1 + ((lxe-lxs)/lxst)
      kno = 1 + ((lye-lys)/lyst)

      call st_minmax ( %val(IPIM), NX, NY, BS, BZ, IMTYPE, INVAL, 	!Z scale
     +                  RINVAL, mxs, mxe, mys, mye, rvmin, rvmax,
     +                  kp, ngood, nbad )
      call get2r ( 'VPRANGE', rvmin, rvmax, .true., -1.0e20, 1.0e20 )	!Min and max allowed
      if ( ST_FAILED ) return

      if ( rvmax.eq.rvmin ) then
         zscale = 1.0
      else
         zscale = 0.7/abs(rvmax-rvmin)
      endif

      call gd_open ( istat )						!Open display
      if ( istat.ne.0 ) return
      if ( ST_FAILED ) return

      call pgenv ( 0.0, 1.0, 0.0, 1.0, 0, -2 )

      if ( kdo(k).eq.0 ) then						!Draw axes
         call pgmove ( 0.0, 0.0 )
         call pgdraw ( 0.0, 1.0 )
         call pgmove ( 0.0, 0.0 )
         call pgdraw ( 1.0, 0.0 )
         call pgdraw ( 1.0, 0.26 )
      elseif ( kdo(k).eq.1 ) then
         call pgmove ( 0.0, 0.0 )
         call pgdraw ( 0.0, 1.0 )
         call pgmove ( 0.0, 0.0 )
         call pgdraw ( 0.75, 0.0 )
         call pgdraw ( 1.0, 0.26 )
      else
         call pgmove ( 1.0, 0.0 )
         call pgdraw ( 1.0, 1.0 )
         call pgmove ( 1.0, 0.0 )
         call pgdraw ( 0.22, 0.0 )
         call pgdraw ( 0.0, 0.23 )
      endif


      xfoff = xsoff*0.1*abs(lye-lys)/ysize + 0.01
      if ( kdo(k).eq.0 )  xfoff = xfoff + 0.02
      if ( kdo(k).eq.-1 ) xfoff = xfoff + 0.13
      if ( kopt.eq.1 ) then
         call azeror ( rvr, 1000 )
         do k = 1, kno

            dk = (real(k)-1.0)/real(kno)
            ka = lys + (k-1)*lyst
            xoff = xfoff + xdoff*0.2*dk
            yoff = 0.2*dk
            if ( IMTYPE.eq.'SHORT' ) then
               if ( kc.eq.1 .or. kc.eq.3 ) then
                  iv = im(lxs,ka)
               else
                  iv = im(ka,lxs)
               endif
               if ( iv.eq.INVAL ) then
                  rvm = 0.0
               else
                  rvm = BS*real(iv) + BZ
               endif
            else
               if ( kc.eq.1 .or. kc.eq.3 ) then
                  rv = rim(lxs,ka)
               else
                  rv = rim(ka,lxs)
               endif
               if ( rv.eq.RINVAL ) then
                  rvm = 0.0
               else
                  rvm = BS*rv + BZ
               endif
            endif
            rvm = min(1.0,max(0.0,((rvm-rvmin)*zscale)))

            do j = 2, jno
               ja = lxs + (j-1)*lxst
               if ( IMTYPE.eq.'SHORT' ) then
                  if ( kc.eq.1 .or. kc.eq.3 ) then
                     iv = im(ja,ka)
                  else
                     iv = im(ka,ja)
                  endif
                  if ( iv.eq.INVAL ) then
                     rv = 0.0
                  else
                     rv = BS*real(iv) + BZ
                  endif
               else
                  if ( kc.eq.1 .or. kc.eq.3 ) then
                     rv = rim(ja,ka)
                  else
                     rv = rim(ka,ja)
                  endif
                  if ( rv.eq.RINVAL ) then
                     rv = 0.0
                  else
                     rv = BS*rv + BZ
                  endif
               endif
               rv = min(1.0,max(0.0,((rv-rvmin)*zscale)))
               x1 = (real(j)-1.0)*abs(lxst)*xscale + xoff
               x2 = x1 + abs(lxst)*xscale
               kx1 = max(1,min(1000,int(1000.0*x1)))
               kx2 = max(1,min(1000,int(1000.0*x2)))
               y1 = rvm + yoff
               y2 = rv + yoff
               if ( kx2.eq.kx1 ) then
                  akx = 1.0
               else
                  akx = 1.0/abs(kx2-kx1)
               endif
               do l = kx1, kx2
                  rvd(l) = y1 + (l-kx1)*akx*(y2-y1)
               enddo
               l = kx1
               over = .false.
               do while ( .not.over )
                  start = .false.
                  last = .false.
                  l = l - 1
                  do while ( .not.(start.and.last) .and. .not.over)
                     l = l + 1
                     if ( l.eq.kx2 ) over = .true.
                     if ( j.eq.1 .or. rvr(l).le.rvd(l) ) then
                        if ( .not.start ) then
                           start = .true.
                           kstart = l
                        endif
                     else
                       if ( start ) last = .true.
                     endif
                  enddo
                  x1 = real(kstart)/1000.0
                  x2 = real(l)/1000.0
                  y1 = rvd(kstart)
                  y2 = rvd(l)
                  if ( (last.or.over) .and. start ) then
                     call pgmove ( x1, y1 )
                     call pgdraw ( x2, y2 )
                  endif
               enddo
               do l = kx1, kx2
                  if ( rvd(l).gt.rvr(l) ) rvr(l) = rvd(l)
               enddo
               rvm = rv
            enddo

	 enddo
      else
         do k = 1, kno

            dk = (real(k)-1.0)/real(kno)
            ka = lys + (k-1)*lyst
            xoff = xfoff + xdoff*0.2*dk
            yoff = 0.2*dk

            if ( IMTYPE.eq.'SHORT' ) then
               if ( kc.eq.1 .or. kc.eq.3 ) then
                  iv = im(lxs,ka)
               else
                  iv = im(ka,lxs)
               endif
               if ( iv.eq.INVAL ) then
                  rvm = 0.0
               else
                  rvm = BS*real(iv) + BZ
               endif
            else
               if ( kc.eq.1 .or. kc.eq.3 ) then
                  rv = rim(lxs,ka)
               else
                  rv = rim(ka,lxs)
               endif
               if ( rv.eq.RINVAL ) then
                  rvm = 0.0
               else
                  rvm = BS*rv + BZ
               endif
            endif
            rvm = min(1.0,max(0.0,((rvm-rvmin)*zscale)))

            do j = 2, jno
               ja = lxs + (j-1)*lxst
               if ( IMTYPE.eq.'SHORT' ) then
                  if ( kc.eq.1 .or. kc.eq.3 ) then
                     iv = im(ja,ka)
                  else
                     iv = im(ka,ja)
                  endif
                  if ( iv.eq.INVAL ) then
                     rv = 0.0
                  else
                     rv = BS*real(iv) + BZ
                  endif
               else
                  if ( kc.eq.1 .or. kc.eq.3 ) then
                     rv = rim(ja,ka)
                  else
                     rv = rim(ka,ja)
                  endif
                  if ( rv.eq.RINVAL ) then
                     rv = 0.0
                  else
                     rv = BS*rv + BZ
                  endif
               endif
               rv = min(1.0,max(0.0,((rv-rvmin)*zscale)))
               x1 = (real(j)-1.0)*abs(lxst)*xscale + xoff
               x2 = x1 + abs(lxst)*xscale
               y1 = rvm + yoff
               y2 = rv + yoff
               call pgmove ( x1, y1 )
               call pgdraw ( x2, y2 )
               rvm = rv
            enddo

	 enddo
      endif

      call pgupdt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_ASTATS -- Area statistics
C
C  alan penny                ral                      1990-06-15

      subroutine ins_astats ( gotimage )

      implicit none
      include 'inspect.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      logical  gotimage		!i: Got an input image?

C--
      integer kx(2), ky(2), ngood, nbad, ierr, kp(4), ipa, ierra
      real am, std, amax, amin, rm
Cbegin


      if ( ST_FAILED ) return

      if ( .not.gotimage ) then
         call printo ( 'No image got yet' )
         return
      endif

      kx(1) = NXS
      kx(2) = NXE
      ky(1) = NYS
      ky(2) = NYE
      if ( IMTYPE.eq.'SHORT' ) then
         call ranges ( %val(IPIM), NX, NY, kx, ky, INVAL, am, std, ierr)
      else
         call ranger ( %val(IPIM), NX, NY, kx, ky, RINVAL, am, std,ierr)
      endif
      if ( ierr.ne.3 ) then
         am = BS*am + BZ
         std = BS*std
      else
         am = 0.0
         std = 0.0
      endif

      call st_minmax ( %val(IPIM), NX, NY, BS, BZ, IMTYPE, INVAL,
     +         RINVAL, NXS, NXE, NYS, NYE, amin, amax, kp, ngood, nbad )

      if ( IMTYPE.eq.'SHORT' ) then
          call gtwrki ( 'XX_MEDIAN', NX*NY, ipa, ierra )
          call medians ( %val(IPIM), NX, NY, %val(ipa), kx, ky, INVAL,
     +                   rm, ierr )
          call wrkcan ( 'XX_MEDIAN' )
       else
          call gtwrkr ( 'XX_MEDIAN', NX*NY, ipa, ierra )
          call medianr ( %val(IPIM), NX, NY, %val(ipa), kx, ky, RINVAL,
     +                   rm, ierr )
          call wrkcan ( 'XX_MEDIAN' )
      endif
      if ( ierr.ne.3 ) then
         rm = BS*rm + BZ
      else
         rm = 0.0
      endif

      call printo ( ' ' )
      if ( IMTITLE.eq.' ' ) then
         call printo ( '  Image has no title' )
      else
         call pargc ( IMTITLE )
         call printd ( '  Image title is: %c ' )
      endif
      call pargc ( IMTYPE )
      call printd    ( '  Image type is: %c ' )

      call pargi ( NX )
      call pargi ( NY )
      call printd    ( '  X size =  %d  Y size = %d ' )

      call pargr ( BS )
      call pargr ( BZ )
      if ( IMTYPE.eq.'SHORT' ) then
         call pargi ( INVAL )
         call printd ( '  BSCALE = %f  BZERO = %f  INVAL = %d' )
      else
         call pargr ( RINVAL )
         call printd ( '  BSCALE = %f  BZERO = %f  INVAL = %f' )
      endif
      call printo ( ' ' )

      call pargi ( NXS )
      call pargi ( NXE )
      call pargi ( NYS )
      call pargi ( NYE )
      call printd (
     + '  Inspected Area covers: X range %d to %d ; Y range %d to %d' )
      call printo ( '  All pixels in this area analysed' )

      call pargr ( am )
      call pargr ( std )
      call printd ( '  Mean = %f ; Std Dev = %f  ' )
      call pargr ( rm )
      call printd ( '  Median = %f ' )
      call pargr ( amin )
      call pargi ( kp(1) )
      call pargi ( kp(2) )
      call pargr ( amax )
      call pargi ( kp(3) )
      call pargi ( kp(4) )
      call printd ('  Min = %f at ( %d ,%d ) ; Max = %f at ( %d ,%d )' )

      call pargi ( ngood )
      call pargi ( nbad )
      call printd ( '  Number of good pixels = %d ; '
     +              //'of invalid pixels = %d' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_HISTO -- Put out histogram
C
C  alan penny                ral                      1990-06-15

      subroutine ins_histo ( im, rim )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2 im(NX,NY)	!i: Image (SHORT)
      real      rim(NX,NY)	!i: Image (REAL)
C--
      integer j, k, l, numbin, ngood, nbad, iv, kp(4), istat
      real alo, ahi, step, val, x(1000), ydata(1000),
     +     xmin, xmax, ymin, ymax, rv
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( .not.GOTHRANGE ) then
         call st_minmax ( im, NX, NY, BS, BZ, IMTYPE, INVAL, RINVAL,
     +                 NXS, NXE, NYS, NYE, alo, ahi, kp, ngood, nbad )
         step = (ahi-alo)/20.0
         if ( step.eq.0.0 ) step = 1.0
      endif
      GOTHRANGE = .true.
      call get3r ( 'HRANGE', alo, ahi, step, .true., -1.0e20, 1.0e20 )	!Get range
      if ( ST_FAILED ) return
      call cswopr ( alo, ahi )
      step = abs(step)
      if ( step.eq.0.0 ) step = 1.0

      numbin = (abs(ahi-alo)+step)/step
      if ( numbin.lt.1 ) numbin = 1
      if ( numbin.gt.1000 ) then
         numbin = 1000
         step = abs(ahi-alo)/(1000.0-1.0)
      endif

      call azeror ( ydata, numbin )					!Calc histogram
      if ( IMTYPE.eq.'SHORT' ) then
         do k = NYS, NYE, NYSTEP
            do j = NXS, NXE, NXSTEP
               iv = im(j,k)
               if ( iv.ne.INVAL ) then
                  val = BS*real(iv) + BZ
                  l = int((val-alo)/step) + 1
                  if (l.ge.1.and.l.le.numbin) ydata(l) = ydata(l) + 1.0
               endif
            enddo
         enddo
      else
         do k = NYS, NYE, NYSTEP
            do j = NXS, NXE, NXSTEP
               rv = rim(j,k)
               if ( rv.ne.RINVAL ) then
                  val = BS*rv + BZ
                  l = int((val-alo)/step) + 1
                  if (l.ge.1.and.l.le.numbin) ydata(l) = ydata(l) + 1.0
               endif
            enddo
         enddo
      endif

      call ins_loadx ( x, alo, step, numbin )				!Load x values

      call alimr ( ydata, numbin, ymin, ymax )				!Get y display range
      ymax = ymax + 0.1*(ymax-ymin)
      xmin = alo							!Get x range
      xmax = alo + step*real(numbin)

      call gd_open ( istat )						!Display histogram
      if ( istat.ne.0 ) return
      call pgenv ( xmin, xmax, ymin, ymax, 0, 1 )

      call pgbin ( numbin, x, ydata, .true. )
      call pglabel ( 'Values', 'No of Values', IMTITLE )
      call pgupdt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_RADIUS -- Get radius, height of star
C
C  alan penny                ral                      1991 June

      subroutine ins_radius ( )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      character text*79, textc*7
      logical more
      real trunc
      external trunc
      real x, y, dx, dy, rx, ry, anx, any, amag,
     +     rms, heightg, baseg, aamag, arx, ary, rxga, ryga, rv,
     +     xs, xe, ys, ye
      integer kx, ky, kbut, ierr, lxga, lyga, iter, kw, ninval,
     +        kxs, kxe, kys, kye
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      rxga = 3.0
      ryga = 3.0
      lxga = 20
      lyga = 20

      call get2i ( 'IGBOX', lxga, lyga, .true., 5, 100 )
      if ( ST_FAILED ) return

      more = .true.
      do while ( more )

         call ds_gcur ( .true., kx, ky,  kbut, ierr )

         if ( kbut.eq.3 ) then
            more = .false.
         elseif ( kx.ge.1.and.kx.le.NX.and.ky.ge.1.and.ky.le.NY ) then
            kw = 0
            if ( kbut.eq.2 ) then
               call get2r ( 'IGRADII', rxga, ryga, .true., 0.2, 60.0 )
               if ( ST_FAILED ) return
               kw = 1
            endif
            x = kx
            y = ky
            if ( IMTYPE.eq.'SHORT' ) then
               call gauss2sa ( %val(IPIM), NX, NY, x, y, lxga, lyga,
     +                kw, rxga, ryga, INVAL, 20, amag, heightg, baseg,
     +                dx, dy, anx, any, rx, ry, rms, iter, ninval )
            else
               call gauss2ra ( %val(IPIM), NX, NY, x, y, lxga, lyga,
     +                kw, rxga, ryga, RINVAL, 20, amag, heightg, baseg,
     +                dx, dy, anx, any, rx, ry, rms, iter, ninval )
            endif

            if ( amag.gt.49.0 .or. rx.lt.0.1 .or. ry.lt.0.1 .or.
     +           rx.gt.100.0 .or. ry.gt.100.0 ) then
                call printo ( '  ' )
                call printo ( '  No result - Cannot fit a Gaussian' )
                call printo ( '              Star may be too far away' )
            else

               call printo ( ' ' )
               arx = trunc(rx,3)
               ary = trunc(ry,3)
               write ( text, '(1x,'' Gaussian ; X Radius = '',
     +                 f6.2,'' ; Y Radius = '',f6.2)' ) arx,ary
               call printo ( text )
               rv = 1.65*(rx+ry)/2.0
               rv = trunc(rv,3)
               write ( text, '(1x,'' Seeing   = '',f6.2)' ) rv
               call printo ( text )
               x = trunc(anx,4)
               y = trunc(any,4)
               write ( text, '(1x,'' Position = '',f8.2,''  '',
     +              f8.2)' ) x, y
               call printo ( text )
               amag = amag - 2.5*alog10(BS)
               heightg = BS*heightg
               baseg = baseg + BZ
               aamag = trunc(amag,3)
               write ( textc, '(f7.3)' ) aamag
               call pargr ( heightg )
               call pargr ( baseg )
               call printd ( '  Gaussian : Mag = '//textc//
     +                       ' : Height = %f  : Base = %f' )

               kxs = x - real(lxga)/2.0
               kxe = x + real(lxga)/2.0
               kys = y - real(lyga)/2.0
               kye = y + real(lyga)/2.0
               xs = max(1,min(NX,kxs))
               xe = max(1,min(NX,kxe))
               ys = max(1,min(NY,kys))
               ye = max(1,min(NY,kye))
               call ds_box ( xs, xe, ys, ye, 1 )

            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SLICE -- Plot out slice through image
C
C  alan penny                ral                      1990-06-15

      subroutine ins_slice ( )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      real xa, ya, xb, yb, dx, dy, d
      integer kxa, kya, kxb, kyb, kbut, ierr, kd, ipa, ipb, i
      logical ok
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      ok = .false.
      do while ( .not.ok )
         call ds_gcur ( .true., kxa, kya,  kbut, ierr )
         call pargi ( kxa )
         call pargi ( kya )
         call printd ( ' X = %d  Y = %d' )
         ok = .true.
         if ( kxa.lt.1 .or. kxa.gt.NX ) ok = .false.
         if ( kya.lt.1 .or. kya.gt.NY ) ok = .false.
         if ( ok ) then
            xa = kxa
            ya = kya
            call ds_cross ( xa, ya, 2.0, 4 )
         endif
      enddo

      ok = .false.
      do while ( .not.ok )
         call ds_gcur ( .true., kxb, kyb,  kbut, ierr )
         call pargi ( kxb )
         call pargi ( kyb )
         call printd ( ' X = %d  Y = %d' )
         ok = .true.
         if ( kxb.lt.1 .or. kxb.gt.NX ) ok = .false.
         if ( kyb.lt.1 .or. kyb.gt.NY ) ok = .false.
         if ( ok ) then
            xb = kxb
            yb = kyb
            call ds_cross ( xb, yb, 2.0, 4 )
         endif
      enddo

      call ds_line ( xa, ya, xb, yb, 4 )


      dx = kxb - kxa
      dy = kyb - kya
      d = 1.0 + sqrt(max(0.0001,(dx*dx+dy*dy)))

      if ( d.lt.2.0 ) then
         call printo ( 'Cant do just one point' )
      else
         kd = d
         call gtwrkr ( 'SLICEA', 2*kd, ipa, i )
         call gtwrkr ( 'SLICEB', 2*kd, ipb, i )
         if ( IMTYPE.eq.'SHORT' ) then
            call ins_sliceas ( %val(ipa), %val(ipb), kd, %val(IPIM),
     +                         kxa, kya, kxb, kyb )
         else
            call ins_slicear ( %val(ipa), %val(ipb), kd, %val(IPIM),
     +                         kxa, kya, kxb, kyb )
         endif
         call wrkcan ( 'SLICEA' )
         call wrkcan ( 'SLICEB' )
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SLICEAR -- Plot out slice through real image (part 2)
C
C  alan penny                ral                      1990-06-15

      subroutine ins_slicear ( x, y, kd, im, kxa, kya, kxb, kyb )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	kd		!i: Length of slice
      real	x(2*kd)		!o: 'Length' work space
      real	y(2*kd)		!o: 'Height' work space
      real      im(NX,NY)	!i: Image
      integer	kxa		!i: Slice X start
      integer	kya		!i: Slice Y start
      integer	kxb		!i: Slice X end
      integer	kyb		!i: Slice Y end
C--
      real dx, dy, xmin, xmax, ymin, ymax, ymina, ymaxa,
     +     dxa, dya, d, xv, yv, rv
      integer j, k, jj, kk, istat
Cbegin


      if ( ST_FAILED ) return

      dx = kxb - kxa
      dy = kyb - kya
      d = sqrt(dx*dx+dy*dy)
      if ( abs(dx).lt.1.0e-5 ) then
         dxa = 0.0
         dya = sign(1.0,dy)
      elseif( abs(dy).lt.1.0e-5 ) then
         dxa = sign(1.0,dx)
         dya = 0.0
      else
         dxa = dx/d
         dya = dy/d
      endif

      call azeror ( y, kd )
      jj = 0
      do kk = 1, kd
         xv = kk
         j = kxa + dxa*(real(kk-1))
         k = kya + dya*(real(kk-1))
         rv = im(j,k)
         yv = 0.0
         if ( rv.ne.RINVAL ) yv = BS*rv + BZ
         jj = jj + 1
         x(jj) = xv - 0.5
         y(jj) = yv
         jj = jj + 1
         x(jj) = xv + 0.5
         y(jj) = yv
      enddo

      call alimr ( y, 2*kd, ymina, ymaxa )				!Get y display range
      ymin = ymina - 0.1*(ymaxa-ymina)
      ymax = ymaxa + 0.1*(ymaxa-ymina)
      call get2r ( 'VPRANGE', ymin, ymax, .true., -1.0e20, 1.0e20 )	!Min and max allowed
      if ( ST_FAILED ) return
      xmin = 0.5							!Get x range
      xmax = kd + 0.5

      call gd_open ( istat )						!Display slice
      if ( istat.ne.0 ) return
      call pgenv ( xmin, xmax, ymin, ymax, 0, 1 )

      call pgline ( 2*kd, x, y )
      call pglabel ( 'Distance', 'Value', IMTITLE )
      call pgupdt


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_SLICEAS -- Plot out slice through int*2 image (part 2)
C
C  alan penny                ral                      1990-06-15

      subroutine ins_sliceas ( x, y, kd, im, kxa, kya, kxb, kyb )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	kd		!i: Length of slice
      real	x(2*kd)		!o: 'Length' work space
      real	y(2*kd)		!o: 'Height' work space
      integer*2 im(NX,NY)	!i: Image
      integer	kxa		!i: Slice X start
      integer	kya		!i: Slice Y start
      integer	kxb		!i: Slice X end
      integer	kyb		!i: Slice Y end
C--
      real dx, dy, xmin, xmax, ymin, ymax, ymina, ymaxa,
     +     dxa, dya, d, xv, yv
      integer kk, j, k, iv, jj, istat
Cbegin


      if ( ST_FAILED ) return

      dx = kxb - kxa
      dy = kyb - kya
      d = sqrt(dx*dx+dy*dy)
      if ( abs(dx).lt.1.0e-5 ) then
         dxa = 0.0
         dya = sign(1.0,dy)
      elseif( abs(dy).lt.1.0e-5 ) then
         dxa = sign(1.0,dx)
         dya = 0.0
      else
         dxa = dx/d
         dya = dy/d
      endif

      call azeror ( y, kd )
      jj = 0
      do kk = 1, kd
         xv = kk
         j = kxa + dxa*(real(kk-1))
         k = kya + dya*(real(kk-1))
         iv = im(j,k)
         yv = 0.0
         if ( iv.ne.INVAL ) yv = BS*real(iv) + BZ
         jj = jj + 1
         x(jj) = xv - 0.5
         y(jj) = yv
         jj = jj + 1
         x(jj) = xv + 0.5
         y(jj) = yv
      enddo

      call alimr ( y, 2*kd, ymina, ymaxa )				!Get y display range
      ymin = ymina - 0.1*(ymaxa-ymina)
      ymax = ymaxa + 0.1*(ymaxa-ymina)
      call get2r ( 'VPRANGE', ymin, ymax, .true., -1.0e20, 1.0e20 )	!Min and max allowed
      if ( ST_FAILED ) return
      xmin = 0.5							!Get x range
      xmax = kd + 0.5

      call gd_open ( istat )						!Display slice
      if ( istat.ne.0 ) return
      call pgenv ( xmin, xmax, ymin, ymax, 0, 1 )

      call pgline ( 2*kd, x, y )
      call pglabel ( 'Distance', 'Value', IMTITLE )
      call pgupdt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GCONTOUR -- Put out contour plot to device
C
C  alan penny                ral                      1990-06-15

      subroutine ins_gcontour ( )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer lx, ly, ierr, ipconr, ipcons, j, ja, ngood, nbad, ncon,
     +        kl, lens, kp(4), istat
      real alx, aly, c(200), tr(6), alo, ahi, astep, x, y, size
      character*200 text
      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( NXS.eq.NXE .or. NYS.eq.NYE ) then
         call printo ( 'ERROR: Each side must be more than 1 pixel' )
         return
      endif

      lx = NXE - NXS + 1						!Get area into work space
      ly = NYE - NYS + 1
      call gtwrkr ( 'CONTR', lx*ly, ipconr, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'CONTS', lx*ly, ipcons, ierr )
      if ( ierr.ne.0 ) return
      if ( IMTYPE.eq.'SHORT' ) then
         call copss ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call copssr ( %val(ipcons), lx, ly, BS, BZ, INVAL,
     +                 %val(ipconr) )
      else
         call coprr ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call coprrr ( %val(ipcons), lx, ly, BS, BZ, RINVAL,
     +                 %val(ipconr) )
      endif

      call st_minmax ( %val(ipcons), lx, ly, BS, BZ, IMTYPE, INVAL, 	!Get contour values
     +             RINVAL, 1, lx, 1, ly, alo, ahi, kp, ngood, nbad )
      astep = (ahi-alo)/10.0
      if ( astep.eq.0.0 ) astep = 1.0
      call get3r ( 'CONTOUR', alo, ahi, astep, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      ncon = 1 + int((ahi-alo)/astep)
      ncon = min(200,ncon)
      do j = 1, ncon
         c(j) = alo + astep*(real(j)-1.0)
      enddo

      call gd_open ( istat )						!Open display
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) return
      alx = lx
      aly = ly
      size = max(alx,aly)
      call pgenv ( 1.0, alx, 1.0, aly, 1, -2 )

CX      ax = 0.8
CX      ay = 0.8
CX      if ( alx.gt.aly ) then
CX         ax = 0.8
CX         ay = 0.1 + 0.7*aly/alx
CX      elseif ( alx.lt.aly ) then
CX         ax = 0.1 + 0.7*alx/aly
CX         ay = 0.8
CX      endif
CX      call pgvport ( 0.1, ax, 0.1, ay )

      call pgbox ( 'ABCNST', 0.0, 0, 'ABCNST', 0.0, 0 )			!Plot contour
      call pglabel ( 'X', 'Y', ' ' )
      call pgcons ( %val(ipconr), lx, ly, 1, lx, 1, ly, c, ncon, tr )

      write ( text, '('' Origin (1,1) is at image ('',i6,'','',i6,      !Put caption
     +               '')'' )' ) NXS, NYS
      j = index(text,'e (')
      call lbgone(text(j+3:))
      ja = index(text(j:),',')
      call lbgone(text(j+ja:))
      kl = lens(text)
      x = 1.0
      y = aly + (size-1.0)*0.05
      call pgtext ( x, y, text(1:kl) )

      write ( text, '('' Contour levels start at '',f16.3,'' and '',
     +               ''step by '',f16.3)' ) alo, astep
      j = index(text,' at ')
      call lbgone(text(j+4:))
      j = index(text,' by ')
      call lbgone(text(j+4:))
      kl = lens(text)
      x = 1.0
      y = aly + (size-1.0)*0.10
      call pgtext ( x, y, text(1:kl) )

      text = ' Image title is: '//IMTITLE
      kl = lens(text)
      x = 1.0
      y = aly + (size-1.0)*0.15
      call pgtext ( x, y, text(1:kl) )

      call pgupdt

      call wrkcan ( 'CONTR' )						!Close work areas
      call wrkcan ( 'CONTS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_LOADX -- Load X values for histogram
C
C  alan penny                ral                      1990-06-15

      subroutine ins_loadx ( x, alo, step, n )

      implicit none

      integer	n		!i: no of points
      real	x(n)		!o: X values
      real	alo		!i: Start X value
      real	step 		!i: X Step value
C--
      integer k
      real v
Cbegin


      v = alo
      do k = 1, n
         x(k) = v
         v = v + step
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_GDISPLAY --  PGPLOT picture of image area to device (hardcopy maybe)
C
C  alan penny                ral                      1990-06-15

      subroutine ins_gdisplay ( )

      implicit none
      include 'inspect.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'
C--
      integer lx, ly, ipconr, ipcons, ierr, istat
      real alx, aly, tr(6), abot, atop
      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( NXS.eq.NXE .or. NYS.eq.NYE ) then
         call printo ( 'ERROR: Each side must be more than 1 pixel' )
         return
      endif

      lx = NXE - NXS + 1						!Get area into work space
      ly = NYE - NYS + 1
      call gtwrkr ( 'CONTR', lx*ly, ipconr, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'CONTS', lx*ly, ipcons, ierr )
      if ( ierr.ne.0 ) then
         call canpar ( 'CONTR' )
         return
      endif
      if ( IMTYPE.eq.'SHORT' ) then
         call copss ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call copssr ( %val(ipcons), lx, ly, BS, BZ, INVAL,
     +                 %val(ipconr) )
      else
         call coprr ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call coprrr ( %val(ipcons), lx, ly, BS, BZ, RINVAL,
     +                 %val(ipconr) )
      endif


      call gd_open ( istat )						!Open display
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         call wrkcan ( 'CONTR' )
         call wrkcan ( 'CONTS' )
         return
      endif
      alx = lx
      aly = ly
      if ( alx.le.1.0 ) alx = 2.0
      if ( aly.le.1.0 ) aly = 2.0
      call pgenv ( 1.0, alx, 1.0, aly, 1, -2 )

CX      if ( alx.eq.aly ) then
CX         ax = 0.8
CX         ay = 0.8
CX      elseif ( alx.gt.aly ) then
CX         ax = 0.8
CX         ay = 0.1 + 0.7*aly/alx
CX      else
CX         ax = 0.1 + 0.7*alx/aly
CX         ay = 0.8
CX      endif
CX      call get2r ( 'XYPPLOT', ax, ay, .true., 0.101, 0.9 )
CX      call pgvport ( 0.1, ax, 0.1, ay )

      abot = 0.0
      atop = 1.0
      if ( DSKVRANGE.ne.0 ) then
         abot = BS*DSVMIN + BZ
         atop = BS*DSVMAX + BZ
      endif
      call get2r ( 'VPRANGE', abot, atop, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return

      call pggray ( %val(ipconr), lx, ly, 1, lx, 1, ly, atop, abot, tr)	!Plot picture

      call pgupdt

      call wrkcan ( 'CONTR' )						!Close work areas
      call wrkcan ( 'CONTS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INS_OPDISP -- Open display
C
C alan penny                    ral                  1990-06-16

      subroutine ins_opdisp ( ierr )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( ST_FAILED ) return

      if ( OPDISP ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return

      call ds_init ( IMTITLE, 0, ierr )					!Open display
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return
      OPDISP = .true.

      call ds_gtcomf ( 1 )						!Get image display size compression


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INTERACT -- (Program) Display image: plot + get positions: aperture photometry
C
C    alan penny             ral               1990 Jan

      subroutine interact ( ierradam )

      implicit none

      integer    ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_interact

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POSNMAG.FOR    Programs for dealing with positions and Gauss Mags
C
C   Contains:-
C
C T_POSNMAG    Get/plot positions and magnitudes
C PO_OPTION_SETUP  Set up options choices
C PO_SDEF      Set defaults on posnmag factors
C PO_CROSS     Get the cross colours/size
C PO_GETPOS    Get the cursor positions
C PO_GETMAG    Get the cursor magnitudes
C PO_GAUSS     Fit Gaussian at position
C PO_STORP     Store cursor positions in file
C PO_STORM     Store cursor magnitudes in file
C PO_LIST      Type out posns or Gauss posn and mags
C PO_PLOTFILE  Put file positions on screen
C PO_BRIEF     Tell user present setups
C PO_MAINGET   Get main prog params to 'position' ones
C PO_SRMAINGET Get main prog params to temp params
C PO_MAINPUT   Put 'position' params to main prog ones
C PO_SRAPGET   Get 'positions' s/r params to temp ones



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_POSNMAG -- Get/plot positions and magnitudes
C
C  alan penny             ral             1990 Jan

      subroutine t_posnmag ( kopt )

      implicit none

      include 'gposns.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      integer     kopt		!i: 1=default to posns; 2= default to magns
C--
      logical loop
      integer ierr, kx, ky, kb
      character*12 ktopt
Cbegin


      if ( ST_FAILED ) return

      if ( .not.DISPLAYED ) then					!Check image to find
         call printo ( 'No image displayed' )
         return
      endif

      GOTPOSNFILE = .false.

      call po_mainget							!Housekeeping

      call po_brief							!Tell user setup

      call po_option_setup ( ktopt, 2, .true. )
      if ( ST_FAILED ) return
      CH_DEF_TEXT(2,1) = 'cur_posn'					!Default Option
      if ( kopt.eq.1 ) CH_DEF_TEXT(2,1) = 'cur_posn'
      if ( kopt.eq.2 ) CH_DEF_TEXT(2,1) = 'cur_magn'
      loop = .true.
      do while ( loop )

         call po_option_setup ( ktopt, 2, .false. )
         call get_choice ( ktopt, 1 ) 					!Get choice

         if ( ktopt.eq.'cur_magn' .or. ktopt.eq.'cur_posn' .or.         !Check display open for
     +        ktopt.eq.'plot_file' .or.		                	! options that need it
     +        ktopt.eq.'zoom'     .or. ktopt.eq.'reset' .or.
     +        ktopt.eq.'flash'    .or. ktopt.eq.'im_get_flash' .or.
     +        ktopt.eq.'display'  .or. ktopt.eq.'clear' ) then
            call in_opdisp ( ierr )
         endif

         if ( .not.GOTIMAGE .and. (ktopt.eq.'cur_magn' .or.		!Check got image
     +        ktopt.eq.'cur_posn' .or. ktopt.eq.'plot_file' .or.
     +        ktopt.eq.'flash' .or. ktopt.eq.'display')  ) then
            call printo ( 'No image got yet' )
            ktopt = ' '
         endif


         if ( ktopt.eq.'clear' ) then		                      	!Erase display
                                 call ds_erase
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'close' ) then                                  !Close display screen
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )                !Open display screen

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb, 	!Get cursor image values
     +                                             ierr )

         if ( ktopt.eq.'display' ) then					!Display
                       call ds_dodisp ( %val(IPIM), NX, NY, IMTYPE,
     +                                  DSKVRANGE, IMTITLE )
                       DISPLAYED = .true.
                       endif

         if ( ktopt.eq.'flash' ) then					!Flash image
                                 call ds_doflash ( %val(IPIM), NX, NY,
     +                                   IMTYPE, DSKVRANGE, IMTITLE )
                                 DISPLAYED = .true.
                                 endif

         if ( ktopt.eq.'image' ) call in_newim ( ierr )			!Get new image

         if ( ktopt.eq.'im_get_flash' ) then				!Input new image and display
                                       call in_newim ( ierr )
                                       if ( .not.ST_FAILED .and.
     +                                    ierr.eq.0 ) call
     +                                    ds_doflash ( %val(IPIM),
     +                                    NX, NY, IMTYPE, DSKVRANGE,
     +                                    IMTITLE )
                                          DISPLAYED = .true.
                                       endif

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Set zoom, pan to null

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom, pan display

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice

         if ( ktopt.eq.'boxshow' ) call get1b ( 'BOXSHOW', BSHOW, 	!Paint up box round mag posn fit
     +                                          BSHOW )

         if ( ktopt.eq.'flush_magn' ) NUMGM = 0				!Cancel magnitudes

         if ( ktopt.eq.'flush_posn' ) NUMGP = 0				!Cancel positions

         if ( ktopt.eq.'cur_magn' ) call po_getmag			!Get magns

         if ( ktopt.eq.'cur_posn' ) call po_getpos			!Get posns

         if ( ktopt.eq.'list_posn' ) call po_list ( 1 )			!List posns

         if ( ktopt.eq.'list_magn' ) call po_list ( 2 )			!List magns

         if ( ktopt.eq.'plot_file' ) call po_plotfile			!Plot posns/magns input file

         if ( ktopt.eq.'fit_params' ) then				!Change fit parameters
                           call get1b ( 'RADFIX', RADGFIX, RADGFIX )
                           call get2r ( 'RADSIZE', RXGF, RYGF, .true.,
     +                                  0.2, 100.0 )
                           call get2i ( 'BOXSIZE', NXGB, NYGB, .true.,	!Gauss box size
     +                                  3, 200 )
                           endif

         if ( ktopt.eq.'store_magn' ) call po_storm			!Store magns

         if ( ktopt.eq.'store_posn' ) call po_storp			!Store posns

         if ( ktopt.eq.'cross' )   call po_cross			!Cross size

         if ( ktopt.eq.'review' ) call po_brief				!Type out setup

         if ( ktopt.eq.'main' .or. ktopt.eq.'alter' .or. 		!Return to main program
     +        ktopt.eq.'aperture' .or. ktopt.eq.'colour' .or.
     +        ktopt.eq.'inspect' .or.
     +        ktopt.eq.'scrutiny' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo

      if ( GOTPOSNFILE ) then
         call printo ( 'Closed input position file' )
         call canpar ( 'INTAB' )
         GOTPOSNFILE = .false.
      endif

      call po_mainput							!Housekeeping


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_OPTION_SETUP -- Set up options choices
C
C   alan penny                        ral              1990-01-31

      subroutine po_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'

      character*12   ktopt		!i: Chosen option
      integer        set_num		!i: Code for set of options
      logical        koutside		!i: Is this called from outside loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=31 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'im_get_flash', 'Input new image and display it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, without displaying an image.',
     + 'The screen may be any size you want, through keyboard entry',
     + 'via the SCREEN parameter.',
     + ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'boxshow', 'Toggle showing and not the box round mag positions',
     + 'Toggle between showing and not showing the box round the',
     + 'fitted magnitude positions. ',
     + ' ', ' ', ' ', ' ' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'cross', 'Change colour/size of plot/get cross' ,
     + 'This changes the size and colour of the cross painted up, ',
     + 'either when the cursor marks a position or magnitude, or a',
     + ' table of positions from a file is plotted up. The colour ',
     + 'choice is -red:green:blue:cyan:magenta:yellow:coral:palegreen-',
     + 'Different colours for the different modes are allowed.' ,
     + 'The same cross size is used for all modes.' /

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'review', 'Type out the present setups' ,
     + 'Types out the -present- state of this -Posns/Magns- option. ',
     + 'These are:- the box size; the characteristics of the Gaussian',
     + 'fit for the magnitude getting; the number of posns and magns',
     + 'got; the display of the cross.',
     + ' ', ' ' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'flush_magn', 'Forget all star Gauss positions + magnitudes' ,
     + 'Forget all Gaussian positions + magnitudes. (NOT the simple' ,
     + 'positions.) ',
     + ' ', ' ', ' ', ' ' /

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'flush_posn', 'Forget all positions' ,
     + 'Forget all positions. (NOT the Gaussian ones.)',
     + ' ', ' ', ' ', ' ', ' ' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'cur_magn', 'Use cursor to get Gauss positions + magnitudes' ,
     + '| Buttons: Left = Enter posn/Centre = Enter Zoom/Right = End |',
     + 'A position selected, a 2-D Gaussian is fitted to the data in a',
     + 'box round it, expecting a single star inside. If a fit is made',
     + 'the box is recentered, and a new fit made. The pos and box are',
     + 'painted up, the results typed and remembered. The fit can have',
     + 'fixed or variable radii. Mag = 30- 2.5*log10(Height.RadX.RadY)'/

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'cur_posn', 'Use cursor to get positions',
     + '| Buttons: Left = Enter posn/Centre = Enter Zoom/Right = End |',
     + 'A position selected, the position and image pixel value is',
     + 'typed out and remembered by the program.',
     + ' ',
     + 'These positions are kept entirely separate from the list',
     + 'of Gaussian positions and magnitudes' /

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'list_posn', 'Type out positions and pixel values',
     + 'Type out the positions and pixel values marked so far.',
     + 'This gives the X and Y positions of the places marked and the',
     + 'value of the image pixel at that position',
     + ' ', ' ', ' ' /

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'plot_file', 'Get positions from a file and plot them' ,
     + 'Input a table of positions from a file. This file must be in',
     + 'the standard -Starman- table format, and the XY positions must',
     + 'be in the first two columns of the table. The postions are  ',
     + 'plotted out on the image. The file is then closed and the ',
     + 'positions forgotten. ',
     + ' ' /

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'fit_params', 'Set:- radii; fix/vary radii in fit; box size' ,
     + 'To get a stars position and profile magnitude by fitting a 2-D',
     + 'Gaussian, the program selects a rectangle round the star and',
     + 'fits the profile. The profile has fixed or variable X and Y',
     + 'radii, and the size of the box can be changed. The size should',
     + 'be about 3 times the seeing. Fixed radii are more accurate.',
     + 'The box used is painted on the display, with the fitted posn.' /

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'store_magn', 'Put acquired Gauss magnitudes into a file',
     + 'Put the Gauss magnitudes acquired so far into a file. The full',
     + 'details of the stars measured using the Gaussians are put into',
     + 'a file in the standard -Starman- format, together with ',
     + 'ancillary information. The magnitudes are NOT then forgotten.',
     + ' ', ' ' /

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'store_posn', 'Put acquired positions into a file ',
     + 'Put the positions and pixel values acquired so far into a ',
     + 'file. The file has the standard -Starman- format, together ',
     + 'with ancillary information. The magnitudes are NOT then ',
     + 'forgotten.',
     + ' ', ' ' /

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'list_magn', 'Type out Gauss positions and magnitudes' ,
     + 'Type out the Gaussian measured stars done so far.',
     + 'It shows:- X Y position fitted; X Yshift from the cursor posn;',
     + 'magnitude; Gauss height; Gauss base; star max pixel; no of',
     + 'invalid pixels in box; -rms- of fit; fitted Gaussian radii.',
     + 'Gauss = base+height.exp(-((x-xo)/rx)**2).exp(-((y-yo)/ry)**2)',
     + 'Mag = 30 - 2.5*log10(Height.RadX.RadY)'/

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from program. Do not access main option list first.',
     + ' ', ' ', ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /


      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact_Posns/Mags', 'GOPTION', 2 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', ' ' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / 'POSITIONS', 'GAUSSIANS', 'SETUPS',
     +                 'IMAGE', 'DISPLAY', 'FUNCTIONS', 'CONTROL' /

      character*200 sect_text(sect_num)
      data sect_text(1) / 'cur_posn:store_posn:flush_posn:list_posn:
     +                     plot_file' /
      data sect_text(2) / 'cur_magn:store_magn:fit_params:boxshow:
     +                     flush_magn:list_magn' /
      data sect_text(3) / 'cross:review' /
      data sect_text(4) / 'image:im_get_flash' /
      data sect_text(5) / 'clear:close:cvalues:display:flash:
     +                     open:reset:zoom' /
      data sect_text(6) / 'alter:aperture:colour:
     +                     inspect:main:scrutiny' /
      data sect_text(7) / 'panel:exit' /

      integer help_num
      parameter ( help_num=25 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '                 ' ,
     + 'For workstation use, the Cursor must be in image window area' ,
     + 'for the cursor buttons to work when in -WORKING- mode. The ' ,
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' '   ,
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' ,
     + '     Centre Button twice            = zoom x2' /
      data (help_text(k),k=11,20) /
     + '     Left Button then Centre Button = pan' ,
     + 'Zoom means zoom around present position of cursor ' ,
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ' ,
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' ,
     + '   ' /
      data (help_text(k),k=21,help_num) /
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' ,
     + 'X Windows        YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button exits ' /
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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_SDEF -- Set defaults on posnmag factors
C
C    a j penny                    ral         1990 jan

      subroutine po_sdef ()

      implicit none
      include 'gposns.inc'
C--
Cbegin


      CRSIZG  = 5.0				!Position marking cross size
      KCOLGG  = 3				!Colour of getting posn cross
      KCOLGM  = 1				!Colour of getting magn posn cross
      KCOLGP  = 2				!Colour of plotting cross

      NUMGP   = 0				!No of posns got
      NUMGM   = 0				!Number of mags got

      NXGB    = 10				!X size of magnitude box
      NYGB    = 10				!Y size of magnitude box
      BSHOW   = .true.				!Box showing control
      RXGF    = 2.0				!X size of fixed magnitude radius
      RYGF    = 2.0				!Y size of fixed magnitude radius
      RADGFIX = .false.				!Use fixed radius


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_CROSS -- Get the cross colours/size
C
C  alan penny             ral             1990 Jan

      subroutine po_cross ( )

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'
C--
      integer k, kin

      character*1000 topt
      data topt /
     +     'red:green:blue:cyan:magenta:yellow:coral:palegreen' /
      integer nthelp
      parameter ( nthelp=2 )
      character*68 thelp(nthelp)
      data thelp /
     +  'Colour of the crosses to display.' ,
     +'Choice is:-red:green:blue:cyan:magenta:yellow:coral:palegreen' /
Cbegin


      if ( ST_FAILED ) return

      call printo ( 'Cross size?' )
      call get1r ( 'CROSSL', CRSIZG, CRSIZG, 1.0, 1.0e8 )
      if ( ST_FAILED ) return

      call printo ( 'Getting posn cross colour?' )
      kin = KCOLGG
      call get_job ( 'CRCOL', topt, k, kin, thelp, nthelp )
      KCOLGG = k

      call printo ( 'Getting magn cross and box colour?' )
      kin = KCOLGM
      call get_job ( 'CRCOL', topt, k, kin, thelp, nthelp )
      KCOLGM = k

      if ( ST_FAILED ) return
      call printo ( 'Plotting cross colour?' )
      kin = KCOLGP
      call get_job ( 'CRCOL', topt, k, kin, thelp, nthelp )
      KCOLGP = k


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_GETPOS -- Get the cursor positions
C
C  alan penny             ral             1990 Jan

      subroutine po_getpos ( )

      implicit none
      include 'gposns.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
C--
      character text*70
      real      x, y, vl, trunc, rv
      integer   istat, kbut, jx, jy, kv
      integer*2 is
      logical   more
      external  trunc

Cbegin


      call ds_scol ( 1.0, KCOLGG )

      call printo ( '    Star     X      Y      Value' )

      more = .true.
      do while ( more )

         call ds_gcur ( .true., jx, jy, kbut, istat )			!Get cursor reply

         if ( istat.ne.0 ) then
             call printo ( 'ERROR: Invalid entry' )
             kbut = -1
         elseif ( kbut.eq.3 ) then					!Exit
            more = .false.
         elseif ( jx.gt.DSNXE .or. jx.lt.DSNXS .or.			!Out of image?
     +           jy.gt.DSNYE .or. jy.lt.DSNYS ) then
             kbut = kbut
         elseif ( kbut.eq.2 ) then					!Zoom
            call ds_zoom ( .true., 0, 1 )
         else
            NUMGP = NUMGP + 1						!Add position
            if ( NUMGP.gt.NMAXGP ) then
               write ( text, '(''ERROR: Can only store '',i7,
     +                         '' entries '')' ) NMAXGP
               call printo ( text )
               NUMGP = NMAXGP
            else
               KXGP(NUMGP) = jx
               KYGP(NUMGP) = jy
               x = jx
               y = jy
               call ds_cross ( x, y, CRSIZG, KCOLGG )
               if ( IMTYPE.eq.'SHORT' ) then
                  call cops1 ( %val(IPIM), NX, NY, jx, jy, is )
                  kv = is
                  if ( kv.ne.INVAL ) then
                     VALGP(NUMGP) = kv*BS + BZ
                     vl = trunc(VALGP(NUMGP),9)
                     write ( text, '(1x, 3i7,f14.3)' ) NUMGP, jx, jy, vl
                  else
                     VALGP(NUMGP) = 0.0
                     write ( text, '(1x,3i7,
     +                          ''   Bad pixel value'')' ) NUMGP, jx, jy
                  endif
               else
                  call copr1 ( %val(IPIM), NX, NY, jx, jy, rv )
                  if ( rv.ne.RINVAL ) then
                     VALGP(NUMGP) = rv*BS + BZ
                     vl = trunc(VALGP(NUMGP),9)
                     write ( text, '(1x, 3i7,f14.3)' ) NUMGP, jx, jy, vl
                  else
                     VALGP(NUMGP) = 0.0
                     write ( text, '(1x,3i7,
     +                          ''   Bad pixel value'')' ) NUMGP, jx, jy
                  endif
               endif
               call printo ( text )
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_GETMAG -- Get the cursor magnitudes
C
C  alan penny             ral             1990 Jan

      subroutine po_getmag ( )

      implicit none
      include 'gposns.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
C--
      character text*80
      real      trunc
      integer   istat, kbut, jx, jy
      logical   more
      external  trunc

Cbegin


      call ds_scol ( 1.0, KCOLGM )

      write ( text, '('' Star   Xpos   Ypos Dx Dy   Mag   Height'',
     +                ''  Base Max Pix Inv  Rms  Radx  Rady'')' )
      call printo ( text )

      more = .true.
      do while ( more )

         call ds_gcur ( .true., jx, jy, kbut, istat )			!Get cursor reply

         if ( istat.ne.0 ) then						!Invalid response
             call printo ( 'ERROR: Invalid entry' )
             kbut = -1
         elseif ( kbut.eq.3 ) then					!Exit
            more = .false.
         elseif ( jx.gt.DSNXE .or. jx.lt.DSNXS .or.			!Out of image?
     +           jy.gt.DSNYE .or. jy.lt.DSNYS ) then
             kbut = kbut
         elseif ( kbut.eq.2 ) then					!Zoom
            call ds_zoom ( .true., 0, 1 )
         else

            NUMGM = NUMGM + 1						!Get magn
            if ( NUMGM.gt.NMAXGM ) then
               write ( text, '(''ERROR: Can only store '',i7,
     +                         '' entries '')' ) NMAXGM
               call printo ( text )
               NUMGM = NMAXGM
            else
               call po_gauss ( jx, jy )
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_GAUSS -- Fit Gaussian at position
C
C  alan penny             ral             1990 Jan

      subroutine po_gauss ( jx, jy )

      implicit none
      include 'ST_IMAGE_INC'
      include 'gposns.inc'

      integer  jx		!i: Input X posn
      integer  jy		!i: Input Y posn
C--
      integer kw, iter, ninval, kxs, kxe, kys, kye, ngood, nbad,
     +        kp(4), kinval, karms, kah, kab, kavmax, kadx, kady
      real    x, y, amag, ht, base, dx, dy, xa, ya, rx, ry, rms,
     +        vmax, vmin, arx, ary, axa, aya, xs, xe, ys, ye
      character*80 text

      real trunc
      external trunc
Cbegin


      kw = 0								!Fit star
      if ( RADGFIX ) kw = 1
      x = jx
      y = jy
      if ( IMTYPE.eq.'SHORT' ) then
         call gauss2sa ( %val(IPIM), NX, NY, x, y, NXGB, NYGB, kw,
     +                   RXGF, RYGF, INVAL, 20, amag, ht, base, dx,
     +                   dy, xa, ya, rx, ry, rms, iter, ninval )
      else
         call gauss2ra ( %val(IPIM), NX, NY, x, y, NXGB, NYGB, kw,
     +                   RXGF, RYGF, RINVAL, 20, amag, ht, base, dx,
     +                   dy, xa, ya, rx, ry, rms, iter, ninval )
      endif
      ht = ht*BS
      if ( amag.lt.49.0 ) then
         amag = amag - 2.5*alog10(BS)
         base = base*BS + BZ
      else
         base = 0.0
      endif
      rms = rms*BS

      kxs = xa - NXGB/2							!Fitted box
      kxe = kxs + NXGB - 1
      kys = ya - NYGB/2
      kye = kys + NYGB - 1

      call st_minmax ( %val(IPIM), NX, NY, BS, BZ, IMTYPE, INVAL, 	!Max pixel
     +                 RINVAL, kxs, kxe, kys, kye, vmin, vmax, kp,
     +                 ngood, nbad )

      karms  = trunc(rms,5)						!Type results
      kah    = trunc(ht,6)
      kab    = trunc(base,6)
      axa    = trunc(xa,4)
      aya    = trunc(ya,4)
      arx    = trunc(rx,3)
      ary    = trunc(ry,3)
      kadx   = trunc(dx,3)
      kady   = trunc(dy,3)
      kavmax = trunc(vmax,6)
      kinval = min(ninval,999)
      write ( text, '(1x,i4, 1x,f6.1,1x,f6.1, 2i3, f8.3, i7, i6, 2x,i6,
     +                i4, i5,f6.2,f6.2)' ) NUMGM, axa, aya, kadx, kady,
     +                amag, kah, kab, kavmax, kinval, karms, arx, ary
      call printo ( text )

      RESMAG(1,NUMGM) = xa						!Store results
      RESMAG(2,NUMGM) = ya
      RESMAG(3,NUMGM) = amag
      RESMAG(4,NUMGM) = dx
      RESMAG(5,NUMGM) = dy
      RESMAG(6,NUMGM) = real(iter)
      RESMAG(7,NUMGM) = rms
      RESMAG(8,NUMGM) = real(ninval)
      RESMAG(9,NUMGM) = ht
      RESMAG(10,NUMGM) = base
      RESMAG(11,NUMGM) = vmax
      RESMAG(12,NUMGM) = rx
      RESMAG(13,NUMGM) = ry
      RESMAG(14,NUMGM) = NXGB
      RESMAG(15,NUMGM) = NYGB

      call ds_cross ( xa, ya, CRSIZG, KCOLGM )				!Plot star centre
      if ( BSHOW ) then							!Plot star box
         xs = max(1,min(NX,kxs))
         xe = max(1,min(NX,kxe))
         ys = max(1,min(NY,kys))
         ye = max(1,min(NY,kye))
         call ds_box ( xs, xe, ys, ye, KCOLGM )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_STORP -- Store cursor positions in file
C
C  alan penny             ral             1990 Jan

      subroutine po_storp ( )

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'
C--
      integer ip, istat
      character*50 cv
Cbegin


      if ( ST_FAILED ) return

      if ( NUMGP.lt.1 ) then
         call printo ( 'ERROR: No positions to store' )
         return
      endif

      call optabw ( 'OUTTAB', ip, 8, NUMGP, .true., istat )
      if ( ST_FAILED ) return

      if ( istat.eq.2 ) then
         call printo ( 'No file written' )
      elseif ( istat.eq.0 ) then
         call ident  ( %val(ip), 8, NUMGP )
         call coptir ( KXGP,  NUMGP, %val(ip), 8, NUMGP, 6, 1 )
         call coptir ( KYGP,  NUMGP, %val(ip), 8, NUMGP, 7, 1 )
         call coptrr ( VALGP, NUMGP, %val(ip), 8, NUMGP, 8, 1 )
         call pthead ( 'OUTTAB', 1, 'X', istat )
         call pthead ( 'OUTTAB', 2, 'Y', istat )
         call pthead ( 'OUTTAB', 3, 'VALUE', istat )
         call get1c  ( 'TITLE', cv,
     +                 'XY Positions from Interact:Posns', .true. )
         call ptdesc ( 'OUTTAB', 'TITLE', cv )
         NUMGP = 0
         call canpar ( 'OUTTAB' )
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_STORM -- Store cursor magnitudes in file
C
C  alan penny             ral             1990 Jan

      subroutine po_storm ( )

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'
C--
      integer ip, istat, k
      character*50 cv

      character*20 head(NXMAG)
      data head / 'X', 'Y', 'Magnitude', 'Dx', 'Dy', 'Iterations',
     +       'Rms', 'Numinval', 'Height', 'Base', 'Max pixel',
     +       'Rx', 'Ry', 'Xbox', 'Ybox' /
Cbegin


      if ( ST_FAILED ) return

      if ( NUMGM.lt.1 ) then
         call printo ( 'ERROR: No magnitudes to store' )
         return
      endif

      call optabw ( 'OUTTAB', ip, NXMAG+5, NUMGM, .true., istat )
      if ( ST_FAILED ) return

      if ( istat.eq.2 ) then
         call printo ( 'No file written' )
      elseif ( istat.eq.0 ) then

         call get1c  ( 'TITLE', cv, 'Magnitudes from Interact:Magns',
     +                 .true. )
         if ( ST_FAILED ) return
         do k = 1, NXMAG
            call pthead ( 'OUTTAB', k, head(k), istat )
         enddo
         call ptdesc ( 'OUTTAB', 'TITLE', cv )
         call ident  ( %val(ip), NXMAG+5, NUMGM )
         call coprr ( RESMAG, NXMAG, NUMGM, 1, NXMAG, 1, NUMGM,
     +                %val(ip), NXMAG+5, NUMGM, 6, 1 )
         NUMGM = 0
         call canpar ( 'OUTTAB' )

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_LIST -- Type out posns or Gauss posn and mags
C
C  alan penny             ral             1990 Jan

      subroutine po_list ( kopt )

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'

      integer      kopt		!i: WHich list (1=posns;2=Gauss posns + magns)
C--
      integer k, iter, ninval, kinval, karms, kah, kab, kavmax, kadx,
     +        kady, jx, jy
      real    amag, ht, base, dx, dy, xa, ya, rx, ry, rms,
     +        vmax, arx, ary, axa, aya, vl
      character*80 text

      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 ) then

         if ( NUMGP.lt.1 ) then
            call printo ( 'No posns got yet' )
         else
            call printo ( '    Star     X      Y      Value' )
            do k = 1, NUMGP
               jx = KXGP(k)
               jy = KYGP(k)
               vl = trunc(VALGP(k),9)
               write ( text, '(1x, 3i7,f14.3)' ) k, jx, jy, vl
               call printo ( text )
            enddo
         endif

      else

         if ( NUMGM.lt.1 ) then
            call printo ( 'No Gauss posns and magnitudes got yet' )
         else
           write ( text, '('' Star   Xpos   Ypos Dx Dy   Mag   Height'',
     +                   ''  Base Max Pix Inv  Rms  Radx  Rady'')' )
           call printo ( text )

           do k = 1, NUMGM

              xa = RESMAG(1,k)
              ya = RESMAG(2,k)
              amag = RESMAG(3,k)
              dx = RESMAG(4,k)
              dy = RESMAG(5,k)
              iter = RESMAG(6,k)
              rms = RESMAG(7,k)
              ninval = RESMAG(8,k)
              ht = RESMAG(9,k)
              base = RESMAG(10,k)
              vmax = RESMAG(11,k)
              rx = RESMAG(12,k)
              ry = RESMAG(13,k)

              karms  = trunc(rms,5)
              kah    = trunc(ht,6)
              kab    = trunc(base,6)
              axa    = trunc(xa,4)
              aya    = trunc(ya,4)
              arx    = trunc(rx,3)
              ary    = trunc(ry,3)
              kadx   = trunc(dx,3)
              kady   = trunc(dy,3)
              kavmax = trunc(vmax,6)
              kinval = min(ninval,999)
              write ( text, '(1x,i4, 1x,f6.1,1x,f6.1, 2i3, f8.3, i7,
     +                        i6, 2x,i6, i4, i5,f6.2,f6.2)' )
     +                       k, axa, aya, kadx, kady,
     +                amag, kah, kab, kavmax, kinval, karms, arx, ary
              call printo ( text )
           enddo
         endif

      endif

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_PLOTFILE -- Put file positions on screen
C
C  alan penny             ral             1990 Jan

      subroutine po_plotfile ()

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'
C--
      real xa, ya
      integer k, ierr
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'INTAB', IPINTAB, NTBX, NTBY, .true., ierr )
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         if ( ierr.eq.2 ) call printo ( 'ERROR: No file accessed' )
         return
      endif

      call ds_scol ( 1.0, KCOLGP )

      do k = 1, NTBY
         call copr1 ( %val(IPINTAB), NTBX, NTBY, 6, k, xa )
         call copr1 ( %val(IPINTAB), NTBX, NTBY, 7, k, ya )
         call ds_cross ( xa, ya, CRSIZG, KCOLGP )
      enddo

      call canpar ( 'INTAB' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_BRIEF -- Tell user present setups
C
C    a j penny                    ral         1990 jan

      subroutine po_brief ()

      implicit none
      include 'gposns.inc'
      include 'STARMAN_INC'
C--
      character*15 col(8)
      data col / 'red', 'green', 'blue', 'cyan', 'magenta',
     +           'yellow', 'coral', 'palegreen' /
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      call pargi ( NUMGP )
      call printd ( 'Number of positions marked is - : %d ' )
      call pargi ( NUMGM )
      call printd ( 'Number of magnitudes got is -   : %d ' )
      call pargi ( NXGB )
      call pargi ( NYGB )
      call printd ( 'Size of magnitude getting box - : %d x %d ' )
      if ( BSHOW ) then
         call printo ( 'Box round magn position -       : shown' )
      else
         call printo ( 'Box round magn position -       : not shown' )
      endif
      if ( RADGFIX ) then
         call printo ( 'Radii of Gaussian are -         : fixed ' )
      else
         call printo ( 'Radii of Gaussian are -         : variable ' )
      endif
      call pargr ( RXGF )
      call pargr ( RYGF )
      call printd ( 'Default Magnitude Gauss Radii - : %f %f ' )

      call pargc ( col(KCOLGG) )
      call printd ( 'Cross posns colour is -         : %c ' )
      call pargc ( col(KCOLGM) )
      call printd ( 'Cross and box magns colour is - : %c ' )
      call pargc ( col(KCOLGP) )
      call printd ( 'Cross plotting file colour is - : %c ' )
      call pargr ( CRSIZG )
      call printd ( 'Cross size is -                 : %f ' )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_MAINGET -- Get main prog params to 'position' ones
C
C   a j penny                 ral               1990-06-09

      subroutine po_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call po_srmainget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine po_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_MAINPUT -- Put 'position' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine po_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call po_srapget

      OPDISP    = X_OPDISP
      GOTIMAGE  = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PO_SRAPGET -- Get 'positions' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine po_srapget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'

C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    SCRUTINY.FOR
C
C    Contains:-
C
C T_SCRUTINY    Perform the SCRUTINY program
C SC_OPTION_SETUP  Set up option choices
C SC_CHECKDI    Check display ok and image displayed
C SC_MAINGET    Translate main prog params to 'scrutiny' ones
C SC_SRMAINGET  Get main prog params to temp ones
C SC_MAINPUT    Translate 'scrutiny' params to main prog ones
C SC_SRSCRUTGET Get 'scrutiny' s/r params to temp ones
C SC_SDEF       Set up starting default values
C SC_GETLIST    Get star list
C SC_GETPROF    Get star profile. Copy map into real array.
C SC_LISTSEL    Define a selection of the star list. number or area
C SC_AREM       Display the residuals to the fit, replace by data; or
C SC_DOSEL      Star to be dealt with (star list posn or area posn)?
C SC_ARCLEAR(RS)Make an array of a section the original image with
C SC_DISIMG     Display image
C SC_LWRITE     Writes to file star and extra list
C SC_DOTYPE     Type or print to file the input star measures
C SC_ADOFORM    Puts out lines of formatted text to screen and/or file
C SC_GTFORM     Gets format of output line; - Number of characters
C SC_TEXTLINE   Makes a line of text from a line of data
C SC_WRTHEAD    Write a text line of headers
C SC_DOFULL     Types out all the details on a star
C SC_LNTIDY     Removes the unwanted Identifier characters and if
C SC_PRFILE     Type out data
C SC_XIDENT     Loads a table row with an identifier
C SC_OPDISP	Open display


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_SCRUTINY -- Perform the SCRUTINY program function
C
C   a j penny                 dao           1988-04-25

      subroutine t_scrutiny ( )

      implicit none
      include 'scrut.inc'
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr, kx, ky, kb, iva
      real    rva, rvb
      logical loop
      character*12 ktopt, ktoptc, ktoptd
Cbegin


      if ( ST_FAILED ) return

      call sc_mainget							!Bring in new info

      call sc_option_setup ( ktopt, 4, .true. )
      if ( ST_FAILED ) return
      loop = .true.
      do while ( loop )							!Loop thru options

         if ( ktopt.eq.'false' ) ktopt = ktoptd

         call sc_option_setup ( ktopt, 4, .false. )
         call get_choice ( ktopt, 1 )					!Get choice

         call sc_checkdi ( ktopt, ktoptc, ktoptd )			!Check displayed image

         if ( ktopt.eq.'stars_add' ) call sc_cadd 			!Add via cursor

         if ( ktopt.eq.'blink' ) call ds_blink ( iva, rva, rvb )        !Blink

         if ( ktopt.eq.'display' ) call sc_disimg			!Display

         if ( ktopt.eq.'flash' ) then                                   !Flash image
                                    call in_opdisp ( ierr )
                                    call ds_doflash ( %val(IPIM), NX,
     +                                NY, IMTYPE, DSKVRANGE, IMTITLE )
                                    DISPLAYED = .true.
                                 endif

         if ( ktopt.eq.'comps_dist' ) call sc_jpaint ( %val(IPIN) )	!Lookclist

         if ( ktopt.eq.'comps_all' ) call sc_lpaint ( %val(IPIN) ) 	!Looklist

         if ( ktopt.eq.'nearest' ) call sc_gnearest ( %val(IPIN) )	!Get posn and nearest star

         if ( ktopt.eq.'remove' ) call sc_arem ( %val(IPIN), 1 ) 	!Remove

         if ( ktopt.eq.'replace' ) call sc_arem ( %val(IPIN), 2 )	!Replace

         if ( ktopt.eq.'reremove' ) call sc_arem ( %val(IPIN), 3 ) 	!Reremove

         if ( ktopt.eq.'clear' ) then					!Clear (display)
                                    call ds_erase
                                    DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'close' ) then                                   !Close display
                                    call ds_close ( ierr )
                                    OPDISP = .false.
                                    DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )                 !Open display

         if ( ktopt.eq.'box' ) call sc_doring				!Ring

         if ( ktopt.eq.'mark_old' ) call sc_bpaint ( %val(IPIN), 2 )	!Shoposn

         if ( ktopt.eq.'mark_new' ) call sc_bpaint ( %val(IPIN), 1 )	!Shnposn

         if ( ktopt.eq.'zoom' ) call ds_zoom ( .true., 0, 0 )		!Zoom/pan display

         if ( ktopt.eq.'reset' ) call ds_zoom ( .true., 1, 0 )		!Reset Zoom/pan display

         if ( ktopt.eq.'type_one' ) call sc_dofull ( %val(IPIN) )	!Full

         if ( ktopt.eq.'image' ) call in_newim ( ierr ) 		!Image

         if ( ktopt.eq.'im_get_flash' ) then                            !Input new image and display
                                          call in_newim ( ierr )
                                          if ( .not.ST_FAILED .and.
     +                                         ierr.eq.0 ) then
                                             call in_opdisp ( ierr )
                                             call ds_doflash (
     +                                       %val(IPIM), NX, NY,
     +                                       IMTYPE, DSKVRANGE,
     +                                       IMTITLE )
                                             DISPLAYED = .true.
                                          endif
                                       endif

         if ( ktopt.eq.'area' ) call in_imgsiz ( NX, NY, 1 )		!Change area to show

         if ( ktopt.eq.'list_in' ) call sc_getlist 			!List

         if ( ktopt.eq.'list_out' ) call sc_lwrite ( %val(IPIN) )	!Write new

         if ( ktopt.eq.'profile_in' ) call sc_getprof			!Profile

         if ( ktopt.eq.'printlist' ) call sc_dotype ( %val(IPIN), 1 )	!Printlist

         if ( ktopt.eq.'typelist' ) call sc_dotype ( %val(IPIN), 2 )	!Typelist

         if ( ktopt.eq.'lselect' ) call sc_listsel 			!Change list selection

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb,	!Cursor get image values
     +                                             ierr )

         if ( ktopt.eq.'panel' ) call choice_panel_sw 			!Panel choice

         if ( ktopt.eq.'stars_rem' ) call sc_crem ( %val(IPIN) )	!Remove via cursor

         if ( ktopt.eq.'main' .or. ktopt.eq.'alter' .or. 		!Return to main program
     +        ktopt.eq.'aperture' .or. ktopt.eq.'colour' .or.
     +        ktopt.eq.'inspect' .or. ktopt.eq.'fit_magns' .or.
     +        ktopt.eq.'positions' .or.
     +        ktopt.eq.'exit' )  then
                                    MAIN_SELECT = ktopt
                                    loop = .false.
                                 endif

         if ( ST_FAILED ) loop = .false.

      enddo

      call sc_mainput							!Translate to main prog params


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1994 jan

      subroutine sc_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'

      character*12   ktopt		!i: Chosen option
      integer        set_num		!i: Code for set of options
      logical        koutside		!i: Is this called from outside loop?
C--
      integer j,k

      integer opt_num
      parameter ( opt_num=39 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen. (The programme does not forget about',
     + 'the input image.)',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return.' /

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'im_get_flash', 'Input new image and display it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, without displaying an image.',
     + 'The screen may be any size you want, through keyboard entry',
     + 'via the SCREEN parameter.',
     + ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.)',
     + ' ', ' ', ' '/

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'blink', 'Blink the displayed image' ,
     + 'By alternately putting the window -above- and -below- any ',
     + 'other window present, -blinking- is simulated. To use, open',
     + 'another window, display an image you want blinked this one,',
     + 'and align it with this one. The BLINK_CH keyboard parameter',
     + 'chooses between end:auto:undr:over modes. For auto, ',
     + 'the LH button slows x2 the blink, centre speeds x2, RH exits.' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'comps_dist', 'Mark posns for next list star with distant comp',
     + 'This searches the MEASURE output list, starting at the list ',
     + 'number you input (0 to end this option), for the next one with',
     + 'a companion star(s), used in the fitting and whose centre is',
     + 'outside the box used for fitting. Such stars have',
     + 'wings affecting the main star, whilst being distant from it.',
     + 'The details are typed out and the star and companions painted.'/

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'comps_all','Mark posn of a star and its companions in the fit',
     + 'This takes the MEASURE list of magnitudes, and looks at the',
     + 'star whose number you choose. (Enter 0 to end this option.)',
     + 'The details of the fit are typed out and the star and its',
     + 'companions are painted on the displayed image.',
     + ' ',
     + 'This can only be used for stars with companions in their fit.' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'lselect', 'Select a portion of the list to look at' ,
     + 'First, choose the method of selecting which star to look at.',
     + 'The two ways are either by choosing a block of stars in the ',
     + 'input MEASURE list, or by selecting an area of the image (and',
     + 'then only stars in that area are looked at). ',
     + ' ',
     + 'Second, choose that list range or image area. ' /

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'nearest', 'Give nearest list star to cursor position',
     + 'Place the cursor near a star, and press the left mouse button.',
     + 'The program finds the nearest star in the list and types',
     + 'out the cursor position and the details of the nearest star ',
     + '(number, name, X, Y, height, magnitude). Press the RH button ',
     + 'to end this option. The list has input and fitted posns, so ',
     + 'the star may be different in the two, so both are typed.' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'replace', 'Replace the -Remove- box by the original image data',
     + 'In the -remove- option in this list, an area round a fitted ',
     + 'in the displayed image may be cleaned of stars. This option ',
     + 'lets you put back the area to what it was before. ',
     + ' ', ' ', ' ' /

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'remove', 'Clean an area round a list star with stars removed',
     + 'Take a star from the input list, and clear the area in the',
     + 'displayed image used in its fit. Subtract the star itself and',
     + 'all (if any) of the other stars near it which were taken into',
     + 'account in the fitting. The sloping backfround found by the ',
     + 'fit may be removed. The display in this area may be done at ',
     + 'a high contrast to show the fine details.   **It is slow.**' /

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'reremove', 'Display the -Remove- box again',
     + 'In the -remove- option in this list, an area round a fitted ',
     + 'in the displayed image may be cleaned of stars. Then the',
     + '-rpleace- option lets you put the area back as it was. This ',
     + 'option lets you -remove- the area again, quickly.',
     + ' ', ' ' /

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'box', 'Put a box in the image outside the star -Remove- area',
     + 'In the -remove- option in this list, an area round a fitted ',
     + 'in the displayed image is cleaned of stars. This option',
     + 'paints a rectangle round this area round the present star.',
     + ' ', ' ', ' ' /

      data opt_text(22),opt_head(22),(opt_help(j,22),j=1,6) /
     + 'mark_new', 'Mark up on the display the -new- star list posns',
     + 'Mark on the display the positions of the stars in the MEASURE',
     + 'star list. Both the fitted (-new-) positions and those input',
     + 'before fitting (-old-) are stored in the list. This option',
     + 'uses the -new- ones. The mark can be either a spot or a cross',
     + 'of size defined by you. You can choose the colour for stars ',
     + 'with -good- fits and that for stars with -failed- fits.' /

      data opt_text(23),opt_head(23),(opt_help(j,23),j=1,6) /
     + 'mark_old', 'Mark up on the display the -old- star list posns',
     + 'Mark on the display the positions of the stars in the MEASURE',
     + 'star list. Both the fitted (-new-) positions and those input',
     + 'before fitting (-old-) are stored in the list. This option',
     + 'uses the -old- ones. The mark can be either a spot or a cross',
     + 'of size defined by you. You can choose the colours for stars ',
     + 'with -good- fits and -failed- fits (from the fitting).' /

      data opt_text(24),opt_head(24),(opt_help(j,24),j=1,6) /
     + 'stars_add', 'Use cursor to add stars to star list',
     + 'Place the cursor in the displayed image and press the mouse',
     + 'left-hand button. The cursor position is added on to the end ',
     + 'of the star input list. Press the right-hand button to exit',
     + 'from this option. Either a spot or a cross may be used to ',
     + 'mark the new location. ',
     + ' ' /

      data opt_text(25),opt_head(25),(opt_help(j,25),j=1,6) /
     + 'stars_rem', 'Use cursor to remove stars from star list',
     + 'Place the cursor in the displayed image and press the mouse',
     + 'left-hand button. The star in the input list that is nearest ',
     + 'to the cursor position is removed from the list. Press the ',
     + 'right-hand button to exit from this option. Either a spot or a',
     + 'cross may be used to mark this removed star. One of the extra',
     + 'stars that may have been added by -stars_add- may be selected.'/

      data opt_text(26),opt_head(26),(opt_help(j,26),j=1,6) /
     + 'area', 'Select area of image to display',
     + 'If the image is too large for the display screen, one can ',
     + 'select an area to display. ',
     + 'This is done through the keyboard parameters, XRANGE and',
     + 'YRANGE, which delimit the X and Y areas to work with.',
     + ' ', ' ' /

      data opt_text(27),opt_head(27),(opt_help(j,27),j=1,6) /
     + 'type_one', 'Type out all of a star fit (from number/name)',
     + 'Type out all the details of a star in the MEASURE list. You ',
     + 'can choose whether to select the star by its name or its ',
     + '-number- (that is its row number in the star list).',
     + 'All the details of the stars fit are given out, with their  ',
     + 'short description names. ',
     + ' ' /

      data opt_text(28),opt_head(28),(opt_help(j,28),j=1,6) /
     + 'printlist', 'Put a neat summary of the fits into an ascii file',
     + 'Put out into a file the -lselect- portion of the stars list. ',
     + 'Either:- a standard selection of the list columns',
     + '        (with or without the XY positions)',
     + '    or:- a user-defined selection of the 51 parameters in the',
     + '         list. Any can be chosen, and put in defined form',
     + '         as long as there is room in a single output line' /

      data opt_text(29),opt_head(29),(opt_help(j,29),j=1,6) /
     + 'typelist', 'Put a neat summary of the fits onto the terminal',
     + 'Type out the -lselect- selected portion of the stars list. ',
     + 'Either:- a standard selection of the list columns',
     + '        (with or without the XY positions)',
     + '    or:- a user-defined selection of the 51 parameters in the',
     + '         list. Any can be chosen, and put in defined form',
     + '         as long as there is room in a single output line' /

      data opt_text(30),opt_head(30),(opt_help(j,30),j=1,6) /
     + 'profile_in', 'Load a new star profile (discard the old one)',
     + 'Input the file containing the star profile. This is the image',
     + 'that is produced by PROFILE and used in MEASURE. This program',
     + 'uses it in adding or removing stars in the image. ',
     + ' ',
     + 'The image has descriptors for the parameters of the analytical',
     + 'part of the profile, and pixel values for the empirical part.'/

      data opt_text(31),opt_head(31),(opt_help(j,31),j=1,6) /
     + 'list_in', 'Load a new star list (discard the old one)',
     + 'Input the MEASURE output file with the fitted star magnitudes.',
     + 'This is the main file of this SCRUTINY function, on which all',
     + 'the options operate. Such a list has the positions and ',
     + 'magnitudes of the stars, and many other parameters of the fit',
     + '(51 in all). If the list is not a MEASURE output, it can still',
     + 'be used, but care is called for.' /

      data opt_text(32),opt_head(32),(opt_help(j,32),j=1,6) /
     + 'list_out', 'Write to file the -Add- used (extra,removed) list',
     + 'The -stars_add- and -stars_rem- options of this function can',
     + 'be used to add and remove stars from the list.',
     + 'This option permits you to save the changed list to file. ',
     + ' ', ' ', ' ' /

      data opt_text(33),opt_head(33),(opt_help(j,33),j=1,6) /
     + 'exit', 'Exit from main program',
     + ' ',
     + 'Exit from program. Do not access main option list first.',
     + ' ', ' ', ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'main', 'Functions that appeared at start of program',
     + 'New set of buttons appear. These are the buttons that were',
     + 'seen at the start of program.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(34),opt_head(34),(opt_help(j,34),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      data opt_text(35),opt_head(35),(opt_help(j,35),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(36),opt_head(36),(opt_help(j,36),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(37),opt_head(37),(opt_help(j,37),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(38),opt_head(38),(opt_help(j,38),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(39),opt_head(39),(opt_help(j,39),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /


      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact - Scrutiny', 'SOPTION', 4 /

      integer def_x, def_y
      parameter ( def_x=2 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'list_in' /

      integer sect_num
      parameter ( sect_num=7 )
      character*10 sect_head(sect_num)
      data sect_head / 'STARS', 'LOOKING', 'ACTIONS', 'IMAGE',
     +                 'DISPLAY', 'FUNCTIONS', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'profile_in:list_in:list_out' /
      data sect_text(2) / 'typelist:printlist:type_one:mark_new:
     +                     mark_old:lselect:comps_all:comps_dist:
     +                     nearest' /
      data sect_text(3) / 'blink:replace:remove:reremove:box:
     +                     stars_add:stars_rem' /
      data sect_text(4) / 'image:im_get_flash' /
      data sect_text(5) / 'area:clear:close:cvalues:display:
     +                     flash:open:reset:zoom' /
      data sect_text(6) / 'alter:aperture:colour:fit_magns:
     +                     inspect:main:positions' /
      data sect_text(7) / 'panel:exit' /

      integer help_num
      parameter ( help_num=34 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + ' ' ,
     + ' ' ,
     + 'For workstation use, the Cursor must be in image window area',
     + 'for the cursor buttons to work when in -WORKING- mode. The ',
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' ',
     + ' Buttons for Blink: Left Button = slow blink x2' ,
     + '                    Centre Button = speed blink x2' /
      data (help_text(k),k=11,20) /
     + '                    Right Button = switch to hand blink' ,
     + '           hand blink operates via keyboard' ,
     + ' ' ,
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' ,
     + '     Centre Button twice            = zoom x2' ,
     + '     Left Button then Centre Button = pan' ,
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Zoom means zoom around present position of cursor ' /
      data (help_text(k),k=21,30) /
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by' ,
     + ' request on the terminal if the panel is not being used.' ,
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' ,
     + ' XWindows        YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits ' /
      data (help_text(k),k=31,help_num) /
     + '     ' ,
     + 'Buttons from add/remove stars to list:-',
     + '   Left and Centre = Add/remove a star',
     + '   Right           = End' /
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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_CHECKDI -- Check display ok and image displayed
C
C   a j penny                 ral               1990-06-09

      subroutine sc_checkdi ( ktopt, ktoptc, ktoptd )
      implicit none

      include 'interact.inc'
      character*(*)  ktopt	!i/o: Only check if needed for this option
				!     [on output if bad, set to 'false']
      character*(*)  ktoptc	!o: If bad, set to input value of 'ktopt';
				!   If good, set to ' '.
      character*(*)  ktoptd	!o: Input value of KTOPT
C--
Cbegin


      ktoptc = ' '
      ktoptd = ktopt

      if ( ktopt.eq.'mark_new' .or. ktopt.eq.'mark_old' .or.		!Check displayed image
     +     ktopt.eq.'comps_all' .or. ktopt.eq.'comps_dist' .or.
     +     ktopt.eq.'nearest' .or.
     +     ktopt.eq.'blink' .or. ktopt.eq.'replace' .or.
     +     ktopt.eq.'remove' .or. ktopt.eq.'reremove' .or.
     +     ktopt.eq.'ring' .or.
     +     ktopt.eq.'stars_add' .or. ktopt.eq.'stars_rem' .or.
     +     ktopt.eq.'clear' .or. ktopt.eq.'cvalues' .or.
     +     ktopt.eq.'zoom' ) then

         if ( .not.GOTIMAGE ) then
            call printo ( 'No image got yet' )
            ktoptc = 'image'
            ktopt = 'false'
         elseif ( .not.DISPLAYED ) then
            call printo ( 'No displayed image yet' )
            ktoptc = 'flash'
            ktopt = 'false'
         endif

      endif

      if ( ktopt.eq.'display' .or. ktopt.eq.'flash' ) then
         if ( .not.GOTIMAGE ) then
            call printo ( 'No image got yet' )
            ktoptc = 'image'
            ktopt = 'false'
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_MAINGET -- Get main prog params to 'scrutiny' ones
C
C   a j penny                 ral               1990-06-09

      subroutine sc_mainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call sc_srmainget

      OPDISP   = X_OPDISP
      GOTIMAGE = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_SRMAINGET -- Get main prog params to temp params
C
C   a j penny                 ral               1990-06-09

      subroutine sc_srmainget ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_MAINPUT -- Put 'scrutiny' params to main prog ones
C
C   a j penny                 ral               1990-06-09

      subroutine sc_mainput ()

      implicit none

      include 'interact.inc'
      include 'x_main.inc'
C--
Cbegin


      call sc_srscrutget

      OPDISP   = X_OPDISP
      GOTIMAGE = X_GOTIMAGE
      DISPLAYED = X_DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_SRSCRUTGET -- Get 'scrutiny' s/r params to temp ones
C
C   a j penny                 ral               1990-06-09

      subroutine sc_srscrutget ( )

      implicit none

      include 'interact.inc'
      include 'x_main.inc'

C--
Cbegin


      X_OPDISP    = OPDISP
      X_GOTIMAGE  = GOTIMAGE
      X_DISPLAYED = DISPLAYED


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_SDEF -- Set up starting default values
C
C   a j penny                 dao           1988-04-25

      subroutine sc_sdef ( )

      implicit none
      include 'scrut.inc'
      include 'interact.inc'
C--
      integer ipd, istat
Cbegin


      GOTIMAGE = .false.
      OPDISP = .false.

      OPSCRUT = .true.
      NEXTRA = 0
      GOTLIST = .false.
      GOTPROF = .false.
      DREMOVE = .false.
      DREPLACE = .false.
      LX = 0
      LY = 0
      LXOLD = 0
      LYOLD = 0
      LXOLDA = 0
      LYOLDA = 0
      LXS = 0
      LYS = 0
      LXSOLD = 0
      LYSOLD = 0
      DOCONT = .false.
      DOSLOPE = .true.
      DOPREF = .true.
      DOFORM = .false.
      DOXY = .true.
      LPOS(1) = 1
      LPOS(2) = 1000000
      LAREA(1,1) = -1000000.0
      LAREA(2,1) = 1000000.0
      LAREA(1,2) = -1000000.0
      LAREA(2,2) = 1000000.0
      BYLIST = .true.
      DSIZE = 512

      call gtwrkr ( 'DUMMY', 1, ipd, istat )				!Open dummy array to provide default pointer for list
      IPIN = ipd
      TBVXS = 6
      TBYS = 1
      TBXS = 1
      IPMAP = ipd
      IPP = ipd
      MX = 1
      MY = 1
      MZ = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_GETLIST -- Get star list
C
C   a j penny                 dao           1988-04-25

      subroutine sc_getlist ()

      implicit none

      include 'scrut.inc'
C--
      integer istat
Cbegin


      if ( GOTLIST ) then
         call canpar ( 'INSTARS' )
         GOTLIST = .false.
      endif

      call optabr ( 'INSTARS', IPIN, TBVXS, TBYS, .true., istat )
      if ( istat.eq.0 ) then
         GOTLIST = .true.
         TBXS = TBVXS - 5
      else
         GOTLIST = .false.
         return
      endif

      if ( TBXS.ne.51 ) call printo ( 'WARNING: File may not be '//
     +                  'MEASURE output. Careful for SCRUTINY' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_GETPROF -- Get star profile. Copy map into real array.
C
C   a j penny                 dao           1988-04-25

      subroutine sc_getprof ()

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'
C--
      real pbs, pbz, qbase, volk, volradk
      integer istat
Cbegin


      if ( GOTPROF ) then
         call wrkcan ( 'PROFA' )
         GOTPROF = .false.
      endif

      call get_mprof ( 'PROFILE', IPMAP, PROF, MX, MY, MZ, MAGNIFK,
     +                 MAPXK, MAPYK, pbs, pbz, qbase, volk, volradk,
     +                 istat )
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         call canpar ( 'PROFILE' )
         call printo ( 'ERROR: Cannot get profile' )
      else
         GOTPROF = .true.
         call gtwrkr ( 'PROFA', MX*MY*MZ, IPP, istat )
         call copsr ( %val(IPMAP), MX, MY*MZ, 1, MX, 1, MY*MZ,
     +                %val(IPP), MX, MY*MZ, 1, 1 )
         call arrsc ( %val(IPP), MX, MY*MZ, pbs, pbz )
         call rchzero ( %val(IPP), MX, MY*MZ, 1, 1, MX, MY*MZ, DOMAP )
         if ( DOMAP ) call get1b ( 'DOMAP', DOMAP, DOMAP )
         call canpar ( 'PROFILE' )
         if ( ST_FAILED ) return
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_LISTSEL -- Define a selection of the star list. number or area
C
C          a j penny                 dao          1988-05-28

      subroutine sc_listsel ()

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'BYLIST', BYLIST, BYLIST )
      if ( ST_FAILED ) return

      if ( BYLIST ) then
         if ( GOTLIST ) then
            LPOS(1) = max(1,min(TBYS,LPOS(1)))
            LPOS(2) = max(1,min(TBYS,LPOS(2)))
         endif
         call get2i ( 'LRANGE', LPOS(1), LPOS(2), .true., 1, TBYS )
      else
         call get2r ( 'LXRANGE', LAREA(1,1), LAREA(2,1), .true.,
     +                -1.0e10, 1.0e10 )
         call get2r ( 'LYRANGE', LAREA(1,2), LAREA(2,2), .true.,
     +                -1.0e10, 1.0e10 )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_AREM -- display the residuals to the fit, replace by data; or
C            reremove the fit
C
C    a j penny                            stsci             1987 mar


      subroutine sc_arem ( data, kopt )

      implicit none

      include 'scrut.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real	data(TBVXS,TBYS)	!i: Starman:Measure input table
      integer	kopt			!i: Option (1=Remove;2=Replace;
					!           3=Reremove)
C--
      integer lxsa, lxsb, lysa, lysb, ipwa, kstar, istat
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 .and. .not.GOTLIST ) then				!Check set up
         call printo ( 'ERROR: No list' )
         return
      elseif ( kopt.eq.1 .and. .not.GOTPROF ) then
         call printo ( 'ERROR: No profile' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      elseif ( .not.OPDISP ) then
         call printo ( 'ERROR: No image displayed' )
         return
      elseif ( kopt.ne.1 .and. .not.DREMOVE ) then
         call printo ( 'No area removed' )
         return
      endif

      if ( kopt.eq.1 ) then						!Calculate and display new area

         call get1i ( 'NUMBER', kstar, 1, 1, TBYS )
         call get1b ( 'DOSLOPE', DOSLOPE, DOSLOPE )
         call get1b ( 'CONTRAST', DOCONT, DOCONT )
         if ( ST_FAILED ) return

         LX = nint(data(29+5,kstar))					!Calculate removed area
         LY = nint(data(30+5,kstar))

         if ( .not.DREMOVE .or. LX.ne.LXOLD. or. LY.ne.LYOLD ) then
            if ( DREMOVE ) call wrkcan ( 'WORKA' )
            call gtwrkr ( 'WORKA', LX*LY, ipwa, istat )
            LXOLD = LX
            LYOLD = LY
         endif
         DREMOVE = .true.

         if ( IMTYPE.eq.'SHORT' ) then
            call sc_arclears ( %val(IPIM), data, kstar, %val(ipwa),
     +                         LX, LY )
         else
            call sc_arclearr ( %val(IPIM), data, kstar, %val(ipwa),
     +                         LX, LY )
         endif
         if ( DOCONT ) call ds_imgscl ( %val(ipwa), LX, LY, IMTYPE,
     +                                   1, LX, 1, LY )
         call ds_acim ( %val(ipwa), LX, LY, IMTYPE, 1, LX, 1, LY, LXS,
     +                  LYS, .false. )

      endif

      if ( kopt.eq.2 ) then						!Display image into that area

         if ( .not.DREPLACE .or. LX.ne.LXOLDA. or. LY.ne.LYOLDA ) then
            LXOLDA = LX
            LYOLDA = LY
         endif
         DREPLACE = .true.
         if ( LXS.ne.LXSOLD .or. LYS.ne.LYSOLD .or. LX.ne.LXOLDA .or.
     +        LY.ne.LYOLDA ) then
            lxsa = min(NX,max(1,LXS))
            lysa = min(NY,max(1,LYS))
            lxsb = min(NX,max(1,LXS+LX-1))
            lysb = min(NY,max(1,LYS+LY-1))
            LXSOLD = LXS
            LYSOLD = LYS
         endif
         call ds_acim ( %val(IPIM), NX, NY, IMTYPE, lxsa, lxsb,
     +                  lysa, lysb, LXS, LYS, .false. )

      endif

      if ( kopt.eq.3 ) then						!Diplay removed area

         if ( DOCONT ) call ds_imgscl ( %val(ipwa), LX, LY, IMTYPE,
     +                                  1, LX, 1,LY )
         call ds_acim ( %val(ipwa), LX, LY, IMTYPE, 1, LX, 1, LY,
     +                  LXS, LYS, .false. )

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DOSEL -- Star to be dealt with (star list posn or area posn)?
C
C    a j penny                    dao          1988-05-29

      subroutine sc_dosel ( k, x, y, doit )

      implicit none
      include 'scrut.inc'

      integer	k		!i: If by list posn, this is list position
      real	x		!i: If by XY posn, this is X position
      real	y		!i: If by XY posn, this is Y position
      logical	doit		!o: If to be used,=>true.
C--
Cbegin


      doit = .false.
      if ( BYLIST ) then
         if ( k.le.max(LPOS(1),LPOS(2)) .and.
     +        k.ge.min(LPOS(1),LPOS(2)) ) doit = .true.
      else
         if ( x.le.max(LAREA(1,1),LAREA(2,1)) .and.
     +        x.ge.min(LAREA(1,1),LAREA(2,1)) .and.
     +        y.le.max(LAREA(1,2),LAREA(2,2)) .and.
     +        y.ge.min(LAREA(1,2),LAREA(2,2)) ) doit = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_ARCLEARR -- Make an array of a section the original image with
C               the fitted stars subtracted, and maybe the fitted
C               slope subtracted. The area is round a designated star
C
C   a.j.penny                   rgo                    83-2-22

      subroutine sc_arclearr ( im, data, kstar, imres, lxi, lyi )

      implicit none

      include 'scrut.inc'
      include 'ST_IMAGE_INC'

      real      im(NX,NY)		!i: Image
      real	data(TBVXS,TBYS)	!i: Star list
      integer	kstar			!i: Star to make array round
      integer   lxi			!i: X size of 'removed' array
      integer   lyi			!i: Y size of 'removed' array
      real      imres(lxi,lyi)		!o: 'Removed' array
C--
      integer smax
      parameter (smax=15)
      integer mapnum(smax), magnif(smax), mapx(smax), mapy(smax)
      real gx(smax), gy(smax), gx2(smax), gy2(smax), p(smax), hx2(smax),
     +     hy2(smax), co(smax), si(smax), sim(smax), qh(smax), qr(smax),
     +     qp(smax), xp(smax), yp(smax), hp(smax)

      integer k, kk, kxs, kxe, kys, kye, jx, jy, nst
      real val, resid, rv
Cbegin


      nst = data(21+5,kstar) + 1					!Set up fit parameters
      do k = 1, nst
         if ( k.eq.1 ) then
            kk = kstar
         else
            if (k.le.8) kk = nint(data((20+k+5),kstar))
            if (k.gt.8) kk = nint(data((40+k-8+5),kstar))
         endif
         if ( data(9+5,kk).gt.0.0 ) then
            xp(k) = data(1+5,kk)
            yp(k) = data(2+5,kk)
            hp(k) = data(9+5,kk)
            gx(k) = 1.0/data(12+5,kk)
            gy(k) = 1.0/data(13+5,kk)
            gx2(k) = gx(k)*gx(k)
            gy2(k) = gy(k)*gy(k)
            p(k) = data(14+5,kk)
            hx2(k) = 1.0/(data(15+5,kk)*data(15+5,kk))
            hy2(k) = 1.0/(data(16+5,kk)*data(16+5,kk))
            co(k) = cos(data(17+5,kk))
            si(k) = sin(data(17+5,kk))
            sim(k) = -1.0*si(k)
            qh(k) = data(18+5,kk)
            qr(k) = 1.0/data(19+5,kk)
            qp(k) = data(20+5,kk)
            mapnum(k) = nint(data(11+5,kk))
            mapx(k) = nint(data(38+5,kk))
            mapy(k) = nint(data(39+5,kk))
            magnif(k) = nint(data(40+5,kk))
         endif
      enddo

      kxs = xp(1) - lxi/2						!Calculate the box around the wanted star
      kxs = min ( NX, max(1,kxs) )
      kxe = min ( NX, (kxs+LX-1) )
      kys = yp(1) - lyi/2
      kys = min ( NY, max(1,kys) )
      kye = min ( NY, (kys+LY-1) )

      LXS = kxs								!Return blh corner coords
      LYS = kys

      if ( kxe.gt.kxs .and. kye.gt.kys ) then				!Calculate the residuals between the fit and the data
         do jy = kys, kye
            do jx = kxs, kxe
               if ( im(jx,jy).ne.RINVAL ) then

                  val = 0.0
                  do k = 1, nst
                     if ( hp(k).ne.0.0 ) call profval (
     +                  rv, jx, jy, xp(k), yp(k), hp(k), lx, ly, co(k),
     +                  si(k), sim(k), gx2(k), gy2(k), p(k), hx2(k),
     +                  hy2(k), qh(k), qr(k), qp(k), .true.,
     +                  %val(IPP), MX, MY, MZ, mapnum(k), mapx(k),
     +                  mapy(k), magnif(k) )
                     val = val + rv
                  enddo

                  if ( DOSLOPE ) val = val + 				!Apply slope
     +                                 data(32+5,kstar)*(jx-kxs+1) +
     +                                 data(33+5,kstar)*(jy-kys+1)

                  resid = im(jx,jy) - val/BS
               else
                  resid = RINVAL
               endif
               imres((jx-kxs+1),(jy-kys+1)) = resid
            enddo
         enddo
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_ARCLEARS -- Make an array of a section the original image with
C               the fitted stars subtracted, and maybe the fitted
C               slope subtracted. The area is round a designated star
C
C   a.j.penny                   rgo                    83-2-22

      subroutine sc_arclears ( im, data, kstar, imres, lxi, lyi )

      implicit none

      include 'scrut.inc'
      include 'ST_IMAGE_INC'

      integer*2	im(NX,NY)		!i: Image
      real	data(TBVXS,TBYS)	!i: Star list
      integer	kstar			!i: Star to make array round
      integer   lxi			!i: X size of 'removed' array
      integer   lyi			!i: Y size of 'removed' array
      integer*2	imres(lxi,lyi)		!o: 'Removed' array
C--
      integer smax
      parameter (smax=15)
      integer mapnum(smax), magnif(smax), mapx(smax), mapy(smax)
      real gx(smax), gy(smax), gx2(smax), gy2(smax), p(smax), hx2(smax),
     +     hy2(smax), co(smax), si(smax), sim(smax), qh(smax), qr(smax),
     +     qp(smax), xp(smax), yp(smax), hp(smax)

      integer k, kk, kxs, kxe, kys, kye, jx, jy, nst, kresid
      real val, rv
Cbegin


      nst = data(21+5,kstar) + 1					!Set up fit parameters
      do k = 1, nst
         if ( k.eq.1 ) then
            kk = kstar
         else
            if (k.le.8) kk = nint(data((20+k+5),kstar))
            if (k.gt.8) kk = nint(data((40+k-8+5),kstar))
         endif
         if ( data(9+5,kk).gt.0.0 ) then
            xp(k) = data(1+5,kk)
            yp(k) = data(2+5,kk)
            hp(k) = data(9+5,kk)
            gx(k) = 1.0/data(12+5,kk)
            gy(k) = 1.0/data(13+5,kk)
            gx2(k) = gx(k)*gx(k)
            gy2(k) = gy(k)*gy(k)
            p(k) = data(14+5,kk)
            hx2(k) = 1.0/(data(15+5,kk)*data(15+5,kk))
            hy2(k) = 1.0/(data(16+5,kk)*data(16+5,kk))
            co(k) = cos(data(17+5,kk))
            si(k) = sin(data(17+5,kk))
            sim(k) = -1.0*si(k)
            qh(k) = data(18+5,kk)
            qr(k) = 1.0/data(19+5,kk)
            qp(k) = data(20+5,kk)
            mapnum(k) = nint(data(11+5,kk))
            mapx(k) = nint(data(38+5,kk))
            mapy(k) = nint(data(39+5,kk))
            magnif(k) = nint(data(40+5,kk))
         endif
      enddo

      kxs = xp(1) - lxi/2						!Calculate the box around the wanted star
      kxs = min ( NX, max(1,kxs) )
      kxe = min ( NX, (kxs+LX-1) )
      kys = yp(1) - lyi/2
      kys = min ( NY, max(1,kys) )
      kye = min ( NY, (kys+LY-1) )

      LXS = kxs								!Return blh corner coords
      LYS = kys

      if ( kxe.gt.kxs .and. kye.gt.kys ) then				!Calculate the residuals between the fit and the data
         do jy = kys, kye
            do jx = kxs, kxe
               if ( im(jx,jy).ne.INVAL ) then

                  val = 0.0
                  do k = 1, nst
                     if ( hp(k).ne.0.0 ) call profval (
     +                  rv, jx, jy, xp(k), yp(k), hp(k), lx, ly, co(k),
     +                  si(k), sim(k), gx2(k), gy2(k), p(k), hx2(k),
     +                  hy2(k), qh(k), qr(k), qp(k), .true.,
     +                  %val(IPP), MX, MY, MZ, mapnum(k), mapx(k),
     +                  mapy(k), magnif(k) )
                     val = val + rv
                  enddo

                  if ( DOSLOPE ) val = val + 				!Apply slope
     +                                 data(32+5,kstar)*(jx-kxs+1) +
     +                                 data(33+5,kstar)*(jy-kys+1)

                  kresid = im(jx,jy) - val/BS
               else
                  kresid = INVAL
               endif
               imres((jx-kxs+1),(jy-kys+1)) = kresid
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DISIMG -- Display image
C
C   a j penny                 dao           1988-04-25

      subroutine sc_disimg ( )

      implicit none

      include 'interact.inc'
      include 'ST_IMAGE_INC'
C--
Cbegin


      if ( .not.GOTIMAGE ) then					!Check if image there
         call printo ( 'ERROR: No image' )
         return
      endif
      if ( .not.OPDISP ) then
         call printo ( 'ERROR: Display not open' )
         return
      endif

      call ds_disp ( %val(IPIM), NX, NY, IMTYPE, 2 )

      DISPLAYED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_LWRITE -- Writes to file star and extra list
C
C a j penny             stsci                  1987-03-22

      subroutine sc_lwrite ( data )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'

      real	data(TBVXS,TBYS)		!i: star list
C--
      logical removed, ismore
      integer j, k, ktot, kout, ipxyo, istat
      character pref*72, text*72
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST .or. TBYS.EQ.0 ) then				!Check star list opened
         call printo ( 'ERROR: No input list' )
         return
      endif

      ismore = .false.							!See if any removed from star list and any added
      ktot = TBYS
      if ( NEXTRA.ne.0 ) then
         do k = 1, TBYS
            removed = .false.
            do j = 1, NEXTRA
               if ( nint(EXTRA(1,j)).eq.k .and. nint(EXTRA(3,j)).eq.2 )
     +              removed = .true.
            enddo
            if ( removed ) ktot = ktot - 1
         enddo
         do k = 1, NEXTRA
            if ( nint(EXTRA(3,k)).eq.1 ) then
               ktot = ktot + 1
               ismore = .true.
            endif
         enddo
      endif

      if ( ktot.eq.0 ) then						!Check any to output
         call printo ( 'No output' )
         return
      endif

      call optabw ( 'OUTSTARS', ipxyo, TBVXS, ktot, .true., istat )	!Open output file
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         call printo ( 'No file written' )
         return
      endif

      call tcopdes ( 'INSTARS', 'OUTSTARS', istat )			!Transfer headers
      call get1c  ( 'TITLE', text, 'Stars from Interact:Scrutiny',
     +              .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUTSTARS', 'TITLE', text )

      if ( ismore ) then						!Get whether to add prefixes to extra output star names
         call get1b ( 'DOPREF', DOPREF, DOPREF )
         if ( ST_FAILED ) return
         if ( .not.DOPREF ) then
            call printo ( 'No prefix - output reidentified' )
         else
            call get1c ( 'PREFIX', pref, 'A', .true. )
            if ( ST_FAILED ) return
         endif
      endif

      call azeror ( %val(ipxyo), TBVXS*ktot )				!Clear output file

      kout = 0								!Load stars to output
      do k = 1, TBYS
         removed = .false.
         if ( NEXTRA.gt.0 ) then
            do j = 1, NEXTRA
               if ( nint(EXTRA(1,j)).eq.k .and. nint(EXTRA(3,j)).eq.2 )
     +            removed = .true.
            enddo
         endif
         if ( .not.removed ) then
            kout = kout + 1
            call coprr ( data, TBVXS, TBYS, 1, TBVXS, k, k,
     +                   %val(ipxyo), TBVXS, ktot, 1, kout )
         endif
      enddo

      if ( NEXTRA.ne.0 ) then						!Add extra stars
         do k = 1, NEXTRA
            if ( nint(EXTRA(3,k)).eq.1 ) then
               kout = kout + 1
               call coprr ( EXTRA, NXEE, NYEE, 1, 2, k, k,
     +                      %val(ipxyo), TBVXS, ktot, 6, kout )
               if ( TBVXS.gt.7 ) call cop1r ( 50.0,
     +                              %val(ipxyo), TBVXS, ktot, 8, kout )
               call sc_xident ( %val(ipxyo), TBVXS, ktot, kout, k, pref)
            endif
         enddo
      endif

      if ( .not.DOPREF .and. ismore ) call ident ( %val(ipxyo), 	!If not adding suffixes, then
     +                                             TBVXS, ktot )	! renumber all stars if extra ones added

      call canpar ( 'OUTSTARS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DOTYPE -- Type or print to file the input star measures
C
C     a j penny                    dao                  1988-05-29

      subroutine sc_dotype ( data, kopt )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'

      real	data(TBVXS,TBYS)	!i: star list
      integer	kopt			!i: =1 => print; =2 => type
C--
      real adata(9)
      character title*30, name*36, file*40, text*72
      integer lu, k, iv, istat

      real trunc
      external trunc
Cbegin


      if ( .not.GOTLIST .or. TBYS.eq.0 ) then				!Check star list opened
         call printo ( 'ERROR: No input list' )
         return
      endif

      call get1b ( 'DOFORM', DOFORM, DOFORM )
      if ( ST_FAILED ) return
      if ( DOFORM ) then
         call sc_gtform
      else
         call get1b ( 'LISTXY', DOXY, DOXY )
         if ( ST_FAILED ) return
      endif

      call gtdesc ( 'INSTARS', 'TITLE', title, ' ', iv, istat )

      if ( kopt.eq.1 ) then						!Open output file, if wanted
         call get1c ( 'OUTFILE', name, 'PRMEASURE', .true. )
         if ( ST_FAILED ) return
         file = name//'.lis'
         lu = 1
         open ( unit=lu, file=file, status='NEW', form='FORMATTED' )
         write ( lu, '( /// ,27x,''Title: '',a,//)') title
      endif

      if ( TBVXS.ge.25 ) then						!Type Profile if there is one
         call amovr (  data(11+1+5,1), adata, 9 )
         adata(6) = adata(6)*180.0/3.14159
         do k = 1, 9
            adata(k) = trunc(adata(k),3)
         enddo
         if ( kopt.eq.2 ) then
            call printo ( ' ' )
            call printo ( 'Profile of first star is :- ' )
            write ( text, '(1x,9f7.3)' ) (adata(k),k=1,9)
            call printo ( text )
            call printo ( ' ' )
         else
            write ( lu, '(''  Profile of first star is :-'')' )
            write ( lu, '(1x,9f7.3)' ) (adata(k),k=1,9)
            write ( lu, '(2x)' )
         endif
      endif

      if ( DOFORM ) then						!Do the work
         call sc_adoform ( data, lu, kopt )
      else
         call sc_prfile ( data, lu, kopt )
      endif

      if ( kopt.eq.1 ) close ( unit=lu )				!Close output file if one


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_ADOFORM -- Puts out lines of formatted text to screen and/or file
C
C   a j penny                stsci               1987-03-21

      subroutine sc_adoform ( data, lu, kopt )

      implicit none

      include 'scrut.inc'

      real	data(TBVXS,TBYS)	!i: star list
      integer	lu			!i: Output file unit no
      integer	kopt			!i: 1 => print; 2=>type
C--
      character atext*79, btext*131
      logical doit
      real x, y
      integer j, k, klen
Cbegin


      call sc_wrthead ( atext, btext, klen )
      if ( klen.le.79 ) call printo ( atext )
      if ( klen.gt.79 ) call printo ( btext )

      do k = 1, TBYS
         x = data(6,k)
         y = data(7,k)
         call sc_dosel ( k, x, y, doit )
         if ( doit ) then
            call sc_textline ( data, k, atext, btext, klen )
            if ( kopt.eq.2 .and. klen.le.79 ) call printo ( atext )
            if ( kopt.eq.2 .and. klen.gt.79 ) call printo ( btext )
            if ( kopt.eq.1 .and. klen.le.79 ) write
     +                       ( lu, '(1x,79a1)' ) (atext(j:j),j=1,klen)
            if ( kopt.eq.1 .and. klen.gt.79 ) write
     +                      ( lu, '(1x,131a1)' ) (btext(j:j),j=1,klen)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_GTFORM -- Gets format of output line; - Number of characters
C              in star name, which columns to output, the number
C              of decimal places before and after decimal point in
C              the numbers in each column.
C
C  a j penny              stsci            1987-03-21

      subroutine sc_gtform()

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'
C--
      logical loop
      integer length, k, numt
Cbegin


      call get1i ( 'NUMCHAR', NUMID, 12, 1, 20 )			!Get number of characters of Identifier to output
      if ( ST_FAILED ) return
      length = 6 + NUMID

      numt = 0
      k = 0
      loop = .true.
      do while ( loop )

         k = k  + 1
         numt = numt + 1
         call get1i ( 'NUMCOL', numt, numt, 0, TBXS )
         if ( ST_FAILED ) return
         if ( numt.lt.1 ) then
            loop = .false.
         else
            NUMCOL(k) = numt
            call get1i ( 'NUMBEF', NUMBEF(k), 4, 1, 130 )
            call get1i ( 'NUMAFT', NUMAFT(k), 2, 0, 130 )
            if ( ST_FAILED ) return
            length = length + 2 + NUMBEF(k) + NUMAFT(k)
            if ( NUMAFT(k).eq.0 ) length = length - 1
            if ( length.gt.130 ) then
               call printo ( 'ERROR: Too long - wont take this last' )
               loop = .false.
            endif
         endif

      enddo

      NUMCOLTOT = k - 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_TEXTLINE -- Makes a line of text from a line of data
C
C a j penny                stsci                 1987-03-22

      subroutine sc_textline ( data, kline, atext, btext, kl )

      implicit none

      include 'scrut.inc'

      real      	data(TBVXS,TBYS)	!i: Star list
      integer		kline			!i: Star number to output
      character*79	atext			!o: Text string (used if
						!                length<=79)
      character*131	btext			!o: Text string (used if
						!                length<79)
      integer		kl			!o: Length of text string
C--
      character id*20, forinb*11
      character*200 texta, texto, textb
      character*7 forcha(100), forina
      integer maxitm
      parameter (maxitm=100)			!max number of items to be printed
      real adata(maxitm-5)
      integer k, ka, knc
      real trunc
      external trunc
Cbegin


      forcha(1) = '(1x,'						!Load Format statements
      forcha(2) = '20a1,'
      do k = 1, NUMCOLTOT
         ka = NUMBEF(k) + NUMAFT(k) + 2
         write ( forina, '(''f'',i2,''.'',i2,'','')' ) ka, NUMAFT(k)
         forcha(k+2) = forina
      enddo
      forcha(NUMCOLTOT+2)(7:7) = ')'

      call namegt ( data, TBVXS, TBYS, kline, id )			!Get star name

      do k = 1, NUMCOLTOT						!Get and order data, and check and
         knc = NUMCOL(k) + 5						! change numbers too big to fit in.
         adata(k) = trunc ( data(knc,kline), NUMBEF(k) )
      enddo

      write ( texta, forcha) (id(k:k),k=1,20), (adata(k),k=1,NUMCOLTOT)	!Type and store a line of information
      call sc_lntidy ( texta, 200, NUMID, NUMAFT, 1, textb, kl )
      write ( forinb, '(''(1h ,'',i3.3,''a1)'')' ) kl
      write ( texto, forinb ) (textb(k:k),k=1,kl)

      if ( kl.le.79 ) write ( atext, '(79a1)' ) (texto(k:k),k=1,kl)
      if ( kl.gt.79 ) write ( btext, '(131a1)' ) (texto(k:k),k=1,kl)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_WRTHEAD -- Write a text line of headers
C
C  a j penny               stsci                  1987-03-21


      subroutine sc_wrthead ( atext, btext, klena )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'

      character*79	atext		!o: Text string(used if length<=79)
      character*131	btext		!o: Text string (used if length>79)
      integer		klena		!o: Text string length
C--
      integer kcc(100)
      character*13 forcha(100), forina
      character hvala*20, texta*200, textb*200, texto*200
      character*20 hval(100)
      integer k, ka, kb, kc, ke, j, klen, numcolt, istat
Cbegin


      if ( ST_FAILED ) return

      forcha(1) = '(5hIdent,'						!Load Header Formats
      forcha(2) = '8hifier   ,'
      if ( NUMID.lt.12 ) then
         forcha(1) = '(5hName ,'
         forcha(2) = '8h        ,'
      endif
      forcha(3) = '7h       ,'
      do k = 1, NUMCOLTOT
         numcolt = NUMCOL(k)
         kc = NUMBEF(k) + NUMAFT(k) + 1
         if ( NUMAFT(k).eq.0 ) kc = kc - 1
         call gthead ( 'INSTARS', numcolt, hvala, istat )
         if ( ST_FAILED ) return
         call charln ( hvala, klen )
         do j = 1, klen
           hval(k)(j:j) = hvala(j:j)
         enddo
         klen = max(1,klen)
         kc = min(kc,klen)
         kcc(k) = kc
         kb = NUMBEF(k) - kc + 1
         kb = max(kb,1)
         ke = NUMBEF(k) + NUMAFT(k) + 2 - kb - kc
         if ( NUMAFT(k).eq.0 ) ke = ke - 1
         if ( ke.gt.0 ) then
            write ( forina, '(i2,''x,'',i2,''a1,'',i2,''x,'')' )kb,kc,ke
         else
            write ( forina, '(i2,''x,'',i2,''a1,'')' ) kb, kc
         endif
         forcha(k+3) = forina
      enddo
      ka = NUMCOLTOT + 3
      forcha(ka)(13:13) = ')'

      write ( texta, forcha ) ((hval(k)(j:j),j=1,kcc(k)),k=1,NUMCOLTOT)	!Write Header
      call sc_lntidy ( texta, 200, NUMID, NUMAFT, 0, textb, klena )
      write ( forina, '(''(1h ,'',i3.3,''a1)'')' ) klena
      write ( texto, forina ) (textb(k:k),k=1,klena)

      if ( klena.le.78 ) then
         write ( atext, '(79a1)' ) (texto(k:k),k=1,klena+1)
      else
         write ( btext, '(131a1)' ) (texto(k:k),k=1,klena+1)
      endif
      klena = klena + 2


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DOFULL -- Types out all the details on a star
C
C a j penny                 stsci                 1987-03-22

      subroutine sc_dofull ( data )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'

      real	data(TBVXS,TBYS)		!i: Star list
C--
      character name*20, text*72
      integer j, k, lnum, kget, kd
      real    rname(5)
      integer maxitm
      parameter (maxitm=100)		! max no of items to deal with
      real ldata(maxitm)
      real trunc
      external trunc

CX      integer klen
CX      character*20 hval(maxitm)
CX      character*60 hvala

      character*72 htext(11)
      data htext /
     +'       X            Y           Mag             Dx           Dy',
     +'Iterations         Rms      No invalid        Height       Base',
     +'Mapnumber      X Radius      Y Radius        Power    X Pow rad',
     +'Y Pow Y           Angle     Wing ratio   Wing radius Wing power',
     +'No comps        Comp 1        Comp 2         Comp 3      Comp 4',
     +'  Comp 5       Comp 6        Comp 7        X box         Y box ',
     +'Comp import    X slope      Y slope         Flag 1       Flag 2',
     +'Prof Vol     Order fitted  Map X size   Map Y size   Map Magnif',
     +'  Comp 8         Comp 9       Comp 10      Comp 11      Comp 12',
     +'  Comp 13       Comp 14    Diff iter -2   Guess height    Error',
     +'  Damp                                                       ' /
      character*68 thelp(2)
      data thelp /
     + 'Choose wether to identify wanted star in -Option/full- choice',
     + 'by NAME or NUMBER.' /
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST ) then						!Check a list has been opened
         call printo ( 'ERROR: No List' )
         return
      endif

      call get_job ( 'SEEK', 'number:name', kget, 1, thelp, 2 )		!Get wether to find star by
      if ( ST_FAILED ) return
      if ( kget.eq.1 ) then						! number in list or by name.
         call get1i ( 'NUMBER', lnum, 1, 1, TBYS )			! If later, check in list
         if ( ST_FAILED ) return
         call namegt ( data, TBVXS, TBYS, lnum, name )
      else
         call get1c ( 'NAME', name, ' ', .true. )
         if ( ST_FAILED ) return
         call nametr ( name, rname )
         k = 0
         kd = 1
         do while ( kd.eq.1 .and. k.lt.TBYS )
            k = k + 1
            call namechr ( data(1,k), rname, kd )
         enddo
         lnum = k
         if ( kd.eq.1 ) then
            call printo ( 'ERROR: No such star' )
            return
         endif
      endif

      do k = 1, min(TBXS,maxitm)					!Copy line into data buffer and
         ldata(k) = trunc ( data(k+5,lnum), 8 )				! modify numbers too large to print
      enddo

CX      do k = 1, TBXS							!Get the headers
CX         call gthead ( 'INSTARS', k, hvala, istat )
CX         call charln ( hvala, klen )
CX         hval(k)(1:) = hvala(1:min(klen,20))
CX      enddo

      write ( text, '(1h ,''Star number = '',i6,''  Name = '',a20)')	!Type the information
     +      lnum, name
      call printo ( text )
      do k = 1, 1+((TBXS-1)/5)
         call printo ( htext(k) )
         write ( text, '(1x,5f12.3)' )
     +           (ldata(j), j = 1+5*(k-1), min(TBVXS,5*k) )
         call printo ( text )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_LNTIDY -- Removes the unwanted Identifier characters and if
C              flagged, removes any unwanted decimal points (which are
C              known by having no numbers after them).
C
C   a.j.penny                   rgo                    83-2-26

      subroutine sc_lntidy ( texta, n, numid, numaft, kw, textb, klen )

      implicit none

      character*(*)	texta		!i: text to tidy
      integer		n		!i: length of input text
      integer		numid		!i: no of chars wanted in name
      integer		numaft(100)	!i: no of chars after each dec point
      integer		kw		!i: 1 => remove dec points of ints
      character*(*)	textb		!o: tidied text
      integer		klen		!o: length of tidied text
C--
      integer k, kk, j, ja
Cbegin


      do k = 1, n							!Load input text
         textb(k:k) = texta(k:k)
      enddo

      if ( kw.eq.1 ) then						!Strip redundant decimals, if wanted
         kk = 0
         do k = 21, n
            if ( kk.lt.100 .and. textb(k:k).eq.'.' ) then
               kk = kk + 1
               if ( numaft(kk).eq.0 ) then
                  do j = k, n-1
                     ja = j + 1
                     textb(j:j) = textb(ja:ja)
                  enddo
                  textb(n:n) = ' '
               endif
            endif
         enddo
      endif

      if ( numid.ne.20 ) then						!Strip unwanted Identifier characters
         kk = 20 - numid
         do j = numid+1, n-kk
            ja = j + kk
            textb(j:j) = textb(ja:ja)
         enddo
         do j = n-kk+1, n
            textb(j:j) = ' '
         enddo
      endif

      call charln ( textb, klen )					!Find length


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_PRFILE -- type out data
C
C  a j penny                      dao        82-6-21

      subroutine sc_prfile ( data, lu, kopt )

      implicit none

      include 'scrut.inc'

      real      data(TBVXS,TBYS)	!i: Star list
      integer	lu			!i: Unit to write file to
      integer	kopt			!i: Print option (1=>file;2=>user)
C--
      real adata(100), x, y, ax, ay, aimp, adiffm2, amag,arms,
     +     adx, ady, ah, adamp
      integer k, iter, nstar, ninval
      logical doit
      character texta*80, textb*80, textc*80
      real trunc
      external trunc
Cbegin


      texta = ' '//' Star'//'   Mag  '//' Height '//'  Dx  '//'  Dy  '//
     +         '  Rms '//' Its '//'Inval'//' Comps'//' Imp '//
     +         ' Diff - 2'//'  Damp'
      textb = ' '//' Star'//'   X   '//'   Y   '//'   Mag  '//
     +         ' Height '//'  Dx  '//'  Dy  '//'  Rms '//' Its '//
     +         'Inval'//' Imp'


      textc = texta							!Output heading
      if ( DOXY ) textc = textb
      if ( kopt.eq.1 ) write ( lu, '(a)' ) textc
      if ( kopt.eq.2 ) call printo ( textc )

      do k = 1, TBYS							!Scan through list entries

         call amovr ( data(6,k), adata, TBXS )				!Load data

         x = adata(1)							!Select if to be output
         y = adata(2)
         call sc_dosel ( k, x, y, doit )

         if ( doit ) then

            ax      = trunc(adata(1),4)					!Type and store a line of information
            ay      = trunc(adata(2),4)
            amag    = trunc(adata(3),2)
            adx     = trunc(adata(4),3)
            ady     = trunc(adata(5),3)
            iter    =  nint(adata(6))
            arms    = trunc(adata(7),4)
            ninval  =  nint(adata(8))
            ah      = trunc(adata(9),5)
            nstar   =  nint(adata(21))
            aimp    = trunc(adata(31),2)
            adiffm2 = trunc(adata(48),2)
            adamp   = trunc(adata(51),2)
            if ( DOXY ) then
               write ( textc, '(1h ,i5,2f7.1,f7.2,f8.1,2f6.1,f7.1,i4,
     +                 i5,f6.2)' ) k, ax, ay, amag, ah, adx, ady,
     +                             arms, iter, ninval, aimp
            else
               write ( textc, '(1h ,i5,f7.2,f8.1,2f6.1,f7.1,i4,i5,i6,
     +                         f6.2,f8.4,f7.3)' ) k, amag, ah, adx, ady,
     +                         arms, iter, ninval, nstar, aimp, adiffm2,
     +                         adamp
            endif
            if ( kopt.eq.1 ) write ( lu, '(1x,a80)' ) textc
            if ( kopt.eq.2 ) call printo ( textc )

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_XIDENT -- Loads an table row with an identifier
C              of form #'pref'k, where 'pref' is a one character
C              and k is an integer.
C
C      A.J.Penny                                        82-7-7

      subroutine sc_xident ( data, tbx, tby, kp, k, pref )

      implicit none

      integer	tbx			!i: No of cols in star list
      integer	tby			!i: No of rows in star list
      byte	data(tbx*4,tby)		!o: Star list
      integer	kp			!i: Row in table to insert name
      integer	k			!i: Number to put in name
      character*(*) pref		!i: 1st char of this string added
C--
      character id*20
      integer j
Cbegin


      write ( id, '(i20)' ) k
      id(1:1) = '#'
      id(2:2) = pref(1:1)
      call lbgone ( id(3:) )
      do j = 1, 20
         data(j,kp) = ichar(id(j:j))
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_OPDISP -- Open display
C
C alan penny                    ral                  1990-06-16

      subroutine sc_opdisp ( ierr )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( OPDISP ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) return

      call ds_init ( IMTITLE, 0, ierr )					!Open display
      if ( ierr.ne.0 ) return
      OPDISP = .true.

      call ds_gtcomf ( 1 )						!Get image display size compression


      end



C  This is SCRUTSUBG.FOR
C
C  Contains:-
C
C SC_LPAINT    Types companion star posns, paints up star + companions
C SC_DORING    Paints a rectangle round the removed area
C SC_BPAINT    Paints up all stars in list in their old + new posns
C SC_JPAINT    Takes a star number. Searches star list for next star
C SC_DOPAINT   Take a star from star list, paint its position, etc
C SC_GNEAREST  Get cursor posn and nearest list star
C SC_CREM      Removes stars from the list by the image cursor
C SC_CADD      Adds and removes stars from the list by the image cursor



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_LPAINT -- Types companion star posns, paints up star + companions
C
C      a j penny               dao              1988-05-29

      subroutine sc_lpaint ( data )

      implicit none

      include 'scrut.inc'
      include 'interact.inc'
      include 'STARMAN_INC'

      real      data(TBVXS,TBYS)		!i: Star list
C--
      logical ok
      integer k, ka
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST .or. TBYS.eq.0 ) then				!Check image and list
         call printo ( 'ERROR: No MEASURE list of magnitudes' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      endif

      ok = .false.							!Check some to display.
      do k = 1, TBYS							! If not return
         if ( data(26,k).gt.0.5 ) ok = .true.
      enddo
      if ( .not.ok ) then
         call printo ( 'ERROR: No stars with companions' )
         return
      endif

      k = 1								!Choose star and do type,
      ka = 1								! mark
      do while ( k.ge.1 )
         if ( ka.ne.1 ) k = k + 1
         ka = 2
         if ( k.gt.TBYS ) k = 1
         if ( data(26,k).gt.0.5 ) then
            call get1i ( 'NUMBER', k, k, 0, TBYS )
            if ( ST_FAILED ) return
            if ( k.ge.1 ) call sc_dopaint ( data, k )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DORING -- Paints a rectangle round the removed area
C
C  a j penny                    stsci            1987-03-21

      subroutine sc_doring ( )

      implicit none

      include 'scrut.inc'
      include 'interact.inc'
      include 'ST_IMAGE_INC'
C--
      real xs, xe, ys, ye
Cbegin


      if ( .not.GOTIMAGE ) then						!Check set up
         call printo ( 'ERROR: No image' )
         return
      elseif ( .not.OPDISP ) then
         call printo ( 'ERROR: No image displayed' )
         return
      elseif ( .not.DREMOVE ) then
         call printo ( 'ERROR: No area removed' )
         return
      endif

      xs = LXS - 1 - 1
      ys = LYS - 1 - 1
      xe = xs + LX - 1 + 2
      ye = ys + LY - 1 + 2
      xs = max( 1.0, min(real(NX),xs) )
      ys = max( 1.0, min(real(NY),ys) )
      xe = max( 1.0, min(real(NX),xe) )
      ye = max( 1.0, min(real(NY),ye) )

      call ds_box ( xs, xe, ys, ye, 6 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_BPAINT -- paints up all stars in list in their old + new posns
C    Marks good and failed new positions.
C
C a j penny                       stsci                 1987-03-22


      subroutine sc_bpaint ( data, kopt )
      implicit none

      include 'scrut.inc'
      include 'interact.inc'
      include 'STARMAN_INC'

      real      data(TBVXS,TBYS)		!i: Star list
      integer   kopt				!i: Option (1=new:2=old)
C--
      character*1000 topt
      data topt /
     +    'red:green:blue:cyan:magenta:yellow:coral:palegreen' /
      integer nthelp, kt(2)
      parameter ( nthelp=2 )
      character*68 thelp(nthelp)
      data thelp /
     +  'Colour of the crosses to display.' ,
     +'Choice is:- red:green:blue:cyan:magenta:yellow:coral:palegreen' /

      real x, y, alen
      logical doit, dospot
      integer k, kc
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST ) then						!Check set up
         call printo ( 'ERROR: No MEASURE list of magnitudes' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      endif

      call get1b ( 'DOSPOT', dospot, .false. )				!Get mark type
      if ( .not.dospot ) call get1r ( 'LENGTH', alen, 2.0, 0.5, 100.0 )
      if ( ST_FAILED ) return

      call get_job ( 'COL_OK', topt, kt(1), 1, thelp, nthelp )		!Get colours
      if ( ST_FAILED ) return
      call get_job ( 'COL_BAD', topt, kt(2), 2, thelp, nthelp )
      if ( ST_FAILED ) return

      call ds_scol ( 1.0, kt(1) )

      if ( kt(1).ne.9 .or. kt(2).ne.9 ) then				!Mark positions

         do k = 1, TBYS

            if ( kopt.eq.1 ) then
               x = data(6,k)
               y = data(7,k)
            else
               x = data(6,k) - data(9,k)
               y = data(7,k) - data(10,k)
            endif

            kc = kt(1)
            if ( TBVXS.ge.8 ) then
               if ( data(8,k).gt.49.0 ) kc = kt(2)
            endif

            call sc_dosel ( k, x, y, doit )
            if ( doit .and. dospot ) then
               call ds_spot ( x, y, kc )
            elseif ( doit .and. .not.dospot ) then
               call ds_cross ( x, y, alen, kc )
            endif

         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_JPAINT -- Takes a star number. Searches star list for next star
C              (including itself) to have companions distant from star.
C              Types details and paints up star and its companions.
C
C a j penny                   stsci                  1987-03-22

      subroutine sc_jpaint ( data )

      implicit none

      include 'scrut.inc'
      include 'interact.inc'
      include 'STARMAN_INC'

      real      data(TBVXS,TBYS)		!i: Star list
C--
      logical loop, found
      integer k, ka, kgo, j, ja
      real xl, yl, xm, ym, xma, yma, xd, yd
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST .or. TBYS.lt.1 ) then				!Check image and list there
         call printo ( 'ERROR: No MEASURE list of magnitudes' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      endif

      kgo = 0								!Write
      loop = .true.
      ka = TBYS
      do while ( loop )
         kgo = kgo + 1
         ka = ka + 1
         if ( ka.gt.TBYS ) ka = 1
         call get1i ( 'NUMBER', ka, ka, 0, TBYS )
         if ( ST_FAILED ) return
         if ( ka.le.0 ) then
            loop = .false.
         else
            found = .false.
            k = ka - 1
            do while ( k.lt.TBYS .and. .not.found )
               k = k + 1
               if ( data(26,k).gt.0.5 ) then
                  xl = nint(data(29+5,k))
                  yl = nint(data(30+5,k))
                  xm = data(6,k) - data(9,k)
                  ym = data(7,k) - data(10,k)
                  do j = 1, nint(data(26,k))
                     if ( j.le.7 ) ja = nint(data(5+21+j,k))
                     if ( j.gt.7 ) ja = nint(data(5+41+j-8,k))
                     if ( ja.lt.1 .or. ja.gt.TBYS ) then
                        call pargi ( j )
                        call pargi ( ja )
                        call printd (
     +        'Companion No %d is List number %d - Input file suspect' )
                        if ( ja.gt.TBYS ) then
                           call pargi ( TBYS )
                           call printd ( '   - List has only %d stars' )
                        endif
                     else
                        xma = data(6,ja) - data(9,ja)
                        yma = data(7,ja) - data(10,ja)
                        xd = xma - xm
                        yd = yma - ym
                        if ( abs(xd).gt.xl .or. abs(yd).gt.yl )
     +                     found = .true.
                     endif
                  enddo
               endif
            enddo
            ka = k

            if ( k.eq.TBYS .and. .not.found ) call printo ( 'No stars'//
     +              ' with distant companions in the rest of the list' )

            if ( found ) call sc_dopaint ( data, k )

         endif

      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_DOPAINT -- Take a star from star list, paint its position, etc
C  Also paint the measuring box round it, its companion stars, and
C  draw lines to any companion stars outside box.
C
C    a j penny                    dao      1988-05-29

      subroutine sc_dopaint ( data, k )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real	data(TBVXS,TBYS)	!i: star list
      integer	k			!i: Star in list to access
C--
      real x, y, xo, yo, xd, yd, rr, rl, offx, offy, tcos, tsin,
     +     xs, xe, ys, ye, am, ht, alen
      integer klx, kly, j, ja, nst, kc
      logical dospot, donew
      character text*72

      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'DOSPOT', dospot, .false. )				!Get mark type
      if ( .not.dospot ) call get1r ( 'LENGTH', alen, 2.0, 0.5, 100.0 )
      call get1b ( 'DONEW', donew, .true. )
      if ( ST_FAILED ) return

      x = data(6,k)
      y = data(7,k)
      if ( .not.donew ) then
         x = x - data(9,k)
         y = y - data(10,k)
      endif
      klx = nint(data(29+5,k))						!box size
      kly = nint(data(30+5,k))

      xs = x - klx/2 - 1 - 1						!Draw box round main star
      ys = y - kly/2 - 1 - 1						! to include all measured
      xe = xs + klx + 1							! area
      ye = ys + kly + 1
      xs = max( 1.0, min(real(NX),xs) )
      ys = max( 1.0, min(real(NY),ys) )
      xe = max( 1.0, min(real(NX),xe) )
      ye = max( 1.0, min(real(NY),ye) )
      call ds_box ( xs, xe, ys, ye, 6 )


      kc = 2								!Draw spot at old position
      if ( data(8,k).gt.49.0 .or. data(8,k).lt.1.0 ) kc = 1		! in appropriate colour
      if ( dospot ) then
         call ds_spot ( x, y, kc )
      else
         call ds_cross ( x, y, alen, kc )
      endif

C  Type out details of companion star positions, and mark and draw lines
C  from main star to them, if they lie outside measuring box.

      rl = 1.2*real(klx+kly)/4.0
      nst = nint(data(26,k))						!no of comps
      if ( nst.ge.1 ) then
         call printo ( '  Main no  comp no    x    y     dx      dy  '//
     +                 '      Height    Mag   ' )

         do j = 1, nst

            if ( j.le.7 ) ja = nint(data(5+21+j,k))			!old posn
            if ( j.gt.7 ) ja = nint(data(5+41+j-8,k))
            if ( ja.lt.1 .or. ja.gt.TBYS ) then
               call pargi ( j )
               call pargi ( ja )
               call printd (
     +      'Companion No %d is List number %d - Input file suspect' )
               if ( ja.gt.TBYS ) then
                  call pargi ( TBYS )
                  call printd ( '   - List has only %d stars' )
               endif
            else
               xo = data(6,ja)
               yo = data(7,ja)
               if ( .not.donew ) then
                  xo = xo - data(9,ja)
                  yo = yo - data(10,ja)
               endif
               xd = xo - x
               yd = yo - y
               ht = data(14,ja)
               ht = trunc(ht,10)
               am = data(8,ja)
               am = trunc(am,8)

               write ( text, '(1x,i6,i3,i6,4f7.1,f14.3,f12.3)' ) k,	!Type
     +                        j, ja, xo, yo, xd, yd, ht, am
               call printo ( text )

               if ( abs(xd).gt.real(klx) .or. abs(yd).gt.real(kly) ) 	!line
     +            then
                  rr = sqrt ( (xd*xd) + (yd*yd) )
                  offx = xd*(rr-rl)/rr
                  offy = yd*(rr-rl)/rr
                  tcos = offx/sqrt(offx*offx+offy*offy)
                  tsin = offy/sqrt(offx*offx+offy*offy)
                  xs = xo - offx
                  ys = yo - offy
                  xe = xo - 2.0*abs(tcos)
                  ye = yo - 2.0*abs(tsin)
                  call ds_line ( xs, ys, xe, ye, 6 )
               endif
            endif
         enddo

         do j = 1, nst

            if ( j.le.7 ) ja = nint(data(5+21+j,k))			!old posn
            if ( j.gt.7 ) ja = nint(data(5+41+j-8,k))
            if ( ja.ge.1 .and. ja.le.TBYS ) then
               xo = data(6,ja)
               yo = data(7,ja)
               if ( .not.donew ) then
                  xo = xo - data(9,ja)
                  yo = yo - data(10,ja)
               endif

               if ( dospot ) then						!Mark
                  call ds_spot ( xo, yo, 3 )
               else
                call ds_cross ( xo, yo, alen, 3 )
               endif
            endif

         enddo

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_GNEAREST -- Get cursor posn and nearest list star
C
C    a j penny                 stsci                 1987-03-22

      subroutine sc_gnearest ( data )

      implicit none

      include 'scrut.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'

      real      data(TBVXS,TBYS)		!i: Star list
C--
      logical more
      character name*20, texta*82, text*72
      integer kx, ky, kbut, kold, knew, kolda, knewa, k, kk, istat
      real domax, dnmax, xnew, ynew, xold, yold, d, x, y, h, amag,
     +     dx, dy

      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call ds_pscur ( DSCURPOSX, DSCURPOSY )				!Put cursor at old posn

      more = .true.
      do while ( more )

         call ds_gcur ( .true., kx, ky, kbut, istat )			!Get position

         if ( kbut.eq.3 ) more = .false.

         if ( more .and. kx.ge.1 .and. kx.le.NX .and.
     +        ky.ge.1 .and. ky.le.NY ) then

            call printo ( ' ' )
            write ( text, '('' Position = '',2i5)' ) kx, ky
            call printo ( text )

            if ( GOTLIST ) then

            domax = 1.0e10						!Find nearest star in
            kold = 0							! star list
            dnmax = 1.0e10
            knew = 0
            do k = 1, TBYS
               xnew = data(6,k)
               ynew = data(7,k)
               d = (xnew-kx)*(xnew-kx) + (ynew-ky)*(ynew-ky)
               if ( d.lt.dnmax ) then
                  dnmax = d
                  knew = k
               endif
               xold = xnew
               yold = ynew
               if ( TBVXS.ge.10 ) then
                  xold = xnew - data(9,k)
                  yold = ynew - data(10,k)
               endif
               d = (xold-kx)*(xold-kx) + (yold-ky)*(yold-ky)
               if ( d.lt.domax ) then
                  domax = d
                  kold = k
               endif
            enddo

            knewa = 0
            kolda = 0
            if ( kbut.eq.2 ) then					!Find 2nd nearest star
               domax = 1.0e10						! in star list
               dnmax = 1.0e10
               do k = 1, TBYS
                  xnew = data(6,k)
                  ynew = data(7,k)
                  if ( k.ne.knew ) then
                     d = (xnew-kx)*(xnew-kx) + (ynew-ky)*(ynew-ky)
                     if ( d.lt.dnmax ) then
                        dnmax = d
                        knewa = k
                     endif
                  endif
                  if ( k.ne.kold ) then
                     xold = xnew
                     yold = ynew
                     if ( TBVXS.ge.10 ) then
                        xold = xnew - data(9,k)
                        yold = ynew - data(10,k)
                     endif
                     d = (xold-kx)*(xold-kx) + (yold-ky)*(yold-ky)
                     if ( d.lt.domax ) then
                        domax = d
                        kolda = k
                     endif
                  endif
               enddo
            endif

            if ( kbut.eq.1 ) call printo ( 'Nearest star is:-' )
            if ( kbut.eq.2 ) call printo ('The two nearest stars are:-')
            write ( texta, '(''  Number   '',
     +                      ''Name                    '',
     +                      '' X       Y       Height    Mag'')' )
            call printo ( texta )
            if ( knew.ne.0 ) then
               call namegt ( data, TBVXS, TBYS, knew, name )
               if ( ST_FAILED ) return
               x = trunc ( data(6,knew),  5 )
               y = trunc ( data(7,knew),  5 )
               dx = x - kx
               dy = y - ky
               h = 0.0
               if ( TBVXS.ge.14 ) h = trunc ( data(14,knew), 8 )
               amag = 50.0
               if ( TBVXS.ge.8 ) amag = trunc ( data(8,knew),  8 )
               kk = min(99999,knew)
               write ( texta,
     +               '('' New'',i5,2x,a20,2f8.2,f11.2,f8.3)' )
     +                 kk, name, x, y, h, amag
               call printo ( texta )
            endif
            if ( knewa.ne.0 ) then
               call namegt ( data, TBVXS, TBYS, knewa, name )
               if ( ST_FAILED ) return
               x = trunc ( data(6,knewa),  5 )
               y = trunc ( data(7,knewa),  5 )
               dx = x - kx
               dy = y - ky
               h = 0.0
               if ( TBVXS.ge.14 ) h = trunc ( data(14,knewa), 8 )
               amag = 50.0
               if ( TBVXS.ge.8 ) amag = trunc ( data(8,knewa),  8 )
               kk = min(99999,knewa)
               write ( texta,
     +               '('' New'',i5,2x,a20,2f8.2,f11.2,f8.3)' )
     +                 kk, name,  x, y, h, amag
               call printo ( texta )
            endif
            if ( kold.ne.0 ) then
               call namegt ( data, TBVXS, TBYS, kold, name )
               if ( ST_FAILED ) return
               if ( TBVXS.ge.10 ) then
                  x = trunc ( (data(6,kold)-data(9,kold)), 5 )
                  y = trunc ( (data(7,kold)-data(10,kold)), 5 )
               else
                  x = trunc ( (data(6,kold)), 5 )
                  y = trunc ( (data(7,kold)), 5 )
               endif
               dx = x - kx
               dy = y - ky
               h = 0.0
               if ( TBVXS.ge.14 ) h = trunc ( data(14,kold), 8 )
               amag = 50.0
               if ( TBVXS.ge.8 ) amag = trunc ( data(8,kold),  8 )
               kk = min(99999,kold)
               write ( texta,
     +               '('' Old'',i5,2x,a20,2f8.2,f11.2,f8.3)' )
     +                 kk, name, x, y, h, amag
               call printo ( texta )
            endif
            if ( kolda.ne.0 ) then
               call namegt ( data, TBVXS, TBYS, kolda, name )
               if ( ST_FAILED ) return
               if ( TBVXS.ge.10 ) then
                  x = trunc ( (data(6,kolda)-data(9,kolda)), 5 )
                  y = trunc ( (data(7,kolda)-data(10,kolda)), 5 )
               else
                  x = trunc ( (data(6,kolda)), 5 )
                  y = trunc ( (data(7,kolda)), 5 )
               endif
               dx = x - kx
               dy = y - ky
               h = 0.0
               if ( TBVXS.ge.14 ) h = trunc ( data(14,kolda), 8 )
               amag = 50.0
               if ( TBVXS.ge.8 ) amag = trunc ( data(8,kolda),  8 )
               kk = min(99999,kolda)
               write ( texta,
     +               '('' Old'',i5,2x,a20,2f8.2,f11.2,f8.3)' )
     +                 kk, name,  x, y, h, amag
               call printo ( texta )
            endif

         endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_CREM -- Removes stars from the list by the image cursor
C
C a j penny             stsci                  1987-03-22

      subroutine sc_crem ( data )

      implicit none
      include 'scrut.inc'
      include 'interact.inc'
      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real      data(TBVXS,TBYS)		!i: Star list
C--
      logical loop, removed, dospot, doit
      integer kw, kx, ky, kl, j, k, ke, kbut, ierr, kc
      real x, y, d, dmax, alen
      character text*72

      integer nthelp1
      parameter ( nthelp1=14 )
      character*68 thelp1(nthelp1)
      data (thelp1(j),j=1,10) /
     + 'This deals with the removal of stars from the star list.',
     + '    ',
     + 'The removal is done by the user using the cursor to mark ',
     + 'a position in the image and the program thens removes the ',
     + ' -nearest- star in the list to that position. This choice ',
     + 'now is whether the nearest star is defined from the output ',
     + 'positions of the Starman:Measure table (the normal positions)',
     + 'or the positions that the Starman:Measure programme run had, ',
     + 'as input, before it was run.',
     + '    ' /
      data (thelp1(j),j=11,nthelp1) /
     + 'Option   Function',
     + '------   --------',
     + 'Old      The -old- original positions in the list ',
     + 'New      The -new- fitted positions' /

      character*1000 topt
      data topt /
     +    'red:green:blue:cyan:magenta:yellow:coral:palegreen' /
      integer nthelp2
      parameter ( nthelp2=2 )
      character*68 thelp2(nthelp2)
      data thelp2 /
     +  'Colour of the crosses to display.' ,
     +'Choice is:- red:green:blue:cyan:magenta:yellow:coral:palegreen' /

Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST ) then						!Check list and image there
         call printo ( 'ERROR: No MEASURE list of magnitudes' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      endif

      call ds_pscur ( DSCURPOSX, DSCURPOSY )				!Set cursor at old posn

      call printo ( 'Remove stars from list' )

      call get_job ( 'WLIST', 'old:new', kw, 2, thelp1, nthelp1 )
      if ( ST_FAILED ) return

      call get1b ( 'DOSPOT', dospot, .false. )				!Get mark type
      if ( .not.dospot ) call get1r ( 'LENGTH', alen, 2.0, 0.5, 100.0 )
      if ( ST_FAILED ) return

      kc = 3
      call get_job ( 'COL_STAR', topt, kc, 1, thelp2, nthelp2 )		!Get colours
      if ( ST_FAILED ) return
      call ds_scol ( 1.0, kc )

      call printo ( ' ' )
      call printo ( ' Star number        X      Y    List taken from' )
      loop = .true.
      do while ( loop )

         call ds_gcur ( .true., kx, ky, kbut, ierr )

         doit = .true.

         if ( kbut.eq.3 ) then
            doit = .false.
            loop = .false.
         endif

         if ( NEXTRA.eq.NYEE ) then
            write ( text, '(1x,''Only'',i7,'' extra allowed'')' ) NYEE
            call printo ( text )
            loop = .false.
            doit = .false.
         endif

         if (kx.lt.1.or.kx.gt.NX.or.ky.lt.1.or.ky.gt.NY) doit = .false.

         if ( doit ) then

            dmax = 1.0e10
            kl = 0
            if ( GOTLIST .and. TBYS.ne.0 ) then
               do k = 1, TBYS
                  removed = .false.
                  if ( NEXTRA.gt.0 ) then
                     do j = 1, NEXTRA
                        if ( nint(EXTRA(1,j)).eq.k .and.
     +                       nint(EXTRA(3,j)).eq.2 ) removed = .true.
                     enddo
                  endif
                  if ( .not.removed ) then
                     if ( kw.eq.2 ) then
                        x = data(6,k)
                        y = data(7,k)
                     else
                        x = data(6,k) - data(9,k)
                        y = data(7,k) - data(10,k)
                     endif
                     d = (x-kx)*(x-kx) + (y-ky)*(y-ky)
                     if ( d.lt.dmax ) then
                        dmax = d
                        kl = k
                     endif
                  endif
               enddo
            endif

            ke = 0
            do k = 1, NYEE
               if ( nint(EXTRA(3,k)).eq.1 ) then
                  x = EXTRA(1,k)
                  y = EXTRA(2,k)
                  d = (x-kx)*(x-kx) + (y-ky)*(y-ky)
                  if ( d.lt.dmax ) then
                     dmax = d
                     ke = k
                  endif
               endif
            enddo

            if ( ke.ne.0 ) then
               EXTRA(3,ke) = 0.0
               x = EXTRA(1,ke)
               y = EXTRA(2,ke)
            write ( text, '(1x,i6,8x,2i7,''   Extra'')' ) ke, kx, ky
            call printo ( text )
            else
               NEXTRA = NEXTRA + 1
               EXTRA(1,NEXTRA) = real(kl)
               EXTRA(3,NEXTRA) = 2.0
               if ( kw.eq.2 ) then
                  x = data(6,kl)
                  y = data(7,kl)
               else
                  x = data(6,kl) - data(9,kl)
                  y = data(7,kl) - data(10,kl)
               endif
            write ( text, '(1x,i6,8x,2i7,''   Input'')' ) kl, kx, ky
            call printo ( text )
            endif

            if ( dospot ) then
               call ds_spot ( x, y, kc )
            else
               call ds_cross ( x, y, alen, kc )
            endif

         endif


      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SC_CADD -- Adds stars from the list by the image cursor
C
C a j penny             stsci                  1987-03-22

      subroutine sc_cadd ( )

      implicit none
      include 'scrut.inc'
      include 'interact.inc'
      include 'ST_DS_GEN_INC'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      logical loop, dospot, doit
      integer kx, ky, kbut, ierr, kc
      real x, y, alen
      character text*72

      character*1000 topt
      data topt /
     +    'red:green:blue:cyan:magenta:yellow:coral:palegreen' /
      integer nthelp2
      parameter ( nthelp2=2 )
      character*68 thelp2(nthelp2)
      data thelp2 /
     +  'Colour of the crosses to display.' ,
     +'Choice is:- red:green:blue:cyan:magenta:yellow:coral:palegreen' /

Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTLIST ) then						!Check list and image there
         call printo ( 'ERROR: No MEASURE list of magnitudes' )
         return
      elseif ( .not.GOTIMAGE ) then
         call printo ( 'ERROR: No image' )
         return
      endif

      call ds_pscur ( DSCURPOSX, DSCURPOSY )				!Set cursor at old posn

      call printo ( 'Add stars to list' )

      call get1b ( 'DOSPOT', dospot, .false. )				!Get mark type
      if ( .not.dospot ) call get1r ( 'LENGTH', alen, 2.0, 0.5, 100.0 )
      if ( ST_FAILED ) return

      kc = 4
      call get_job ( 'COL_STAR', topt, kc, 1, thelp2, nthelp2 )		!Get colours
      if ( ST_FAILED ) return
      call ds_scol ( 1.0, kc )

      call printo ( ' ' )
      call printo ( ' Star number        X      Y ' )
      loop = .true.
      do while ( loop )

         call ds_gcur ( .true., kx, ky, kbut, ierr )

         doit = .true.

         if ( kbut.eq.3 ) then
            loop = .false.
            doit = .false.
         endif

         if ( NEXTRA.eq.NYEE ) then
             write ( text, '(1x,''Only'',i7,'' extra allowed'')' ) NYEE
             call printo ( text )
             loop = .false.
             doit = .false.
         endif

         if (kx.lt.1.or.kx.gt.NX.or.ky.lt.1.or.ky.gt.NY) doit = .false.

         if ( doit ) then
            x = kx
            y = ky
            if ( dospot ) then
               call ds_spot ( x, y, kc )
            else
               call ds_cross ( x, y, alen, kc )
            endif
            NEXTRA = NEXTRA + 1
            EXTRA(1,NEXTRA) = kx
            EXTRA(2,NEXTRA) = ky
            EXTRA(3,NEXTRA) = 1.0
            write ( text, '(1x,i6,8x,2i7)' ) NEXTRA, kx, ky
            call printo ( text )
         endif

      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   T_INTERACT.F
C
C   Contains:-
C
C T_INTERACT  Run the INTERACTive use of the image display
C IN_OPTION_SETUP  Set up the choice options
C IN_OPDISP   Open display
C IN_NEWIM    Get new image
C IN_SDEF     Set defaults on interact program
C IN_IMGSIZ   Get image size for display
C IN_EASYSTART  Flash up an image on the display from easystart


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_INTERACT -- Run the INTERACTive use of the image display
C
C   alan penny                ral          1990 Jan

      subroutine t_interact ()

      implicit none

      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'
      include 'interact.inc'
C--
      integer ierr, kx, ky, kb
      logical doauto, loop
      character*12 ktopt, ktoptc
Cbegin


      call in_sdef							!Set display defaults

      call in_option_setup ( ktopt, 1, .true. )
      if ( ST_FAILED ) return

      call get1b ( 'EASYSTART', doauto, .true. )
      if ( doauto ) then
         call in_easystart
         KLEF_X = CH_DEF_X
         ktopt = 'inspect'
         call choice_panel_sw
      else
         call type_hchoice
      endif

      loop = .true.
      do while ( loop )							!Loop thru choices

         if ( ktopt.eq.'false' ) ktopt = ktoptc

         if ( MAIN_SELECT.eq.'main' ) then
            call in_option_setup ( ktopt, 1, .false. )
            call get_choice ( ktopt, 1 )				!Get choice
         else
            ktopt = MAIN_SELECT
            MAIN_SELECT = 'main'
         endif

         if ( ktopt.eq.'flash'      .or. ktopt.eq.'display'   .or.	!Check image there if needed
     +        ktopt.eq.'aperture'   .or. ktopt.eq.'positions' .or.
     +        ktopt.eq.'fit_magns'  .or. ktopt.eq.'inspect'   .or.
     +        ktopt.eq.'alter' ) then
             if ( .not.GOTIMAGE ) then
                call printo ( 'No image yet' )
                ktoptc = ktopt
                ktopt = 'false'
             endif
         endif

         if ( ktopt.eq.'flash' ) then					!Flash image
                          call in_opdisp ( ierr )
                          call ds_doflash ( %val(IPIM), NX, NY, IMTYPE,
     +                                      DSKVRANGE, IMTITLE )
                          DISPLAYED = .true.
                          endif

         if ( ktopt.eq.'display' ) then					!Display
                          call in_opdisp ( ierr )
                          call ds_dodisp ( %val(IPIM), NX, NY, IMTYPE,
     +                                     DSKVRANGE, IMTITLE )
                          DISPLAYED = .true.
                          endif

         if ( ktopt.eq.'aperture' ) call t_aperture 			!Aperture photometry

         if ( ktopt.eq.'positions' ) call t_posnmag ( 1 )		!Get/plot/store positions

         if ( ktopt.eq.'fit_magns' ) call t_posnmag ( 2 )		!Get/plot/store Gauss magns

         if ( ktopt.eq.'inspect' ) call t_inspect 			!Inspect area

         if ( ktopt.eq.'alter' ) call t_alter 				!Play with image

         if ( ktopt.eq.'colour' ) call t_colour 			!Change LUT

         if ( ktopt.eq.'scrutiny' ) call t_scrutiny			!Scrutiny MEASURE mags list

         if ( ktopt.eq.'image' ) call in_newim ( ierr ) 		!Get new image

         if ( ktopt.eq.'im_get_flash' ) then                            !Input new image and display
                                          call in_newim ( ierr )
                                          if ( .not.ST_FAILED .and.
     +                                         ierr.eq.0 ) then
                                          call ds_doflash ( %val(IPIM),
     +                                    NX, NY, IMTYPE, DSKVRANGE,
     +                                    IMTITLE )
                                          DISPLAYED = .true.
                                          endif
                                       endif

         if ( ktopt.eq.'area' ) then					!Change area to show
                                call in_imgsiz ( NX, NY, 1 )
                                endif

         if ( ktopt.eq.'close' ) then					!Close display
                                 call ds_close ( ierr )
                                 OPDISP = .false.
                                 DISPLAYED = .false.
                                 endif

         if ( ktopt.eq.'zoom' .and. OPDISP ) call ds_zoom ( .true.,0,0)	!Zoom, pan display

         if ( ktopt.eq.'reset' .and. OPDISP ) call ds_zoom (.true.,1,0)	!Reset Zoom, pan of display to null

         if ( ktopt.eq.'clear' .and. OPDISP ) then			!Erase display
                                              call ds_erase
                                              DISPLAYED = .false.
                                              endif

         if ( ktopt.eq.'open' ) call in_opdisp ( ierr )			!Open display

         if ( ktopt.eq.'cvalues' ) call ds_gtcur ( .true., kx, ky, kb,	!Get cursor image values
     +                                             ierr )

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Panel choice

         if ( ktopt.eq.'exit' ) loop = .false.				!Exit

         if ( ktopt.eq.'compress' ) call ds_gtcomf ( 2 )		!Get image display size compression

         if ( ST_FAILED ) loop = .false.

      enddo

      call ds_close ( ierr )
      call ds_p_close ( ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1990-01-31

      subroutine in_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt		!i: Chosen option
      integer set_num                   !i: Code for set of options
      logical  koutside                 !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=21 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'area', 'Select area of image to display',
     + 'Only use that part of the image defined by the XRANGE and',
     + 'YRANGE keyboard parameters, which give the start and end',
     + 'pixels in X and Y to use.',
     + ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'clear', 'Clear screen' ,
     + 'Clear the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'close', 'Close the display screen',
     + 'Close the display screen (the programme does not forget about',
     + 'the input image).',
     + ' ', ' ', ' ', ' '/

      data opt_text(4),opt_head(4),(opt_help(j,4),j=1,6) /
     + 'cvalues', 'Show the cursor position and image pixel value',
     + 'Show the cursor position and image pixel value.',
     + 'Whilst the cursor is inside the last displayed image in the ',
     + 'the display screen, and that window is -active-, put these',
     + 'values up in the panel window.',
     + ' ', 'Click on the right-hand button to return. ' /

      data opt_text(5),opt_head(5),(opt_help(j,5),j=1,6) /
     + 'display', 'Display image',
     + 'Display the present input image. The program asks you for:-' ,
     + '1) The display range (default: +3 and -2 std. devs. from mean)',
     + '2) Whether to -wrap- values outside that range or to show them',
     + '      as at the range ends (default: -no- or last chosen).' ,
     + '3) Where to put the bottom left hand corner of the image' ,
     + '      (default: so as to centre the image in the screen).' /

      data opt_text(6),opt_head(6),(opt_help(j,6),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(7),opt_head(7),(opt_help(j,7),j=1,6) /
     + 'flash', 'Display image in standard way',
     + 'Display the present input image. The programme uses the ',
     + 'standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(8),opt_head(8),(opt_help(j,8),j=1,6) /
     + 'image', 'Input new image' ,
     + 'This asks for a new image (via the keyboard), and forgets',
     + 'about the present image.', ' ', ' ', ' ', ' '/

      data opt_text(9),opt_head(9),(opt_help(j,9),j=1,6) /
     + 'im_get_flash', 'Input new image and display (flash) it',
     + 'This asks you for a new input image (via the keyboard), and',
     + 'then displays the image with the standard display mode:- ',
     + '(1) The display range is +3 and -2 std. devs. from the mean',
     + '(2) Show values outside the display range as if at the' ,
     + '    applicable end of the range',
     + '(3) Place image at centre of screen' /

      data opt_text(10),opt_head(10),(opt_help(j,10),j=1,6) /
     + 'open', 'Open the display screen',
     + 'Open the display screen, with a size given by keyboard entry.',
     + 'Do not display any image.',
     + ' ', ' ', ' ', ' '/

      data opt_text(11),opt_head(11),(opt_help(j,11),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.',
     + ' ', ' ', ' '/

      data opt_text(12),opt_head(12),(opt_help(j,12),j=1,6) /
     + 'reset', 'Reset zoom/pan to unity and centre',
     + 'Reset zoom/pan of the image display to unity and centre.',
     + ' ', ' ', ' ', ' ', ' '/

      data opt_text(13),opt_head(13),(opt_help(j,13),j=1,6) /
     + 'zoom', 'Zoom and pan image by using mouse location ',
     + '-Zoom- means zoom around present position of cursor. ' ,
     + '-Pan-  means set present position of cursor to screen centre.' ,
     + '     Left Button twice                      = zoom down by x2' ,
     + '     Centre Button twice                 = zoom up by x2' ,
     + '     Left Button then Centre Button = pan'  ,
     + '     Right button once                        = exit' /

      data opt_text(14),opt_head(14),(opt_help(j,14),j=1,6) /
     + 'aperture', 'Functions for aperture photometry ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Cursor getting of posns to measure flux in a circle. Sky level',
     + 'can be subtracted (annulus/circle). Allowance for extinction,',
     + 'automatically getting filter and airmass from image header.',
     + 'Exposure time can be allowed for. Different images can be',
     + 'accessed easily. The results can be output to a file. ' /

      data opt_text(15),opt_head(15),(opt_help(j,15),j=1,6) /
     + 'colour', 'Functions for changing colour display of the image.',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use different ways to change -Look-Up Table- that controls the',
     + 'colour display of image. ',
     + 'A number of standard LUTs can be loaded. Also you can modify ',
     + 'the LUT being used in a number of ways. You can also store the',
     + 'LUT you have modified, and access it again.' /

      data opt_text(16),opt_head(16),(opt_help(j,16),j=1,6) /
     + 'inspect', 'Functions for image inspection in various ways',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Selection of area, look at values. Histograms, graphical',
     + 'display of a slice, solid body plots, look at headers, find ',
     + 'radii of the stars, blink the image, contour map, statistics,',
     + 'display area. The output can be put onto any device, not just',
     + 'the screen.' /

      data opt_text(17),opt_head(17),(opt_help(j,17),j=1,6) /
     + 'fit_magns', 'Functions to get star magnitudes with Gaussians',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use cursor to pick a star, and fit a 2-D Gaussian to it. ',
     + 'This finds the star magnitude and its radius. Also gets an ',
     + 'estimate of the sky and star height. Output results.',
     + ' ', ' ' /

      data opt_text(18),opt_head(18),(opt_help(j,18),j=1,6) /
     + 'positions', 'Functions to get or plot a list of positions ',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Use the cursor to mark and store a list of XY positions. ',
     + 'Also take a list of positions from a file and plot it up. ',
     + ' ', ' ', ' ' /

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'scrutiny', 'Functions to look at Starman MEASURE output',
     + 'New set of buttons appear. Functions then available include:-',
     + 'Lets you go through the output of using the Starman MEASURE',
     + 'program, which gets magnitudes by exact profile fitting.',
     + 'This output is very complex and this can show it well:- ',
     + 'Type it out; display fits; show how nearby stars affect each ',
     + 'other; look at how well stars fitted.' /

      data opt_text(20),opt_head(20),(opt_help(j,20),j=1,6) /
     + 'compress', 'Compress image in either X or Y for display' ,
     + ' ',
     + 'This asks for the X and Y compression factors (via the ',
     + 'keyboard) that the image is squeezed down by before display.',
     + ' ',
     + 'These must be integer factors.',
     + ' '/

      data opt_text(21),opt_head(21),(opt_help(j,21),j=1,6) /
     + 'alter', 'Functions for image altering in various ways',
     + 'New set of buttons appear. The present selected area is copied',
     + 'to a work area in memory, and various alterations may be done',
     + 'to the image (rotate, flip, smooth, unsharp mask, calculate,',
     + 'etc.). Most of the capabilities of the image programs are here',
     + 'in an interactive mode. ',
     + 'The new image may then be written out to disk.' /

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Interact - Main', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=5 )
      parameter ( def_y=2 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'image', 'flash',   'panel', 'inspect',
     +                ' ', 'image', 'display', 'panel', 'inspect' /

      integer sect_num
      parameter ( sect_num=4 )
      character*10 sect_head(sect_num)
      data sect_head / 'FUNCTIONS', 'IMAGE', 'DISPLAY', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1)/ 'alter:aperture:colour:fit_magns:inspect:
     +                    positions:scrutiny' /
      data sect_text(2)/ 'image:im_get_flash' /
      data sect_text(3)/ 'area:clear:close:compress:cvalues:
     +                    display:flash:open:reset:zoom' /
      data sect_text(4)/ 'panel:exit' /

      integer help_num
      parameter ( help_num=25 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,10) /
     + '             ',
     + '             ',
     + 'For workstation use, the Cursor must be in image window area',
     + 'for the cursor buttons to work when in -WORKING- mode. The ' ,
     + 'window also must be -active- - that is the bar at the top ' ,
     + 'must be set. Click on this bar if it is not before using ' ,
     + 'the buttons.' ,
     + ' ',
     + '  Buttons for Zoom/Pan work:-  ' ,
     + '     Left Button twice              = zoom /2' /
      data (help_text(k),k=11,20) /
     + '     Centre Button twice            = zoom x2' ,
     + '     Left Button then Centre Button = pan' ,
     + '     Right button once             = exit' ,
     + ' ' ,
     + 'Zoom means zoom around present position of cursor ' ,
     + 'Pan means set present position of cursor to screen centre' ,
     + ' ',
     + ' Buttons for Cvalues work:-' ,
     + ' The values are output continuously again on the panel, or by',
     + ' request on the terminal if the panel is not being used.' /
      data (help_text(k),k=21,help_num) /
     + ' Device      -Panel- mode?    Button actions ' ,
     + ' ------      ------------     -------------- ' ,
     + 'X Windows        YES         all buttons exit ' ,
     + '                  NO         Buttons Left,Centre give values' ,
     + '                               Right Button 3 exits ' /

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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_OPDISP -- Open display
C
C alan penny                    ral                  1990-06-16

      subroutine in_opdisp ( ierr )

      implicit none
      include 'interact.inc'
      include 'ST_IMAGE_INC'

      integer	ierr		!o: Error flag (0=ok;1=bad)
C--
Cbegin


      if ( OPDISP ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) return

      call ds_init ( IMTITLE, 0, ierr )					!Open display
      if ( ierr.ne.0 ) return
      OPDISP = .true.

      call ds_gtcomf ( 1 )						!Get image display size compression


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_NEWIM -- Get new image
C
C    a j penny                    ral         1990 jan

      subroutine in_newim ( ierr )

      implicit none
      include 'interact.inc'
      include 'inspect.inc'
      include 'ST_IMAGE_INC'
      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'

      integer ierr		!o: Error flag
C--
      integer ierra
Cbegin


      if ( GOTIMAGE ) then
         call canpar ( 'IN' )
      else
         call wrkcan ( 'DEFAULT' )
      endif

      if ( WORK_LOAD ) then
         WORK_LOAD = .false.
         call wrkcan ( 'WORKALT' )
      endif

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )		!Get image
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         call gtwrks ( 'DEFAULT', 1, IPIM, ierra )
         NX = 1
         NY = 1
         GOTIMAGE = .false.
      else
         IMTITLE = ' '
         call gtimzd ( 'IN', IMTYPE, BS, BZ,INVAL,RINVAL,IMTITLE,ierr)	!Image scalers
         if ( .not.GOTIMAGE ) then					!Area of interest
            NXS =  1
            NXE = NX
            NYS = 1
            NYE = NY
         else
            NXS = min(NXS,NX)
            NXE = min(NXE,NX)
            NYS = min(NYS,NY)
            NYE = min(NYE,NY)
         endif

         DSNXS = 1							!Display area
         DSNXE = NX
         DSNYS = 1
         DSNYE = NY

         if ( GOTIMAGE ) call ds_gtcomf ( 1 )				!Get image display size compression
         GOTIMAGE = .true.
         DSKVRANGE = 0
      endif
      DISPLAYED = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_SDEF -- Set defaults on interact program
C
C    a j penny                    ral         1990 jan

      subroutine in_sdef ()

      implicit none
      include 'interact.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
C--
      integer ierr
Cbegin


      NX = 1						!Open default 'image'
      NY = 1
      call gtwrkr ( 'DEFAULT', 1, IPIM, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      BS     = 1.0					!Image value scales
      BZ     = 0.0
      INVAL  = INT_INVALSI
      RINVAL  = INT_INVALR

      WORK_LOAD = .false.				!Loaded image into work array?

      MAIN_SELECT = 'main'				!Function chosen is 'MAIN'

      call po_sdef					!Set posn/mag defaults

      call ds_sdef ( 4, 13 )				!Set Display defaults

      call ap_sdef					!Set Aperture defaults

      call sc_sdef					!Set Scrutiny defaults

      call ins_sdef					!Set Inspect defaults



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_IMGSIZ -- Get image size for display
C CL parameters accessed:-
C    XRANGE,YRANGE	X,Y range of the area of the image to be displayed
C
C    a j penny                    ral         1990 jan

      subroutine in_imgsiz ( nx, ny, kopt )

      implicit none

      include 'ST_DS_GEN_INC'
      include 'STARMAN_INC'

      integer	nx		!i: X image size
      integer	ny		!i: Y image size
      integer   kopt		!i: Option for default (0=full image;1=last choice)
C--
      integer ia, ib
Cbegin


      if ( kopt.eq.0 ) then					!Set defaults
         DSNXS = 1
         DSNXE = nx
         DSNYS = 1
         DSNYE = ny
      endif

      ia = DSNXS						!Get X window size
      ib = DSNXE
      call get2i ( 'XRANGE', ia, ib, .true., 1, nx )
      if ( ST_FAILED ) return
      DSNXS = min(ia,ib)
      DSNXE = max(ia,ib)

      ia = DSNYS						!Get Y window size
      ib = DSNYE
      call get2i ( 'YRANGE', ia, ib, .true., 1, ny )
      if ( ST_FAILED ) return
      DSNYS = min(ia,ib)
      DSNYE = max(ia,ib)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IN_EASYSTART -- Flash up an image on the display from easystart
C
C   alan penny                ral          1990 Jan

      subroutine in_easystart ()

      implicit none

      include 'interact.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
C--
      real amin, amax
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Forbidden answer' )
         return
      endif

      if ( WORK_LOAD ) then
         WORK_LOAD = .false.
         call wrkcan ( 'WORKALT' )
      endif

      call opimgr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )		!Get image
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( IMTYPE.ne.'REAL' .and. IMTYPE.ne.'SHORT' ) then
         call printo ( 'ERROR: Image must be REAL or INTEGER*2' )
         call pargc ( IMTYPE )
         call printo ( 'ERROR: Image type is: %c ' )
         ST_FAILED = .true.
         return
      endif
      GOTIMAGE = .true.

      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr)

      DSNXS = 1								!Image size (faked up for opening needs, if small)
      DSNXE = max(NX,600)
      DSNYS = 1
      DSNYE = max(NY,600)

      call ds_init ( IMTITLE, 1, ierr )					!Open display
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Can not open that display device' )
         ST_FAILED = .true.
         return
      endif
      OPDISP = .true.

      DSNXE = NX							!Set back to true size
      DSNYE = NY

      call ds_gtcomf ( 0 )						!Get image display size

      call ds_imgscl ( %val(IPIM), NX, NY, IMTYPE, 1, NX, 1, NY )	!Get display scale
      DSKVRANGE = 1

      call printo ( ' Title is: '//IMTITLE )				!Write title

      amin = BS*DSVMIN + BZ						!Write display range
      amax = BS*DSVMAX + BZ
      call pargr ( amin )
      call pargr ( amax )
      call printd ( ' Display values: Min = %f  Max = %f' )
      call pargi ( NX )							!Write size
      call pargi ( NY )
      call pargc ( IMTYPE )
      call printd ( ' Size = %dx%d  :  Type = %c' )

      call ds_disp ( %val(IPIM), NX, NY, IMTYPE, 0 )			!Display
      DISPLAYED = .true.


      end

