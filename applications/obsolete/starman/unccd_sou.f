CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   UNCCD.FOR
C
C   Contains:-
C
C T_UNCCD       Do the 'UNCCD' program function
C UN_OPINIM     Open input and output image
C UN_OPOUTIM    Open output image (if any)
C UN_OPREFIM    Open reference images
C UN_GINOUT     Get input/output information
C UN_GOPT       Get user input controls
C UN_CHECK      Check input values for faults
C UN_CHAREA     Check area is in possible area
C UN_GWORK      Get work space
C UN_LBPL       Check BPLs and load to bad pixel maps
C UN_MBPL       Load BPL to map
C UN_PROC       Do the CCD processing
C UN_MAPMAGIC(RS) Convert part of bad mapped image to magic valued image
C UN_NORM       Get and apply normalising value
C UN_PRES       Print out results
C UN_MOVLR(RS)  Put real bad mapped line into (real:short) flagged scaled array
C UN_PREPFL     Get ready for image display
C UN_FLASH      Display the image
C UN_SELECT     Load display area from working area into blh of working area
C UN_LOADBSBZ   Load the image parameters for display
C UN_DSTORE     Load the descriptors of the output image
C UN_PROCA      Process an image
C UN_BLOAD      Load a bias store from the good points in an image line
C UN_SUB        Subtract a scaled line from a line and allow for bad pixels
C UN_ESUB       Subtract a doubly scaled line from a line: allow for bad pixels
C UN_DIV        Divide a scaled line into a line and allow for bad pixels
C UN_MOVUR      Move a part of a line of an unsigned short array to a real vector
C UN_MOVSR      Move a part of a line of an short array to a real vector
C UN_MOVRR      Move a part of a line of a real array to a real vector
C UN_LIN1       Linearise a line for 1987 AAT fault
C UN_CORR1      Correct 1987 AAT rows and column faults
C UN_RIMSCALE   Convert image for display


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_UNCCD -- Do the 'UNCCD' program function
C
C   It can :-
C      Select an area
C      find and subtract bias level
C      bias, dark, flat, fringe, and bad area image it
C      normalize result
C      display result
C      store result
C
C   alan penny               ral                 1988-11-21

      subroutine t_unccd ()

      implicit none
C--
      integer ierr
Cbegin


      call un_opinim			!Open input image

      call un_opoutim			!Open output image

      call un_oprefim			!Open calib images

      call un_ginout			!Get input/output information

      call un_gopt			!Get user controls

      call un_check			!Check user input

      call un_prepfl			!Prepare for any display

      call un_gwork			!Get work space

      call un_lbpl                      !Load BPLs to bpmaps

      call un_proc			!Do CCD processing

      call un_flash 			!Display image maybe

      call ds_close ( ierr )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_OPINIM -- Open input and output image
C
C alan penny          ral                   1988-12-2

      subroutine un_opinim()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer i, istat
      character*72 title
      character*68 thelp(16)
      data thelp /
     + 'The input images are taken as Integer*2 16-bit numbers, or',
     + 'real 32-bit numbers. They are treated as real numbers inside',
     + 'the program.',
     + ' ',
     + 'The data is read in in the proper way. The input image may ',
     + 'have BSCALE and BZERO parameters, and these are applied, but ',
     + 'any magic value INVAL parameter is ignored.',
     + ' ',
     + 'Is the input data ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Signed     Signed 16-bit integer (-32768 to 32767)',
     + 'Unsigned   Unsigned 16-bit integer (0-65535)',
     + 'Badsigned  Which is flagged as signed but actually',
     + '           contains unsigned numbers (0-65535)',
     + 'Real       Real 32-bit numbers' /

Cbegin


      if ( ST_FAILED ) return

      call get_job ('DTYPE', 'signed:unsigned:badsigned:real', KTYPE,	!Get type of input image
     +                       1, thelp, 16 )
      if ( ST_FAILED ) return

      if ( KTYPE.eq.1 ) call opimsr ( 'IN', IPIM, NX, NY, .false., i )	!Get input image
      if ( KTYPE.eq.2 ) call opimur ( 'IN', IPIM, NX, NY, .false., i )
      if ( KTYPE.eq.3 ) call opimsr ( 'IN', IPIM, NX, NY, .false., i )
      if ( KTYPE.eq.4 ) call opimrr ( 'IN', IPIM, NX, NY, .false., i )
      if ( i.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      if ( KTYPE.eq.4 ) then						!Get input image descriptors
         call gtimrd ( 'IN', BS, BZ, RINVAL, title, istat )
      else
         call gtimsd ( 'IN', BS, BZ, INVAL, title, istat )
      endif
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_OPOUTIM -- Open output image (if any)
C
C   alan penny               ral                  1988-12-2

      subroutine un_opoutim ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'
C--
      integer istat, kin, kopt
      integer nth
      parameter ( nth=5 )
      character*68 th(nth)
      data th /
     + 'Are the output data ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Real     Real 32-bit numbers',
     + 'Short    Signed 16-bit integer '/
Cbegin


      if ( ST_FAILED ) return

      NXW(1) = 1							!Get area of input image to process and output
      NXW(2) = NX
      NYW(1) = 1
      NYW(2) = NY
      call get4i ( 'NAREA', NXW(1), NXW(2), NYW(1), NYW(2),
     +             .true., 1, 100000 )
      if ( ST_FAILED ) return
      call cswopi ( NXW(1), NXW(2) )
      call cswopi ( NYW(1), NYW(2) )
      NXO = NXW(2) - NXW(1) + 1
      NYO = NYW(2) - NYW(1) + 1

      kin = 2								!Get output type
      if ( KTYPE.eq.4 ) kin = 1
      call get_job ( 'OUTTYPE', 'real:short', kopt, kin, th, nth )
      if ( ST_FAILED ) return
      OUTTYPE = 'REAL'
      if ( kopt.eq.1 ) OUTTYPE = 'REAL'
      if ( kopt.eq.2 ) OUTTYPE = 'SHORT'

      call opimzw ( 'OUT', OUTTYPE, IPO, NXO, NYO, .false., istat )	!Obtain output image frame
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         DOSTORE = .false.
         IPO = IPIM
         INVALO = INT_INVALSI
         BSO = 1.0
         BZO = 28000.0
      else
         DOSTORE = .true.
         INVALO = INT_INVALSI
         RINVALO = INT_INVALR
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_OPREFIM -- Open reference images
C
C alan penny                     ral                      1988-12-2

      subroutine un_oprefim ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
C--
      integer istat, i
      character*72 text
Cbegin


      if ( ST_FAILED ) return

      IPB = 1								!Bias - Option to subtract image
      NSB(1) = 1
      NSB(2) = 1
      call opimsr ( 'IM_B', IPB, NSB(1), NSB(2), .true., istat )
      if ( ST_FAILED ) return
      DOBIAS = .false.
      if ( istat.eq.0 ) then
         DOBIAS = .true.
      elseif ( istat.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      if ( DOBIAS ) call gtimsd ( 'IM_B', BSB, BZB, INVALB, text, i )

      IPP = 1								!Preflash - Option to subtract image
      NSP(1) = 1
      NSP(2) = 1
      call opimsr ( 'IM_P', IPP, NSP(1), NSP(2), .true., istat )
      if ( ST_FAILED ) return
      DOPREF = .false.
      if ( istat.eq.0 ) then
         DOPREF = .true.
      elseif ( istat.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      if ( DOPREF ) call gtimsd ( 'IM_P', BSP, BZP, INVALP, text, i )

      IPD = 1								!Dark - Option to subtract scaled image
      NSD(1) = 1
      NSD(2) = 1
      call opimsr ( 'IM_D', IPD, NSD(1), NSD(2), .true., istat )
      if ( ST_FAILED ) return
      DODARK = .false.
      if ( istat.eq.0 ) then
         DODARK = .true.
      elseif ( istat.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      if ( DODARK ) call gtimsd ( 'IM_D', BSD, BZD, INVALD, text, i )

      IPFL = 1								!Flat - Option to divide by image
      NSFL(1) = 1
      NSFL(2) = 1
      call opimsr  ( 'IM_FL', IPFL, NSFL(1), NSFL(2), .true., istat )
      if ( ST_FAILED ) return
      DOFLAT = .false.
      if ( istat.eq.0 ) then
         DOFLAT = .true.
      elseif ( istat.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      if ( DOFLAT ) call gtimsd ( 'IM_FL', BSFL, BZFL, INVALFL, text,i)

      IPFR = 1								!Fringe - Option to subtract scaled image
      NSFR(1) = 1
      NSFR(2) = 1
      call opimsr ( 'IM_FR', IPFR, NSFR(1), NSFR(2), .true., istat )
      if ( ST_FAILED ) return
      DOFRIN = .false.
      if ( istat.eq.0 ) then
         DOFRIN = .true.
      elseif ( istat.eq.1 ) then
         ST_FAILED = .true.
         return
      endif
      if ( DOFRIN ) call gtimsd ( 'IM_FR', BSFR, BZFR, INVALFR, text,i)

      NXOF = 0								!Reference frame offsets
      NYOF = 0
      if ( DOBIAS .or. DODARK .or. DOPREF .or. DOFLAT .or. DOFRIN ) then
         call get2i ( 'OFFSET', NXOF, NYOF, .true., -10000, 10000 )
         if ( ST_FAILED ) return
         NXOF = -1*NXOF
         NYOF = -1*NYOF
      endif

      IPBPL = 1								!Bad pixel list
      NXBPL = 1
      NYBPL = 1
      call optabr ( 'BAD', IPBPL, NXBPL, NYBPL, .true., istat )
      if ( ST_FAILED ) return
      DOBAD = .true.
      if ( istat.ne.0 ) DOBAD = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_GINOUT -- Get input/output information
C
C  a j penny                ral          1988-12-2

      subroutine un_ginout ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
C--
      integer k, kl, iv, ierr
      character*32 cva
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'OBJNAME', OBJNAME, 'OBJECT', .true. )		!Where image name descriptor is and get image name
      if ( ST_FAILED ) return
      call lowcase ( OBJNAME, cva )
      if ( cva.eq.'none' ) then
         NAME = ' '
      else
         call gtdesc ( 'IN', OBJNAME, NAME, '    1', iv, ierr )
         if ( ierr.ne.0 ) then
            NAME = ' '
         else
            call lbgone ( NAME )
            call charln ( NAME, kl )
            do k = 1, kl
               if ( NAME(k:k).eq.char(39) ) NAME(k:k) = ' '
            enddo
            call lbgone ( NAME )
         endif
      endif

      call get1c ( 'TIT', TITLEO, NAME, .true. )			!Get output image title
      if ( ST_FAILED ) return

      call get1c ( 'EXPNAME', EXPNAME, 'EXPOSED', .true. )		!Get exposure descriptor name and exposure
      if ( ST_FAILED ) return
      call lowcase ( EXPNAME, cva )
      if ( cva.eq.'none' ) then
         call get1r ( 'EXPTIME', EXPT, 1.0, 0.0, 1.0e8 )
         if ( ST_FAILED ) return
      else
         call gtdesr ( 'IN', EXPNAME, EXPT, 1.0, ierr )
         if ( ierr.eq.0 ) then
            call pargr ( EXPT )
            call printd ( 'Exposure time: %f' )
         else
            call printo('WARNING: Cant get exposure time. Put as 1.0')
            EXPT = 1.0
         endif
      endif

      if ( DOPREF ) then						!Get exposure descriptor name and exposure
         call get1c ( 'PREFNAME', PREFNAME, 'PREFLASH', .true. )
         if ( ST_FAILED ) return
         PREFT = 1.0
         call lowcase ( PREFNAME, cva )
         if ( cva.eq.'none' ) then
            call get1r ( 'PREFTIME', PREFT, PREFT, 0.0, 1.0e8 )
            if ( ST_FAILED ) return
         else
            call gtdesr ( 'IN', PREFNAME, PREFT, 1.0, ierr )
            if ( ierr.ne.0 ) PREFT = 1.0
         endif
      endif

      call get1r ( 'NOISE', ZNOISE, 0.0, -1.0e8, 1.0e8 )		!Get chip characteristics
      if ( ST_FAILED ) return
      call get1r ( 'GAIN',   ZGAIN, 1.0, -1.0e8, 1.0e8 )
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_GOPT -- Get user input controls
C
C  a j penny                ral          1988-12-2

      subroutine un_gopt ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer kk, istat
      character*68 thelp1(7)
      data thelp1 /
     + 'Do you want to correct the input data for non-linearity?',
     + 'Option   Function',
     + '------   --------',
     + 'None     No linearity correction',
     + 'Mode1    This correction is of the form:-',
     + '         Raw-bias = ',
     + '         (Corrected-bias)*[1.0 + factor*(Corrected-bias)]' /
      character*68 thelp2(7)
      data thelp2 /
     + 'Do you want to correct the image for some type of fault?',
     + ' ',
     + 'Option   Function',
     + '------   --------',
     + 'None     No correction',
     + 'Mode1    The AAT type row/column fault. At present the ',
     + '         high/low adjacent rows are just averaged.' /
      character*68 thelp3(8)
      data thelp3 /
     + 'The method of normalising the images before averaging them.',
     + ' ',
     + 'Option   Function',
     + '------   --------',
     + 'Flux     Normalise average val to 1.0',
     + 'Time     Norm exp time to 1.0 second',
     + 'None     No normalisations ',
     + 'Sub      Subtract mean' /
Cbegin


      if ( ST_FAILED ) return

      NXBW(1) = 1							!Bias overscan work
      NXBW(2) = 1							! Defaults
      NYBW(1) = 1
      NYBW(2) = 1
      NXB     = 1
      NYB     = 1
      IPBBPL  = 1
      NXBBPL  = 1
      NYBBPL  = 1
      DOBVAL  = .false.
      DOBADB  = .false.
      call get1b ( 'BIAS', DOBVAL, .false. )
      if ( ST_FAILED ) return
      if ( DOBVAL) then
         call get4i ( 'NBIAS', NXBW(1), NXBW(2), NYBW(1), NYBW(2),
     +                .true., 1, 100000 )
         if ( ST_FAILED ) return
         call cswopi ( NXBW(1), NXBW(2) )
         call cswopi ( NYBW(1), NYBW(2) )
         NXB = NXBW(2) - NXBW(1) + 1
         NYB = NYBW(2) - NYBW(1) + 1
         call optabr ( 'BIASBAD', IPBBPL, NXBBPL, NYBBPL, .true., istat)
         if ( ST_FAILED ) return
         DOBADB = .true.
         if ( istat.ne.0 ) DOBADB = .false.
      endif

      call get_job ( 'LIN', 'none:mode1', kk, 1, thelp1, 7 )		!Option for linearising
      if ( ST_FAILED ) return
      LINTYPE = kk - 1
      if ( LINTYPE.eq.1 ) call get1r ( 'LINF', ALPHA, 3.2e-6,
     +                                 -1.0e8, 1.0e8 )
      if ( ST_FAILED ) return

      call get_job ( 'CORRECT', 'none:mode1', CORRTYPE, 1, thelp2, 7 )	!Option for fault correcting
      if ( ST_FAILED ) return
      CORRTYPE = CORRTYPE - 1

      call get1b ( 'DOLIMS', DOLIMS, .false. )				!Limits flagging option
      if ( ST_FAILED ) return
      if ( DOLIMS ) then
         AHILIM = 32767.0
         ALOLIM = 0.0
         if ( KTYPE.eq.2 ) AHILIM = 65535.0
         call get2r ( 'LIMS', ALOLIM, AHILIM, .true., -1.0E10, 1.0e10 )
         if ( ST_FAILED ) return
      endif

      call get_job ( 'NORM', 'flux:none:time:sub', KNORM, 2, thelp3, 8 )!Option of normalisation method
      if ( ST_FAILED ) return
      DONORM = .false.
      if ( KNORM.ne.2 ) DONORM = .true.
      NXNW(1) = NXW(1)
      NXNW(2) = NXW(2)
      NYNW(1) = NYW(1)
      NYNW(2) = NYW(2)
      if ( DONORM ) then
         call get4i ( 'NNORM', NXNW(1), NXNW(2), NYNW(1), NYNW(2),
     +                .true., 1, 100000 )
         if ( ST_FAILED ) return
         call cswopi ( NXNW(1), NXNW(2) )
         call cswopi ( NYNW(1), NYNW(2) )
      endif

      if ( OUTTYPE.eq.'SHORT' ) then
         if ( DONORM ) then
            BSO = 0.00005
            BZO = 20000.0
            call printo ( 'This program suggests the following '//
     +     'scale/zero to keep fine details of the normalised image' )
         else
            call pargr ( BS )
            call pargr ( BZ )
            call printd ( 'Input image had pixel value: scale - %f '//
     +                 'and zero - %f' )
            call printo ( 'This program suggests the following '//
     +                'scale/zero to avoid the risk of going -ve' )
            BSO = 1.0
            BZO = 28000.0
         endif
         call get2r ( 'SCALE', BSO, BZO, .true., -1.0e20, 1.0e20 )	!Get output image `bscale' and `bzero' and `inval'
         if ( ST_FAILED ) return
      else
         BSO = 1.0
         BZO = 0.0
      endif

      call get1b ( 'DISP', DODISP, .false. )				!Option for type of displaying result
      if ( ST_FAILED ) return
      if ( DODISP ) then
         NXDW(1) = NXW(1)
         NXDW(2) = NXW(2)
         NYDW(1) = NYW(1)
         NYDW(2) = NYW(2)
         call get4i ( 'NDISP', NXDW(1), NXDW(2), NYDW(1), NYDW(2),
     +                .true., 1, 100000 )
         if ( ST_FAILED ) return
         call cswopi ( NXDW(1), NXDW(2) )
         call cswopi ( NYDW(1), NYDW(2) )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_CHECK -- Check input values for faults
C
C  alan penny         ral                   1988-12-5

      subroutine un_check ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer iv, nrefs
      logical gotref
Cbegin


      if ( ST_FAILED ) return

      if ( NX.gt.5000 ) then						!Image width check
         call printo ( 'ERROR: Images must be 5000 or less wide' )
         ST_FAILED = .true.
      endif

      if ( .not.ST_FAILED .and. DOSTORE .and. abs(BSO).lt.1.0e-10 ) then	!Output pixel value scale check
         call printo ( 'ERROR: Cant use that output scale - too small' )
         ST_FAILED = .true.
      endif

      gotref = .false.							!Reference areas check
      nrefs = 0
      NXC = 1
      NYC = 1

      if ( .not.ST_FAILED .and. DOBIAS ) then				!Bias area check
         if ( gotref ) then
            if ( NXC.ne.NSB(1) .or. NYC.ne.NSB(2) ) then
               ST_FAILED = .true.
               call printo ( 'ERROR: Bias image wrong size' )
               call pargi ( NSB(1) )
               call pargi ( NSB(2) )
               call pargi ( NXC )
               call pargi ( NYC )
               call printd ('        Image is %dx%d - should be %dx%d')
            endif
         else
            gotref = .true.
            nrefs = nrefs + 1
            NXC = NSB(1)
            NYC = NSB(2)
         endif
      endif

      if ( DODARK .and. .not.ST_FAILED ) then				!Dark area check
         if ( gotref ) then
            if ( NXC.ne.NSD(1) .or. NYC.ne.NSD(2) ) then
               ST_FAILED = .true.
               call printo ( 'ERROR: Dark image wrong size' )
               call pargi ( NSD(1) )
               call pargi ( NSD(2) )
               call pargi ( NXC )
               call pargi ( NYC )
               call printd ('        Image is %dx%d - should be %dx%d')
            endif
         else
            gotref = .true.
            nrefs = nrefs + 1
            NXC = NSD(1)
            NYC = NSD(2)
         endif
      endif

      if ( DOPREF .and. .not.ST_FAILED ) then				!Preflash area check
         if ( gotref ) then
            if ( NXC.ne.NSP(1) .or. NYC.ne.NSP(2) ) then
               ST_FAILED = .true.
               call printo ( 'ERROR: Pre-flash image wrong size' )
               call pargi ( NSP(1) )
               call pargi ( NSP(2) )
               call pargi ( NXC )
               call pargi ( NYC )
               call printd ('        Image is %dx%d - should be %dx%d')
            endif
         else
            gotref = .true.
            nrefs = nrefs + 1
            NXC = NSP(1)
            NYC = NSP(2)
         endif
      endif

      if ( DOFLAT .and. .not.ST_FAILED ) then				!Flat area check
         if ( gotref ) then
            if ( NXC.ne.NSFL(1) .or. NYC.ne.NSFL(2) ) then
               ST_FAILED = .true.
               call printo ( 'ERROR: Flat image wrong size' )
               call pargi ( NSFL(1) )
               call pargi ( NSFL(2) )
               call pargi ( NXC )
               call pargi ( NYC )
               call printd ('        Image is %dx%d - should be %dx%d')
            endif
         else
            gotref = .true.
            nrefs = nrefs + 1
            NXC = NSFL(1)
            NYC = NSFL(2)
         endif
      endif

      if ( DOFRIN .and. .not.ST_FAILED ) then				!Fringe area check
         if ( gotref ) then
            if ( NXC.ne.NSFR(1) .or. NYC.ne.NSFR(2) ) then
               ST_FAILED = .true.
               call printo ( 'ERROR: Fringe image wrong size' )
               call pargi ( NSFR(1) )
               call pargi ( NSFR(2) )
               call pargi ( NXC )
               call pargi ( NYC )
               call printd ('        Image is %dx%d - should be %dx%d')
            endif
         else
            gotref = .true.
            nrefs = nrefs + 1
            NXC = NSFR(1)
            NYC = NSFR(2)
         endif
      endif

      if ( gotref .and. .not.ST_FAILED ) then				!Reference images cover area to be used?
         if ( (NXW(1)+NXOF).lt.1 .or. (NXW(2)+NXOF).gt.NXC .or.
     +        (NYW(1)+NYOF).lt.1 .or. (NYW(2)+NYOF).gt.NYC      ) then
            ST_FAILED = .true.
            if ( nrefs.eq.1 ) then
               call printo (
     +        'ERROR: Reference image does not cover area to be used' )
            else
               call printo (
     +        'ERROR: Reference images do not cover area to be used' )
            endif
            iv = NXW(1) + NXOF
            if ( iv.lt.1 ) then
               iv = 1 - iv
               call pargi ( iv )
               call printd ( '        Area used starts at %d pixel(s)'//
     +                       ' to left of reference image' )
            endif
            iv = NXW(2) + NXOF
            if ( iv.gt.NXC ) then
               iv = iv - NXC
               call pargi ( iv )
               call printd ( '        Area used ends at %d pixel(s)'//
     +                       ' to right of reference image' )
            endif
            iv = NYW(1) + NYOF
            if ( iv.lt.1 ) then
               iv = 1 - iv
               call pargi ( iv )
               call printd ( '        Area used starts at %d pixel(s)'//
     +                       ' below bottom of reference image' )
            endif
            iv = NYW(2) + NYOF
            if ( iv.gt.NYC ) then
               iv = iv - NYC
               call pargi ( iv )
               call printd ( '        Area used ends at %d pixel(s)'//
     +                       ' above top of reference image' )
            endif
         endif
      endif

      call un_charea ( DODISP, NXDW, NYDW, NXW(1), NXW(2), NYW(1), 	!Display area in output area?
     +                                     NYW(2), 'Display' )
      call un_charea ( DONORM, NXNW, NYNW, NXW(1), NXW(2), NYW(1), 	!Normalising area in output area?
     +                                     NYW(2), 'Normalising' )
      call un_charea ( DOBVAL, NXBW, NYBW, 1, NX, 1, NY, 'Bias' )	!Bias area in input image?


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_CHAREA -- Check area is in possible area
C
C  alan penny           ral               1990-070-26

      subroutine un_charea ( dothis, kxw, kyw, kxs, kxe, kys, kye,
     +                       text )


      implicit none
      include 'STARMAN_INC'

      logical		dothis		!i: Do this check
      integer		kxw(2)		!i: X range of area
      integer		kyw(2)		!i: Y range of area
      integer		kxs		!i: X start of check area
      integer		kxe		!i: X end of check area
      integer		kys		!i: Y start of check area
      integer		kye		!i: Y end of check area
      character*(*)	text		!i: Name of area
C--
Cbegin


      if ( ST_FAILED .or. .not.dothis ) return

      if ( kxw(1).lt.kxs .or. kxw(1).gt.kxe .or.
     +     kxw(2).lt.kxs .or. kxw(2).gt.kxe .or.
     +     kyw(1).lt.kys .or. kyw(1).gt.kye .or.
     +     kyw(2).lt.kys .or. kyw(2).gt.kye      ) then
         ST_FAILED = .true.
         call pargc ( text )
         call printd ( 'ERROR: %c area not all in possible area' )
         call pargi ( kxw(1) )
         call pargi ( kxw(2) )
         call pargi ( kxs )
         call pargi ( kxe )
         call printd (
     +           '       X range is %d to %d - should be in %d to %d' )
         call pargi ( kyw(1) )
         call pargi ( kyw(2) )
         call pargi ( kys )
         call pargi ( kye )
         call printd (
     +           '       Y range is %d to %d - should be in %d to %d' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_GWORK -- Get work space
C
C  alan penny           ral               1988-11-21

      subroutine un_gwork()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      IPBPMAP = 1							!Main bad map
      if ( DOBAD ) call gtwrks ( 'WIC', NX*NY, IPBPMAP, istat )

      IPWB = 1								!Bias space and bad bias space
      IPBBPMAP = 1
      if ( DOBVAL ) then
         call gtwrkr ( 'WID', NXB*NYB, IPWB, istat )
         if ( DOBADB ) call gtwrks ( 'WIE', NX*NY, IPBBPMAP, istat )
      endif

      call gtwrkr ( 'WIA', NXO*NYO, IPT,  istat )			!
      call gtwrks ( 'WIB', NXO*NYO, IPTB, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_LBPL -- Check BPLs and load to bad pixel maps
C
C alan penny                 ral                           1988-12-2

      subroutine un_lbpl ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( DOBAD ) then							!Main bad pixel map
         if ( NXBPL.lt.9 .or. NYBPL.lt.1 ) then
            call printo ( 'ERROR: Bad Pixel List wrong' )
            ST_FAILED = .true.
         else
            call un_mbpl ( %val(IPBPL), NXBPL, NYBPL, %val(IPBPMAP),
     +                     NX, NY )
         endif
      endif

      if ( DOBADB ) then						!Bias bad pixel map
         if ( NXBBPL.lt.9 .or. NYBBPL.lt.1 ) then
            call printo ( 'ERROR: Bias Bad Pixel List wrong' )
            ST_FAILED = .true.
         else
            call un_mbpl ( %val(IPBBPL), NXBBPL, NYBBPL, %val(IPBBPMAP),
     +                     NX, NY )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MBPL -- Load BPL to map
C   Assumes that the BPL is in a table. Each row of the BPL
C   is a bad area with the blh corner of the bad area given by
C   parameters 6 and 8, and the trh corner given by parameters
C   7 and 9.
C
C   The map is then loaded with 0's at good areas and 1's at bad
C   areas.
C
C alan penny                 ral                           1988-12-2

      subroutine un_mbpl ( bpl, nxbpl, nybpl, bpmap, nx, ny )

      implicit none
      include 'STARMAN_INC'

      integer   nxbpl			!i: Bad pixel list x size
      integer   nybpl			!i: Bad pixel list y size
      real      bpl(nxbpl,nybpl)	!i: Bad pixel list
      integer   nx			!i: Bad pixel map x size
      integer   ny			!i: Bad pixel map y size
      integer*2 bpmap(nx,ny)		!o: Bad pixel map
C--
      integer j, k, l, kxa, kxb, kya, kyb
Cbegin


      if ( ST_FAILED ) return

      call azeros ( bpmap, nx*ny )
      do l = 1, nybpl
         kxa = nint(bpl(6,l))
         kxb = nint(bpl(7,l))
         kya = nint(bpl(8,l))
         kyb = nint(bpl(9,l))
         call cswopi ( kxa, kxb )
         call cswopi ( kya, kyb )
         if ( kxa.le.nx .and. kxb.ge.1 .and.
     +        kya.le.ny .and. kyb.ge.1 ) then
            kxa = min(nx,max(1,kxa))
            kxb = min(nx,max(1,kxb))
            kya = min(ny,max(1,kya))
            kyb = min(ny,max(1,kyb))
            do k = kya, kyb
               do j = kxa, kxb
                  bpmap(j,k) = 1
               enddo
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_PROC -- Do the CCD processing
C
C   alan penny        ral                        1988-12-2

      subroutine un_proc ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
C--
Cbegin

      if ( ST_FAILED ) return

      call un_proca ( %val(IPBPMAP), %val(IPBBPMAP), %val(IPB), 	!Process image
     +                %val(IPP), %val(IPD), %val(IPFL), %val(IPFR),
     +                %val(IPT), %val(IPTB), %val(IPWB) )

      if ( .not.ST_FAILED .and. DONORM ) then				!Apply normalisation
         call un_norm ( %val(IPT), %val(IPTB) )
         if ( DOSTORE ) then						!Store if wanted
            if ( OUTTYPE.eq.'SHORT' ) then
               call un_mapmagics ( %val(IPT), %val(IPTB), %val(IPO),
     +                             NXO, NYO, BSO, BZO, INVALO )
            else
               call un_mapmagicr ( %val(IPT), %val(IPTB), %val(IPO),
     +                             NXO, NYO, BSO, BZO, RINVALO )
            endif
            call un_dstore
         endif
      endif

      call un_pres 							!Print result


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MAPMAGICR -- Convert part of bad mapped image to magic real image
C
C   alan penny        ral                        1988-12-2

      subroutine un_mapmagicr ( rim, rimb, out, nx, ny, bs, bz, rinval)

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      real      rim(nx,ny)  	!i: image
      integer*2 rimb(nx,ny)	!i: image bad pixel map
      real      out(nx,ny)	!o: Output magic valued image
      real	bs		!i: Output image value scale
      real	bz		!i: Output image value zero
      real      rinval		!i: Output image scale
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ny
         call un_movlrr ( rim(1,k), rimb(1,k), nx, ny, k,
     +                    out, bs, bz, rinval )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MAPMAGICS -- Convert part of bad mapped image to magic valued image
C
C   alan penny        ral                        1988-12-2

      subroutine un_mapmagics ( rim, rimb, iout, nx, ny, bs, bz, inval )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      real      rim(nx,ny)  	!i: image
      integer*2 rimb(nx,ny)	!i: image bad pixel map
      integer*2 iout(nx,ny)	!o: Output magic valued image
      real	bs		!i: Output image value scale
      real	bz		!i: Output image value zero
      integer	inval		!i: Output image scale
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ny
         call un_movlrs ( rim(1,k), rimb(1,k), nx, ny, k,
     +                    iout, bs, bz, inval )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_NORM -- Get and apply normalising value
C
C   alan penny        ral                        1988-12-2

      subroutine un_norm ( rim, rimb )

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'

      real      rim(NXO,NYO)  	!i/o: image
      integer*2 rimb(NXO,NYO)	!i: image bad pixel map
C--
      integer npt, kxs, kxe, kys, kye
      real rstd, a
Cbegin


      if ( ST_FAILED ) return

      if ( KNORM.eq.1 .or. KNORM.eq.4 ) then				!Calc normalistaion
         kxs = NXNW(1) - NXW(1) + 1
         kxe = NXNW(2) - NXW(1) + 1
         kys = NYNW(1) - NYW(1) + 1
         kye = NYNW(2) - NYW(1) + 1
         call bptavgr ( rim, rimb, NXO, NYO, kxs, kxe, kys, kye, ANORM,
     +                  rstd, npt )
      elseif ( KNORM.eq.3 ) then
         ANORM = EXPT
      endif

      if ( KNORM.eq.1 .or. KNORM.eq.3 ) then				!Apply normalisation
         if ( abs(ANORM).gt.1.0e-10 ) then
            a = 1.0/ANORM
            call amulkr ( rim, a, rim, NXO*NYO )
         endif
      else
         call asubkr ( rim, ANORM, rim, NXO*NYO )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_PRES -- Print out results
C
C   a j penny                ral                       1988-11-17

      subroutine un_pres ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
C--
      character*72 text
Cbegin


      if ( ST_FAILED ) return

      if ( DOBVAL .and. DONORM ) then
         write ( text, '(36x,''Normalise'')' )
         call printo ( text )
        write ( text, '(10x,''Name'',13x,''Exp'',8x,''Value'',7x,
     +                   ''Bias'',7x,''Std'')' )
      elseif ( DOBVAL ) then
         write ( text,
     +           '(10x,''Name'',13x,''Exp'',9x,''Bias'',7x,''Std'')' )
      elseif ( DONORM ) then
         write ( text, '(36x,''Normalise'')' )
         call printo ( text )
         write ( text, '(10x,''Name'',13x,''Exp'',8x,''Value'')'  )
      else
         write ( text, '(10x,''Name'',13x,''Exp'')' )
      endif
      call printo ( text )

      if ( DOBVAL .and. DONORM ) then
         write ( text, '(1x,2x,a20,f10.3,2f12.2,f12.4)' )
     +              NAME, EXPT, ANORM, ABIAS, ABIASSTD
      elseif ( DOBVAL ) then
         write ( text, '(1x,2x,a20,f10.3,f12.2,f12.4)' )
     +              NAME, EXPT, ABIAS, ABIASSTD
      elseif ( DONORM ) then
         write ( text,' (1x,2x,a20,f10.3,f12.2)' ) NAME, EXPT, ANORM
      else
         write ( text, '(1x,2x,a20,f10.3)' ) NAME, EXPT
      endif
      call printo ( text )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MOVLRR -- Put real bad mapped line into real flagged scaled array
C
C   alan penny                   ral                    1988-12-23

      subroutine un_movlrr ( rim, map, nx, ny, ky, out, sc, ze, rinv )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X size of output
      integer   ny		!i: Y size of output
      real      rim(nx)		!i: Input line
      integer*2 map(nx)		!i: Bad pixel map of line
      integer   ky		!i: Output line
      real      out(nx,ny)	!o: Output
      real      sc		!i: Scale of output
      real      ze		!i: Zero of output
      real      rinv		!i: Level in output to take invalid flag
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, nx
         if ( map(j).ne.0 ) then
            out(j,ky) = rinv
         else
            out(j,ky) = (rim(j)-ze)/sc
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MOVLRS -- Put real bad mapped line into short flagged scaled array
C
C   alan penny                   ral                    1988-12-23

      subroutine un_movlrs ( rim, map, nx, ny, ky, iout, sc, ze, inv )

      implicit none
      include 'ST_LIMITS_INC'
      include 'STARMAN_INC'

      integer   nx		!i: X size of output
      integer   ny		!i: Y size of output
      real      rim(nx)		!i: Input line
      integer*2 map(nx)		!i: Bad pixel map of line
      integer   ky		!i: Output line
      integer*2 iout(nx,ny)	!o: Output
      real      sc		!i: Scale of output
      real      ze		!i: Zero of output
      integer   inv		!i: Level in output to take invalid flag
C--
      integer j
      real data
Cbegin


      if ( ST_FAILED ) return

      do j = 1, nx
         if ( map(j).ne.0 ) then
            iout(j,ky) = inv
         else
            data = (rim(j)-ze)/sc
            if ( data.gt.INT_MAXSR .or. data.lt.INT_MINSR ) then
               iout(j,ky) = inv
            else
               iout(j,ky) = nint(data)
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_PREPFL -- Get ready for image display
C
C   alan penny                ral            1988-12-05

      subroutine un_prepfl ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_DS_GEN_INC'
C--
      integer nxd, nyd, ierr
Cbegin


      if ( ST_FAILED .or. .not.DODISP ) return

      call ds_sdef ( 1, 1 )						!Set Display defaults

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      nxd = NXDW(2) - NXDW(1) + 1
      nyd = NYDW(2) - NYDW(1) + 1
      DSNXS = 1								!Image size
      DSNXE = nxd
      DSNYS = 1
      DSNYE = nyd

      call ds_init ( TITLEO, 0, ierr )					!Open display
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_FLASH -- Display the image
C
C   alan penny                ral            1988-12-05

      subroutine un_flash ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
      include 'ST_DS_GEN_INC'
C--
      logical enough
      real am, std, amin, amax
      integer kxr(2), kyr(2), nxd, nyd, ierr, i
Cbegin


      if ( ST_FAILED .or. .not.DODISP ) return

      nxd = NXDW(2) - NXDW(1) + 1
      nyd = NYDW(2) - NYDW(1) + 1

      call gtwrks ( 'WIF', nxd*nyd, ipd, ierr )				!Get work space

      if ( NXW(1).ne.NXDW(1) .or. NXW(2).ne.NXDW(2) .or.
     +     NYW(1).ne.NYDW(1) .or. NYW(1).ne.NYDW(2) ) call un_select
     +                                      ( %val(IPT), %val(IPTB) )

      call un_mapmagics ( %val(IPT), %val(IPTB), %val(ipd),		!Convert to i*2 magic values array
     +                    nxd, nyd, BSO, BZO, INVALO )

      kxr(1) = 1							!Get display range
      kxr(2) = nxd
      kyr(1) = 1
      kyr(2) = nyd
      call ranges ( %val(ipd), nxd, nyd, kxr, kyr, INVALO, am, std, i )
      DSVMIN = am + 3.0*std
      DSVMAX = am - 2.0*std
      if ( DSVMIN.eq.DSVMAX ) DSVMAX = DSVMIN - 1.0

      call un_loadbsbz ( BSO, BZO, INVALO )				!Load image params for display

      call ds_gtcomf ( 1 )						!Get image display size compression

      call ds_disp ( %val(ipd), nxd, nyd, 'SHORT', 0 )			!Display

      amin = BSO*DSVMIN + BZO						!Write display range
      amax = BSO*DSVMAX + BZO
      call pargr ( amin )
      call pargr ( amax )
      call printd ( 'Display values: Min = %f  Max = %f' )

      enough = .false.
      do while ( .not.enough )
         call get1b ( 'ENOUGH', enough, .true. )
         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_SELECT -- Load display area from working area into blh of working area
C
C alan penny      ral                   1988-12-2

      subroutine un_select ( rim, rimb )

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'

      real      rim(NXO*NYO)		!i/o: Working area/output display area
      integer*2 rimb(NXO*NYO)		!i/o: Working area/output display area
					!     bad pixel map
C--
      integer j, k, jo, ji, nxd, nyd, ja, ka, kxd, kyd
Cbegin


      if ( ST_FAILED ) return

      kxd = NXDW(1) - NXW(1)
      kyd = NYDW(1) - NYW(1)

      nxd = NXDW(2) - NXDW(1) + 1
      nyd = NYDW(2) - NYDW(1) + 1

      do k = 1, nyd
         do j = 1, nxd
            jo = j + (k-1)*nxd
            ja = j + kxd
            ka = k + kyd
            ji = ja + (ka-1)*NXO
            rim(jo) = rim(ji)
            rimb(jo) = rimb(ji)
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_LOADBSBZ -- Load the image parameters for display
C
C alan penny      ral                   1988-12-2

      subroutine un_loadbsbz ( absc, abze, ainval )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      real	absc		!i: Image BS
      real	abze		!i: Image BZ
      integer	ainval		!i: Image INVAL
C--
Cbegin


      if ( ST_FAILED ) return

      BS = absc
      BZ = abze
      INVAL = ainval


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_DSTORE -- Load the descriptors of the output image
C
C alan penny      ral                   1988-12-2

      subroutine un_dstore ()

      implicit none
      include 'unccd.inc'
      include 'STARMAN_INC'
C--
      character date_time*52, descr1*11
      integer istat, idt1, idt2
Cbegin


      if ( ST_FAILED ) return

      call icopdes (  'IN', 'OUT', istat )
      call ptdesc ( 'OUT', 'TITLE',  TITLEO )
      if ( OUTTYPE.eq.'SHORT' ) then
         call ptdesi ( 'OUT', 'INVAL',  INVALO )
      else
         call ptdesr ( 'OUT', 'INVAL',  RINVALO )
      endif
      call ptdesr ( 'OUT', 'BSCALE', BSO )
      call ptdesr ( 'OUT', 'BZERO',  BZO )

      istat = 0
      call ajdate ( date_time )
      call endstrip ( date_time, idt1, idt2 )

      if ( ZNOISE.gt.0.0 ) then
         write ( descr1, '(e11.4)' ) ZNOISE
         call ptdesc ( 'OUT', 'NOISE', descr1//
     +     '    / +/- 0.6  electrons' )
      endif

      if ( ZGAIN.gt.0.0 ) then
         write ( descr1, '(e11.4)' ) ZGAIN
         call ptdesc ( 'OUT', 'GAIN', descr1//
     +     '    / +/- 0.1 electrons per adu' )
      endif

      if ( LINTYPE.eq.1 ) then
         write ( descr1, '(e11.4)' ) ALPHA
         call ptdesc ( 'OUT', 'LINFACT', descr1//
     +     '    / Linearised '//date_time(idt1:idt2) )
      endif

      if ( CORRTYPE.eq.1 ) then
         write ( descr1, '(e11.4)' ) CORRTYPE
         call ptdesc ( 'OUT', 'CORRECT', descr1//
     +     '    / Correction type '//date_time(idt1:idt2) )
      endif

      if ( DOBVAL ) then
         write ( descr1, '(f11.4)' ) ABIAS
         call ptdesc ( 'OUT', 'BIASVAL', descr1//
     +           '     / Debiased '//date_time(idt1:idt2) )
      endif

      if ( DOFLAT ) call ptdesc ( 'OUT', 'FLAT', date_time(idt1:idt2) )

      if ( DOBIAS ) call ptdesc ( 'OUT', 'BIAS', date_time(idt1:idt2) )

      if ( DODARK ) call ptdesc ( 'OUT', 'DARK', date_time(idt1:idt2) )

      if ( DOPREF ) call ptdesc ( 'OUT', 'PREF', date_time(idt1:idt2) )

      if ( DOFRIN ) call ptdesc ( 'OUT', 'FRIN', date_time(idt1:idt2) )



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_PROCA -- Process an image
C
C alan penny                ral               1988-11-18

      subroutine un_proca ( bpmap, bbpmap, kbias, kpref, kdark, kflat,
     +                      kfrin, rim, rimb, tbias )

      implicit none
      include 'unccd.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer*2 bpmap(NX,NY)
      integer*2 bbpmap(NX,NY)
      integer*2 kbias(NXC,NYC)
      integer*2 kpref(NXC,NYC)
      integer*2 kdark(NXC,NYC)
      integer*2 kflat(NXC,NYC)
      integer*2 kfrin(NXC,NYC)
      real      rim(NXO,NYO)
      integer*2 rimb(NXO,NYO)
      real      tbias(NXB*NYB)
C--
      integer j, k, ka, kb, np, ng, ks, ke
      real rtemp(5000)
      integer*2 rtempb(5000)
Cbegin


      if ( ST_FAILED ) return

C Load and scale image into store and into bias store, line-by-line

      np = 1
      ks = min(NYW(1),NYBW(1))
      ke = max(NYW(2),NYBW(2))
      kb = 0

      do k = ks, ke
         ka = k + NYOF

         call azeros ( rtempb, NX )					!Zero bp line

         if ( KTYPE.eq.1 ) then						!Load image
            call un_movsr ( %val(IPIM), NX, NY, 1, k, rtemp, NX, BS, 	!+ bp line
     +                      BZ, INVAL, rtempb )
         elseif ( KTYPE.eq.2 .or. KTYPE.eq.3 ) then
            call un_movur ( %val(IPIM), NX, NY, 1, k, rtemp, NX, BS,
     +                      BZ, INVAL, rtempb )
         else
            call un_movrr ( %val(IPIM), NX, NY, 1, k, rtemp, NX, BS,
     +                      BZ, RINVAL, rtempb )
         endif

         if ( DOBAD ) call abors ( bpmap(1,ka), rtempb, rtempb, NX )	!Load bp map

         if ( DOLIMS ) call blimr ( rtemp, rtempb, NX, ALOLIM, AHILIM )	!Check limits

         if ( DOBVAL .and. k.ge.NYBW(1) .and. k.le.NYBW(2) ) then
            if ( DOBADB ) call abors ( bbpmap(1,ka), rtempb, rtempb, NX)!Load bias bp map
            call un_bload(rtemp, rtempb, NXBW(1), NXBW(2), tbias(np),ng)!Load bias pixels to bias store
            np = np + ng
         endif

         if ( k.ge.NYW(1) .and. k.le.NYW(2) ) then			!Store image and mask in temp store
            kb = kb + 1
            call amovr ( rtemp(NXW(1)), rim(1,kb), NXO )
            call amovs ( rtempb(NXW(1)), rimb(1,kb), NXO )
         endif

      enddo

      if ( DOBVAL ) call aavgr ( tbias, np, ABIAS, ABIASSTD )

      if ( CORRTYPE.eq.1 ) call un_corr1 ( rim, NXO, NYO, NXW(1), 	!Correct faults
     +                                     NYW(1) )

      j = NXW(1) + NXOF 						!Do the processing
      do k = 1, NYO
         ka = k + NYW(1) + NYOF - 1

         if ( LINTYPE.eq.1 ) call un_lin1 ( rim(1,k), ABIAS, ALPHA, NXO)

         if ( DOBVAL ) call asubkr ( rim(1,k), ABIAS, rim(1,k), NXO )

         if ( DOBIAS ) call un_sub ( rim(1,k), kbias(j,ka), BSB, BZB,
     +                               rimb(1,k), INVALB, NXO )

         if ( DOPREF ) call un_esub ( rim(1,k), kpref(j,ka), BSP, BZP,
     +                                PREFT, rimb(1,k), INVALP, NXO )

         if ( DODARK ) call un_esub ( rim(1,k), kdark(j,ka), BSD, BZD,
     +                                EXPT, rimb(1,k), INVALD, NXO )

         if ( DOFLAT ) call un_div ( rim(1,k), kflat(j,ka), BSFL, BZFL,
     +                               rimb(1,k), INVALFL, NXO )

         if ( DOFRIN ) call un_esub ( rim(1,k), kfrin(j,ka), BSFR, BZFR,
     +                                EXPT, rimb(1,k), INVALFR, NXO )

         if ( DOSTORE .and. .not.DONORM ) then
                   if ( OUTTYPE.eq.'SHORT' ) then
                      call un_movlrs ( rim(1,k), rimb(1,k), NXO, NYO,
     +                               k, %val(IPO), BSO, BZO, INVALO )
                   else
                      call un_movlrr ( rim(1,k), rimb(1,k), NXO, NYO,
     +                               k, %val(IPO), BSO, BZO, RINVALO )
                   endif
                                          endif

      enddo

      if ( DOSTORE .and. .not.DONORM ) call un_dstore			!Store output header info


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_BLOAD -- Load a bias store from the good points in an image line
C
C alan penny           ral            1988-12-28

      subroutine un_bload ( a, ibp, ns, ne, out, ng )

      implicit none
      include 'STARMAN_INC'

      integer   ns		!i: Start point to work from
      integer   ne		!i: Finish point to work to
      real      a(ne)		!i: Data
      integer*2 ibp(ne)		!i: BP map
      real      out(ne)		!o: Place to load good data to
      integer   ng		!o: number of good points loaded
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      ng = 0
      do j = ns, ne
         if ( ibp(j).eq.0 ) then
            ng = ng + 1
            out(ng) = a(j)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_SUB -- Subtract a scaled line from a line and allow for bad pixels
C
C alan penny           ral            1988-12-28

      subroutine un_sub ( a, b, sc, ze, c, inval, n )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: No of points to be done
      real      a(n)		!i/o: Line
      integer*2 b(n)		!i: Line to be subtracted
      real      sc		!i: Subtraction line scale
      real      ze		!i: Subtraction line zero
      integer*2 c(n)		!i/o: Bad pixel flags
      integer   inval		!i: Subtraction line bad flag value
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, n
         a(j) = a(j) - (sc*real(b(j)) + ze)
         if ( b(j).eq.inval ) c(j) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_ESUB -- Subtract a doubly scaled line from a line: allow for bad pixels
C
C alan penny           ral            1988-12-28

      subroutine un_esub ( a, b, sc, ze, e, c, inval, n )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: No of points to be done
      real      a(n)		!i/o: Line
      integer*2 b(n)		!i: Line to be subtracted
      real      sc		!i: Subtraction line scale
      real      ze		!i: Subtraction line zero
      real      e		!i: Subtraction line multiplyer
      integer*2 c(n)		!i/o: Bad pixel flags
      integer   inval		!i: Subtraction line bad flag value
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, n
         a(j) = a(j) - e*(sc*real(b(j)) + ze)
         if ( b(j).eq.inval ) c(j) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_DIV -- Divide a scaled line into a line and allow for bad pixels
C
C alan penny           ral            1988-12-28

      subroutine un_div ( a, b, sc, ze, c, inval, n )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: No of points to be done
      real      a(n)		!i/o: Line
      integer*2 b(n)		!i: Line to be subtracted
      real      sc		!i: Division line scale
      real      ze		!i: Division line zero
      integer*2 c(n)		!i/o: Bad pixel flags
      integer   inval		!i: Division line bad flag value
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      do j = 1, n
         a(j) = a(j)/(sc*real(b(j)) + ze)
         if ( b(j).eq.inval ) c(j) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MOVUR -- Move a part of a line of an unsigned short array to a real vector
C
C     alan penny        ral                    1988-11-24

      subroutine un_movur ( kdata, nx, ny, ix, iy, out, n, sc, ze,
     +                      inv, outb )

      implicit none
      include 'STARMAN_INC'

      integer   nx             	!i: Input array X size
      integer   ny             	!i: Input array Y size
      integer*2 kdata(nx,ny)   	!i: Input array
      integer   ix             	!i: X position of start of part of line
      integer   iy             	!i: Y position of line
      integer   n              	!i: Number of segments to move
      real      out(n)         	!o: Output vector
      real      sc		!i: Scale
      real	ze		!i: Zero
      integer   inv		!i: Invalid pixel flag
      integer*2 outb(n)		!i/o: Invalid pixel map
C--
      integer k, l
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         l = kdata(ix+k-1,iy)
         if ( l.eq.inv ) outb(k) = 1
         if ( l.lt.0 ) l = l + 65536
         out(k) = sc*real(l) + ze
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MOVSR -- Move a part of a line of an short array to a real vector
C
C     alan penny        ral                    1988-11-24

      subroutine un_movsr ( kdata, nx, ny, ix, iy, out, n, sc, ze,
     +                      inv, outb )

      implicit none
      include 'STARMAN_INC'

      integer   nx             	!i: Input array X size
      integer   ny             	!i: Input array Y size
      integer*2 kdata(nx,ny)   	!i: Input array
      integer   ix             	!i: X position of start of part of line
      integer   iy             	!i: Y position of line
      integer   n              	!i: Number of segments to move
      real      out(n)         	!o: Output vector
      real      sc		!i: Scale
      real	ze		!i: Zero
      integer   inv		!i: Invalid pixel flag
      integer*2 outb(n)		!i/o: Invalid pixel map
C--
      integer k, l
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         l = kdata(ix+k-1,iy)
         if ( l.eq.inv ) outb(k) = 1
         out(k) = sc*real(l) + ze
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_MOVRR -- Move a part of a line of a real array to a real vector
C
C     alan penny        ral                    1988-11-24

      subroutine un_movrr ( data, nx, ny, ix, iy, out, n, sc, ze,
     +                      rinv, outb )

      implicit none
      include 'STARMAN_INC'

      integer   nx              !i: Input array X size
      integer   ny              !i: Input array Y size
      real      data(nx,ny)     !i: Input array
      integer   ix              !i: X position of start of part of line
      integer   iy              !i: Y position of line
      integer   n               !i: Number of segments to move
      real      out(n)          !o: Output vector
      real      sc		!i: Scale
      real	ze		!i: Zero
      real	rinv		!i: Invalid pixel flag
      integer*2 outb(n)		!i/o: Invalid pixel map
C--
      integer k
      real v
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         v = data(ix+k-1,iy)
         if ( v.eq.rinv ) outb(k) = 1
         out(k) = sc*v + ze
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_LIN1 -- Linearise a line for 1987 AAT fault
C
C    a j penny             stsci               1987 jul 18

      subroutine un_lin1 ( data, abias, alpha, n )

      implicit none
      include 'STARMAN_INC'

      integer n		!i:   No of values to do
      real data(n)	!i/o: Numbers to be linearised
      real abias	!i:   Bias number
      real alpha	!i:   Linearising constant
C--
      real tdata
      integer i
Cbegin


      if ( ST_FAILED ) return

      do i = 1, n
         tdata = abs(1.0+4.0*alpha*(data(i)-abias))
         data(i) = abias + (sqrt(tdata)-1.0)/(2.0*alpha)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UN_CORR1 -- Correct 1987 AAT rows and column faults
C
C alan penny             ral               1988-12-23

      subroutine un_corr1 ( rimage, inv, nxi, nyi, inval )

      implicit none
      include 'STARMAN_INC'

      integer   nxi			!i: X size of image
      integer   nyi			!i: Y size of image
      real      rimage(nxi,nyi)		!i/o: image
      integer*2 inv(nxi,nyi)		!i: Bad pixel map
      integer   inval			!i: Invalid flag
C--
      real s, value
      integer i, j, jrow, i1, i2, j1, j2, is
      integer irowb(15), iroww(15), nxd, nrows
      data nrows, nxd /15,320/
      data irowb /20,29,49,77,254,256,258,280,311,314,333,354,406,
     1            409,481/
      data iroww /21,30,50,78,255,257,259,281,312,316,334,355,408,
     1            410,482/
Cbegin


      if ( ST_FAILED ) return

      do jrow = 1, nrows						!Defect interpolation
         j1 = irowb(jrow)
         j2 = iroww(jrow)
         do i = 1, nxd
            s = 0.0
            value = 0.0
            do j = j1, j2
               if ( inv(i,j).ne.inval) then
                  s = s + 1.0
                  value = value + rimage(i,j)
               endif
            enddo
            if ( s.gt.0.5 ) value = value/s
            do j = j1, j2
              rimage(i,j)  = value
            enddo
         enddo
      enddo

      i1 = 315								!and the vertical pair...
      i2 = 316
      do j = 356, nyi
         value = 0.0
         is = 0
         if ( inv(i1,j).ne.inval ) then
            value = value + rimage(i1,j)
            is = is + 1
         endif
         if ( inv(i2,j).ne.inval ) then
            value = value + rimage(i2,j)
            is = is + 1
         endif
         if ( is.eq.2 ) value = value/2.0
         rimage(i1,j) = value
         rimage(i2,j) = value
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UNCCD -- (Program) Remove instrument effects from a raw CCD image
C
C     a j penny               ral                      1988-11-21

      subroutine unccd ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_unccd

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UNCCDSUB.FOR
C
C   Contains:-
C
C BAVGR      Mean and standard deviation of a real badpix array
C BLIMR      Bad pixel element set if data element outside limits
C BPTAVGR    Mean and std dev of part of a real badpix array



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BAVGR -- Mean and standard deviation of a real badpix array
C
C   a j penny                 dao           1988-04-25

      subroutine bavgr ( a, ib, n, av, sd, ng )
      implicit none
      integer	n	!i: no of elements in vector
      real	a(n)	!i: input vector
      integer*2 ib(n)   !i: input bad pixel flag
      real	av	!o: Mean
      real	sd	!o: Standard deviation
      integer   ng	!o: No of good points
C--
      integer j
      double precision ds, dss, dn
Cbegin


      av = 0.0
      sd = 0.0
      ng = 0
      if ( n.lt.1 ) return

      ds = 0.0
      dss = 0.0
      do j = 1, n
         if ( ib(n).eq.0 ) then
            ds = ds + a(j)
            dss = dss + a(j)*a(j)
            ng = ng + 1
         endif
      enddo

      if ( ng.ne.0 ) then
         dn = dble(ng)
         av = ds/dn
         if ( ng.gt.1 ) then
            dss = ( dss-ds*ds/dn)/(dn*(dn-1.0d0))
            if ( dss.gt.1.0e-20 ) sd = sqrt(dss)
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BLIMR -- Bad pixel element set if data element outside limits
C
C alan penny          ral                    1988-12-29

      subroutine blimr ( a, ib, n, lolim, uplim )

      implicit none
      integer   n		!i: No of points
      real      a(n)		!i: Data
      integer*2 ib(n)		!i/o: Bad pixel map
      real      lolim		!i: Lowest allowed value
      real      uplim		!i: Highest allowed value
C--
      integer j
Cbegin

      do j = 1, n
         if ( a(j).lt.lolim .or. a(j).gt.uplim ) ib(j) = 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BPTAVGR -- Mean and std dev of part of a real badpix array
C
C   a j penny                 dao           1988-04-25

      subroutine bptavgr ( a, ib, nx, ny, nxs, nxe, nys, nye, av, sd,ng)
      implicit none
      integer	nx		!i: x size of array
      integer	ny		!i: y size of array
      real	a(nx,ny)	!i: input array
      integer*2 ib(nx,ny)	!i: input bad pixel flag
      integer   nxs		!i: X start of area to use
      integer   nxe		!i: X end   of area to use
      integer   nys		!i: Y start of area to use
      integer   nye		!i: Y end   of area to use
      real	av		!o: Mean
      real	sd		!o: Standard deviation
      integer   ng		!o: No of good points
C--
      integer j, k
      double precision ds, dss, dn
Cbegin

      av = 0.0
      sd = 0.0
      ng = 0

      if ( nx.lt.1 .or. ny.lt.1 .or. nxs.lt.1 .or. nxe.lt.1 .or.
     +     nys.lt.1 .or. nye.lt.1 ) return

      ds = 0.0
      dss = 0.0
      do k = nys, nye
         do j = nxs, nxe
            if ( ib(j,k).eq.0 ) then
               ds = ds + a(j,k)
               dss = dss + a(j,k)*a(j,k)
               ng = ng + 1
            endif
         enddo
      enddo

      if ( ng.ne.0 ) then
         dn = dble(ng)
         av = ds/dn
         if ( ng.gt.1 ) then
            dss = ( dss-ds*ds/dn)/(dn*(dn-1.0d0))
            if ( dss.gt.1.0e-20 ) sd = sqrt(dss)
         endif
      endif


      end

