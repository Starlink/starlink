CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is MEASURE.INC
C                  
C   alan penny                 ral                     1989-08-13    

      integer tbx			!Output table no of columns
      parameter ( tbx=51 )
      integer tbxvo			!Output table no of columns + 5
      parameter ( tbxvo=tbx+5 )
      integer SMAX			!Max no of companion stars in fit
      parameter ( SMAX=15 )
      integer	nummax			!Max no of paramters in fit
      parameter ( nummax=(3+(3*SMAX)) )

      real      radius		!Radius out to integrate for 'volume'
      real      sky   		!Mean sky level
      real      pbs   		!Map scale value
      real      pbz   		!Map zero value
      real      volk  		!Volume of profile
      real      volradk		!Radius out for profile volume
      real      zgain		!Poisson scale of image
      real      znoise		!Extra Poisson noise of image
      real      xcoeffs(3)	! X transform coeffs to be applied to input positions
      real      ycoeffs(3)	! Y transform coeffs to be applied to input positions

      integer   ipima		!Pointer for work version of input image
      integer   iptba		!Pointer for input star positions list
      integer   tbxva  		!X size+5 of input star positions list
      integer   tbya  		!Y size for input star positions list
      integer   iptbb		!Pointer for input before measured list
      integer   tbxvb  		!X size+5 of input before measured list
      integer   tbyb  		!Y size of input before measured list
      integer   iptbe		!Pointer for input extra star list
      integer   tbxve  		!X size+5 of input extra star list
      integer   tbye  		!Y size of input extra star list
      integer   iptbout		!Pointer for output star measures list
      integer   tby		!No of stars in output star measures list
      integer   maxarea		!Max image fitting box area
      integer   iprwork		!Pointer for work area
      integer   ipbwork		!Pointer for work area
      integer   ipfwork		!Pointer for work area
      integer   khead(16)	!Column posns of info in input star list
      integer   kheade(16)	!Column posns of info in extra star list
      integer   ipmap		!Pointer to profile map data
      integer   mx		!X size of profile map
      integer   my		!Y size of profile map
      integer   mz		!Z size of profile map
      integer   number		!No of stars to do
      integer   ipimcl    	!Pointer to output cleaned image
      integer   magnifk   	!Default map magnification
      integer   mapxk     	!Default map X size
      integer   mapyk     	!Default map Y size
      integer   mapnumk   	!Default map number
      integer	ipclean		!Work space for cleaned image
      integer	ipres		!Work space for results
      integer	ipinae		!Work space for original posns, heights
      integer	ipdom		!Work space for profile volumes
      integer	iprmap		!Work space for real profile map
      integer	ipha		!Work space for heights A
      integer	iphb		!Work space for heights B
      integer	ipkrem		!Work space for removed flags

      integer   numpar		!Number of parallel processors                        
      integer   kpout		!Printing line number
 
      integer   printit         !Debugging control number

      character*30 stitle	!Title to put on output table
      character*30 ctitle	!Title to put on output cleaned image
      character*20 header(tbx)	!Headers to put in output table
                               
      logical	before		!Is there a 'before measures' file?
      logical	clstore		!Store cleaned image?
      logical	domap		!Use the profile map?
      logical	esth_do		!Use the heights in the input table as
				!  first guesses?
      logical	extra		!Is there an 'extra' input stars file?
      logical	outfix(2)	!Flag to fix positions of:- 1) companion
				! stars outside fitting box; 2) as 1) but
				! on second fitting loop
      logical	pboss 		!Star list profiles override default 
				!  profile?
      logical	printall	!Print out all fit details?
      logical	recycle		!Do two fitting loops?
      logical	redocl 		!Do undone ones on cleaned image?
      logical	twos		!Put stars at centres of undone doubles?
      logical   dodisp		!Display work as proceed?
      logical	success(SMAX)	!True = main star in fit is fitted ok
      logical	cdomap(SMAX)	!Use map of stars in fit
      logical   posnfix		!Fix all positions?
      logical   htfix		!Fix all heights?
      logical   dopar		!Use parallel processors
      logical   dfirst		!Next marked box display will be the first 
      logical   dsecond 	!Next cleaned box display will be the second

      logical	me_dum1
CX      logical	me_dum2
CX      logical	me_dum3

      real	dloc(3,4)	!Bounds of display fit affected areas
      real	dlocb(3,4)	!Bounds of display fit box areas
      integer   dlocn		!Position in DLOC at present
      integer	kstar		!Number of star being done
      integer	lx		!X size of fitting area
      integer	ly		!Y size of fitting area
      integer   lxsize		!Demanded fitting box X size (0=vary)
      integer   lysize		!Demanded fitting box Y size (0=vary)
      real	base		!Fitted base in fit
      real	xslope		!Fitted X slope in fit
      real	yslope		!Fitted Y slope in fit
      real	hp(SMAX)	!Fitted star heights in fit
      real	xp(SMAX)	!Fitted star X posns in fit
      real	yp(SMAX)	!Fitted star Y posns in fit
      real	rad(SMAX)	!Mean profile radius for stars
      integer	nst		!Number of stars in a fit
      real	hpi(SMAX)	!Input star heights to a fit
      real	xpi(SMAX)	!Input star X posns to a fit
      real	ypi(SMAX)	!Input star Y posns to a fit
      real	shiftf		!Max allowed star move (in terms of mean
				! radius) before fit bad
      integer	iter		!Number of iterations done in a fit
      real	diffm		!Difference in magnitude between fit and
				! value in last fit iteration but one
      real	profk(9)	!
      real	xpor(SMAX)	!Original (input list file) star X posn
      real	ypor(SMAX)	!Original (input list file) star Y posn
      real	prof(9,SMAX)	!Profile of fitting stars
      integer	mapnum(SMAX)	!Map numbers of fitting stars
      integer	mapx(SMAX)	!Map X sizes of fitting stars
      integer	mapy(SMAX)	!Map Y sizes of fitting stars
      integer	magnif(SMAX)	!Map magnifications of fitting stars
      integer	loopnum		!1st or 2nd go through whole business
      integer	numop		!Type of operation (1=normal;2=do cleaned
				! image;3=do undone pairs on cleaned image)
      real	rchi		!Reduced chi-squared
      real      rdamp		!Damping factor
      real	errm		!Magnitude error
      integer	ninval		!Number of INVALid points in fitting box
      integer	lxs		!Fitting box BLH X posn in image
      integer	lys		!Fitting box BLH Y posn in image
      integer   inform		!Control for printing information during fitting
                                               
      data header/ 'X', 'Y', 'MAG', 'DX', 'DY',
     +            'ITERATIONS', 'CHI', 'NUMINVAL', 'HEIGHT', 'BASE',
     +            'MAPNUMBER', 'RX', 'RY', 'P', 'PRX',              
     +            'PRY', 'THETA', 'QH', 'QR', 'QP',                 
     +            'NUMCOMPS','COMP1', 'COMP2', 'COMP3', 'COMP4',    
     +            'COMP5', 'COMP6', 'COMP7', 'XBOX', 'YBOX',        
     +            'IMPORTANCE OF COMPS', 'XSLOPE', 'YSLOPE', 'FLAG1',
     +            'FLAG2',                                           
     +            'VOLUME', 'ORDER', 'MAPX', 'MAPY', 'MAGNIF',       
     +            'COMP8', 'COMP9', 'COMP10', 'COMP11', 'COMP12',    
     +            'COMP13', 'COMP14', 'DIFF LAST ITS', 'GUESS HEIGHT',
     +            'ERROR', 'DAMPING'  /
                                                                     
C                                                                    
C  Flag 1 is - loop 1 and more                                       
C         loop1       bit  meaning                                   
C                 1   dont do star                                   
C                 2   done star                                      
C                 3   mags 1 done                                    
C                 4   mags 1 to be cleaned                           
C                 5   mags 2 done                                    
C                 6   mags 2 to be cleaned                           
C                 7   dotwos done                                    
C                 8   dotwos to be cleaned
C                                                                    
C          more   9   Star box at least partially inside image
C                10   Star has merged with another - remove forever from 
C                      to do ones
C                11   Star is 'pure before' not done this fit
C                12   Star has been cleaned from image
C                                                                    
C  Flag 2 is - loop 2 :- the same                                    
C                                                                    
C                                        
                            
                                                                     
      common /ameasa/ profk,       prof,   radius,     sky,     pbs,
     +                pbz,         volk,  volradk,   iptba,   tbxva,
     +                tbya,       iptbb,    tbxvb,    tbyb,   iptbe,
     +                tbxve,       tbye,  iptbout,     tby,   kstar,
     +                ipfwork,  iprwork,    khead,  kheade, maxarea,
     +                ipbwork,  ipclean,    ipmap,      mx,      my, 
     +                mz,        number,    ipima,  ipimcl,  magnifk,   
     +                mapxk,      mapyk,  mapnumk,   ipres,     ipha, 
     +                iphb,      ipkrem,    ipdom,  iprmap,   ipinae, 
     +                zgain,     znoise,       lx,      ly,   lxsize,
     +                lysize,      base,   xslope,  yslope,       hp, 
     +                xp,            yp,      nst,     hpi,      xpi, 
     +                ypi,       shiftf,     iter,   diffm,     xpor, 
     +                ypor,     loopnum,    numop,  mapnum,     mapx, 
     +                mapy,      magnif,      lxs,     lys,     rchi, 
     +                rad,       ninval,     errm,    dloc,    dlocb,
     +                dlocn,      rdamp,   inform,   kpout,
     +                printit,   numpar,  xcoeffs, ycoeffs
 
      common /ameasac/ stitle,  ctitle
 
      common /ameasab/ esth_do,    extra,     clstore,    redocl,
     +                    twos,    domap,       pboss,    before,
     +                 recycle,   outfix,    printall,   dsecond,
     +                 success,   cdomap,     posnfix,     htfix,
     +                  dodisp,    dopar,      dfirst,   me_dum1
 
 
C Parallel processor results (equivalent to normal meanings)
 
      integer p_iter, p_ninval, p_lxs, p_lys, p_nst, p_lx, p_ly
      real    p_hp(SMAX), p_xp(SMAX), p_yp(SMAX), p_base, p_xslope,
     +        p_yslope, p_diffm, p_errm, p_rchi, p_xpor(SMAX),
     +        p_ypor(SMAX), p_rdamp
      logical p_success(SMAX)
 
      logical me_pdum1
      logical me_pdum2
      logical me_pdum3
 
      common /ameaspar/  p_iter,       p_lxs,    p_lys,    p_lx,   p_ly,
     +                   p_hp,          p_xp,     p_yp,  p_base,  p_nst,
     +                   p_xslope,  p_yslope,  p_diffm,  p_errm, p_rchi,
     +                   p_xpor,      p_ypor, p_ninval,  p_rdamp
      common /ameasparb/ p_success,   me_pdum1,   me_pdum2,   me_pdum3
