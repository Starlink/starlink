CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJOIN.INC -- Include file for IMJOIN
C
C alan penny                  ral          1991 march

      integer    maxn		 !Maximum number of input images
      parameter ( maxn=10 )

      character*6 imty(maxn)	 !Types (REAL,SHORT) of input images
      character*6 outtype	 !Type (REAL,SHORT) of output image

      integer    ipr		 !Input reference image pointer
      integer    ipl		 !Input reference load image pointer
      integer    ipi(maxn)	 !Input images pointer
      integer    ipwr		 !Working reference image pointer
      integer    ipwl		 !Working reference load image pointer
      integer    ipor		 !Output reference image pointer
      integer    ipol		 !Output reference load image pointer
      integer    ipo		 !Output image pointer
      integer    nxi(maxn)	 !X size of input images
      integer    nyi(maxn)	 !Y size of input images
      integer    nxr		 !X size of reference image 
      integer    nyr		 !Y size of reference image 
      real       bsr		 !Scale of reference image 
      real       bzr		 !Zero of reference image 
      real       invalref	 !'INVALID' flag of reference image 
      real       invalir	 !Invalid flag for real input image
      integer    invalis	 !Invalid flag for int*2 input image
      real       bso		 !Scale of output image 
      real       bzo		 !Zero of output image 
      integer    invalo		 !'INVALID' flag of int*2 output image 
      real       rinvalo	 !'INVALID' flag of real output image 
      integer    ntot		 !Number of input images
    
      integer    kmeth		 !Interpolation method (1=nearest:2=linear:3=uniform)
      integer    kshift		 !Shift method (1=full:2=xyshift)
      real       arc(6,maxn)	 !Transformation coefficents: [input image=>ref im]
      real       trc(6,maxn)	 !Transformation coefficents: [ref im => 
				 ! input image], reverse of that input
      real       scale(maxn)	 !Input images 'scale' (= 'exposure time')
      integer    kelim(2,2,maxn) !Ends in ref image of input image areas
      real       zero		 !Sky level found for input image

      logical    dozero		 !Subtract median flag
      logical    noref		 !No input reference image flag
      logical    doutref	 !Output a reference image?
      logical    doutim		 !Output an output image?

CX      logical    imj_dum1
CX      logical    imj_dum2
CX      logical    imj_dum3


      common / imjoina /    ipr,    ipl,     nxr,    nyr,   bsr,
     +                      bzr,    trc,   scale,   ipol,   ipi,
     +                     ipor,  kmeth,  kshift,   zero,  ipwr,
     +                     ipwl,    nxi,     nyi,   ntot,  kelim,
     +                      arc,    ipo,     bso,    bzo, 
     +                      invalref, invalir, invalis, invalo,  
     +                      rinvalo

      common / imjoinb /   doutim,   dozero,   noref,  doutref

      common /imjoinc / imty, outtype
