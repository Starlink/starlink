CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADDSTARS.INC -- Include file for ADDSTARS

      integer	ipo		!Pointer to output map
      integer	ipxy		!Pointer to input table
      integer	tbx5		!No of columns in input table (inc 5 for names)
      integer	tby		!No of rows in input table
      integer	iptmap		!Pointer to input profile map
      integer	mx		!X size of input profile map
      integer   my		!Y size of input image
      real	bsm		!Pixel value scale of input profile map
      real	bzm		!Pixel value zero of input profile map
      integer   magnifk		!Default magnification of input profile map
      integer   mapxk		!Default X size of map in input profile map
      integer   mapyk		!Default Y size of map in input profile map
      integer	mz		!Z size (Max no of maps) of input profile map
      real	prof(9)		!Profile from input profile map
      real	stargain	!Factor to convert input star heights to
				!to flux for calculating Poisson noise
      real	sky		!Sky level to add
      real	skygain		!Factor to convert added sky to level to
				!give Poisson noise
      real	pixnoise	!Noise to add to output image (std dev)
      integer   nran		!Random number generator seed

      character*3 add		!Add ('ADD') or subtract ('SUB') stars?
      character*6 outtype       ! Output image type (REAL,SHORT)

      logical   starpois	!Add star noise
      logical   skypois		!Add skynoise
      logical	addim		!Add created image to an input one?
      logical   domap		!Use the input profile map map values?
      logical	pboss		!Does the input profile override any profile
				!from the star list?

      logical  ad_dum1		!4*n logicals
      logical  ad_dum2
      logical  ad_dum3

      common /addsa/ ipo,   
     +               ipxy,    tbx5,      tby,       iptmap,    mx, 
     +               my,      bsm,       bzm,       magnifk,   mapxk, 
     +               mapyk,   mz,        prof,      stargain,  sky, 
     +               skygain, pixnoise,  nran

      common /addsb/    addim,    domap,    pboss,   starpois,  
     +                skypois,  ad_dum1,  ad_dum2,    ad_dum3

      common /addsc/ add, outtype

