CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AUTOMAG.INC   Include file for AUTOMAG.FOR
C
C   alan penny                ral         1990 dec

      real	pi		! Pi
      parameter ( pi=3.1415926536)
      integer   nmax		!Max no of stars that can be found
      parameter ( nmax=200 )

      character*79 estra	!Exp string ra
      character*79 estdec	!Exp string dec
      character*79 estsidt	!Exp string sidereal time
      character*79 object	!Star name
      character*79 filter	!Filter name
      character*79 date		!Date of exposure
      character*20 fname(10)	!Filter names
      character*20 fsname(10)	!Filter standard file names
      character*79 filtname	!Name of descriptor with filter info
      character*79 expname	!Name of descriptor having exposure time
      character*79 sidtname	!Name of descriptor having sidereal time
      character*79 objname	!Name of descriptor having Image name
      character*79 raname	!Name of descriptor having Image RA
      character*79 decname	!Name of descriptor having Image Declination

      integer   locexp(2)	!Start and end posns of exposure time in descriptor
      integer   locfilt(2)	!Filter descriptor location start/end
      integer   locsidt(2)	!Sid T descriptor location start/end
      integer   locobj(2)	!Image name descriptor location start/end

      integer   ncol(10)	!Column number of filter `n'
      real      extval(10)	!Possible extinction values
      real      dxsky(4)        !Sky apertures X offset in pixels
      real      dysky(4)        !Sky apertures Y offset in pixels

      real	xp(nmax)	!X posns of found stars
      real	yp(nmax)	!Y posns of found stars
      real      pmag(nmax)	!`Raw' magnitudes of found stars
      integer   ntot		!No of stars found

      integer   ipwk		!Workspace for data pointer
      integer   nloop		!No of input image in process of doing
      integer   posntype	!Ra, Dec input format (1=degrees,2=radians,3=string)
      integer   totfilt		!Total number of filters loaded
      real      extinc		!Actual extinction value
      real	stardia		!Star aperture diameter
      real	skydia		!Sky aperture diameter
      integer   ipstd		!Standards file pointer
      integer   tbvx		!Standards file - no of columns (+5)
      integer   tby		!Standards file - no of rows
      integer   naper		!No of sky apertures
      real      expoff		!Exposure time offset to be added to exposure time
      real      thresh		!Threshold above sky to take
      real      avlim		!Difference from mean in mags to reject a sky measure
      integer   ibox		!Box size for largest image
      real      zgain		!Poisson scale
      real      znoise		!Zero pixel noise

      real	edec		!Exposure Declination
      real	era		!Exposure Right Ascension
      real	sdec		!Standard Declination
      real	sra		!Standard Right Ascension
      real	expt		!Exposure time
      integer   numfilt		!Filter number of exposure

      logical   isposn		!Doesimage have position?
      logical   dofilt		!Filter descriptor to be used?
      logical   badim		!Has this image failed?
      logical   imore           !Do for another image?
      logical   multiple        !Do for more than one image?
      logical   dostds		!Use standards file?
      logical   dofile		!Store results in text file
      logical   sfound		!Found std in standards file?
      logical   centre		!Gauss centre on found stars?
      logical   stdmast		!If both image and stds posn present,
				!  is std master?
      logical   dosecz		!Do secZ calcs?
      logical   noexpname	!No name for the exposure header?
      logical   noobjname	!No name for the object header?

      double precision  ddate	!Date (Julian epoch - e.g. 1990.234d0)
      double precision  dsidt	!Sidereal time - radians
      double precision  dah	!Hour angle - radians
      double precision  dlat	!Site latitude - radians
      double precision  ddec	!Declination - radians
      double precision  dra	!Right Ascension - radians
      double precision  dedec	!Std Declination - radians
      double precision  dera	!Std Right Ascension - radians
      double precision  dsdec	!Exp Declination - radians
      double precision  dsra	!Exp Right Ascension - radians


      logical au_dum1
      logical au_dum2
      logical au_dum3


      common / autoa / expt,     xp,      yp,      ntot,    ncol, 
     +                 skydia,   locfilt, locexp,  locobj,  locsidt, 
     +                 tbvx,     tby,     naper,   dxsky,   dysky, 
     +                 extval,   extinc,  totfilt, thresh,  pmag, 
     +                 posntype, nloop,   ipwk,    ibox,    stardia, 
     +                 expoff,   znoise,  ipstd,   zgain,   numfilt,
     +                 edec,     era,     sdec,    sra,     avlim

      common / autob / dofilt,     imore,    multiple,  noexpname,  
     +                 dostds,     dofile,     sfound,     centre,
     +                 stdmast,    dosecz,      badim,     isposn,
     +                 noobjname,  au_dum1,   au_dum2,    au_dum3

      common / autoc / object,   filter,   date,   
     +                 filtname, expname,  sidtname, objname,
     +                 raname,   decname,  estra,    estdec, 
     +                 estsidt,  fname,    fsname

      common / autod / dera,     dedec,    dsra,    dsdec,    dsidt, 
     +                 dlat,     ddate,    ddec,    dra,      dah

