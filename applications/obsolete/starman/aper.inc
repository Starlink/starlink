CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C APER.INC
C
C   alan penny                 ral             1990=-02-05


      integer  NST		! Star number
      integer  NSTLAST		! Star number at end of last image

      real     STAREA 		! Area in star aperture
      integer  NSTBAD 		! No of bad pixels in star
      real     STFL 		! Star flux in star aperture
      real     STRAW		! Total flux in star aperture
      real     STFERR		! std dev error in star flux
      real     STMERR		! std dev error in star mag
      real     STMAG		! Star magnitude
      real     STTOP            ! Max pixel value in star ap
      real     STAPRX 		! Star aperture Y radius
      real     XST 		! X posn of star ap
      real     YST 		! Y posn of star ap

      real     SKAREA		! Area in sky aperture/annulus
      integer  NSKBAD 		! No of bad pixels in sky aperture/annulus
      real     SKFL 		! Sky flux
      real     SKLEV 		! Sky level
      real     SKERR		! Std dev error in sky level
      real     SKAPRX 		! Sky aperture X radius
      real     ANAPRX(2)	! Sky annulus inner and outer X radii
      real     APANG		! Angle of oval aperture to X axis in degs
      real     ELLIPSE		! Ellipticity of oval aperture 
      real     XSK 		! Sky X position
      real     YSK 		! Sky Y position
      integer  ADOCENTRE	! Centering flag on aperture photometry
				! 1/2/3/4 = no centre/max pixel/centroid/gauss
      real     AGRX		! Gauss centering X fix radius
      real     AGRY		! Gauss centering Y fix radius

      real     POISV 		! Events per  pixel count
      real     NOISE 		! Extra noise in pixel over poisson
      real     ZP		! Magnitude zero point

      integer  NXRS		! X size of results store (NOT incl 5 names
				!  columns)
      integer  NYRS		! Y size of results store
      parameter ( NXRS=32 )
      parameter ( NYRS=4000 )
      real     DATAS(NXRS,NYRS)	  ! Data Results store
      character*20 AP_NAMES(NYRS) ! Names store

      integer  NCIR		! Oval notes - number done
      real     XCIR(NYRS)	! Oval notes - X position
      real     YCIR(NYRS)	! Oval notes - Y position
      real     RCIR(6,NYRS)	! Oval notes - star x radius; sky x rad
				!    sky annulus inner x rad; sky annulus outer x rad; angle; ellipticity

      logical  REMCIR(NYRS)	! Circle notes - circle removed
      logical  ANNULUS 		! Flag to use annulus (not separate aperture) 
				!  for sky
      logical  DOMEDIAN		! Flag to use median (not mean) to get sky 
				!  level
      logical  APSHOW		! Flag to show cursor apertures whilst 
				!  getting stars
      logical  FIXGRAD		! Use fixed radii on gauss centering

   
      common / apera /  stfl,     xst,        yst,      xsk,      ysk, 
     +                nstbad,     nst,       skfl,    sklev,    datas, 
     +                 noise,   poisv,         zp,   stferr,   skaprx,  
     +                staprx,  anaprx,      apang,  ellipse,    straw,
     +                 sttop,  nskbad,     starea,   skarea,     xcir,   
     +                  ycir,    rcir,       ncir,     agrx,     agry,  
     +                 skerr,  adocentre, nstlast,    stmag,   stmerr

      common / aperb /  remcir,
     +                  annulus,   domedian,      apshow,    fixgrad

      common / aperc /  ap_names
