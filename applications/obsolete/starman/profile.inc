CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   PROFILE.INC   Include file for PROFILE
C
C   alan penny                ral             1990 Apr

      real pi				!Pi
      parameter ( pi=3.1415926536 )
      real hopi				!Angle/pi conversion factor
      parameter ( hopi=180.0/pi )
      real radmin			!Minimum radius allowed
      parameter ( radmin=0.3 )

      integer maxwork			!Work space limit
      parameter ( maxwork=400*400 )
      integer itslim			!Max no of iterations allowed
      parameter ( itslim=30 )

      integer tby		!No of stars
      integer lx		!Fit box X size
      integer ly		!Fit box Y size
      integer lxd		!Display box X size
      integer lyd		!Display box Y size
      integer lxm		!Profile map/resids X array size (LX*MAGNIF)
      integer lym		!Profile map/resids Y array size (LY*MAGNIF)
      integer lxr		!Old value of LX
      integer lyr		!Old value of LY
      integer mx		!X size of 3-D profile map array
      integer my		!Y size of 3-D profile map array
      integer mz		!Z size of 3-D profile map array
      integer mapx		!X size of Z plane of 3-D profile map array
      integer mapy		!Y size of Z plane of 3-D profile map array
      integer magnif		!Magnification of Z plane of 3-D profile map 
				! array
      integer lxw		!X size of wing area
      integer lyw		!Y size of wing area
      integer lxwd		!X size of wing display area
      integer lywd		!Y size of wing display area
      integer lywn		!Y size of wing area * no of stars
      integer kfitpr		!Type out intermediate fit results (1/0=y/n)

      integer ipdata		!Pointer for solving work space
      integer ipdatar		!Pointer for solving work space
      integer ipdisp		!Pointer for display space
      integer ipdispa		!Pointer for display space
      integer ipdispb		!Pointer for display space
      integer ipmap		!Pointer for profile map
      integer iptr		!Pointer for residuals map work space
      integer ipth		!Pointer for resids filling map work space
      integer ipocc		!Pointer for resids temp work space
      integer ipocd		!Pointer for resids filling temp work space

      real    scalemap		!Scale of values in profile map
      real    damp		!Damping factor in fit
      real    ftlim		!Fit params fractional error for convergence
      real    htlim		!Fit height fractional error for convergence
      real    wdamp		!Damping factor in Gaussian wings fit
      real    wftlim		!Wing params fractional error for convergence
      real    whtlim		!Wing height fractional error for convergence
      real    sr(9)		!Present star profile parameters
      real    qbase		!Star wing profile base
      real    vol		!Volume of profile of unit height
      real    volrad		!Radius out to which 'VOL' is calculated
      real    comfac		!Image display compression factor
      real    dxfac		!"     "       X offset
      real    dyfac		!"     "       Y offset
      real    cwidth		!Width of cross to display star posns

      real    pdist(200)	!Radial distances from centre
      real    pdata(200)	!Radial residuals (data-fit)
      real    pfit(200)		!Radial fit values
      integer nptot		!Number of radial data points

      logical	donemap		!Calculated profile map after last change?
      logical	donefix		!Solved all with fixed profile?
      logical	doneres		!Profile map filled with current prof fit 
				! resids?
      logical	donestor	!Results stored with current fit + box size?
      logical	donevol		!Calculated current profile volume?
      logical	donemean	!Calculated current mean profile?
      logical	donerang	!Calculated display range?

      logical   prdsok		!Opened the display yet?

      logical   fixangle	!Fix the angle in the profile parameter?
      logical   domap		!Use the profile map in the work?
      logical   inprof		!Input star list was PROFILE program output?
      logical   inmeas		!Input star list was MEASURE program output?
      logical   fixprof(5)	!Fix the main (rmaj,rmin,power,prmaj,prmin)

      logical   dopar		!Use the parallel processors?
      logical   parloaded	!Loaded parallel processors yet?
      logical   paintspot	!Star posns displayed as spots (T) or crosses(F)?
      logical   dogrid		!Paint grid when displaying areas?


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Various storage loading size parameters

      integer tbxmax			!Max no of columns in results array
      parameter (tbxmax=50)
      integer tbx			!No of columns in results array
      parameter (tbx=38)
      integer tbxv			!No of columns+5 in results array
      parameter (tbxv=tbx+5)
      integer maxcl			!Max no of close/bright comps
      parameter (maxcl=7)
      integer maxfa			!Max no of distant/faint comps
      parameter (maxfa=8)
      integer maxbad			!Max no of bad areas
      parameter (maxbad=16)
      integer maxsum			!Sum of comps + bad areas
      parameter 
     +      (maxsum=maxcl+maxfa+maxbad)
      integer tbymax			!Max no of main stars to be done
      parameter ( tbymax=200 )
      integer maxtot			!6200 = Size of stars*(comps+bad)
      parameter (maxtot=maxsum*tbymax)


CCCCCCCCCCCCCCCCCCCCCCCCCC
C Allocating storage space for the various storage arrays. These
C values are not used in the program. They just makesure that enough
C array space is available

      integer qtbx			!Max no of columns in results array
      parameter ( qtbx=38 )
      integer qmaxcl			!Max no of br/close comps per star
      parameter ( qmaxcl=7 )
      integer qmaxfa			!Max no of faint comps per star
      parameter ( qmaxfa=8 )
      integer qmaxbad			!Max no of bad areas per star
      parameter ( qmaxbad=16 )
      integer qqsum			!Max no of (br/close + faint + bad)
      parameter  
     +  ( qqsum=qmaxcl+qmaxfa+qmaxbad )
      integer qtbymax			!Max no of stars
      parameter ( qtbymax=200 )
      integer qmaxtot			!Max no of (stars*(comps+bads))
      parameter 
     +     ( qmaxtot=qqsum*qtbymax )


CCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Header data for output file loading

      character*20 header(qtbx)		!Header data for output file loading
      data header/
     +      'X', 'Y', 'MAG', 'DX', 'DY',
     +      'ITERATIONS', 'RMS', 'NUMINVAL', 'HEIGHT', 'BASE',
     +      'MAPNUMBER', 'RX', 'RY', 'P', 'PRX',
     +      'PRY', 'THETA', 'QH', 'QR', 'QP',
     +      'NUMCOMPS','COMP1', 'COMP2', 'COMP3', 'COMP4',
     +      'COMP5', 'COMP6', 'COMP7', 'XBOX', 'YBOX',
     +      'IMPORTANCE OF COMPS', 'XSLOPE', 'YSLOPE', 'FLAG1', 'FLAG2',
     +      'FLAG3', 'XSTART', 'YSTART' /

C  Flag1 > 0=not to be used ; 1=to be used
C  Flag3 > 0 = fitted,displayed; 1 = fitted; 2=displayed; 3= neither


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Array working space for fit results and bad areas/comp info 

      character*20 wheader(6)		!Header data for output file loading
      data wheader/
     +      'X OR XS', 'Y OR YS', 'HT OR XE', 'NULL OR YE', 
     +      'STAR', '1:2:3=COMP:FC:BAREA' /

      real res(qtbx,qtbymax)		!Fit Working results. Columns as 
					! in 'HEADER'
      real com(6,qmaxtot)		!companion stars or bad areas array
					!param 6:1=comp;2=faint comp;
					!        3=bad area
					!for star params 1=x;2=y;3=height;
					!                4=null
					!for bad areas 1=xs;2=ys;3=xe;4=ye

      real resa(qtbx,qtbymax)		!As RES and COM, but used as working
      real coma(6,qmaxtot)		! space copies. These do not get 
					! loaded back into the main RES,COM

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      real inxy(2,qtbymax)		!Input X, Y positions
      real xnfix(qtbymax)		!X posns if stars fixed
      real ynfix(qtbymax)		!Y posns if stars fixed
      real ruse(qtbymax)		!real work space for pr_avstd vals
      integer nuse(qtbymax)		!int work space for pr_avstd flags

      integer lxs(qtbymax)		!X posns in main image of blh of
					!  star fit areas
      integer lys(qtbymax)		!Y posns in main image of blh of
					!  star fit areas
      integer lxsd(qtbymax)		!X posns in main image of blh of
					!  star display areas
      integer lysd(qtbymax)		!Y posns in main image of blh of
					!  star display areas

      integer lxsw(qtbymax)		!X posns in main image of blh of
					!  star wing fit areas
      integer lysw(qtbymax)		!Y posns in main image of blh of 
					!  star wing fit areas
      integer lxswd(qtbymax)		!X posns in main image of blh of
					!  star wing display areas
      integer lyswd(qtbymax)		!Y posns in main image of blh of 
					!  star wing display areas

      integer xynfix(qtbymax)		!Flag for fixing star posns

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      logical pr_dum1			!Make up to 4n logicals
      logical pr_dum2			!Make up to 4n logicals
      logical pr_dum3			!Make up to 4n logicals

      common /profa/ kfitpr,
     +               res, resa, ruse, com, coma, tby, inxy, nuse,
     +               mx, my, mz, damp, ftlim, 
     +               htlim, mapx, mapy, magnif, scalemap,
     +               lx, ly, lxr, lyr, lxm, lym, lxd, lyd, lxs, lys, 
     +               lxsd, lysd, lxw, lyw, lywn, lxsw, lysw, 
     +               lxwd, lywd, lxswd, lyswd, 
     +               wdamp, wftlim, whtlim, 
     +               sr, vol, volrad, comfac, dxfac, dyfac,
     +               xnfix, ynfix, xynfix, ipdata, ipdatar, 
     +               ipdisp, ipdispa, ipdispb, ipmap, qbase, cwidth,
     +               iptr, ipth, ipocc, ipocd, pdist, pdata, pfit, nptot

      common /profb/    domap,   fixangle,    parloaded, fixprof,
     +                donefix,    doneres,  donestor,    donevol,
     +               donemean,    donemap,  donerang,   paintspot,
     +                 prdsok,     inprof,    inmeas,      dopar,
     +                 dogrid,    pr_dum1,   pr_dum2,    pr_dum3


