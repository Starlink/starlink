CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AVERAGE.INC
C
C  Include file for AVERAGE


      integer kmaxr			!Max no of good stars in a list that will be processed
      parameter ( kmaxr=20000 )      
      integer maxr			!Same as kmaxr -but can be changed from default
					! (See data declaration)

      real	goodmg(kmaxr)		!Mag of Nth good * in table k
      real	rdiff(kmaxr)		!Diff of mag * in k - * in 1
      real      tbnames(5,kmaxr)	!Output file star names

      integer 	knumf			!Max allowed no of input star tables
      parameter ( knumf=100 )

      integer 	ipi(knumf)		!Pointers to input tables
      integer 	tbvxi(knumf)		!No of columns (inc 5 name) in input tables
      integer 	tbyi(knumf)		!No of rows in input tables
      character*50 titlei(knumf)	!Titles (from descriptors) of input tables

      integer 	ldiff(20,knumf)		!Distribution of good diffs from 1st table
					! (in 0.05mag bins)
      integer 	numg(knumf)		!No of good close matches used to 
					! estimate mean diff from 1st table
      real 	alchi(knumf)		!Chi-squared acceptability limits at each mag step
      real      errfac(knumf)		!Ratio, for each input table, of `true' values of 
					! input errors to what they are numerically in the 
					! input tables
      real	errlim(knumf)		!A difference of a star magnitude in one table from 
					! the mean of that star from all tables which is 
					! less than this limit, cannot be used as an excuse 
					! to reject table's value whilst averaging.
      integer   numgo(knumf+1)		!Final number of stars with that number of used mags 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   !Values for star in each input table
      real 	px(knumf)		!X positions
      real 	py(knumf)		!Y positions
      real 	amag(knumf)		!Magnitudes
      real 	dx(knumf)		!Shift in X from MEASURE start position
      real 	dy(knumf)		!Shift in Y from MEASURE start position
      real 	rrx(knumf)		!Major profile radius 
      real 	rry(knumf)		!Minor profile radius 
      integer 	iter(knumf)		!No of iterations
      real 	chi(knumf)		!Chi-squared values
      integer 	ninval(knumf)		!No of INVALID pixels
      real 	comp(knumf)		!Importance of companion stare
      real 	sigma(knumf)		!Magnitude errors 
      real      faintcorr(knumf)	!Magnitude correction for faintness of star
      integer   numinv(knumf)		!No of 'invalid' pixels in fit area

      real 	avdiff(knumf)		!Mean mag zero-point difference of file from file 1
      real 	xdiffm(knumf)		!Mean X posn difference of file from file 1
      real 	ydiffm(knumf)		!Mean Y posn difference of file from file 1

      integer 	kmaxchi			!Max no of Chi-squared limits at diff mags in a file
      parameter ( kmaxchi=100 )
      real 	chil(kmaxchi,knumf)	!Chi-squared limits at diff mags in the files


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     !Data from run of mag differences with mag
      integer 	knx			!Number of magnitude bins
      parameter ( knx=50 )
      integer 	knumd			!Number of mag difference bins
      parameter ( knumd = 20 )			
      real 	difstd(knx)		!Std dev of diffs at a mag bin
      real 	diflim(knx)		!(User input) Max mag diff allowed 
					! for star in table from mean for 
					! that star to be acceptable
      integer 	ndifl(knx,knumd)	!No in (mag,mag diff) box


      integer   tbvxo			!No of columns (incl 5 for name) in output file
      parameter ( tbvxo=18 )
      integer   tbxo
      parameter ( tbxo=13 )

      integer   numcolp
      parameter ( numcolp=13 )
      integer 	colpos(numcolp,knumf)	!Number of column containing relevant info in each 
					! input table: X,Y,MAG,DX,DY,ITERATIONS,CHI,NUMINVAL,
					! IMPORTANCE OF COMPS,SIGMA,FAINTCORR, RX, RY
					! (the number ignores the 5 'name' columns
					! - thus col 1 has COLPAS = 1)
      real 	chiprin			!Factor used in listing chisqs (they are out as integers 
					! so may all be 0s,1s,2s,etc - unless multiplied by this)
      real 	dcen			!Max acceptable pixel move in X or Y for a star from 
					! MEASURE start position
      integer 	numinvlim		!Max acceptable no of 'invalid' pixels in a star fit
      integer 	numitslim		!Max acceptable no of iterations in a star fit
      real 	sigmalim		!Max acceptable input mag error 
      real	sigmamin		!Min input mag error estimate (any below this, set to this)
      real      errmul			!No of std devs of input error estimates away from mean and still ok
      real      difmul			!No of std devs of scatter between files that a mag can be away and still ok
      integer   kscatrej		!Rejection method for auto scatter (1=mag;2=err;3=deddfault)
 
      integer   ipnums			!Star locations in files table
      integer 	ipo   			!Pointer to output table
      integer 	ktot			!Total number of input tables
      integer 	tbyo			!No of rows in output table

      logical 	cenacc			!Ignore any centering error disqualification flag
      logical 	usechi			!Ignore any centering error disqualification flag
      logical   fixdiflim		!Checking ch-sq values flag
      logical   fuse(knumf)		!Use star val in list (N) flag
      logical   fprinting		!Making a print file
      logical   names			!Use star names to match stars from different files?
      logical   usesigma		!Use input mag errors as weights

      character*132	fpname		!File to print complete analysis out into
      character*50 	title		!Title to add as descriptor to output table file
      character*6 	mode            !Automatic or interactive mode (auto:inter)?

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   ! Results for a star
      real 	avpx 			!X posn 
      real 	avpy 			!Y posn
      real 	avm 			!Mag
      real 	avnum			!No of good measures
      real 	aviter 			!No of iterations
      real 	avchi 			!Chi error
      real 	avcomp 			!Importance of companions
      real 	avdx 			!X shift from input posn
      real 	avdy			!Y shift from input posn
      real 	avnuminv 		!No of invalid points
      real 	avsigma 		!Calculated mag error
      real 	avsigmad		!Mag error via file differences
      real      avfaintcorr		!Mag corr for faint stars


      logical	av_dum1			!Dummies to make 4*n logicals
      logical	av_dum2			!Dummies to make 4*n logicals


      common / avera / colpos,      iter,    ninval,    ldiff,      ipi,
     +                 tbvxi,       numg,     alchi,      chi,     chil,
     +                 difstd,  sigmalim,     ndifl,     amag,   ipnums,
     +                 avdiff,        dx,        dy,       px,       py,
     +                 ipo,       goodmg,     rdiff,     ktot,     tbyo,
     +                 chiprin,   numinv,  numinvlim, numitslim,   comp,
     +                 dcen,        maxr,     diflim,  errfac,    sigma,
     +                 sigmamin,    avpx,       avpy,     avm,    avnum,
     +                 aviter,     avchi,     avcomp,    avdx,     avdy,
     +                 avnuminv, avsigma,   avsigmad,  avfaintcorr, 
     +                 faintcorr,  numgo,   kscatrej,     rrx,      rry,
     +                 tbnames,     tbyi,     errlim,   errmul,  xdiffm,
     +                 ydiffm,    difmul

      common / averb / fuse,
     +                 cenacc,    usechi,   fixdiflim,  fprinting,
     +                 names,   usesigma,     av_dum1,    av_dum2

      common / averc / fpname, title, titlei, mode

