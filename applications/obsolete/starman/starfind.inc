C
C
C  Data tables
C
C         xy      Output results
C                  1 = x posns 
C                  2 = y posns
C                  3 = height above sky
C                  4 = utest value
C                  5 = flag for type 1:single 2:nipple 3:ellipse 
C                                    11:single-inval 12:nip-inval 
C                                    13:ellip-inval 4:weeded ellipse centre 
C                                    -1:ellipse weeded out. 
C                  6 = elliptical companion number (=0 if none)
C
C         fp       elliptical stars info:
C                       1 = 0/1 for not this one/this one (basic
C                           check is: (fp(10)=5 and fp(11) more than 7))
C                       2,3 = x,y posn of 1st star
C                       4 = utest value "  "   "
C                       5 = peak value  "  "   "
C                       6,7 = x,y posn of 2nd star
C                       8 = utest value "  "   "
C                       9 = peak value  "  "   "
C                       10 = no of lowerings ellipticity found in (
C                            to be real this must be 5. Less than 5
C                            = not significant; 6 = flag for patch
C                            not really elliptical since at image edge, or
C                            contains another star)
C                       11 = number of utest points in double image.
C                            Must be more than 7 for real image.
C         xp       x posns
C         yp       y posns
C         wp       flux values
C         zp       U map values
C         vp       flag for type - 1:single 2:nipple 3:ellipse 
C                                  [vp is also used as temporary work 
C                                  space in fi_smear s/r]
C 
C Data tables sizes
C
C         nelli    elliptical stars info table: no of cols
C         numtot   input/output tables: max poss no of rows
C         numxy    results table: number of cols

C  Working images
C
C         imu      input image (un INVAID ed)
C         map      U-test image (0=certain, 10000=90% chance of real diff)
C         asky     (sky+std dev) image
C         msky     (sky) image
C
C         wc       Peak locations image (pixel value meanings:-
C                      0  not significant value
C                      1  newly above significance level
C                      2  newly above significance level, and touching old
C                         level significance level pixels
C                      3  contiguous to star peak at significance level
C                      4  star peak loaction
C         wn      Work space (shares area of wc, and used afterwards in fi_nipple)
C

      integer	nelli		!elliptical stars info table: no of cols
      parameter ( nelli=11 )
      integer	numxy		!results table: number of cols
      parameter ( numxy=6 )

      integer   nxsd		!Sky estimating box X size
      integer   nysd		!Sky estimating box Y size
      integer   nxsky		!Sky estimates array X size
      integer   nysky		!Sky estimates array Y size
      integer	numtot		!Max poss no of stars allowed
      integer   numf		!No of stars found (ellips count as 1)
      integer   numfg		!No of stars found (ellips count as 2) [numf =+ some]
      integer   numfp		!No of stars found (ellips count as 2) with close pairs weeded
				! [numfg-some]
      integer	numout		!No of stars output to output file (weed out multiple stars in 
				! invalid peaks) [numfp-some]
      real	plimit		!Probable limit
      real	wlimit		!Wing probable limit
      real	radius		!Radius of stars
      real	elliplim	!Star ellipticity bigger than this for star to be elliptical
      real	smalim		!Star semi-major axis bigger than this for star to be elliptical
      integer   inform		!Inform control parameter(0=none,1=some,2=full)
      integer   dosky		!Sky finding type
      integer   lcheck(10001)	!Flags for U-test map pixels with these step values

      integer   ipxy		!Pointer to results table
      integer   ipxp		!Pointer to X posns table
      integer   ipyp		!Pointer to Y posns table
      integer   ipzp		!Pointer to U maps table
      integer   ipwp		!Pointer to Fluxs table
      integer   ipvp		!Pointer to types table
      integer   ipfp		!Pointer to elliptical stars info table

      integer   ipimu		!Pointer to input image with INVALIDs removed image
      integer   ipwb		!Pointer to U test image 
      integer   ipwc		!Pointer to peak locations image
      integer   ipwd		!Pointer to (sky) image 
      integer   ipwe		!Pointer to (sky+std) image 

      integer   ipwf		!Pointer to sky_1 s/r workspace array 1
      integer   ipwg		!Pointer to sky_1 s/r workspace array 2

      integer   ipoutu		!Pointer to optional output U-test image

      character*30 title	!Output table title

      logical	smear		!Do the rejection of smeared doubles
      logical	ellip		!Search for elliptical images
      logical	faint		!Do faint companions
      logical   usave		!Save U-test image

CX      logical   fi_dum1		!Dummy to make up 4*n
CX      logical   fi_dum2		!Dummy to make up 4*n
CX      logical   fi_dum3		!Dummy to make up 4*n


      common /finda/ numtot,  numout,  radius,  numfg,  dosky,
     +               nxsd,    nysd,    nxsky,   nysky,  plimit, 
     +               inform,  wlimit,  numf,    numfp,  elliplim, 
     +               smalim,  lcheck,  ipimu,   ipxy,   ipxp, 
     +               ipyp,    ipwp,    ipvp,    ipzp,   ipfp, 
     +               ipwb,    ipwc,    ipwd,    ipwe,   ipwf, 
     +               ipwg,    ipoutu

      common /findb/ smear,  ellip, faint,  usave

      common /findc/ title

