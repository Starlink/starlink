CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DUSTRING.INC        Include file for DUSTRING.FOR
C
C  alan penny                      RAL                     1990-10-15


      integer   maxrings		! Max number of noted rings allowed
      parameter ( maxrings=1000 )
      integer   tbx			!No of columns of ring information
      parameter ( tbx=9 )

      integer	nrings		! Number of rings noted
      integer	iside		!
      integer   ipw		! Pointer to working version of image
      integer   nxs             ! X start of area of interest
      integer   nxe             ! X   end of area of interest
      integer   nys             ! Y start of area of interest
      integer   nye             ! Y   end of area of interest
   
      real	xp		!X position of ring
      real	yp		!Y position of ring
      real	back		!Background for ring
      real	slopein		!slope of inner ramp of ring
      real	slopeout	!slope of inner ramp of ring
      real	percent		!percentage depth of ring
      real	rin		!inner radius of ring
      real	rout		!outer radius of ring
      real	tscale		!
      real	tzero		!
      real	res(tbx,maxrings)	!Internal table of rings

      logical	displayed	!Displayed an image?
      logical	opdisp          !Opened display?
      logical   gotimage        !Got an image?
      logical   backauto	!Auto use + load level at ring centre to background
      logical   backload	!Auto load determined level to background

      logical du_dum1, du_dum2	!Make up to 4*n logicals
      logical du_dum3


      common / dusta / nrings, iside, xp, yp, back, slopein, slopeout,
     +                 rin, rout, tscale, tzero, res, percent,
     +                 ipw,  nxs, nxe, nys, nye

      common / dustb / displayed, opdisp, gotimage, backauto, 
     +                 backload, du_dum1, du_dum2, du_dum3

