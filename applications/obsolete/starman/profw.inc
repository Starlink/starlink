CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PROFW.INC -- (Include) Include file for PROSUBW
C
C alan penny             ral           1990 Aug

      
      integer	nwmax		!Max no of wing profile points
      parameter ( nwmax=500 )

      integer	jff(4)		!Control parameter for wing fit 
				! (0=fix;1=vary)
      real	base		!Wing profile base fit
      real	height		!Wing profile height
      real	qh		!Wing profile fractional height
      real	qp		!Wing profile power
      real	qr		!Wing profile radius
      real	x(nwmax)	!Radial profile X values
      real	y(nwmax)	!Radial profile Y values
      real	apnum(nwmax)	!Radial profile no of points at distances
      integer	ntot		!No of points in radial profile
      real	xc(nwmax)	!Hand made radial profile X values
      real	yc(nwmax)	!Hand made radial profile Y values
      integer	numc		!Hand made radial profile no of points
      real	xmin		!Starting X posn to plot from
      real	xmax		!End X posn to plot to

      logical	firstf		!First wing fit to current data?
      logical   wstarcalc	!Done the star fits for wing option yet?
      logical	firstp		!First plotting of wing data?
      logical	pointsput	!Put points in hand-made radial profile?
      logical	fitted		!Fitted wing profile?
      logical   gotxrange	!Got x display range

      logical	wpr_dum1
      logical	wpr_dum2
CX      logical	wpr_dum3

      common / wingcoma / qh, qp, qr, base, height, jff,
     +                    x, y, apnum, ntot, xc, yc, numc, 
     +                    xmin, xmax

      common / wingcomb / firstf, wstarcalc, firstp, pointsput,
     +                    fitted, gotxrange, wpr_dum1, wpr_dum2
