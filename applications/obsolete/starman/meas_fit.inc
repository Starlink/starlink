CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEAS_FIT.INC -- Include file for MEASURE MESUBA variables
C
C  These are the paramters for each of the (up to SMAX) fitting stars
C  in a single fit.
C
C  alan penny                    ral              90-04-14

      real      p(SMAX)		!Profile power
      real      gx(SMAX)	!Profile 1/X radius
      real      gy(SMAX)	!Profile 1/Y radius
      real      gx2(SMAX)	!Profile 1/X radius squared
      real      gy2(SMAX)	!Profile 1/Y radius squared
      real      hx(SMAX)	!Profile 1/X power radius
      real      hy(SMAX)	!Profile 1/Y power radius
      real      hx2(SMAX)	!Profile 1/X power radius squared
      real      hy2(SMAX)	!Profile 1/Y power radius squared
      real      co(SMAX)	!Profile cos(angle)
      real      si(SMAX)	!Profile sin(angle)
      real      sim(SMAX)	!Profile -1*sin(angle)
      real      qf1(SMAX)	!Profile 
      real      qf2(SMAX)	!Profile 
      real      qf3(SMAX)	!Profile 
      real      qf4(SMAX)	!Profile 
      real      theta(SMAX)	!Profile angle
      real      qh(SMAX)	!Profile gaussian height
      real      qr(SMAX)	!Profile gaussian 1/radius
      real      qp(SMAX)	!Profile power
      integer   lbox(SMAX)	!Pixel subdivisions
      real      xmstep(SMAX)	!Max X posn shift in one iteration
      real      ymstep(SMAX)	!Max Y posn shift in one iteration

      integer    numv			!No of 1-D profile intervals for
					! a star
      parameter ( numv=200 )
      real      vald(NUMV,SMAX)		!1-D profiles 

      logical	loadit(SMAX)		!Save sim eqn load vals
      logical	getit(SMAX)		!Restore sim eqn load vals

      logical   me_fdum1
      logical   me_fdum2

      common /mefit/ p, gx, gy, gx2, gy2, hx, hy, hx2, hy2, co, si, sim,
     +               qf1, qf2, qf3, qf4, theta, qh, qr, qp, lbox, vald, 
     +               xmstep, ymstep
      common /mefitb/ loadit, getit, me_fdum1, me_fdum2
