      subroutine flux_mark(results,resvar,fitsta,centres,xsects,npts,
     :     flux,line,three)
*+
* Name:
*    FLUX_MARK

* Invocation:
*    CALL FLUX_MARK(RESULTS,RESVAR,FITSTA,CENTRES,XSECTS,NPTS,
*          FLUX,LINE,THREE)

* Purpose:
*   Mark according to flux value

* Description:
*    To put markers onto a line centre v. xsect plot, heavier for lines
*  with greater flux.
*    The method is to put a filled circle on each point of area
*  proportional to the log of [the flux at that point (per unit slit
*  length), divided by the minimum flux (again per unit slit length) for
*  which there is a point].
*    Alternatively the strongest 1/3 of points can be marked with a
*  filled circle, and the next strongest 1/3 with an open circle.

* History:
*      T.N.Wilkins Manchester 11/8/87
*      A C Davenhall, Starlink, Edinburgh.
*      TNW 27/1/89 opt_cmn not used
*      TNW/Cambridge 12/12/90 Algorithm changed a bit, only sort for
*      three=.true.
*      ACD 15/9/00 Added a check for 'divide by zero' before scaling
*         the flux by dividing by rnwin.
*
* Arguments
*   Given):-
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube"
*     RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube" variance
*     FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*     NPTS = INTEGER (Given)
*        Dimension of last 2 arrays
*     LINE = INTEGER (Given)
*        Number of line being worked on
*     THREE = LOGICAL (Given)
*        If to give only 3 increments for
*                               marker or nomarker. Otherwise a log scale
*                               for marker size is used.
*     CENTRES(NPTS) = REAL ARRAY (Given and returned)
*        Centres (Y position)
*     XSECTS(NPTS) = REAL ARRAY (Given and returned)
*        Cross-sections (X position)
*     FLUX(NPTS) = REAL ARRAY (Workspace)
*        Flux array
* Global variables:
*     MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of results array
*
* N.B. The centres array must have the separate components of a line in
* the same order as in the results cube, i.e. component 1 first, 2
* second e.t.c.
*-
      implicit none
      include 'arc_dims'
      include 'PRM_PAR'
      integer npts,line
      real results(mxpars,nyp,nxp),xsects(npts),centres(npts),flux(npts)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      integer oxsect
      logical three
      integer third
      integer i,gauss
      real area,area_err,dummy,fluxmax,fluxmin
      real marksize
      integer get_parnum
      logical sort
      real lratio
      include 'status_inc'
      integer ngood,ixsect,ppos
      real rnwin,rcen

      oxsect = -9999
      gauss = 1

C     print4000, npts
C4000 format(1x, 'Inside FLUX_MARK, npts: ', i4)

      do i = 1, npts
C       print4001, i
C4001   format(3x, 'i: ', i4)

        ixsect = nint(xsects(i))

*     If this point has the same cross-section as the last, then it must
*     be another component of the same fit

        if(oxsect.eq.ixsect) then
          gauss = gauss + 1
        else
          ppos = get_parnum('Space1_pos')
          rcen = results(ppos,line,ixsect)
          rnwin = sqrt(resvar(ppos,line,ixsect))
          rnwin = min(rnwin,(nxp-rcen))
          rnwin = rnwin*2.0
          call decode_status(ncntrl,fitsta(1,line,ixsect),deccntr)

*     multiple or double

          gauss = 1
          oxsect = ixsect
        end if

*     Evaluate flux/cross-section

C       print3000, 'before get_flux'
C3000   format(5x, a)

        call get_flux(results,resvar,mxpars,nyp,nxp,area,1.0,area_err,
     :       deccntr(FIT_MODEL),ixsect,line,gauss)
        flux(i) = area

* Convert to flux/xsect

C       print3002, rnwin
C3002   format(5x, 'rnwin: ', 1pe15.6)

        if (abs(rnwin) .gt. 1.0e-6) then
           flux(i) = flux(i)/rnwin
        else
           flux(i) = 0.0e0
        end if
      end do

C     print3003, three
C3003 format(5x, 'three: ', l5)

      if(three) then

*   Sort into descending order of flux

        sort = .true.
        do while(sort)
          sort = .false.
          do i = 2, npts
            if(flux(i).gt.flux(i-1)) then
              dummy = flux(i-1)
              flux(i-1) = flux(i)
              flux(i) = dummy
              dummy = centres(i-1)
              centres(i-1) = centres(i)
              centres(i) = dummy
              dummy = xsects(i-1)
              xsects(i-1) = xsects(i)
              xsects(i) = dummy
              sort = .true.
            end if
          end do
        end do

* Mark the strongest 1/3 points with a filled circle

        third = npts/3

* Prevent adjustable array dimension errors, due to THIRD = 0.

        if(npts.eq.2) then
          third = 1
        else if(third.eq.0) then
          goto 550
        end if

C       print3000, 'before pgsch'

        call pgsch(1.0)
        call pgpoint(third,xsects,centres,17)

* Mark the next strongest 1/3 points with an open circle

        call pgpoint(third,xsects(third+1),centres(third+1),22)
      else

*   Ensure all points used above zero, and get max and min flux

        ngood = 0
        fluxmin = VAL__MAXR
        fluxmax = 0.0
        do i = 1, npts
          if(flux(i).gt.0.0) then
            ngood = ngood + 1
            flux(ngood) = flux(i)
            centres(ngood) = centres(i)
            xsects(ngood) = xsects(i)
            fluxmax = max(fluxmax,flux(ngood))
            fluxmin = min(fluxmin,flux(ngood))
          end if
        end do

*     If we have more than 1 good point, then we can use the markers.

C       print3000, 'before last loop'

        if(ngood.gt.1) then
          lratio = 0.01/sqrt(log10((fluxmax/fluxmin)))
          call pgsfs(1)
          do i = 1, ngood
            marksize = sqrt(log10(flux(i)/fluxmin))*lratio
            if(marksize.gt.0.0) then
              call gr_circ(xsects(i),centres(i),marksize)
            end if
          end do
        end if
      end if
 550  continue
      call pgsch(1.0)
      end
