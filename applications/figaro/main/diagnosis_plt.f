      subroutine diagnosis_plt(results,resvar,fitsta,yref,xref,ptitle,
     :        xlab,ylab,error_bars,xplot,yplot,x_err,y_err,ifsoft)
*+
* Name:
*    DIAGNOSIS_PLT

* Invocation:
*    CALL DIAGNOSIS_PLT(RESULTS,RESVAR,FITSTA,YREF,XREF,PTITLE,
*             XLAB,YLAB,ERROR_BARS,XPLOT,YPLOT,X_ERR,Y_ERR,IFSOFT)

* Purpose:
*   To plot the diagnosis and errors where applicable.

* Description:
*   To plot the diagnosis and errors where applicable. This will plot
*  the values of the parameters from different lines at the same
*  cross-section against each other, starting its search at
*  cross-section one.
*
*  Arguments(given):-
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube"
*     YREF = INTEGER (Given)
*        Pointer to Y values (in cube)
*     XREF = INTEGER (Given)
*        Pointer to X values (in cube)
*           If XREF or YREF is -ve, it is taken as a reference to the
*           variance array.
*     PTITLE = CHARACTER*(*) (Given)
*        Title for plot
*     XLAB = CHARACTER*(*) (Given)
*        X label for plot
*     YLAB = CHARACTER*(*) (Given)
*        Y label for plot
*     ERROR_BARS = LOGICAL (Given)
*        If to plot Y error bars
*     XPLOT(NYP) = REAL ARRAY (Given)
*        Work array
*     YPLOT(NYP) = REAL ARRAY (Given)
*        Work array
*     X_ERR(NYP) = REAL ARRAY (Given)
*        Work array (required for routine PLDIAGNOSIS called by this)
*     Y_ERR(NYP) = REAL ARRAY (Given)
*        Work array
*     IFSOFT = LOGICAL (Given)
*        If plot in softcopy
* Global variables:
*     MXPARS,NYP,NXP = INTEGER (Given)
*        dimensions of results block (include file arc_dims)
*     NCNTRL = INTEGER (Given)
*        Length of element of fitsta array (include file arc_dims)
*
* Authors:
*   T.N.Wilkins Manchester
* History:
*   New algorithm, T.N.Wilkins IOA Cambridge 2/4/90
*   Changes for new results structure, TNW/IOA 1-8/7/91
*-
      implicit none
      include 'arc_dims'
      integer yref,xref
      logical ifsoft
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      character*(*) ptitle,xlab,ylab
      logical error_bars
      integer line,xsect
      integer status
      real xplot(nyp),yplot(nyp)
      real x_err(nyp),y_err(nyp)
      real rhnwin,rcen
      integer npts,get_parnum,ppos
      include 'status_inc'
      character*25 leg1
      logical par_quest,first
      integer ystart,yend,len1

      xsect=1

* Loop while XSECT is less than number of cross-secions

      do while (xsect.le.nxp)
        line = 1
        npts = 0
        first = .true.
        do line = 1, nyp
          call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)

*    Check for successful fit

          if(deccntr(FIT_STAT).eq.1) then
            npts=npts+1
            if(yref.gt.0) then
              yplot(npts)=results(yref,line,xsect)
              y_err(npts)=sqrt(resvar(yref,line,xsect))
            else
              yplot(npts)=sqrt(resvar(-yref,line,xsect))
              y_err(npts)=0.0
            end if
            if(xref.gt.0) then
              xplot(npts)=results(xref,line,xsect)
            else
              xplot(npts)=sqrt(resvar(-xref,line,xsect))
            end if

*         For the first data point for a given cross-section range we
*         will get the values of NWINDOW and YSTART (probably the same
*         as XSECT).

            if(first) then
              ppos = get_parnum('Space1_pos')
              rhnwin = resvar(ppos,line,xsect)
              rcen = results(ppos,line,xsect)
              if(rhnwin.gt.0.0) rhnwin = sqrt(rhnwin)
              ystart = nint(rcen - rhnwin + 0.5)
              yend = nint(rcen + rhnwin - 0.5)
              first = .false.
            end if
          end if
        end do

*    Do we have any data points? If so we will plot them

        if(npts.gt.0) then
          len1 = 0
          call chr_fill(' ',leg1)
          call chr_putc('Cross-section',leg1,len1)
          call encode_range(' ','s',ystart,yend,leg1,len1)

*     Create plot

          call pldiagnosis(npts,xplot,yplot,x_err,y_err,ptitle,xlab,
     :             ylab,error_bars,leg1,'y',ifsoft,.false.)

*      If exit required, then set XSECT to a value greater
*      than MXPARS

          if(.not.par_quest('Go to next plot',.true.)) xsect=nxp+1
        else
          call par_wruser('No data for this plot, will go onto next'
     :        ,status)
        end if

*   Move onto next range of cross-sections

        xsect = max(xsect+1,(yend + 1))
      end do
      end
