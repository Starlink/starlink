      subroutine fill_map(xlim,map,nl,nwindow,aa,kp1,mord,xin,yin,xout
     :    ,yout,status,athree,maxnpts,a3all)
*+
* Name:
*    FILL_MAP

* Invocation:
*    CALL FILL_MAP(XLIM,MAP,NL,NWINDOW,AA,KP1,MORD,XIN,YIN,XOUT
*         ,YOUT,STATUS,ATHREE,MAXNPTS,A3ALL)

* Purpose:
*   Evaluate polynomials at given locations

* Description:
*   Fill in the map of tooth position using the window limits supplied
*   in the array XLIM for each of the NWINDOW teeth and the Chebyshev
*   coefficients supplied in aa. The Chebyshev polynomials are
*   evaluated to give in-range points, and spline extrapolation is used
*   for out-of-range points.
*   The array map provides a set of fiducial points for geometrical
*   correction of the image

* Arguments:
*    XLIM(2,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Limits of polynomial
*    MAP(NL,NWINDOW) = DOUBLE PRECISION ARRAY (Returned)
*        Crossection pos of window
*    NL = INTEGER (Given)
*        Number of channels in data
*    NWINDOW = INTEGER (Given)
*        Number of windows (lines)
*    AA(MORD,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Chosen coeffs for each window
*    KP1 = INTEGER (Given)
*        Order+1
*    MORD = INTEGER (Given)
*        Maximum order allowed (array dim)
*    XIN(NL) = DOUBLE PRECISION ARRAY (Workspace)
*    YIN(NL) = DOUBLE PRECISION ARRAY (Workspace)
*    XOUT(NL) = DOUBLE PRECISION ARRAY (Workspace)
*    YOUT(NL) = DOUBLE PRECISION ARRAY (Workspace)
*    STATUS = INTEGER (Given and returned)
*        Error status
*    ATHREE(3*MAXNPTS2+3*MAX_ORD) = DOUBLE PRECISION
*        PDA_DPOLFT fit co-effs
*        (MAXNPTS2 and MAX_ORD are local parameters.)
*    MAXNPTS = INTEGER (Given)
*        Max number of data points.  Note that though this argument
*        is passed it is not actually used.
*    A3ALL(3*MAXNPTS2+3*MAX_ORD,nwindow) = DOUBLE PRECISION
*        (MAXNPTS2 and MAX_ORD are local parameters.)

* Bugs:
*   Assumes axis values are 1, 2, ..., nl.

* Authors:
*   TNW: T. N. Wilkins, Durham
*   AJH: A. J. Holloway, Manchester
*   ACD: A C Davenhall, Edinburgh

* History:
*   TNW: 11/6/93 Halved tests for in/out, reduced workspace required.
*   TNW: 14/6/93 Reduced workspace more
*   AJH: October 97 Edit to support PDA co-effs array propogation.
*   ACD: 19/12/00 Revised the list of arguments in the prologue comments
*          to correspond to the actual arguments.
* ---------------------------------------------------------------------
*-
      implicit none
      include 'SAE_PAR'

* max order of polynomial

      integer MAX_ORD
      parameter (MAX_ORD = 10)

* max no. of points

      integer maxnpts2
      parameter (maxnpts2=2048)

* Import
*
      integer mord
      integer kp1,nwindow
      integer nl
      integer maxnpts
      double precision aa(mord,nwindow)
      double precision xlim(2,nwindow)
      double precision athree(3*maxnpts2+3*MAX_ORD)
      double precision a3all(3*maxnpts2+3*MAX_ORD,nwindow)
      double precision loca3(3*maxnpts2+3*MAX_ORD)

*
* export
*
      double precision map(nl,nwindow)
      integer status

* local
*
      double precision xin(nl)
      double precision yin(nl)
      double precision xout(nl)
      double precision yout(nl)
      double precision coeffs(MAX_ORD)
      double precision xstart
      double precision xend,value
      integer in
      integer outr
      integer outl
      integer line
      integer e_cpoly
      integer j,kk,k,kkk

      if(status.ne.SAI__OK) return

      do j =1,nwindow
*
* select correct coefficients
*
         do kk =1,kp1
            coeffs(kk) = aa(kk,j)
         end do

         do kkk=1,3*maxnpts2+3*mord
            loca3(kkk)=a3all(kkk,j)
         end do


*
* initialize valid range of cheby fit for each tooth
* and counters of interior and exterior channels
*
        xend   = xlim(2,j)
        xstart = xlim(1,j)
        in  =  1
        outl =  0
        outr =  0
        xin(1)=xstart
*
* loop over channels deciding if interior or exterior
*
        do k = 1,nl
          value = dble(k)
          if (value.le.xstart) then
            outl = outl + 1
            xout(outl) = value
          else if(value.ge.xend) then
            outr  = outr + 1
            xout(outr+outl) = value
          else
            in = in + 1
            xin(in) = value
          end if
        end do
        in=in+1
        xin(in)=xend
*
* evaluate the chebyshev polynomial for all interior points
*
        status = e_cpoly(xin,yin,coeffs,KP1,in,loca3,mord,
     :                   maxnpts2)
*
* evaluate the shifts for exterior points
*
        if ((outl+outr).ne.0) then
           call intrpl(in,xin,yin,outl+outr,xout,yout,status)
        endif
*
* finaly copy over the y-positions into the map
*
        line = 0
*
* interpolated points to left
*
        do kk = 1,outl
          line = line + 1
          map(line,j) = yout(kk)
        end do
*
* interior points allowing for dummy start and end points
*
        do kk = 2,(in-1)
          line = line + 1
          map(line,j) = yin(kk)
        end do
*
* exterior to right
*
        do kk = 1,outr
          line = line + 1
          map(line,j) = yout(kk+outl)
        end do
      end do
      end
