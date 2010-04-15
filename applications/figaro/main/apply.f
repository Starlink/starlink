      subroutine apply(data,aa,nl,ni,mord,xlim,nwindow,kp1,
     :   tooth_map,ref_tooth,current_tooth,x_sect,xout,yin,yout,xplot,
     :   yplot,yplt2,ypref,xin1,yin1)
*+
* Name:
*    APPLY

* Invocation:
*    CALL APPLY(DATA,AA,NL,NI,MORD,XLIM,NWINDOW,KP1,
*        TOOTH_MAP,REF_TOOTH,CURRENT_TOOTH,X_SECT,XOUT,YIN,YOUT,XPLOT,
*        YPLOT,YPLT2,YPREF,XIN1,YIN1)

* Purpose:
*   Correct image for S-distortion

* Description:
*    To interpolate the values of the shifts required and apply the
*   correction to the data. Used for correction of data for
*   S-distortion.
*
* Arguments:
*    AA(MORD,MWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Coefficients of polynomials (Chebyshev)
*    NL = INTEGER (Given)
*        Number of channels in data
*    NI = INTEGER (Given)
*        Number of cross-sections in data
*    MORD = INTEGER (Given)
*        Maximum order for which room is allowed
*    KP1 = INTEGER (Given)
*        Order of polynomial + 1
*    NWINDOW = INTEGER (Given)
*        Number of windows-teeth of comb
*    XPLOT(NI) = REAL ARRAY (Given)
*        x plot array (2nd axis of data file)
*    DATA(NL,NI) = REAL ARRAY (Given and returned)
*        Intensity array
*    XLIM(2,NWINDOW) = DOUBLE PRECISION ARRAY (Given and returned)
*        Limits used for polynomials
*    TOOTH_MAP(NL,NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos's for each channel
*    REF_TOOTH(NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos in ref channel
*    CURRENT_TOOTH(NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos in current channel
*    X_SECT(NI) = DOUBLE PRECISION ARRAY (Workspace)
*        current x-sect positions
*    XOUT(NI) = DOUBLE PRECISION ARRAY (Workspace)
*        interpolated x-sect postions
*    YIN(NI) = DOUBLE PRECISION ARRAY (Workspace)
*        current counts array
*    YOUT(NI) = DOUBLE PRECISION ARRAY (Workspace)
*        interpolated counts array
*    YPLOT(NI) = REAL ARRAY (Workspace)
*        y plot buffer
*    YPLT2(NI) = REAL ARRAY (Workspace)
*        y plot of corrected data
*    YPREF(NI) = REAL ARRAY (Workspace)
*        y ref plot buffer
*    XIN1(NL) = DOUBLE PRECISION ARRAY (Workspace)
*    YIN1(NL) = DOUBLE PRECISION ARRAY (Workspace)

* Note:
*   XOUT and YOUT also used as workspace for FILL_MAP

*
*  Include files:
*   gr_inc
*     Variables referenced from common = REAL (Workspace)
*
*    TERMINAL = LOGICAL (Workspace)
*        If softcopy graphics device open
*    HARDCOPY = LOGICAL (Workspace)
*        If hardcopy graphics device open
*
*  Subroutines called
*   PGPAGE           : Clear graphics screen
*   FILL_MAP         : Fill in map of "tooth" positions
*   INTRPL           : Cubic spline interpolation
*   PLOT_CUT         : Plot a cut through the data
*   PAR_WRUSER = INTEGER (Workspace)
*        Write character string to sys$output

* Author:
*  TNW: T.N.Wilkins, Manchester/Cambridge/Durham
*  ACD: A C Davenhall, Edinburh

* History:
*  Altered to allow use of only 1 tooth, TNW 21/12/90
*  Reduce workspace required, TNW 11/6/93, 14/6/93
*  Corrected the arguments to the (revised) FILL_MAP, ACD, 19/12/00.
*-
      implicit none
      include 'SAE_PAR'
*
* import
*
      integer ni
      integer nwindow
      integer nl
      real data(nl,ni)
      integer mord
      double precision aa(mord,nwindow)
      double precision xlim(2,nwindow)
*
* local
*
      double precision tooth_map(nl,nwindow)
      double precision ref_tooth(nwindow)
      double precision current_tooth(nwindow)
      double precision x_sect(ni)
      double precision xout(ni)
      double precision yin(ni)
      double precision yout(ni)
      real xplot(ni)
      real yplot(ni)
      real yplt2(ni)
      real ypref(ni)
      double precision xin1(nl)
      double precision yin1(nl)
      integer status
      integer i,j
      character*25 chars
      integer order
      integer PLOTSTEP
      integer kp1
      double precision shift
      integer n_chan
      parameter (PLOTSTEP=200)
      include 'gr_inc'

* max order of polynomial

      integer MAX_ORD
      parameter (MAX_ORD = 10)

* max no. of points

      integer maxnpts2
      parameter (maxnpts2=2048)

      double precision athree(3*maxnpts2+3*MAX_ORD)
      double precision a3all(3*maxnpts2+3*MAX_ORD,nwindow)

*
* pivot around the central channel
*
      n_chan        = nl/2
      write(chars,'(a,i4)') 'pivot channel = ',n_chan
      call par_wruser(chars,status)
      order=kp1-1
      write(chars,'(a,i4)') 'fit order = ',order
      call par_wruser(chars,status)
*
* fill in the position of each tooth at each channel
*
      status = SAI__OK
      call fill_map(xlim,tooth_map,nl,nwindow,aa,kp1,mord,xin1,yin1,
     :     xout,yout,status,athree,maxnpts2,a3all)
*
* extract the teeth positions for the reference channel
*
      do i = 1,nwindow
        ref_tooth(i) = tooth_map(n_chan,i)
      end do
*
* fill the array of required x-sect positions up to NI x-sects
*
      do i = 1,ni
        x_sect(i) = dble(xplot(i))
        ypref(i) = data(n_chan,i)
      end do
*
* loop over channels
*
      do i = 1,nl
*
* extract the current teeth positions
*
        do j = 1,nwindow
          current_tooth(j) = tooth_map(i,j)
        end do
*
* work out XOUT the location that each crossection in the
* reference channel has in the current tooth
*
        if(nwindow.gt.1) then
          call intrpl(nwindow,ref_tooth,current_tooth,ni,x_sect,xout,
     :            status)

*   assume we have at least 1 tooth!

        else
          shift = current_tooth(1) - ref_tooth(1)
          do j = 1, ni
            xout(j) = shift + x_sect(j)
          end do
        end if
*
* read in counts for each crossection
*
        do j=1,ni
          yin(j) = data(i,j)
          yplot(j) = real(yin(j))
        end do

*  Interpolate the counts at the position XOUT
*  thereby  Applying corrections to data.
*
        call intrpl(ni,x_sect,yin,ni,xout,yout,status)
*
* Write data to stack zeroing out values which have been
* interpolated beyond the X-section limits of the data
*
        do j=1,ni
          if((xout(j).lt.1).or.(xout(j).gt.ni)) then
            yout(j) = 0
          end if
          data(i,j)= real(yout(j))
          yplt2(j) = data(i,j)
        end  do
*
* Plot out reference channel,original of current channel
* and the interpolated shifted version of current channel
*
        if((mod(i,PLOTSTEP).eq.1).and.(terminal.or.hardcopy)) then
          call plot_cut(xplot,yplot,ypref,yplt2,ni,i,'Channel'
     :         ,'Cross-sections')
        end if

* i=1,nl

      end do
      call par_wruser('Interpolation finished',status)
      end
