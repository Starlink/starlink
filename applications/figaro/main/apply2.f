      subroutine apply2(data,aa,nl,ni,mord,xlim,nwindow,kp1,
     :     tooth_map,graphics,xin1,yin1,xout,chann,yin,yout,
     :     xplot,yplot,ypref,ref_tooth,current_tooth)
*+
* Name:
*    APPLY2

* Invocation:
*    CALL APPLY2(DATA,AA,NL,NI,MORD,XLIM,NWINDOW,KP1,TOOTH_MAP,
*            GRAPHICS,XIN1,YIN1,XOUT,CHANN,YIN,YOUT,XPLOT,
*            YPLOT,YPREF,REF_TOOTH,CURRENT_TOOTH)

* Purpose:
*   Correct image for line curvature

* Description:
*    To interpolate the values of the shifts required and apply the
*   correction to the data. Used for correction of data for
*   line curvature
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
*    DATA(NL,NI) = REAL ARRAY (Given and returned)
*        Intensity array
*    XLIM(2,NWINDOW) = DOUBLE PRECISION ARRAY (Given and returned)
*        Limits used for polynomials
*    GRAPHICS = LOGICAL (Given and returned)
*        If plots to be made (on already open device)
*    XIN1(NI) = DOUBLE PRECISION ARRAY (Workspace)
*    YIN1(NI) = DOUBLE PRECISION ARRAY (Workspace)
*    TOOTH_MAP(NI,NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos's for eacg channel
*    XOUT(NL) = DOUBLE PRECISION ARRAY (Workspace)
*        interpolated x-sect postions
*    CHANN(NL) = DOUBLE PRECISION ARRAY (Workspace)
*        current x-sect positions
*    YIN(NL) = DOUBLE PRECISION ARRAY (Workspace)
*        current counts array
*    YOUT(NL) = DOUBLE PRECISION ARRAY (Workspace)
*        interpolated counts array
*    XPLOT(NL) = REAL ARRAY (Workspace)
*        x plot buffer
*    YPLOT(NL) = REAL ARRAY (Workspace)
*        y plot buffer
*    YPREF(NL) = REAL ARRAY (Workspace)
*        y ref plot buffer
*    REF_TOOTH(NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos in ref channel
*    CURRENT_TOOTH(NWINDOW) = DOUBLE PRECISION ARRAY (Workspace)
*        tooth pos in current channel
*
*  Subroutines called
*   PGPAGE           : Clear graphics screen
*   FILL_MAP         : Fill in map of "tooth" positions
*   INTRPL           : Cubic spline interpolation
*   PLOT_CUT         : Plot a cut through the data
*   PAR_WRUSER = INTEGER (Workspace)
*        Write character string to sys$output

* History:
*   Ref_tooth & current_tooth passed as workspace, 19/5/89
*   Minor efficiency changes, TNW 8/6/92
*   Workspace reduced, TNW 11/6/93, 14/6/93
*  Corrected the arguments to the (revised) FILL_MAP, ACD, 19/12/00.
*-
*   --------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
*
* import
*
      logical graphics
      integer ni
      integer nwindow
      integer nl
      real data(nl,ni)
      integer mord
      double precision aa(mord,nwindow)
      double precision xlim(2,nwindow)

* local

      double precision tooth_map(ni,nwindow)
      double precision chann(nl)
      double precision xout(nl)
      double precision yin(nl)
      double precision yout(nl)
      real xplot(nl)
      real yplot(nl)
      real ypref(nl)
      double precision xin1(ni)
      double precision yin1(ni)
      double precision ref_tooth(nwindow)
      double precision current_tooth(nwindow)
      integer status
      integer n_xsec,kp1,i,j
      character*23 chars
      integer PLOTSTEP
      parameter (PLOTSTEP=20)

* max order of polynomial

      integer MAX_ORD
      parameter (MAX_ORD = 10)

* max no. of points

      integer maxnpts2
      parameter (maxnpts2=2048)

      double precision athree(3*maxnpts2+3*MAX_ORD)
      double precision a3all(3*maxnpts2+3*MAX_ORD,nwindow)

      status = SAI__OK
*
* pivot around the central x-section
*
      n_xsec        = ni/2
      write(chars,'(a,i4)') 'pivot x-section = ',n_xsec
      call par_wruser(chars,status)
      write(chars,'(a,i4)') 'fit order = ',(kp1-1)
      call par_wruser(chars,status)
*
* fill in the position of each tooth at each x-sect
*
      call fill_map(xlim,tooth_map,ni,nwindow,aa,kp1,mord,xin1,yin1,
     :     xout,xout,status,athree,maxnpts2,a3all)
*
* extract the teeth position for the reference channel
*
      do i = 1,nwindow
        ref_tooth(i) = tooth_map(n_xsec,i)
      end do
*
* fill the array of required channel positions up to NL channels
*
      do i =1,nl
        chann(i) = dble(i)
        xplot(i) = real(i)
        Ypref(i) = data(i,n_xsec)
      end do
*
* loop over x-sects
*
      do i = 1, ni
*
* extract the current teeth positions
*
        do j = 1,nwindow
          current_tooth(j) = tooth_map(i,j)
        end do
*
* work out XOUT the location that each channel in the
* reference channel has in the current tooth
*
        call intrpl(nwindow,ref_tooth,current_tooth,nl,chann,xout,
     :            status)
*
* read in counts for each crossection, saving old data for plotting
*
        do j=1,nl
          yplot(j) = data(j,i)
          yin(j) = dble(yplot(j))
        end do
*
*  Interpolate the counts at the position XOUT
*  thereby  Applying corrections to data.
*
        call intrpl(nl,chann,yin,nl,xout,yout,status)
*
* write data to stack zeroing out values which have been
* interpolated beyond the channel limits of the data
*
        do j=1,nl
          if((xout(j).lt.1.0d0).or.(xout(j).gt.dble(nl))) then
            yout(j) = 0.0d0
          end if
          data(j,i)= real(yout(j))
        end  do
*
* Plot out reference channel,original of current channel
* and the interpolated shifted version of current channel
* Unlike APPLY, we can pass DATA(1,I) rather than copying to
* YPLT2
*
        if((mod(i,PLOTSTEP).eq.1).and.(graphics)) then
          call plot_cut(xplot,yplot,ypref,data(1,i),nl,i,
     :         'Cross-section','Channels')
        end if

* i=1,nl

      end do
      call par_wruser('Interpolation finished',status)
      end
