      subroutine seek_order(ss,kp1,order,a,x,y,npts,labelx
     : ,residuals,athree,max_kplus1,maxnpts)
*+
* Name:
*    SEEK_ORDER

* Invocation:
*    CALL SEEK_ORDER(SS,KP1,ORDER,A,X,Y,NPTS,LABELX,RESIDUALS
* ,ATHREE,MAX_KPLUS1,MAXNPTS)
* Purpose:
*  To get the order for polynomial fitting.

* Description:
*  To get the order for polynomial fitting.

* Arguments:
*      SS(KP1) = DOUBLE PRECISION ARRAY (Given)
*        Residual sums of squares
*      KP1 = INTEGER (Given)
*        Maximum order+1
*      A(MAX_KPLUS1,MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Given)
*        Coefficients
*      X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        X array
*      Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*        Y array
*      NPTS = INTEGER (Given)
*        Number of points in X and Y arrays
*      LABELX = CHARACTER*(*) (Given)
*        Plot label for X axis
*      ORDER = INTEGER (Returned)
*        Order returned for fitting
*      RESIDUALS(NPTS) = DOUBLE PRECISION ARRAY (Workspace)
*        Residuals
*      MAXNPTS = INTEGER (Given)
*        Max number of data points
*      MAX_KPLUS1 = INTEGER (Given)
*        Max order of polynomial fit
*      ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY
*        PDA_DPOLFT fit co-effs

* History:
*   Changes to graphics T.N.Wilkins 1/12/87
*   Changed to use getwork TNW 29/11/88
*   2 workspace calls combined, TNW/CAVAD 20/3/90
*   Workspace changes, TNW 18/9/90
*   PAR_CNPAR called, TNW 25/3/91
*   Minor changes, TNW 31/7/92
*   PDA support added A.J. Holloway Oct 97
*   Remove local unused variables: ACD, 28/9/00
*-
      implicit none
* import
*
      integer kp1
      integer npts
      integer maxnpts

* residual sum of squares

      double precision ss(kp1)
      double precision x(npts)
      double precision y(npts)
      character*(*) labelx
      integer max_kplus1
      integer loc_kplus1
      parameter (loc_kplus1 = 10)

      double precision a(MAX_KPLUS1,MAX_KPLUS1)
      double precision athree(3*maxnpts+3*max_kplus1)
      double precision coeff2(loc_kplus1)
*
* export
*
      integer order
*
* local (including workspace passed from above)
*
      double precision residuals(npts)
      integer kk,kp
      real value
      logical seek,given
      logical par_quest
      logical par_given
      logical qstat
      real rmaxk

*
* status
*
      integer status
* --------------------------------------------------------------------
*
* plot sum of squares versus order
*
      given = par_given('order')
      seek = .not.given

* plot sum of squares versus order if required

* and plot RssQ v Order

      call plot_order(kp1,ss)

      qstat = par_quest('Go onto next plot?',.true.)
      if(seek) then
        order = 1
        call par_wruser(' S E E K   M E N U',status)
      end if
      do while(seek)

* Copy correct coeffficients from A into coeffs

        kp = order + 1
        do kk = 1, kp
           coeff2(kk) = a(kp,kk)
        end do

* Calculate residuals for this order

        call calc_resid(coeff2,kp,npts,residuals,x,y,athree
     :       ,max_kplus1,maxnpts)


*  Display residuals.

        call pgpage
        call plot_datatn(npts,x,residuals,labelx,'Residuals',
     :            'Residuals For Order',order,x,3)
        seek = par_quest('Look at another order?',.true.)
        order = mod(order,npts-1) + 1
      end do

* get the order chosen from the user

      rmaxk = MAX_KPLUS1-1
*      call par_rdval('order',1.0,rmaxk,3.0,' ',value)
      call par_gdr0r('order',3.0,1.0,rmaxk,.false.,value,status)

* Cancel value, so can re-prompt if required (needed for ADAM version)

*      call par_cnpar('order')
      call par_cancl('order',status)
      order = nint(value)
      if(given) then

* Copy correct coeffficients from A into coeffs

        kp = order + 1
        do kk = 1, kp
          coeff2(kk) = a(kp,kk)
        end do

* Calculate residuals for this order

        call calc_resid(coeff2,kp,npts,residuals,x,y,athree
     :       ,max_kplus1,maxnpts)

*  Display residuals.

        call pgpage
        call plot_datatn(npts,x,residuals,labelx,'Residuals',
     :            'Residuals For Order',order,x,3)
      end if
      end
