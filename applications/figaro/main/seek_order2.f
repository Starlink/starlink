      subroutine seek_order2(ss,max_kplus1,order,a,x,y,npts,w,mode,
     :                max_fit,chosen_order,labelx,labely,title,work,
     :                athree,maxnpts)
*+
* Name:
*    SEEK_ORDER2

* Invocation:
*    CALL SEEK_ORDER2(SS,MAX_KPLUS1,ORDER,A,X,Y,NPTS,W,MODE,
*            MAX_FIT,CHOSEN_ORDER,LABELX,LABELY,TITLE,WORK,
*            ATHREE,MAXNPTS)

* Purpose:
*  To get the user to decide upon an order for polynomial fits to data.

* Description:
*   This version is the one used by CONTROL_CPOLY2 and rotuines like
*   VIG. SEEK_ORDER is used by ARC2D.
*   The plotting surface is subdivided into 2 zones. In the lower
*   zone we plot fit v postion, in the upper zone we plot
*   residuals v postion. The user has two ways open for inspectinh
*   the fits as a function of order.
*   SEQUENTIAL in which they can continuously roll through the orders
*   in ascending order. Or
*   INSPECT in which they may choose the orders to be viewed at
*   random.

* Arguments:
*   MAX_FIT = INTEGER (Given)
*         maximum number of chosen_order actually fitted
*   MODE = INTEGER (Given)
*         Mode to use in plotting residuals (for plot_data)
*   NPTS = INTEGER (Given)
*         number of X,Y points
*   SS(MAX_FIT) = DOUBLE PRECISION ARRAY (Given)
*         residual sum of squares
*   MAX_KPLUS1 = INTEGER (Given)
*         max number of chosen_order allowed
*   A(MAX_KPLUS1,MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Given)
*         cheby coeffs store with order
*   X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*         x data points
*   Y(NPTS) = DOUBLE PRECISION ARRAY (Given)
*         y data points
*   W(NPTS) = DOUBLE PRECISION ARRAY (Given)
*         weights
*   CHOSEN_ORDER(MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Given)
*         coeffs of current order
*   LABELX = CHARACTER (Given)
*         label for X axis
*   LABELY = CHARACTER (Given)
*         label for Y axis
*   TITLE = CHARACTER (Given)
*         Title for plot
*   ORDER = INTEGER (Returned)
*         order selected by user
*   WORK(NPTS) = DOUBLE PRECISION ARRAY (Workspace)
*         Workspace
*   MAXNPTS = INTEGER (Given)
*         Max number of data points
*   ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY
*         PDA_DPOLFT fit co-effs

* History:
*  Tidied a bit, TNW/CAVAD 16/8/90, 31/7/92
*  Bug fix-default value for par_qnum was integer, TNW 22/4/94
*  Corrected the calling arguments for the (revised) CALC_RESID and
*  E_CPOLY.  Consequently added additional arguments to this routine,
*  ACD 19/12/00.
*-
      implicit none
      include 'SAE_PAR'

* import
*
      integer max_fit
      integer npts
      double precision ss(max_fit)
      integer max_kplus1
      double precision a(max_kplus1,max_kplus1)
      double precision x(npts)
      double precision y(npts)
      double precision w(npts)
      double precision chosen_order(max_kplus1)
      character*(*) labelx
      character*(*) labely
      character*(*) title
*
* export
*
      integer order

* Workspace

      double precision work(npts)
      integer maxnpts
      double precision athree(3*maxnpts+3*max_kplus1)

*
* local
*

* current number of coefficients

      integer kp

* true if SEEKING

      logical seek

* QMENU answer

      integer iseek

* menu

      character*36 dict_seek(2)

* max allowed order used by Par_qnum

      real rmaxk

* par_qnum response

      real value
      integer kk

* type of plot to produce with PLOT_DATA

      integer plot_type
      integer dumi
      real dumr
      character dumc
      integer FIT

* symbolic Constant for pLOT_DATA

      parameter (FIT =1)
      logical par_quest,par_qnum,qstat
      integer status,mode,e_cpoly
      data dict_seek/
     :     'SEQUENTIAL : Scan in ascending order',
     :     'INSPECT : Inspect certain orders'/
* --------------------------------------------------------------------
*
* get soft copy device and select the base zone

      status = SAI__OK
      call gr_soft(status)

* plot sum of squares versus order
*
      call plot_order(max_fit,ss)

* offer the user the seek options

      call qmenu('Seek Menu',dict_seek,2,2,dumr,dumc,iseek,dumi,
     :      status)

* makes first order looked at 1 for sequential mode

      order = 0
      seek = .true.

      do while(seek)

* if we are inspecting user chosen orders then prompt for
* the order to be looked at next. Otherwise increment the
* order by 1.

        if(iseek.eq.2) then
          rmaxk = max_fit-1
          qstat = par_qnum('Enter the order you want to look at',1.0,
     :                rmaxk,1.0,.false.,' ',value)
          order = nint(value)
        else

* make sure that the order chosen does not exceed the nubmer of
* orders actaully fitted. Dont stop though , just reset to 1st order
* so that the user can exit when they are happy.

          order = mod(order,max_fit-1) + 1
        end if

* set the number of chosen_order for the current order

        kp = order + 1

* Copy correct coeffficients from A into chosen_order

        do kk=1,kp
          chosen_order(kk)=a(kp,kk)
        end do

* Calculate residuals for this order

        call calc_resid(chosen_order,kp,npts,work,x,y,
     :    athree,max_kplus1,maxnpts)

*  Display residuals. selecting the correct plot zone

        call pgpage
        call pgvport(0.07,0.99,0.07,0.47)

        call plot_datatn(npts,x,work,labelx,'Residuals',
     :            'Residuals for order',order,w,mode)

* select the zone for displaying the fit

        call pgvport(0.07,0.99,0.57,0.97)

* plot the actual data and superimpose the fit for this order

        plot_type = FIT
        call plot_data(x,y,npts,labelx,title,w,plot_type,labely)

*  evaluate cheby poly at each x value

        status = e_cpoly(x,work,chosen_order,kp,npts,
     :    athree,max_kplus1,maxnpts)

*  plot the poly line on the data plot

        call plot_dcurv(x,work,npts)

        seek = par_quest('Go onto next plot?',.true.)
      end do
*
* get the order chosen from the user
* this must be less than MAX_FIT the maximum order that could
* be fitted to the present data set.
*
      rmaxk = max_fit-1
      qstat = par_qnum('Enter the order you want to use',1.0,rmaxk,1.0,
     :       .false.,' ',value)
      call pgpage
      order = nint(value)
      end
