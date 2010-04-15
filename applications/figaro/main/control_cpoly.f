      subroutine control_cpoly(xpos,ypos,npts,nl,nwindow,aa,mord,kp1,
     :    xlabel,ylabel,weights,w,used,batch,a,work,athree,a3all)
*+
* Name:
*    CONTROL_CPOLY

* Invocation:
*    CALL CONTROL_CPOLY(XPOS,YPOS,NPTS,NL,NWINDOW,AA,MORD,KP1,
*         XLABEL,YLABEL,WEIGHTS,W,USED,BATCH,A,WORK,ATHREE,A3ALL)

*
* Purpose:
*  Fit Chebyshev polynomials

* Description:
*   Subroutine to fit Chebyshev polynomials to the data in the arrays
*   XPOS and YPOS.
*   There is the option of performing a weighted fit.

* Arguments:
*    XPOS(NL,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        X positions of data
*    YPOS(NL,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Y    "       "   "
*    NPTS(NWINDOW) = INTEGER ARRAY (Given)
*        Number of points in each window
*    NL = INTEGER (Given)
*        Dimension of data along windows
*    NWINDOW = INTEGER (Given)
*        Number of windows
*    MORD = INTEGER (Given)
*        Maximum order
*    XLABEL = CHARACTER*(*) (Given)
*        X plot label
*    YLABEL = CHARACTER*(*) (Given)
*        Y plot label
*    WEIGHTS(NL,NWINDOW) = DOUBLE PRECISION ARRAY (Given)
*        Weights
*    BATCH = LOGICAL (Given)
*        If running in batch mode
*    KP1 = INTEGER (Returned)
*        Order used + 1 (this is number of coefficients)
*    AA(MORD,NWINDOW) = DOUBLE PRECISION ARRAY (Returned)
*        Fit coefficients at chosen order.
*    USED(NWINDOW) = LOGICAL ARRAY (Returned)
*        Which "lines" were used
*    W(NL) = DOUBLE PRECISION ARRAY (Workspace)
*        Weights
*    A(400) = DOUBLE PRECISION ARRAY (Workspace)
*        Cheby coeffs as funtion of order
*    WORK(NL*3+MAX_KPLUS1*2) = DOUBLE PRECISION ARRAY (Workspace)
*    A3ALL(3*MAXNPTS+3*MAX_KPLUS1,NWINDOW) = DOUBLE PREC ARRAY
*        PDA fit array from PDA_DPOLFT
*    ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PREC ARRAY
*        PDA fit co-effs for particular window
* Subroutines/functions referenced:
*    CALC_RESID          : Evaluate residuals
*    COPD2D              : Copy double precision one array to another
*    FIT_CPOLY = INTEGER (Workspace)
*        Fit a Chebyshev polynomial
*    E_CPOLY             : Evaluate a Chebyshev polynomial
*    GR_HARD             : Select hard-copy graphics device
*    GR_CLEAR            : Advance to next graphics frame
*    GR_SELCT            : Select graphics device
*    PLOT_DCURV          : Plot fit on plot of points
*    PLOT_DATATN         : Plot a series of points
*    SEEK_ORDER          : Get user to decide on order for fit
*    ZERO_DBLE           : Zero double precision array
*
*    PAR_QUEST           : Obtain yes/no response from user
*    PAR_WRUSER          : Write character string to user

* History:
*   Changes to graphics 1/12/87 T.N.Wilkins
*   TNW 5/12/88 Use of COPARR, replaced 6/12/88 with COPD2D
*   TNW/Cambridge, 26/3/91 Workspace reduced.
*   TNW/Durham Jun  9 1993, Make sure don't access USED outside range
*   AJH 17 October 197, Chnage max_kplus1 to 10 to limit order.
*-
      implicit none
      include 'SAE_PAR'
* import

      integer mord
      integer nwindow,nl
      integer npts(nwindow)
      double precision xpos(nl,nwindow)
      double precision ypos(nl,nwindow)
      double precision weights(nl,nwindow)
      character*(*) xlabel,ylabel
      logical batch

* Export

      double precision aa(mord,nwindow)
      integer kp1
      logical used(nwindow)

* status

      integer status
      integer pstat

* local

      double precision work(*)
      integer kmax
      integer k,i,kk,kset
      integer fstatus,fit_cpolb,e_cpoly

* actual order used

      integer order

* max order of polynomial

      integer MAX_KPLUS1
*      parameter (MAX_KPLUS1 = 20)
*     Change due to limitation in tay2cheb

      parameter (MAX_KPLUS1 = 10)

      double precision w(nl)

* residual sum of squares

      double precision ss(MAX_KPLUS1)

* current coeffs
      integer maxnpts
      parameter (maxnpts = 2048)
      double precision coeffs(MAX_KPLUS1)
      double precision a(MAX_KPLUS1,MAX_KPLUS1)
      double precision a3all(3*maxnpts+3*MAX_KPLUS1,nwindow)
      double precision athree(3*maxnpts+3*MAX_KPLUS1)


* if weights to be used

      logical weight
      integer cheby_max
      logical par_quest
      logical plot
      logical hardplots
      include 'gr_inc'

* debug output

      character*20 chars

* passed dimensions

      cheby_max = MAX_KPLUS1

* for first window use seek mode

      k = 1
 10   continue
      used(k) = .false.
      do while(npts(k).le.2)
        k=k+1
        if(k.gt.nwindow) then
          call par_wruser('Not enough fits found!!',pstat)
          goto 500
        end if
        used(k) = .false.
      end do
      kset=k+1
      status = SAI__OK
      if(batch) then
        weight=.true.
      else
        weight = par_quest('Weight fits?',.true.)
      end if
      call gr_selct((.not.batch),status)

*  fill data arrays.  if 'weight' specified, get weight

      if (weight) then
        call par_wruser('Performing weighted fit',pstat)
        call copd2d(npts(k),weights(1,k),w)
      else
        call par_wruser('Performing unweighted fit',pstat)
        do i = 1,npts(k)
          w(i) = 1.0
        end do
      end if

*  zero out sum of squares

      call zero_dble(ss,MAX_KPLUS1)
      call zero_dble(athree,3*maxnpts+3*MAX_KPLUS1)

* do polynomial fit

      fstatus = fit_cpolb(w,npts(k),xpos(1,k),ypos(1,k),a,ss,kmax,
     :                  cheby_max,work,athree,maxnpts)

* intitial test

*  If cannot succesfully fit this set of points, then try next

      if(fstatus.ne.SAI__OK) then
        k = k + 1
        go to 10
      end if
      plot = terminal.or.hardcopy
      hardplots = hardcopy

* look for first

      order = 2
      if ( plot.and. (kmax.ge.2)) then
        call seek_order(ss,kmax,order,a,xpos(1,k),ypos(1,k),npts(k),
     :                  xlabel,work,athree,cheby_max,maxnpts)

*   Inform user of chosen order

        write (chars,'(a,i3)') 'Order returned = ',order
        call par_wruser(chars,pstat)
        if(kmax.gt.order) used(k) = .true.
      end if

* Copy correct coeffficients from A(i) into AA(i,1).

      kp1 = order+1
      do kk=1,kp1
         aa(kk,1)=a(kp1,kk)
         coeffs(kk) =a(kp1,kk)
      end do

      do kk=1,3*maxnpts+3*MAX_KPLUS1
         a3all(kk,1)=athree(kk)
      end do

* plot the centriod data for each tooth

      call gr_selct((.not.hardplots),status)
      call pgvstand
      call plot_datatn(npts(k),xpos(1,k),ypos(1,k),xlabel,ylabel
     :            ,'Distortion of tooth',1,xpos,3)

* evaluate cheby poly at each x value

      fstatus = e_cpoly(xpos(1,k),work,coeffs,kp1,npts(k),athree
     : ,max_kplus1,maxnpts)

* plot the poly line on the centriod plot

      call plot_dcurv(xpos(1,k),work,npts(k))
      if(plot.and.(.not.hardplots)) then
        plot = par_quest('Continue plotting fits?',.true.)
        call pgpage
      end if

* loop over remaining teeth

      do k = kset, nwindow

*   fill data arrays.  if 'weight' specified, get weight

        if(npts(k).le.(order+1)) then
          used(k) = .false.
          call par_wruser('Not enough points to fit this line',pstat)
        else
          if (weight) then
            call par_wruser('Performing weighted fit',pstat)
            call copd2d(npts(k),weights(1,k),w)
          else
            call par_wruser('Performing unweighted fit',pstat)
            do i = 1,npts(k)
              w(i) = 1.0
            end do
          end if

*     zero out sum of squares

          call zero_dble(ss,MAX_KPLUS1)

*     fit the polynomial

          kmax = kp1
          used(k) = fit_cpolb(w,npts(k),xpos(1,k),ypos(1,k),a,ss,kmax,
     :                  cheby_max,work,athree,maxnpts) .eq. 0

          if(used(k)) then

*      Copy correct coeffficients from A into AA

             do kk=1,kp1
                aa(kk,k)=a(kp1,kk)
                coeffs(kk) =a(kp1,kk)
             end do

             do kk=1,3*maxnpts+3*MAX_KPLUS1
                a3all(kk,k)=athree(kk)
             end do

             if(plot) then

*        Calculate and plot residuals for this tooth

              call calc_resid(coeffs,kp1,npts(k),work,xpos(1,k),
     :                  ypos(1,k),athree,max_kplus1,maxnpts)
              if(hardplots) then
                call gr_hard(status)
                call pgvstand
              else
                call pgvport(0.07,0.99,0.57,0.95)
              end if
              call plot_datatn(npts(k),xpos(1,k),work,xlabel,
     :                  'Residuals','Residuals For Order',order,
     :                  xpos,3)
              if(hardplots) then
                call gr_hard(status)
                call pgvstand
              else
                call pgvport(0.07,0.99,0.07,0.45)
              end if

*         plot the centriod data for each tooth

              call plot_datatn(npts(k),xpos(1,k),ypos(1,k),xlabel,
     :                  ylabel,'Distortion of tooth',k,xpos,3)

*         evaluate cheby poly at each x value

              fstatus = e_cpoly(xpos(1,k),work,coeffs,kp1,npts(k),
     :         athree,max_kplus1,maxnpts)

*         plot the poly line on the centriod plot

              call plot_dcurv(xpos(1,k),work,npts(k))
              if(.not.hardplots) then
                plot=par_quest('Go onto next plot?',.true.)
                call pgpage
              end if
            end if
          end if
        end if

* K Loop

      end do

* Hardcopies of plots?

      if(.not.batch) then
        if(par_quest(
     :  'Produce hardcopies of line centre & residual plots',.false.))
     :       then
          do k = 1, nwindow
            if(used(k)) then
              call gr_hard(status)

*         Put data into 1D array.

              call calc_resid(aa(1,k),kp1,npts(k),work,xpos(1,k),
     :                  ypos(1,k),a3all(1,k),max_kplus1,maxnpts)
              call pgvstand
              call plot_datatn(npts(k),xpos(1,k),work,xlabel,
     :                     'Residuals','Residuals For Order',order,
     :                      xpos(1,k),3)
* Added (1,k) to 2nd xpos
*         plot the centriod data for each tooth

              call gr_hard(status)
              call pgvstand
              call plot_datatn(npts(k),xpos(1,k),ypos(1,k),xlabel,
     :                  ylabel,'Distortion of tooth',k,xpos(1,k),3)
*Added (1,k) to 2nd xpos
*         evaluate cheby poly at each x value

              fstatus = e_cpoly(xpos(1,k),work,aa(1,k),kp1,npts(k),
     :                   a3all(1,k),max_kplus1,maxnpts)

*         plot the poly line on the centriod plot

              call plot_dcurv(xpos(1,k),work,npts(k))
            end if
          end do
        end if
      end if
 500  continue
      end
