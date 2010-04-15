      subroutine contrl_cpoly2(pos,intensity,nli,w,coeffs,max_kp1,kp1,
     :             xlabel,label,mode,a,plot,seek,work)
*+
* Name:
*    CONTRL_CPOLY2

* Invocation:
*    CALL CONTRL_CPOLY2(POS,INTENSITY,NLI,W,COEFFS,MAX_KP1,KP1,
*                  XLABEL,LABEL,MODE,A,PLOT,SEEK,WORK)

* Purpose:
*   Fit Chebyshev polynomial to data

* Description:
*   Subroutine to fit a polynomial to the data.
*  Called by VIG (once in each direction) & by CSUB. Also used for base
*  fitting in LONGSLIT.
*
* Arguments:
*      NLI = INTEGER (Given)
*        Number of data points
*      MAX_KP1 = INTEGER (Given)
*        Order dimensions
*      POS(NLI) = DOUBLE PRECISION ARRAY (Given)
*        Position of each intensity
*      INTENSITY(NLI) = DOUBLE PRECISION ARRAY (Given)
*        Intensity (Y axis) data
*      LABEL = CHARACTER*(*) (Given)
*        Label
*      XLABEL = CHARACTER*(*) (Given)
*        X label for plotting
*      W(NLI) = DOUBLE PRECISION ARRAY (Given)
*        Stores weights
*      MODE = INTEGER (Given)
*        Mode for plotting in seek_order
*                                 0 = full range
*                                 1 = scale using weights
*      SEEK = LOGICAL (Given)
*        If to seek for order
*      PLOT = LOGICAL (Given)
*        If to make plots
*      COEFFS(MAX_KP1) = DOUBLE PRECISION ARRAY (Returned)
*        Stores cheby coeffs at choosen order
*      KP1 = INTEGER (Returned)
*        Number of coeffs
*   Workspace
*    WORK(NLI*3+MAX_KP1*3) = DOUBLE PRECISION ARRAY (Returned)
*        Workspace for FIT_CPOLY and SEEK_ORDER2
*    A(MAX_KP1,MAX_KP1) = DOUBLE PRECISION ARRAY (Returned)
*        stores cheby coeffs as fun of order
*    ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY (Returned)
*        stores PDA fit co-effs from PDA_DPOLFT
* Subroutines/functions called:
*  E_CPOLY, FIT_CPOLY, PLOT_DATA, PLOT_DCURV, SEEK_ORDER2, ZERO_DBLE,
*  PGVSTAND, PAR_WRUSER.
*
* History
*  Residual sum of squares made workspace passed from above-this should
*  allow this to be used to fit any order. TNW/CAVAD 20/9/90
*  AJH: propogate PDA fitting array through to polynomial evaluation
*       routines
*  ACD: Correct the calling arguments for the (revised) SEEK_ORDER2.

*-
      implicit none
* import
*
      integer nli
      integer max_kp1

      double precision pos(nli)
      double precision intensity(nli)
      character*(*) label
      character*(*) xlabel
      double precision w(nli)
      integer mode
      logical seek,plot
*
* Export
*
      double precision coeffs(max_kp1)
      integer kp1
*
* status
*
      integer status

* Workspace

      double precision work(*)
*
* local
*

* actual order used

      integer order
      integer kk,maxfit

* Pointer to different parts of

      integer SSPTR,workptr

*                                   work array
      integer maxnpts
      parameter (maxnpts = 2048)
      integer maxkp1
      parameter (maxkp1 = 10)
      parameter (SSPTR = 1)
      double precision a(max_kp1,max_kp1)
      double precision athree(3*maxnpts+3*maxkp1)
      integer fstatus,fit_cpolb,e_cpoly
      logical plot1
      character*25 chars

      workptr = SSPTR + max_kp1
      plot1 = plot
*
* passed dimensions
*
      maxfit  = max_kp1
*
* use seek mode
* fill data arrays.
* zero out sum of squares
*
      call zero_dble(work(SSPTR),max_kp1)
*
* do polynomial fit
*
      fstatus = fit_cpolb(w,nli,pos,intensity,a,work(SSPTR),maxfit,
     :            max_kp1,work(workptr),athree,maxnpts)
*
* look for first
*
      if ((maxfit.ge.2).and.(seek)) then
        call seek_order2(work(SSPTR),max_kp1,order,a,pos,intensity,
     :            nli,w,mode,maxfit,coeffs,' ',' ',' ',work(workptr),
     :            athree,maxnpts)

* We've just looked at the plots, so we don't need to again!

        plot1 = .false.
*
* inform user of chosen order
*
        write (chars,'(a,i3)')'Order returned = ',order
        call par_wruser(chars,status)
        kp1 = order+1
      end if
*
* Copy correct coeffficients to A(i) from COEFFS(i,1).
*
      do kk=1,kp1
        coeffs(kk) =a(kp1,kk)
      end do
*
* plot the intensity data for each tooth
*
      if(plot1) then
        call pgvstand
        call plot_data(pos,intensity,nli,xlabel,label,w,1,' ')
      end if
*
* evaluate cheby poly at each x value
*
      fstatus = e_cpoly(pos,intensity,coeffs,kp1,nli,
     :     athree,max_kp1,maxnpts)
*
* plot the poly line on the intensity plot
*
      if(plot1) call plot_dcurv(pos,intensity,nli)
      end
