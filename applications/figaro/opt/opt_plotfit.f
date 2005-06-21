      subroutine opt_plotfit(deccntr,fitpar,x,y,start,nbaswrk,vbase
     :  ,diags,xlim,xsect,nwindow,status,work)
*+
* Name:
*    OPT_PLOTFIT

* Invocation:
*    CALL OPT_PLOTFIT(DECCNTR,FITPAR,X,Y,START,NBASWRK,VBASE
*       ,DIAGS,XLIM,XSECT,NWINDOW,STATUS,WORK)

*
* Purpose:
*    To plot the fits.

* Description:
*    To plot the fits. The data and residuals are plotted for
*    manual fits. Otherwise just the fits are plotted.
*
* Arguments:
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit coding
*      FITPAR(*) = REAL ARRAY (Given)
*        Fit parameters
*      X(*) = REAL ARRAY (Given)
*        X array
*      Y(*) = REAL ARRAY (Given)
*        Y array
*      START = INTEGER (Given)
*        Start index of X, Y being used
*      NBASWRK = INTEGER (Given)
*        Size of base workspace
*      VBASE = INTEGER (Given)
*        "Pointer" to base
*      DIAGS(2) = INTEGER ARRAY (Given)
*        Diagram references
*      XLIM(2) = REAL ARRAY (Given)
*        Limits in X of plots
*      XSECT = INTEGER (Given)
*        Start cross-section of fit
*      NWINDOW = INTEGER (Given)
*        Number of xsects in fit
*      WORK = INTEGER (Given)
*        "Pointer" to workspace
*                          For Singles:
*                               5*m*VAL__NBD
*                               5*m*VAL__NBD
*                               nbaswrk*5
*                               m*5*VAL__NBR
*                          For Doubles and Multiples:
*                               m*(deccntr(FIT_NCMP)+1)*VAL__NBR
*
*      STATUS = INTEGER (Given and returned)
*        Error status
*
* Global variables:
*      MPTS = INTEGER (Workspace)
*        Number of elements of X and Y which are used (include file
*        opt_cmn)
*
*  Subroutines/functions referenced:
*
* Author:
*   TNW: T.N.Wilkins, Cambridge, 11-SEP-1991
*
* History:
*    TNW: 11-SEP-1991 Original version
*    TNW: 9-JUN-1992 More comments
*    TNW: 21/8/92 Changed so functions gaussian/lorentz
*                            passed around as arguments (external)
*    TNW: 30/6/93 Checks on fstat etc. moved to fit_line
*-
      implicit none
      include 'status_inc'
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      real fitpar(*)
      real x(*)
      real y(*)
      integer start
      integer nbaswrk
      integer vbase
      integer diags(2)
      real xlim(2)
      integer xsect
      integer nwindow
      integer status
      integer work
      real gaussian, lorentz
      external gaussian, lorentz

      include 'opt_cmn'
      include 'arc_dims'
      include 'fit_coding_inc'
      real y1(3),pltpar(6)
      integer vbase5,work2,work3,nbad,cnv_fmtcnv
      logical isnew1,isnew2,isnew3

      if(status.ne.SAI__OK) return

* First singles, for which we simply plot the sum of the fit, including
* base

      if(deccntr(FIT_TYPE).eq.SINGLE) then

        call dyn_incad(work,'double',5*mpts,vbase5,isnew1,status)
        call dyn_incad(vbase5,'double',5*mpts,work2,isnew2,status)
        call dyn_incad(work2,'byte',5*nbaswrk,work3,isnew3,status)

        call fill_dat(x(start),mpts,%VAL(CNF_PVAL(work3)))
        status = cnv_fmtcnv('real','double',%VAL(CNF_PVAL(work3)),
     :                      %VAL(CNF_PVAL(work)),mpts*5,nbad)
        call fit_glbase(xsect,nwindow,x,%VAL(CNF_PVAL(work3)),deccntr,
     :                  1,mpts*5,vbase5,.false.,%VAL(CNF_PVAL(work)),
     :                  work2,status)
        call copr2r(6,fitpar,pltpar)
        call plot_fit(pltpar,deccntr(FIT_MODEL),mpts,
     :                %VAL(CNF_PVAL(vbase5)))

         if (isnew1) call cnf_unregp(vbase5)
         if (isnew2) call cnf_unregp(work2)
         if (isnew3) call cnf_unregp(work3)
      else

* Doubles or multiples, we plot each component, shifted if Y if needed
* to get on the plot (if Y minimum isn't zero), the sum of the fit, and
* possibly the residuals

        call dyn_incad(work,'float',mpts,work2,isnew2,status)
        if(deccntr(FIT_MAN).eq.MAN_ALTER) then
          call pgpage
          call gr_seld(diags(1),status)
          call disp_window2(xlim(1),xlim(2),x(start),y(start),mpts,
     :                      xunits,.false.)
        end if

* Plot components

        if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
          call comp_plot(fitpar,deccntr(FIT_NCMP),mpts,
     :                   %VAL(CNF_PVAL(work2)),x(start),y1,gaussian)

        else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
          call comp_plot(fitpar,deccntr(FIT_NCMP),mpts,
     :                   %VAL(CNF_PVAL(work2)),x(start),y1,lorentz)
        end if

* Sum of components (and base)

        call multi_plot(fitpar,deccntr(FIT_NCMP),mpts,
     :                  %VAL(CNF_PVAL(work2)),%VAL(CNF_PVAL(work)),
     :                  x(start),deccntr(BACK_MODEL),
     :                  %VAL(CNF_PVAL(vbase)),y1)

* Residuals plot if applicable

        if(deccntr(FIT_MAN).eq.MAN_ALTER) then
          call gr_seld(diags(2),status)
          call multi_resid(x(start),y(start),mpts,%VAL(CNF_PVAL(work2)),
     :                     %VAL(CNF_PVAL(work)),.true.,.true.,title,
     :                     legend)
          call gr_seld(diags(1),status)
        end if
         if (isnew2) call cnf_unregp(work2)
      end if

      end
