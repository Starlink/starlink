      subroutine store_results(fitpar,fiterr,nnew,nfailed,line,istartx,
     :     iendx,deccntr,odensc,results,resvar,fitsta,mask,istarty,
     :     iendy,aic)
*+
* Name:
*    STORE_RESULTS

* Invocation:
*    CALL STORE_RESULTS(FITPAR,FITERR,NNEW,NFAILED,LINE,
*          ISTARTX,IENDX,DECCNTR,ODENSC,RESULTS,RESVAR,FITSTA,MASK,
*          ISTARTY,IENDY,AIC)

* Purpose:
*   Store results from Gaussian etc. fitting.

* Description:
*   Store results from Gaussian etc. fitting.

* Arguments:
*     FITPAR(MPARMS) = INTEGER ARRAY (Given)
*        Fit parameters
*     FITERR(MPARMS) = INTEGER ARRAY (Given)
*        Errors on fit parameters
*     LINE = INTEGER (Given)
*        Number of line
*     ISTARTX = INTEGER (Given)
*        Starting cross-section of fit
*     IENDX = INTEGER (Given)
*        End cross-section of fit
*     ISTARTY = INTEGER (Given)
*        Start of fit in 2nd spatial dimension
*     IENDY = INTEGER (Given)
*        End    "  "   "   "   "         "
*     ODENSC = INTEGER (Given)
*        Density scale factor
*     AIC = REAL (Given)
*        Akaike's information criterion
*     DECCNTR(*) = INTEGER ARRAY (Given)
*        Profile model of fit
*     NFAILED = INTEGER (Given and returned)
*        Number of failed fits
*     NNEW = INTEGER (Given and returned)
*        Number of new fits
*     RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results array
*     RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results array variance
*     FITSTA(NCNTRL,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Fit status
*     MASK(NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Mask array
* Global variables:
*     CRASH = LOGICAL (Given)
*        If a crash occurred (in opt_cmn)
*     NAGERROR = LOGICAL (Given)
*        If a Nag error occurred (in opt_cmn)
*     MXPARS,NYP,NXP,SPDIM2 = INTEGER (Given)
*        Dimensions of results "cube" (in arc_dims)
*     MPARMS = INTEGER (Given)
*        Dimension of parameters arrays (in arc_dims)
*     OPT_LU = INTEGER (Given)
*        Logical unit for .itt file (in opt_cmn)

* Authors
*   TNW: T.N.Wilkins, Manchester until 1/89, then Cambridge until 9/92

* History:
*   TNW: 26/10/88 to free opt_lu if crash occurs
*   TNW: 27/7/89 to access results and mask arrays directly
*   TNW: 22/1/90 Should work for 3-d data!
*   TNW: 28/5/91 New fit encoding
*   TNW: 1-8/7/91 for new results stucture
*-
      implicit none
      include 'PRM_PAR'
      include 'arc_dims'
      include 'status_inc'
*  Input
      real odensc
      real aic
      real fitpar(mparms)
      real fiterr(mparms)
*
*  Input and Output
*
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(ncntrl,nyp,nxp,spdim2)
      integer*2 mask(nyp,nxp,spdim2)
*
*  Local
*

* No. of new fits

      integer nnew

* Line number being worked on

      integer line
      integer istartx

* Channel boundaries for current LINE

      integer iendx
      integer fit_flag(MAX_CONTROL)
      integer istarty
      integer iendy
      integer nfailed
      integer istore
      integer jstore,i
      real value
      real rnwindx,rnwindy

* fit type etc.

      integer get_parnum
      integer ppos1,ppos2,ppos3,ppos4
      integer pstat
*
* common
*
      include 'opt_cmn'
* ---------------------------------------------------------------
      deccntr(FIT_STAT) = 0

* save fit_flag for latter ,as as yet cannot set status word

      if(deccntr(FIT_MODEL).eq.0) then
        call opt_wruser('No fit to store!',pstat)
        goto 550
      end if

*  loop - should perhaps be istore rather than istartx
*
*  See if condition handler was entered...
*

* ...condition handler was entered

      if(crash) then
*
*   ...if so, then send code to diagnostics file...
*
        nfailed=nfailed+1
        deccntr(FIT_STAT) = 3
        nnew    = nnew - 1
      else

* check for a nag error

        if(nagerror) then
          nfailed = nfailed + 1
          deccntr(FIT_STAT) = 1
        end if

*            ... in principal fit is OK

        deccntr(FIT_STAT) = deccntr(FIT_STAT) + 1
        do jstore = istarty,iendy
          do istore = istartx,iendx
            call strescore(results,resvar,line,istore,jstore,odensc,
     :                  fitpar,fiterr,deccntr)
          end do

*   ... fill in RESULTS

        end do

* ...check on CRASHES

      end if

*  Store the status word in results

      call encode_contrl(deccntr,ncntrl,fit_flag)
      ppos1 = get_parnum('Space1_pos')
      if(spdim2.gt.1) then
        ppos2 = get_parnum('Space2_pos')
        ppos3 = get_parnum('Pts_in_Fit')
      end if

* Store AIC if calculated and room available in results block

      if(aic.ne.VAL__BADR) then
        ppos4 = get_parnum('Akaike IC')
      else
        ppos4 = 0
      end if

* store the blocking information and parameters of fit in storage block
* Also the fit status. In old format fibre files BlockXspat and
* BlockYspat are not present, so we must check for this. In order that
* any windows for which the true blocking is less than nwindow (due to
* going off the end of the data for example) have a sensible value
* stored for nwindow, we take this from iendx and iendy. Ditto for
* windowy

      rnwindx = real(iendx-istartx+1)*0.5
      rnwindx = rnwindx*rnwindx
      rnwindy = real(iendy-istarty+1)*0.5
      rnwindy = rnwindy*rnwindy
      do jstore = istarty,iendy
        do istore = istartx,iendx

*     starting x-sect number

          value = real(istartx+iendx)*0.5
          results(ppos1,line,istore,jstore) = value

          resvar(ppos1,line,istore,jstore) = rnwindx
          mask(line,istore,jstore) = iteration
          if(spdim2.gt.1) then
            value = real(istarty+iendy)*0.5
            results(ppos2,line,istore,jstore) = value
            resvar(ppos2,line,istore,jstore) = rnwindy
            value = sqrt(rnwindx*rnwindy)*4.0
            results(ppos3,line,istore,jstore) = value
          end if
          do i = 1, ncntrl
            fitsta(i,line,istore,jstore) = fit_flag(i)
          end do
          if(ppos4.ne.0) results(ppos4,line,istore,jstore) = aic
        end do
      end do

 550  continue
      end
