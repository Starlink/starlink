      subroutine fibstres(g_parms,g_error,nparms,ref,results,ix,iy,
     :      deccntr,npts,mask,nfailed,nnew,aic,resvar,fitsta)
*+
* Name:
*    FIBSTRES

* Invocation:
*    CALL FIBSTRES(G_PARMS,G_ERROR,NPARMS,REF,RESULTS,IX,IY,
*           DECCNTR,NPTS,MASK,NFAILED,NNEW,AIC,RESVAR,FITSTA)

*
* Purpose:
*  To store the fit results for the fibre analysis package.
*
* Description:
*  To store the fit results for the fibre analysis package.
*
* Arguments:
*   NPARMS = INTEGER (Given)
*        Number of fit results
*   G_PARMS(NPARMS) = REAL ARRAY (Given)
*        Fit results
*   G_ERROR(NPARMS) = REAL ARRAY (Given)
*        Errors on fit results
*   IX = INTEGER (Given)
*        X position of a point selected
*   IY = INTEGER (Given)
*        Y position of a point selected
*   REF(SPDIM1,SPDIM2) = INTEGER*2 ARRAY (Given)
*        Reference array (profiles being used)
*   NPTS = INTEGER (Given)
*        Number of points in fit
*   AIC = REAL (Given)
*        Akaike's information criterion
*   DECCNTR(*) = INTEGER ARRAY (Given)
*        Profile model of fit
*   MASK(SPDIM1,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Mask
*   NFAILED = INTEGER (Given and returned)
*        Number of failed fits
*   NNEW = INTEGER (Given and returned)
*        Number of new fits
*   RESULTS(MXPARS,SPDIM1,SPDIM2) = REAL ARRAY (Returned)
*        Results block
*   RESVAR(MXPARS,SPDIM1,SPDIM2) = REAL ARRAY (Returned)
*        Results block variance
*   FITSTA(NCNTRL,SPDIM1,SPDIM2) = REAL ARRAY (Returned)
*        Fit status
*  Global variables:
*   ITERATION = INTEGER (Given)
*        Iteration number
*   MXPARS = INTEGER (Given)
*        first dimension of results block
*   SPDIM1 = INTEGER (Given)
*        X (spatial) dimension of data
*   SPDIM2 = INTEGER (Given)
*        Y (spatial) dimension of data
*   NCNTRL = INTEGER (Given)
*        Number of elements in control
*
*  Subroutines referenced:
*   GEN_CFILL   : Fill array with given value
*
* Author:
*   T.N.Wilkins Manchester 13/6/88
* History:
*   TNW: Various revisions up to 15/7/88
*   TNW 26/10/88 Changed to free opt_lu if crashed
*   TNW 18/1/89 Change to positions of skew/Cauchy, badval a parameter
*   TNW 1-8/7/91 Changes for new results structure
*-
      implicit none
      include 'arc_dims'
      include 'PRM_PAR'
      include 'opt_cmn'
      include 'status_inc'
      integer nparms,npts,nfailed,nnew
      real g_parms(nparms),g_error(nparms),results(mxpars,spdim1,spdim2)
      real resvar(mxpars,spdim1,spdim2)
      integer fitsta(ncntrl,spdim1,spdim2)
      integer*2 ref(spdim1,spdim2),mask(spdim1,spdim2)
      real aic
      integer i,j,ix,iy,get_parnum,ppos,k
      integer fitstat(max_control)

      nnew = nnew + 1
      deccntr(fit_stat) = 0
      if(deccntr(fit_model).eq.0) goto 550

*  loop - should perhaps be istore rather than istartx
*
*  See if condition handler was entered...
*

* ...condition handler was entered

      if(crash) then
        nfailed=nfailed+1
        deccntr(fit_stat) = 3
        nnew    = nnew - 1
      else

* check for a nag error

        if(nagerror) then
          nfailed = nfailed + 1
          deccntr(fit_stat) = 1
        end if

*   ... in principal fit is OK


        deccntr(fit_stat)=deccntr(fit_stat)+1
      end if
      call encode_contrl(deccntr,ncntrl,fitstat)

* Store AIC if calculated an room available in results block

      if(aic.ne.VAL__BADR) then
        ppos = get_parnum('Akaike IC')
      else
        ppos = 0
      end if
      do j = 1, spdim2
        do i = 1, spdim1

*        Flag elements of array for this position as bad,
*        so that if a previous fit was made it is completely
*        erased.

          call gen_cfill(1,mxpars,val__badr,results(1,i,j))
          call gen_cfill(1,mxpars,val__badr,resvar(1,i,j))
          if(ref(i,j).eq.1) then
            mask(i,j) = iteration
            do k = 1, ncntrl
              fitsta(k,i,j) = fitstat(k)
            end do
            results(get_parnum('Space1_pos'),i,j) = real(ix)
            results(get_parnum('Space2_pos'),i,j) = real(iy)
            results(get_parnum('Pts_in_Fit'),i,j) = real(npts)
            if(ppos.gt.0) results(ppos,i,j) = aic
            if(.not.crash) then
              call strescore(results,resvar,1,i,j,real(densc),g_parms,
     :                  g_error,deccntr)
            end if
          end if
        end do
      end do
 550  continue
      end
