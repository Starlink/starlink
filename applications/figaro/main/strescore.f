      subroutine strescore(results,resvar,line,istore,jstore,odensc,
     :            fitpar,fiterr,deccntr)
*+
* Name:
*    STRESCORE

* Invocation:
*    CALL STRESCORE(RESULTS,RESVAR,LINE,ISTORE,JSTORE,ODENSC,
*                 FITPAR,FITERR,DECCNTR)

* Purpose:
*   Core part of routines for storing fit results.

* Description:
*   Core part of routines for storing fit results.
*
* Arguments:
*      LINE = INTEGER (Given)
*        Line of fit
*      ISTORE = INTEGER (Given)
*        1st coordinate of fit
*      JSTORE = INTEGER (Given)
*        2nd coordinate of fit
*      ODENSC = DOUBLE PRECISION (Given)
*        Density scale factor
*      FITPAR(MPARMS) = REAL ARRAY (Given)
*        Fit parameters
*      FITERR(MPARMS) = REAL ARRAY (Given)
*        Fit errors
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Profile model of fit
*      RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results block
* Global variables:
*      MPARMS = INTEGER (Given)
*        Dimensions of parameters arrays (in arc_dims)
* Subroutines/functions referenced:
*      GET_PARNUM

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 17-JAN-1990 Original version
*   TNW: 1 to 8-JUL-1991 Changes for new results structure
*-
      implicit none
      include 'status_inc'
      include 'arc_dims'
      include 'fit_coding_inc'
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      integer line
      integer istore
      integer jstore
      real odensc
      real fitpar(mparms)
      real fiterr(mparms)

*

      integer i
      integer ppos,pos1,get_parnum
      character*1 number(9)
      data number/'1','2','3','4','5','6','7','8','9'/

* Store scale factor in results

      ppos = get_parnum('Dens_scale')
      results(ppos,line,istore,jstore) = odensc

      if(deccntr(fit_type).ge.SKEW_MODEL) then

* Multiple and double gaussians, base:


*  Position for base

        pos1 = 1

* single gaussian

      else

* save the fit parameters:-

* store the skew and cauchy parameters if those
* models have been used

*   skew

        if(deccntr(fit_model).eq.SKEW_MODEL) then
          ppos = get_parnum('Skew_1')
          results(ppos,line,istore,jstore) = fitpar(5)
          resvar(ppos,line,istore,jstore) = fiterr(5)*fiterr(5)

*   Cauchy

        else if(deccntr(fit_model).eq.CAUCHY_MODEL) then
          ppos = get_parnum('Cauchy_1')
          results(ppos,line,istore,jstore) = fitpar(5)
          resvar(ppos,line,istore,jstore) = fiterr(5)*fiterr(5)
        end if

*  Position for base

        pos1 = 4
      end if

      ppos = get_parnum('Base')
      results(ppos,line,istore,jstore) = fitpar(1)
      resvar(ppos,line,istore,jstore) = fiterr(1)*fiterr(1)


* loop over gaussians

      do i=1,deccntr(FIT_NCMP)
        pos1 = i * 3 - 1
        ppos = get_parnum('Width_'//number(i))
        results(ppos,line,istore,jstore) = fitpar(pos1)
        resvar(ppos,line,istore,jstore) = fiterr(pos1)*fiterr(pos1)
        pos1 = pos1 + 1
        ppos = get_parnum('Height_'//number(i))
        results(ppos,line,istore,jstore) = fitpar(pos1)
        resvar(ppos,line,istore,jstore) = fiterr(pos1)*fiterr(pos1)
        pos1 = pos1 + 1
        ppos = get_parnum('Centre_'//number(i))
        results(ppos,line,istore,jstore) = fitpar(pos1)
        resvar(ppos,line,istore,jstore) = fiterr(pos1)*fiterr(pos1)
      end do

      end
