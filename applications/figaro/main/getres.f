      subroutine getres(results,line,ix,iy,fitpar,deccntr,odensc,
     :               fitsta,status)
*+
* Name:
*    GETRES

* Invocation:
*    CALL GETRES(RESULTS,LINE,IX,IY,FITPAR,DECCNTR,ODENSC,
*                    FITSTA,STATUS)

* Purpose:
*    To read the results from a stored fit into the array fitpar
*
* Description:
*    To read the results from a stored fit into the array fitpar
*
* Arguments:
*     RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results block
*     FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*     LINE = INTEGER (Given)
*        Line to get fit for
*     IX = INTEGER (Given)
*        X position
*     IY = INTEGER (Given)
*        Y position
*     FITPAR(16) = REAL ARRAY (Returned)
*        Fit parameters, in the following order:
*                            base, width, height, centre, (skew/width2),
*                            (cauchy/height2), centre2, ....
*     DECCNTR(*) = INTEGER ARRAY (Returned)
*        Fit coding
*     ODENSC = REAL (Returned)
*        Old density scale factor
*     STATUS = INTEGER (Returned)
*        Error status (0=ok)

* Global variables:
*     MXPARS = INTEGER (Returned)
*        1st dimension of results block (include file arc_dims)
*     SPDIM1 = INTEGER (Returned)
*        2nd dimension of results block (include file arc_dims)
*     SPDIM2 = INTEGER (Returned)
*        3rd dimension of results block (include file arc_dims)
*     NYP = INTEGER (Returned)
*        4th dimension of results block (include file arc_dims)

* Author:
*     TNW: T.N.Wilkins Manchester until 1/89, then Cambridge

* History:
*     TNW: 18/8/88
*     TNW: 8/7/91 Simplified, and new results structure
*-

      implicit none
      include 'opt_cmn'
      integer status
      include 'SAE_PAR'
      include 'arc_dims'
      include 'status_inc'
      include 'fit_coding_inc'
      integer ix,iy,line,cstat
      real results(mxpars,nyp,spdim1,spdim2),fitpar(MAX_PARMS)
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      real odensc
      integer i
      real EFOLD
      parameter (EFOLD = 2.35482)
      include 'PRM_PAR'
      integer k1,p1,gauss
      integer get_parnum
      character*1 nums(9)
      data nums/'1','2','3','4','5','6','7','8','9'/

      status = 1

* Get and decode fit status

      call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
      cstat = deccntr(FIT_STAT)

      if((cstat.eq.1).or.(cstat.eq.2).or.(cstat.eq.4)) status = 0

      if(status.eq.SAI__OK) then

*    Find out number of components and put results into fitpar

*   Base

        if(deccntr(BACK_MODEL).eq.0) then
          fitpar(1)=0.0
        else
          fitpar(1)=results(get_parnum('Base'),line,ix,iy)
        end if

*     skew

        if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then

          fitpar(5)=results(get_parnum('Skew_1'),line,ix,iy)
          p1 = 5

*     cauchy

        else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then

          fitpar(5)=results(get_parnum('Cauchy_1'),line,ix,iy)
          p1 = 5

        end if

        do gauss = 1, deccntr(FIT_NCMP)
          p1 = gauss*3-1
          k1 = get_parnum('Width_'//nums(gauss))
          fitpar(p1)=results(k1,line,ix,iy)

*     If not skew or Cauchy, then divide width by EFOLD

          if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
            fitpar(p1) = fitpar(p1)/EFOLD
          else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
            fitpar(p1) = fitpar(p1)*0.5
          end if
          p1 = p1 + 1
          k1 = get_parnum('Height_'//nums(gauss))
          fitpar(p1)=results(k1,line,ix,iy)
          p1 = p1 + 1
          k1 = get_parnum('Centre_'//nums(gauss))
          fitpar(p1)=results(k1,line,ix,iy)
        end do
      end if
      do i = 1, p1
        if(fitpar(i).eq.VAL__BADR) then
          status = 20
        end if
      end do
      if(status.ne.SAI__OK) then
        deccntr(FIT_MODEL) = 0
      end if

* Get old density scale factor

      odensc = results(get_parnum('Dens_scale'),line,ix,iy)
      end
