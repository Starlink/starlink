      subroutine prvel_sub(results,resvar,line,ix,iy,deccntr,
     :   lu,first_xsect,last_xsect,dispersion,wavelength,vcorr,status)
*+
* Name:
*    PRVEL_SUB

* Invocation:
*    CALL PRVEL_SUB(RESULTS,RESVAR,LINE,IX,IY,DECCNTR,
*        LU,FIRST_XSECT,LAST_XSECT,DISPERSION,WAVELENGTH,VCORR,STATUS)

* Purpose:
*   Core part of code for printing velocity curves

* Description:
*   This is intended as the "core" of prvel and fib_prvel, so that more
*   of the code can be in common between the two.
*
* Arguments:
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array
*      RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array variance
*      LINE = INTEGER (Given)
*        Number of line
*      IX = INTEGER (Given)
*        Position of fit in 1st spatial dimension
*      IY = INTEGER (Given)
*        Position of fit in 2nd spatial dimension
*      LU = INTEGER (Given)
*        Logical unit of file
*      FIRST_XSECT = INTEGER (Given)
*        First cross-section of fit (longslit only)
*      LAST_XSECT = INTEGER (Given)
*        Last cross-section of fit (longslit only)
*      DISPERSION = REAL (Given)
*        Dispersion of data
*      WAVELENGTH = REAL (Given)
*        Wavelength of line
*      VCORR = REAL (Given)
*        Correction to velocity
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit model/status
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*      MXPARS = INTEGER (Workspace)
*        Number of fit parameters (include file arc_dims)
*      NYP = INTEGER (Workspace)
*        Number of lines that can be stored (include file arc_dims)
*      SPDIM1 = INTEGER (Workspace)
*        1st Spatial dimension (include file arc_dims)
*      SPDIM2 = INTEGER (Workspace)
*        2nd spatial dimension (include file arc_dims)
* Subroutines/functions referenced:
*     MODFLUX           : Evaluate flux in line/component
*     WAVE2VEL          : Convert wavelengths to velocities
*     WFT               : Write string to file (or terminal)

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 15-JAN-1990
*   TNW: 28/5/91 New fit status decoding
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer line
      integer ix
      integer iy
      integer lu
      integer first_xsect
      integer last_xsect
      real dispersion
      real wavelength
      real vcorr
      integer status
      include 'status_inc'

*

      real wave_obs,width
      real vel_obs,vel_w,flux,flux_err
      real wave_err,width_err,vel_err,vel_w_err
      character*80 chars
      real fit_parms(6)
      real fit_error(6)
      integer slen,gauss,len1
      integer get_parnum,ppos
      real cauchy,cauchy_err,skew,skew_err
      character*1 number(9)
      data number/'1','2','3','4','5','6','7','8','9'/

* Set number of components. For single component fit, if this is Cauchy
* or skew we need to get the results for these at this stage.
* N.B. Modflux doesn't use elements 3 and 4 of fit_parms and fit_error.

*   skew

      if(deccntr(fit_model).eq.2) then
        ppos = get_parnum('Skew_1')
        skew = results(ppos,line,ix,iy)
        skew_err = sqrt(abs(resvar(ppos,line,ix,iy)))
        fit_parms(5) = skew
        fit_error(5) = skew_err

*   cauchy

      else if(deccntr(fit_model).eq.3) then
        ppos = get_parnum('Cauchy_1')
        cauchy = results(ppos,line,ix,iy)
        cauchy_err = sqrt(abs(resvar(ppos,line,ix,iy)))
        fit_parms(5) = cauchy
        fit_error(5) = cauchy_err
      end if

*  Loop over components

      do gauss = 1, deccntr(fit_ncmp)

        ppos = get_parnum('Centre_'//number(gauss))
        wave_obs = results(ppos,line,ix,iy)
        wave_err = sqrt(abs(resvar(ppos,line,ix,iy)))

        ppos = get_parnum('Width_'//number(gauss))
        width = abs(results(ppos,line,ix,iy))
        width_err = sqrt(abs(resvar(ppos,line,ix,iy)))

        call wave2vel(wavelength,wave_obs,wave_err,width,width_err,
     :          vel_obs,vel_err,vel_w,vel_w_err)
        fit_parms(1) = width/dispersion
        ppos = get_parnum('Height_'//number(gauss))
        fit_parms(2) = results(ppos,line,ix,iy)
        fit_error(1) = width_err/dispersion
        fit_error(2) = sqrt(abs(results(ppos,line,ix,iy)))
        call modflux(flux,fit_parms,flux_err,fit_error,
     :            deccntr(fit_model))
        vel_obs = vel_obs - vcorr

        call chr_fill(' ',chars)
        if(spdim2.eq.1) then
          if(gauss.eq.1) then
            len1 = 3
            call encode_range(' ',' ',first_xsect,last_xsect,chars,len1)
          end if
          len1 = 18
        else
          len1 = 5
        end if
        slen = len1

*   If more than 1 component then output number of component

        if(deccntr(fit_ncmp).gt.1) then
          call chr_puti(gauss,chars,len1)
        end if
        len1 = slen + 10
        call chr_putr(vel_obs,chars,len1)
        len1 = slen + 24
        call chr_putr(vel_w,chars,len1)
        len1 = slen + 38
        call chr_putr(flux,chars,len1)

*   Skew and Cauchy

        len1 = slen + 50
        if(deccntr(fit_model).eq.2) then
          call chr_putr(skew,chars,len1)
        else if(deccntr(fit_model).eq.3) then
          call chr_putr(cauchy,chars,len1)
        end if
        call wft(chars,lu,status)

        call chr_fill(' ',chars)
        len1 = slen + 10
        call chr_putr(vel_err,chars,len1)
        len1 = slen + 24
        call chr_putr(vel_w_err,chars,len1)
        len1 = slen + 38
        call chr_putr(flux_err,chars,len1)

*   Skew and Cauchy errors

        len1 = slen + 50
        if(deccntr(fit_model).eq.2) then
          call chr_putr(skew_err,chars,len1)
        else if(deccntr(fit_model).eq.3) then
          call chr_putr(cauchy_err,chars,len1)
        end if
        call wft(chars,lu,status)
      end do
      end
