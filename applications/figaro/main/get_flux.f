      subroutine get_flux(results,resvar,mxpars,nyp,nxp,area,dispersion,
     :                  area_err,model,xsect,line,gauss)
*+
* Name:
*    GET_FLUX

* Invocation:
*    CALL GET_FLUX(RESULTS,RESVAR,MXPARS,NYP,NXP,AREA,DISPERSION,
*                       AREA_ERR,MODEL,XSECT,LINE,GAUSS)

* Purpose:
*    To get the flux at a given position in a given line.

* Description:
*    To get the flux at a given position in a given line.

* Arguments:-
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*     RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube variance
*     MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of above
*     MODEL = INTEGER (Given)
*        Fit model
*     XSECT = INTEGER (Given)
*        Cross-section
*     GAUSS = INTEGER (Given)
*        Number of gaussian
*     LINE = INTEGER (Given)
*        Number of line
*     DISPERSION = REAL (Given)
*        Dispersion of spectrum
*     AREA = REAL (Returned)
*        Area under line (flux), excludes any base
*     AREA_ERR = REAL (Returned)
*        Error on above
*  Subroutines/functions referenced:
*      GET_PARNUM = INTEGER (Returned)
*        Get parameter position
*      MODFLUX                : Calculate flux in line
*
* History:
*    T.N.Wilkins Manchester 13/8/87
*         "      Cambridge 21/7/89 GET_PARNUM introduced
*  Use of IMAP, TNW 24/1/89
*  Change to call of modflux, TNW 27/1/89
*-
      implicit none
      integer mxpars,nyp,nxp,xsect,line,gauss
      real parms(6),errors(6),results(mxpars,nyp,nxp)
      include 'fit_coding_inc'
      real resvar(mxpars,nyp,nxp)
      real area,area_err
      real dispersion
      integer pos,model,get_parnum
      character*1 nums(9)
      data nums/'1','2','3','4','5','6','7','8','9'/

* Get results from cube

      pos = get_parnum('Width_'//nums(gauss))
      parms(1) = results(pos,line,xsect)/dispersion
      errors(1) = sqrt(resvar(pos,line,xsect))/dispersion

      if(model.eq.SKEW_MODEL) then

* Skew gaussian

        pos = get_parnum('Skew_1')
        parms(5) = results(pos,line,xsect)
        errors(5) = sqrt(resvar(pos,line,xsect))
      else if(model.eq.CAUCHY_MODEL) then

* Cauchy function

        pos = get_parnum('Cauchy_1')
        parms(5) = results(pos,line,xsect)
        errors(5) = sqrt(resvar(pos,line,xsect))
      end if

* Width > 0

      parms(1) = abs(parms(1))

* Height

      pos = get_parnum('Height_'//nums(gauss))
      parms(2) = results(pos,line,xsect)
      errors(2) = sqrt(resvar(pos,line,xsect))

* Note that centre and base aren't used, so we don't set them

* Calculate flux in line

      call modflux(area,parms,area_err,errors,model)
      end
