      subroutine get_aic(fit_parms,nparms,sdata,sdens,m,w,funct,aic)
*+
* Name:
*    GET_AIC

* Invocation:
*    CALL GET_AIC(FIT_PARMS,NPARMS,SDATA,SDENS,M,W,FUNCT,AIC)

* Purpose:
*  To evaluate Akaike's information criterion

* Description:
*  To evaluate Akaike's information criterion
*
* Arguments:
*      FIT_PARMS(NPARMS) = REAL ARRAY (Given)
*        Fit parameters
*      NPARMS = INTEGER (Given)
*        Number of parameters
*      SDATA(M) = REAL ARRAY (Given)
*        X array data
*      SDENS(M) = REAL ARRAY (Given)
*        Intensity data
*      M = INTEGER (Given)
*        Number of pixels in range
*      W(M) = DOUBLE PRECISION ARRAY (Given)
*        weights
*      FUNCT = INTEGER (Given)
*        Function to evaluate (<=1 = Gaussian, 2=skew,
*                         3 = Cauchy)
*      AIC = REAL (Returned)
*        Akaike's information criterion
* History:
*   T.N.Wilkins, Cambridge,  9-MAY-1989
*        "           "       9-JUN-1992 Use fit_coding_inc
*        "           "       25-JUN-1992 Include Lorentzian
*        "           "       10/9/92 Minor changes
*-
      implicit none
      include 'fit_coding_inc'
      integer nparms
      real fit_parms(nparms)
      integer m
      real sdata(m)
      real sdens(m)
      double precision w(m)
      real aic
      integer funct,k1,k2,k3,j,nless2
      real diff,tot,xx,ss,tmp
      real A
      real xhalf,rmean
      double precision power,ca,term,xto2
*

* log(2.0d0)

      parameter (A = 0.6931471806)

*

      ss = 0.0

* Evaluate weighted sum of squares

      if(funct.le.GAUSSIAN_MODEL) then

*   Gaussian

        nless2 = nparms - 2
        do j=1,m
          xx = sdata(j)
          tot = fit_parms(1)
          do k1 = 2, nless2, 3
            k2=k1+1
            k3=k1+2
            tmp = (xx-fit_parms(k3))/fit_parms(k1)
            tot = tot + fit_parms(k2)*exp(-(tmp*tmp)*0.5)
          end do
          diff = tot - sdens(j)
          ss = ss + real(w(j)) * diff * diff
        end do
      else if(funct.eq.SKEW_MODEL) then

*   Skew

        do j=1,m
          xhalf = fit_parms(1)*fit_parms(5)/sinh(fit_parms(5))
          rmean = 2.0*fit_parms(5)*(xx-fit_parms(3))/xhalf
*
*  ** Test to see if RMEAN<=-1
*
          if (rmean.gt.-1.0e0) then
            tmp = (log((1.0+rmean)))/(fit_parms(5))
            tot = fit_parms(2)*exp(-A*tmp*tmp)+fit_parms(4)
            diff = tot - sdens(j)
            ss = ss + real(w(j)) * diff * diff
          end if
        end do
      else if(funct.eq.CAUCHY_MODEL) then

*   Cauchy

        do j=1,m
          xto2 = fit_parms(5)*fit_parms(5)
*
* arithmetic trap for small cauchy
*
          if(xto2.gt.1.0d-4) then
            power = -1.0d0/xto2
            term  = 2.0d0**xto2-1.0d0
          else
            power = 1.0d-4
            term  = 0.0d0
            xto2  = 1.0d04
          end if
          tmp = (xx-fit_parms(3))/fit_parms(1)
          ca  = (abs(4.0d0*term*tmp*tmp+1.0d0))**power
          tot = fit_parms(2)*real(ca)+fit_parms(4)
          diff = tot - sdens(j)
          ss = ss + real(w(j)) * diff * diff
        end do
      else if(funct.eq.LORENTZ_MODEL) then
        nless2 = nparms - 2
        do j=1,m
          xx = sdata(j)
          tot = fit_parms(1)
          do k1 = 2, nless2, 3
            k2=k1+1
            k3=k1+2
            tmp = (xx-fit_parms(k3))/fit_parms(k1)
            tot = tot + fit_parms(k2)/(1.0 + tmp*tmp)
          end do
          diff = tot - sdens(j)
          ss = ss + real(w(j)) * diff * diff
        end do
      end if

* Evaluate AIC

      aic = real(m) * log(ss) + real(2 * nparms)
      end
