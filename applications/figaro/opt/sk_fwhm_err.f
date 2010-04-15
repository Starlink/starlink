      subroutine sk_fwhm_err(sg_parm,sg_error,n,error)
*+
* Name:
*    SK_FWHM_ERR

* Invocation:
*    CALL SK_FWHM_ERR(SG_PARM,SG_ERROR,N,ERROR)

* Purpose:
*   Evaluate full-width half maximum error.

* Description:
*   Evaluate errors On fwhm of a line which has been fitted with a
*   skew gausian.
*
* Arguments:
*    SG_PARM(N) = REAL ARRAY (Given)
*
*    SG_ERROR(N) = REAL ARRAY (Given)
*
*    N = INTEGER (Given)
*
*    ERROR = REAL (Returned)
*        error on width
*
*   Optimised, TNW/Cambridge, 21/3/91
*
*-
      implicit none
      integer n
      real sg_parm(n)
      real sg_error(n)
      real error

* local
*

* skew

      double precision b

* opt parameter

      double precision xhalf

* error on height

      double precision dely

* error on b

      double precision delb

* error on xhalf

      double precision delhalf
      double precision xerr,yerr,yerr1,ds,dc,berr
      double precision CONST

* dlog(2.0d0)

      parameter (CONST = 0.6931471806d0)
*
* fill in the local variables
*
      b       = sg_parm(5)
      xhalf   = sg_parm(2)
      delb    = sg_error(5)
      delhalf = sg_error(2)
      dely    = sg_error(3)/sg_parm(3)
      ds      = sinh(b)
      dc      = cosh(b)
*
* square error with respect to XHALF
*
      xerr    = ds * delhalf / b
      xerr    = xerr*xerr
*
* square error with respect to b
*
      berr    = (dc-ds/b)*(xhalf/b)*delb
      berr    = berr*berr
*
* square error on height
*
      yerr1   = 0.5d0 * dc * xhalf / CONST
      yerr1   = yerr1*yerr1

      yerr    = yerr1 * (dely * dely)
*
* add in quadrature
*
      error   = real(sqrt( xerr + yerr + berr ))
      end
