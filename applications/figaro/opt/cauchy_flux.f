      real function cauchy_flux(sg_pars,sg_error,error)
*+
* Name:
*    CAUCHY_FLUX

* Invocation:
*   (REAL) = CAUCHY_FLUX(SG_PARS,SG_ERROR,ERROR)

* Purpose:
*   Evaluate area under Cauchy function

* Description:
*   The area of the CAUCHY function becomes INFINITE when the CACUHY
*   free parameter EXCEEDS SQRT(2.0). This condition is checked for
*    and trapped and causes the AREA to be retruned as
*          VAL__MAXR area undefined (infinite-could use VAL__BADR but
*          may cause a crash)
*   Similarly if the GAMMA function evaluation fails then we retrun the
*   AREA of the CAUCHY as
*           0      gamma function evaluation failed
* Arguments:
*   SG_PARS(5) = REAL ARRAY (Given)
*        Fit parameters (width, height, ignored, ignored, cauchy)
*   SG_ERROR(5) = REAL ARRAY (Given)
*        Errors on fit parameters
*   CAUCHY_FLUX = REAL (Returned)
*        Flux
*   ERROR = REAL (Returned)
*        Error on flux
* History:
*   DJA   Manchster 8/JULY/1991 Bug fix re AREA
*   TNW   Durham 11/12/92 Use ln(gamma) rather than gamma-slightly more robust
*   JWP   Feb 97  Removed NAG call tp S14ABF
* ---------------------------------------------------------------
*-
      implicit none
      include 'prm_par'

* import

      real sg_error(5)
      real sg_pars(5)

* export

      real error

* EXTERNAL REFERENCES

* NETLIB gamma function

      double precision dgamln

* digamma function

      real digamma

*local

      integer status, OK
      parameter (OK = 0)
      integer HEIGHT
      integer WIDTH
      integer CAUCHY_PAR
      parameter( CAUCHY_PAR = 5)
      parameter( WIDTH      = 1)
      parameter( HEIGHT     = 2)

* sqrt of two pi

      real RTWOPI
      parameter (RTWOPI = 2.506628275 )

* small CAUCHY critical value

      double precision SMALL
      parameter (SMALL = 1.0d-2 )

* SQRT(2.0D0)

      double precision TEST
      parameter ( TEST     = 1.414213562d0 )

* Ln2
      double precision LN2
      parameter ( LN2      =  0.6931471806d0)
      double precision c_sq
      real area
      double precision cauchy
      double precision d1
      double precision d2
      double precision d3
      double precision d4
      double precision d5

* value of gamma

      double precision lngamma1

* value of gamma

      double precision lngamma2
      real ss1
      real ss2
      real ss3
      real first_term
      real second_term
      real third_term
      real total_term
      integer ifail

* nag error messages

      character*60 gamma_error(2)
*
* Error messages
*
      data gamma_error/
     : 'Argument negative or 0 : Gamma set to 0',
     : 'Argument too large     : Gamma set to nearest valid argument'/

* get the CAUCHY parameter
      cauchy = sg_pars(CAUCHY_PAR)
      area   = 0.0
      error  = 0.0

* Establsih if we have infinite area

      if( cauchy .ge. TEST ) then

        area = VAL__MAXR
      else
*
* get first Area estimate as if a gausian and gausian flux

        area = sg_pars(HEIGHT) * sg_pars(WIDTH) * 0.5 * RTWOPI
        area = area /TEST

* Note that we have not properly evaluated the AREA
* as we need to multiply by the terms containing  the CAUCHY parameter.

*
*  trap very small cauchy
*
        if( abs(cauchy) .gt. SMALL) then
          c_sq   =  cauchy * cauchy
          d1     = 1.0d0 /c_sq
          d2     = d1 - 0.5d0
          d4     = 2.0d0 ** c_sq
          d5     = d4 - 1.0d0
          d3     = sqrt( d5 )
          ifail  = 1

*     EVALUATE GAMMA FUNCTION

          lngamma1 = dgamln(d2,ifail)
*
          if ( ifail .eq. OK) then
            ifail  = 1

*       EVALUATE GAMMA FUNCTION

            lngamma2 = dgamln(d1,ifail)

* if all is OK to this point then we can now modify the area
* to its correct value.

            if ( ifail .eq. OK) then
              area = area * exp(lngamma1-lngamma2) / d3
            else
              area = 0.0
              call par_wruser('Error evaluating outer gamma function'
     :                 ,status)
              call par_wruser(gamma_error(ifail),status)
            end if
          else
            area = 0.0
            call par_wruser('Error evaluating inner gamma function'
     :              ,status)
            call par_wruser(gamma_error(ifail),status)

*     ifail = OK

          end if

*   abs(cauchy)>0.01

        end if

* cauchy > test

      end if

      CAUCHY_FLUX = area

* Evaluate the error on the LOG(AREA) of the Cauchy profile
* we do it this way because we can then use the DIGAMMA function
* to evalaute some of the terms in the differentials with
* respect to CAUCHY

      first_term = digamma(real(d2),ifail)

      if ( ifail .NE. OK) then
        call par_wruser('Error evaluating digamma function',status)
      end if

* still carry on since digamma returns zero if there is a fail

      ifail = OK
      second_term = digamma(real(d1),ifail)

      if ( ifail .NE. OK) then
        call par_wruser('Error evaluating digamma function',status)
      end if

      second_term = second_term - first_term
      second_term = 2.0 * real(d1) * second_term /real(cauchy)
      third_term  = real(LN2 * d4 * cauchy)
      third_term  =  third_term / real(d5)
      total_term  =  second_term - third_term
*
*  individual error terms
*
      ss1 = sg_error(WIDTH)/sg_pars(WIDTH)
      ss2 = sg_error(HEIGHT)/sg_pars(HEIGHT)
      ss3 =  total_term * sg_error(CAUCHY_PAR)

* add in quadrature to get ERROR on log(AREA)
* and set return value of error by converting to error on AREA
*
      error = area * sqrt(ss1*ss1 + ss2*ss2 + ss3*ss3)

      end
