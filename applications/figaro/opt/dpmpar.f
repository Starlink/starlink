      double precision function dpmpar(i)
*+
* Name:
*    DPMPAR

* Invocation:
*   (DOUBLE PRECISION) = DPMPAR(I)
      integer i
*
* Description:
*     This function provides double precision machine parameters
*     when the appropriate set of data statements is activated (by
*     removing the c from column 1) and all other data statements are
*     rendered inactive. Most of the parameter values were obtained
*     from the corresponding Bell Laboratories Port Library function.

* Arguments:
*    I = INTEGER (Given)
*       is an integer input variable set to 1, 2, or 3 which
*         selects the desired machine parameter. If the machine has
*         t base b digits and its smallest and largest exponents are
*         emin and emax, respectively, then these parameters are
* Returned value:
*      -   dpmpar(1) = b**(1 - t), the machine precision,
*      -   dpmpar(2) = b**(emin - 1), the smallest magnitude,
*      -   dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude.

* History:
*     Argonne National Laboratory. MINPACK Project. June 1983.
*     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
*     T.N.Wilkins, Durham. Use prm_par. Sep 29 1993
*-
*     **********
      include 'prm_par'

      if(i.eq.1) then
         dpmpar = VAL__EPSD
      else if(i.eq.2) then
         dpmpar = VAL__SMLD
      else if(i.eq.3) then
         dpmpar = VAL__MAXD
      else
* Error
         dpmpar = VAL__BADD
      endif
      end
