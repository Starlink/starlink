
* See gau2_pro for discussion

      subroutine gau2_initv (liv, lv, niter, calcerrs,
     :     modtyp, iv, v, gau2par, status)

*+
*   Description:
*     Initialises the NSG working and parameter vectors IV and V.  This
*     is where we switch off all of NSG's chatter, and tell it whether
*     or not to produce a covariance matrix.
*
*   Arguments:
*     liv = integer (given)
*       The length of IV
*     lv = integer (given)
*       The length of V
*     niter = integer (given)
*       The maximum number of iterations allowed.  If negative, use the NSG
*       default (150)
*     calcerrs = logical (given)
*        Will we be calculating uncertainties?  If not, then inialise
*        iv(57) to be zero, suppressing (expensive) calculation of the
*        covariance matrix.
*     modtyp = integer (given)
*        If modtyp=gau2regdiag, then we will want to produce a
*        regression diagnostic array.
*     iv = integer(liv) (returned)
*       The integer parameter vector
*     v = doubleprecision(lv) (returned)
*       The real parameter vector
*     gau2par = integer(gau2len) (given and modified)
*       Array of parameters
*     status = integer (given)
*       Inherited status
*-

*   types
      implicit none
      include 'SAE_PAR'
      include 'GAU_PAR'

*   arguments
      integer liv, lv, niter, modtyp
      integer iv(liv)
      doubleprecision v(lv)
      logical calcerrs
      integer gau2par(gau2len)
      integer status

*      integer i,d,rows

      if (status .ne. sai__ok) return

      call gau2_divset (1, iv, liv, lv, v)

*   gau2_divset sets iv(1)=12.  We rely on this, so check it and return
*   with an error if not true
      if (iv(1) .ne. 12) then
         gau2par(gau2status) = gau2code
         gau2par(gau2xstatus) = 2
         status = sai__error    ! so we jump out
         call err_rep (' ','gaufit: coding error (2)',status)
         goto 999
      endif

*   Set the non-default upper limit on iterations
      if (niter .gt. 0) iv(18) = niter

*   Switch off printing altogether - set printing unit iv(prunit)=iv(21)
*   to zero, see NSG p9.
      if (gau2par(gau2debug) .le. 1) then
         iv(21) = 0
      endif

*   iv(57) controls calculation of covariance matrix and regression
*   diagnostic (selected by ORing with 1 and 2 respectively).  We want
*   the covariance matrix only if calcerrs is true.
      if (calcerrs) then
         iv(57) = 1
      else
         iv(57) = 0
      endif
*   We want the regression diagnostic if we're being asked to produce it
*   as one of the final products
      if (modtyp .eq. gau2regdiag)
     :     iv(57) = iv(57) + 2

 999  return
      end
