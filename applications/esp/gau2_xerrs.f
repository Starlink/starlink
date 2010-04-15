

* See gau2_pro for discussion

      subroutine gau2_xerrs (iv, v, p, liv, lv, n,
     :     xerrs, sigma, gau2par, status)

*+
*   Description:
*     Calculate the standard deviations of the recovered X vector
*
*   Arguments:
*     iv = integer(liv) (given)
*       NSG's integer parameter vector
*     v = doubleprecision(lv) (given)
*       NSG's real parameter vector
*     p = integer (given)
*       The number of non-linear parameters
*     liv = integer (given)
*       Length of IV
*     lv = integer (given)
*       Length of V
*     xerrs = doubleprecision(p) (returned)
*       The standard deviations of X(i), obtained from NSG, remapped to
*       correspond to guess(10,7)
*     sigma = real (returned)
*       Returns the value of the data standard deviation _assumed_ for
*       the covariance calculation.
*     gau2par = integer(gau2len) (given and modified)
*       Array of parameters and status
*     status = integer (given)
*       Inherited status
*-

*   types
      implicit none
      include 'SAE_PAR'
      include 'GAU_PAR'

*   arguments
      integer p, liv, lv, status, n
      doubleprecision xerrs(p), v(lv)
      integer iv(liv)
      integer gau2par(gau2len)
      real sigma

*   local variables
      integer i, j

*   check inherited status
      if (status .ne. sai__ok) return

*   As described in NSG sect. 10, the lower triangle of the
*   covariance matrix CM, is stored in V(IV(26)..), stored as CM(1,1),
*   CM(2,1), CM(2,2), CM(3,1),..., and the diagonal elements of this are
*   the variances of the corresponding solution components X(i).  These
*   are calculated based on the assumptions: ``(1) that the observations
*   $y_i$ are subject to independent errors whose variances are well
*   estimated by $1/(n-p)$ times the residual sum of squares (i.e.,
*   $2f(x^{final})/(n-p)$), and (2) that approximating $f$ by a
*   second-order Taylor expansion does not introduce `too much' error
*   into the estimated covariance matrix.''

*   First, sanity-check that iv(covmat)=iv(26) - the starting index for the
*   covariance matrix - is positive.  See NSG p14.
      if (iv(26) .lt. 1) then
         call msg_out (' ', 'No covariance matrix', status)
         gau2par(gau2status) = gau2code
         gau2par(gau2xstatus) = 1
         status = sai__error
         call err_rep (' ','gaufit: coding error (1)',status)

      else

*      If the data standard deviation assumed by the covariance
*      calculation is bad, then we will want to scale these errors,
*      obtained from the covariance matrix, in some appropriate way.
*      See the discussion in comments in gau2_rep.
*
*      Need to return this value in argument SIGMA
         sigma = sqrt(2*v(10)/(n-p))

*      Copy the diagonal elements to XERRS
         j = iv(26)-1
         do 20, i=1,p
            j = j + i
            xerrs(i) = sqrt(v(j))
 20      continue

      endif

      end
