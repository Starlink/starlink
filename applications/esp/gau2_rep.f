
* See gau2_pro for discussion

      subroutine gau2_rep (n, iv, v, x, c, xerrs, calcerrs,
     :     liv, lv, p, l, gau2par, status)

*+
*   Description:
*     Final remarks from the gau2_pro routine.  These should be
*     principally remarks about the performance of the algorithm, rather
*     than reporting of the results of the calculation, and the user
*     should be encouraged to examine the numbers produced here.
*
*   Arguments:
*     Not all of these arguments are used in this routine.  They are
*     present so that I can add further performance checks without
*     disruption.
*
*     n = integer (given)
*       Number of data points
*     iv = integer(liv) (given)
*       NSG integer parameters
*     v = doubleprecision(lv) (given)
*       NSG real parameters
*     x = doubleprecision(p) (given)
*       Vector of non-linear parameters
*     c = doubleprecision(l) (given)
*       Vector of linear parameters
*     xerrs = doubleprecision(p) (given)
*       Errors in x
*     calcerrs = logical (given)
*       Were errors calculated?
*     liv = integer (given)
*       Size of IV()
*     lv = integer (given)
*       Size of V()
*     p = integer (given)
*       Size of X and XERRS
*     l = integer (given)
*       Size of C
*     gau2par = integer(gau2len) (given)
*       gau2_pro parameters
*     status = integer (given)
*       Inherited status
*-

*   Types
      implicit none
      include 'SAE_PAR'
      include 'GAU_PAR'

*   Arguments
      integer n, liv, lv, p,l,status
      integer iv(liv), gau2par(gau2len)
      double precision v(lv), x(p), c(l), xerrs(p)
      integer ngaussians
      logical calcerrs

*   Local variables
      integer i
      double precision vt       ! temp real
      character remark*40       ! variable remark

      if (status .ne. sai__ok) return

      call msg_blank (status)
      call msg_out (' ', 'GAUFIT2: algorithm performance', status)

      if (gau2par(gau2bg) .eq. 0) then
*      background was subtracted, not fitted
         ngaussians = l
      else
         ngaussians = l-1
      endif

      if (gau2par(gau2debug) .gt. 0) then
         write (*,
     :     '("Finished! ngaussians=",i2," status=",i10," iv(1)=",i10)')
     :        ngaussians, status, iv(1)
         write (*,'(" x=",5f12.3)') (x(i),i=1,5*ngaussians)
         if (calcerrs)
     :        write (*,'("dx=",5f12.3)') (xerrs(i),i=1,5*ngaussians)
         write (*,'(" c=",5f12.3)') (c(i),i=1,ngaussians)
         if (gau2par(gau2bg) .ne. 0)
     :        write (*,'("Background fit=",f10.3)') c(l)
      endif

*      Write the value of the data standard deviation which was
*      _assumed_ in the calculation of the covariance matrix.  This
*      should be output at the end of the calculation, and EXAMINED by
*      the user, to check that it is roughly consistent with what would
*      have been input (it probably should be OK).  If this turns out to
*      be a problem, then we can have a think about the statistics of
*      what's going on here, and see if we can accomodate the user's
*      value in a statistically legitimate way.  See NSG sect. 10 for
*      notes about (simple) rescaling of this value.
      call msg_setr( 'SD', real(sqrt(2*v(10)/(n-p))))
      call msg_out (' ','Effective data s.d.: ^SD Check reasonable',
     :     status)

*      call msg_setr ('SIGMA', real(sqrt(2*v(10)/(n-p))))
*      call msg_out  (' ', '  Effective data s.d.: ^SIGMA', status)
*      Also write out the condition number from v(rcond)=v(53) (`the
*      reciprocal of the square-root of a lower bound on the
*      Euclidean condition number of the final Hessian
*      \nabla^2f(x^{final})', see NSG p18).
      vt = real(1./v(53)**2)
      if (vt .lt. 1e3) then
         remark = 'good'
      else
         if (vt .lt. 1e6) then
            remark = 'poor -- uncertainties plausible'
         else
            remark = 'not good -- uncertainties unreliable'
         endif
      endif
      call msg_setr('VT', real(vt))
      call msg_setc('REM', remark)
      call msg_out (' ','Condition number: ^VT ^REM',status)
*      call msg_setr ('COND', real(1/v(53)**2))
*      call msg_out  (' ', '  Condition number:    >^COND', status)

*   Calculate a metric to measure how well the assumptions in the calls
*   in mrnsg to calcada are being satisfied.
*     iv(6):  the number of A evaluations, including for regression calcn
*     iv(52): the number for regression calcn
*     iv(30): the number of DA evaluations, including for regression
*     iv(53): the number for regression calcn
*     gau2par(gau2calcda): the number of REcalculations of DA, done
*       because the DA left over from the previous call to calcada was
*       for a different X from the value of X required now.
*   Without the reusing of DA in mrnsg, there would be iv(6)+iv(30)
*   calls to calcada; with it, there were iv(6)+gau2par(gau2calcda).
*   Divide the second by the first, and linearly rescale the result so
*   that gau2calcda=0 is 1 (meaning ideal optimisation) and
*   gau2calcda=iv(30) is 0 (meaning that the `optimisation' saved
*   nothing).  Turns out, when you do that, that the metric is...
      vt = 1.0 - real(gau2par(gau2calcda)) / real(iv(30))
*   ...which is what you might have guessed.
      if (vt .gt. 0.95) then
         remark = 'Excellent!'
      else
         if (vt .gt. 0.25) then ! fairly arbitrary cut-off
            remark = 'Acceptable'
         else
            remark = 'Poor'
         endif
      endif
      call msg_setr('VT', real(vt))
      call msg_setc('REM', remark)
      call msg_out (' ','Optimisation metric: ^VT ^REM',status)
*      call msg_setr ('OPT',
*     :     1.0 - real(gau2par(gau2calcda)) / real(iv(30)))
*      call msg_out (' ', '  Optimisation metric: ^OPT', status)

      call msg_blank (status)

      end
