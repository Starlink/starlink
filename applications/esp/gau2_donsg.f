
* See gau2_pro for discussion

      subroutine gau2_donsg (a, da, xinit, in, iv, v, img,
     :     xv, yv,
     :     driftscale, liv, lv, n, l, p, x, c, gau2par, status)

*+
*   Description:
*     This is the routine which does all the work of running the NSG
*     algorithm.  It repeatedly calls gau2_rnsg, and calls gau2_calc
*     to supply NSG with the arrays of function and jacobian evaluations
*     it asks for.
*
*   Arguments:
*     a = doubleprecision(n*l) (given)
*       Work array.  This is filled with the array of function
*       evaluations by gau2_calc.
*     da = doubleprecision(n*p) (given)
*       Work array.  This is filled with the array of jacobian
*       evaluations by gau2_calc.
*     xinit = doubleprecision(p) (given)
*       Holds the initial-guess values of x.  The routine gives up,
*       setting gau2par(gau2status)=gau2drifted if X drifts too far from
*       this initial value.
*     in = integer(2,p) (given)
*       The index array, indicating which elements of the jacobian array
*       are which.  See the NSg documentation and gau2_inita for
*       details.
*     iv = integer(liv) (given)
*       Integer work array for NSG.
*     v = doubleprecision(lv) (given)
*       Real work array for NSG.
*     img = doubleprecision(n) (given)
*       The data!
*     xv = integer(n) (given)
*     yv = integer(n) (given)
*       img(i) is the data for point (xv(i),yv(i))
*     driftscale = rdoubleprecision(gau2maxfits) (given)
*       The scalings for the drift calculation.  See gau2_vetodrift for
*       details.
*     liv = integer (given)
*       The length of IV.
*     lv = integer (given)
*       The length of V.
*     n = integer (given)
*       The number of (good/non-masked) data points
*     l = integer (given)
*       The number of linear parameters/amplitudes.  This will be one
*       more than the number of gaussians if we're fitting the
*       background as well.
*     p = integer (given)
*       The number of non-linear parameters.  This will be five times
*       the number of gaussians (ie, 5 n-l parameters each).
*     x = doubleprecision(p) (returned)
*       The array of (non-linear) parameters which have been fitted.
*       This is The Answer!
*     c = doubleprecision(l) (returned)
*       The array of linear parameters to be fitted (ie, the amplitudes
*       of the gaussians).
*     gau2par = integer(gau2len) (given and modified)
*       Array of parameters and feedback.  See the include file gau_par
*       for the indexes
*     status = integer (given)
*       Inherited status
*-

*   type declarations
      implicit none
      include 'SAE_PAR'
      include 'GAU_PAR'

*   arguments
      integer liv, lv, n, l, p
      doubleprecision a(n,l),da(n,p),x(p),xinit(p),c(l),v(lv),img(n)
      doubleprecision driftscale(5*gau2maxfits)
      integer iv(liv), in(2,p), xv(n), yv(n), status

*   For a description of the parameters in gau2par, see gau_par
      integer gau2par(gau2len)

*   The routine returns gau2par(gau2status)=0 on normal completion, or 1 if the
*   parameters had wandered too far from their initial values.

*   Local variables

*   When we calculate a and da, we actually do both at once, since the
*   calculations of the derivatives only take a few multiplications more
*   than the calculations of the function values.  This will usually
*   result in a little wasted effort, but this will be repaid, since
*   whenever we want the gradients (less often than the values), they'll
*   be sitting there without further calculation.  Obviously, we can
*   only do this if we keep track of which evaluations correspond to
*   each other; that's what savenf, iv(6) and iv(7) are for.
*
*   The plan, then is this: drnsg returns asking for A (and giving an
*   invocation count iv(6)), or a DA (giving the invocation count iv(7)
*   to which the x vector corresponds).  If we're asked for an A, then
*   we calculate both with calcada, and store iv(6) in saveda.  If we're
*   asked for a DA, then we expect that the last thing we were asked for
*   was the A for this same x, which we can check by comparing
*   saveda=iv(7), and which we can immediately supply - this isn't
*   guaranteed, or even suggested, by the algorithm, nor do we rely on
*   it, but it turns out to be generally true in practice.  That is, the
*   usual, but not invariable, pattern is several requests for A,
*   followed by a request for DA evaluated at the last X at which we
*   evaluated A.  We monitor how true this is by counting up in
*   gau2par(gau2calcda) the number of times we had to call calcada
*   because we had to supply a DA `out of turn' - this number should be
*   close to zero.
*
*   The pattern is slightly different when we're calculating the
*   regression diagnostic - that's primarily (entirely?) requests for A
*   alone.
*
*   The documentation for drnsg says that A and DA are overwritten.
*   This appears not to be true for DA: inspection of the code suggests
*   that although A is replaced by part of its QR decomposition, DA is
*   simply read.  This is given support by examining checksums of the
*   two arrays before and after the call to drnsg.  This means that we
*   can safely leave DA in place and hand it over next time we're asked
*   for it.
      integer saveda

*   Save the current iteration number, so we can produce a report on
*   each iteration
      integer iterno

      integer i, ngaussians
      logical keeplooping
      real driftmetric, gau2_drift

      character *(80)line       ! output line
      integer nchar             ! running line length

      if (status .ne. sai__ok) return

      gau2par(gau2status) = 0

      if (gau2par(gau2debug) .gt. 1) then
         write (*,5) iv(1),(gau2par(i),i=1,3),n,l,p
 5       format ('gau2_donsg: iv(1)=',i3,' gau2par=(',3i4,') n=',i6,
     :        ' l=',i2,' p=',i3)
         do 10, i=1,p
            write (*,'("in(*,",i3,")=",2i4)') i, in(1,i), in(2,i)
 10      continue
      endif

*   Set the initial values of X to be XINIT, and keep a check on
*   how far the X has strayed from its initial value.
      do 20, i=1,p
         x(i) = xinit(i)
 20   continue

      if (gau2par(gau2bg) .gt. 0) then
*      fitting background
         ngaussians = l-1
      else
         ngaussians = l
      endif


      keeplooping = .true.
      saveda = 0
*   initialise negative, so we produce a header first time
      iterno = -1

*   Repeatedly call drnsg while iv(1) is returned as 1 or 2
      do while (keeplooping)

*      In the call to drnsg, pass only the copies of a or da
         call gau2_drnsg (a, x, c, da, in, iv,
     :        l, l, n, liv, lv, n, p, p, v, img)

         driftmetric = gau2_drift (ngaussians, x, xinit,
     :        driftscale)

         if (driftmetric .gt. 1.0) then

            gau2par(gau2status) = gau2drifted
            status = sai__error
            call err_rep (' ','gaufit: solution has drifted too far',
     :           status)
            keeplooping = .false.

         else if (iv(1).eq.1) then

*         Don't ever bother checking saveda - it's always overwritten by RNDG.
*         Calculate a and da for this evaluation-count, iv(6)
            if (gau2par(gau2debug) .gt. 0)
     :           write (*, '("A: nf=",i3,"   x="/(5e10.3))')
     :           iv(6), (x(i),i=1,p)
            call gau2_calc (0, n, p, l, x, iv(6), xv, yv,
     :           gau2par, a, da)

            saveda = iv(6)

         else if (iv(1).eq.2) then

            if (saveda .ne. iv(7)) then

*            Calculate just da for the x for which the evaluation-count
*            was iv(7)
               if (gau2par(gau2debug) .gt. 0)
     :              write (*, '("DA: nf=",i3,"/",i3," x="/(5e10.3))')
     :              iv(7), saveda, (x(i),i=1,p)
               call gau2_calc (-1, n, p, l, x, iv(7), xv, yv,
     :              gau2par, %val(0), da)

*            It's actually surprising we're being asked for this - count
*            up the cases where this happens and worry about it if it
*            happens too often.
               gau2par(gau2calcda) = gau2par(gau2calcda)+1

               saveda = iv(7)
            endif

         else
*         Finish loop
            keeplooping = .false.

*         Why has this happened?
*         See return codes in NSG Usage Summary, sect. 3
            if (iv(1) .ge. 3 .and. iv(1) .le. 6) then
               gau2par(gau2status) = 0 ! that's OK
               gau2par(gau2xstatus) = iv(1) ! for reference
            else if (iv(1) .eq. 7 .or. iv(1) .eq. 8) then
               gau2par(gau2status) = gau2noconv
               status = sai__error
               call err_rep (' ','convergence tests failed in NSG',
     :              status)
            else if (iv(1) .eq. 9 .or. iv(1) .eq. 10) then
               gau2par(gau2status) = gau2maxiter
               gau2par(gau2xstatus) = iv(18) ! pass back the iteration limit
               status = sai__error
               call err_rep (' ','too many iterations in NSG',status)
            else
               gau2par(gau2status) = gau2unkerror ! odd...
               gau2par(gau2xstatus) = iv(1)
               status = sai__error
               call msg_seti ('ECODE',iv(1))
               call err_rep (' ','unknown error (^ECODE) in NSG',
     :              status)
            endif

*         If there's any error, bail out
            if (status .ne. sai__ok) goto 999

         endif

*      iv(niter)=iv(31) is iteration number.  NSG p17
*      v(F)=v(10) is current f(x^{final}).  NSG p17
         if (iterno .ne. iv(31)) then
            if (iterno .lt. 0) then
*            first time round
               call msg_blank (status)
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012345678
*  IT    NF     DRIFT    NL'D RESID
* iii  iiii  ffffffffff  ffffffffff
               nchar=0
               call chr_putc ('   IT    NF     DRIFT    NL''D RESID',
     :              line, nchar)
               call msg_out (' ', line(1:nchar), status)
            endif
            iterno = iv(31)
*         Write out iteration number, the number of function
*         evaluations, the current value of the drift metric and the
*         size of the residual at the beginning of the last iteration
*         (v(10) peculiarly seems to be always zero at this point)
*         roughly normalised.  The last two columns aren't really
*         informative to the user, but they do provide numbers to glaze
*         at.
            write (line, '(t3,i3,t8,i4,t14,g10.3,t26,d10.2)')
     :           iterno, iv(6), driftmetric, v(13)/real(n)
            call msg_out (' ', line(:35), status)
         endif

      enddo

 999  continue

      end


***** gau2_drift

      real function gau2_drift (ng, x, xinit, driftscale)

*   Description:
*     Returns a measure of how far the parameters in X have drifted from
*     the initial values in XINIT.  The metric returned is scaled so
*     that >1 indicates `too far'.  The parameter scales on
*     which this decision is made are passed in driftscales, but the
*     detailed semantics of these scales are left up to this routine.
*
*   Arguments:
*     ng = integer (given)
*       The number of gaussians
*     x = doubleprecision(5*ng) (given)
*       The current parameter vector.
*     xinit = doubleprecision(5*ng) (given)
*       The initial parameter vector.
*     driftscale = doubleprecision(5*ng) (given)
*       The scales of the elements in the parameter vector.
*
*   Discussion:
*     What we do here is just add up (x(i)-xinit(i))/driftscale(i),
*     i=1..5*ng, where driftscale(i)>=0, and divide the result by the
*     number of positive values in driftscale().
*     We could alternatively make the calculation based
*     on whether each component, or each group of five, had drifted.
*
*     That is, we can exempt an element of the vector from this
*     calculation by setting the corresponding driftscale negative.
*
*     An earlier version of this routine returned a logical value.  This
*     is probably more sensible, given the potential variations in the
*     way the decision is made, but it inhibits me from providing a single
*     numerical measure of drift to present as feedback to the user.

      implicit none

*   arguments
      integer ng
      doubleprecision x(5*ng), xinit(5*ng), driftscale(5*ng)

*   local variables
      doubleprecision total
      integer i, npos

      total = 0.
      npos = 0

      do 10, i=1,5*ng
         if (driftscale(i) .gt. 0.0) then
            total = total + ((x(i)-xinit(i))/driftscale(i))**2
            npos = npos + 1
         endif
 10   continue

*   Scale the answer so that >1 means `too far'
      gau2_drift = real(total)/real(npos)

      return
      end
