*+
*   This module contains a call to the non-linear solving routine NSG, as
*   contained in module dnsg.f.
*
*   It has the same calling sequence as gau1_pro, as it is intended
*   to be a drop-in replacement.
*
*   Below, I will use the term NSG (as in `NSG's working arrays', or `the
*   data as seen by NSG') to refer both to the algorithm as
*   expressed in the routine gau2_drnsg and the module dnsg.f, and
*   to the supporting parts of the algorithm contained in the
*   harness below.  This is true even though we're using DRNSG
*   (double-precision, reverse-communication, NSG), and the module
*   dnsg.f actually contains all the code for NSF as well.  References
*   to page or section numbers in `NSG' are to the `Usage Summary for
*   Selected Optimization Routines', in the file port-usage.ps.  The
*   detailed documentation of the routines is in port3doc.tar.gz,
*   available where PORT is (http://www.bell-labs.com/project/PORT/ is
*   the master).
*
*   Where there is any conflict between the variable names as used in
*   gaufit and gau2_pro, and the variable names as used in the NSG
*   documentation (ie, two names for the same object), I have used the
*   latter here, to make matching this module with its documentation
*   easier.  The conflicts are:
*
*      gaufit | NSG    | meaning
*     --------+--------+-------------------------------------------
*      upix   |  n     | number of good pixels/data points
*      nsour  |  l     | number of gaussians/linear parameters
*     5*nsour |  p     | 5 per gaussian/number of non-linear params
*
*
*   Bugs:
*     None I'm aware of at present.
*
*   Deficiencies:
*
*     We calculate the uncertainties on the fitted parameters by
*     square-rooting the diagonal elements of the covariance matrix
*     produced by NSG.  It would probably be useful to examine the
*     off-diagonal elements and produce some sort of goodness-of-fit
*     diagnostic based on those, but I don't know how to do that at
*     present.  Similarly, is there anything we could do with the
*     `regression diagnostic'?  NSG's covariance calculation makes two
*     assumptions about the observational errors (see gau2_xerrs below):
*     these seem to be borne out, but it would be good to check.
*     Finally, fully half the number of function evaluations in NSG are
*     for the calculation of the covariance matrix: should I support a
*     no-errors mode which omits these with consequent speedup?
*
*     NSG doesn't appear to produce uncertainties on the linear
*     parameters.  Is this an NSG limitation (surely not!), or something
*     I can easily supply myself?
*
*     Development wrinkles: gau1_texto doesn't deal with passerrs;
*     gau1_fmode hasn't been altered at all; gau2_* still have writes;
*     make sure to restrict the output gaussian angle to +/- pi,
*     worrying about what this will do to the corresponding error.
*-




      SUBROUTINE GAU2_PRO(NSOUR, MODTYP, ANGCON, ANGOFF, PSIZE,
     :     NITER, RLIM, BACK, SIGMA, ELEMS, UPIX, POINT, PRANGE,
     :     GUESS, GUESSERRS, calcerrs, STATUS)
*+              
*  Name:
*     GAU2_PRO
*
*  Purpose:
*     Determines the optimal parameters to fit NSOUR model gaussians to
*     the image data.
*
*  Language:
*     Fortran 77
*
*  Invocation:
*     CALL GAU2_PRO(NSOUR,MODTYP,
*                   ANGCON,ANGOFF,PSIZE,NITER,
*                   RLIM,BACK,SIGMA,
*                   ELEMS,UPIX,POINT,PRANGE,GUESS,GUESSERRS,
*                   STATUS)
*
*  Description:
*     The routine is an interface to the routine gau2_nlfit below, which
*     handles the interaction with the NSG routines taken from the
*     public-domain part of the PORT library, and incorporated into
*     dnsg.F
*
*  Arguments:
*     In general, the arguments here have the same meanings as in
*      gau1_pro, but with some omissions.
*
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     MODTYP = INTEGER (Given)
*        Type of output image.  If it's gau2whole
*        image of the complete model, if gau2residual, create the image and
*        subtract it from the residuals.  If gau2regdiag, get NSG to
*        calculate the regression diagnostic and put it in here.
*        The image goes into point(3).
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.  NOT YET USED!
*     ANGOFF = REAL (Given) 
*        Angular offset for position angles generated. Units degrees.
*       NOT YET USED!
*     PSIZE = REAL (Given)
*        Pixel size for display.  Units arcsec
*     NITER = INTEGER (Given)
*        Number of iterations.  Negative means use default 150.
*     RLIM(gau2maxfits) = REAL (Given)
*        The maximum distance from the origin at which profiling 
*        takes place.
*     BACK = REAL (Given, and possibly returned)
*        The background count for the image.  If negative, this is to be
*        fitted for, rather than given as input.
*     SIGMA = REAL (Given and returned)
*        Standard deviation value of BACK (ignored if back<0).  In fact,
*        the input value of this is _always_ ignored in this version, as
*        the calculation of the covariance matrix assumes a value based
*        on the size of the residual, and it is this value which is
*        returned on completion (see the NSG documentation and notes
*        below in gau2_xerrs).
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     UPIX = INTEGER (Given)
*        Number of non-masked pixels
*     POINT(6) = INTEGER (Given)
*        Pointers to the image arrays.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.  NOT YET USED!
*     GUESS(gau2maxfits,7) = REAL (Given and Returned)
*        Current parameter estimates.  Also returns best-fit estimates
*        of parameters.
*     GUESSERRS(gau2maxfits,7) = REAL (Returned)
*        Standard deviations of the corresponding parameters in guess
*     CALCERRS = REAL (given)
*        Should this routine calculate errors on GUESS()?
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Authors:
*     NG: Norman Gray, Glasgow
*
*  History:
*     21-May-1997 (NG)
*     (Original version)
*
*  Bugs:
*     None known.
*
*-

*  Type Definitions:
      implicit none             ! No implicit typing

*  Global Constants:
      include 'SAE_PAR'         ! Standard SAE constants
      include 'gau_par'         ! gau2 parameters

*   Status:
      INTEGER STATUS            ! Global status

*   Arguments Given:
      LOGICAL ANGCON            ! Angle rotation convention
      INTEGER MODTYP            ! Type of output image
      INTEGER NITER             ! Number of iterations
      INTEGER NSOUR             ! Number of sources
      INTEGER ELEMS             ! Number of pixels in the image
      INTEGER POINT(6)          ! Pointers to images
      INTEGER PRANGE(2)         ! Image size
      INTEGER UPIX              ! Number of unmasked pixels
      REAL ANGOFF               ! Angular offset
      REAL PSIZE                ! Pixel size/arcsec
      REAL BACK                 ! Background count value
      REAL RLIM(gau2maxfits)    ! Maximum source radius used
      REAL SIGMA                ! Std deviation of BACK
      
*  Arguments Given and Returned:
      REAL GUESS(gau2maxfits,7) ! Current estimates of the parameters
      REAL GUESSERRS(gau2maxfits,7) ! sd of parameter estimates
      logical calcerrs          ! calculate errors?

*   local variables
      
*   allocated image array
      integer img
      
*   Arguments passed to NSG
      integer l			! number of non-linear params=nsour or nsour+1
      integer p			! no. n.l. parameters = 5*l
      integer liv,lv            ! parameter arrays for NSG routines
      integer gau2par(gau2len)
*   gau2par(gau2status)=result code passed around this module
*   gau2par(gau2bg)=+ve if background is to be fitted
*   allocated arrays
      integer in,iv,v,x,xinit,c
*   arrays allocated for drnsg
      integer a,da
*   We reuse the space allocated to xinit below (for gau2_xerrs).  For
*   clarity, we rename the array to xerrs there
      integer xerrs

*   loop counter
      integer i

c$$$c$$$ Signal handler, to track down underflow.  I hope to return to
c$$$c$$$ this and track down the underflow which is happening in the NSG
c$$$c$$$ code (desipite its attempts to avoid it).  So leave this
c$$$c$$$ commented-out code in place, even though that's discouraged by
c$$$c$$$ the coding guidelines.
c$$$      integer ieee_handler
c$$$      external common_handler

*   Monitor how far the fitted solution drifts from its initial value,
*   and abort the fit if it strays too far.  Set the scales for the
*   calculation in driftscales, but leave the decision to function
*   gau2_vetodrift
      doubleprecision driftscale(5*gau2maxfits)

      if (status .ne. sai__ok) return

c$$$c$$$ Signal handler
c$$$      i = ieee_handler ("set", "underflow", common_handler)
c$$$      if (i .ne. 0) then
c$$$         print *, "Failed to establish fp handler"
c$$$      else
c$$$         print *, 'Established fp handler'
c$$$      endif

*   There are 5 non-linear parameters for each gaussian
      p = 5*nsour
*   There are NSOUR linear parameters - one for each gaussian - but this
*   will be incremented in gau2_prep if the background is to be fitted
*   for as well.
      l = nsour
      
*   Sanity-check the parameters.  Are these all the checks we need?
      if (nsour .lt. 1 .or. upix .lt. 1 .or. upix .lt. p+1) then
         call msg_seti ('NSOUR', nsour)
         call msg_seti ('UPIX', upix)
         call msg_seti ('P', p)
         call msg_out (' ', 'Sanity-check failed: '//
     :        'crazy input parameters: nsour=^NSOUR, upix=^UPIX'//
     :        ', p=^P')
         status = sai__error
         goto 999
      endif
      

*   There are a total of ELEMS pixels in the image, UPIX of which are
*   unmasked/good.  Denote the 6 arrays point(1..6) by p1,...,p6.  The
*   pixel values are in real p2(1..elems), with the good pixels at p2(p4(i)),
*   i=1..upix, and (x,y)=(p5(i),p6(i)), i=1..upix.  p4, p5, p6 all
*   integer(1..upix).
      
      call gau2_prep (%val(point(2)), %val(point(4)), elems, upix,
     :     back, niter, calcerrs, modtyp, 
     :     guess, img, in, iv, v, x, xinit, c,
     :     l, p, liv, lv, gau2par, 
     :     status)
      
*   Now allocate the space for the A and DA
*   matrices, and initialise the values required by drnsg.
*   The example in gau2_dnsg does some initialisation by a dummy call to
*   gau2_drnsg, but I don't need to do this here, as the extra arrays I
*   need I will allocate separately.
      
      if (status .ne. sai__ok) goto 999

*   Correspondence with rnsg documentation is 
*      nda = p                   ! no. columns of DA = ncols. of INC
*      la = n                    ! row-dim of A and DA
*      l1 = l                    ! =l because no (l+1)-th column
      call psx_calloc (upix*l, '_DOUBLE', a,  status)
      call psx_calloc (upix*p, '_DOUBLE', da, status)
      
      if (status .ne. sai__ok) then
         gau2par(gau2status) = gau2memory
         status = sai__error
         goto 998
      endif

*   Work out the limits for the drift limits.  It's not clear what the
*   best scales to choose here are.  Just use rlim() for the moment.
      do 10, i=1,nsour
         driftscale((i-1)*5+1) = rlim(i)
         driftscale((i-1)*5+2) = rlim(i)
         driftscale((i-1)*5+3) = rlim(i)*2.
         driftscale((i-1)*5+4) = rlim(i)*2.
*         driftscale((i-1)*5+5) = 2*3.141592654
         driftscale((i-1)*5+5) = 0.
 10   continue

      if (gau2par(gau2debug) .gt. 0) then
         write (*,20) upix,p,l,liv,lv,
     :        gau2par(gau2status),gau2par(gau2bg)
 20      format ('gau2_pro: upix=',i5,' p=',i4,' l=',i4,' liv=',i4,
     :        ' lv=',i10,' gau2status=',i1,' gau2bg=',i1)
      endif


*   Off we go (wheeee!)
      call gau2_donsg (%val(a), %val(da), %val(xinit), 
     :     %val(in), %val(iv), %val(v), %val(img), 
     :     %val(point(5)), %val(point(6)),
     :     driftscale, liv, lv, upix, l, p, 
     :     %val(x), %val(c), gau2par, status)
      
*   Calculate the errors in the X vector.  We no longer need xinit, so
*   rename that to xerrs and pass it to gau2_xerrs 
      if (calcerrs) then
         xerrs = xinit
         call gau2_xerrs (%val(iv), %val(v), p, liv, lv, upix, 
     :        %val(xerrs), sigma, gau2par, status)
      endif
      
      call gau2_rep (upix, %val(iv), %val(v), %val(x), %val(c), 
     :     %val(xerrs), calcerrs, liv, lv, p, l, gau2par, status)
      
      if (modtyp .eq. gau2regdiag) then
         call gau2_getrd (%val(iv), liv, %val(v), lv, 
     :        %val(point(4)), upix, elems, %val(point(3)),
     :        status)
      else
*      Create the output image
         call gau2_outim (modtyp, %val(point(2)), %val(point(4)),
     :        %val(point(5)), %val(point(6)), %val(x), %val(c), 
     :        elems, upix, p, l, gau2par, %val(a), %val(point(3)),
     :        status)
      endif
      
      call gau2_uinit (guess, guesserrs, calcerrs,
     :     %val(x), %val(xerrs), %val(c), p, l, back, status)
      
      call gau2_uprep (img, in, iv, v, x, xinit, c, gau2par, status)

 998  call psx_free (a,  status)
      call psx_free (da, status)

 999  continue
      call gau2_errs (gau2par, status)

      end

***** gau2_donsg
      
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
*     driftscale = real(gau2maxfits) (given)
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
      include 'gau_par'
      
*   arguments
      integer liv, lv, n, l, p
      doubleprecision a(n,l),da(n,p),x(p),xinit(p),c(l),v(lv),img(n)
      real driftscale(5*gau2maxfits)
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
*         See return codes in NSG sect. 3
            if (iv(1) .ge. 3 .and. iv(1) .le. 6) then
               gau2par(gau2status) = 0 ! that's OK
               gau2par(gau2xstatus) = iv(1) ! for reference
            else if (iv(1) .eq. 7 .or. iv(1) .eq. 8) then
               gau2par(gau2status) = gau2noconv
            else if (iv(1) .eq. 9 .or. iv(1) .eq. 10) then
               gau2par(gau2status) = gau2maxiter
               gau2par(gau2xstatus) = iv(18) ! pass back the iteration limit
            else
               gau2par(gau2status) = gau2unkerror ! odd...
               gau2par(gau2xstatus) = iv(1)
            endif
            
            if (gau2par(gau2status) .ne. 0) status = sai__error

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



***** gau2_prep

      subroutine gau2_prep (sourceimg, mask, elems, n, 
     :     bkgd, niter, calcerrs, modtyp, 
     :     guess, workimg, inc, iv, v, x, xinit, c,
     :     l, p, liv, lv, gau2par, 
     :     status)

*+      
*   Description:
*     This routine allocates and initialises the working arrays for the
*     NSG routines, based on the arrays passed to it from its
*     caller, gau2_pro.
*
*   Arguments:
*     sourceimg = real(elems) (given)
*       gaufit's point(2) array: the complete collection of pixels in
*       the image, some of which are bad.
*     mask = integer(n) (given)
*       gaufit's point(4) array: the indexes of IMG which correspond to
*       good pixels.
*     elems = integer (given)
*       The size of IMG
*     n = integer (given)
*       The size of MASK: the number of good pixels in the image.  This
*       is referred to as UPIX in the caller.
*     bkgd = real (given)
*       The size of the background in the image.  If negative, this is
*       to be fitted for.
*     niter = integer (given)
*       The maximum number of iterations allowed.  If negative, use the NSG
*       default (150)
*     calcerrs = logical (given)
*       Will we be calculating uncertainties (passed to gau2_init)
*     modtyp = integer (given)
*       What type of final model image will we produce.  Passed to gau2_initv.
*     guess = real(gau2maxfits,7) (given)
*       The initial estimates for the 10 gaussians.  See documentation
*       for gau2_inita.
*     workimg = integer (returned)
*       The data array which will be used in the rest of the NSG routines.
*       Allocated doubleprecision(upix), and initialised within
*       gau2_inita.
*     inc = integer (returned)
*       Work array for gaufit2.  Allocated integer(2,p), and initialised
*       within gau2_inita.
*     iv = integer (returned)
*       Work array for gaufit2.  Allocated integer(liv), and initialised
*       within gau2_inita.
*     v = integer (returned)
*       Work array for gaufit2.  Allocated doubleprecision(lv), and
*       initialised within gau2_inita.
*     x = integer (returned)
*       Work array for gaufit2.  Allocated doubleprecision(p), and
*       initialised within gau2_inita.
*     xinit = integer (returned)
*       Work array for gaufit2.  Allocated doubleprecision(p), and
*       initialised within gau2_inita.
*     c = integer (returned)
*       Work array for gaufit2.  Allocated doubleprecision(l), and
*       initialised within gau2_inita.
*     l = integer (given and possibly altered)
*       The number of linear parameters (ie, the number of gaussians) in
*       the fit.  This will be initialised to NSOUR in the caller, but
*       will be incremented here, if the background is to be fitted.
*     p = integer (given)
*       The number of non-linear parameters (this will be 5*l, but
*       there's no reason we should build this assumption in here).
*     liv = integer (returned)
*       The size of NSG's IV working array.
*     lv = integer (returned)
*       The size of NSG's V working array.
*     gau2par = integer(gau2len) (given)
*       Other parameters passed to and returned from NSG.  See include
*       file gau2_par
*     status = integer (given and returned)
*-

*   types and includes
      implicit none
      include 'gau_par'
      include '/star/include/psx_err'
      include 'SAE_PAR'

*   arguments
      integer elems, n, l, p, liv, lv
      real sourceimg(elems)
      integer mask(n)
      real guess(gau2maxfits,7)
      real bkgd
      logical calcerrs
      integer modtyp
      integer gau2par(gau2len), niter, status
*   arrays to be allocated
      integer workimg, inc, iv, v, x, xinit, c


*   Local variables
*   m=number of 1s in INC
      integer m,i
      integer incc
      

      if (status .ne. sai__ok) return

      
*   Zero all the elements of gau2par
      do 20, i=1,gau2len
         gau2par(i) = 0
 20   continue

*   debugging: set gau2debug positive - zero to suppress all
      gau2par(gau2debug) = 0

      if (bkgd .le. 0.0) then
         gau2par(gau2bg) = 1
         l = l+1
      else
         gau2par(gau2bg) = 0
      endif
*      write (*, '("gau2_prep: bkgd=",f10.5," gau2bg=",i1)')
*     :     bkgd,gau2par(gau2bg)
      
*   there will be p ones in INC (so m=p, in NSG documentation)
      m = p
      incc = l+1

      liv = 110 + l + p
      lv = 105 + p*(2*p+17) + 2*n + (l*(l+3)+1)/2 + (l+p)*(n+l+p+1)

*      write (*,'("gau2_prep: p=",i4," n=",i5," l=",i2," liv=",i5,
*     :     " lv=",i10)') p,n,l,liv,lv

      call psx_calloc (n,         '_DOUBLE',  workimg, status)
      call psx_calloc (liv,       '_INTEGER', iv,      status)
      call psx_calloc (lv,        '_DOUBLE',  v,       status)
      call psx_calloc (p,         '_DOUBLE',  x,       status)
      call psx_calloc (p,         '_DOUBLE',  xinit,   status)
      call psx_calloc (l,         '_DOUBLE',  c,       status)
      call psx_calloc (2*p,       '_INTEGER', inc,     status)
 
      if (status .eq. sai__ok) then
*      call a subroutine to initialise workimg and inc
         call gau2_inita (n, p, l, bkgd, sourceimg, mask, elems, 
     :        guess, %val(xinit), %val(workimg), %val(inc))
      endif

      close (10)

*      write (*,'("gau2_prep: p=",i5," l=",i5," n=",i5,
*     :     " incc=",i5," lv=",i10," liv=",i5)')
*     :     p, l, n, incc, lv, liv
      
*   We've allocated the v and iv arrays, now initialise them
      call gau2_initv (liv, lv, niter, calcerrs,
     :     modtyp, %val(iv), %val(v), gau2par, status)

      end

***** gau2_uprep

      subroutine gau2_uprep (img, in, iv, v, x, xinit, c, 
     :     gau2par, status)

*+
*   Description:
*     Undoes the memory allocations done in gau2_prep (qv)
*
*   Arguments:
*     Arguments as in gau2_prep
*-

      implicit none
      include 'gau_par'
      include 'SAE_PAR'

*   arguments
      integer img, in, iv, v, x, xinit, c, status
      integer gau2par(gau2len)
      
      if (status .ne. sai__ok) return
      
      call psx_free (img,   status)
      call psx_free (in,    status)
      call psx_free (iv,    status)
      call psx_free (v,     status)
      call psx_free (x,     status)
      call psx_free (xinit, status)
      call psx_free (c,     status)
      
      if (status .ne. sai__ok) then
         gau2par(gau2status) = gau2memory
         status = sai__error
      endif
      
      end

***** gau2_inita

      subroutine gau2_inita (n, p, l, bg, initdata, mask,
     :     initdatasize, guess, xinit, data, in)
      
*+
*   Description:
*     Initialise the data and in arrays from the initial data and mask.
*     Good pixels are initdata(mask(i=1..n))
*
*   Arguments:
*     n = integer (given)
*       Number of good data points
*     p = integer (given)
*       Number of non-linear parameters
*     l = integer (given)
*       Number of linear parameters, plus one if we're fitting the
*       background.
*     bg = real (given)
*       Size of the background to be subtracted off, or negative if the
*       background is to be fitted.
*     initdata = real(initdatasize) (given)
*       Initial data which is to be translated into the DATA() array.
*     mask = integer(n) (given)
*       Indexes of the good data points in initdata.
*     initdatasize = integer (given)
*       Size of initdata
*     guess = real(gau2maxfits,7) (given)
*       The initial estimates for the 10 gaussians.  
*       Mapping is:
*         guess(i,1): x-coord of gaussian 
*         guess(i,2): y-coord
*         guess(i,3): ?
*         guess(i,4): peak height/pixels
*         guess(i,5): major-axis sigma/pixels
*         guess(i,6): minor-axis sigma/pixels
*         guess(i,7): angle of major axis
*       See subroutine gau1_build.
*     xinit = doubleprecision(p) (returned)
*       The initial estimate of the parameters, obtained from guess(,)
*     data = doubleprecision(n) (returned)
*       The newly-allocated data array which will be passed to the NSG
*       routines.
*     in = integer(2,p) (returned)
*       The information array which is passed to the NSG routines.  See
*       the NSG documentation for details (too complicated to summarise
*       here).
*-

*   types and includes
      implicit none
      include 'gau_par'

*   arguments
      integer n, p, l, initdatasize
      real bg
      integer in(2,p)
      doubleprecision data(n),xinit(p)
      real initdata(initdatasize)
      integer mask(n)
      real guess(gau2maxfits,7)

*   local variables
      integer i, ngaussians
      
*   Copy the initial data to the newly-allocated array
      do 10, i=1,n
         data(i) = dble(initdata(mask(i)))
 10   continue

*   Background negative means fit background from data, in which case
*   l=ngaussians+1 
      if (bg .gt. 0.0) then
*      subtract off background
         do 20, i=1,n
            data(i) = data(i)-bg
 20      continue
         ngaussians = l
      else
         ngaussians = l-1
      endif

*   fill in the IN array IN(2,p).
*   Column j of DA [ie, DA(*,j)] should be the partial derivative with 
*   respect to X(IN(1,j)) of column IN(2,j) of A(X)
*   (number of gaussians is p/5)
      do 30, i=1,p
         in(1,i) = i
         in(2,i) = (i+4)/5
 30   continue

*   Fill in the initial estimates
      do 40, i=1,ngaussians
         xinit(5*(i-1)+1) = guess(i,1) ! x_0
         xinit(5*(i-1)+2) = guess(i,2) ! y_0
         xinit(5*(i-1)+3) = guess(i,5) ! major-axis sigma/pixels
         xinit(5*(i-1)+4) = guess(i,6) ! minor-axis sigma/pixels
         xinit(5*(i-1)+5) = guess(i,7) ! angle of major axis
*      Note no initial guess needed for (linear) peak height
 40   continue
      
      end

***** gau2_uinit

      subroutine gau2_uinit (guess, guesserrs, calcerrs,
     :     x, xerrs, c, p, l, back, status)

*+   
*   Description:
*     Put the current parameter set back into the array guess, inverting
*     the mapping described in gau2_inita.
*     
*   Arguments:
*     guess = real(gau2maxfits,7) (returned)
*       The returned estimates for the 10 gaussians.  
*       Mapping is:
*         guess(i,1): x-coord of gaussian 
*         guess(i,2): y-coord
*         guess(i,3): ?
*         guess(i,4): peak height/pixels
*         guess(i,5): major-axis sigma/pixels
*         guess(i,6): minor-axis sigma/pixels
*         guess(i,7): angle of major axis
*       See subroutine gau1_build.
*     guesserrs = real(gau2maxfits,7) (returned)
*       Standard deviations for guess()
*     calcerrs = logical (given)
*       Do we want errors?    If _not_,
*       then this routine should (defensively) set all the elements of
*       guesserrs to a negative value, which indicates that the
*       information is missing.
*     x = doubleprecision(p) (given)
*       The final vector of non-linear parameters
*     xerrs = doubleprecision(p) (given)
*       The standard deviations for x, remapped to guesserrs.
*     c = doubleprecision(ng) (given)
*       The final vector of linear parameters
*     p = integer (given)
*       The number of non-linear parameters
*     l = integer (given)
*       The number of non-linear parameters.
*     back = real (given and possibly returned)
*       The background count.  If this is negative, then the background
*       was being fitted, rather than provided, so l=ngaussians+1, and
*       BACK will be given the fitted value on return.
*     status = integer (given)
*       The inherited status
*-

*   types
      implicit none
      include 'SAE_PAR'
      include 'gau_par'

*   arguments
      integer p, l
      real guess(gau2maxfits,7), guesserrs(gau2maxfits,7), back
      doubleprecision x(p), xerrs(p), c(l)
      integer status
      logical calcerrs

*   local variables
      integer i, j, ngaussians

      if (status .ne. sai__ok) return
      
      if (back .lt. 0.0) then   ! we fitted background
         ngaussians = l-1
         back = c(l)
      else
         ngaussians = l
      endif

      do 20, i=1,ngaussians
         guess(i,1) = real(x(5*(i-1)+1)) ! x_0
         guess(i,2) = real(x(5*(i-1)+2)) ! y_0
         guess(i,5) = real(x(5*(i-1)+3)) ! major-axis sigma/pixels
         guess(i,6) = real(x(5*(i-1)+4)) ! minor-axis sigma/pixels
         guess(i,7) = real(x(5*(i-1)+5)) ! angle of major axis

         guess(i,4) = c(i)         ! height of peak
         guess(i,3) = 0.0          ! what is this value?

         if (calcerrs) then
            guesserrs(i,1) = xerrs(5*(i-1)+1)
            guesserrs(i,2) = xerrs(5*(i-1)+2)
            guesserrs(i,5) = xerrs(5*(i-1)+3)
            guesserrs(i,6) = xerrs(5*(i-1)+4)
            guesserrs(i,7) = xerrs(5*(i-1)+5)

            guesserrs(i,4) = -1.0 ! no errors available on these, yet
            guesserrs(i,3) = -1.0
         else
*         Flag all uncertainties as `missing'
            do j=1,7
               guesserrs(i,j) = -1.0
            enddo
         endif

 20   continue

      end

***** gau2_initv

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
      include 'gau_par'

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
         goto 999
      endif
      
*   Set the non-default upper limit on iterations
      if (niter .gt. 0) iv(18) = niter
      
*   Switch off printing altogether - set printing unit iv(prunit)=iv(21)
*   to zero, see NSG p9.
      if (gau2par(gau2debug) .gt. 1) then
         iv(21) = 1
      else
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


***** gau2_xerrs

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
      include 'gau_par'

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
      
***** gau2_rep

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
      include 'gau_par'

*   Arguments
      integer n, liv, lv, p,l,status
      integer iv(liv), gau2par(gau2len)
      doubleprecision v(lv), x(p), c(l), xerrs(p)
      integer ngaussians
      logical calcerrs

*   Local variables
      integer i

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
      call msg_setr ('SIGMA', real(sqrt(2*v(10)/(n-p))))
      call msg_out  (' ', '  Effective data s.d.: ^SIGMA', status)
*      Also write out the condition number from v(rcond)=v(53) (`the
*      reciprocal of the square-root of a lower bound on the
*      Euclidean condition number of the final Hessian
*      \nabla^2f(x^{final})', see NSG p18).
      call msg_setr ('COND', real(1/v(53)**2))
      call msg_out  (' ', '  Condition number:    >^COND', status)

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
      call msg_setr ('OPT',      
     :     1.0 - real(gau2par(gau2calcda)) / real(iv(30)))
*   ...which is what you might have guessed.
      call msg_out (' ', '  Optimisation metric: ^OPT', status)
         
      call msg_blank (status)
      
      end

***** gau2_calc
      
      subroutine gau2_calc (which, n, p, l, x, nf, xv, yv, 
     :     gau2par, a, da)

*+
*   Description:
*     This is the central routine, which calculates the array of
*     function and jacobian values.  It obtains arrays A(i,j) and
*     DA(i,j), where
*       A(i,g) is the height of gaussian number g at the position
*         corresponding to datapoint i;
*       DA(i,k=5(g-1)+n) is the derivative of gaussian number g, with
*         respect to parameter x_n (n=1..5), evaluated at the position
*         corresponding to datapoint i.
*
*   Arguments:
*     which = integer (given)
*       if which >= 0, calculate A; which <= 0, calculate DA; so do both if
*       which==0
*     n = integer (given)
*       number of data points
*     p = integer (given)
*       number of non-linear parameters
*     l = integer (given)
*       number of linear parameters (equal to the number of gaussians,
*       plus one if the background is being fitted)
*     x = doubleprecision(p) (given)
*       the current vector of parameters
*     nf = integer (given)
*       invocation number.  This is set by the routine gau2_drnsg.  It
*       is _not_ used by this routine, but it can be a useful debugging
*       aid, if problems appear in future.
*     xv = integer(n) (given)
*       xv(i) is the x-coordinate of the datapoint i
*     yv = integer(n) (given)
*       yv(i) is the y-coordinate of the datapoint i
*     gau2par = integer(gau2len) (given and modified)
*       Array of parameters and feedback.  See the include file gau_par
*       for the indexes
*     a = doubleprecision(n,ngaussians) (returned)
*       the matrix A (see above).  ngaussians=l, or ngaussians=l-1 if
*       the background is being fitted
*     da = doubleprecision(n,5*ngaussians) (returned)
*       the matrix DA (see above).
*-

*   type declarations
      implicit none
      include 'gau_par'

*   arguments
      integer n, p, l, gau2par(gau2len), nf
      integer which
      doubleprecision x(p), a(n,l+1), da(n,p)
      integer xv(n), yv(n)

*   local variables      
*   pixi is index 1..n
      integer pixi
*   Renamed parameters: ngaussians is the number of gaussians.  
      integer ngaussians
*   fitbkgd is true if we're fitting the background
      logical fitbkgd
*   gn is gaussian number 1..ngaussians.  ofs is offset into x array
      integer gn,ofs
*   temporary variables
      doubleprecision t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t14,t15,
     :     t16,t17,t21,t23,t26

      fitbkgd = (gau2par(gau2bg) .ne. 0)
   
      if (gau2par(gau2debug) .gt. 0)
     :     write (*, '("calcada: nf=",i2,"   which=",sp,i2)') nf, which

      if (fitbkgd) then
         ngaussians = l-1
      else
         ngaussians = l
      endif
      
*   loop over gaussians
      do 20, gn=1,ngaussians
         ofs = (gn-1)*5

*      loop over pixel numbers, increasing y fastest
         do 10, pixi=1,n
*         Parameters in x are x(ofs+1)=x0, x(ofs+2)=y0, 
*         x(ofs+3)=sa, x(ofs+4)=sb, x(ofs+5)=theta
*         partial derivatives in da are da(pixi,ofs+1)=dA(pixi)/dx0, etc
*         This is all made a bit simpler by each of the terms A_ij
*         depending on a separate set of parameters 
*         x_{5(i-1)+1} to x_{5(i-1)+5}.

            t1 = cos(x(ofs+5))
            t2 = xv(pixi)-x(ofs+1)
            t3 = t1*t2
            t4 = sin(x(ofs+5))
            t5 = yv(pixi)-x(ofs+2)
            t6 = t4*t5
            t7 = t3+t6
            t8 = x(ofs+3)**2
            t9 = 1/t8
            t14 = -t4*t2+t1*t5
            t15 = x(ofs+4)**2
            t16 = 1/t15
            t21 = t7**2
            t23 = t14**2
            t26 = exp(-t21*t9/2-t23*t16/2)

            if (which .ge. 0) then
               a(pixi,gn)     = t26
            endif

            if (which .le. 0) then
*            This array is filled in in column order - the wrong way
*            round.  I did some quick tests with a C analogue of this,
*            though, and it actually made very little difference.
               t10 = t7*t9
               t17 = t14*t16
               da(pixi,ofs+1) = (t10*t1-t17*t4)*t26
               da(pixi,ofs+2) = (t10*t4+t17*t1)*t26
               da(pixi,ofs+3) = t21/t8/x(ofs+3)*t26
               da(pixi,ofs+4) = t23/t15/x(ofs+4)*t26
               da(pixi,ofs+5) = (-t10*t14-t17*(-t3-t6))*t26
            endif

 10      continue
 20   continue

      if (fitbkgd .and. (which .ge. 0)) then
*      fitting background - add extra all-1 column to a(*,l)
         do 130, pixi=1,n
            a(pixi,l) = 1
 130     continue
      endif

      end


      subroutine gau2_errs (gau2par, status)

*+
*   Description:
*     If gau2err is zero, ensure status=sai__ok.  Else set status=sai__error
*     and print out a suitable message.  Registry of codes in gau_par.
*
*   Arguments:
*     gau2par = integer(gau2len) (given)
*       Error code and status information
*     status = integer (given, and possible modified)
*       SAE inherited status
*-

*   types
      implicit none
      include 'SAE_PAR'
      include 'gau_par'
      
*   Arguments
      integer gau2par(gau2len), status

*   Local variables
      integer errno, errinf
*   We need to pass a separate status variable to the msg_* routines.
      integer localstatus

*   It _should_ be the case that 
*   (status.eq.sai__ok .eqv. gau2par(gau2status).eq.0).  We don't rely
*   on this, but if it's not true, it suggests something has gone amiss
*   elsewhere.
      if (status.eq.sai__ok .neqv. gau2par(gau2status).eq.0) then
         call msg_seti ('ST', status)
         call msg_seti ('IST', gau2par(gau2status))
         call msg_out (' ', 'GAUFIT2: warning: status inconsistency '//
     :        'status=^ST but internal status=^IST', localstatus)
      endif

      errno = gau2par(gau2status)
      
      if (errno .eq. 0) then
         status = sai__ok

*      ...but put the final status code in the message, just for reference
         call msg_seti ('IV', gau2par(gau2xstatus))
         call msg_out (' ', 'GAUFIT2: converged (^IV)', localstatus)
      else
         status = sai__error
         
         call msg_out (' ', 'GAUFIT2: algorithm failed:', localstatus)
         
         errinf = gau2par(gau2xstatus)

         if (errno .eq. gau2noconv) then
            call msg_out (' ', 'no convergence', localstatus)
         else if (errno .eq. gau2maxiter) then
            call msg_seti ('MXIT', errinf)
            call msg_out (' ', 'too many iterations: max=^MXIT', 
     :           localstatus)
         else if (errno .eq. gau2unkerror) then
            call msg_seti ('CODE', errinf)
            call msg_out (' ', 'NSG error no. ^CODE', localstatus)
         else if (errno .eq. gau2memory) then
            call msg_out (' ', 'not enough memory', localstatus)
         else if (errno .eq. gau2drifted) then
            call msg_out (' ', 'solution drifted too far', localstatus)
         else if (errno .eq. gau2code) then
            call msg_seti ('CODE', errinf)
            call msg_out (' ', 'coding error no. ^CODE', localstatus)
         else
            call msg_seti ('CODE', errno)
            call msg_out (' ', 'Unexpected error!! no. ^CODE', 
     :           localstatus)
         endif

      endif

      end

***** gau2_outim

      subroutine gau2_outim (modtyp, img, idx, xco, yco, x, c, 
     :     elems, n, p, l, gau2par, a, model, status)
*+
*   Description:
*     Creates the output image, depending on the value of MODTYP.
*     If MODTYP=gau2whole, then create an image of the model.  If
*     MODTYP=gau2residual, create the model image and subtract it from
*     the data (in IMG) to get the residuals.  The image goes into MODEL.
*
*   Arguments:
*     modtyp = integer (Given)
*       The type of image to be constructed.  modtyp=gau2whole means whole
*       model, modtyp=gau2residual means subtract from data to get residuals.
*     img = real(elems) (Given)
*       The input data.  Ignored if modtype=gau2whole
*     idx = real(n) (Given)
*       Indices of the good pixels in img.
*     xco = real(n) (Given)
*       X-coordinates of the good pixels in img.
*     yco = real(n) (Given)
*       Y-coordinates of the good pixels in img.
*     x = doubleprecision(p) (Given)
*       The vector of non-linear parameters.
*     c = doubleprecision(l) (Given)
*       The vector of linear parameters.  c(1..ngaussians) are the peak
*       fluxes, c(l) is the background (if gau2par(gau2bg)==0, then
*       l=ngaussians, and the background was not fitted).
*     elems = integer (Given)
*       The total number of pixels in the image.
*     n = integer (Given)
*       The number of data points = the number of good pixels.
*     p = integer (Given)
*       The number of non-linear parameters
*     l = integer (Given)
*       The number of linear parameters.
*     gau2par = integer (Given)
*       gau2 parameters.
*     a = doubleprecision(upix,l) (Returned)
*       Work array, passed to gau2_calc to receive individual gaussians.
*     model = real(elems) (Returned)
*       The image to be constructed.
*     status = integer (Given)
*       Inherited status.
*-

*   Types
      implicit none
      include 'gau_par'
      include 'SAE_PAR'
      include 'PRM_PAR'         ! PRIMDAT primitive data constants

*   Arguments
      integer modtyp
      integer elems
      integer n
      integer p
      integer l
      real img(elems)
      integer idx(n)
      integer xco(n)
      integer yco(n)
      doubleprecision x(p)
      doubleprecision c(l)
      doubleprecision a(n,l)
      integer gau2par(gau2len)
      integer status

*   Arguments returned
      real model(elems) 

*   Local variables
      integer ngaussians, i, j
      real total
      logical residuals
      
      
      if (status .ne. sai__ok) return
      
      if (gau2par(gau2bg) .gt. 0) then ! we fitted the background
         ngaussians = l-1
      else
         ngaussians = l
      endif
      
      residuals = modtyp .eq. gau2residual

*   First call gau2_calc to get the contributions from the different
*   gaussians.  We don't need the jacobian information, so pass 0 in the
*   DA slot, and pass WHICH positive.
      call gau2_calc (+1, n, p, l, x, 0, xco, yco, 
     :     gau2par, a, %val(0))

*   Now initialise the output array with bad values
      do i=1,elems
         model(i) = VAL__BADR
      enddo

*   ...and sum the elements of A into the appropriate elements of MODEL
      do i=1,n
         total = 0.0
         do j=1,ngaussians
            total = total + c(j) * a(i,j)
         enddo
*      If we're calculating the residuals, then subtract the model from
*      the data.
         if (residuals) then
            model(idx(i)) = img(idx(i)) - total
         else
            model(idx(i)) = total
         endif
      enddo

      end

***** gau2_getrd

      subroutine gau2_getrd (iv, liv, v, lv, idx, upix, elems, img,
     :     status)

*+
*   Description:
*     Copy the regression diagnostic in V(IV(67)) into the appropriate
*     elements of img.
*
*   Arguments:
*     iv = integer(liv) (Given)
*       NSG's integer work array
*     liv = integer (Given)
*       Size of IV()
*     v = real(lv) (Given)
*       NSG's real work array
*     lv = integer (Given)
*       Size of V()
*     idx = integer(upix) (Given)
*       The indexes of the good pixels in the data array.
*     upix = integer (Given)
*       The number of good pixels
*     elems = integer (Given)
*       The size of the data and image arrays
*     img = real(elems) (Returned)
*       The array to be filled with the regression diagnostic.
*     status = integer (Given and Returned)
*       The inherited status.
*-

*   Types
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'         ! PRIMDAT primitive data constants

*   Arguments
      integer liv, lv, elems, upix
      integer iv(liv)
      doubleprecision v(lv)
      real img(elems)
      integer status

*   Arguments returned      
      integer idx(upix)

*   Local variables
      integer i, j              ! loop counters


      if (status .ne. sai__ok) return
      
*   First, initialise the destination array with bad values
      do i=1,elems
         img(i) = VAL__BADR
      enddo
      
*   iv(67) is the starting subscript in V() for the regression
*   diagnostic array (NSG p.14)
      j = iv(67)
      if (j .lt. 1) then
*      This is a `can't-happen' error.
         call msg_seti ('CODE', j)
         call msg_out (' ', 
     :        'No regression diagnostic available (code ^CODE)',
     :        status)
         status = sai__error
      else
         do i=1,upix
            img(idx(i)) = v(j)
            j = j + 1
         enddo
      endif

      end



c$$$c$$$ common signal handler.  SunOS5 only.  See Sun `Numerical
c$$$c$$$ Computation Guide'
c$$$      integer function common_handler (sig, sip, uap)
c$$$      
c$$$      integer sig
c$$$      structure /fault/
c$$$         integer address
c$$$      end structure
c$$$      structure /siginfo/
c$$$         integer si_signo
c$$$         integer si_code
c$$$         integer si_errno
c$$$         record /fault/ fault
c$$$      end structure
c$$$      record /siginfo/ sip
c$$$
c$$$* sys/machsig.h:
c$$$*   invalid=7  divide=3  overflow=4  underflow=5  inexact=6
c$$$      write (*, '("ieee exception ",i4," at address ",z8)')
c$$$     :     sip.si_code, sip.fault.address
c$$$      
c$$$      end
c$$$         
