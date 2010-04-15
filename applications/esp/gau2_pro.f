*
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
*




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
      include 'GAU_PAR'         ! gau2 parameters
      include 'CNF_PAR'

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
         status = sai__error
         call msg_seti ('NSOUR', nsour)
         call msg_seti ('UPIX', upix)
         call msg_seti ('P', p)
         call err_rep (' ', 'Sanity-check failed: '//
     :        'crazy input parameters: nsour=^NSOUR, upix=^UPIX'//
     :        ', p=^P', status)
         goto 999
      endif


*   There are a total of ELEMS pixels in the image, UPIX of which are
*   unmasked/good.  Denote the 6 arrays point(1..6) by p1,...,p6.  The
*   pixel values are in real p2(1..elems), with the good pixels at p2(p4(i)),
*   i=1..upix, and (x,y)=(p5(i),p6(i)), i=1..upix.  p4, p5, p6 all
*   integer(1..upix).

      call gau2_prep (%val(cnf_pval(point(2))),
     :     %val(cnf_pval(point(4))), elems, upix,
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
         call err_rep (' ', 'Could not allocate memory for image',
     :        status)
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
      call gau2_donsg (%val(cnf_pval(a)), %val(cnf_pval(da)),
     :     %val(cnf_pval(xinit)),
     :     %val(cnf_pval(in)), %val(cnf_pval(iv)),
     :     %val(cnf_pval(v)), %val(cnf_pval(img)),
     :     %val(cnf_pval(point(5))), %val(cnf_pval(point(6))),
     :     driftscale, liv, lv, upix, l, p,
     :     %val(cnf_pval(x)), %val(cnf_pval(c)), gau2par, status)

*   Calculate the errors in the X vector.  We no longer need xinit, so
*   rename that to xerrs and pass it to gau2_xerrs
      if (calcerrs) then
         xerrs = xinit
         call gau2_xerrs (%val(cnf_pval(iv)), %val(cnf_pval(v)),
     :        p, liv, lv, upix,
     :        %val(cnf_pval(xerrs)), sigma, gau2par, status)
      endif

      call gau2_rep (upix, %val(cnf_pval(iv)), %val(cnf_pval(v)),
     :     %val(cnf_pval(x)), %val(cnf_pval(c)),
     :     %val(cnf_pval(xerrs)), calcerrs, liv, lv, p, l, gau2par,
     :     status)

      if (modtyp .eq. gau2regdiag) then
         call gau2_getrd (%val(cnf_pval(iv)), liv,
     :        %val(cnf_pval(v)), lv,
     :        %val(cnf_pval(point(4))), upix, elems,
     :        %val(cnf_pval(point(3))),
     :        status)
      else
*      Create the output image
         call gau2_outim (modtyp, %val(cnf_pval(point(2))),
     :        %val(cnf_pval(point(4))),
     :        %val(cnf_pval(point(5))),
     :        %val(cnf_pval(point(6))),
     :        %val(cnf_pval(x)), %val(cnf_pval(c)),
     :        elems, upix, p, l, gau2par, %val(cnf_pval(a)),
     :        %val(cnf_pval(point(3))),
     :        status)
      endif

      call gau2_uinit (guess, guesserrs, calcerrs,
     :     %val(cnf_pval(x)), %val(cnf_pval(xerrs)),
     :     %val(cnf_pval(c)), p, l, back, status)

      call gau2_uprep (img, in, iv, v, x, xinit, c, gau2par, status)

 998  call psx_free (a,  status)
      call psx_free (da, status)

 999  continue
      call gau2_errs (gau2par, status)

      end
