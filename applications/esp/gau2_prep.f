


* See gau2_pro for discussion

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
      include 'GAU_PAR'
      include 'PSX_ERR'
      include 'SAE_PAR'
      include 'CNF_PAR'

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
     :        guess, %val(cnf_pval(xinit)),
     :        %val(cnf_pval(workimg)),
     :        %val(cnf_pval(inc)))
      endif

      close (10)

*      write (*,'("gau2_prep: p=",i5," l=",i5," n=",i5,
*     :     " incc=",i5," lv=",i10," liv=",i5)')
*     :     p, l, n, incc, lv, liv

*   We've allocated the v and iv arrays, now initialise them
      call gau2_initv (liv, lv, niter, calcerrs,
     :     modtyp, %val(cnf_pval(iv)), %val(cnf_pval(v)),
     :     gau2par, status)

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
      include 'GAU_PAR'
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
         call err_rep (' ','gaufit: error freeing memory',status)
      endif

      end
