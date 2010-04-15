
* See gau2_pro for discussion

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
      include 'GAU_PAR'
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
