
* See gau2_pro for discussion

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
      include 'GAU_PAR'

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
