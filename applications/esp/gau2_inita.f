
* See gau2_pro for discussion

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
      include 'GAU_PAR'

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
