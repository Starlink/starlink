      subroutine plot_fit(fitpar,funct,m,base)
*+
* Name:
*    PLOT_FIT

* Invocation:
*    CALL PLOT_FIT(FITPAR,FUNCT,M,BASE)
*
* Purpose:
*       Plot line profile fit
*
* Description:
*       Plot line profile fit
*
* Subroutines/functions referenced:
*     HANDLER       : General purpose condition handler
*     GR_SPEN       : Select graphics pen
*
*     PGQWIN        : Get limits of plot
*     PGDRAW        : Draw polyline
*     PGMOVE        : Move graphics pen
*
*     ESTABLISH : Establish condition handler
*
* Arguments:
*    FITPAR(*) = REAL ARRAY (Given)
*        Fit parameters
*    FUNCT = INTEGER (Given)
*        Function 1 Gaussian, 2 skew, 3 Cauchy
*    M = INTEGER (Given)
*        Number of elements in window
*    BASE(M*5) = REAL ARRAY (Given)
*        Base
*
* Altered  TNW 24/1/89 to remove use of common
* TNW 11/4/90 Use PGDRAW etc., thus not needing work arrays
* TNW 24/8/90 Made to allow variable base, also resolution *5 rather
* than *10 so as to be like other routines
* TNW 23/6/92 Lorentz added
*
*- --------------------------------------------------------------------
      implicit none
      integer funct
      integer m
      real fitpar(*)
      real base(m*5)
      include 'fit_coding_inc'
*
* local
*

* number of plot points

      integer m5

* step increment

      real xinc

* start x-value

      real st1

* function value at current step

      real pt1
      real xmin,xmax,ymin,ymax
      integer i
      real gaussian,skew,cauchy,lorentz
      logical first
      integer handler
      external handler
*
* select pen 2
*
      call gr_spen(2)
      call establish(handler)
*
*  improve resolution by a factor of 5 to get nice plot
*
      call pgqwin(xmin,xmax,ymin,ymax)
      m5  = (m-1)*5
      xinc = (xmax-xmin) / m5
      st1  = xmin
      m5  = m5+1
*
* loop over the data at the improved resolution
*
      i=1
      call pgbbuf
      first = .true.
      do  while(i.le.m5)
        if (funct.eq.GAUSSIAN_MODEL) then
          pt1 = gaussian(st1,fitpar)
        else if(funct.eq.SKEW_MODEL) then
          pt1 = skew(st1,fitpar)
        else if(funct.eq.CAUCHY_MODEL) then
*
* if cauchy very small jump out and plot gaussian
*
          if(abs(fitpar(5)) .le. 1.0e-3) then
            pt1 = gaussian(st1,fitpar)
          else
            pt1 = cauchy(st1,fitpar)
          end if
        else if(funct.eq.LORENTZ_MODEL) then
          pt1 = lorentz(st1,fitpar)
        end if
        pt1 = pt1 + base(i)
        if(first) then
          call pgmove(st1,pt1)
          first = .false.
        else
          call pgdraw(st1,pt1)
        end if
        if (abs(st1-fitpar(3)).gt.(fitpar(1)*2)) then
          st1 = st1+xinc*5
          i=i+5
        else
          st1 = st1+xinc
          i=i+1
        end if
      end do
      call pgebuf
*
* reset the graphics pen
*
      call gr_spen(1)
      end
