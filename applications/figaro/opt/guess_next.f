      subroutine guess_next(sdata,resid,m,g_p,absorption,work)
*+
* Name:
*    GUESS_NEXT

* Invocation:
*    CALL GUESS_NEXT(SDATA,RESID,M,G_P,ABSORPTION,WORK)
* Purpose:
*   To guess the parameters for the next gaussian in the fit

* Description:
*   The peak in the residuals is located, and the modal value of the
*   residuals. The mode is then subtracted and the peak delimited.
*   The standard deviation within this range is then found.

* Arguments:
*      M = INTEGER (Given)
*        Number of data points
*      SDATA(M) = REAL ARRAY (Given)
*        Wavelength etc. array
*      RESID(M) = REAL ARRAY (Given)
*        Intensity array
*      ABSORPTION = LOGICAL (Given)
*        If absorption line
*      G_P(4) = REAL ARRAY (Returned)
*        Parameters of next Gaussian
*      WORK(M*2) = REAL ARRAY (Workspace)
*   Subroutines referenced:
*     GET_MEDIAN : Find median value of data
*     SCALE_PARS : Scale parameters to range used for fitting

* History:
*   Changed TNW 29/11/88 To use getwork
*   TNW 17/9/91 Changes to allow use for absorption lines, etc.
*   TNW/Durham, 28/5/93 Workspace passed from above.
*-
      implicit none
      include 'prm_par'
      integer m
      real sdata(m)
      real resid(m)
      logical absorption
      real g_p(4)
      real work(m*2)
      double precision dg_p(4)
      integer i
      real height,mode
      real centre
      real unscaled_pars(4)
      integer centre_chan
      real diff,var,dat_tot
      integer outl,outr

* Ensure all values +ve and find maximum point

      if(absorption) then
        height = VAL__MAXR
        do i=1,m
          if(resid(i).lt.height) then
            centre_chan = i
            height = resid(i)
          end if
          resid(i) = min(0.0,resid(i))
        end do
      else
        height=0.0
        do i=1,m
          if(resid(i).gt.height) then
            centre_chan = i
            height = resid(i)
          end if
          resid(i) = max(0.0,resid(i))
        end do
      end if
      centre = sdata(centre_chan)

* Get modal value, and subtract from data, keeping all values +ve

      call get_median(resid,work,m,mode)
      if(absorption) then
        do i=1,m
          resid(i) = min(0.0,(resid(i)-mode))
        end do
      else
        do i=1,m
          resid(i)=max(0.0,(resid(i)-mode))
        end do
      end if

* Find left and right edges of line

      outl = 1
      outr = m
      do i = 2,centre_chan
        if(resid(i).eq.0.0) outl = i
      end do
      do i = (m-1),centre_chan,-1
        if(resid(i).eq.0.0) outr = i
      end do

* Work out standard deviation

      dat_tot = 0.0
      var = 0.0
      do i = outl,outr
        dat_tot = dat_tot + resid(i)
        diff = sdata(i) - centre
        var = var + (diff*diff*resid(i))
      end do
      var = var/dat_tot
      height = height-mode

* Put into parameters array and scale


* fwhm

      unscaled_pars(2) = sqrt(abs(var))
      unscaled_pars(3) = height
      unscaled_pars(4) = centre

* base

      unscaled_pars(1)=0.0
      call scale_pars(unscaled_pars,dg_p,4)
      do i = 1, 4
        g_p(i) = real(dg_p(i))
      enddo
      end
