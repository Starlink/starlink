      subroutine comp_plot(xc,ngauss,m,fit_values,sdata,yrange,funct)
*+
* Name:
*    COMP_PLOT

* Invocation:
*    CALL COMP_PLOT(XC,NGAUSS,M,FIT_VALUES,SDATA,YRANGE,FUNCT)

* Purpose:
*  Plot components of a fit

* Description:
*  Plot a multiple gaussian on the screen or hardcopy device
* Each plot comprises the individual componments thereof.

* Arguments:-
*      XC(*) = REAL ARRAY (Given)
*        Fit parameters
*      NGAUSS = INTEGER (Given)
*        Number of gaussians
*      M = INTEGER (Given)
*        Number of pixels in window
*      SDATA(M) = REAL ARRAY (Given)
*        X array
*      FUNCT(X,FITPAR) = REAL ARRAY (Given)
*        Function to evaluate fit
*      FIT_VALUES(M,NGAUSS) = REAL ARRAY (Returned)
*        Values of fit to each component
*      YRANGE(3) = REAL ARRAY (Returned)
*        Y range of plot (3rd element is total
*                                  used for components).

* History:
*     TNW 21/8/92 Changed so functions gaussian/lorentz passed around as
*         arguments (external)
*-
* local
*
      implicit none
      integer m
      integer j,ngauss,k1
      real fit_values(m,ngauss),sdata(m)
      real xc(*)
      real yrange(3),x1,x2
      real fitpar(4)
      real funct
      external funct

      call pgqwin(x1,x2,yrange(1),yrange(2))
      call gr_spen(3)

* loop over the gaussians plotting the individual components

      yrange(3) = 0.0
      do j=1,ngauss
        k1=j*3-1
        fitpar(2) = xc(k1)
        k1 = k1 + 1
        fitpar(3) = xc(k1)
        k1 = k1 + 1
        fitpar(4) = xc(k1)
        if(fitpar(2).gt.0.0) then
          fitpar(1) = yrange(1)
        else
          fitpar(1) = yrange(2)
        end if
        yrange(3) = yrange(3) + fitpar(1)
        call plot_gaussian(fitpar,4,m,fit_values,sdata,j,funct)
      end do
      end
