       subroutine plot_gaussian(fitpar,n,m,fit_values,sdata,
     :      line_number,funct)
*+
* Name:
*    PLOT_GAUSSIAN

* Invocation:
*    CALL PLOT_GAUSSIAN(FITPAR,N,M,FIT_VALUES,SDATA,
*           LINE_NUMBER,FUNCT)

* Purpose:
*   Plot a single component of a multiple line profile fit.

* Description:
*   Plot a single component of a multiple line profile fit.
*
* Arguments:
*    FITPAR(N) = REAL ARRAY (Given)
*
*    N = INTEGER (Given)
*
*    M = INTEGER (Given)
*
*    SDATA(M) = REAL ARRAY (Given)
*
*    LINE_NUMBER = INTEGER (Given)
*
*    FUNCT(x, fitpar) = REAL ARRAY (Given)
*        Function to evaluate fit
*    FIT_VALUES(M,LINE_NUMBER) = REAL ARRAY (Returned)
*
*-
      implicit none
      integer m,n
      integer line_number
      real fitpar(n)
      real sdata(m)
      real fit_values(m,line_number)

* local

      integer i
      real funct

* We want to plot the Gaussian with its zero level at the bottom of the
* plot

      do i=1,m
        fit_values(i,line_number) = funct(sdata(i),fitpar)
      end do

*  draw result

      call pgline(m,sdata,fit_values(1,line_number))
      end
