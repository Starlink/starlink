      subroutine multi_plot(xc,ngauss,m,fit_values,fit_total,sdata
     :   ,back,vbase,yrange)
*+
* Name:
*    MULTI_PLOT

* Invocation:
*    CALL MULTI_PLOT(XC,NGAUSS,M,FIT_VALUES,FIT_TOTAL,SDATA
*        ,BACK,VBASE,YRANGE)


* Purpose:
*   Plot a multiple gaussian on the screen or hardcopy device

* Description:
*   Each plot comprises both the summed profile of the
*   current model and the individual componments thereoff
*   to passes through the data are needed to accumplish this
*   This routine plots the summed profile.
*
*  Subroutines referenced:
*    PGLINE     : Draw polynomial line
*    GEN_ADDAF  : Add to real arrays
*    GEN_CFILL  : Set array to constant
*    GR_SPEN    : Select graphics pen
*
* Arguments:
*    XC(NGAUSS*3+1) = REAL ARRAY (Given)
*
*    NGAUSS = INTEGER (Given)
*
*    M = INTEGER (Given)
*
*    FIT_VALUES(M,NGAUSS) = REAL ARRAY (Given)
*
*    FIT_TOTAL(M) = REAL ARRAY (Given)
*
*    SDATA(M) = REAL ARRAY (Given)
*
*    BACK = INTEGER (Given)
*
*    VBASE(M) = REAL ARRAY (Given)
*        variable component of base (Chebyshev fit)
*    YRANGE(3) = REAL ARRAY (Given)
*

* History:
*  Altered to use GEN routines TNW 5/12/88
*  PGPLOT version TNW/Cambridge 3/90
*  T.N.Wilkins, Cambridge, 9-JUN-1992 Use fit_coding_inc
*  TNW 10/9/92, Minor changes
*
*-
      implicit none
      include 'fit_coding_inc'
      integer m
      integer i,j,ngauss
      real fit_values(m,ngauss)
      real sdata(m)
      real fit_total(m)
      real base
      real vbase(m)
      integer back
      real xc(ngauss*3+1)
      real yrange(3),value

      call gr_spen(2)
      base=xc(1)
      if(back.ge.CUBIC_SPLINE) then
        do i=1,m
          fit_total(i) = base + vbase(i)
        end do
      else
        call gen_cfill(1,m,base,fit_total)
      end if

* Components will be plotted offset from the plot bottom or top, rather
* than 0 neccesarily.

      value = -yrange(3)
      call gen_addcaf(fit_total,m,value,fit_total)

* Now add components

      do j = 1, ngauss
        do i = 1, m
          fit_total(i) = fit_values(i,j) +  fit_total(i)
        enddo
      enddo

      call pgline(m,sdata,fit_total)
      end
