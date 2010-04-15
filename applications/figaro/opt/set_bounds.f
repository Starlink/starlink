      subroutine set_bounds(bounds,npar,bl,bu,times,max_cmp,
     :    max_times)
*+
* Name:
*    SET_BOUNDS

* Invocation:
*    CALL SET_BOUNDS(BOUNDS,NPAR,BL,BU,TIMES,MAX_CMP,
*         MAX_TIMES)

* Purpose:
*   Copy bounds from storage array to arrays for fitting

* Description:
*   This routine copies the upper and lower bounds as required
*   by NAG for a multiple gaussian fit
*
* Arguments:
*    NPAR = INTEGER (Given)
*        number of free parameters
*    TIMES = INTEGER (Given)
*
*    MAX_CMP = INTEGER (Given)
*
*    MAX_TIMES = INTEGER (Given)
*
*    BOUNDS(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Returned)
*        Bounds store ( absolute units)
*    BL(NPAR) = DOUBLE PRECISION ARRAY (Returned)
*        scaled lower bounds
*    BU(NPAR) = DOUBLE PRECISION ARRAY (Returned)
*        scaled upper bounds

* History:
*  TNW 24/1/89 Change to argument list-add max_cmp & max_times,
*  remove use of common
*
      implicit none
      integer MAX_PARS
      parameter (MAX_PARS  = 4)
*-
      integer npar
      integer max_cmp
      integer max_times
      integer times
      real bounds(MAX_PARS,max_cmp,max_times,2)
      double precision bu(npar)
      double precision bl(npar)
* -----------------------------------------------------------------

* Local

      integer ngauss
      integer n1
      integer j

      ngauss = npar/3

* loop over the number of Gausian components scaling the
* bounds for the height,centre and width of each component.

      do j = 1,ngauss
        n1 = j * 3 - 1

*   sigma

        bu(n1) = bounds(2,j,times,1)
        bl(n1) = bounds(2,j,times,2)
        n1 = n1 + 1

*   height

        bu(n1) = bounds(3,j,times,1)
        bl(n1) = bounds(3,j,times,2)
        n1 = n1 + 1

*   centre

        bu(n1) = bounds(4,j,times,1)
        bl(n1) = bounds(4,j,times,2)
      end do

* scale the base

      bu(1) = bounds(1,1,times,1)
      bl(1) = bounds(1,1,times,2)
      end
