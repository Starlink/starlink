      subroutine e_npoly(x,y,coeffs,k1,npts)
*+
* Name:
*    E_NPOLY

* Invocation:
*    CALL E_NPOLY(X,Y,COEFFS,K1,NPTS)

* Purpose:
*   Evaluate normal power series polynomial

* Description:
*   To evaluate a power series polynomial at a given set of points
*   Basically a wrapup of GEN_EPOLYD with the output returned
*   in the array Y

* Arguments:
*    X(NPTS) = DOUBLE PRECISION ARRAY (Given)
*         Array containing list of points to be evaluated
*    NPTS = INTEGER (Given)
*         Number of points to be evaluated
*    COEFFS(K1) = DOUBLE PRECISION ARRAY (Given)
*         Coeffs of Power series polynomial
*    K1 = INTEGER (Given)
*         Order+1 of series
*    Y(NPTS) = DOUBLE PRECISION ARRAY (Returned)
*         Output values corresponding to each X
* History
*    Original code TNW
*    Changed to support PDA AJH Oct 97
*-
      implicit none

* Import

      integer npts
      integer k1
      double precision x(npts)
      double precision coeffs(k1)
      double precision revcof(11)

* Export

      double precision y(npts)
* ---------------------------------------------------------------------
*
* Local


* do loop

      integer i

* FUNCTIONS


* FIGARO GEN_ library

      double precision gen_epolyd
      include 'PRM_PAR'

      do i=1,k1
         revcof(k1-i+1)=coeffs(i)
      end do

      do i=1,npts
        if(abs(x(i)).lt.1.0e38) then
*          y(i)=gen_epolyd(x(i),coeffs,k1)
          y(i)=gen_epolyd(x(i),revcof,k1)
        else

*     The user might guess

          y(i) = VAL__BADD
        end if
      end do

      end




