      subroutine weight_fit(y_error,npts,weight,mode)
*+
* Name:
*    WEIGHT_FIT

* Invocation:
*    CALL WEIGHT_FIT(Y_ERROR,NPTS,WEIGHT,MODE)

* Purpose:
*   Set up weights array for optimization

* Description:
*   The weights are either all set to 1, or to 1/y_error for each point.

* Arguments:
*    Y_ERROR(NPTS) = REAL ARRAY (Given)
*        standard deviations for the dependant variable
*    NPTS = INTEGER (Given)
*        nunber of datum
*    MODE = LOGICAL (Given)
*        .true. if weights to be applied
*    WEIGHT(NPTS) = DOUBLE PRECISION ARRAY (Returned)
*        weights array

* History:
*     Change to order of statements so test for mode is
*     carried out once only, TNW 25/11/88
*     TNW/CAVAD 11/4/89 Errors array made single precision
*
*- ---------------------------------------------------------------------
      implicit none
      integer npts
      real y_error(npts)
      logical mode
      double precision weight(npts)

* local

      integer i
*
*       evaluate weights
*
      if(.not.mode) then
        do i=1,npts
          weight(i) = 1.0d0
        end do
      else
        do i=1,npts
          weight(i) = 1.0d0/dble(y_error(i))
        end do
      end if
      end
