      subroutine set_control(control,line,i,j,fit_status)
*+
* Name:
*    SET_CONTROL

* Invocation:
*    CALL SET_CONTROL(CONTROL,LINE,I,J,FIT_STATUS)

* Purpose:
*    To set an element of the control array

* Description:
*    To set an element of the control array
*
* Arguments:
*      LINE = INTEGER (Given)
*        Line to set
*      I = INTEGER (Given)
*        1st coordinate
*      J = INTEGER (Given)
*        2nd coordinate
*      FIT_STATUS(NCNTRL) = INTEGER ARRAY (Given)
*        Fit status
*      CONTROL(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Returned)
*        Control array
* Subroutines/functions referenced:

* Author:
*    TNW: T.N.Wilkins, Cambridge

* History:
*    TNW: 9-APR-1991 Original version
*-
      implicit none
      include 'arc_dims'
      integer control(ncntrl,nyp,spdim1,spdim2)
      integer line
      integer i
      integer j
      integer fit_status(ncntrl)
      integer k

*

      do k = 1,ncntrl
        control(k,line,i,j) = fit_status(k)
      end do
      end
