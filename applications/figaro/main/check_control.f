      subroutine check_control(control,iftransfer,newfit)
*+
* Name:
*    CHECK_CONTROL

* Invocation:
*    CALL CHECK_CONTROL(CONTROL,IFTRANSFER,NEWFIT)

* Purpose:
*    To check the control array for the presence of "normal" and
*    transfer fits.

* Description:
*    Transfer fits are identified by negative values of
*    control(ncntrl,*,*,*), normal fits by positive values over 100.

* Arguments:
*      CONTRrOL(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Control array
*      IFTRANSFER = LOGICAL (Returned)
*        If any transfer fits defined
*      NEWFIT = LOGICAL (Returned)
*        If any totally new (i.e. normal) fits defined

* Subroutines/functions referenced:

* Author:
*    T.N.Wilkins, Cambridge, 16-APR-1991
* History:
*-
      implicit none
      include 'arc_dims'
      integer control(ncntrl,nyp,spdim1,spdim2)
      logical iftransfer
      logical newfit

*

      integer line,i,j,curval,maxval,minval

      minval = control(1,1,1,1)
      maxval = minval

      do j = 1, spdim2
        do i = 1, spdim1
          do line = 1, line_count
            curval = control(ncntrl,line,i,j)
            maxval = max(curval,maxval)
            minval = min(curval,minval)
          end do
        end do
      end do
      iftransfer = minval.lt.0
      newfit = maxval.gt.100
      end
