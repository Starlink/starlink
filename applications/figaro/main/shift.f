      subroutine shift(new,old,results,control,mask)
*+
* Name:
*    SHIFT

* Invocation:
*    CALL SHIFT(NEW,OLD,RESULTS,CONTROL,MASK)

* Purpose:
*    To shift a line in the results, control and mask arrays.

* Description:
*    To shift a line in the results, control and mask arrays.
*
* Arguments:
*      NEW = INTEGER (Given)
*        New position  for line
*      OLD = INTEGER (Given)
*        Old position  for line
* Global variables:
*      ITERATION = INTEGER (Given)
*        Current value of iteration
*      MXPARS = INTEGER (Given)
*        1st dimension of results
*      NYP = INTEGER (Given)
*        2nd dimension of results
*      NXP = INTEGER (Given)
*        3rd dimension of results
*      SPDIM2 = INTEGER (Given)
*        4th dimension of results)
*      RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results array
*      CONTROL(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Results array
*      MASK(NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Results array
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  7-FEB-1991
* History:
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp,spdim2)
      integer control(ncntrl,nyp,nxp,spdim2)
      integer*2 mask(nyp,nxp,spdim2)
      integer new,i,k,l,old
      include 'PRM_PAR'

      if(old.eq.new) return

*  Would be more efficient to reduce number of do loops, but this can
*  wait till it's working.

      do l = 1, spdim2
        do k = 1, nxp
          do i = 1, mxpars
            results(i,new,k,l) = results(i,old,k,l)
            results(i,old,k,l) = val__badr
          end do
          do i = 1, ncntrl
            control(i,new,k,l) = control(i,old,k,l)
          end do
          mask(new,k,l) = mask(old,k,l)
          mask(old,k,l) = iteration
        end do
      end do
      end
