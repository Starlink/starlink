      subroutine updtmsk(fitsta,mask)
*+
* Name:
*    UPDTMSK

* Invocation:
*    CALL UPDTMSK(FITSTA,MASK)
*
* Purpose:
*   To update the mask array. Elements which have a successful fit
* Description:
*   To update the mask array. Elements which have a successful fit
* are left alone.
*
* Arguments:
*     FITSTA(NZP,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Fit status block
*     MASK(NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        Mask array
* Global variables:
*     NYP,NXP,NZP = INTEGER (Given)
*        Dimensions of fitsta block etc. (common arc_dims)
*     ITERATION = INTEGER (Given)
*        Iteration number (common arc_dims)
*     SPDIM2 = INTEGER (Given)
*        4th dimension of fitsta block (common arc_dims)
*     LINE_COUNT = INTEGER (Given)
*        Maximum number to consider for dimension 2 (common arc_dims)
*
*   Subroutine referenced:
*    DECODE_STATUS   : Decode fit status element
*
* Author:
*   T.N.Wilkins Manchester 8/7/88
* History:
*   TNW 15/8/88 Altered to avoid integer overflow if status word is a
*   missing value.
*   TNW IOA, Cambridge 15/12/89 Made into version for use by LONGSLIT
*   etc., as well as FIBDISP.
*   TNW IOA, 8/7/91 Now use fit status array and ARC_DIMS include file
*-
      implicit none
      integer i,j,line
      include 'arc_dims'
      integer*2 mask(nyp,nxp,spdim2)
      include 'status_inc'
      real fitsta(ncntrl,nyp,nxp,spdim2)

      do j = 1, spdim2
        do i = 1, nxp
          do line = 1, line_count

*        Check fit status, if fit ok then leave alone, otherwise
*        update to current value of ITERATION.

            call decode_status(ncntrl,fitsta(1,line,i,j),deccntr)
            if(deccntr(FIT_STAT).ne.1) then
              mask(line,i,j) = iteration
            end if
          end do
        end do
      end do
      end
