      subroutine check_statii(line,fit,nold,control,istartx,iendx,
     :                  istarty,iendy,fitsta)
*+
* Name:
*    CHECK_STATII

* Invocation:
*    CALL CHECK_STATII(LINE,FIT,NOLD,CONTROL,ISTARTX,IENDX,
*                       ISTARTY,IENDY,FITSTA)

* Purpose:
*   Check status of fits at given point and whether we want a new fit

* Description:
*   Check whether point has already been processed. If it has been
*   processed then fitting is only allowed if the proposed fit is more
*   sophisticated than the previously stored fit. Previous fits are only
*   considered for this if they were sucessful (and haven't since been
*   rejected).
*
*  Note: the philosophy behind the above is that normally LONGSLIT is used
*     to pick up the most well-defined features first. The input parameters are
*     then relaxed and LONGSLIT rerun to pick up progressively lower signal to
*     noise features. The user has only to tamper with the "fits" plane if this
*     approach does not meet with his approval.
*
*
* Arguments:
*    LINE = INTEGER (Given)
*        Current line
*    FIT = LOGICAL (Given)
*        If to go ahead with fitting
*    NOLD = INTEGER (Given)
*        Number of old fits
*    CONTROL(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        current fit control
*    ISTARTX = INTEGER (Given)
*        Start of fitting block
*    IENDX = INTEGER (Given)
*        End   "     "      "
*    ISTARTY = INTEGER (Given)
*        Start "     "      "
*    IENDY = INTEGER (Given)
*        End   "     "      "
*    FITSTA(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
* History:
*   TNW/CAVAD 5-JUL-1991 Simplified
*-
      implicit none
      integer line,nold
      logical fit
*local
      include 'status_inc'
      integer pmodel,ptype
      integer k
      integer status
      include 'arc_dims'
      integer fitsta(ncntrl,nyp,nxp,spdim2)
      integer control(ncntrl,nyp,nxp,spdim2)
      integer istartx,iendx,istarty,iendy,fitsav(max_control),ix,iy
      character*79 chars

* check on previous status of these elements in status arrays

* Search for most sophisticated (highest value of model element) model

      pmodel = 0
      ptype = 0
      do iy = istarty, iendy
        do ix = istartx, iendx

*       decode the status at this point

          call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
          if(deccntr(FIT_STAT).eq.1) then

*         We have a succesful fit, if it is more advanced than any we've
*         found so far then save it.

            if((deccntr(FIT_TYPE).gt.ptype).or.
     :            ( (deccntr(FIT_TYPE).eq.ptype).and.
     :                        (deccntr(FIT_MODEL).gt.pmodel) ) ) then
              pmodel = deccntr(FIT_MODEL)
              ptype = deccntr(FIT_TYPE)
              do k = 1, ncntrl
                fitsav(k) = fitsta(k,line,ix,iy)
              end do
            end if
          end if
        end do
      end do

      if(pmodel .eq. 0) then

* then a fit will be attempted and the datablock updated

        fit = .true.
        call opt_wruser('No previous fit',status)
      else

*   Describe previous fit

        call opt_wruser('Previous fit',status)
        call decode_status(ncntrl,fitsav,deccntr)
        call describe_fit(deccntr,chars)
        call opt_wruser(chars,status)

*   The point has already been fitted successfully before
*   fit will be attempted and the datablock updated
*   only if the current model is more advanced

*   check on current fit parameters in CONTROL

        call decode_status(ncntrl,control(1,line,istartx,istarty),
     :            deccntr)

        fit = (deccntr(FIT_TYPE).ge.ptype).or.
     :        ( (deccntr(FIT_TYPE).eq.ptype).and.
     :                (deccntr(FIT_MODEL).gt.pmodel) )
        if(.not.fit) nold = nold + 1
      end if
      end
