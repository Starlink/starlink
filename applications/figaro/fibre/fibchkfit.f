      subroutine fibchkfit(fitsta,ndeccntr,ix,iy,fit)
*+
* Name:
*    FIBCHKFIT

* Invocation:
*    CALL FIBCHKFIT(FITSTA,NDECCNTR,IX,IY,FIT)
*
* Purpose:
*  To check whether to proceed with a fit (is the fit more "advanced"
*  than any previous fit?).
*
* Description:
*  To check whether to proceed with a fit (is the fit more "advanced"
*  than any previous fit?).
*
* Arguments:
*     FITSTA(NCNTRL,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status block
*     NDECCNTR(9) = INTEGER ARRAY (Given)
*        Fit model
*     IX = INTEGER (Given)
*        X point to check
*     IY = INTEGER (Given)
*        Y point to check
*     FIT = LOGICAL (Returned)
*        Whether to proceed with fit (or warn user)
*  Given in common ARC_DIMS:
*     SPDIM1 = INTEGER (Returned)
*        1st spatial dimension of data
*     SPDIM2 = INTEGER (Returned)
*        2nd spatial dimension of data
*     NZP = INTEGER (Returned)
*        1st dimension of results block
*
*   Subroutine referenced:
*     DECODE_STATUS  : Decode fit status word
*
* Author:
*    T.N.Wilkins Manchester
* History:
*    T.N.Wilkins Cambridge, 8-JUL-1991 Altered for new results structure
*-
      implicit none
      include 'arc_dims'
      integer ix,iy
      include 'status_inc'
      integer ndeccntr(max_profile_control)
      logical fit,tmpfit
      character*35 chars
      real fitsta(ncntrl,spdim1,spdim2)
      integer pstat

      call decode_status(1,fitsta(1,ix,iy),deccntr)
      if(deccntr(fit_stat).eq.1) then
        tmpfit = (ndeccntr(fit_model).gt.deccntr(fit_model)).or.
     :        ( (ndeccntr(fit_type).ge.deccntr(fit_type)).and.
     :          (ndeccntr(fit_model).eq.deccntr(fit_model)) )

        if(.not.tmpfit) then
          fit = .false.
          write(chars,
     :       '(''Point'',i3,'','',i3,'' has more advanced fit'')')ix,iy
          call par_wruser(chars,pstat)
        end if

      end if
      end
