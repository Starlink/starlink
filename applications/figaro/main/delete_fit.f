      subroutine delete_fit(fitsta,ncntrl,nyp,nxp,ixstart,ixend,line)
*+
* Name:
*    DELETE_FIT

* Invocation:
*    CALL DELETE_FIT(FITSTA,NCNTRL,NYP,NXP,IXSTART,IXEND,LINE)

* Purpose:
*   Delete fits in results data array.

* Description:
*  To set the status word to 0xxx for the selected fits, thus
* effectively deleting the fit.
*
* Arguments:
*    NCNTRLL = INTEGER (Given)
*        Dimension of fitsta array
*    NYP = INTEGER (Given)
*        Dimension of fitsta array
*    NXP = INTEGER (Given)
*        Dimension of fitsta array
*    IXSTART = INTEGER (Given)
*        Start of fit (x-sects)
*    IXEND = INTEGER (Given)
*        End o fit
*    LINE = INTEGER (Given)
*        Line number
*    FITSTA(NCNTRL,NYP,NXP) = REAL ARRAY (Given and returned)
*        Fitsta array
*
*-
      implicit none
      integer ncntrl,nyp,nxp
      integer line
      integer ixstart,ixend
      integer fitsta(ncntrl,nyp,nxp)
      integer i
      include 'status_inc'
*
      do i = ixstart,ixend
        call decode_status(ncntrl,fitsta(1,line,i),deccntr)
        deccntr(FIT_STAT) = 0
        deccntr(BACK_STAT) = 0
        call encode_contrl(deccntr,ncntrl,fitsta(1,line,i))
      end do
      end
