      subroutine gr_ebar(ebar,x,y,xerr,yerr,ticklen,n)
*+
* Name:
*    GR_EBAR

* Invocation:
*    CALL GR_EBAR(EBAR,X,Y,XERR,YERR,TICKLEN,N)

* Purpose:
*   Plot error bars

* Description:
*   Plot error bars

* Arguments:
*     EBAR = CHARACTER*(*) (Given)
*        Error bars required, e.g. "X","XY" or "Y"
*     X(N) = REAL ARRAY (Given)
*        X array
*     Y(N) = REAL ARRAY (Given)
*        Y array
*     XERR(N) = REAL ARRAY (Given)
*        X errors
*     YERR(N) = REAL ARRAY (Given)
*        Y errors
*     TICKLEN = REAL (Given)
*        Ticklength (factor to multiply default)
*     N = INTEGER (Given)
*        Number of points

* Author:
*      T.N.Wilkins Manchester
*-
      implicit none
      include 'SAE_PAR'
      integer n
      real x(n),y(n),xerr(n),yerr(n),ticklen
      character*(*) ebar
      character*5 ebar1
      integer emin,emax,slot,status
      include 'bytesdef'
      include 'DYNAMIC_MEMORY'

* See if x or y error bars, or both are required.

      ebar1 = ebar(:(min(5,len(ebar))))
      call chr_ucase(ebar1)
      status = SAI__OK
      call getwork(n*2,'float',emin,slot,status)
      if(status.ne.SAI__OK) return
      emax = emin + n*bytes_float

* X error bars

      if(index(ebar1,'X').ne.0) then
        call gen_subaf(n,x,xerr,dynamic_mem(emin))
        call gen_addaf(n,x,xerr,dynamic_mem(emax))
        call pgerrx(n,dynamic_mem(emin),dynamic_mem(emax),y,ticklen)
      end if

* Y error bars

      if(index(ebar1,'Y').ne.0) then
        call gen_subaf(n,y,yerr,dynamic_mem(emin))
        call gen_addaf(n,y,yerr,dynamic_mem(emax))
        call pgerry(n,x,dynamic_mem(emin),dynamic_mem(emax),ticklen)
      end if
      call dsa_free_workspace(slot,status)
      end
