      subroutine chkparr(mxpars,spdim2,params)
*+
* Name:
*    CHKPARR

* Invocation:
*    CALL CHKPARR(MXPARS,SPDIM2,PARAMS)

* Purpose:
*  To check the parameter names array.

* Description:
*  To check the parameter names array, updating it if required. This is
*  because the older versions of LONGSLIT etc. didn't have a consistent
*  naming policy. The 2nd section is to trap a bug in earlier versions.
*
* Arguments:
*      MXPARS = INTEGER (Given)
*        Number of parameters
*      SPDIM2 = INTEGER (Given)
*        2nd spatial dimension of data
*      PARAMS = CHARACTER*(*) (Given and returned)
*        Parameter names array
*    Subroutines/functions referenced:
* Author:
*   T.N.Wilkins, Cambridge, 14-MAY-1990
* History:
*   T.N.Wilkins, 4-JUL-1991 PARAMS an argument-only used for old format
*-
      implicit none
      character*(*) params
      integer status,mxpars,spdim2
      integer tmpstart,tmpend
      logical update,newres

*

      update = .false.
      tmpstart = len(params) - 19
      tmpend = tmpstart + 7
      if(params(41:50).eq.'FWHM') then
        update = .true.
        newres = .false.
      else if(params(tmpstart:tmpend).eq.'E_Width_') then
        update = .true.
        newres = .true.
      end if
      if(update) then
        call par_wruser('Updating old parameter names array',status)
        call ofill_params(params,newres,(spdim2.gt.1),mxpars)
      end if
      end
