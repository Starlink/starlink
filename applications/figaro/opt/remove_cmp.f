      subroutine remove_cmp(fitpar,nparms,loop,ngauss,minht,minsig,
     :     maxsig,nagerror)
*+
* Name:
*    REMOVE_CMP

* Invocation:
*    CALL REMOVE_CMP(FITPAR,NPARMS,LOOP,NGAUSS,MINHT,
*                  MINSIG,MAXSIG,NAGERROR)

* Purpose:
*   Remove bad components in a fit

* Description:
*    To remove bad components in a fit, to loop the fit if required.
*   It is assumed that, whatever the bad component when optimised, the
*   bad one in the guesses is the last. This is done by checking that the
*   fit passes the tolerances on widths and miniumum height. If a NAG
*   error has occured then a component is removed also. If the number of
*   components on entering the routine is only one, then the fit is
*   accepted anyway.

* Arguments:
*     FITPAR(MPARMS) = REAL ARRAY (Given)
*        Fit parameters (results)
*     MINSIG = REAL (Given)
*        Minimum sigma
*     MAXSIG = REAL (Given)
*        Maximum sigma
*     MINHT = REAL (Given)
*        Minimum height
*     LOOP = LOGICAL (Returned)
*        If to loop to repeat fit
*     NAGERROR = LOGICAL (Given and returned)
*        If a NAG error has occurred
*     NPARMS = INTEGER (Given and returned)
*        Number of parameters
*     NGAUSS = INTEGER (Given and returned)
*        Number of gaussians
* Author:
*   T.N.Wilkins Manchester September 1987
* History:
*   Altered to reject on NAG error 5/11/87 TNW.
*   Resets NAGERROR to .false. if re-fitting, TNW 10/12/90
*   MPARMS removed, TNW 29/6/93
*-
      implicit none
      integer nparms,ngauss
      logical loop,nagerror
      real minht,minsig,maxsig
      real fitpar(*)
      integer i,status

      loop = .false.

* Prevent number of components from dropping below zero

      if(ngauss.eq.0) then
        return

* If a NAG error then remove one component anyway, without need to check
* tolerances.

      else if(nagerror) then
        nagerror = .false.
        goto 400
      end if

* Check for low heights - worst error
* Check for bad widths

      do i = 1, ngauss
        if((fitpar(i*3).lt.minht).or.
     :        (fitpar(i*3-1).lt.minsig).or.
     :        (fitpar(i*3-1).gt.maxsig)) then
          goto 400
        end if
      end do
      return

 400  continue
      call opt_wruser('...re-fitting, fits not acceptable',status)
      loop = .true.
      ngauss = ngauss - 1
      nparms = nparms - 3
      end
