      subroutine copy_guess(deccntr,fitpar,guess)
*+
* Name:
*    COPY_GUESS

* Invocation:
*    CALL COPY_GUESS(DECCNTR,FITPAR,GUESS)

* Purpose:
*    Take guesses from fit parameters

* Description:
*    To copy the guesses from the fit parameters to the guess store,
*    scaling them to match the normalised data.

* Arguments:
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit coding
*      FITPAR(*) = REAL ARRAY (Given)
*        Old fit parameters
*      GUESS(*) = REAL ARRAY (Returned)
*        Guesses
* Subroutines/functions referenced:

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW: 19-SEP-1991 Original version
*-
      implicit none
      include 'opt_cmn'
      include 'status_inc'
      include 'fit_coding_inc'
      real fitpar(*)
      real guess(4,*)

*

      integer cmp,counter,n

* At present a different format is used for single fits to that for
* doubles and multiples.

      if((deccntr(FIT_OPT).gt.1).or.(deccntr(FIT_NCMP).gt.1)) then
        counter = 2
        do cmp = 1, deccntr(FIT_NCMP)
          guess(1,cmp) = (fitpar(1) - real(denszero))/real(densc)
          guess(2,cmp) = fitpar(counter)/real(datsc)
          counter = counter + 1
          guess(3,cmp) = fitpar(counter)/real(densc)
          counter = counter + 1
          guess(4,cmp) = (fitpar(counter) - real(datazero))/
     :         real(datsc)
          counter = counter + 1
        end do
      else
        if(deccntr(BACK_MODEL).eq.0) then
          n = 3
        else if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
          n = 4
        else
          n = 5
        end if
        guess(1,1) = fitpar(2)/real(datsc)
        guess(2,1) = fitpar(3)/real(densc)
        guess(3,1) = (fitpar(4) - real(datazero))/real(datsc)
        if(n.gt.3) guess(4,1) = (fitpar(1) - real(denszero))/
     :       real(densc)
        if(n.gt.4) guess(1,2) = fitpar(5)
        if(n.gt.5) guess(2,2) = fitpar(6)
      end if
      end
