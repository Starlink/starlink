      subroutine tied_param(deccntr,datsc,guess,ratio)
*+
* Name:
*    TIED_PARAM

* Invocation:
*    CALL TIED_PARAM(DECCNTR,DATSC,GUESS,RATIO)
*
* Description:
*    To get the parameter for tying fits.
*
* Purpose:
*    To get the parameter for tying fits.
*
* Arguments:
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Fit coding
*      DATSC = DOUBLE PRECISION (Given)
*        X scaling parameter
*      GUESS(7) = DOUBLE PRECISION ARRAY (Given and returned)
*        Guesses
*      RATIO = DOUBLE PRECISION (Returned)
*        Tying parameter
* Given in ARC_DIMS:
*      BATCH = LOGICAL (Workspace)
*        If in batch mode
* Subroutines/functions referenced:
*    CHR_PUTC, CHR_PUTR OPT_WRUSER, PAR_CNPAR, PAR_RDVAL
* Author:
*    T.N.Wilkins, Cambridge, 12-SEP-1991
* History:
*-
      implicit none
      include 'status_inc'
      include 'fit_coding_inc'
      include 'arc_dims'
      real guess(7)
      double precision ratio,datsc

*

      character*35 chars
      integer pstat,len1
      real value,gratio

* Work out value from guesses and tell user

      if(deccntr(FIT_TYPE).eq.DOUBLE_FS) then
        gratio = (guess(7) - guess(4)) * real(datsc)
      else if(deccntr(FIT_TYPE).eq.DOUBLE_FH) then
        gratio = guess(6) / guess(3)
      else
        gratio = guess(5) / guess(2)
      end if
      len1 = 0
      call chr_putc('Ratio from guesses is ',chars,len1)
      call chr_putr(gratio,chars,len1)
      call opt_wruser(chars(:len1),pstat)

* Get value from user

      call par_rdval('fitrat',0.0,1.0e30,gratio,' ',value)

* If interative, then cancel parameter

      if(.not.batch) call par_cnpar('fitrat')

* Copy into optimisation element RATIO, scaling for separation

      gratio = value
      if(deccntr(FIT_TYPE).eq.DOUBLE_FS) then
        ratio = dble(value)/datsc
      else
        ratio = dble(value)
      end if

* Alter guesses so they match the current ratio (this probably need
* only be done to one parameter, but for the moment we'll do it for both).

      if(deccntr(FIT_TYPE).eq.DOUBLE_FS) then
        value = (guess(7) + guess(4)) * 0.5
        guess(4) = value - 0.5 * gratio
        guess(7) = value + 0.5 * gratio
      else if(deccntr(FIT_TYPE).eq.DOUBLE_FH) then
        value = sqrt( guess(6) * guess(3) )
        guess(3) = value / sqrt(gratio)
        guess(6) = value * sqrt(gratio)
      else
        value = sqrt( guess(5) * guess(2) )
        guess(2) = value / sqrt(gratio)
        guess(5) = value * sqrt(gratio)
      end if
      end
