      subroutine opt_wrfit(deccntr,fitpar,fiterr,iferr)
*+
* Name:
*    OPT_WRFIT

* Invocation:
*    CALL OPT_WRFIT(DECCNTR,FITPAR,FITERR,IFERR)

* Purpose:
*   To write the fit parameters (and errors if required) to the terminal

* Description:
*   To write the fit parameters (and errors if required) to the terminal

* Arguments:
*   DECCNTR(*) = INTEGER ARRAY(Given)
*      Fit coding
*   FITPAR(*) = REAL ARRAY (Given)
*      Fit parameters
*   FITERR(*) = REAL ARRAY (Given)
*      Fit errors
*   IFERR = LOGICAL (Given)
*      If to output errors

* Authors:
*   T.N.Wilkins/IoA Cambridge, 18-Oct-1991
*-
      implicit none
      include 'status_inc'
      include 'fit_coding_inc'
      real fitpar(*), fiterr(*)
      logical iferr

*

      integer len1, len2, ref, cmp, pstat, start
      character*80 parcha, errcha
      real width,width_err,skew_fwhm
      real EFOLD
      parameter (EFOLD = 2.35482004)

* Make up header

      call chr_fill(' ',parcha)
      if(deccntr(FIT_NCMP).gt.1) then
        len1 = 0
        call chr_putc('Component',parcha,len1)
        start = 10
      else
        start = 2
      endif
      len1 = start + 1

      if(iferr) then
        call par_wruser('Final values & errors :-',pstat)
      else
        call par_wruser('Guesses:',pstat)
      endif

      if(deccntr(FIT_NCMP).gt.0) then

        call chr_putc(
     :       'Centre            fwhm           Height         Base',
     :       parcha,len1)
        len1 = start + 62
        if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
          call chr_putc('Skew',parcha,len1)
        else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
          call chr_putc('Cauchy',parcha,len1)
        endif
        call par_wruser(parcha(:len1),pstat)

      else

        call chr_fill(' ',parcha)
        len1 = 0
        call chr_putc('Base = ',parcha,len1)
        call chr_putr(fitpar(1),parcha,len1)
        if(iferr) then
          call chr_putc(', error = ',parcha,len1)
          call chr_putr(fiterr(1),parcha,len1)
        endif
        call par_wruser(parcha(:len1),pstat)

      endif

      ref = 2
      do cmp = 1, deccntr(FIT_NCMP)
        call chr_fill(' ',parcha)
        call chr_fill(' ',errcha)
        if(deccntr(FIT_NCMP).gt.1) then
          len1 = 3
          call chr_puti(cmp,parcha,len1)
        endif

*   Centre

        len1 = start
        len2 = len1
        call chr_putr(fitpar(ref+2),parcha,len1)
        if(iferr) call chr_putr(fiterr(ref+2),errcha,len2)

*   Width

        len1 = start + 16
        len2 = len1
        width = fitpar(ref)
        if(iferr) width_err = fiterr(ref)
        if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
          width = skew_fwhm(fitpar(ref+3),width)
          if(iferr) call sk_fwhm_err(fitpar(ref-1),fiterr(ref-1),5,
     :         width_err)
        else if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
          width = width*EFOLD
          width_err = width_err*EFOLD
        else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
          width = width*2.0
          width_err = width_err*2.0
        endif
        call chr_putr(width,parcha,len1)
        if(iferr) call chr_putr(width_err,errcha,len2)

* Height

        ref = ref + 1
        len1 = start + 32
        len2 = len1
        call chr_putr(fitpar(ref),parcha,len1)
        if(iferr) call chr_putr(fiterr(ref),errcha,len2)

*   Base

        if(cmp.eq.1) then
          len1 = start + 48
          len2 = len1
          call chr_putr(fitpar(1),parcha,len1)
          if(iferr) call chr_putr(fiterr(1),errcha,len2)
        endif

*   Skew or Cauchy

        ref = ref + 1
        if((deccntr(FIT_MODEL).eq.SKEW_MODEL).or.
     :       (deccntr(FIT_MODEL).eq.CAUCHY_MODEL)) then
          ref = ref + 1
          len1 = start + 60
          len2 = len1
          call chr_putr(fitpar(ref),parcha,len1)
          if(iferr) call chr_putr(fiterr(ref),errcha,len2)
        endif
        call par_wruser(parcha(:len1),pstat)
        if(iferr) call par_wruser(errcha(:len2),pstat)
        ref = ref + 1
      enddo
      end
