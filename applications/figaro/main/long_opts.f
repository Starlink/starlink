      subroutine long_opts(iopt,spdim2,iteration,status)
*+
* Name:
*    LONG_OPTS

* Invocation:
*    CALL LONG_OPTS(IOPT,SPDIM2,ITERATION,STATUS)

* Purpose:
*    Main menu for LONGSLIT

* Description:
*   Provides a menu and returns the option selected, for the main options
*   in LONGSLIT.

* Arguments:
*    SPDIM2 = INTEGER (Given)
*        2nd spatial dimension of data
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    IOPT = INTEGER (Returned)
*        Option selected:-
*                   1 - Add more lines
*                   2 - change iteration
*                   3 - Template Input - used for Lines or Continuum
*                   4 - Automatic fitting
*                   5 - Manual fitting
*                   6 - Define fits for automatic fitting
*                   7 - Edit results cube
*                   8 - Apply tolerances
*                   9 - Output results (rotation curves e.t.c.)
*                  10 - Sky line Vignetting correction or subraction
*                  11 - Synthetic Spectra from Spectrum/fits arithmetic
*                  12 - Exit from LONGSLIT
*                  13 - Cuban
*    ITERATION = INTEGER*2 (Given and returned)
*       Fit iteration (used for masking)

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham
*   DJA: D.J.Axon

* History:
*   TNW: 23/10/87 Original version
*   SOFT and HARD options removed TNW 17/11/87
*   STATUS argument added, TNW 27/9/89
*   QMENU used 3/11/89 TNW
*   Changed order and added new items DJA/LPO 15/4/91
*   Set iteration here, TNW 30/9/93
*-
      implicit none
      integer status,iopt,OPT_MANUAL,MNDICT,ndict,spdim2
      parameter (OPT_MANUAL = 5)
      parameter (MNDICT = 13)
      integer*2 iteration,newit
      integer dumi,OPT_IT,len1,pstat
      parameter (OPT_IT = 2)
      real value
      character dumc
      character*64 dict(MNDICT)
      data dict/
     :     'ADD : Add more lines',
     :     'IT %F : Reduce iteration (now ',
     :     'TEMPLATES : Input data for template based fits',
     :     'AUTO : Fixed window & fits type (use DEFINE first)'
     :     ,'MANUAL : Interactive setup of window & fits',
     :     'DEFINE : Define fits for AUTO'
     :     ,'EDIT : Edit/look at results structures',
     :     'TOLS : Apply tolerances',
     :     'OUTPUT : Create output plots, etc.',
     :     'SKY : Sky subtraction & data vignetting corrections',
     :'SYNTHETIC : Generate synthetic spectra from data &/or fits',
     :     'EXIT : Leave program',
     :     'CUBAN : Cuban-style interface'/


      if(status.ne.0) return
      if(spdim2.eq.1) then
        ndict = MNDICT - 1
      else
        ndict = MNDICT
      endif
      iopt = OPT_IT
      do while(iopt.eq.OPT_IT)
         len1 = 30
         dumi = iteration
         call chr_puti(dumi,dict(OPT_IT),len1)
         call chr_putc(')     ',dict(OPT_IT),len1)

         call qmenu('Main menu',dict,ndict,OPT_MANUAL,value,dumc,iopt,
     :     dumi,status)

*  If we want to change iteration do so now, if new value ok

         if(iopt.eq.OPT_IT) then
            newit = nint(value)
            if((iteration.gt.newit).and.(newit.ge.0)) then
               iteration = newit
            else
               call par_wruser('Error, invalid value for iteration',
     :              pstat)
            endif
         endif
      enddo
      end
