      subroutine describe_fit(deccntr,descrip)
*+
* Name:
*    DESCRIBE_FIT

* Invocation:
*    CALL DESCRIBE_FIT(DECCNTR,DESCRIP)

* Purpose:
*    To describe a fit given the decoded fit status.

* Description:
*    To describe a fit given the decoded fit status.
*
* Arguments:
*      DECCNTR(*) = INTEGER ARRAY (Given)
*        Profile model
*      DESCRIP = CHARACTER*(*) (Returned)
*        Description
* Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 24-MAY-1991
* History:
*          "          "      1-8-JUL-1991 Various
*-
      implicit none
      character*(*) descrip
      include 'status_inc'
      include 'fit_coding_inc'

*

      integer len1
      character*8 fittyp(0:6)
      character*15 fitmod(0:5)
      character*18 fitdbl(2:5)
      character*29 fails(0:8)
      character*27 basmod(0:4)
      character*10 em_or_abs(0:1)
      character*6 optmet(0:3)
      character*8 gues(0:10)
      character*8 weigh(0:1)
c      character*1 constr(1)
      character*12 stst(0:2)
      data em_or_abs/'emission','absorption'/
      data fittyp/'?','Single','Double','Double','Double','Double',
     :      'Multiple'/
      data fitdbl/'fixed separation','fixed width ratio'
     :   ,'fixed height ratio','unconstained'/
      data fitmod/'?','Gaussian','Skew Gaussian','Cauchy function',
     :            'Centroid','Lorentz'/
      data fails/'no fit','successful','minor NAG error',
     :            'serious NAG error','Crash','Failed tolerances',
     :            'Failed tolerances (Nag error)',' ',' '/
      data basmod/'no','constant','cubic spline','Chebyshev polynomial',
     :      'stored Chebyshev polynomial'/
      data optmet/'?','E04GBF','E04KDF','L-M'/
      data gues/'?','Centroid','Peak','Bimodf',' ',' ',' ',' ',' ',' '
     :     ,'P Cyg'/
      data weigh/'No','Variance'/
c      data constr/' '/
      data stst/' ','Post-fit AIC','Pre-fit AIC'/

*   FIT_MODEL  - Model, e.g. Gaussian x
*   FIT_TYPE   - Type of fit x
*   FIT_NCMP   - Number of components x
*   FIT_OPT    - Optimisation method x
*   FIT_STAT   - Success status x
*   FIT_ABS    - If aborption or emission x
*   FIT_GUES   - Guessing method x
*   FIT_WEIGH  - Weighting method x
*   FIT_CONSTR - Constraints x
*   FIT_STST   - Statistical test to apply (if any) x
*   FIT_DYNWEI - Dynamic weight flag
*   BACK_MODEL - Model for background x
*   BACK_ORDER - Order for model (if polynomial etc.) x
*   BACK_WEIGH - Weighting method
*   BACK_OPT   -
*   BACK_LOCAL -
*   BACK_STAT  -
*   BACK_REMOV - Removal method

      call chr_fill(' ',descrip)
      len1 = 0
      call chr_appnd(fittyp(deccntr(FIT_TYPE)),descrip,len1)
      len1 = len1 + 1
      call chr_appnd(fitmod(deccntr(FIT_MODEL)),descrip,len1)
      len1 = len1 + 1
      call chr_appnd(em_or_abs(deccntr(FIT_ABS)),descrip,len1)
      call chr_putc(', ',descrip,len1)

      if(deccntr(FIT_TYPE).ge.MULTIPLE) then

*  Multiple

        call chr_puti(deccntr(FIT_NCMP),descrip,len1)
        call chr_putc(' components, ',descrip,len1)

* Double or single

      else if(deccntr(FIT_NCMP).eq.2) then
        call chr_putc(' (',descrip,len1)
        call chr_appnd(fitdbl(deccntr(FIT_TYPE)),descrip,len1)
        call chr_putc('), ',descrip,len1)
      end if

*  Background model

      call chr_appnd(basmod(deccntr(BACK_MODEL)),descrip,len1)
      call chr_putc(' base',descrip,len1)

      if(deccntr(BACK_MODEL).eq.CHEBYSHEV) then
        call chr_putc(' (order ',descrip,len1)
        call chr_puti(deccntr(BACK_ORDER),descrip,len1)
        call chr_putc(')',descrip,len1)
      end if

* Give success or otherwise of fit

      call chr_putc(', ',descrip,len1)
      call chr_appnd(fails(deccntr(FIT_STAT)),descrip,len1)

      if(len(descrip).gt.(len1+50)) then
        call chr_putc(', Optimisation ',descrip,len1)
        call chr_appnd(optmet(deccntr(FIT_OPT)),descrip,len1)
        call chr_putc(', Guessing ',descrip,len1)
        call chr_appnd(gues(deccntr(FIT_GUES)),descrip,len1)
        call chr_putc(', ',descrip,len1)
        call chr_appnd(weigh(deccntr(FIT_WEIGH)),descrip,len1)
        call chr_putc(' weighting, ',descrip,len1)
c        call chr_appnd(constr(deccntr(FIT_CONTR)),descrip,len1)
c        call chr_putc(', ',descrip,len1)
        call chr_appnd(stst(deccntr(FIT_STST)),descrip,len1)
      endif
      end
