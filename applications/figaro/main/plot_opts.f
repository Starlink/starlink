      subroutine Plot_opts(iopt,status)
*+
* Name:
*    PLOT_OPTS

* Invocation:
*    CALL PLOT_OPTS(IOPT,STATUS)

* Purpose:
*   Offer user plotting options

* Description:
*   Offer user plotting options

* Desciption:
*   To provide a menu and return the option selected, for the OUTPUT
*   of line profiles  options  in LONGSLIT.
* Arguments:
*  STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*  IOPT = INTEGER (Returned)
*        Option selected:-
*                   1 - Plot All Lines
*                   2 - Plot Whole Spectrum
*                   3 - Plot Continuum Components
*                   4 - Residuals
*                   5 - Plot Error Bars
*                   6 - Bayes Confidence Limits for Components
*                   7 - Full Fit
*                   8 - Specific Lines Only
*                   9 - Individual Profile Components
*                  10 - Plot Models/ Dont Plot Models
*                  11 - Plot Data/Dont Plot Data
*                  12 - Quit
*   D.J.Axon Manchester 19/6/91
*-
      implicit none
      integer status, iopt, opt_manual, ndict
      parameter (OPT_MANUAL = 12)
      parameter (NDICT = 12)
      character*43 dict(NDICT)
      data dict/
     :     'ALL       : Plot All lines',
     :     'WHOLE     : Plot Whole Spectrum',
     :     'CONTINUUM : Show Continuum components',
     :     'RESIDUALS : Residuals',
     :     'ERRORS    : Profile error bars',
     :     'BAYES     : Bayes Confidence Limits',
     :     'TOTAL     : Full Fit',
     :     'SPECIFIC  : Specific Lines Only',
     :     'INDIVIDUAL : Individual Profile Components',
     :     'MODEL     : Plot Model',
     :     'DATA      : Plot Data',
     :     'QUIT      : Quit'/

      call qmenu('Plot menu',dict,NDICT,OPT_MANUAL,0.0,' ',iopt,0,
     :     status)
      end
