      subroutine sky_opts(iopt,status)
*+
* Name:
*    SKY_OPTS

* Invocation:
*    CALL SKY_OPTS(IOPT,STATUS)

* Purpose:
*  Provide menu of sky operation options.

* Description:
*   To provide a menu and return the option selected, for the sky options
*   in LONGSLIT.
* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    IOPT = INTEGER (Returned)
*        Option selected:-
*                   1 - Velocity Zero point corrections
*                   2 - Sky subtraction
*                   3 - Sky line Vignetting correction
*                   4 - Generate a Synthetic Sky Spectrum
*                   5 - Exit from SKY options
* History:
*   T.N.Wilkins Manchester 23/10/87
*   SOFT and HARD options removed TNW 17/11/87
*   STATUS argument added, TNW/CAVAD 27/9/89
*   QMENU used 3/11/89     "    "
*   Changed order and added new items DJA/LPO 15/4/91
*-
      implicit none
      integer status,iopt,OPT_MANUAL,NDICT
      parameter (OPT_MANUAL = 5)
      parameter (NDICT = 5)
      real dumr
      character dumc
      integer dumi
      character*43 dict(NDICT)
      data dict/
     :     'ZERO : Sky line velocity/width measurements',
     :     'REMOVE : Subtract sky',
     :     'VIG    : Sky line Vignetting correction',
     :     'SYNTHETIC :Generate synthetic Sky spectra',
     :     'EXIT   : Leave SKY'/

      call qmenu('Sky menu',dict,NDICT,OPT_MANUAL,dumr,dumc,iopt,dumi,
     :     status)
      end
