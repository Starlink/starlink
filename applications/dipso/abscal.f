!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
!     SUBROUTINE ABSCAL
!
!     Purpose:  to absolutely calibrate IUE data from LBLFIT,
!     including temperature-dependent corrections but not
!     time-dependent corrections (from IUE Newsletter 19)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE ABSCAL
     : (PARAMS,
     : ASIZE1, WAVE, FLUX, NPOINT,
     : MAXBRK, BREAKS, NBREAK,
     : TITLE, WORV,
     : NPLOTS, DEVTYP, MARK, IPAL,
     : LBOX, POLTST, HSTTST, MRKTST, ROTST,
     : SUBCHK)

*

       IMPLICIT NONE

!
*   Common area
!
       INTEGER STKSZE, BSTSZE, MAXSTK, WORKSZ
       INTEGER STKLST, BSTLST, NONSTK

       PARAMETER (STKSZE = 100000,
     : BSTSZE = 5000,
     : MAXSTK = 50,
     : WORKSZ = 80000)

       INTEGER BSTACK(BSTSZE)
       INTEGER BSTNPT(MAXSTK), STKNPT(MAXSTK)
       INTEGER BPOINT(MAXSTK), POINTR(MAXSTK)

       REAL XSTACK(STKSZE), YSTACK(STKSZE)
       REAL WORK(WORKSZ)
       REAL WORVST(MAXSTK)

       CHARACTER*80 STITLE(MAXSTK)

       LOGICAL PUSHW

!
*  Imports
!
       CHARACTER*(*) PARAMS
       CHARACTER*(*) TITLE

       LOGICAL LBOX, POLTST, HSTTST, MRKTST, ROTST
       LOGICAL SUBCHK

       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK
       INTEGER BREAKS(MAXBRK)
       INTEGER NPLOTS, DEVTYP, IPAL

       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL MARK, WORV

       INTEGER IWORK(1)
       EQUIVALENCE(WORK(1), IWORK(1))

!
*  Local variables
!

       INTEGER ICAM, I

       REAL DTEMP, TCORR, ABSCOR

       CHARACTER*3 CAMSTR

!
*   Store coefficients for linear temperature corrections
*   (Used values from IUEDR, values from ESA Newsletter
*   19 commented out)
!

       REAL FIDTMP(3)
       REAL TFAC(3)

C      PARAMETER (FIDTMP1=XX.XX,
C    :            FIDTMP(2)=XX.XX,
C    :            FIDTMP(3)=XX.XX)
C      PARAMETER (TFAC(1) = -0.21,
C    :            TFAC(2) = -0.78,
C    :            TFAC(3) = -0.54)

       DATA FIDTMP /00.00, 12.00, 08.00/

       DATA TFAC /+0.00, -1.10, -0.50/

!
*   Get parameters
!

       SUBCHK = .FALSE.

       WRITE (*,
     : '(''   ABSCAL not implemented in this version of DIPSO'',A)')7

       END
