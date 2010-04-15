!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
!   SUBROUTINE DACFIT
!   Sets up initial parameters (template, gaussian weights) for
!   discrete components fit
!   (DACCALC returns just calculated profile)
!
!   Imports:
!     NPOINT         Length of Wave, Flux arrays
!     NTERMS         No. of doublets to be fitted
!     WAVE(NPOINT)   Grid of observed wavelengths
!     COMMND         Command name
!
!   Updates:
!     FLUX(NPOINT)   Observed spectrum on input, returns fitted spectrum
!
!   Internal variables (some sharing WORK space):
!     TWAV, TFLX     Wavelengths and fluxes of stored template.
!                    Interpolated immediately onto observed wavelength grid
!     X, Y           Wavelengths and fluxes for calculating theoretical
!                    `narrows' spectra.   Ultimately collapsed to WAVE grid.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE DACFIT
     : (WAVE, FLUX, NPOINT,
     :  VARRAY, MAXV,
     :  DEVTYP, NPLOTS, CURSOR,
     :  TITLE, OK,
     :  INWORK, COMMND,
     :  ASIZE, MAXBRK, BREAKS, NBREAK)

       IMPLICIT NONE

!

       INTEGER CMNWSZ, STKSZE, MAXSTK, BSTSZE
       INTEGER ASIZE, MAXBRK, NBREAK
       INTEGER BREAKS(MAXBRK)
       INTEGER IHX
       PARAMETER
     : (CMNWSZ=80000, STKSZE=100000, MAXSTK=50, BSTSZE=5000)

       REAL XSTACK(STKSZE),YSTACK(STKSZE)
       INTEGER BSTACK(BSTSZE), BSTNPT(MAXSTK)
       INTEGER POINTR(MAXSTK), STKNPT(MAXSTK), BPOINT(MAXSTK)
       CHARACTER*80 STITLE(MAXSTK)
       CHARACTER*(*) COMMND
       REAL WORVST(MAXSTK)
       REAL CMNWRK(CMNWSZ)
       INTEGER NONSTK, STKLST, BSTLST

       INTEGER DEVTYP, NPLOTS
!

!       SAVE /RESTW/, /GWTS/, /XYHI/

!

       DOUBLE PRECISION FWHMC, CW1, CW2
       DOUBLE PRECISION FWHMN, NW1, NW2
       DOUBLE PRECISION FWHMS, SW1, SW2
       DOUBLE PRECISION FWHMAL, ALW1, ALW2
       DOUBLE PRECISION CLIGHT
       INTEGER MAXA, MAXN, MAXT, MAXX
       INTEGER INWORK

       PARAMETER (FWHMC = 0.155D0)
       PARAMETER (FWHMS = 0.140D0)
       PARAMETER (FWHMN = 0.124D0)
       PARAMETER (CW1 = 1548.188D0)
       PARAMETER (CW2 = 1550.762D0)
       PARAMETER (SW1 = 1393.755D0)
       PARAMETER (SW2 = 1402.770D0)
       PARAMETER (NW1 = 1238.808D0)
       PARAMETER (NW2 = 1242.796D0)
       PARAMETER (FWHMAL = 0.186D0)
       PARAMETER (ALW1 = 1854.720D0)
       PARAMETER (ALW2 = 1862.795D0)

       PARAMETER (MAXA = 27)
       PARAMETER (MAXN = 750)
       PARAMETER (MAXT = 1000)
       PARAMETER (MAXX = 3501)

       PARAMETER (CLIGHT = 2.997925D+05)

!

       INTEGER NPOINT, NTERMS, WORKSZ, MAXV
       INTEGER I, IERR, IUNIT
       INTEGER N, J, NCALLS
       INTEGER SLEN, NT, I1
       INTEGER IOUT

       REAL WAVE(NPOINT), FLUX(NPOINT)
       REAL VARRAY(MAXV)
       REAL TWAV(MAXT), TFLX(MAXT)
       REAL GWAV1, GWAV2, BRKVAL

       PARAMETER (BRKVAL = -987654.3)

       DOUBLE PRECISION A(MAXA), SIGMAA(MAXA)
       DOUBLE PRECISION AJ
       DOUBLE PRECISION X(MAXX), Y(MAXX)
       DOUBLE PRECISION YCALC(MAXN)
       DOUBLE PRECISION DERIVS(MAXN,MAXA)
       REAL WORK(2*MAXX)
       REAL X1, X2, Y1, Y2
       DOUBLE PRECISION WEIGHT(-20:20)
       DOUBLE PRECISION SIGMA, FWHM, TEMP
       DOUBLE PRECISION CHISQ, CHISQ1
       DOUBLE PRECISION X0(2)

       LOGICAL OK, CURSIN, CURSOR

!

       CHARACTER*(*) TITLE
       CHARACTER*80 TFILE
       CHARACTER*10 ANSER

!

       INTEGER W01, W02, W03, W04, W05, W06, W07, W08, W09, W10
       INTEGER K, L
       INTEGER L1, L2

       EQUIVALENCE (CMNWRK(1),TWAV(1))
       PARAMETER (W01= 1+MAXT)
       EQUIVALENCE (CMNWRK(W01),TFLX(1))
       PARAMETER (W02= W01+MAXT)
       EQUIVALENCE (CMNWRK(W02),A(1))
       PARAMETER (W03= W02+2*MAXA)
       EQUIVALENCE (CMNWRK(W03),SIGMAA(1))
       PARAMETER (W04= W03+2*MAXA)
       EQUIVALENCE (CMNWRK(W04),X(1))
       PARAMETER (W05= W04+2*MAXX)
       EQUIVALENCE (CMNWRK(W05),Y(1))
       PARAMETER (W06= W05+2*MAXX)
       EQUIVALENCE (CMNWRK(W06),YCALC(1))
       PARAMETER (W07= W06+2*MAXN)
       EQUIVALENCE (CMNWRK(W07),DERIVS(1,1))
       PARAMETER (W08= W07+2*MAXA*MAXN)
       EQUIVALENCE (CMNWRK(W08),WORK(1))


!

       OK = .FALSE.
       WRITE (*,'(''   DAC commands not available'',
     : '' in this version of DIPSO'')')

       END
