!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
!      program atfit
!
!      performs a least-squares fit of (e.g. Kurucz) model
!      atmospheres to (e.g. IUE) data
!
!      data requirements:
!
!         a grid of model atmospheres (wavelength, flux per record)
!         corresponding to Teff = 8000,10000,12000,14000....40000K
!         in files named 10000.KRZ,12000.KRZ etc.
!
!         a dataset (wavelength, flux, weight per record)
!
!         the maximum size of the atmosphere arrays is MAXA, and
!         the wavelength range must encompass the dataset range
!
!         the maximum size of the dataset is 300
!
!      N.B.  this is a 'one-off' program, and does NOT have data checks!
!      =================================================================
!
!      The models are expected to be in the format output from DIPSO
!      (for Kurucz models);  i.e. astrophysical fluxes*pi
!
!      The data are expected to be in erg/cm2/s/A
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE ATFIT
     : (WAVE, FLUX, MAXBRK, BREAK, NBREAK,
     : NOBS, WORV, OK, VARS)
!   declarations
       INCLUDE 'DECLARE_STKS'
!
       INTEGER  MAXBRK
       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL VARS(3)
       REAL WORV

       LOGICAL OK
       INTEGER NBREAK, BREAK(MAXBRK)

       WRITE (*,
     : '(''   ATFIT:  unavailable in this version of DIPSO'')')
       OK = .FALSE.

       END
