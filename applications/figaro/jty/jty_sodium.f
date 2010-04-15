      SUBROUTINE JTY_SODIUM(NPIX,WAVEB,WAVER,V,TEMP,NAD)
* Program to make a "sodium correction spectrum" from a passed template.
* Given a continuum subtracted template spectrum, SODIUM will create
* another that has just NaD of a maximum amplitude of 1. The NaD will
* be shifted to a redshift of V km/s.
*
* The parameter statement had no brackets. The Sun compiler doesn't like
* that.
*                                HME/UoE, Starlink. 01-SEP-1992.
      REAL*4 NAD(NPIX), TEMP(NPIX)
      PARAMETER ( PI = 3.14159265 )
      PIXEL(W) = A * ALOG(W) + B
      WAVELENGTH(P) = EXP((P - B) / A)

      A = NPIX / ALOG(WAVER/WAVEB)
      B = -A * ALOG(WAVEB)
      I1 = PIXEL(5885.)
      I2 = PIXEL(5901.)
      NSQUASH = 20
      DO I = 1,I1-NSQUASH
          NAD(I) = 0
      END DO
      DO I = 1,NSQUASH
          ARG = PI * FLOAT(I-1)/(NSQUASH-1)
          FACTOR = .5 * (1 - COS(ARG))
          NAD(I+I1-NSQUASH) = FACTOR * TEMP(I+I1-NSQUASH)
          NAD(I2+NSQUASH-I+1) = FACTOR * TEMP(I2+NSQUASH-I+1)
      END DO
      DO I = I1,I2
          NAD(I) = TEMP(I)
      END DO
      DO I = I2+NSQUASH,NPIX
          NAD(I) = 0
      END DO
      DMIN = 0
      DO I = I1,I2
          DMIN = AMIN1(NAD(I),DMIN)
      END DO
      DO I = I1-NSQUASH,I2+NSQUASH
          NAD(I) = -NAD(I) / DMIN
      END DO
      IF(V.EQ.0) RETURN
      LOG2N = NINT(ALOG(FLOAT(NPIX))/ALOG(2.))
      SHIFT = A * ALOG(V/299793.+1)
      PHASE = 2*PI*SHIFT/NPIX
      CALL JTY_RCVECTOR(NPIX,NAD,NAD)
      CALL JTY_FFT2C(NAD,LOG2N,+1)
      CALL JTY_ADDPHASE(NPIX,NAD,PHASE)
      CALL JTY_FFT2C(NAD,LOG2N,-1)
      CALL JTY_CRVECTOR(NPIX,NAD,NAD)
      RETURN
      END
