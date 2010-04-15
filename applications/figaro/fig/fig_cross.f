C+
      SUBROUTINE FIG_CROSS (ARRAY0,ARRAY1,NX0,NX,CFIT,ZPC,KZ,NORM,FT0,
     :                                   FT1,FTCFN,XV,CFN,SHIFT,WIDTH)
C
C     F I G _ C R O S S
C
C     Figaro utility routine.  Calculates the cross-correlation function
C     of two spectra and fits the central peak, returning the relative
C     shift between the two spectra and the width of the central peak.
C
C     Parameters -  (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) ARRAY0    (Real array ARRAY0(NX0)) Passed containing the first
C                   of the two spectra.  Used as workspace as the data
C                   has its continuum subtracted prior to being fourier
C                   transformed.
C     (!) ARRAY1    (Real array ARRAY1(NX0)) Passed containing the second
C                   of the two spectra.  Modified as for ARRAY0.
C     (>) NX0       (Integer) The number of elements in the two spectra.
C     (>) NX        (Integer) Either equal to NX or to the next highest
C                   number that is an integer power of 2.  Note that
C                   most of the work and output arrays must be NX long.
C     (>) CFIT      (Logical) If false, disables the continuum fit normally
C                   performed prior to application of the cosine bell.
C     (>) ZPC       (Real) The percentage of the spectrum covered at each
C                   end by a cosine bell prior to fourier transformation.
C     (>) KZ        (Integer array KZ(4)).  Defines the cosine bell
C                   used to filter the fourier transforms of the arrays.
C                   The filter used will be zero at KZ(1) and KZ(4), and
C                   will be 1. at KZ(2) and KZ(3).
C     (>) NORM      (Logical) True if the cross-correlation function
C                   is to be normalised - see CFN.
C     (W) FT0       (Complex array FT0(NX)) Used to hold the fourier
C                   transform of the first spectrum.
C     (W) FT1       (Complex array FT1(NX)) Used to hold the fourier
C                   transform of the second spectrum.
C     (W) FTCFN     (Complex array FTCFN(NX)) Used to hold the fourier
C                   transform of the correlation spectrum.
C     (W) XV        (Real array XV(NX)) Used to hold the pixel values
C                   for each spectrum for the fitting processes.
C     (<) CFN       (Real array CFN(NX)) The correlation function.
C                   If NORM is true this is returned scaled so that
C                   the cross-correlation peak will have a value of 1.
C                   (This is approximate - the exact range will vary
C                   slightly with the KZ values)
C     (<) SHIFT     (Real) The shift of the peak of the correlation
C                   function from the zero point.
C     (<) WIDTH     (Real) The width of the correlation function.
C
C     This routine is really nothing more than a wrap-up for Figaro
C     purposes of a number of routines written by John Tonry.  Any
C     misinformation in the comments arises from my failure to
C     understand all the subtleties of these routines.
C
C                                              KS / CIT 3rd Oct 1983
C     Modified:
C
C     6th May 1985.  KS / AAO.  CFIT parameter added.
C     21st Jan 1991. JMS / AAO. Test for zero spectra added - these could
C                    crashes in SUBCONT and other places.
C     18th Feb 1999. TDCA / RAL. Minor style changes.
C     26th May 1999. TDCA / RAL. Size of array now passed to JTY_FFT2C.
C     21st Feb 2001. ACD / UoE, Starlink. Changed the check for non-zero
C                    arrays to check against the smallest non-zero REAL
C                    number, rather than checking for equality with zero.
C+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'      ! Primitive data types; see SUN/39.
C
C     Parameters
C
      LOGICAL CFIT,NORM
      INTEGER NX0,NX,KZ(4)
      REAL ARRAY0(NX),ARRAY1(NX),ZPC,XV(NX),CFN(NX),SHIFT,WIDTH
      COMPLEX FT0(NX),FT1(NX),FTCFN(NX)

C
C     Functions used
C
      INTEGER NINT
      REAL JTY_RMSFILTER
C
C     Local variables
C
      LOGICAL ZERO_0,ZERO_1
      LOGICAL*1 ERROR
      INTEGER I,J,LOG2N,KZ1,KZ2,KZ3,KZ4
      REAL CENTER,RMS0,RMS1,SCALE
      DOUBLE PRECISION A(5)

c     print4000, 'hello'
c4000 format(1x, a)

      DO J = 1,5
         A(J)=0.0
      END DO
      DO J = 1,NX
           CFN(J)=0.0
      END DO
C
C     Make sure arrays are non-zero
C
      ZERO_0=.TRUE.
      ZERO_1=.TRUE.
      DO I = 1,NX0
         IF (ABS(ARRAY0(I)).GE.VAL__SMLR) ZERO_0=.FALSE.
         IF (ABS(ARRAY1(I)).GE.VAL__SMLR) ZERO_1=.FALSE.
      END DO
      IF (ZERO_0.OR.ZERO_1) THEN
        DO J = 1,NX
           CFN(J)=0
        END DO
        SHIFT=0
        WIDTH=0
        GOTO 500     ! Drop out of loop and subroutine
      END IF
C
C     Subtract the continua from the spectra
C
      IF (CFIT) THEN
         CALL JTY_SUBCONT(NX0,ARRAY0,XV)
         CALL JTY_SUBCONT(NX0,ARRAY1,XV)
      END IF
c     print4000, 'after subtracting continua'

C
C     Apodize the spectra (with a cosine bell) and zero fill to
C     an integer power of two elements.
C
c     print4001, nx0,nx
c4001 format(1x, 'nx0,nx: ', i5, i5)
      CALL JTY_APODIZE(NX0,1,NX0,ARRAY0,ZPC)
c     print4000, 'after first apodize'
      CALL JTY_APODIZE(NX0,1,NX0,ARRAY1,ZPC)
c     print4000, 'after second apodize'
      CALL JTY_RCVECTOR(NX0,ARRAY0,FT0)
c     print4000, 'after first rcvector'
      CALL JTY_RCVECTOR(NX0,ARRAY1,FT1)
c     print4000, 'after second rcvector'
c     print4000, 'after first apodization'

      DO I=NX0+1,NX
         FT0(I)=(0.,0.)
         FT1(I)=(0.,0.)
      END DO

C
C     Fourier transform both arrays, and get the RMS values between
C     the limits given by the KZ array values
C
      KZ1=KZ(1)
      KZ2=KZ(2)
      KZ3=KZ(3)
      KZ4=KZ(4)
      LOG2N=NINT(ALOG(FLOAT(NX))/ALOG(2.))

      CALL JTY_FFT2C(FT0,LOG2N,+1,NX)
      CALL JTY_FFT2C(FT1,LOG2N,+1,NX)
c     print4000, 'after fourier transforms'

      RMS0=JTY_RMSFILTER(NX,KZ1,KZ2,KZ3,KZ4,FT0)
      RMS1=JTY_RMSFILTER(NX,KZ1,KZ2,KZ3,KZ4,FT1)
c     print4000, 'after rms filtering'
C
C     Calculate the transform of the cross-correlation function
C
      CALL JTY_CORRELATE(NX,FT1,FT0,FTCFN)
c     print4000, 'after cross-correlation'
C
C     Filter it, and transform back
C
      CALL JTY_FILTER(NX,KZ1,KZ2,KZ3,KZ4,FTCFN)
      CALL JTY_FFT2C(FTCFN,LOG2N,-1,NX)
      CALL JTY_CRVECTOR(NX,FTCFN,CFN)
      CALL JTY_FLIP(NX,CFN)
c     print4000, 'after filtering and transforming back'
C
C     If required, Scale the cross-correlation to the approx
C     range 0 - 1
C
      IF (NORM) THEN
         SCALE=1./(RMS0*RMS1*FLOAT(NX))
         CALL GEN_MULCAF(CFN,NX,SCALE,CFN)
      END IF
c     print4000, 'after scaling'
C
C     Reset the XV array, and then use PEAKFIT to find the location
C     and width of the cross-correlation peak
C

      CALL GEN_NFILLF(NX,XV)
      CALL JTY_PEAKFIT(NX,XV,0.,FLOAT(NX),CFN,CENTER,WIDTH,A,ERROR)
      SHIFT=FLOAT(NX/2)-CENTER+1.
c     print4000, 'after fitting peak'
C
500   CONTINUE
C
      END








