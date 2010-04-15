C+
      SUBROUTINE EXTIN
C
C     E X T I N
C
C     Corrects a spectrum for extinction, given a coefficient spectrum
C     which gives the interpolated extinction coefficients over the
C     wavelength range of the spectrum.  The spectrum must have
C     a valid .OBS.SECZ value.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be corrected.
C     COEFF       (Character) The coefficient spectrum.
C     OUTPUT      (Character) The resulting spectrum.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                         KS / CIT 24th July 1984
C     Modified:
C
C     18th Feb 1985  KS / AAO.  Modified to work on 2D data.
C     11th July 1986 KS / AAO.  Data in the one operand case now
C                    mapped with MUVAR instead of MRVAR.  (In V1
C                    you could get away with this; in V2 you can't)
C     27th July 1987 DJA /AAO.  Revised DSA_ routines - some specs
C                    changed. Modified dynamic memory handling - now
C                    uses DYN_ package.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     31st Aug 1992  INCLUDE changed. TABs removed.  HME / UoE,
C                    Starlink.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      REAL      AIRM             ! The air mass
      INTEGER   CPTR             ! Dynamic-memory pointer to coefficient
                                 ! array
      INTEGER   CSLOT            ! Map slot number of input coefficient
                                 ! array
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      CHARACTER DUMMY*1          ! Dummy arguement for DATA_INFO routine
      LOGICAL   ISNEW            ! Is address new to CNF?
      INTEGER   IY               !
      LOGICAL   MAGS             !
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
                                 ! array
      INTEGER   NX               ! Size of 1st dimension
      INTEGER   NY               ! Size of 2nd dimension
      INTEGER   OPTR             ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER   OSLOT            ! Map slot number output data array
      LOGICAL   PISNEW           ! Previous CNF pointer new?
      INTEGER   STATUS           ! Running status for DSA_ routines
      INTEGER   TPTR             ! Temp dynamic-memory pointer
      DOUBLE PRECISION  VALUE    ! Magnitude flag
      LOGICAL   XEXIST           ! Used to check on x-axis compatiblity
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of spectrum and open it
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data array
C
      CALL DSA_DATA_SIZE('SPECT',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of COEFF array and open it
C
      CALL DSA_INPUT ('COEFF','COEFF',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get size of coefficient spectrum array
C
      CALL DSA_SEEK_AXIS('COEFF',1,XEXIST,STATUS)
      IF ((STATUS.NE.0).OR.(.NOT.XEXIST)) GO TO 500
C
C     Get air mass value - insist on this
C
      CALL DSA_GET_AIRMASS('SPECT','Mean',AIRM,STATUS)
C
C     See if data is calibrated in magnitudes
C
      CALL DSA_GET_DATA_INFO ('SPECT',0,DUMMY,1,VALUE,STATUS)
      IF (STATUS.EQ.0) MAGS=(VALUE.NE.0.)
C
C     Get name of OUTPUT file
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map output spectrum (note: FIG_EXTCOR will allow its
C     input and result arrays to be the same)
C
      CALL DSA_MAP_DATA ('COEFF','READ','FLOAT',CPTR,CSLOT,STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Generate the corrected spectrum (or spectra).  FIG_EXTCOR
C     processes one spectrum at at time.
C
      PISNEW = .FALSE.
      DO IY=1,NY
         CALL FIG_EXTCOR(NX,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(CPTR)),
     :                   AIRM,MAGS,%VAL(CNF_PVAL(OPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
         IF (ISNEW) CALL CNF_UNREGP(OPTR)
         OPTR=TPTR
         PISNEW = ISNEW
      END DO
      IF (ISNEW) CALL CNF_UNREGP(OPTR)

C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_EXTCOR(NX,SDATA,CDATA,AIRM,MAGS,RESULT)
C
C     F I G _ E X T C O R
C
C     Given a spectrum, and the airmass at which it was taken, and a
C     spectrum giving the extinction coefficients over the wavelength
C     range of the spectrum, corrects the spectrum for atmospheric
C     extinction.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) Number of elements in the spectra
C     (>) SDATA   (Real array SDATA(NX)) The spectrum to be corrected
C     (>) CDATA   (Real array CDATA(NX)) The extinction coefficients
C                 applicable to the spectrum
C     (>) AIRM    (Real) The star air-mass value
C     (>) MAGS    (Logical) True if the data is calibrated in magnitudes,
C                 false otherwise.
C     (<) RESULT  (Real array RESULT(NX)) The corrected spectrum
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C     Algorithm -
C
C     The result spectrum is calculated as
C     RESULT(i)=SDATA(i)*10**(0.4*CDATA(i)*AIRM)
C     if the data is not in magnitudes (ie if MAGS=.FALSE.), and as
C     RESULT(i)=SDATA(i)-CDATA(i)*AIRM
C     for magnitude data (ie if MAGS=.TRUE.)
C
C                                           KS / CIT 24th July 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL MAGS
      INTEGER NX
      REAL    SDATA(NX), CDATA(NX), AIRM, RESULT(NX)
C
C     Local variables
C
      INTEGER IX
C
C     Correct spectrum
C
      IF (MAGS) THEN
         DO IX=1,NX
            RESULT(IX)=SDATA(IX)-CDATA(IX)*AIRM
         END DO
      ELSE
         DO IX=1,NX
            RESULT(IX)=SDATA(IX)*10.0**(0.4*CDATA(IX)*AIRM)
         END DO
      END IF
C
      END
