C+
      SUBROUTINE FIGSEE
C
C     F I G S E E
C
C     Figaro function that attempts to produce a seeing ripple spectrum
C     from a Figs spectrum, averaging the data from one or more
C     detectors, normalising the result to unity, and generating a
C     spectrum in which the normalised data from these detectors
C     (ideally ones not contaminated by spectral features) are repeated
C     for each detector.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The name of the file containing the
C                 spectrum to be used.
C     NDET        (Integer) The number of detectros to be used.
C     DETECTORS   (Numeric array) The detectors to be used.
C     OUTPUT      (Character) The name of the resulting ripple spectrum.
C
C     Command keywords -  None
C
C                                                KS / AAO 11th Feb 1987
C     Modified:
C
C     5th  Aug 1987  Revised DSA_ routines - some specs changed. Now
C                    uses DYN_ routines for dynamic memory handling
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. TABs
C                    removed.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      INTEGER ICH_ENCODE
C
C     Number of FIGS detectors
C
      INTEGER DETECTORS
      PARAMETER (DETECTORS=16)
C
C     Local variables
C
      INTEGER  DIMS(10)          ! Sizes of dimensions of data
      LOGICAL  DUSED(DETECTORS)  !
      REAL     DVALS(DETECTORS)  !
      INTEGER  I                 !
      INTEGER  IGNORE            ! Used to pass ignorable status
      INTEGER  INVOKE            ! Used to invoke functions
      INTEGER  J                 !
      INTEGER  NDIM              ! Number of dimensions in data
      INTEGER  NDET              ! The number of detectors to be used
      INTEGER  NEXT              !
      INTEGER  NX                ! Size of 1st dimension
      INTEGER  OPTR              ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER  OSLOT             ! Map slot number output data array
      INTEGER  STATUS            ! Running status for DSA_ routines
      CHARACTER*80 STRING        ! Output string
      LOGICAL  UFLAG             !
      REAL     VALUE             ! Temporary real number
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of Spectrum to be used and open it
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data array, make sure it might be a FIGS spectrum
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (MOD(NX,DETECTORS).NE.0) THEN
         CALL PAR_WRUSER('Spectrum length is not a multiple of the '//
     :                   'number of detectors.',IGNORE)
         GO TO 500
      END IF
C
C     Get detectors to be used.
C
      CALL PAR_RDVAL('NDET',1.0,FLOAT(DETECTORS),1.0,' ',VALUE)
      NDET=NINT(VALUE)
      DO I=1,DETECTORS
         DVALS(I)=0.0
      END DO
      CALL PAR_RDARY('DETECTORS',0.0,FLOAT(DETECTORS),'N',' ',NDET,
     :               DETECTORS,DVALS)
C
C     List the detectors used
C
      CALL PAR_WRUSER(' ',STATUS)
      STRING='Detectors used: '
      NEXT=16
      UFLAG=.FALSE.
      DO I=1,DETECTORS
         DUSED(I)=.FALSE.
         DO J=1,NDET
            IF (DVALS(J).EQ.FLOAT(I)) THEN
               DUSED(I)=.TRUE.
               GO TO 340
            END IF
         END DO
  340    CONTINUE
         IF (DUSED(I)) THEN
            IF (UFLAG) STRING(NEXT:NEXT)=','
            INVOKE=ICH_ENCODE(STRING,FLOAT(I),NEXT+1,0,NEXT)
            UFLAG=.TRUE.
         END IF
      END DO
      IF (UFLAG) THEN
         CALL PAR_WRUSER(STRING,STATUS)
      ELSE
         CALL PAR_WRUSER('No detectors specified',STATUS)
         GO TO 500
      END IF
      CALL PAR_WRUSER(' ',STATUS)
C
C     Get name of resulting output spectrum
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',optr,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on data
C
      CALL FIG_FIGSEE(%VAL(CNF_PVAL(OPTR)),NX,DUSED,DETECTORS)
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_FIGSEE(DATA,NX,DUSED,DETECTORS)
C
C     F I G _ F I G S E E
C
C     This routine does the real work for the Figaro application FIGSEE,
C     producing a seeing ripple spectrum from a Figs spectrum by averaging
C     the signal in each time period from the specified detectors and then
C     using the normalised result for all points in the spectrum that
C     correspond to that time period.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (!) DATA      (Real array DATA(NX)) The data in question.
C                   (>) The original spectrum.
C                   (<) The seeing ripple spectrum.
C     (>) NX        (Integer) The number of elements in the spectrum.  Note
C                   this routine assumes that NX is a multiple of DETECTORS.
C     (>) DUSED     (Logical array, DUSED(DETECTORS)) If an element of DUSED
C                   is set, the corresponding detector is to be used to
C                   generate the seeing ripple spectrum.
C     (>) DETECTORS (Integer) The number of detectors.
C
C     Common variables used - None
C
C     Functions subroutines used - None
C
C                                               KS / AAO 10th Feb 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, DETECTORS
      LOGICAL DUSED(DETECTORS)
      REAL    DATA(NX)
C
C     Local variables
C
      INTEGER IDET, IPIX, ITIM, NDET, NT, PIXEL
      REAL    SUM, TOTAL, VALUE
C
C     First, sum all the data from the specified detectors.  Also,
C     see how many detectors are being used.
C
      NT=NX/DETECTORS
      NDET=0
      TOTAL=0.0
      PIXEL=1
      DO IDET=1,DETECTORS
         IF (DUSED(IDET)) THEN
            NDET=NDET+1
            DO IPIX=1,NT
               TOTAL=TOTAL+DATA(PIXEL)
               PIXEL=PIXEL+1
            END DO
         ELSE
            PIXEL=PIXEL+NT
         END IF
      END DO
C
C     Now, for each time period, first average the data for the
C     specified detectors, then replace the spectral data with the
C     normalised average value.
C
      DO ITIM=1,NT
         PIXEL=ITIM
         SUM=0.0
         DO IDET=1,DETECTORS
            IF (DUSED(IDET)) SUM=SUM+DATA(PIXEL)
            PIXEL=PIXEL+NT
         END DO
         VALUE=SUM*FLOAT(NT)/TOTAL
         PIXEL=ITIM
         DO IDET=1,DETECTORS
            DATA(PIXEL)=VALUE
            PIXEL=PIXEL+NT
         END DO
      END DO
C
      END
