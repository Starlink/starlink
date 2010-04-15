C+
      SUBROUTINE BSMULT
C
C     B S M U L T
C
C     Multiplies a spectrum by a B star calibration spectrum - which
C     should be 1.0 except in regions where there are atmospheric
C     bands.  This routine differs from an ordinary multiplication (eg
C     as in IMULT) since it includes a term for the correction due
C     to the different air masses of the two objects.  This means
C     that both the original spectrum and the B star spectrum must
C     have valid .OBS.SECZ values.  If the spectrum to be multiplied
C     is 2 dimensional, the same operation is repeated for each of
C     the spectra it contains.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be corrected.
C     BSTAR       (Character) The B star calibration spectrum.
C     BETA        (Numeric) Power to which relative air mass is
C                 to be raised - see FIG_BSMULT.
C     OUTPUT      (Character) The resulting spectrum.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                         KS / CIT 3rd July 1984
C     Modified:
C
C     6th May 1986   KS / AAO.  Now will accept an image for SPECT,
C                    although it still expects BSTAR to be a single
C                    spectrum.
C     24th Aug 1987  DJA/ AAO.  Now uses DSA_ routines - some specs
C                    changed. Now uses DYN_ package for dynamic-memory
C                    handling.
C     19th Dec 1990  JMS / AAO. Now works for 2D B Star Data.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     5th  Oct 1992  HME / UoE, Starlink. Changed INCLUDE. TABs removed.
C     1st  May 1997  JJL / Soton, Starlink. Propogates the variances
C     29th Jul 1997  MJCL / Starlink, UCL.  Initialised VEXIST to
C                    .FALSE.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      REAL      BAIRM            ! The air mass for the B star
      INTEGER   BDIM             ! Number of dimensions for the B-star
                                 ! data
      REAL      BETA             ! See above
      INTEGER   BPTR             ! Dynamic-memory pointer to B-star data
      INTEGER   BSLOT            ! Map slot number of input B star data
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      INTEGER   I                ! Counter for each spectrum in image
      LOGICAL   ISNEVB           ! Is VBPTR address new to CNF?
      LOGICAL   ISNEW            ! Is OPTR address new to CNF?
      LOGICAL   ISNEWB           ! Is BPTR address new to CNF?
      LOGICAL   ISNEWV           ! Is VOPTR address new to CNF?
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
      INTEGER   NX               ! Size of 1st dimension
      INTEGER   NY               ! Size of 2nd dimension (if present)
      INTEGER   OPTR             ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER   OSLOT            ! Map slot number for output data array
      LOGICAL   PISNB            ! Previous CNF pointer to B-star data
                                 ! new?
      LOGICAL   PISNEW           ! Previous CNF pointer to output data
                                 ! new?
      LOGICAL   PISNVB           ! Previous CNF pointer to B-star errors
                                 ! new?
      LOGICAL   PISNVO           ! Previous CNF pointer to output errors
                                 ! new?
      INTEGER   SAIRM            ! The air mass for the input spectrum
      INTEGER   STATUS           ! Running status for DSA_ routines
      INTEGER   TPTR             ! Temp dynamic-memory pointer
      INTEGER   VBPTR            ! Dyn-memory pointer to B-star
                                 ! variances
      LOGICAL   VEXIST           ! Variances exist in both spectra?
      INTEGER   VOPTR            ! Dyn-memory pointer to output
                                 ! variance array
      INTEGER   VBSLOT           ! Map slot number of input B-star
                                 ! variance
      INTEGER   VOSLOT           ! Map slot number for output variance
                                 ! array
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of spectrum and open it
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get size of data array
C
      CALL DSA_DATA_SIZE('SPECT',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
C
C     Get name of BSTAR array and open it
C
      CALL DSA_INPUT('BST','BSTAR',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Make sure B star array is the same length as the spectrum
C
      CALL DSA_DATA_SIZE('BST',2,BDIM,DIMS,NELM,STATUS)
      CALL DSA_MATCH_DIMENSION('SPECT',1,'BST',1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get air mass values - insist on these; they're easy enough to
C     put in by hand if necessary.
C
      CALL DSA_GET_AIRMASS('SPECT','Mean',SAIRM,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_GET_AIRMASS('BST','Mean',BAIRM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get name of OUTPUT file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get value of BETA
C
      CALL PAR_RDVAL('BETA',0.,10.,0.55,' ',BETA)
C
C     Map input and output spectra (note: FIG_BSMULT will allow its
C     input and result arrays to be the same). Also test to see if
C     a variance array exists.
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_MAP_DATA('BST','READ','FLOAT',BPTR,BSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE('SPECT',VEXIST,STATUS)
      IF (VEXIST) CALL DSA_SEEK_VARIANCE('BST',VEXIST,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     If the variance structure exists, map it.
C
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VOPTR,
     :                          VOSLOT,STATUS)
         CALL DSA_MAP_VARIANCE('BST','READ','FLOAT',VBPTR,VBSLOT,
     :                          STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Generate the corrected spectrum
C
      PISNEW = .FALSE.
      PISNB = .FALSE.
      PISNVO = .FALSE.
      PISNVB = .FALSE.
      DO I=1,NY
         CALL FIG_BSMULT(NX,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VOPTR)),
     :                      %VAL(CNF_PVAL(BPTR)),%VAL(CNF_PVAL(VBPTR)),
     :                      SAIRM,BAIRM,BETA,VEXIST,
     :                      %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VOPTR)))

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEW,STATUS)
         IF (PISNEW) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNEW = ISNEW

         IF (VEXIST) THEN
            CALL DYN_INCAD(VOPTR,'FLOAT',NX,TPTR,ISNEWV,STATUS)
            IF (ISNEWV) CALL CNF_UNREGP(VOPTR)
            VOPTR = TPTR
            PISNVO = ISNEWV
         END IF
         IF (BDIM.EQ.2) THEN
            CALL DYN_INCAD(BPTR,'FLOAT',NX,TPTR,ISNEWB,STATUS)
            IF (ISNEWB) CALL CNF_UNREGP(BPTR)
            BPTR = TPTR
            PISNB = ISNEWB

            IF (VEXIST) THEN
               CALL DYN_INCAD(VBPTR,'FLOAT',NX,TPTR,ISNEVB,STATUS)
               IF (ISNEVB) CALL CNF_UNREGP(VBPTR)
               VBPTR = TPTR
               PISNVB = ISNEVB
            END IF
         END IF
      END DO

      IF (ISNEW) CALL CNF_UNREGP(OPTR)
      IF (ISNEWV) CALL CNF_UNREGP(VOPTR)
      IF (ISNEWB) CALL CNF_UNREGP(BPTR)
      IF (ISNEVB)  CALL CNF_UNREGP(VBPTR)
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_BSMULT(NX,SDATA,SVAR,BDATA,BVAR,SSECZ,BSECZ,
     :                      BETA,VEXIST,RESULT,VRESULT)
C
C     F I G _ B S M U L T
C
C     Performs the actual multiplication of a spectrum by a B star
C     calibration spectrum, allowing for the difference in air mass.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX      (Integer) Number of elements in the spectra
C     (>) SDATA   (Real array SDATA(NX)) The spectrum to be corrected
C     (>) SVAR    (Real array SVAR(NX)) Variances of the spec. to be corrected
C     (>) BDATA   (Real array BDATA(NX)) The B star calibration spectrum
C     (>) BVAR    (Real array BVAR(NX)) The B star variances
C     (>) SSECZ   (Real) The star air-mass value
C     (>) BSECZ   (Real) The B star air-mass value
C     (>) BETA    (Real) Power to which air mass ratio is to be raised.
C     (>) VEXIST  (Logical) TRUE if variances exist
C     (<) RESULT  (Real array RESULT(NX)) The corrected spectrum
C     (<) VRESULT (Real array VRESULT(NX)) The corrected spectrum variances
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C     Algorithm -
C
C     The result spectrum is calculated as
C     RESULT(i)=SDATA(i)*(BDATA(i)**(SSECZ/BSECZ)**BETA)
C     where BETA should be 0.5 for saturated lines, 1.0 for
C     unsaturated lines, and the 'best' value for all lines, determined
C     by P.J.Young, is 0.55
C                                           KS / CIT 10th April 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    SDATA(NX), BDATA(NX), SSECZ, BSECZ, BETA, RESULT(NX),
     :        SVAR(NX), BVAR(NX), VRESULT(NX)
      LOGICAL VEXIST
C
C     Local variables
C
      INTEGER IX
      REAL    SFACT, temp
C
C     Calculate scaling factor based on air mass
C
      IF (BSECZ.GT.0.) THEN
         SFACT=(SSECZ/BSECZ)**BETA
      ELSE
         SFACT=0.
      END IF
C
C     Correct spectrum
C
      DO IX=1,NX
         RESULT(IX)=SDATA(IX)*BDATA(IX)**SFACT
      END DO
C
C     Propagate the variances
C
      IF (VEXIST) THEN
         DO IX=1,NX
         TEMP = SVAR(IX) * (BVAR(IX)**(2*((SSECZ/BSECZ)**BETA)))
         TEMP = TEMP + ((((SSECZ/BSECZ)**BETA)**2.) * BVAR(IX)*
     :                 (BDATA(IX)**(((SSECZ/BSECZ)**BETA)-1. )))
         VRESULT(IX)=TEMP
         END DO
      END IF
C
      END
