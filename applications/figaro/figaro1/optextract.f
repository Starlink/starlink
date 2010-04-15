C+
      SUBROUTINE OPTEXTRACT
C
C     O P T E X T R A C T
C
C     OPTEXTRACT performs optimal extraction of a star spectrum from a
C     2D long slit spectrum using the algorithm of Horne 1986
C     (PASP 98,609). A spatial profile image should have previously been
C     determined using the PROFILE command, and the 2D spectrum to be
C     extracted must be sky subtracted (e.g. using the POLYSKY command).
C
C     The input IMAGE array may have error and quality information. An
C     error or variance array is required to perform true optimal
C     extraction as the points should be weighted using the errors on
C     each point as well as the spatial profile information. If an error
C     array is not present (or not all the errors in a column are known)
C     the extraction is performed assuming equal errors on every point
C     in the column. The output spectrum has an error array only if the
C     input image has an error array. The WEIGHT keyword may be set
C     false to suppress error weighting even when errors are available.
C
C     The spatial profile array used by OPTEXTRACT must satisfy the
C     following requirements.
C      (i)  Its quality array must specify a window (of arbitrary shape)
C           in which the extraction will be performed. Points inside the
C           window must have zero-quality values, other points must
C           have non-zero-quality values.
C      (ii) The data must be normalized. i.e. the sum along each column
C           must be one.
C      (iii) The data values may not be negative.
C
C  Command parameters -
C
C     'IMAGE'    The name of the input 2D file. This should be a
C                sky subtracted 2D spectrum from which the spectrum
C                of an object is to be extracted. It may have error
C                and quality arrays
C     'PROFILE'  The normalized spatial profile image used to perform
C                the extraction. The quality array should specify a
C                window in which the profile is determined. The data
C                within this window is the spatial profile to be used.
C     'SPECTRUM' The name of the resulting 1D spectrum. If the input
C                image has errors, so will the output spectrum. The
C                quality will be set to bad only if there is no data in
C                the relevant column.
C
C  Command keywords -
C
C     'WEIGHT'   Use the error or variance array to weight the
C                pixels during extraction.
C
C
C                                     JAB / JAC 8th Feb 1991
C
C  Modified:
C     08 Mar 1992  JAB / JAC .  Use Variance instead of error, add
C                  WEIGHT keyword.
C     22 Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Quality is unsigned byte.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
      INTEGER   DIMS(2)          ! Image dimensions
      LOGICAL   EXIST            ! True if error information available
      INTEGER   IPTR             ! Dynamic-memory pointer for image data
      INTEGER   IQPTR            ! Dynamic-memory pointer for image
                                 ! quality
      INTEGER   IVPTR            ! Dynamic-memory pointer for image
                                 ! variance
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   PQPTR            ! Dynamic-memory pointer for profile
                                 ! quality
      INTEGER   PPTR             ! Dynamic-memory pointer for profile
                                 ! data
      INTEGER   QPTR             ! Dynamic-memory pointer for spectrum
                                 ! quality
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   SPTR             ! Dynamic-memory element for spectrum
                                 ! data
      INTEGER   STATUS           ! Running status for DSA routines
      INTEGER   VPTR             ! Dynamic-memory pointer for spectrum
                                 ! variance
      LOGICAL   WEIGHT           ! True if errors are to be used
C
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_USE_QUALITY('IMAGE',STATUS)
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (NDIM.EQ.1) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
C
C     Open spatial profile file
C
      CALL DSA_INPUT('PROFILE','PROFILE',STATUS)
      CALL DSA_MATCH_SIZES('IMAGE','PROFILE',STATUS)
      IF (STATUS .NE. 0) GOTO 500
      CALL DSA_USE_QUALITY('PROFILE',STATUS)
C
C     Is there error information?
C
      CALL DSA_SEEK_ERRORS('IMAGE',EXIST,STATUS)
C
C     If there is error information, should it be used to weight the data?
C
      IF (EXIST) THEN
         CALL PAR_RDKEY('WEIGHT',.TRUE.,WEIGHT)
      ELSE
         WEIGHT = .FALSE.
      END IF
C
C     Create new spectrum file.
C
      CALL DSA_OUTPUT ('SPECT','SPECTRUM','IMAGE',1,1,STATUS)
C
C     Create the new data and axis arrays in the spectrum file.
C
      CALL DSA_RESHAPE_DATA('SPECT','IMAGE',1,NX,STATUS)
      CALL DSA_RESHAPE_AXIS('SPECT',1,'IMAGE',1,1,NX,STATUS)
      CALL DSA_USE_QUALITY('SPECT',STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',IQPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      CALL DSA_MAP_QUALITY('SPECT','WRITE','BYTE',QPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA('PROFILE','READ','FLOAT',PPTR,SLOT,STATUS)
      CALL DSA_MAP_QUALITY('PROFILE','READ','BYTE',PQPTR,SLOT,STATUS)
C
C     Only map the variance arrays if we need them - if not no
C     variance will be created for the output spectrum
C
      IF (WEIGHT) THEN
          CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',IVPTR,
     :                          SLOT,STATUS)
          CALL DSA_MAP_VARIANCE('SPECT','WRITE','FLOAT',VPTR,
     :                          SLOT,STATUS)
      END IF
C
C     Perform the extraction
C
      IF (STATUS .EQ. 0) THEN
         CALL OPTEXTRACT_WORK(%VAL(CNF_PVAL(IPTR)),NX,NY,
     :                        %VAL(CNF_PVAL(IQPTR)),
     :                        %VAL(CNF_PVAL(IVPTR)),
     :                        %VAL(CNF_PVAL(PPTR)),
     :                        %VAL(CNF_PVAL(PQPTR)),
     :                        %VAL(CNF_PVAL(SPTR)),%VAL(CNF_PVAL(VPTR)),
     :                        %VAL(CNF_PVAL(QPTR)),WEIGHT)
      END IF
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE OPTEXTRACT_WORK(IMAGE,NX,NY,
     :     IQUAL,IVAR,PROF,QPROF,SPECT,VARIANCE,QUALITY,WEIGHT)
C
C     OPptimal extraction of a spectrum
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IMAGE   (Real array IMAGE(NX,NY)) The image array
C     (>) NX      (Integer) The first dimension of IMAGE
C     (>) NY      (Integer) The second dimension of IMAGE
C     (>) IQUAL   (Byte array IQUAL(NX,NY) Input quality array
C     (>) IVAR    (Real array IVAR(NX,NY) Input variance array
C     (>) PROF    (Real array PROF(NX,NY) Spatial profile array
C     (>) QPROF   (Byte array QPROF(NX,NY) Quality of profile array
C     (<) SPECT   (Real array SPECT(NX)) The resulting spectrum.
C     (<) VARIANCE(Real array VARIANCE(NX)) The variances on the spectrum.
C     (<) QUALITY (Byte array QUALITY(NX)) The output quality array.
C     (>) WEIGHT  (Logical)   TRUE if the error array is to be used
C                                to weight the data.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C
C                                            JAB / JAC 8th Feb 1991
C     Modified:
C       1991 Feb 10th   Use Quality array of profile to specify window
C                                                          JAB / JAC
C       1991 Feb 10th   Include error calculation   JAB / JAC
C       1991 Mar 8th    Use variance rather than error.
C                        Add WEIGHT parameter       JAB / JAC
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY
      BYTE    QUALITY(NX), IQUAL(NX,NY), QPROF(NX,NY)
      REAL    SPECT(NX),VARIANCE(NX),IMAGE(NX,NY),IVAR(NX,NY)
      REAL    PROF(NX,NY)
      LOGICAL WEIGHT
C
C     Local variables
C
      INTEGER IY,IX
      REAL SUM,SUM2,NUM,DENOM
      LOGICAL ERRUSE
      INTEGER STATUS
      REAL FUNKNOWN_IN,FUNKNOWN_OUT
C
C     Set up values for unknown variance - This is to support a possible
C     future upgrade to DSA to allow flagged values in error and variance
C     arrays. When such a DSA version is available FUNKNOWN_OUT should be
C     set equal to the flag value
C
      STATUS = 0
      CALL DSA_GET_FLAG_VALUE('FLOAT',FUNKNOWN_IN,STATUS)
      FUNKNOWN_OUT = 0
C
C     Create the spectrum array. This is an implementation of equations
C      8 and 9 of Horne(1986).
C
      DO IX=1,NX
          ERRUSE = WEIGHT
          DO IY=1,NY
              IF ((IQUAL(IX,IY).EQ.0) .AND. (QPROF(IX,IY).EQ.0)) THEN
                  IF (ERRUSE) THEN
                      IF (IVAR(IX,IY) .LE. 0 .OR.
     :                      IVAR(IX,IY) .EQ. FUNKNOWN_IN) THEN
                          ERRUSE = .FALSE.
                      END IF
                  END IF
              END IF
          END DO
          SUM=0.0
          SUM2=0.0
          DO IY=1,NY
C
C     Only use points for which the quality is good in both the image and
C     profile structures
C
              IF ((IQUAL(IX,IY).EQ.0) .AND. (QPROF(IX,IY).EQ.0)) THEN
C
C      Sum contributions to weighted spectrum value
C
                  NUM=PROF(IX,IY)*IMAGE(IX,IY)
                  DENOM=PROF(IX,IY)*PROF(IX,IY)
                  IF (ERRUSE) THEN
C
C      Weight according to variance if available
C
                      NUM=NUM/IVAR(IX,IY)
                      DENOM=DENOM/IVAR(IX,IY)
                  END IF
                  SUM=SUM+NUM
                  SUM2=SUM2+DENOM
              END IF
          END DO
          IF (SUM2 .GT. 0.0) THEN
C
C      Form the output spectrum estimate ...
C
              SPECT(IX)=SUM/SUM2
              QUALITY(IX)=0
              IF (ERRUSE) THEN
C
C      ... and the error if possible
C
                  VARIANCE(IX) = 1.0/SUM2
              ELSE IF (WEIGHT .AND. (.NOT. ERRUSE)) THEN
                  VARIANCE(IX) = FUNKNOWN_OUT
              END IF
          ELSE
C
C      Set quality to bad if there is no data in the column
C
              SPECT(IX)=0.0
              QUALITY(IX)=1
          END IF
      END DO

      END
