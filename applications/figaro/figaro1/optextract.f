C+
      SUBROUTINE OPTEXTRACT
C
C     O P T E X T R A C T
C
C     OPTEXTRACT performs optimal extraction of a star spectrum from a
C     2D long slit spectrum using the algorithm of Horne 1986 (PASP 98,609).
C     A spatial profile image should have previously been determined using
C     the PROFILE command, and the 2D spectrum to be extracted must be
C     sky subtracted (e.g. using the POLYSKY command).
C
C     The input IMAGE array may have error and quality information. An error
C     or variance array is required to perform true optimal extraction as 
C     the points should be weighted using the errors on each point as well 
C     as the spatial profile information. If an error array is not present (or
C     not all the errors in a column are known) the extraction is performed
C     assuming equal errors on every point in the column. The output spectrum
C     has an error array only if the input image has an error array. The
C     WEIGHT keyword may be set false to suppress error weighting even
C     when errors are available.
C
C     The spatial profile array used by OPTEXTRACT must satisfy the following
C     requirements.
C      (i)  Its quality array must specify a window (of arbitrary shape)
C           in which the extraction will be performed. Points inside the window
C           must have zero quality values, other points must have non zero
C           quality values
C      (ii) The data must be normalized. i.e. The sum along each column
C           must be one.
C      (iii) The data values may not be negative. 
C
C  Command parameters -
C
C     'IMAGE'    The name of the input 2D file. This should be a 
C                  sky subtracted 2D spectrum from which the spectrum
C                  of an object is to be extracted. It may have error and
C                  quality arrays
C     'PROFILE'  The normalized spatial profile image used to perform the
C                  extraction. The quality array should specify a window
C                  in which the profile is determined. The data within
C                  this window is the spatial profile to be used.
C     'SPECTRUM' The name of the resulting 1D spectrum. If the input image
C                  has errors, so will the output spectrum. The quality
C                  will be set to bad only if there is no data in the
C                  relevant column.
C
C  Command keywords -
C    
C     'WEIGHT'    Use the error or variance array to weight the
C                 pixels during extraction.
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
C+
      IMPLICIT NONE
C
C     Functions
C

      INTEGER   DYN_ELEMENT
C
      INTEGER   ADDRESS            ! Virtual address for data array
      INTEGER   DIMS(2)            ! Image dimensions
      LOGICAL   EXIST              ! True if error information available
      INTEGER   IPTR               ! Dynamic memory element for image data
      INTEGER   IQPTR              ! Dynamic memory element for image quality
      INTEGER   IVPTR              ! Dynamic memory element for image variance
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   NX                 ! First dimension of image
      INTEGER   NY                 ! Second dimension of image
      INTEGER   PQPTR              ! Dynamic memory element for profile quality
      INTEGER   PPTR               ! Dynamic memory element for profile data
      INTEGER   QPTR               ! Dynamic memory element for spectrum quality
      INTEGER   SLOT               ! Slot number for mapped data - ignored
      INTEGER   SPTR               ! Dynamic memory element for spectrum data
      INTEGER   STATUS             ! Running status for DSA routines
      INTEGER   VPTR               ! Dynamic memory element for spectrum variance
      LOGICAL   WEIGHT             ! True if errors are to be used
C
C     Dynamic memory common - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
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
      ENDIF
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
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',ADDRESS,SLOT,STATUS)
      IPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',ADDRESS,SLOT,STATUS)
      IQPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',ADDRESS,SLOT,STATUS)
      SPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_QUALITY('SPECT','WRITE','BYTE',ADDRESS,SLOT,STATUS)
      QPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_DATA('PROFILE','READ','FLOAT',ADDRESS,SLOT,STATUS)
      PPTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_QUALITY('PROFILE','READ','BYTE',ADDRESS,SLOT,STATUS)
      PQPTR = DYN_ELEMENT(ADDRESS)
C
C     Only map the variance arrays if we need them - if not no
C     variance will be created for the output spectrum
C
      IF (WEIGHT) THEN      
          CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',ADDRESS,
     :          SLOT,STATUS)
          IVPTR=DYN_ELEMENT(ADDRESS)
          CALL DSA_MAP_VARIANCE('SPECT','WRITE','FLOAT',ADDRESS,
     :          SLOT,STATUS)
          VPTR=DYN_ELEMENT(ADDRESS)
      ENDIF
C
C     Perform the extraction
C
      IF (STATUS .EQ. 0) THEN
         CALL OPTEXTRACT_WORK(DYNAMIC_MEM(IPTR),NX,NY,
     :   DYNAMIC_MEM(IQPTR),DYNAMIC_MEM(IVPTR),DYNAMIC_MEM(PPTR),
     :   DYNAMIC_MEM(PQPTR),DYNAMIC_MEM(SPTR),DYNAMIC_MEM(VPTR),
     :   DYNAMIC_MEM(QPTR),WEIGHT)
      ENDIF
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
                      ENDIF
                  ENDIF
              ENDIF
          ENDDO
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
                  ENDIF
                  SUM=SUM+NUM
                  SUM2=SUM2+DENOM
              ENDIF
          ENDDO
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
              ENDIF
          ELSE
C
C      Set quality to bad if there is no data in the column
C
              SPECT(IX)=0.0
              QUALITY(IX)=1
          ENDIF
      ENDDO

      END
