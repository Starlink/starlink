C+
      SUBROUTINE COADD
C
C     C O A D D
C
C     Form a spectrum which is the mean of all the rows in an image
C     or form an image which is the mean of all the planes in a cube.
C     The errors on the result are the standard errors of the mean
C     (i.e. SIGMA/SQRT(N) when N rows or planes are combined). Any error
C     information in the original image or cube is ignored.
C
C     An XY image is collapsed along the Y direction to give a spectrum,
C     and an XYT cube is collapsed along the T direction to give an XY
C     image.
C
C     Typical uses include the combination of the various cycles of a
C     CGS2 or FIGS observation as output by the FIGS322 or RCGS2
C     programs, or coadding of CGS4 observations (for this purpose the
C     individual images must be first grown into a cube using GROWXY).
C
C     If the NORM keyword is specified the errors are calculated after
C     normalizing each row or plane so that the mean value is the same
C     for all rows (planes). This does not effect the output data but
C     generates errors which are determined only by the noise level in
C     the data and are not influenced by any general trend in the data.
C
C     If the CUTOFF parameter is specified, points which deviate from
C     the mean by more than CUTOFF times the standard error for the
C     mean are excluded from the calculation. The mean is recalculated
C     until no points exceed the CUTOFF limit. This procedure allows
C     spikes in the data to be removed.
C
C     Command parameters -
C
C     'IMAGE'    The name of the input 2D or 3D file.
C     'YSTART'   (or TSTART) The first Y or T value to use.
C     'YEND'     (or TEND) The last Y or T value to use.
C     'SPECTRUM' The name of the resulting spectrum or image.
C     'CUTOFF'   The level (in sigma) at which a point will
C                be ignored.
C
C     Command keywords -
C
C     'NORM'     Normalize data to mean level.
C
C     Input data -
C
C                                     JAB / JAC 7th Dec 1990
C
C     Modified:
C     1st  Mar 1991  Add 3-2 dimensional coadding.
C     2nd  Apr 1991  JMS/AAO. Added STATUS checks to support user
C                    requested aborts. Now aborts if Input Data is
C                    not 2 or 3 dimensional.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. TABs
C                    removed.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
      INTEGER   DIMS(3)            ! Image dimensions
      INTEGER   IPTR               ! Dynamic memory element for image data
      INTEGER   IYEN               ! Last image row to extract
      INTEGER   IYST               ! First image row to extract
      INTEGER   NDIM               ! Number of image dimensions
      INTEGER   NELM               ! Number of elements in image - ignored
      INTEGER   NX                 ! First dimension of image
      INTEGER   NY                 ! Second dimension of image
      INTEGER   SLOT               ! Slot number for mapped data - ignored
      INTEGER   SPTR               ! Dynamic memory element for spectrum data
      INTEGER   EPTR               ! Dynamic memory element for spectrum errors
      INTEGER   EXPTR              ! Pointer to EXCLUDE workspace array
      INTEGER   NPTR               ! Pointer to NUM_CYCLES workspace array
      INTEGER   IQPTR              ! Input quality pointer
      INTEGER   QPTR               ! Output quality pointer
      INTEGER   STATUS             ! Running status for DSA routines
      LOGICAL   NORM               ! Normalize data flag
      REAL      CUTOFF             ! Cutoff value for despiking
      REAL      YEND               ! Last Y value used - ignored
      REAL      YSTART             ! First Y value used - ignored
C
C     Initial values
C
      STATUS=0
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Open IMAGE file
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_USE_QUALITY('IMAGE',STATUS)
      IF (STATUS .NE. 0) GOTO 500
C
C     Get size of data in IMAGE - Note that we handle the 3 dimensional
C     case by treating it as a 2D image of size DIMS(1)*DIMS(2) by DIMS(3)
C
      CALL DSA_DATA_SIZE ('IMAGE',3,NDIM,DIMS,NELM,STATUS)
      IF (NDIM .EQ. 2) THEN
         NX=DIMS(1)
         NY=DIMS(2)
      ELSE IF (NDIM .EQ. 3) THEN
         NX=DIMS(1)*DIMS(2)
         NY=DIMS(3)
      ELSE
         CALL PAR_WRUSER('Input Data must be 2 or 3 dimensional',STATUS)
         GOTO 500
      END IF
C
C     Get range of Y or T values.
C
      CALL DSA_AXIS_RANGE ('IMAGE',NDIM,' ',.FALSE.,YSTART,YEND,
     :                                             IYST,IYEN,STATUS)
C
C     Get normalize keyword
C
      CALL PAR_RDKEY('NORM',.FALSE.,NORM)
C
C     Get cutoff level
C
      CALL PAR_SDVAL('CUTOFF',1E38,STATUS)
      CALL PAR_RDVAL('CUTOFF',0,1.1E38,1E38,'SIGMA',CUTOFF)
C
C     Create new spectrum file.
C
      CALL DSA_OUTPUT ('SPECT','SPECTRUM','IMAGE',1,1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Create the new data, and axis arrays in the spectrum file.
C     If the input data is 3-dimensional we have to create the second
C     axis array as well as the X-axis
C
      CALL DSA_RESHAPE_DATA('SPECT','IMAGE',NDIM-1,DIMS,STATUS)
      CALL DSA_RESHAPE_AXIS('SPECT',1,'IMAGE',1,1,DIMS(1),STATUS)
      IF (NDIM .EQ. 3) THEN
         CALL DSA_RESHAPE_AXIS('SPECT',2,'IMAGE',2,1,DIMS(2),STATUS)
      END IF
      CALL DSA_USE_QUALITY('SPECT',STATUS)
C
C     Map the input and output data
C
      CALL DSA_MAP_DATA ('SPECT','WRITE','FLOAT',SPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_QUALITY('IMAGE','READ','BYTE',IQPTR,SLOT,STATUS)
      CALL DSA_MAP_QUALITY('SPECT','WRITE','BYTE',QPTR,SLOT,STATUS)
C
C     Only map the output error array (which will cause it to be created)
C     if there is more than one cycle
C
      IF (IYEN .NE. IYST) THEN
         CALL DSA_MAP_ERRORS('SPECT','WRITE','FLOAT',EPTR,SLOT,
     :                       STATUS)
      END IF
C
C     Get workspace arays
C
      CALL DSA_GET_WORK_ARRAY(NX*NY,'BYTE',EXPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'INT',NPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Perform the coadding
C
      CALL FIG_COADD(%VAL(CNF_PVAL(IPTR)),NX,NY,IYST,IYEN,NORM,CUTOFF,
     :               %VAL(CNF_PVAL(IQPTR)),%VAL(CNF_PVAL(EXPTR)),
     :               %VAL(CNF_PVAL(NPTR)),%VAL(CNF_PVAL(SPTR)),
     :               %VAL(CNF_PVAL(EPTR)),%VAL(CNF_PVAL(QPTR)))
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE FIG_COADD(IMAGE,NX,NY,IYST,IYEN,NORM,CUTOFF,
     :     IQUAL,EXCLUDE,NUM_CYCLES,SPECT,ERRORS,QUALITY)
C
C     F I G _ C O A D D
C
C     Collapses an image down to a single spectrum.
C
C     The rows of the image are averaged to form a spectrum with
C     statistical errors.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IMAGE   (Real array IMAGE(NX,NY)) The image array
C     (>) NX      (Integer) The first dimension of IMAGE
C     (>) NY      (Integer) The second dimension of IMAGE
C     (>) IYST    (Integer) The first Y value to use
C     (>) IYEN    (Integer) The last Y value to use
C     (>) NORM    (Logical) True if normalizing of cycles required
C     (>) CUTOFF  (Real) Cutoff value for despiking (sigma)
C     (>) IQUAL   (Byte array (NX,NY) Input quality array
C     (>) EXCLUDE (Byte array (NX,NY) Workspace array
C                 used to indicate points to be excluded.
C     (>) NUM_CYCLES (Integer array NUM_CYCLES(NX))  workspace array
C                 holding number of cycles actually added in to each
C                 spectral point.
C     (<) SPECT   (Real array SPECT(NX)) The resulting spectrum.
C     (<) ERRORS  (Real array ERRORS(NX)) The errors on the spectrum.
C     (<) QUALITY (Byte array QUALITY(NX)) The output quality array.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     GEN_FILL    (GEN_ package) Fill an array of bytes with a constant.
C
C                                            KS / AAO 21st May 1985
C     Modified:
C
C     12th Aug 1985.  KS / AAO.  Errors now calculated as percentages.
C     9th Apr 1986   JAB / AAO.  CUTOFF added
C     22nd July 1986  KS / AAO.  Revert to calculating errors as
C                     absolute values.
C     8th Dec 1990   JAB / JAC.  Adapted from FIGS321.
C     9th Dec 1990   JAB / JAC.  Add handling of quality.
C     3rd Mar 1991   JAB / JAC.  Improve Commenting.
C     6th Mar 1991   JAB / JAC.  Handle unknown variance case.
C     23rd Jan 1992  HME / UoE, Starlink.  Set output qualities zero
C                                initially.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY, NUM_CYCLES(NX), IYST, IYEN
      LOGICAL NORM
      BYTE    EXCLUDE(NX,NY), QUALITY(NX), IQUAL(NX,NY)
      REAL    SPECT(NX), ERRORS(NX), IMAGE(NX,NY), CUTOFF
C
C     Local variables
C
      LOGICAL FINISHED
      INTEGER IY, IX, NSUM, STATUS
      REAL X, SUM, NF, SIGMA
      CHARACTER*80 OUTMES
      REAL FUNKNOWN_OUT
C
C     Set up value for unknown variance - This is to support a possible
C     future upgrade to DSA to allow flagged values in error and variance
C     arrays. When such a DSA version is available FUNKNOWN_OUT should be
C     set equal to the flag value
C
      FUNKNOWN_OUT = 0
C
C     Copy the quality array of the original image to form the
C     initial EXCLUDE array. Additional points may subsequently get
C     added to the EXCLUDE array if the CUTOFF option is in use.
C
      CALL GEN_MOVE(NX*NY,IQUAL,EXCLUDE)
C
C     Fill output quality with zeroes.
C
      CALL GEN_FILL(NX,0,QUALITY)
C
C     Create the spectrum array
C
      FINISHED = .FALSE.
      DO WHILE (.NOT. FINISHED)
         FINISHED = .TRUE.
         IF (NORM) THEN
C
C        If the normalize option is in use
C        Sum all points in the image to get mean level
C
            SUM = 0
            NSUM = 0
            DO IY=IYST,IYEN
               DO IX=1,NX
                  IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                     SUM=SUM+IMAGE(IX,IY)
                     NSUM=NSUM+1
                  END IF
               END DO
            END DO
            IF (NSUM .NE. 0) THEN
                SUM=SUM/NSUM
            ELSE
                SUM = 0.0
            END IF
         END IF
C
C        Calculate the spectrum by summing the points for all rows and
C        dividing by the number of good cycles
C
         CALL GEN_FILL(NX*4,0,SPECT)
         CALL GEN_FILL(NX*4,0,NUM_CYCLES)
         DO IX=1,NX
            DO IY=IYST,IYEN
               IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                   SPECT(IX)=SPECT(IX)+IMAGE(IX,IY)
                   NUM_CYCLES(IX)=NUM_CYCLES(IX)+1
               END IF
            END DO
            IF (NUM_CYCLES(IX) .NE. 0) THEN
                SPECT(IX)=SPECT(IX)/REAL(NUM_CYCLES(IX))
            ELSE
                QUALITY(IX) = 1
            END IF
         END DO
C
C     Create the errors array
C
         IF ((IYEN-IYST) .GE. 1) THEN
            CALL GEN_FILL(NX*4,0,ERRORS)
            DO IY=IYST,IYEN
               IF (NORM) THEN
C
C     For the normalized case first calculate the mean value for the row
C
                  NF = 0.0
                  NSUM = 0
                  DO IX=1,NX
                     IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                         NF=NF+IMAGE(IX,IY)
                         NSUM = NSUM+1
                     END IF
                  END DO
                  IF (NSUM .NE. 0) THEN
                      NF=NF/NSUM
                  ELSE
                      NF = 0.0
                  END IF
C
C     Now calculate the rms deviation from the mean after scaling each
C     point by the factor SUM/NF which sets the mean for the cycle to
C     the mean for the whole image
C
                  DO IX=1,NX
                     IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                         X=SUM/NF*IMAGE(IX,IY)-SPECT(IX)
                         ERRORS(IX)=ERRORS(IX)+X*X
                     END IF
                  END DO
               ELSE
C
C     When not normalizing simply calculate the rms deviation from the mean
C
                  DO IX=1,NX
                     IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                        X=IMAGE(IX,IY)-SPECT(IX)
                        ERRORS(IX)=ERRORS(IX)+X*X
                     END IF
                  END DO
               END IF
            END DO
C
C     Calculate standard error
C          sample standard deviation SIGMA = SQRT(RMS/(N-1))
C          standard error of mean = SIGMA/(SQRT(N)
C
            DO IX=1,NX
               IF (NUM_CYCLES(IX) .GT. 1) THEN
                   ERRORS(IX)=SQRT(ERRORS(IX)/NUM_CYCLES(IX)/
     :                     (NUM_CYCLES(IX)-1))
               ELSE
                   ERRORS(IX)=FUNKNOWN_OUT
               END IF
            END DO
         END IF
C
C     Check for points to exclude - i.e. any that deviate from
C     the mean by more than CUTOFF times SIGMA - if there are any, set
C     FINISHED to false to repeat calculation of spectrum and error
C
         IF (CUTOFF .LT. 1E37) THEN
            DO IX=1,NX
               IF (NUM_CYCLES(IX) .GT. 1) THEN
                  SIGMA = ERRORS(IX) * SQRT(REAL(NUM_CYCLES(IX)))
                  DO IY=IYST,IYEN
                     IF (EXCLUDE(IX,IY) .EQ. 0) THEN
                        IF (ABS(IMAGE(IX,IY)-SPECT(IX)) .GT.
     :                          (CUTOFF*SIGMA)) THEN
                           EXCLUDE(IX,IY) = 1
                           FINISHED = .FALSE.
                           WRITE(OUTMES,'('' Point'',I4,'' Cycle'',I4,
     :                          '' Excluded'')') IX,IY
                           CALL PAR_WRUSER(OUTMES,STATUS)
                        END IF
                     END IF
                  END DO
               END IF
            END DO
         END IF
C
C   Loop back to repeat calculation of mean and errors until there are
C   no more points to exclude
C
      END DO
      END
