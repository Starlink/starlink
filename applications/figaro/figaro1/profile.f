C+
      SUBROUTINE PROFILE
C
C     P R O F I L E
C
C     PROFILE determines a spatial profile image for subsequent use
C     by the optimum extraction program OPTEXTRACT. PROFILE uses the
C     technique described by Horne, 1986 (PASP 98, 609). An initial
C     (noisy) estimate of the spatial profile is made by dividing the
C     values along each column by the sum over that column. A smoothed
C     version of this profile is then constructed by fitting polynomials
C     along each row to account for smooth variations of the spatial
C     profile with wavelength. The technique ensures that the profile
C     is normalized (sums to one along each column) and is everywhere
C     positive. Rejection of the NREJECT worst points in each fit
C     allows the method to be insensitive to cosmic-ray hits or other
C     bad data points.
C
C     Error and quality information may be present on the input image.
C     Points with bad quality will be ignored in the fit, and the errors
C     may be used to weight the fit.
C
C     An image containing the residuals of the fit to the profile is
C     generated and may be used to judge the quality of the fit.
C
C     The Horne algorithm is appropriate for the case where there is
C     only a slight tilt or distortion on the spectrum. Where the tilt
C     or distortion is more extreme, such as in a cross-dispersed
C     echelle spectrum, an algorithm such as that of Marsh, 1989
C     (PASP 101, 1032) should be used to generate the spatial profile.
C
C     Command parameters -
C
C     'IMAGE'     The name of the input file.
C     'YSTART'    Starting Y value to use.
C     'YEND'      Ending Y value to use.
C     'DEGREE'    Degree of polynomial to use in fit.
C     'NREJECT'   Number of points to reject in each fit.
C     'PROFILE'   The name of the output file containing the normalized
C                    spatial profile.
C     'RESIDUAL'  The name of the data file containing the residuals of
C                    the fit.
C
C     Command keywords -
C
C     'WEIGHT'    Use the error or variance array to weight the
C                 polynomial fit.
C
C
C                                     JAB / JAC 8th Feb 1991
C
C     Modified:
C     10th Feb 1991  JAB / JAC  Add NREJECT parameter and RESIDUAL
C                    output file
C     8th  Mar 1991  JAB / JAC  Use Variance instead of error, add
C                    WEIGHT keyword
C     23rd Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     17th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C                    PDA_DPOLFT requires 1/variance as weight.
C     20th Jun 1996  MJC / Starlink, RAL.  Bug fix: moved call to
C                    DSA_COERCE_DATA_ARRAY for RESIDUAL to be before
C                    DSA_SET_OBJECT so that there is a dataset in which
C                    to write the object.
C     29th Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER MAX_DEGREE
      PARAMETER (MAX_DEGREE = 10)
C
C     Functions
C
      LOGICAL   PAR_ABORT        ! (F)PAR abort flag
C
      INTEGER   DEGREE           ! Polynomial degree
      INTEGER   DIMS(2)          ! Image dimensions
      LOGICAL   EXIST            ! True if error information available
      INTEGER   YS               ! Initial pixel number
      INTEGER   YE               ! Final pixel number
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   NREJECT          ! Number of points to reject
      INTEGER   OPTR1            ! Dynamic-memory pointer for image data
      INTEGER   QPTR1            ! Output quality pointer for image
      INTEGER   RPTR             ! Pointer to residual data
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
      CHARACTER*40 STRINGS(2)    ! Strings for SET_DATA_INFO
      INTEGER   TEMP             ! Temporary value
      REAL      VALUE            ! value from PAR_RDVAL
      INTEGER   VPTR1            ! Dynamic-memory pointer for image
                                 ! variance
      LOGICAL   WEIGHT           ! True if errors to be used as weights
      INTEGER   WPTR             ! Dynamic-memory pointer of W array
      INTEGER   W1PTR            ! Dynamic-memory pointer of W1 array
      INTEGER   XPTR             ! Dynamic-memory pointer of X array
      INTEGER   YPTR             ! Dynamic-memory pointer of Y array
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
C
C     Get size of data in IMAGE
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS .NE. 0) GOTO 500
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Data Must be 2 Dimensional',STATUS)
         GO TO 500
      ELSE
         NX=DIMS(1)
         NY=DIMS(2)
      END IF
C
C     Get range
C
      CALL PAR_RDVAL('YSTART',1.0,REAL(NY),1.0,' ',VALUE)
      YS = NINT(VALUE)
      CALL PAR_RDVAL('YEND',1.0,REAL(NY),REAL(NY),' ',VALUE)
      YE = NINT(VALUE)
      IF (YE .LT. YS) THEN
          TEMP = YE
          YE = YS
          YS = TEMP
      END IF
C
C     Get polynomial degree
C
      CALL PAR_RDVAL('DEGREE',1.0,10.0,5.0,' ',VALUE)
      DEGREE=NINT(VALUE)
      CALL PAR_RDVAL('NREJECT',0.0,NX-10.0,10.0,' ',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      NREJECT=NINT(VALUE)
C
C     Is there error information?
C
      CALL DSA_SEEK_ERRORS('IMAGE',EXIST,STATUS)
C
C     If there is error information, should it be used to weight the fit?
C
      IF (EXIST) THEN
         CALL PAR_RDKEY('WEIGHT',.TRUE.,WEIGHT)
         IF ( PAR_ABORT() ) GO TO 500
      ELSE
         WEIGHT = .FALSE.
      END IF
C
C     Create output file.
C
      CALL DSA_OUTPUT ('OUTPUT','PROFILE','IMAGE',0,1,STATUS)
      CALL DSA_USE_QUALITY('OUTPUT',STATUS)
      CALL DSA_SET_OBJECT('OUTPUT','Spatial Profile',STATUS)
      STRINGS(1)='Fraction'
      STRINGS(2)='Spatial Profile'
      CALL DSA_SET_DATA_INFO('OUTPUT',2,STRINGS,0,0,STATUS)
C
C     Create residual file
C
      CALL DSA_OUTPUT('RESIDUAL','RESIDUAL',' ',0,1,STATUS)
      CALL DSA_COERCE_DATA_ARRAY('RESIDUAL','FLOAT',2,DIMS,STATUS)
      CALL DSA_SET_OBJECT('RESIDUAL','Residuals of Profile Fit',STATUS)
      STRINGS(1)='Fraction'
      STRINGS(2)='Spatial Profile Residuals'
      CALL DSA_SET_DATA_INFO('RESIDUAL',2,STRINGS,0,0,STATUS)
C
C     Map the output data - note that we map a variance array even
C     if it does not exist, this will be filled with zeros indicating
C     that the profile is perfect data.
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR1,SLOT,STATUS)
      CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VPTR1,SLOT,STATUS)
      CALL DSA_MAP_QUALITY ('OUTPUT','UPDATE','BYTE',QPTR1,SLOT,STATUS)
      CALL DSA_MAP_DATA('RESIDUAL','WRITE','FLOAT',RPTR,SLOT,STATUS)
C
C     Get workspace arrays
C
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',XPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',YPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',WPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(4*NX+3*(MAX_DEGREE+1),
     :                        'DOUBLE',W1PTR,SLOT,STATUS)
C
C     Process data
C
      IF (STATUS .EQ. 0) THEN
         CALL PROFILE_WORK(NX,NY,4*NX+3*(MAX_DEGREE+1),
     :                     %VAL(CNF_PVAL(OPTR1)),%VAL(CNF_PVAL(VPTR1)),
     :                     %VAL(CNF_PVAL(QPTR1)),%VAL(CNF_PVAL(RPTR)),
     :                     %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(YPTR)),
     :                     %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(W1PTR)),
     :                     YS,YE,DEGREE,NREJECT,WEIGHT)
      END IF

  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END



      SUBROUTINE PROFILE_WORK(NX,NY,NW,I,V,Q,RES,X,Y,W,W1,YS,YE,
     :     DEGREE,NREJECT,WEIGHT)
C
C     Subroutine to perform the work of the PROFILE command
C
C     Parameters -  (">" input, "!" modifed, "<" output, "W" workspace)
C
C     (>) NX      (Integer) The first dimension of IMAGE
C     (>) NY      (Integer) The second dimension of IMAGE
C     (>) NW      (Integer) Size of W1
C     (!) I       (Real array I(NX,NY)) The image array
C     (>) V       (Real array V(NX,NY)) The variances on the image
C     (>) Q       (Byte array Q(NX,NY)) The quality array
C     (>) RES     (Real array RES(NX,NY)) The residuals array
C     (>) X       (Double array X(NY)) X array for polynomial fit
C     (>) Y       (Double array Y(NY)) Y array for polynomial fit
C     (>) W       (Double array W(NY)) Weight array for polynomial fit
C     (>) W1      (Double array W1(NW)) Workspace array for poly fit
C     (>) YS      (Integer) The first Y value to use
C     (>) YE      (Integer) The last Y value to use
C     (>) DEGREE  (Integer) The degree of polynomial to use
C     (>) NREJECT (Integer) Number of points to reject from each fit
C     (>) WEIGHT  (Logical) Use variance to weight fit
C
C     Common variables used - None
C
C                                            JAB / JAC 8th Feb 1991
C     Modified:
C      8th Mar 1991  JAB/JAC  Use variance rather then error, add
C                    WEIGHT parameter
C     13th Mar 1991  JAB/JAC  Reject the whole column if there are
C                    any bad pixels in it
C     17th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C                    PDA_DPOLFT requires 1/variance as weight.
C     24th Apr 1998  ACD / UoE, Starlink.  Trapped the case where
C                    a column of pixels all of value 0.0 (but
C                    without a bad quality) cause the sum for that
C                    column to be 0.0 when computing the initial
C                    estimate of the spacial profile.
C

      IMPLICIT NONE
      INTEGER NX,NY,NW
      REAL I(NX,NY),V(NX,NY),RES(NX,NY)
      BYTE Q(NX,NY)
      DOUBLE PRECISION X(NX),Y(NX),W(NX),W1(NW)
      INTEGER YS,YE,DEGREE,NREJECT
      LOGICAL WEIGHT

C     Maximum polynomial degree

      INTEGER MAX_DEGREE
      PARAMETER (MAX_DEGREE = 10)

C     Minimum value for column sum when computing intial estimate of
C     the spacial profile.

      REAL MINVAL
      PARAMETER (MINVAL = 1.0E-8)
C
C     Local variables
C
      INTEGER IX,IY,JY
      REAL SUM
      INTEGER KPLUS1       ! Maximum degree plus 1
      INTEGER NROWS        ! Number of rows in A
      INTEGER IFAIL,IFAIL2
      DOUBLE PRECISION DX,DX2,EPS
      INTEGER IGNORE
      DOUBLE PRECISION VALUE,DUMMY
      INTEGER NREJ,IMAX
      DOUBLE PRECISION R,RMAX
      INTEGER INDEX
      REAL FUNKNOWN_IN
      INTEGER NGOOD,NDEG
      INTEGER STATUS
      LOGICAL ERRORS

      IGNORE = 0
C
C     Set up unknown error value
C
      STATUS = 0
      CALL DSA_GET_FLAG_VALUE('FLOAT',FUNKNOWN_IN,STATUS)
C
C     Form initial (noisy) estimate of spatial profile image by dividing
C     each pixel by sum over spatial pixels in that column - If there
C     are any bad points in a column we can't use that column, so set quality
C     bad for the whole column.
C
C     The case where the sum for a column of pixels is effectively zero is
C     also trapped and the quality flag set for all the pixels in the
C     column.  In practice this case is only likely to arise if all
C     the pixels in the column are individually zero but do not have
C     a bad quality associated with them.
C
      DO IX=1,NX
          SUM = 0.0
          DO IY=YS,YE
              IF (Q(IX,IY) .EQ. 0) THEN
                  SUM = SUM+I(IX,IY)
              ELSE
                  DO JY=YS,YE
                     Q(IX,JY) = 1
                  END DO
              END IF
          END DO
          IF (ABS(SUM) .GE. MINVAL) THEN
              DO IY=YS,YE
                  IF (Q(IX,IY) .EQ. 0) THEN
                      I(IX,IY)=I(IX,IY)/SUM
                      IF (WEIGHT) THEN
                          V(IX,IY)=V(IX,IY)/(SUM*SUM)
                      END IF
                  END IF
              END DO
          ELSE
              DO IY=YS,YE
                  Q(IX,IY) = 1
              END DO
          END IF
      END DO
C
C     Do polynomial fits along each row to give more accurate profile image
C
C
C     Set up parameters needed for E02ADF
C
      KPLUS1 = DEGREE+1
      NROWS = MAX_DEGREE+1
      DX = DBLE(NX-1)
      DX2 = DBLE(NX+1)
C
C     Loop over rows
C
      DO IY=YS,YE
C
C     First go through the data and count the good points, and
C     find out if all the points have known nonzero variances
C
          ERRORS = WEIGHT
          NGOOD = 0
          DO IX = 1,NX
             IF (Q(IX,IY) .EQ. 0) THEN
                NGOOD = NGOOD+1
                IF (ERRORS) THEN
                   IF (V(IX,IY) .LE. 0 .OR.
     :                 V(IX,IY) .EQ. FUNKNOWN_IN) THEN
                      ERRORS = .FALSE.
                   END IF
                END IF
             END IF
          END DO
C
C     Set up arrays for polynomial fit to curent row.
C     Note that the weight required by E02ADF is 1/uncertainty,
C     not 1/variance as you might expect. See the NAG Manual,
C     introduction to the E02 section.
C     In PDA_DPOLFT the weights should be 1/variance.
C
          INDEX=1
          DO IX=1,NX
              IF (Q(IX,IY) .EQ. 0) THEN
                  X(INDEX) = DBLE(IX)
                  Y(INDEX) = I(IX,IY)
                  IF (ERRORS) THEN
                     W(INDEX) = 1D0/V(IX,IY)
                  ELSE
                     W(INDEX) = 1D0
                  END IF
                  INDEX = INDEX+1
              END IF
          END DO
C
C     Do the polynomial fit
C
          IFAIL2 = 0
          EPS = 0D0
          CALL PDA_DPOLFT(NGOOD,X,Y,W,KPLUS1,NDEG,EPS,W1,
     :       IFAIL,W1(NGOOD+1),IFAIL2)
          IF (NDEG.NE.KPLUS1 .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0 ) THEN
              CALL PAR_WRUSER('Error in PDA_DPOLFT',IGNORE)
              GOTO 500
          END IF

          DO NREJ = 1,NREJECT
C
C     Calculate residuals and delete worst point
C
              RMAX = -1D0
              DO INDEX = 1,NGOOD
                  IFAIL2 = 0
                  CALL PDA_DP1VLU(KPLUS1-1,0,X(INDEX),VALUE,DUMMY,
     :               W1(NGOOD+1),IFAIL2)
                  R = ABS(Y(INDEX)-VALUE)
                  IF (R .GT. RMAX) THEN
                      RMAX = R
                      IMAX = INDEX
                  END IF
              END DO
              DO INDEX = IMAX,NGOOD-1
                 Y(INDEX)=Y(INDEX+1)
                 W(INDEX)=W(INDEX+1)
                 X(INDEX)=X(INDEX+1)
              END DO
              NGOOD=NGOOD-1
C
C     Do the polynomial fit
C
              IFAIL2 = 0
              EPS = 0D0
              CALL PDA_DPOLFT(NGOOD,X,Y,W,KPLUS1,NDEG,EPS,W1,
     :           IFAIL,W1(NGOOD+1),IFAIL2)
              IF (NDEG.NE.KPLUS1 .OR. IFAIL.NE.1 .OR. IFAIL2.NE.0 ) THEN
                  CALL PAR_WRUSER('Error in PDA_DPOLFT',IGNORE)
                  GOTO 500
              END IF
          END DO
C
C    Replace data by fit
C
          DO IX=1,NX
              IFAIL2 = 0
              CALL PDA_DP1VLU(KPLUS1-1,0,DBLE(IX),VALUE,DUMMY,
     :           W1(NGOOD+1),IFAIL2)
              IF (IFAIL2 .EQ. 0) THEN
                  IF (VALUE .LT. 0D0) VALUE=0D0
                  RES(IX,IY) = VALUE-I(IX,IY)
                  I(IX,IY) = VALUE
                  Q(IX,IY) = 0
                  V(IX,IY) = 0.0
              ELSE
                  CALL PAR_WRUSER('Error in PDA_DP1VLU',IGNORE)
                  I(IX,IY) = 0.0
                  RES(IX,IY) = 0.0
                  Q(IX,IY) = 1
              END IF
          END DO
500       CONTINUE
      END DO
C
C     Enforce normalization
C
      DO IX=1,NX
          SUM = 0.0
          DO IY=YS,YE
              IF (Q(IX,IY) .EQ. 0) THEN
                  SUM = SUM+I(IX,IY)
              END IF
          END DO
          DO IY=YS,YE
              I(IX,IY)=I(IX,IY)/SUM
          END DO
      END DO
C
C     Outside window set quality to bad and data to zero
C
      DO IY=1,YS-1
          DO IX=1,NX
              Q(IX,IY)=1
              I(IX,IY)=0.0
              RES(IX,IY)=0.0
          END DO
      END DO
      DO IY=YE+1,NY
          DO IX=1,NX
              Q(IX,IY)=1
              I(IX,IY)=0.0
              RES(IX,IY)=0.0
          END DO
      END DO

      END
