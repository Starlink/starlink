C+
      SUBROUTINE POLYSKY
C
C     P O L Y S K Y
C
C     POLYSKY is used to subtract sky from a long slit spectrum by
C     polynomial fitting in the spatial direction to two regions of
C     sky on either side of an object of interest.
C
C     Only the region of the image between the outer edges of the two
C     sky fields is sky subtracted. Data outside this region is
C     unchanged. This enables POLYSKY to be used repeatedly to remove
C     sky from more than one object spectrum on an image.
C
C     The input image may optionally have associated error and quality
C     information. If quality is present points with bad quality will
C     be omitted from the fit. If error or variance is present the
C     values may be used to weight the fit.
C
C     If a non zero value for NREJECT is specified this number of points
C     will be omitted from the fit to each column. The points chosen for
C     omission will be those which deviate most from the mean for the
C     column.
C
C     Command parameters -
C
C     'IMAGE'    The name of the input file.
C     'YS1'      Starting Y value to use for first sky field
C     'YE1'      Ending Y value to use for first sky field
C     'YS2'      Starting Y value to use for first sky field
C     'YE2'      Ending Y value to use for first sky field
C     'DEGREE'   Degree of the polynomial fit
C     'NREJECT'  Number of points to reject in each fit
C     'OUTPUT'   The name of the output subtracted file.
C
C     Command keywords -
C
C     'WEIGHT'   Use the error or variance array to weight the
C                polynomial fit.
C
C                                     JAB / JAC 7th Feb 1991
C
C     Modified:
C
C      7th Mar 1991  JAB / JAC. Only use error weighting if ALL errors
C                    are known and non-zero.
C      7th Mar 1991  JAB / JAC. Use Variance rather than error.
C      8th Mar 1991  JAB / JAC. Add WEIGHT keyword.
C     23rd Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C      6th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C                    DPOLFT requires 1/variance as weight.
C     18th Mar 1997  JJL / Soton, Starlink. Error propagation included.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Maximum polynomial degree
C
      INTEGER MAX_DEGREE
      PARAMETER (MAX_DEGREE = 10)
C
      INTEGER   DEGREE           ! Degree of polynomial
      INTEGER   DIMS(2)          ! Image dimensions
      INTEGER   DUMMY            ! Dummy argument
      LOGICAL   EXIST            ! TRUE if error information present
      INTEGER   IGNORE           ! Ignoreable status
      INTEGER   NDIM             ! Number of image dimensions
      INTEGER   NELM             ! Number of elements in image - ignored
      INTEGER   NX               ! First dimension of image
      INTEGER   NY               ! Second dimension of image
      INTEGER   OPTR1            ! Dynamic-memory pointer for image data
      INTEGER   REJECT           ! Number of points to reject
      INTEGER   SLOT             ! Slot number for mapped data - ignored
      INTEGER   STATUS           ! Running status for DSA routines
      INTEGER   TEMP             ! Temporary value
      INTEGER   QPTR1            ! Output quality pointer for image
      REAL      VALUE            ! VALUE FROM PAR_RDVAL
      INTEGER   VPTR1            ! Dynamic-memory pointer for image
                                 ! variance
      LOGICAL   WEIGHT           ! Value of WEIGHT keyword
      INTEGER   WPTR             ! Dynamic-memory pointer of W array
      INTEGER   W1PTR            ! Dynamic-memory pointer of W1 array
      INTEGER   XPTR             ! Dynamic-memory pointer of X array
      INTEGER   YPTR             ! Dynamic-memory pointer of Y array
      INTEGER   YS1              ! Initial pixel number for field 1
      INTEGER   YE1              ! Final pixel number for field 1
      INTEGER   YS2              ! Initial pixel number for field 2
      INTEGER   YE2              ! Final pixel number for field 2
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
C     Get range of Y values for first region
C     Put in ascending order if necessary
C
      CALL PAR_RDVAL('YS1',1.0,REAL(NY),1.0,' ',VALUE)
      YS1 = NINT(VALUE)
      CALL PAR_RDVAL('YE1',1.0,REAL(NY),REAL(NY),' ',VALUE)
      YE1 = NINT(VALUE)
      IF (YE1 .LT. YS1) THEN
          TEMP = YE1
          YE1 = YS1
          YS1 = TEMP
      END IF
C
C     Get range of Y values for second region
C     Put in ascending order if necessary
C
      CALL PAR_RDVAL('YS2',1.0,REAL(NY),1.0,' ',VALUE)
      YS2 = NINT(VALUE)
      CALL PAR_RDVAL('YE2',1.0,REAL(NY),REAL(NY),' ',VALUE)
      YE2 = NINT(VALUE)
      IF (YE2 .LT. YS2) THEN
          TEMP = YE2
          YE2 = YS2
          YS2 = TEMP
      END IF
C
C     Check that regions don't overlap
C
      IF (YS2 .LE. YE1) THEN
         IF (YE2 .LT. YS1) THEN
C
C     Swap the regions round - they are in the wrong order
C
            TEMP = YS1
            YS1 = YS2
            YS2 = TEMP
            TEMP = YE1
            YE1 = YE2
            YE2 = TEMP
            IF (YS2 .LE. YE1) THEN
               CALL PAR_WRUSER('The two regions must not overlap',
     :          IGNORE)
               GOTO 500
            END IF
         ELSE
            CALL PAR_WRUSER('The two regions must not overlap',
     :          IGNORE)
            GOTO 500
         END IF
      END IF
C
C     Get the polynomial degree and rejection parameters
C
      CALL PAR_RDVAL('DEGREE',1.0,10.0,3.0,' ',VALUE)
      DEGREE = NINT(VALUE)
      CALL PAR_RDVAL('NREJECT',0.0,10.0,0.0,' ',VALUE)
      REJECT = NINT(VALUE)
C
C     Is there error information?
C
      EXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE('IMAGE',EXIST,STATUS)
C
C     If there is error information, should it be used to weight the
C     fit?
C
      IF (EXIST) THEN
         CALL PAR_RDKEY('WEIGHT',.TRUE.,WEIGHT)
      ELSE
         WEIGHT = .FALSE.
      END IF
C
C     Create output file.
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,1,STATUS)
      CALL DSA_USE_QUALITY('OUTPUT',STATUS)
C
C     Map the output data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR1,SLOT,STATUS)
      IF (WEIGHT) THEN
         CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VPTR1,
     :                         SLOT,STATUS)
      END IF
      CALL DSA_MAP_QUALITY ('OUTPUT','UPDATE','BYTE',QPTR1,SLOT,STATUS)
C
C     Get workspace arrays needed for polynomial fitting
C
      CALL DSA_GET_WORK_ARRAY(NY,'DOUBLE',XPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NY,'DOUBLE',YPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NY,'DOUBLE',WPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(4*NY+3*(MAX_DEGREE+1),
     :                        'DOUBLE',W1PTR,SLOT,STATUS)
C
C     Process data
C
      IF (STATUS .EQ. 0) THEN
         CALL POLYSKY_WORK(NX,NY,4*NY+3*(MAX_DEGREE+1),
     :                     %VAL(CNF_PVAL(OPTR1)),%VAL(CNF_PVAL(VPTR1)),
     :                     %VAL(CNF_PVAL(QPTR1)),%VAL(CNF_PVAL(XPTR)),
     :                     %VAL(CNF_PVAL(YPTR)),%VAL(CNF_PVAL(WPTR)),
     :                     %VAL(CNF_PVAL(W1PTR)),YS1,YE1,YS2,YE2,
     :                     DEGREE,REJECT,WEIGHT)
      END IF

  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END



      SUBROUTINE POLYSKY_WORK(NX,NY,NW,I,V,Q,X,Y,W,WRK,YS1,YE1,YS2,YE2,
     :    DEGREE,REJECT,WEIGHT)
C
C     Subroutine to perform the work of the POLYSKY command
C     A polynomial fit is performed along each column of the
C     input image to the points within the two Y fields specified.
C     This polynomial is then subtracted from the image in this
C     area.
C
C     The data outside the areas covered by the two fields is unchanged.
C
C     Parameters -  (">" input, "!" modifed, "<" output, "W" workspace)
C
C     (>) NX      (Integer) The first dimension of IMAGE
C     (>) NY      (Integer) The second dimension of IMAGE
C     (>) NW      (Integer) size of workspace array
C     (!) I       (Real array I(NX,NY)) The image array
C     (>) V       (Real array V(NX,NY)) The variances on the image
C     (>) Q       (Byte array Q(NX,NY)) The quality array
C     (W) X       (Double array X(NY)) Array of X values for fit
C     (W) Y       (Double array Y(NY)) Array of Y values for fit
C     (W) W       (Double array W(NY)) Array of weights for fit
C     (W) W1      (Double array W1(NW)) Workspace array for E02ADF
C     (>) YS1     (Integer) The first Y value to for sky field 1
C     (>) YE1     (Integer) The last Y value to for sky field 1
C     (>) YS2     (Integer) The first Y value to for sky field 2
C     (>) YE2     (Integer) The last Y value to for sky field 2
C     (>) DEGREE  (Integer) Degree of polynomial to fit
C     (>) REJECT  (Integer) Number of points to reject
C     (>) WEIGHT  (Logical) TRUE if variances are to be used as weights
C
C     Common variables used - None
C
C                                            JAB / JAC 7th Feb 1991
C     Modified:
C
C      7th Mar 1991  JAB / JAC   Only use error weighting if ALL errors
C                    for column are known and non-zero.
C      7th Mar 1991  JAB / JAC   Use variance rather than error.
C      8th Mar 1991  JAB / JAC   Only weight fit if WEIGHT is true.
C      6th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C                    DPOLFT requires 1/variance as weight.
C      11th Dec 1997 ACD / UoE, Starlink.  Fixed a bug, so that
C                    the second moment of the sky background, SVAR,
C                    is only calculated if the variance array, V, is
C                    defined.  (Note: I suspect that this subroutine
C                    was modified by JJL when error propagation was
C                    added.)
C      11th Sep 2002 ACD / UoE, Starlink.  Fixed a bug in the propagation
C                    of errors: the variance in the modified points was
C                    made to depend on the variance of the mean of the
C                    set of points used to compute the polynomial, not
C                    the variance of these points.
C

      IMPLICIT NONE
      INTEGER NX,NY,NW
      REAL I(NX,NY),V(NX,NY)
      BYTE Q(NX,NY)
      DOUBLE PRECISION X(NY),Y(NY),W(NY),WRK(NW)
      INTEGER YS1,YE1,YS2,YE2,DEGREE,REJECT
      LOGICAL WEIGHT

C     Maximum polynomial degree

      INTEGER MAX_DEGREE
      PARAMETER (MAX_DEGREE = 10)

C     Functions

      CHARACTER*14 ICH_CI

C     Local variables

      INTEGER NGOOD        ! Number of good points for fit
      INTEGER KPLUS1       ! Maximum degree plus 1
      INTEGER IFAIL,IFAIL2
      DOUBLE PRECISION DX,DX2
      INTEGER IX,IY,INDEX
      INTEGER IGNORE
      DOUBLE PRECISION VALUE, SVAR
      INTEGER NREJ,IMAX
      DOUBLE PRECISION R,RMAX,MEAN
      REAL FUNKNOWN_IN
      LOGICAL ERRORS
      INTEGER STATUS
      INTEGER NDEG
      DOUBLE PRECISION EPS,DUMMY(1)
      DOUBLE PRECISION VMAX           ! A Large number used to minimise the
                                      ! weight of a "dodgy" point
      IGNORE = 0
      VMAX = 1.0E+20
C
C     Set up unknown error value
C
      STATUS = 0
      CALL DSA_GET_FLAG_VALUE('FLOAT',FUNKNOWN_IN,STATUS)
C
C     Set up parameters needed for E02ADF
C
      KPLUS1 = DEGREE+1
      DX = DBLE(YE2-YS1)
      DX2 = DBLE(YE2+YS1)
C
C     Loop over columns of data
C
      DO IX = 1,NX
C
C     Build array of values to do polynomial fit to. These are
C     used as the X,Y and W parameters in the PDA sunbroutines.
C     The X value is the Y index in the original array
C     The Y value is the data value in the array.
C     The W value (weight):  is 1 if the errors are zero or unknown
C                            is 1/uncertainty if the error is nonzero
C     NOTE - the weight needed is now 1/variance.
C     This should correctly handle the case where there are no errors
C     in the original data.
C
          INDEX = 1
C
C     First go through the data and count the good points, and
C     find out if all the points have known nonzero variances
C
          ERRORS = WEIGHT
          NGOOD = 0
          DO IY = YS1,YE1
             IF (Q(IX,IY) .EQ. 0) THEN
                NGOOD = NGOOD+1
                IF (ERRORS) THEN
                   IF (V(IX,IY) .EQ. FUNKNOWN_IN) THEN
                      ERRORS = .FALSE.
                   END IF
                END IF
             END IF
          END DO
          DO IY = YS2,YE2
             IF (Q(IX,IY) .EQ. 0) THEN
                NGOOD = NGOOD+1
                IF (ERRORS) THEN
                   IF (V(IX,IY) .EQ. FUNKNOWN_IN) THEN
                      ERRORS = .FALSE.
                   END IF
                END IF
             END IF
          END DO
C
C     First sky region
C
          DO IY = YS1,YE1
              IF (Q(IX,IY) .EQ. 0) THEN
                 X(INDEX) = DBLE(IY)
                 Y(INDEX) = I(IX,IY)
                 IF (ERRORS) THEN
                    IF (V(IX,IY).LE.0) THEN
                      W(INDEX) = 1D0/VMAX
                    ELSE
                      W(INDEX) = 1D0/V(IX,IY)
                    END IF
                 ELSE
                    W(INDEX) = 1D0
                 END IF
                 INDEX = INDEX+1
              END IF
          END DO
C
C     Second sky region
C
          DO IY = YS2,YE2
              IF (Q(IX,IY) .EQ. 0) THEN
                 X(INDEX) = DBLE(IY)
                 Y(INDEX) = I(IX,IY)
                 IF (ERRORS) THEN
                    IF (V(IX,IY).LE.0) THEN
                      W(INDEX) = 1.0/VMAX
                    ELSE
                      W(INDEX) = 1D0/V(IX,IY)
                    END IF
                 ELSE
                    W(INDEX) = 1D0
                 END IF
                 INDEX = INDEX+1
              END IF
          END DO
C
C     Remove REJECT worst points
C
          DO NREJ = 1,MIN(REJECT,NGOOD)
C
C     Calculate mean
C
              MEAN = 0D0
              DO INDEX = 1,NGOOD
                  MEAN = MEAN + Y(INDEX)
              END DO
              MEAN = MEAN/NGOOD
C
C     Calculate deviation from mean and delete the worst point
C
              RMAX = -1D0
              DO INDEX = 1,NGOOD
                 R = ABS(Y(INDEX)-MEAN)
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
          END DO
C
C     Now calculate the second moment of the skybackground if errors
C     are being propogated.
C
C     The SVAR computed here is used to calculate the new variances of
C     the modified data points.  The value required is approximated by
C     the variance of the mean of the set of points used to compute the
C     polynomial, not by the variance of these points:
C
C       variance of set of points = (sum of individual variances) / N
C
C       variance of mean = (sum of individual variances) / (N**2)
C
C     where N is the number of points.  Note that this variance is still
C     only an approximation.  To be completely rigorous the errors should
C     be propagated through the polynomial coefficients.
C
          IF (ERRORS) THEN
             SVAR = 0D0
             DO INDEX = 1, NGOOD
                SVAR = SVAR + V(IX,INDEX)
             END DO
             SVAR = SVAR / (NGOOD * NGOOD)
          END IF
C
C     Check that there are enough good points to do a polynomial fit
C     of the required degree - if not set quality to bad for that column
C
          IF (NGOOD .LT. KPLUS1) THEN
             CALL PAR_WRUSER('Insufficient good points to fit to'//
     :         'column '//ICH_CI(IX),IGNORE)
             DO IY=YS1,YE2
                Q(IX,IY) = 0
             END DO
          ELSE
C
C     Do the polynomial fit
C
             EPS=0D0
             IFAIL2=0
             CALL PDA_DPOLFT(NGOOD,X,Y,W,KPLUS1-1,NDEG,EPS,WRK,
     :          IFAIL,WRK(NGOOD+1),IFAIL2)
             IF (NDEG.NE.KPLUS1-1.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) THEN
                 CALL PAR_WRUSER('Error in PDA_DPOLFT',IGNORE)
                 GO TO 500
             END IF
C
C     Subtract sky values
C
             DO IY=YS1,YE2
                IF (Q(IX,IY) .EQ. 0) THEN
                   IFAIL2=0
                   CALL PDA_DP1VLU(KPLUS1-1,0,DBLE(IY),VALUE,DUMMY,
     :                WRK(NGOOD+1),IFAIL2)
                   IF (IFAIL2 .EQ. 0) THEN
                      I(IX,IY) = I(IX,IY) - VALUE
                   ELSE
                      CALL PAR_WRUSER('Error in PDA_DP1VLU',IGNORE)
                   END IF
                END IF
             END DO
C
C     Now propagate the errors through the sky and object regions.
C
             IF (ERRORS) THEN
                DO IY = YS1, YE2
                   IF (V(IX,IY).LE.0) THEN
                       Q (IX,IY)=0
                       V (IX,IY)=0
                   ELSE
                       V (IX,IY) =  ABS(V(IX,IY)) + SVAR
                   END IF
                END DO
             END IF
          END IF
500       CONTINUE
      END DO
      END
