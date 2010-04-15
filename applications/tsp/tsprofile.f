C+
      SUBROUTINE TSPROFILE(STATUS)
C
C            T S P R O F I L E
C
C     Command name:
C        TSPROFILE
C
C     Function:
C        Determine a spatial profile from a time series image
C
C     Description:
C        This command is used to generate a spatial profile time series
C        image which is a smoothed representation of the actual stellar
C        profile. The profile can be used for subsequent optimal extraction
C        of a light curve of the star using the TSEXTRACT command.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset
C    (2) PROFILE   (TSP, 3D)   The output profile
C    (3) RESIDUALS (TSP, 3D)   The residuals file
C    (4) X         (Integer)   X position of centre of star
C    (5) Y         (Integer)   Y position of centre of star
C    (6) SIZE      (Integer)   Size of box to determine profile over
C    (7) DEGREE    (Integer)   Degree of polynomial
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 14/11/1991
C
C-
C
C  History:
C    14/11/1991   Original Version.   JAB/JAC
C    15/11/1991   Remove use of variance.    JAB/JAC
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

      CHARACTER*(DAT__SZLOC) LOC,OLOC,LOC2,RLOC,RLOC2
      CHARACTER*(DAT__SZLOC) XLOC,YLOC,WLOC,W1LOC,W2LOC
      INTEGER PTR,NDIMS,DIMS(3),XPTR,YPTR,WPTR,W1PTR,W2PTR,RPTR
      INTEGER X,Y,SIZE,XS,XE,YS,YE,NX,NY,DEGREE

C     Maximum polynomial degree

      INTEGER MAX_DEGREE
      PARAMETER (MAX_DEGREE = 10)

*  Get the input file

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)
      NX=DIMS(1)
      NY=DIMS(2)

*  Make the output file
      CALL DAT_CREAT('PROFILE','NDF',0,0,STATUS)
      CALL DAT_ASSOC('PROFILE','WRITE',OLOC,STATUS)
      CALL TSP_COPY(LOC,OLOC,STATUS)
      CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR,LOC2,STATUS)

*  Make the residuals file
      CALL DAT_CREAT('RESIDUALS','NDF',0,0,STATUS)
      CALL DAT_ASSOC('RESIDUALS','WRITE',RLOC,STATUS)
      CALL TSP_COPY(LOC,RLOC,STATUS)
      CALL TSP_MAP_DATA(RLOC,'WRITE',RPTR,RLOC2,STATUS)

*  Get parameters of aperture
      CALL PAR_GET0I('X',X,STATUS)
      CALL PAR_GET0I('Y',Y,STATUS)
      CALL PAR_GET0I('SIZE',SIZE,STATUS)
      XE = MIN(NX,X+SIZE/2)
      XS = MAX(1,X-SIZE/2)
      YE = MIN(NY,Y+SIZE/2)
      YS = MAX(1,Y-SIZE/2)

*  Get degree of polynomial
90    CALL PAR_GET0I('DEGREE',DEGREE,STATUS)
      IF (DEGREE .GT. MAX_DEGREE) THEN
         CALL MSG_SETI('MAXD', MAX_DEGREE)
         CALL MSG_OUT(' ','Maximum degree is ^MAXD - please reenter',
     :                STATUS)
         GOTO 90
      ENDIF

*  Workspace arrays
      CALL TSP_TEMP(DIMS(3),'_DOUBLE',XPTR,XLOC,STATUS)
      CALL TSP_TEMP(DIMS(3),'_DOUBLE',YPTR,YLOC,STATUS)
      CALL TSP_TEMP(DIMS(3),'_DOUBLE',WPTR,WLOC,STATUS)
      CALL TSP_TEMP(DIMS(3)*3+3*(DEGREE+1),'_DOUBLE',W1PTR,W1LOC,
     :              STATUS)
      CALL TSP_TEMP(DIMS(3),'_DOUBLE',W2PTR,W2LOC,STATUS)

*  Do the profiling on the frame
      IF (STATUS .EQ. SAI__OK) THEN
          CALL PROFILE_WORK(DIMS(1),DIMS(2),DIMS(3),
     :       DIMS(3)*3+3*(DEGREE+1),
     :       %VAL(PTR),%VAL(RPTR),%VAL(XPTR),%VAL(YPTR),
     :       %VAL(WPTR),%VAL(W1PTR),%VAL(W2PTR),XS,XE,YS,YE,DEGREE,
     :       0,STATUS)
      ENDIF

*  Tidy up
100   CONTINUE
      END



      SUBROUTINE PROFILE_WORK(NX,NY,NZ,NA,I,RES,X,Y,W,A,R,XS,XE,
     :      YS,YE,DEGREE,NREJECT,STATUS)
C+
C
C     Subroutine to perform the work of the PROFILE command
C
C     Parameters -  (">" input, "!" modifed, "<" output, "W" workspace)
C
C     (>) NX      (Integer) The first dimension of the data cube
C     (>) NY      (Integer) The second dimension of the data cube
C     (>) NZ      (Integer) The third dimension of the data cube
C     (>) NA      (Integer) Size of work array A
C     (!) I       (Real array I(NX,NY,NZ)) The image array
C     (<) RES     (Real array RES(NX,NY,NZ)) The residuals array
C     (>) X       (Double array X(NZ)) X array for polynomial fit
C     (>) Y       (Double array Y(NZ)) Y array for polynomial fit
C     (>) W       (Double array W(NZ)) Weight array for polynomial fit
C     (>) A       (Double array A(NA)) Workspace array for poly fit
C     (>) R       (Double array R(NZ)) Results array for poly fit
C     (>) XS      (Integer) The first X value to use
C     (>) XE      (Integer) The last X value to use
C     (>) YS      (Integer) The first Y value to use
C     (>) YE      (Integer) The last Y value to use
C     (>) DEGREE  (Integer) The degree of polynomial to use
C     (>) NREJECT (Integer) Number of points to reject from each fit
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     PDA_DPOLFT      (Starlink PDA library)  Fit Polynomial
C
C                                            JAB / AAO 14th Nov 1991
C     Modified:
C       20/2/97  BKM/Starlink/RAL
C          Convert from NAG routine E02ADF to PDA_POLYFT
C
C+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

      INTEGER NX,NY,NZ,NA
      REAL I(NX,NY,NZ),RES(NX,NY,NZ)
      DOUBLE PRECISION X(NZ),Y(NZ),W(NZ)
      INTEGER XS,XE,YS,YE,DEGREE,NREJECT
      INTEGER STATUS


C     Functions

      INTEGER IX,IY,IZ,JX,JY
      REAL SUM
      DOUBLE PRECISION A(NA) ! Array of polynomial coefficients
      DOUBLE PRECISION R(NZ)
      DOUBLE PRECISION EPS,DUMMY
      INTEGER NDEG
      INTEGER IFAIL
      DOUBLE PRECISION DX,DX2
      DOUBLE PRECISION XCAP,VALUE
      INTEGER NREJ,IMAX
      DOUBLE PRECISION RCUR,RMAX
      INTEGER INDEX
      INTEGER NGOOD

C
C     Form initial (noisy) estimate of spatial profile image by dividing
C     each pixel by sum over spatial pixels in that column - If there
C     are any bad points in a column we can't use that column, so set quality
C     bad for the whole column.
C
      DO IZ=1,NZ

C     Sum good data

          SUM = 0.0
          DO IY=YS,YE
            DO IX=XS,XE
              IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                  SUM = SUM+I(IX,IY,IZ)
              ELSE

*     If there is any bad data set whole column to bad

                  DO JY=YS,YE
                    DO JX=XS,XE
                      I(JX,JY,IZ) = VAL__BADR
                    ENDDO
                  ENDDO
              ENDIF
            ENDDO
          ENDDO

*     Divide data by sum to get normalized profile

          DO IY=YS,YE
            DO IX=XS,XE
              IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                  I(IX,IY,IZ)=I(IX,IY,IZ)/SUM
              ENDIF
            ENDDO
          ENDDO
      ENDDO
C
C     Do polynomial fits along each row to give more accurate profile image
C
C
C     Set up parameters needed for PDA_DPOLFT
C
      DX = DBLE(NZ-1)
      DX2 = DBLE(NZ+1)
C
C     Loop over rows
C
      DO IY=YS,YE
        DO IX=XS,XE
C
C     First go through the data and count the good points, and
C     find out if all the points have known nonzero variances
C
          NGOOD = 0
          DO IZ = 1,NZ
             IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                NGOOD = NGOOD+1
             ENDIF
          ENDDO
C
C     Set up arrays for polynomial fit to curent row.
C     Note that the weight required by PDA_DPOLFT is 1/variance
C     not 1/uncertainty as previously required by the NAG routine E02BDF.
C
          INDEX=1
          DO IZ=1,NZ
              IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                  X(INDEX) = DBLE(IZ)
                  Y(INDEX) = I(IX,IY,IZ)
                  W(INDEX) = 1D0
                  INDEX = INDEX+1
              ENDIF
          ENDDO
C
C     Do the polynomial fit
C
          EPS = 0.0D0
          CALL PDA_DPOLFT(NGOOD,X,Y,W,DEGREE,NDEG,EPS,R,IFAIL,A,STATUS)
          IF (NDEG .NE. DEGREE .OR. IFAIL .NE. 1) THEN
              CALL MSG_SETI('IFAIL', IFAIL)
              CALL MSG_OUT(' ','Error in PDA_DPOLFT - IFAIL = ^IFAIL',
     :                     STATUS)
              GOTO 500
          ENDIF

          DO NREJ = 1,NREJECT
C
C     Calculate residuals and delete worst point
C
              RMAX = -1D0
              DO INDEX = 1,NGOOD
                  XCAP = (2D0*X(INDEX) - DX2)/DX
                  IFAIL=0
                  CALL PDA_DP1VLU(DEGREE,0,XCAP,VALUE,DUMMY,A,IFAIL)
                  RCUR = ABS(Y(INDEX)-VALUE)
                  IF (RCUR .GT. RMAX) THEN
                      RMAX = RCUR
                      IMAX = INDEX
                  ENDIF
              ENDDO
              DO INDEX = IMAX,NGOOD-1
                 Y(INDEX)=Y(INDEX+1)
                 W(INDEX)=W(INDEX+1)
                 X(INDEX)=X(INDEX+1)
              ENDDO
              NGOOD=NGOOD-1
C
C     Do the polynomial fit
C
              EPS=0.0D0
              CALL PDA_DPOLFT(NGOOD,X,Y,W,DEGREE,NDEG,EPS,R,IFAIL,A,
     :                        STATUS)
              IF (IFAIL .NE. 1) THEN
                  CALL MSG_SETI('IFAIL', IFAIL)
                  CALL MSG_OUT(' ','Error in PDA_DPOLFT - IFAIL=^IFAIL',
     :                         STATUS)
                  GOTO 500
              ENDIF
          ENDDO
C
C    Replace data by fit
C
          PRINT *,IX,IY,EPS
          DO IZ=1,NZ
              XCAP = (2D0*DBLE(IZ) - DX2)/DX
              IFAIL=0
              CALL PDA_DP1VLU(DEGREE,0,XCAP,VALUE,DUMMY,A,IFAIL)
              IF (IFAIL .EQ. 0) THEN
                  IF (VALUE .LT. 0D0) VALUE=0D0
                  RES(IX,IY,IZ) = VALUE-I(IX,IY,IZ)
                  I(IX,IY,IZ) = VALUE
              ELSE
                  CALL MSG_SETI('IFAIL',IFAIL)
                  CALL MSG_OUT(' ',
     :                 'Error in PDA_DP1VLU - IFAIL = ^IFAIL',STATUS)
                  I(IX,IY,IZ) = VAL__BADR
                  RES(IX,IY,IZ) = 0.0
              ENDIF
          ENDDO
500       CONTINUE
        ENDDO
      ENDDO
C
C     Enforce normalization
C
      DO IZ=1,NZ
          SUM = 0.0
          DO IY=YS,YE
            DO IX=XS,XE
              IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                  SUM = SUM+I(IX,IY,IZ)
              ENDIF
            ENDDO
          ENDDO
          DO IY=YS,YE
            DO IX=XS,XE
              IF (I(IX,IY,IZ) .NE. VAL__BADR) THEN
                 I(IX,IY,IZ)=I(IX,IY,IZ)/SUM
              ENDIF
            ENDDO
          ENDDO
      ENDDO
C
C     Outside window set quality to bad and data to zero
C

C     Y values below YS

      DO IY=1,YS-1
        DO IX=1,NX
          DO IZ=1,NZ
              I(IX,IY,IZ)=VAL__BADR
              RES(IX,IY,IZ)=0.0
          ENDDO
        ENDDO
      ENDDO

C     Y values above YE

      DO IY=YE+1,NY
        DO IX=1,NX
          DO IZ=1,NZ
              I(IX,IY,IZ)=VAL__BADR
              RES(IX,IY,IZ)=0.0
          ENDDO
        ENDDO
      ENDDO

C     X values below XS

      DO IY=YS,YE
        DO IX=1,XS-1
          DO IZ=1,NZ
              I(IX,IY,IZ)=VAL__BADR
              RES(IX,IY,IZ)=0.0
          ENDDO
        ENDDO
      ENDDO

C     X values above XE

      DO IY=YS,YE
        DO IX=XE+1,NX
          DO IZ=1,NZ
              I(IX,IY,IZ)=VAL__BADR
              RES(IX,IY,IZ)=0.0
          ENDDO
        ENDDO
      ENDDO


      END

