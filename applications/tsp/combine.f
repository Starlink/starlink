C+
      SUBROUTINE COMBINE(STATUS)
C
C            C O M B I N E
C
C     Command name:
C        COMBINE
C
C     Function:
C        Combine two Polarization Datasets
C
C     Description:
C        Two Polarization datasets are added to form a new one of
C        higher signal to noise ratio. Any number of Stokes parameters
C        may be present in the data, but only Stokes parameters present
C        in both spectra will appear in the output.
C
C        COMBINE adds the intensity, Stokes parameters and variances
C        and is therefore appropriate for combining data in the form
C        of IPCS or CCD counts, but not for combining flux calibrated
C        data.
C
C     Parameters:
C    (1) INPUT1     (TSP, nD) - The first input dataset.
C    (2) INPUT2     (TSP, nD) - The second input dataset.
C    (3) OUTPUT     (TSP, nD) - The output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 8/5/1993
C
C-
C
C  History:
C    27/4/1988   Original Version.   JAB/AAO
C    15/8/1988   Allow more than one stokes parameter in
C                the input spectra.   JAB/AAO
C    19/8/1988   Correct bug in above change.   JAB/AAO
C    8/5/1993    Allow data of 1,2 or 3 dimensions.   JAB/AAO
C+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS
      INTEGER PTR1,PTR2,SPTR1,SPTR2,VPTR1,VPTR2
      CHARACTER*(DAT__SZLOC) LOC1,LOC2,OLOC,DLOC1,DLOC2
      CHARACTER*(DAT__SZLOC) SDLOC1,SDLOC2,VDLOC1,VDLOC2
      CHARACTER*(DAT__SZLOC) QLOC1,QLOC2,ULOC1,ULOC2,VLOC1,VLOC2
      INTEGER NDIMS,DIMS(7)
      INTEGER SIZE,SIZE2
      INTEGER NUM
      LOGICAL QZ1,UZ1,VZ1,QZ2,UZ2,VZ2
      LOGICAL VARS

*  Get the input and output locators

      CALL DAT_ASSOC('INPUT1','READ',LOC1,STATUS)
      CALL DAT_ASSOC('INPUT2','READ',LOC2,STATUS)
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Get the intensity data from both datasets and check dimensions

      CALL TSP_SIZE(LOC1,7,DIMS,NDIMS,STATUS)
      IF (NDIMS .EQ. 3) THEN
         SIZE = DIMS(1)*DIMS(2)*DIMS(3)
      ELSE IF (NDIMS .EQ. 2) THEN
         SIZE = DIMS(1)*DIMS(2)
      ELSE IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT1',STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF
      CALL TSP_SIZE(LOC2,7,DIMS,NDIMS,STATUS)
      IF (NDIMS .EQ. 3) THEN
         SIZE2 = DIMS(1)*DIMS(2)*DIMS(3)
      ELSE IF (NDIMS .EQ. 2) THEN
         SIZE2 = DIMS(1)*DIMS(2)
      ELSE IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT1',STATUS)
         STATUS = USER__001
      ELSE
         SIZE2 = DIMS(1)
      ENDIF
      IF (SIZE2 .NE. SIZE) THEN
         CALL MSG_OUT('MSG','Input 1 and 2 Dimensions are different',
     :          STATUS)
         STATUS = USER__001
      ENDIF
      CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR1,DLOC1,STATUS)
      CALL TSP_MAP_DATA(LOC2,'READ',PTR2,DLOC2,STATUS)

*  Get the Stokes data from output dataset

      CALL TSP_STOKES(OLOC,NUM,QZ1,UZ1,VZ1,STATUS)
      IF (QZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'Q',QLOC1,STATUS)
      ENDIF
      IF (UZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'U',ULOC1,STATUS)
      ENDIF
      IF (VZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'V',VLOC1,STATUS)
      ENDIF


      CALL TSP_STOKES(LOC2,NUM,QZ2,UZ2,VZ2,STATUS)
      IF (QZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'Q',QLOC2,STATUS)
      ENDIF
      IF (UZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'U',ULOC2,STATUS)
      ENDIF
      IF (VZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'V',VLOC2,STATUS)
      ENDIF

*  Sum the Stokes arrays and variances

      VARS = .TRUE.
      IF (QZ1) THEN
          CALL TSP_MAP_DATA(QLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(QLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (STATUS .NE.SAI__OK) THEN
              CALL ERR_ANNUL(STATUS)
              VARS = .FALSE.
          ENDIF
          IF (QZ2) THEN
              CALL TSP_MAP_DATA(QLOC2,'READ',SPTR2,SDLOC2,STATUS)
              CALL TSP_MAP_VAR(QLOC2,'READ',VPTR2,VDLOC2,STATUS)
              IF (STATUS .NE.SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  VARS = .FALSE.
              ENDIF
              CALL TSP_COMBINE(SIZE,%VAL(SPTR1),%VAL(SPTR2))
              IF (VARS) CALL TSP_COMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))
              CALL TSP_UNMAP(SDLOC2,STATUS)
              IF (VARS) CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(QLOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','Q present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'Q',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          IF (VARS) CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(QLOC1,STATUS)
      ENDIF

      IF (UZ1) THEN
          CALL TSP_MAP_DATA(ULOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(ULOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (STATUS .NE.SAI__OK) THEN
              CALL ERR_ANNUL(STATUS)
              VARS = .FALSE.
          ENDIF
          IF (UZ2) THEN
              CALL TSP_MAP_DATA(ULOC2,'READ',SPTR2,SDLOC2,STATUS)
              CALL TSP_MAP_VAR(ULOC2,'READ',VPTR2,VDLOC2,STATUS)
              IF (STATUS .NE.SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  VARS = .FALSE.
              ENDIF
              CALL TSP_COMBINE(SIZE,%VAL(SPTR1),%VAL(SPTR2))
              IF (VARS) CALL TSP_COMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))
              CALL TSP_UNMAP(SDLOC2,STATUS)
              IF (VARS) CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(ULOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','U present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'U',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          IF (VARS) CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(ULOC1,STATUS)
      ENDIF

      IF (VZ1) THEN
          CALL TSP_MAP_DATA(VLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(VLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (STATUS .NE.SAI__OK) THEN
              CALL ERR_ANNUL(STATUS)
              VARS = .FALSE.
          ENDIF
          IF (VZ2) THEN
              CALL TSP_MAP_DATA(VLOC2,'READ',SPTR2,SDLOC2,STATUS)
              CALL TSP_MAP_VAR(VLOC2,'READ',VPTR2,VDLOC2,STATUS)
              IF (STATUS .NE.SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  VARS = .FALSE.
              ENDIF
              CALL TSP_COMBINE(SIZE,%VAL(SPTR1),%VAL(SPTR2))
              IF (VARS) CALL TSP_COMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))
              CALL TSP_UNMAP(SDLOC2,STATUS)
              IF (VARS) CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(VLOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','V present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'V',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          IF (VARS) CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(VLOC1,STATUS)
      ENDIF

*  sum the intensities

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_COMBINE(SIZE,%VAL(PTR1),%VAL(PTR2))
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(DLOC1,STATUS)
      CALL TSP_UNMAP(DLOC2,STATUS)
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_COMBINE(SIZE,I1,I2)

*   Subroutine to add the data
*

      IMPLICIT NONE
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE)
      INTEGER I

      DO I=1,SIZE
          I1(I)=I1(I)+I2(I)
      ENDDO
      END

