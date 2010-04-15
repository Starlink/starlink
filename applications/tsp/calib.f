C+
      SUBROUTINE CALIB(STATUS)
C
C            C A L I B
C
C     Command name:
C        CALIB
C
C     Function:
C        Efficiency Calibrate a Polarization Spectrum
C
C     Description:
C        A polarization spectrum is corrected for instrument efficiency
C        by applying a calibration curve obtained using the CALFIT
C        command. The spectrum to be corrected may have any number
C        of Stokes Parameters.
C
C        CALIB leaves the intensity data unafected, but scales the
C        Stokes parameters according to the calibration curve, and the
C        variances of the Stokes parameters by the square of the calibration
C        value.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The Polarization spectrum to be corrected.
C    (2) CALIB      (TSP, 1D)  The calibration spectrum.
C    (3) OUTPUT     (TSP, 1D)  The output corrected dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 19/11/1991
C
C-
C
C  History:
C    29/4/1988   Original Version.   JAB/AAO
C    19/11/1991  Support bad values. JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  Data pointers
      INTEGER SPTR1,SPTR2,VPTR1

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC1,LOC2,OLOC
      CHARACTER*(DAT__SZLOC) SLOC1,SLOC2,SDLOC1,SDLOC2,VDLOC1
      INTEGER NDIMS,DIMS(7),SIZE,NUM
      LOGICAL QZ,UZ,VZ

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC1,STATUS)

*  and the calibration file
      CALL DAT_ASSOC('CALIB','READ',LOC2,STATUS)

*  Create the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Check dimensions of both datasets

      CALL TSP_SIZE(LOC1,7,DIMS,NDIMS,STATUS)

*  Check that the first dataset is one dimensional
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT data',
     :       STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF
      CALL TSP_SIZE(LOC2,7,DIMS,NDIMS,STATUS)

*  Check that the second dataset is 1 dimensional
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for CALIB data',
     :       STATUS)
         STATUS = USER__001
      ENDIF

*  Check that the data and calibration have the same size
      IF (DIMS(1) .NE. SIZE) THEN
         CALL MSG_OUT('MSG','Dimensions are different',STATUS)
         STATUS = USER__001
      ENDIF

*  Get Stokes Data from calibration dataset - whichever stokes
*  parameter is the first to be found in the sequence Q,U,V

      CALL TSP_STOKES(LOC2,NUM,QZ,UZ,VZ,STATUS)
      IF (QZ) THEN
          CALL TSP_GET_STOKES(LOC2,'Q',SLOC2,STATUS)
      ELSE IF (UZ) THEN
          CALL TSP_GET_STOKES(LOC2,'U',SLOC2,STATUS)
      ELSE IF (VZ) THEN
          CALL TSP_GET_STOKES(LOC2,'V',SLOC2,STATUS)
      ELSE

*  Error if there is no Stokes data
          CALL MSG_OUT('MSG','No Stokes Parameter Found',STATUS)
          STATUS = USER__001
      ENDIF

*  Map the Stokes parameter
      CALL TSP_MAP_DATA(SLOC2,'READ',SPTR2,SDLOC2,STATUS)

*  Get the Stokes data from output dataset and correct it

      CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
      IF (QZ) THEN

*  Map the Q stokes parameter and variance - if present
          CALL TSP_GET_STOKES(OLOC,'Q',SLOC1,STATUS)
          CALL TSP_MAP_DATA(SLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(SLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)

*  Calibrate the Q data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_CALIB(SIZE,%VAL(SPTR1),%VAL(VPTR1),%VAL(SPTR2))
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(SLOC1,STATUS)
      ENDIF
      IF (UZ) THEN

*  Map the U stokes parameter and variance - if present

          CALL TSP_GET_STOKES(OLOC,'U',SLOC1,STATUS)
          CALL TSP_MAP_DATA(SLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(SLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)

*  Calibrate the U data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_CALIB(SIZE,%VAL(SPTR1),%VAL(VPTR1),%VAL(SPTR2))
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(SLOC1,STATUS)
      ENDIF
      IF (VZ) THEN

*  Map the V stokes parameter and variance - if present
          CALL TSP_GET_STOKES(OLOC,'V',SLOC1,STATUS)
          CALL TSP_MAP_DATA(SLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)
          CALL TSP_MAP_VAR(SLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)

*  Calibrate the V data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_CALIB(SIZE,%VAL(SPTR1),%VAL(VPTR1),%VAL(SPTR2))
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(SLOC1,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(SDLOC2,STATUS)
      CALL DAT_ANNUL(SLOC2,STATUS)
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_CALIB(SIZE,S1,V1,C)
*+
*   Subroutine to apply the correction to the data. This is done simply by
*   scaling the Stokes parameter by the efficiency factor for the
*   corresponding wavelength in the calibration array
*
*   (>)  SIZE  (Integer)   Size of stokes and calibration arrays
*   (!)  S1    (Real array (SIZE)  Stokes array to be calibrated
*   (!)  V1    (Real array (SIZE)  Variance on Stokes array
*   (>)  C     (Real array (SIZE)  Calibration array
*
*   Jeremy Bailey   29/4/1988
*
*   Modified:
*      19/11/1991  -  Support bad values
*
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL S1(SIZE),V1(SIZE),C(SIZE)

*  Local variables
      REAL CC
      INTEGER I

*  Loop over data points
      DO I=1,SIZE

*  Invert the calibration if the values are negative at the centre
          IF (C(SIZE/2) .LT. 0.0) THEN
              CC = -C(I)
          ELSE
              CC = C(I)
          ENDIF

*  If the data is good, scale the stokes parameter and its variance
          IF (S1(I) .NE. VAL__BADR) THEN
              S1(I) = S1(I)/CC
              V1(I) = V1(I)/(CC*CC)
          ENDIF
      ENDDO
      END

