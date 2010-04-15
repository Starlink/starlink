C+
      SUBROUTINE CALPA(STATUS)
C
C            C A L P A
C
C     Command name:
C        CALPA
C
C     Function:
C        Position Angle Calibrate a Polarization Spectrum
C
C     Description:
C        A polarization spectrum is corrected for wavelength dependent
C        position angle zero point by applying a calibration curve obtained
C        using the CALFITPA command.
C
C        This command is needed for polarimeters which are based on
C        the use of a superachromatic half-wave plate, since such a
C        plate shows significant cyclic  wavelength variations of the
C        position of its fast axis.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The Polarization spectrum to be corrected.
C    (2) CALIB      (TSP, 1D)  The calibration spectrum.
C    (3) OUTPUT     (TSP, 1D)  The output corrected dataset.
C
C     Support:
C        Jeremy Bailey, AAO
C
C     Version date:
C        20/11/1991
C
C-
C
C  History:
C    15/8/1990   Original Version.   JAB/AAO
C    20/11/1991  Handle bad values.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER QPTR1,QPTR2,UPTR1,UPTR2,QVPTR1,UVPTR1

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC1,LOC2,OLOC
      CHARACTER*(DAT__SZLOC) QLOC1,QLOC2,ULOC1,ULOC2,QDLOC1,QDLOC2
      CHARACTER*(DAT__SZLOC) UDLOC1,UDLOC2,QVLOC1,UVLOC1
      INTEGER NDIMS,DIMS(7)
      INTEGER SIZE
      INTEGER NUM

*  Get the input locator

      CALL DAT_ASSOC('INPUT','READ',LOC1,STATUS)

*  Get locator to the calibration frame

      CALL DAT_ASSOC('CALIB','READ',LOC2,STATUS)

*  Create the output file and get locator to it

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Check dimensions of both datasets

      CALL TSP_SIZE(LOC1,7,DIMS,NDIMS,STATUS)

*  Check that the data is one dimensional
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT data',
     :       STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF

*  Check that the calibration is one dimensional
      CALL TSP_SIZE(LOC2,7,DIMS,NDIMS,STATUS)
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for CALIB data',
     :       STATUS)
         STATUS = USER__001
      ENDIF

*  Check that data and calibration are the same size
      IF (DIMS(1) .NE. SIZE) THEN
         CALL MSG_OUT('MSG','Dimensions are different',STATUS)
         STATUS = USER__001
      ENDIF

*  Get Stokes Data from calibration dataset

      CALL TSP_GET_STOKES(LOC2,'Q',QLOC2,STATUS)
      CALL TSP_GET_STOKES(LOC2,'U',ULOC2,STATUS)
      CALL TSP_MAP_DATA(QLOC2,'READ',QPTR2,QDLOC2,STATUS)
      CALL TSP_MAP_DATA(ULOC2,'READ',UPTR2,UDLOC2,STATUS)

*  Get the Stokes data from output dataset

      CALL TSP_GET_STOKES(OLOC,'Q',QLOC1,STATUS)
      CALL TSP_GET_STOKES(OLOC,'U',ULOC1,STATUS)
      CALL TSP_MAP_DATA(QLOC1,'UPDATE',QPTR1,QDLOC1,STATUS)
      CALL TSP_MAP_DATA(ULOC1,'UPDATE',UPTR1,UDLOC1,STATUS)

*  Also get the variances

      CALL TSP_MAP_VAR(QLOC1,'UPDATE',QVPTR1,QVLOC1,STATUS)
      CALL TSP_MAP_VAR(ULOC1,'UPDATE',UVPTR1,UVLOC1,STATUS)

*  Apply the correction

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_CALPA(SIZE,%VAL(QPTR2),%VAL(UPTR2),%VAL(QPTR1),
     :       %VAL(UPTR1),%VAL(QVPTR1),%VAL(UVPTR1))
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(QDLOC2,STATUS)
      CALL TSP_UNMAP(UDLOC2,STATUS)
      CALL DAT_ANNUL(QLOC2,STATUS)
      CALL DAT_ANNUL(ULOC2,STATUS)
      CALL TSP_UNMAP(QDLOC1,STATUS)
      CALL TSP_UNMAP(UDLOC1,STATUS)
      CALL TSP_UNMAP(QVLOC1,STATUS)
      CALL TSP_UNMAP(UVLOC1,STATUS)
      CALL DAT_ANNUL(QLOC1,STATUS)
      CALL DAT_ANNUL(ULOC1,STATUS)
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_CALPA(SIZE,QC,UC,Q,U,QV,UV)
*+
*   Subroutine to apply the correction to the data. This is done by
*   rotating the polarization vector by the appropriate amount for the
*   corresponding wavelength as given by the calibration array
*
*   (>)  SIZE  (Integer)   Size of stokes and calibration arrays
*   (>)  QC    (Real array (SIZE))  Q Stokes parameter of calibration array
*   (>)  UC    (Real array (SIZE))  U Stokes parameter of calibration array
*   (!)  Q     (Real array (SIZE))  Q Stokes parameter to be calibrated
*   (!)  U     (Real array (SIZE))  U Stokes parameter to be calibrated
*   (!)  QV    (Real array (SIZE))  Variance on Q Stokes array
*   (!)  UV    (Real array (SIZE))  Variance on U Stokes array
*
*   Jeremy Bailey   29/4/1988
*
*   Modified:
*      20/11/1991  -  Support bad values
*
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL QC(SIZE),UC(SIZE),Q(SIZE),U(SIZE),QV(SIZE),UV(SIZE)

*  Local variables
      INTEGER I
      REAL QQ,UU,QQV,UUV

*  Loop over points
      DO I=1,SIZE

*  Rotate the polarization vector by an angle determined by the
*  calibration vector
          QQ = QC(I)*Q(I) + UC(I)*U(I)
          UU = QC(I)*U(I) - UC(I)*Q(I)

*  The variances are set to the mean of the values for the two original
*  Stokes parameters
          QQV = 0.5*(QV(I) + UV(I))
          UUV = QQV

*  Put values back into data array
          Q(I) = QQ
          U(I) = UU
          QV(I) = ABS(QQV)
          UV(I) = ABS(UUV)
      ENDDO
      END

