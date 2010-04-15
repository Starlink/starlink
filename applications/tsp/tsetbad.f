C+
      SUBROUTINE TSETBAD(STATUS)
C
C            T S E T B A D
C
C     Command Name:
C        TSETBAD
C
C     Function:
C        Interactively mark bad points in time series
C
C     Description:
C        Mark points in a time series as bad by specifying the channel
C        number and time point number. The intensity and Stokes parameter
C        values for the selected data points are flagged with a bad value
C        which will be ignored by subsequent applications.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset
C    (2) OUTPUT     (TSP, 2D)  The output dataset with
C                               bad points flagged.
C        CHAN       (Integer)  Channel number of point.
C        X          (Integer)  Time bin number of point.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         1/3/1990
C
C-
C
C  History:
C    1/3/1990   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,LOC,QSLOC,USLOC,VSLOC
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,VLOC,TLOC
      INTEGER XSIZE,CHANS,CHAN,X
      INTEGER DIMS(7),NDIMS

*  Data pointers
      INTEGER PTR,QPTR,UPTR,VPTR,TPTR
      INTEGER NSTRT

*  Get Locators to the input dataset
      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Create the output dataset
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(OLOC,7,DIMS,NDIMS,STATUS)
      XSIZE = DIMS(NDIMS)
      CHANS = DIMS(1)

*  Check that it is 2 dimensional
      IF (NDIMS .NE. 2) THEN
          CALL MSG_OUT(' ','Not a 2D dataset',STATUS)
          GOTO 500
      ENDIF

*  Create temporary array
      CALL TSP_TEMP(XSIZE*CHANS,'_REAL',TPTR,TLOC,STATUS)

*  Map the intensity and stokes data of the output data set

*  Map Intensity data
      CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR,LOC,STATUS)

*  Map Q stokes parameter - if it does not exist use the temporary array
      CALL TSP_GET_STOKES(OLOC,'Q',QSLOC,STATUS)
      CALL TSP_MAP_DATA(QSLOC,'UPDATE',QPTR,QLOC,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__OK
          QPTR = TPTR
      ENDIF

*  Map U stokes parameter - if it does not exist use the temporary array
      CALL TSP_GET_STOKES(OLOC,'U',USLOC,STATUS)
      CALL TSP_MAP_DATA(USLOC,'UPDATE',UPTR,ULOC,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__OK
          UPTR = TPTR
      ENDIF

*  Map V stokes parameter - if it does not exist use the temporary array
      CALL TSP_GET_STOKES(OLOC,'V',VSLOC,STATUS)
      CALL TSP_MAP_DATA(VSLOC,'UPDATE',VPTR,VLOC,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__OK
          VPTR = TPTR
      ENDIF

*  Loop over channels

      CHAN = 1
      DO WHILE (CHAN .NE. 0)
          IF (CHAN .LT. 0 .OR. CHAN .GT. CHANS) THEN
              CALL MSG_OUT(' ','No such channel in dataset',STATUS)
          ENDIF

*  Get a channel number
          CALL PAR_GET0I('CHAN',CHAN,STATUS)
          IF (STATUS .NE. SAI__OK) CHAN = 0
          CALL PAR_CANCL('CHAN',STATUS)
          IF (CHAN .GE. 1 .AND. CHAN .LE. CHANS) THEN

*  loop over X values
              X = 1
              DO WHILE (X .NE. 0)
                  IF (X .LT. 0 .OR. X .GT. XSIZE) THEN
                      CALL MSG_OUT(' ','Not in dataset',STATUS)
                  ENDIF

*  Get an X value
                  CALL PAR_GET0I('X',X,STATUS)
                  IF (STATUS .NE. SAI__OK) X = 0
                  CALL PAR_CANCL('X',STATUS)

*  Set a pixel to bad
                  IF (X .GE. 1 .AND. X .LE. XSIZE) THEN
                      CALL TSP_TSETBAD(CHANS,XSIZE,CHAN,X,%VAL(PTR),
     :                  %VAL(QPTR),%VAL(UPTR),%VAL(VPTR))
                  ENDIF
              ENDDO
          ENDIF
      ENDDO

*  Unmap arrays
      CALL TSP_UNMAP(LOC,STATUS)
      CALL TSP_UNMAP(QLOC,STATUS)
      CALL TSP_UNMAP(ULOC,STATUS)
      CALL TSP_UNMAP(VLOC,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
500   CONTINUE
      CALL DAT_ANNUL(QSLOC,STATUS)
      CALL DAT_ANNUL(USLOC,STATUS)
      CALL DAT_ANNUL(VSLOC,STATUS)
      IF (STATUS .NE. SAI__OK) STATUS = SAI__OK
      CALL DAT_ANNUL(ILOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END



      SUBROUTINE TSP_TSETBAD(IX,JX,I,J,X,Q,U,V)
*+
*
*  T S P _ T S E T B A D
*
*  TSETBAD command
*
*    Set the specified pixel to a bad value in the intensity and stokes arrays
*
*  Parameters:
*
*  (>)  IX   (Integer)               First dimension of arrays
*  (>)  JX   (Integer)               Second dimension of arrays
*  (>)  I    (Integer)               First index of pixel to set bad
*  (>)  J    (Integer)               Second index of pixel to set bad
*  (>)  X    (Real array(IX,JX))     Intensity array
*  (>)  Q    (Real array(IX,JX))     Q stokes parameter array
*  (>)  U    (Real array(IX,JX))     U stokes parameter array
*  (>)  V    (Real array(IX,JX))     V stokes parameter array
*
*   Jeremy Bailey     1/3/1990
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER IX,JX,I,J
      REAL X(IX,JX),Q(IX,JX),U(IX,JX),V(IX,JX)

*  Set intensity and Stokes parameters to bad
      X(I,J) = VAL__BADR
      Q(I,J) = VAL__BADR
      U(I,J) = VAL__BADR
      V(I,J) = VAL__BADR

      END
