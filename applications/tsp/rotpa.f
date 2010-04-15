C+
      SUBROUTINE ROTPA(STATUS)
C
C            R O T P A
C
C     Command name:
C        ROTPA
C
C     Function:
C        Rotate the Position Angle of a Polarization Dataset
C
C     Description:
C        Rotate the position angle of a polarization Dataset
C        through a specified amount
C
C        This propgram can be used to correct the position angle
C        for a constant (wavelength independent) calibration errror
C
C     Parameters:
C    (1) INPUT      (TSP, nD)  The Polarization dataset to be corrected.
C    (2) THETA      (Real)     Angle to rotate through (degrees).
C    (3) OUTPUT     (TSP, nD)  The output corrected dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         8/5/1993
C
C-
C
C  History:
C    30/8/1990   Original Version.   JAB/AAO
C    8/5/1993    Make it handle data of 1/2 or 3 dimensions.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER QPTR1,UPTR1,QVPTR1,UVPTR1

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC1,OLOC
      CHARACTER*(DAT__SZLOC) QLOC1,ULOC1,QDLOC1
      CHARACTER*(DAT__SZLOC) UDLOC1,QVLOC1,UVLOC1

*  Array dimensions
      INTEGER NDIMS,DIMS(7)

*  Data size
      INTEGER SIZE
      INTEGER NUM

*  Position angle correction
      REAL THETA

*  Get the input and output locators

      CALL DAT_ASSOC('INPUT','READ',LOC1,STATUS)

*  Get correction angle

      CALL PAR_GET0R('THETA',THETA,STATUS)

*  Create the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Get dimensions of data array

      CALL TSP_SIZE(LOC1,7,DIMS,NDIMS,STATUS)

*  Determine size of data arrays
      IF (NDIMS .EQ. 3) THEN
         SIZE = DIMS(1)*DIMS(2)*DIMS(3)
      ELSE IF (NDIMS .EQ. 2) THEN
         SIZE = DIMS(1)*DIMS(2)
      ELSE IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT data',
     :       STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF

*  Get the Stokes data from output dataset and correct it

*  Find the Stokes parameters
      CALL TSP_GET_STOKES(OLOC,'Q',QLOC1,STATUS)
      CALL TSP_GET_STOKES(OLOC,'U',ULOC1,STATUS)

*  Map the Q stokes data
      CALL TSP_MAP_DATA(QLOC1,'UPDATE',QPTR1,QDLOC1,STATUS)

*  Map the U stokes data
      CALL TSP_MAP_DATA(ULOC1,'UPDATE',UPTR1,UDLOC1,STATUS)

*  Map the Q stokes variance
*     CALL TSP_MAP_VAR(ULOC1,'UPDATE',UVPTR1,UVLOC1,STATUS)

*  If everything OK do the position angle rotation
      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_ROTPA(SIZE,THETA,%VAL(QPTR1),
     :       %VAL(UPTR1))
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(QDLOC1,STATUS)
      CALL TSP_UNMAP(UDLOC1,STATUS)
      CALL DAT_ANNUL(QLOC1,STATUS)
      CALL DAT_ANNUL(ULOC1,STATUS)
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_ROTPA(SIZE,THETA,Q,U)
*+
*
*   T S P _ R O T P A
*
*   ROTPA command
*
*   Subroutine to rotate the position angle of the Stokes data through
*   THETA degrees
*
*   Parameters:
*
*   (>)  SIZE      (Integer)           Number of points in spectrum
*   (>)  THETA     (Real)              Angle to rotate through
*   (!)  Q         (Real array(SIZE))  Q stokes array
*   (!)  U         (Real array(SIZE))  U stokes array
*   (!)  QV        (Real array(SIZE))  Q stokes variance
*   (!)  UV        (Real array(SIZE))  U stokes variance
*
*   Jeremy Bailey   30/8/1990
*
*   Modified:
*       16/12/1991   -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL THETA,Q(SIZE),U(SIZE)

*  Local variables
      INTEGER I
      REAL QQ,UU,QQV,UUV,QC,UC
      REAL DEGRAD
      DEGRAD = 45.0/ATAN(1.0)

*  Form elements of rotation matrix
      QC = COS(2.0*THETA/DEGRAD)
      UC = SIN(2.0*THETA/DEGRAD)

*  Loop over points
      DO I=1,SIZE
        IF (Q(I) .NE. VAL__BADR .AND. U(I) .NE. VAL__BADR) THEN

*  Rotate Stokes vector
          QQ = QC*Q(I) + UC*U(I)
          UU = QC*U(I) - UC*Q(I)

*  Return values to data arrays
          Q(I) = QQ
          U(I) = UU
        ENDIF
      ENDDO
      END

