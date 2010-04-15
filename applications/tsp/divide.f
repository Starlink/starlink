C+
      SUBROUTINE DIVIDE(STATUS)
C
C            D I V I D E
C
C     Command name:
C        DIVIDE
C
C     Function:
C        Divide a polarization spectrum by an intensity spectrum
C
C     Description:
C        Divide a polarization spectrum by the intensity spectrum from another
C        dataset. This can be used to divide data by a smooth spectrum star
C        to remove atmospheric features.
C
C        The intensity and Stokes parameters of the first spectrum are
C        divided by the intensity of the second spectrum. The variances are
C        also scaled accordingly. The spectrum being divided by is assumed
C        to have no errors.
C
C     Parameters:
C    (1) INPUT1     (TSP, 1D)  The input spectrum to be divided.
C    (2) INPUT2     (TSP, 1D)  The spectrum to divide by.
C    (3) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         5/12/1991
C
C-
C
C  History:
C    19/11/1990   Original Version.   JAB/AAO
C     5/12/1991   Handle bad values.  JAB/AAO
C


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER XPTR,CPTR,DPTR,DEPTR,CEPTR
      INTEGER DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,DLOC,SLOC,CLOC,CDLOC
      CHARACTER*(DAT__SZLOC) CELOC,DELOC
      LOGICAL OK,QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      LOGICAL ERROR1,ERROR2

*  Get the Input data

      CALL DAT_ASSOC('INPUT1','READ',ILOC,STATUS)

*  Access the calibration frame

      CALL DAT_ASSOC('INPUT2','READ',CLOC,STATUS)

*  Get the data size

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_SIZE(CLOC,7,DIMS,ACTDIM,STATUS)
         SIZE = DIMS(1)

*  Map calibration data array

         CALL TSP_MAP_DATA(CLOC,'READ',CPTR,CDLOC,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             CALL MSG_OUT(' ','Error Mapping calibration data',STATUS)
             STATUS = USER__001
             GOTO 100
         ENDIF

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input data to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Check sizes of input and calibration match

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
         IF (DIMS(1) .NE. SIZE) THEN
              CALL MSG_OUT('MSG','Wavelength Axes are different Sizes',
     :           STATUS)
              STATUS = USER__001
         ENDIF

*  Map the data

         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Check for errors

         IF (STATUS .NE. SAI__OK) THEN
             CALL MSG_OUT(' ','Error Mapping data',STATUS)
             STATUS = USER__001
             GOTO 500
         ENDIF

*  Map the variance array if there is one, otherwise map a dummy array

         CALL TSP_MAP_VAR(OLOC,'UPDATE',DEPTR,DELOC,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             STATUS = SAI__OK
             CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
             ERROR1 = .FALSE.
         ELSE
             ERROR1 = .TRUE.
         ENDIF

*  Calibrate Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_DIVIDE(SIZE,%VAL(DPTR),%VAL(DEPTR),%VAL(CPTR))
         ENDIF

*  Unmap the arrays

         CALL TSP_UNMAP(DELOC,STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)

*  Calibrate the Stokes parameters, and variances if present

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)

*  Q Stokes parameter

         IF (QZ) THEN
             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map Q data and variance if present

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*  Do the division

             IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_DIVIDE(SIZE,%VAL(DPTR),%VAL(DEPTR),%VAL(CPTR))
             ENDIF

*  Unmap the data

             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  U Stokes parameter

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map U data and variance if present

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*  Do the division

             IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_DIVIDE(SIZE,%VAL(DPTR),%VAL(DEPTR),%VAL(CPTR))
             ENDIF

*  Unmap data

             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  V Stokes parameter

             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map V data and variance if present

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*  Do the division

             IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_DIVIDE(SIZE,%VAL(DPTR),%VAL(DEPTR),%VAL(CPTR))
             ENDIF

*  Unmap the data

             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Unmap arrays and annul locators

500      CONTINUE
         CALL TSP_UNMAP(CDLOC,STATUS)
         CALL DAT_ANNUL(CLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

      ENDIF
100   CONTINUE
      END


      SUBROUTINE TSP_DIVIDE(SIZE,I,IE,C)
*+
*     T S P _ D I V I D E
*
*     Divide an array by a calibration array and divide its variance by the
*     square of the calibration array
*
*     (>)  SIZE     (Integer)   The size of the arrays
*     (>)  I        (Real array(SIZE))  The data array to be divided
*     (>)  IE       (Real array(SIZE))  The variance on the data
*     (>)  C        (Real array(SIZE))  The calibration array to divide by
*
*     Jeremy Bailey   19/11/1990
*
*     Modified:
*         5/12/1991   Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I(SIZE),IE(SIZE),C(SIZE)

*  Local variable
      INTEGER J
      DO J=1,SIZE
          IF (I(J) .NE. VAL__BADR) THEN
              I(J) = I(J)/C(J)
              IE(J) = IE(J)/(C(J)*C(J))
              IF (IE(J) .LT. 0.0) IE(J) = 0.0
          ENDIF
      ENDDO
      END
