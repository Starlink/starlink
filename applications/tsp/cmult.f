C+
      SUBROUTINE CMULT(STATUS)
C
C            C M U L T
C
C     Command name:
C        CMULT
C
C     Function:
C        Multiply a polarization spectrum by a constant
C
C     Description:
C        The intensity and stokes parameters are multiplied
C        by the specified factor and the variances are multiplied
C        by the square of the specified factor
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be multiplied.
C    (2) FACTOR     (Real)     Factor to multiply by
C    (3) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C          20/11/1990
C
C-
C
C  History:
C    4/12/1988   Original Version.   JAB/AAO
C    20/11/1990  Handle bad values.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS
      INTEGER DPTR
      INTEGER NDIM, DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,DLOC,SLOC
      LOGICAL OK,QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      REAL FACTOR

*  Get the Input data

         CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Get the scale factor

         CALL PAR_GET0R('FACTOR',FACTOR,STATUS)

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get the data size

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
         SIZE = DIMS(1)

*  Map intensity data

         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Scale Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)

*  Scale the variance (twice) if present

         STAT = SAI__OK
         CALL TSP_MAP_VAR(OLOC,'UPDATE',DPTR,DLOC,STAT)
         IF (STAT .EQ. SAI__OK) THEN
             CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
             CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
         ENDIF

*  Scale the Stokes parameters, and variances if present

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)

*  Q Stokes parameter

         IF (QZ) THEN
             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the data and scale it

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Map the variance and scale it (if present)

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  U Stokes parameter

         IF (UZ) THEN
             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map the data and scale it

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)

*  Map the variance and scale it (if present)

             STAT = SAI__OK
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  V Stokes parameter

         IF (VZ) THEN
             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*   Map the data and scale it

             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)

*  Map the variance and scale it  (if present)

             STAT = SAI__OK
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_CMULT(FACTOR,SIZE,%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  annul locators

         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

      END



      SUBROUTINE TSP_CMULT (FACTOR,NX,DATA)
*+
*     T S P _ C M U L T
*
*     Multiply all elements of the array DATA by a constant value
*     given by the parameter FACTOR
*
*     Parameters:
*
*     (>)  FACTOR    (Real)     The constant to multiply by
*     (>)  NX        (Integer)  The number of points in the array
*     (>)  DATA      (Real array (NX))  The data array
*
*     Jeremy Bailey   4/12/1988
*
*     Modified:
*         20/11 1991    Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*     Parameters
      INTEGER NX
      REAL    FACTOR, DATA(NX)

*     Local variables
      INTEGER IX

*     Loop over data points scaling all the good points
      DO IX=1,NX
         IF (DATA(IX) .NE. VAL__BADR) DATA(IX)=DATA(IX)*FACTOR
      END DO

      END

