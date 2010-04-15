C+
      SUBROUTINE TCADD(STATUS)
C
C            T C A D D
C
C     Command name:
C        TCADD
C
C     Function:
C        Add Channels of a time series dataset
C
C     Description:
C        TCADD adds a range of channels of a time series dataset
C        to produce a 1-dimensional output array. The number of channels
C        can be one so it can be used to extract a single channel.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C        FIRST      (Integer)  First channel to extract.
C        LAST       (Integer)  Last channel to extract.
C        OUTPUT     (TSP, 1D)  The output binned dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         26/2/1988
C
C-
C
C  History:
C    5/12/1993   Original Version.   JAB/AAO
C


      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,DLOC,SLOC,ODLOC,OSLOC

*  Array sizes
      INTEGER SIZE,CHANS
      INTEGER DIMS(7),NDIMS

*  Pointers
      INTEGER OPTR,DPTR


      LOGICAL QZ,UZ,VZ
      INTEGER FIRST,LAST
      INTEGER NUM

*  Get Locators to the input and output datasets

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of data

      CALL TSP_SIZE(OLOC,7,DIMS,NDIMS,STATUS)
      SIZE = DIMS(NDIMS)
      IF (NDIMS .EQ. 3) THEN
         CHANS = DIMS(1)*DIMS(2)
      ELSE IF (NDIMS .EQ. 2) THEN
         CHANS = DIMS(1)
      ELSE
         CALL MSG_OUT(' ','Data has incorrect number of dimensions',
     :             STATUS)
         GOTO 500
      ENDIF

      DIMS(1)=1
      CALL TSP_RESIZE(OLOC,2,DIMS,STATUS)

      CALL PAR_GET0I('FIRST',FIRST,STATUS)
      CALL PAR_GET0I('LAST',LAST,STATUS)

*  Map intensity data

         CALL TSP_MAP_DATA(ILOC,'READ',DPTR,DLOC,STATUS)
         CALL TSP_MAP_DATA(OLOC,'UPDATE',OPTR,ODLOC,STATUS)

*  Correct Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,%VAL(OPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(ODLOC,STATUS)

*  Correct variance data

         CALL TSP_MAP_VAR(ILOC,'READ',DPTR,DLOC,STATUS)
         CALL TSP_MAP_VAR(OLOC,'UPDATE',OPTR,ODLOC,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,%VAL(OPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL TSP_UNMAP(ODLOC,STATUS)
         ELSE
             CALL ERR_ANNUL(STATUS)
         ENDIF

*  Scale the Stokes parameters, and variances if present

*  Find stokes parameters present in the data

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*  Get Q data and correct it

             CALL TSP_GET_STOKES(ILOC,'Q',SLOC,STATUS)
             CALL TSP_GET_STOKES(OLOC,'Q',OSLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_DATA(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :              %VAL(OPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL TSP_UNMAP(ODLOC,STATUS)
*  Get Q variance and correct it

             CALL TSP_MAP_VAR(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :               %VAL(OPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
                 CALL TSP_UNMAP(ODLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
             CALL DAT_ANNUL(OSLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  Get U data and correct it

             CALL TSP_GET_STOKES(ILOC,'U',SLOC,STATUS)
             CALL TSP_GET_STOKES(OLOC,'U',OSLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_DATA(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :              %VAL(OPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL TSP_UNMAP(ODLOC,STATUS)
*  Get U variance and correct it

             CALL TSP_MAP_VAR(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :               %VAL(OPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
                 CALL TSP_UNMAP(ODLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
             CALL DAT_ANNUL(OSLOC,STATUS)

         ENDIF
         IF (VZ) THEN

*  Get U data and correct it

             CALL TSP_GET_STOKES(ILOC,'U',SLOC,STATUS)
             CALL TSP_GET_STOKES(OLOC,'U',OSLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_DATA(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :              %VAL(OPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL TSP_UNMAP(ODLOC,STATUS)
*  Get U variance and correct it

             CALL TSP_MAP_VAR(SLOC,'READ',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(OSLOC,'UPDATE',OPTR,ODLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TCADD(SIZE,CHANS,%VAL(DPTR),FIRST,LAST,
     :               %VAL(OPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
                 CALL TSP_UNMAP(ODLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
             CALL DAT_ANNUL(OSLOC,STATUS)

         ENDIF


500   CONTINUE
      CALL DAT_ANNUL(ILOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      IF (STATUS .EQ. USER__001) STATUS = SAI__OK
      END



      SUBROUTINE TSP_TCADD (NX,NC,DATA,FIRST,LAST,OUT)
C+
C
C     T S P _ T C A D D
C
C     Add channels FIRST to LAST of a time series data set to produce
C     a single channel output dataset.
C
C     (>)  NX   (Integer)        Number of points in time series
C     (>)  NC   (Integer)        Number of channels
C     (>)  DATA (Real array(NC,NX)) Array of data to be summed
C     (>)  FIRST (Integer)       First channel to add
C     (>)  LAST  (Integer)       Last channel to add
C     (<)  OUT   (Real array(NX))  Output array
C
C     Jeremy Bailey   5/12/1993
C
C     Modified:
C
C+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      INTEGER NX,NC
      REAL OUT(NX),DATA(NC,NX)
      INTEGER FIRST,LAST
C
C     Local variables
C
      INTEGER IX,IC
      REAL X

      DO IX=1,NX
        X = 0.0
        DO IC=FIRST,LAST
          IF (DATA(IC,IX) .NE. VAL__BADR) THEN
             X = X + DATA(IC,IX)

          ELSE
             X = VAL__BADR
          ENDIF
        ENDDO
        OUT(IX) = X
      END DO
C
      END



