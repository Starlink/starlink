C+
      SUBROUTINE RED3_CGS3POL(STATUS)
C
C            R E D 3 _ C G S 3 P O L
C
C     Command name:
C        RED3_CGS3POL
C
C     Function:
C        Reduce CGS3 spectropolarimetry data.
C
C     Description:
C        RED3_CGS3POL reduces data obtained with the CGS3 instrument at UKIRT
C        used in its spectropolarimetry mode. After initial reduction by
C        the CGS3 DR system the data for a single observation consists
C        of four Figaro files containing the frames for plate position
C        0, 45, 22.5 and 67.5 degrees. Each frame contains a 1-D spectrum.
C        These spectra are combined to derive a polarization spectrum in
C        TSP format.
C
C
C     Parameters:
C    (1) POS1       (Char)     The Figaro data file for position 0.0.
C    (2) POS2       (Char)     The Figaro data file for position 45.0.
C    (3) POS3       (Char)     The Figaro data file for position 22.5.
C    (4) POS4       (Char)     The Figaro data file for position 67.5.
C        OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support: Alan Bridger JAC
C
C     Version date:  9/12/1992
C
C-
C
C  History:
C      9/12/92 Original version, taken unashamedly from CGS4POL
C    07-Mar-1996: put into red3 task, not tsp
C    14-Mar-1996: add calls to ndf_begin and _end
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Status argument
      INTEGER STATUS
      INTEGER CSTATUS

*  Data pointers
      INTEGER IPTR,XPTR,IPTR2,IPTR3,IPTR4
      INTEGER IEPTR,IEPTR2,IEPTR3,IEPTR4
      INTEGER OIPTR,OVPTR,UEPTR,UPTR,QEPTR,QPTR,OXPTR,T1PTR,T2PTR,
     :        TEPTR1,TEPTR2

*  Number of elements in data arrays
      INTEGER NDIM, DIMS(7),DIMS2(7)

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,VLOC,OLOC,ULOC,UELOC,QLOC,QELOC,
     :    SLOC,OXLOC,T1LOC,T2LOC,TE1LOC,TE2LOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS
      INTEGER ISLOT,ISLOT2,ISLOT3,ISLOT4,DUMMY
      INTEGER IESLOT,IESLOT2,IESLOT3,IESLOT4
      CHARACTER*64 STRINGS(2)
      INTEGER ELEMENTS,XSLOT

*  Access the First Figaro frame
      CALL DSA_OPEN(STATUS)
      CALL PAR_GET0C('POS1',FNAME,STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Error accessing frame 1',STATUS)
         GOTO 100
      ENDIF

*  Get the data array
      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_DATA_SIZE('INPUT',1,NDIM,DIMS,ELEMENTS,STATUS)

*  Check that it is one dimensional
         IF (NDIM .NE. 1) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Dimensions of Input File Invalid',
     :           STATUS)
            GOTO 100
         ELSE

*  Map the data and errors
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,STATUS)
            CALL DSA_MAP_VARIANCE('INPUT','READ','FLOAT',IEPTR,IESLOT,
     :       STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get the label and units
            CALL DSA_GET_DATA_INFO('INPUT',2,STRINGS,1,DUMMY,STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
                LABEL = STRINGS(2)
                UNITS = STRINGS(1)
            ELSE
                LABEL = ' '
                UNITS = ' '
            ENDIF

*  Map axis data if it exists
            CALL DSA_MAP_AXIS_DATA('INPUT',1,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  Ang get its label and units
            CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Access the Second Figaro frame
             CALL PAR_GET0C('POS2',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT2',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','Error accessing frame 2',STATUS)
               GOTO 100
             ENDIF

*  Get the data array
             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT2',1,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that its dimensions match those of the previous frame
                IF (NDIM.NE.1 .OR. DIMS2(1).NE.DIMS(1)) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP(' ','Dimensions of Input File Invalid',
     :                 STATUS)
                  GOTO 100
                ENDIF

*  Map the data
                CALL DSA_MAP_DATA('INPUT2','READ','FLOAT',IPTR2,ISLOT2,
     :                STATUS)
                CALL DSA_MAP_VARIANCE('INPUT','READ','FLOAT',IEPTR2,
     :           IESLOT2, STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Access the Third Figaro frame
             CALL PAR_GET0C('POS3',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT3',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','Error accessing frame 3',STATUS)
               GOTO 100
             ENDIF

*  Get the data array
             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT3',1,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that its dimensions match those of the previous frames
                IF (NDIM.NE.1 .OR. DIMS2(1).NE.DIMS(1)) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP(' ','Dimensions of Input File Invalid',
     :                 STATUS)
                  GOTO 100
                ENDIF

*  Map the data
                CALL DSA_MAP_DATA('INPUT3','READ','FLOAT',IPTR3,ISLOT3,
     :                STATUS)
                CALL DSA_MAP_VARIANCE('INPUT','READ','FLOAT',IEPTR3,
     :           IESLOT3, STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Access the Fourth Figaro frame
             CALL PAR_GET0C('POS4',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT4',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','Error accessing frame 4',STATUS)
               GOTO 100
             ENDIF

*  Get the data array
             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT4',1,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that the dimensions match those of the previous frames
                IF (NDIM.NE.1 .OR. DIMS2(1).NE.DIMS(1)) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP(' ','Dimensions of Input File Invalid',
     :                 STATUS)
                  GOTO 100
                ENDIF

*  Map the data
                CALL DSA_MAP_DATA('INPUT4','READ','FLOAT',IPTR4,ISLOT4,
     :                STATUS)
                CALL DSA_MAP_VARIANCE('INPUT','READ','FLOAT',IEPTR4,
     :           IESLOT4, STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   STATUS = SAI__ERROR
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Create the output file
            CALL NDF_BEGIN	! apparently necessary, and ndf_end later

            CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
            CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure
             CALL TSP_CREATE_1D(OLOC,DIMS(1),'QU',.FALSE.,
     :            .TRUE.,STATUS)

*  Map axis array
             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',OXPTR,OXLOC,STATUS)

*  Write label and units into it (from input array)
             CALL TSP_WLU_LAMBDA(OLOC,XLABEL,XUNITS,STATUS)

*  Get Stokes Structure
             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q Stokes data and its variance
             CALL TSP_MAP_DATA(SLOC,'WRITE',QPTR,QLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'WRITE',QEPTR,QELOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Map the U Stokes data and its variance
             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'WRITE',UPTR,ULOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'WRITE',UEPTR,UELOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Write the data label and units
             CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Map the intensity data and variance
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)
             CALL TSP_MAP_VAR(OLOC,'WRITE',OVPTR,VLOC,STATUS)

*  Get workspace arrays
             CALL TSP_TEMP(DIMS(1),'_REAL',T1PTR,T1LOC,STATUS)
             CALL TSP_TEMP(DIMS(1),'_REAL',T2PTR,T2LOC,STATUS)
             CALL TSP_TEMP(DIMS(1),'_REAL',TEPTR1,TE1LOC,STATUS)
             CALL TSP_TEMP(DIMS(1),'_REAL',TEPTR2,TE2LOC,STATUS)

*  Reduce the data (This is equivalent to doing CCD2STOKES on each
*  pair and then QUMERGE).
             IF (STATUS .EQ. SAI__OK) THEN

*  Call CGS3STOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter
                    CALL TSP_CGS3STOKES(DIMS(1),%val(IPTR),
     :                %val(IPTR2),%val(IEPTR),%val(IEPTR2),%val(T1PTR),
     :                %val(TEPTR1),%val(UPTR),%val(UEPTR),
     :                %val(XPTR),%val(OXPTR))

*  Call CGS3STOKES for the second pair of frames (22.5 and 67.5) and
*  derive the Q stokes parameter
                    CALL TSP_CGS3STOKES(DIMS(1),%val(IPTR3),
     :                %val(IPTR4),%val(IEPTR3),%val(IEPTR4),%val(T2PTR),
     :                %val(TEPTR2),%val(QPTR),%val(QEPTR),
     :                %val(XPTR),%val(OXPTR))

*  Merge the Q and U data
                CALL TSP_CGS3POLMERGE(DIMS(1),%val(T1PTR),%val(T2PTR),
     :           %val(TEPTR1),%val(TEPTR2),%val(OIPTR),%val(OVPTR),
     :           %val(QPTR),%val(UPTR),%val(QEPTR),%val(UEPTR),STATUS)
             ENDIF

*  Unmap output arrays and annul locators
             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(VLOC,STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(QELOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL TSP_UNMAP(UELOC,STATUS)
             CALL TSP_UNMAP(OXLOC,STATUS)
             CALL TSP_UNMAP(T1LOC,STATUS)
             CALL TSP_UNMAP(T2LOC,STATUS)
             CALL TSP_UNMAP(TE1LOC,STATUS)
             CALL TSP_UNMAP(TE2LOC,STATUS)

             CALL DAT_UPDAT('OUTPUT',STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)
             CALL DAT_CANCL('OUTPUT',STATUS)

          ENDIF

*  Unmap input arrays
      ENDIF
100   CONTINUE
      CSTATUS = SAI__OK
      CALL DSA_CLOSE(CSTATUS)
      CALL NDF_END(CSTATUS)	! close the file down for plotting
      END

      SUBROUTINE TSP_CGS3STOKES(NX,I1,I2,IV1,IV2,INT,INTV,
     : STOKES,VSTOKES,IX,OX)
*+
*
*   Reduce CGS3 spectropolarimetry
*

*   (>)  NX      (Integer)  Number of spectral channels
*   (>)  I1      (Real array(NX)) First input array
*   (>)  I2      (Real array(NX)) Second input array
*   (>)  IV1     (Real array(NX)) Variance of First input array
*   (>)  IV1     (Real array(NX)) Variance of First input array
*   (<)  INT     (Real array(NX)) Output intensity
*   (<)  INTV    (Real array(NX)) Output variance
*   (<)  STOKES  (Real array(NX)) Output Stokes data
*   (<)  VSTOKES (Real array(NX)) Variance on Stokes parameter
*   (>)  IX      (Real array(NX)) Input X axis array
*   (<)  OX      (Real array(NX)) Output X axis array
*
*
*   Alan Bridger   9/12/1992
*
*   Modified:
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX
      REAL I1(NX),I2(NX),IV1(NX),IV2(NX)
      REAL INT(NX),INTV(NX),STOKES(NX),VSTOKES(NX)
      REAL IX(NX),OX(NX)

*  Local variables
      INTEGER I

*  Copy X axis to output
      DO I=1,NX
         OX(I)=IX(I)
      ENDDO

*  Extract spectra
      DO I=1,NX

*  Output intensity is sum of two
          INT(I) = (I1(I) + I2(I))
          INTV(I) = (IV1(I) + IV2(I))

*  Stokes parameter
          STOKES(I) = (I1(I) - I2(I))
          VSTOKES(I) = INTV(I)

      ENDDO

      END

      SUBROUTINE TSP_CGS3POLMERGE(SIZE,I1,I2,IV1,IV2,IO,IVO,QS,US,
     : QV,UV,STATUS)
*+
*   Subroutine to do the merging of the data
*
*    Sum the intensities for the two datasets, and scale up the
*    Stokes arrays and variances to be correct for the new intensity
*
*    (>) SIZE    (Integer)  The number of spectral points
*    (>) I1      (Real array(SIZE))  The first intensity array
*    (>) I2      (Real array(SIZE))  The second intensity array
*    (>) IV1     (Real array(SIZE))  Variance of the first intensity array
*    (>) IV2     (Real array(SIZE))  Variance of the second intensity array
*    (!) IO      (Real array(SIZE))  The output intensity array
*    (!) IVO     (Real array(SIZE))  The output variance array
*    (!) QS      (Real array(SIZE))  The Q stokes parameter array
*    (!) US      (Real array(SIZE))  The U stokes parameter array
*    (!) QV      (Real array(SIZE))  The Q variance array
*    (!) UV      (Real array(SIZE))  The U variance array
*    (!) STATUS  (Integer)  Status value
*
*    Alan Bridger 11/12/1992
*
*    Modified:
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE),IV1(SIZE),IV2(SIZE),IO(SIZE),IVO(SIZE),
     :    QS(SIZE),US(SIZE),QV(SIZE),UV(SIZE)
      INTEGER STATUS

*  Local variables
      REAL QFAC,UFAC,NEWINT,I1S,I2S
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Determine the sum of the intensities
         I1S = 0.0
         I2S = 0.0
         DO I=1,SIZE

*  Only sum channels for which both intensities are good
            IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
                I1S = I1S+I1(I)
                I2S = I2S+I2(I)
            ENDIF
         ENDDO

         NEWINT=I1S+I2S

*  Determine scaling factor for Stokes parameters
         IF (I1S .GT. 0.0) THEN
            QFAC=NEWINT/I2S
         ELSE
            QFAC=0.0
         ENDIF
         IF (I1S .GT. 0.0) THEN
            UFAC=NEWINT/I1S
         ELSE
            UFAC=0.0
         ENDIF

         DO I=1,SIZE

*  Scale up the Stokes parameters (if good data)

            IF (QS(I) .NE. VAL__BADR) QS(I)=QS(I)*QFAC
            IF (US(I) .NE. VAL__BADR) US(I)=US(I)*UFAC

*  Scale up the variances (if good) by the square of the factor

            IF (QV(I) .NE. VAL__BADR) QV(I)=QV(I)*QFAC*QFAC
            IF (UV(I) .NE. VAL__BADR) UV(I)=UV(I)*UFAC*UFAC

*  Add the intensities (set intensity to bad if either input value
*  is bad).

            IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
                IO(I)=(I1(I)+I2(I))
                IVO(I) = (IV1(I)+IV2(I))
            ELSE
                IO(I) = VAL__BADR
                IVO(I) = 0.0
            ENDIF
         ENDDO
      ENDIF

      END
