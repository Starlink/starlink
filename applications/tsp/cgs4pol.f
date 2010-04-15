C+
      SUBROUTINE CGS4POL(STATUS)
C
C            C G S 4 P O L
C
C     Command name:
C        CGS4POL
C
C     Function:
C        Reduce CGS4 spectropolarimetry data.
C
C     Description:
C        CGS4POL reduces data obtained with the CGS4 instrument at UKIRT
C        used in its spectropolarimetry mode. The data for a
C        single observation consists of four Figaro files containing the
C        frames for plate position 0, 45, 22.5 and 67.5 degrees. Within each
C        frame there should be two spectra corresponding to two slit
C        positions. These spectra are combined
C        to derive a polarization spectrum in TSP format.
C
C
C     Parameters:
C    (1) POS1       (Char)     The Figaro data file for position 0.0.
C    (2) POS2       (Char)     The Figaro data file for position 45.0.
C    (3) POS3       (Char)     The Figaro data file for position 22.5.
C    (4) POS4       (Char)     The Figaro data file for position 67.5.
C        ASTART     (Integer)  The Start channel for the A aperture data.
C        BSTART     (Integer)  The Start channel for the B aperture data.
C        WIDTH      (Integer)  The number of channels to include in each
C                               spectrum.
C        APERTURE   (Char)     The aperture containing the star (A or B).
C        OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 17/03/2000
C
C-
C
C  History:
C     10/11/92  Original version
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR,IPTR2,IPTR3,IPTR4
      INTEGER OIPTR,UEPTR,UPTR,QEPTR,QPTR,OXPTR,T1PTR,T2PTR

*  Number of elements in data arrays
      INTEGER NELM
      INTEGER NDIM, DIMS(7),DIMS2(7)
      INTEGER LENNAME

*  Extraction parameters
      INTEGER ASTART,BSTART,OESEP,WIDTH,SEP

*  CCD parameters
      REAL BIAS,PHOTADU,RDN

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,ULOC,UELOC,QLOC,QELOC,
     :    SLOC,OXLOC,T1LOC,T2LOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS,AP
      LOGICAL OK
      INTEGER ISLOT,ISLOT2,ISLOT3,ISLOT4,DUMMY
      CHARACTER*64 STRINGS(2)
      INTEGER ELEMENTS,XSLOT
      CHARACTER*64 ALGORITHM
      LOGICAL RATIO
      DOUBLE PRECISION DDUMMY


      INTEGER ICH_LEN

*  Access the First Figaro frame

      CALL PAR_GET0C('POS1',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_DATA_SIZE('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)

*  Check that it is two dimensional

         IF (NDIM .NE. 2) THEN
            CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :          STATUS)
            GOTO 100
         ELSE
            SEP = DIMS(2)

*  Map the data

            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get the label and units

            CALL DSA_GET_DATA_INFO('INPUT',2,STRINGS,1,DDUMMY,STATUS)
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

            CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DDUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Access the Second Figaro frame

             CALL PAR_GET0C('POS2',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT2',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error accessing frame',STATUS)
               GOTO 100
             ENDIF

*  Get the data array

             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT2',2,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that its dimensions match those of the previous frame

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF

*  Map the data

                CALL DSA_MAP_DATA('INPUT2','READ','FLOAT',IPTR2,ISLOT2,
     :                STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Access the Third Figaro frame

             CALL PAR_GET0C('POS3',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT3',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error accessing frame',STATUS)
               GOTO 100
             ENDIF

*  Get the data array

             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT3',2,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that its dimensions match those of the previous frames

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF

*  Map the data

                CALL DSA_MAP_DATA('INPUT3','READ','FLOAT',IPTR3,ISLOT3,
     :                STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Access the Fourth Figaro frame

             CALL PAR_GET0C('POS4',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT4',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error accessing frame',STATUS)
               STATUS = USER__001
             ENDIF

*  Get the data array

             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT4',2,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check that the dimensions match those of the previous frames

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF

*  Map the data

                CALL DSA_MAP_DATA('INPUT4','READ','FLOAT',IPTR4,ISLOT4,
     :                STATUS)
                IF (STATUS .NE. SAI__OK) THEN
                   CALL ERR_REP(' ','Error Mapping Input Data',STATUS)
                   GOTO 100
                ENDIF
             ENDIF

*  Get information on location of spectra


*  Start of aperture A
             CALL PAR_GET0I('ASTART',ASTART,STATUS)

*  Start of aperture B
             CALL PAR_GET0I('BSTART',BSTART,STATUS)

*  Width for extraction
             CALL PAR_GET0I('WIDTH',WIDTH,STATUS)

*  Check them for validity

             IF (ASTART+(WIDTH-1) .GT. SEP .OR.
     :           BSTART+(WIDTH-1) .GT. SEP) THEN
                 CALL MSG_OUT(' ','Invalid Position of Spectra',STATUS)
                 GOTO 100
             ENDIF

*  Get the aperture name

             OK = .FALSE.
             DO WHILE ((.NOT. OK) .AND. STATUS .EQ. SAI__OK)
                 CALL PAR_GET0C('APERTURE',AP,STATUS)
                 CALL CHR_UCASE(AP)
                 OK = (AP .EQ. 'A' .OR. AP .EQ. 'B')
                 IF (.NOT. OK) THEN
                     CALL PAR_CANCL('APERTURE',STATUS)
                 ENDIF
             ENDDO

*  Create the output file

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

*  Map the intensity data

             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Get workspace arrays

             CALL TSP_TEMP(DIMS(1),'_REAL',T1PTR,T1LOC,STATUS)
             CALL TSP_TEMP(DIMS(1),'_REAL',T2PTR,T2LOC,STATUS)

*  Reduce the data (This is equivalent to doing CCD2STOKES on each
*  pair and then QUMERGE).

             IF (STATUS .EQ. SAI__OK) THEN

*  Call CCD2STOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_CGS4STOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),ASTART,BSTART,
     :                WIDTH,AP,%VAL(T1PTR),
     :                %VAL(UPTR),%VAL(UEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))

*  Call CCD2STOKES for the second pair of frames (22.5 and 67.5) and
*  derive the Q stokes parameter

                    CALL TSP_CGS4STOKES(DIMS(1),DIMS(2),%VAL(IPTR3),
     :                %VAL(IPTR4),ASTART,BSTART,
     :                WIDTH,AP,%VAL(T2PTR),
     :                %VAL(QPTR),%VAL(QEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))

*  Merge the Q and U data

                CALL TSP_CCDPOLMERGE(DIMS(1),%VAL(T1PTR),%VAL(T2PTR),
     :           %VAL(OIPTR),%VAL(QPTR),%VAL(UPTR),%VAL(QEPTR),
     :           %VAL(UEPTR),STATUS)
             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(QELOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL TSP_UNMAP(UELOC,STATUS)
             CALL TSP_UNMAP(OXLOC,STATUS)
             CALL TSP_UNMAP(T1LOC,STATUS)
             CALL TSP_UNMAP(T2LOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF

*  Unmap input arrays

      ENDIF
100   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END


      SUBROUTINE TSP_CGS4STOKES(NX,NY,I1,I2,ASTART,BSTART,WIDTH,
     :    AP,INT,STOKES,VSTOKES,IX,OX)
*+
*
*   Reduce CGS4 spectropolarimetry
*

*   (>)  NX      (Integer)  Number of spectral channels
*   (>)  NY      (Integer)  Size of spatial axis of input arrays
*   (>)  I1      (Real array(NX,NY) First input array
*   (>)  I2      (Real array(NX,NY) Second input array
*   (>)  ASTART  (Integer)  Start of A aperture
*   (>)  BSTART  (Integer)  Start of B aperture
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  AP      (Char)     Name of aperture (A or B)
*   (<)  INT     (Real array(NX))  Output intensity
*   (<)  STOKES  (Real array(NX))  Output Stokes data
*   (<)  VSTOKES (Real array(NX))  Variance on Stokes parameter
*   (>)  IX      (Real array(NX))  Input X axis array
*   (<)  OX      (Real array(NX))  Output X axis array
*
*
*   Jeremy Bailey   10/11/1992
*
*   Modified:
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,ASTART,BSTART,WIDTH
      CHARACTER*(*) AP
      REAL I1(NX,NY),I2(NX,NY)
      REAL INT(NX),STOKES(NX),VSTOKES(NX)
      REAL IX(NX),OX(NX)

*  Local variables
      INTEGER I,J,K
      REAL SPEC2(4)
      INTEGER PT(2)
      REAL STAR,SKY

*  Copy X axis to output
      DO I=1,NX
         OX(I)=IX(I)
      ENDDO

*  Determine position ranges to extract data over
      IF (AP .EQ. 'A') THEN
          PT(1) = ASTART
          PT(2) = BSTART
      ELSE
          PT(1) = BSTART
          PT(2) = ASTART
      ENDIF

*  Extract 4 spectra (star and sky for each of two polarization states)
      DO I=1,NX
          DO J=1,2
              SPEC2(J) = I1(I,PT(J))
              SPEC2(J+2) = I2(I,PT(J))
              DO K=1,WIDTH-1
                  SPEC2(J) = SPEC2(J)+I1(I,PT(J)+K)
                  SPEC2(J+2) = SPEC2(J+2)+I2(I,PT(J)+K)
              ENDDO

          ENDDO

*  Determine star intensity
          STAR = SPEC2(1)+SPEC2(3)

*  Determine sky intensity
          SKY = SPEC2(2)+SPEC2(4)

*  Output intensity is star minus sky
          INT(I) = STAR-SKY

          STOKES(I) = SPEC2(1)-SPEC2(3)-SPEC2(2)+SPEC2(4)

          VSTOKES(I) = (0.01*STOKES(I))**2
      ENDDO
      END

