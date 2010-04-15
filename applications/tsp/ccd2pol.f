C+
      SUBROUTINE CCD2POL(STATUS)
C
C            C C D 2 P O L
C
C     Command name:
C        CCD2POL
C
C     Function:
C        Reduce CCD spectropolarimetry data.
C
C     Description:
C        CCD2POL reduces data obtained with the AAO Half-wave plate
C        spectropolarimeter with the CCD as detector. The data for a
C        single observation consists of four Figaro files containing the
C        frames for plate position 0, 45, 22.5 and 67.5 degrees. Within each
C        frame there are four spectra corresponding to the O and E rays for
C        each of two apertures (A and B). These spectra are combined
C        to derive a polarization spectrum in TSP format.
C        The CCD data are expected to be in raw CCD format which is
C        the wrong way round for Figaro. i.e. the Y axis is the dispersion
C        direction. Thus if the data is preprocessed using Figaro it will
C        have to be rotated back.
C
C        Two different algorithms may be selected for the polarimetry
C        reduction. The two algorithms differ in the method used to
C        compensate for transparency variations between the observations
C        at the two plate positions.
C
C        The variances on the polarization data are calculated from photon
C        statistics plus readout noise.
C
C        If the spectra are laid out in columns with the
C        aperture A spectrum in columns 5 to 7 (O) and columns 17 to 19 (E)
C        and the aperture B spectrum in columns 30 to 32 (O) and 42 to 44 (E)
C        then the parameters should be set as follows ASTART=5, BSTART=30,
C        OESEP=12, WIDTH=3.
C
C
C     Parameters:
C    (1) POS1       (Char)     The Figaro data file for position 0.0.
C    (2) POS2       (Char)     The Figaro data file for position 45.0.
C    (3) POS3       (Char)     The Figaro data file for position 22.5.
C    (4) POS4       (Char)     The Figaro data file for position 67.5.
C        ASTART     (Integer)  The Start column for the A aperture data.
C        BSTART     (Integer)  The Start column for the B aperture data.
C        OESEP      (Integer)  The distance in columns from the start of the O
C                               spectrum to the start of the E spectrum.
C        WIDTH      (Integer)  The number of columns to include in each
C                               extracted spectrum.
C        APERTURE   (Char)     The aperture containing the star (A or B).
C        BIAS       (Real)     Bias level to be subtracted from data.
C        READNOISE  (Real)     CCD readout noise (electrons/pixel).
C        PHOTADU    (Real)     Photons per ADU for the CCD data.
C        ALGORITHM  (Char)     The Algorithm to use for stokes
C                               parameter calculation (OLD, RATIO)
C        OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 17/03/2000
C
C-
C
C  History:
C    27/4/1988   Original Version (CCD2STOKES).   JAB/AAO
C    10/8/1990   Use DSA             JAB/AAO
C    11/8/1990   Add Readout noise   JAB/AAO
C    11/8/1990   Adapted from CCD2STOKES.  JAB/AAO
C    2/10/1991   Add algorithm selection   JAB/AAO
C    20/11/1991  Remove case sensitivity on AP   JAB/AAO
C    20/11/1991  Handle bad values     JAB/AAO
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
            SEP = DIMS(1)

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

*  Map .Y.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',2,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  Ang get its label and units

            CALL DSA_GET_AXIS_INFO('INPUT',2,2,STRINGS,0,DDUMMY,STATUS)
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

*  Separation of O and E spectra
             CALL PAR_GET0I('OESEP',OESEP,STATUS)

*  Width for extraction
             CALL PAR_GET0I('WIDTH',WIDTH,STATUS)

*  Check them for validity

             IF (ASTART+(WIDTH-1) .GT. SEP .OR.
     :           ASTART+(WIDTH-1)+OESEP .GT. SEP .OR.
     :           BSTART+(WIDTH-1) .GT. SEP .OR.
     :           BSTART+(WIDTH-1)+OESEP .GT. SEP) THEN
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

*  Get the CCD parameters

*  Bias level
             CALL PAR_GET0R('BIAS',BIAS,STATUS)

*  Readout noise (electrons)
             CALL PAR_GET0R('READNOISE',RDN,STATUS)

*  Photons per ADU
             CALL PAR_GET0R('PHOTADU',PHOTADU,STATUS)

*  Which Algorithm?

             OK = .FALSE.
             DO WHILE ((.NOT. OK) .AND. STATUS .EQ. SAI__OK )
                CALL PAR_GET0C('ALGORITHM',ALGORITHM,STATUS)
                CALL CHR_UCASE(ALGORITHM)
                OK = (ALGORITHM.EQ.'OLD' .OR. ALGORITHM.EQ.'RATIO')
                IF (.NOT. OK) THEN
                   CALL PAR_CANCL('ALGORITHM',STATUS)
                ENDIF
             ENDDO
             RATIO = ALGORITHM .EQ. 'RATIO'

*  Create the output file

             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure

             CALL TSP_CREATE_1D(OLOC,DIMS(2),'QU',.FALSE.,
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

             CALL TSP_TEMP(DIMS(2),'_REAL',T1PTR,T1LOC,STATUS)
             CALL TSP_TEMP(DIMS(2),'_REAL',T2PTR,T2LOC,STATUS)

*  Reduce the data (This is equivalent to doing CCD2STOKES on each
*  pair and then QUMERGE).

             IF (STATUS .EQ. SAI__OK) THEN
                IF (RATIO) THEN

*  Call CCD2STOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_CCD2STOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),ASTART,BSTART,OESEP,
     :                WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(T1PTR),
     :                %VAL(UPTR),%VAL(UEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))

*  Call CCD2STOKES for the second pair of frames (22.5 and 67.5) and
*  derive the Q stokes parameter

                    CALL TSP_CCD2STOKES(DIMS(1),DIMS(2),%VAL(IPTR3),
     :                %VAL(IPTR4),ASTART,BSTART,OESEP,
     :                WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(T2PTR),
     :                %VAL(QPTR),%VAL(QEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))
                ELSE

*  Alternative versions for the scaling algorithm

                    CALL TSP_CCD2ST_OLD(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),ASTART,BSTART,OESEP,
     :                WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(T1PTR),
     :                %VAL(UPTR),%VAL(UEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))
                    CALL TSP_CCD2ST_OLD(DIMS(1),DIMS(2),%VAL(IPTR3),
     :                %VAL(IPTR4),ASTART,BSTART,OESEP,
     :                WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(T2PTR),
     :                %VAL(QPTR),%VAL(QEPTR),
     :                %VAL(XPTR),%VAL(OXPTR))
                ENDIF

*  Merge the Q and U data

                CALL TSP_CCDPOLMERGE(DIMS(2),%VAL(T1PTR),%VAL(T2PTR),
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





