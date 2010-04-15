C+
      SUBROUTINE CCD2STOKES(STATUS)
C
C            C C D 2 S T O K E S
C
C     Command name:
C        CCD2STOKES
C
C     Function:
C        Reduce CCD spectropolarimetry data.
C
C     Description:
C        CCD2STOKES reduces data obtained with the AAO Pockels cell
C        spectropolarimeter with the CCD as detector. The data for a
C        single observation consists of two Figaro files containing the
C        A and B state frames. Within each A and B frame there are four
C        spectra corresponding to the O and E rays for each of two
C        apertures (A and B). These spectra are combined
C        to derive a Stokes parameter spectrum in TSP format.
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
C        CCD2STOKES can also be used to reduce circular polarization
C        data obtained with the wave-plate polarimeter. The equivalent of
C        the A and B state data are the frames taken at two positions of
C        a quarter-wave plate spaced by 90 degrees.
C
C     Parameters:
C    (1) AFIGARO    (Char)     The Figaro A-state data file.
C    (2) BFIGARO    (Char)     The Figaro B-state data file.
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
C        STOKESPAR  (Char)     The Stokes parameter (Q,U,V).
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
C    27/4/1988   Original Version.   JAB/AAO
C    10/8/1990   Use DSA             JAB/AAO
C    11/8/1990   Add Readout noise   JAB/AAO
C    2/10/1991   Add algorithm selection   JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR,IPTR2
      INTEGER OIPTR,OEPTR,OSPTR,OXPTR

*  Number of elements in data arrays
      INTEGER NELM
      INTEGER FSTATUS
      INTEGER NDIM, DIMS(7),DIMS2(7)
      INTEGER LENNAME

*  Extraction parameters
      INTEGER ASTART,BSTART,OESEP,WIDTH,SEP

*  CCD parameters
      REAL BIAS,PHOTADU,RDN

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,TLOC,XLOC,SLOC,ELOC,ESLOC,OXLOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS,STOKESPAR,ERRMES,AP
      LOGICAL XMAPPED,OK
      INTEGER ISLOT,ISLOT2,DUMMY
      CHARACTER*64 STRINGS(2)
      INTEGER ELEMENTS,XSLOT
      CHARACTER*64 ALGORITHM
      LOGICAL RATIO
      DOUBLE PRECISION DDUMMY

*  Access the First Figaro frame

      CALL PAR_GET0C('AFIGARO',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN

*   Check that it is two dimensional

         CALL DSA_DATA_SIZE('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)
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
                STATUS = SAI__OK
            ENDIF

*  Map .Y.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',2,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  And get its label and units

            CALL DSA_GET_AXIS_INFO('INPUT',2,2,STRINGS,0,DDUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Access the Second Figaro frame

             CALL PAR_GET0C('BFIGARO',FNAME,STATUS)
             CALL DSA_NAMED_INPUT('INPUT2',FNAME,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error accessing frame',STATUS)
               GOTO 100
             ENDIF

*  Get the data array

             IF (STATUS .EQ. SAI__OK) THEN
                CALL DSA_DATA_SIZE('INPUT2',2,NDIM,DIMS2,ELEMENTS,
     :               STATUS)

*  Check dimensions match that of first frame

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF

*   Map the data

                CALL DSA_MAP_DATA('INPUT2','READ','FLOAT',IPTR2,ISLOT2,
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

*  Get CCD parameters

*  Bias level
             CALL PAR_GET0R('BIAS',BIAS,STATUS)

*  Readout noise (electrons)
             CALL PAR_GET0R('READNOISE',RDN,STATUS)

*  Photons per ADU
             CALL PAR_GET0R('PHOTADU',PHOTADU,STATUS)

*  Which Stokes Parameter?

             OK = .FALSE.
             DO WHILE ((.NOT. OK) .AND. STATUS .EQ. SAI__OK )
                CALL PAR_GET0C('STOKESPAR',STOKESPAR,STATUS)
                CALL CHR_UCASE(STOKESPAR)
                OK = (STOKESPAR .EQ. 'Q' .OR. STOKESPAR .EQ. 'U' .OR.
     :                  STOKESPAR .EQ. 'V')
                IF (.NOT. OK) THEN
                   CALL PAR_CANCL('STOKESPAR',STATUS)
                ENDIF
             ENDDO

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

             CALL TSP_CREATE_1D(OLOC,DIMS(2),STOKESPAR,.FALSE.,
     :            .TRUE.,STATUS)

*  Map axis array

             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',OXPTR,OXLOC,STATUS)
             CALL TSP_WLU_LAMBDA(OLOC,XLABEL,XUNITS,STATUS)

*  Get Stokes Structure

             CALL TSP_GET_STOKES(OLOC,STOKESPAR,SLOC,STATUS)

*  Map the Stokes data ...
             CALL TSP_MAP_DATA(SLOC,'WRITE',OSPTR,ESLOC,STATUS)

*  ... and its variance
             CALL TSP_MAP_VAR(SLOC,'WRITE',OEPTR,ELOC,STATUS)

*  Write the label and units (from the input file)
             CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Map the intensity data
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Copy the data
             IF (STATUS .EQ. SAI__OK) THEN
                IF (RATIO) THEN
                    CALL TSP_CCD2STOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :                  %VAL(IPTR2),ASTART,BSTART,OESEP,
     :                  WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(OIPTR),
     :                  %VAL(OSPTR),%VAL(OEPTR),
     :                  %VAL(XPTR),%VAL(OXPTR))
                ELSE
                    CALL TSP_CCD2ST_OLD(DIMS(1),DIMS(2),%VAL(IPTR),
     :                  %VAL(IPTR2),ASTART,BSTART,OESEP,
     :                  WIDTH,AP,BIAS,RDN,PHOTADU,%VAL(OIPTR),
     :                  %VAL(OSPTR),%VAL(OEPTR),
     :                  %VAL(XPTR),%VAL(OXPTR))
                ENDIF
             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(ESLOC,STATUS)
             CALL TSP_UNMAP(XLOC,STATUS)
             CALL TSP_UNMAP(ELOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF

*  Unmap input arrays

      ENDIF
100   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END







