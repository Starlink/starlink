C+
      SUBROUTINE IRISPOLC(STATUS)
C
C            I R I S P O L C
C
C     Command name:
C        IRISPOLC
C
C     Function:
C        Reduce IRIS imaging circular polarimetry data.
C
C     Description:
C        IRISPOL reduces data obtained with the AAT IRIS polarimeter
C        using the wollaston  prism poarizer. The data for a
C        single observation consists of two Figaro files containing the
C        frames for plate positions 90 degrees apart degrees. Within each
C        frame are selected two images corresponding to the O and E rays for
C        a single mask slot. These spectra are combined
C        to derive a polarization image in TSP format.
C
C        Two different algorithms may be selected for the polarimetry
C        reduction. The two algorithms differ in the method used to
C        compensate for transparency variations between the observations
C        at the two plate positions.
C
C        The variances on the polarization data are calculated from photon
C        statistics plus readout noise.
C
C     Parameters:
C    (1) POS1       (Char)     The Figaro data file for position 0.0.
C    (2) POS2       (Char)     The Figaro data file for position 90.0.
C        X1         (Integer)  X coordinate of the bottom left corner of block
C        Y1         (Integer)  Y coordinate of the bottom left corner of block
C        WIDTH      (Integer)  Width of the block
C        HEIGHT     (Integer)  Height of the block
C        XSEP       (Integer)  OE separation vector in X
C        YSEP       (Integer)  OE separation vector in Y
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
C    17/5/1995   Original Version.   JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR,YPTR,IPTR2
      INTEGER OIPTR,VPTR,OXPTR,OYPTR,T1PTR,T2PTR

*  Number of elements in data arrays
      INTEGER NDIM, DIMS(7),DIMS2(7)


*  Extraction parameters
      INTEGER X1,Y1,WIDTH,HEIGHT
      REAL XSEP,YSEP

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,VLOC,
     :    SLOC,OXLOC,OYLOC,T1LOC,T2LOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS,YLABEL,YUNITS
      LOGICAL OK
      INTEGER ISLOT,ISLOT2,DUMMY
      CHARACTER*64 STRINGS(2)
      INTEGER ELEMENTS,XSLOT,YSLOT
      CHARACTER*64 ALGORITHM
      LOGICAL RATIO
      DOUBLE PRECISION DDUMMY

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

*  Map .X.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',1,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  And get its label and units

            CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DDUMMY,STATUS)
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)

*  Map .Y.DATA if it exists

            CALL DSA_MAP_AXIS_DATA('INPUT',2,'READ','FLOAT',YPTR,
     :           YSLOT,STATUS)

*  And get its label and units

            CALL DSA_GET_AXIS_INFO('INPUT',2,2,STRINGS,0,DDUMMY,STATUS)
            YLABEL = STRINGS(2)
            YUNITS = STRINGS(1)

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

*  Get information on location of spectra


*  Start of block in X
             CALL PAR_GET0I('X1',X1,STATUS)

*  Start of block in Y
             CALL PAR_GET0I('Y1',Y1,STATUS)

*  Width of block
             CALL PAR_GET0I('WIDTH',WIDTH,STATUS)

*  Height of block
             CALL PAR_GET0I('HEIGHT',HEIGHT,STATUS)

*  Separation in X
             CALL PAR_GET0R('XSEP',XSEP,STATUS)

*  Separation in Y
             CALL PAR_GET0R('YSEP',YSEP,STATUS)


*  Check them for validity

             IF (X1 .LT. 1 .OR. X1+WIDTH+XSEP-1 .GT. DIMS(1)
     :          .OR. Y1 .LT. 1 .OR. Y1+HEIGHT-1 .GT. DIMS(2)) THEN
               CALL MSG_OUT(' ','Invalid Position of Data Window',
     :           STATUS)
                 GOTO 100
             ENDIF

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

             CALL TSP_CREATE_2D(OLOC,WIDTH,HEIGHT,'V',.FALSE.,
     :            .FALSE.,STATUS)

*  Map axis array

             CALL TSP_MAP_X(OLOC,'WRITE',OXPTR,OXLOC,STATUS)
             CALL TSP_MAP_Y(OLOC,'WRITE',OYPTR,OYLOC,STATUS)

*  Write label and units into it (from input array)

             CALL TSP_WLU_X(OLOC,XLABEL,XUNITS,STATUS)
             CALL TSP_WLU_Y(OLOC,YLABEL,YUNITS,STATUS)

*  Get Stokes Structure

             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map the V Stokes data

             CALL TSP_MAP_DATA(SLOC,'WRITE',VPTR,VLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)


*  Write the data label and units

             CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Map the intensity data

             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)


*  Reduce the data (This is equivalent to doing IRISSTOKES on each
*  pair and then QUMERGE).

             IF (STATUS .EQ. SAI__OK) THEN
                IF (RATIO) THEN

*  Call IRISSTOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_IRISSTOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(OIPTR),%VAL(VPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))

                ELSE

*  Alternative versions for the scaling algorithm

*  Call CCD2STOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_IRISSTOKES2(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(OIPTR),%VAL(VPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))


                ENDIF

             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(VLOC,STATUS)
             CALL TSP_UNMAP(OXLOC,STATUS)
             CALL TSP_UNMAP(OYLOC,STATUS)
             CALL TSP_UNMAP(T1LOC,STATUS)
             CALL TSP_UNMAP(T2LOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF

*  Unmap input arrays

      ENDIF
100   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END










