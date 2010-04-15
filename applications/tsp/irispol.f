C+
      SUBROUTINE IRISPOL(STATUS)
C
C            I R I S P O L
C
C     Command name:
C        IRISPOL
C
C     Function:
C        Reduce IRIS imaging polarimetry data.
C
C     Description:
C        IRISPOL reduces data obtained with the AAT IRIS polarimeter
C        using the wolaston  prism poarizer. The data for a
C        single observation consists of four Figaro files containing the
C        frames for plate position 0, 45, 22.5 and 67.5 degrees. Within each
C        frame are selected two images corresponding to the O and E rays for
C        a siingle mask slot. These spectra are combined
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
C    (2) POS2       (Char)     The Figaro data file for position 45.0.
C    (3) POS3       (Char)     The Figaro data file for position 22.5.
C    (4) POS4       (Char)     The Figaro data file for position 67.5.
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
C    3/5/1993   Original Version.   JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR,YPTR,IPTR2,IPTR3,IPTR4
      INTEGER OIPTR,UPTR,QPTR,OXPTR,OYPTR,T1PTR,T2PTR

*  Number of elements in data arrays
      INTEGER NDIM, DIMS(7),DIMS2(7)


*  Extraction parameters
      INTEGER X1,Y1,WIDTH,HEIGHT
      REAL XSEP,YSEP

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,ULOC,QLOC,
     :    SLOC,OXLOC,OYLOC,T1LOC,T2LOC
      CHARACTER*64 FNAME,LABEL,UNITS,XLABEL,XUNITS,YLABEL,YUNITS
      LOGICAL OK
      INTEGER ISLOT,ISLOT2,ISLOT3,ISLOT4,DUMMY
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

             CALL TSP_CREATE_2D(OLOC,WIDTH,HEIGHT,'QU',.FALSE.,
     :            .FALSE.,STATUS)

*  Map axis array

             CALL TSP_MAP_X(OLOC,'WRITE',OXPTR,OXLOC,STATUS)
             CALL TSP_MAP_Y(OLOC,'WRITE',OYPTR,OYLOC,STATUS)

*  Write label and units into it (from input array)

             CALL TSP_WLU_X(OLOC,XLABEL,XUNITS,STATUS)
             CALL TSP_WLU_Y(OLOC,YLABEL,YUNITS,STATUS)

*  Get Stokes Structure

             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q Stokes data

             CALL TSP_MAP_DATA(SLOC,'WRITE',QPTR,QLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Map the U Stokes data

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'WRITE',UPTR,ULOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Write the data label and units

             CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Map the intensity data

             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Get workspace arrays

             CALL TSP_TEMP(WIDTH*HEIGHT,'_REAL',T1PTR,T1LOC,STATUS)
             CALL TSP_TEMP(WIDTH*HEIGHT,'_REAL',T2PTR,T2LOC,STATUS)

*  Reduce the data (This is equivalent to doing IRISSTOKES on each
*  pair and then QUMERGE).

             IF (STATUS .EQ. SAI__OK) THEN
                IF (RATIO) THEN

*  Call IRISSTOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_IRISSTOKES(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(T1PTR),%VAL(UPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))

*  Call IRISSTOKES for the second pair of frames (22.5 and 67.5) and
*  derive the Q stokes parameter

                    CALL TSP_IRISSTOKES(DIMS(1),DIMS(2),%VAL(IPTR3),
     :                %VAL(IPTR4),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(T2PTR),%VAL(QPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))
                ELSE

*  Alternative versions for the scaling algorithm

*  Call CCD2STOKES for the first pair of frames (0 and 45) and
*  derive the U stokes parameter

                    CALL TSP_IRISSTOKES2(DIMS(1),DIMS(2),%VAL(IPTR),
     :                %VAL(IPTR2),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(T1PTR),%VAL(UPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))

*  Call IRISSTOKES for the second pair of frames (22.5 and 67.5) and
*  derive the Q stokes parameter

                    CALL TSP_IRISSTOKES2(DIMS(1),DIMS(2),%VAL(IPTR3),
     :                %VAL(IPTR4),X1,Y1,WIDTH,HEIGHT,XSEP,YSEP,
     :                %VAL(T2PTR),%VAL(QPTR),
     :                %VAL(XPTR),%VAL(OXPTR),%VAL(YPTR),%VAL(OYPTR))

                ENDIF

*  Merge the Q and U data

                CALL TSP_IRISMERGE(WIDTH,HEIGHT,%VAL(T1PTR),%VAL(T2PTR),
     :           %VAL(OIPTR),%VAL(QPTR),%VAL(UPTR),STATUS)
             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL TSP_UNMAP(OXLOC,STATUS)
             CALL TSP_UNMAP(OYLOC,STATUS)
             CALL TSP_UNMAP(T1LOC,STATUS)
             CALL TSP_UNMAP(T2LOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF

*  Unmap input arrays

      ENDIF
100   CONTINUE
      PRINT *,'Calling DSA_CLOSE'
      CALL DSA_CLOSE(STATUS)
      PRINT *,'DSA_CLOSE Finished'
      END




      SUBROUTINE TSP_IRISMERGE(IX,IY,I1,I2,IO,QS,US,STATUS)
*+
*   Subroutine to do the merging of the data
*
*    Sum the intensities for the two datasets, and scale up the
*    Stokes arrays and variances to be correct for the new intensity
*
*    (>) IX   (Integer)  First dimension of arrays
*    (>) IY   (Integer)  Second dimension of arrays
*    (>) I1      (Real array(SIZE))  The first intensity array
*    (>) I2      (Real array(SIZE))  The second intensity array
*    (!) IO      (Real array(SIZE))  The output intensity array
*    (!) QS      (Real array(SIZE))  The Q stokes parameter array
*    (!) US      (Real array(SIZE))  The U stokes parameter array
*    (!) STATUS  (Integer)  Status value
*
*    Jeremy Bailey   3/5/1993
*
*    Modified:
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER IX,IY
      REAL I1(IX,IY),I2(IX,IY),IO(IX,IY),QS(IX,IY),US(IX,IY)
      INTEGER STATUS

*  Local variables
      REAL QFAC,UFAC,NEWINT,I1S,I2S
      INTEGER I,J

      IF (STATUS .EQ. SAI__OK) THEN

*  Determine the sum of the intensities
         I1S = 0.0
         I2S = 0.0
         DO I=1,IX
           DO J=1,IY

*  Only sum channels for which both intensities are good
            IF (I1(I,J) .NE. VAL__BADR .AND.
     :          I2(I,J) .NE. VAL__BADR) THEN
                I1S = I1S+I1(I,J)
                I2S = I2S+I2(I,J)
            ENDIF
           ENDDO
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
         DO I=1,IX
            DO J=1,IY

*  Scale up the Stokes parameters (if good data)

            IF (QS(I,J) .NE. VAL__BADR) QS(I,J)=QS(I,J)*QFAC
            IF (US(I,J) .NE. VAL__BADR) US(I,J)=US(I,J)*UFAC


*  Add the intensities (set intensity to bad if either input value
*  is bad).

            IF (I1(I,J) .NE. VAL__BADR .AND.
     :         I2(I,J) .NE. VAL__BADR) THEN
                IO(I,J)=I1(I,J)+I2(I,J)
            ELSE
                IO(I,J) = VAL__BADR
            ENDIF
           ENDDO
         ENDDO
      ENDIF
      END







