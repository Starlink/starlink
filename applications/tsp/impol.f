C+
      SUBROUTINE IMPOL(STATUS)
C
C            I M P O L
C
C     Command name:
C        IMPOL
C
C     Function:
C        Reduce IR Polarization images
C
C     Description:
C        IMPOL derives a polarization image from a set of four observations
C        made with a rotating half-wave plate polarimeter at angles of
C        0, 22.5, 45 and 67.5 degrees. It is used to reduce polarization
C        imaging data obtained with the IRIS IR camera and half-wave plate
C        polarimeter at the AAT or the IRPOL/IRCAM polarimeter at UKIRT.
C        It should also be useable with other similar instruments (not
C        necessarily in the IR).
C
C        The input images should be NDF files (not Figaro .DST files).
C
C     Parameters:
C    (1) POS1       (Char)     The input image for position 0.0.
C    (2) POS2       (Char)     The input image for position 45.0.
C    (3) POS3       (Char)     The input image for position 22.5.
C    (4) POS4       (Char)     The input image for position 67.5.
C        OUTPUT     (TSP, 2D)  The Output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 2/6/1992
C
C-
C
C  History:
C     2/6/1992 - Original version    JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Status argument
      INTEGER STATUS

*  NDF identifiers
      INTEGER ID1,ID2,ID3,ID4

*  Data pointers
      INTEGER I1PTR,I2PTR,I3PTR,I4PTR,OPTR,QPTR,UPTR

*  Number of elements in data arrays
      INTEGER NELM
      INTEGER NDIM, DIMS(7),DIMS2(7)

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,OLOC2,SLOC,QLOC,ULOC

*  Access the First frame

      CALL NDF_ASSOC('POS1','READ',ID1,STATUS)

*  Get the data size

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_DIM(ID1,2,DIMS,NDIM,STATUS)

*  Check that it is two dimensional

         IF (NDIM .NE. 2) THEN
            CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :          STATUS)
            GOTO 100
         ELSE

*  Access the Second frame

             CALL NDF_ASSOC('POS2','READ',ID2,STATUS)

*  Get the data size

             IF (STATUS .EQ. SAI__OK) THEN
                CALL NDF_DIM(ID2,2,DIMS2,NDIM,STATUS)

*  Check that its dimensions match those of the previous frame

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF
             ENDIF

*  Access the third frame

             CALL NDF_ASSOC('POS3','READ',ID3,STATUS)

*  Get the data size

             IF (STATUS .EQ. SAI__OK) THEN
                CALL NDF_DIM(ID3,2,DIMS2,NDIM,STATUS)

*  Check that its dimensions match those of the previous frames

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF
             ENDIF

*  Access the Fourth frame

             CALL NDF_ASSOC('POS4','READ',ID4,STATUS)

*  Get the data size

             IF (STATUS .EQ. SAI__OK) THEN
                CALL NDF_DIM(ID4,2,DIMS2,NDIM,STATUS)

*  Check that the dimensions match those of the previous frames

                IF (NDIM .NE. 2 .OR.
     :               DIMS2(1) .NE. DIMS(1) .OR.
     :               DIMS2(2) .NE. DIMS(2)) THEN
                  CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :              STATUS)
                  GOTO 100
                ENDIF
             ENDIF

*  Create the output file

             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure

             CALL TSP_CREATE_2D(OLOC,DIMS(1),DIMS(2),'QU',.FALSE.,
     :            .FALSE.,STATUS)

             CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Get Stokes Structure

             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q Stokes data

             CALL TSP_MAP_DATA(SLOC,'WRITE',QPTR,QLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Map the U Stokes data

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'WRITE',UPTR,ULOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)

*  Map the input files

             CALL NDF_MAP(ID1,'Data','_REAL','READ',I1PTR,NELM,STATUS)
             CALL NDF_MAP(ID2,'Data','_REAL','READ',I2PTR,NELM,STATUS)
             CALL NDF_MAP(ID3,'Data','_REAL','READ',I3PTR,NELM,STATUS)
             CALL NDF_MAP(ID4,'Data','_REAL','READ',I4PTR,NELM,STATUS)

*  Reduce the data

             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_IMPOL(NELM,%VAL(I1PTR),%VAL(I2PTR),%VAL(I3PTR),
     :                  %VAL(I4PTR),%VAL(OPTR),%VAL(QPTR),%VAL(UPTR))
             ENDIF

*  Unmap output arrays and annul locators

             CALL NDF_UNMAP(ID1,'Data',STATUS)
             CALL NDF_UNMAP(ID2,'Data',STATUS)
             CALL NDF_UNMAP(ID3,'Data',STATUS)
             CALL NDF_UNMAP(ID4,'Data',STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF

*  Unmap input arrays

      ENDIF
100   CONTINUE
      END




      SUBROUTINE TSP_IMPOL(SIZE,I1,I2,I3,I4,I,Q,U)
*+
*    IMPOL command
*
*    Subroutine to do the reduction
*
*    Sum the intensities for the four datasets to derive the output
*    intensity, calculate the fractional Stokes parameters from:
*
*       U/I = (I1-I2)/(I1+I2)    Q/I = (I3-I4)/(I3+I4)
*
*    and then scale up to intensity stokes parameters by multiplying
*    by the I array
*
*    (>) SIZE    (Integer)  The number of data points
*    (>) I1      (Real array(SIZE))  The first input array
*    (>) I2      (Real array(SIZE))  The second input array
*    (>) I3      (Real array(SIZE))  The third input array
*    (>) I4      (Real array(SIZE))  The fourth input array
*    (>) I       (Real array(SIZE))  The output intensity array
*    (<) Q       (Real array(SIZE))  The Q stokes parameter array
*    (<) U       (Real array(SIZE))  The U stokes parameter array
*
*    Jeremy Bailey   2/6/1992
*
*    Modified:
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE),I3(SIZE),I4(SIZE),
     :    I(SIZE),Q(SIZE),U(SIZE)
      INTEGER STATUS

*  Local variables
      REAL QF,UF
      INTEGER IX

      DO IX=1,SIZE

*  Only process points for which all intensities are good
         IF (I1(IX) .NE. VAL__BADR .AND. I2(IX) .NE. VAL__BADR .AND.
     :       I3(IX) .NE. VAL__BADR .AND. I4(IX) .NE. VAL__BADR) THEN
             I(IX) = I1(IX)+I2(IX)+I3(IX)+I4(IX)
             UF = (I1(IX)-I2(IX))/(I1(IX)+I2(IX))
             QF = (I3(IX)-I4(IX))/(I3(IX)+I4(IX))
             U(IX) = UF*I(IX)
             Q(IX) = QF*I(IX)
         ELSE
             I(IX) = VAL__BADR
             U(IX) = VAL__BADR
             Q(IX) = VAL__BADR
         ENDIF
      ENDDO
      END

