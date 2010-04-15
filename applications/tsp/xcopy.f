C+
      SUBROUTINE XCOPY(STATUS)
C
C            X C O P Y
C
C     Command name:
C        XCOPY
C
C     Function:
C        Copy Wavelength Data from a Figaro Spectrum
C
C     Description:
C        XCOPY is equivalent to the Figaro program of the same name
C        and copies the wavelength axis information from a Figaro
C        spectrum to a TSP Polarization spectrum. This provides the
C        basic method of wavelength calibrating TSP data. First do the
C        Arc analysis using the Figaro arc fitting programs and then
C        XCOPY the resulting calibration to the TSP polarization spectra.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be calibrated.
C    (2) ARC        (Char)     The Figaro file containing the wavelengths.
C    (3) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         17/03/2000
C
C-
C
C  History:
C    29/4/1988   Original Version.   JAB/AAO
C    16/8/1990   Use DSA.            JAB/AAO
C    17/03/2000  Added DOUBLE PRECISION dummy argument DDUMMY.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,XPTR

*  Data dimensions
      INTEGER NDIM, DIMS(7), ACTDIM
      INTEGER LENNAME

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,LLOC
      CHARACTER*64 FNAME,XLABEL,XUNITS
      INTEGER SIZE
      INTEGER ELEMENTS,DUMMY,XSLOT
      CHARACTER*64 STRINGS(2)
      INTEGER CHR_LEN
      DOUBLE PRECISION DDUMMY

*  Get the Input data

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Access the Figaro frame

      CALL PAR_GET0C('ARC',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         LENNAME = CHR_LEN(FNAME)
         CALL MSG_OUT('MSG','Error Opening Input File '//
     :     FNAME(:LENNAME)//'.DST',STATUS)
         STATUS = USER__001
         GOTO 100
      ENDIF

*  Get the data size

      IF (STATUS .EQ. SAI__OK) THEN
         CALL DSA_DATA_SIZE('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)
         SIZE = DIMS(1)

*  Get Label and Units for axis

         CALL DSA_GET_AXIS_INFO('INPUT',1,2,STRINGS,0,DDUMMY,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            XLABEL = STRINGS(2)
            XUNITS = STRINGS(1)
         ELSE
            XLABEL = ' '
            XUNITS = ' '
         ENDIF

*  Map wavelength axis data

         CALL DSA_MAP_AXIS_DATA('INPUT',1,'READ','FLOAT',IPTR,XSLOT,
     :       STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             CALL MSG_OUT(' ','Error Mapping wavelength data',STATUS)
             STATUS = USER__001
             GOTO 100
         ENDIF

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of output file

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)

*  Check that it matches calibration file

         IF (DIMS(1) .NE. SIZE) THEN
              CALL MSG_OUT('MSG','Wavelength Axes are different Sizes',
     :           STATUS)
              STATUS = USER__001
         ENDIF

*  Map axis array

         CALL TSP_MAP_LAMBDA(OLOC,'WRITE',XPTR,LLOC,STATUS)
         CALL TSP_WLU_LAMBDA(OLOC,XLABEL,XUNITS,STATUS)


*  Copy the X axis

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_XCOPY(SIZE,%VAL(IPTR),%VAL(XPTR))
         ENDIF

*  Unmap output arrays and annul locators

         CALL TSP_UNMAP(LLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

*  Unmap input arrays

      ENDIF
100   CONTINUE
      CALL DSA_CLOSE(STATUS)
      END



      SUBROUTINE TSP_XCOPY(SIZE,X1,X2)
*+
*
*  T S P _ X C O P Y
*
*  XCOPY command
*
*  Copy array X1 to array X2 - used to copy the wavelength axis of
*  the calibration file to the output file
*
*  Parameters:
*     (>)   SIZE     (Integer)               Size of the arrays
*     (>)   X1       (Real array(SIZE))      Input array to be copied
*     (<)   X2       (Real array(SIZE))      Output array
*
*  Jeremy Bailey    16/8/1990
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      REAL X1(SIZE),X2(SIZE)

*  Local variable
      INTEGER I


*  Copy input to output
      DO I=1,SIZE
          X2(I) = X1(I)
      ENDDO
      END

