C+
      SUBROUTINE SPFLUX(STATUS)
C
C            S P F L U X
C
C     Command name:
C        SPFLUX
C
C     Function:
C        Apply flux calibration to a polarization spectrum
C
C     Description:
C        SPFLUX is equivalent to the Figaro program of the same name.
C        It applies a Figaro flux calibration spectrum to a TSP
C        polarization spectrum to generate a flux calibrated spectrum.
C        The flux calibration spectrum can be generated using the techniques
C        described in the section on fluxing in the Figaro manual.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be calibrated.
C    (2) CALSPECT   (Char)     The Figaro file containing flux calibration
C                               spectrum.
C    (3) TIME       (Real)     Integration time in seconds for the
C                               spectrum to be calibrated.
C    (4) OUTPUT     (TSP, 1D)  The Output dataset.
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
C    16/8/1988   Original Version.   JAB/AAO
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
      INTEGER XPTR,CPTR,DPTR

*  Array dimensions
      INTEGER NDIM, DIMS(7), ACTDIM

*  Length of file name
      INTEGER LENNAME

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,DLOC,SLOC
      DOUBLE PRECISION DDUMMY

*  File name
      CHARACTER*64 FNAME

*  Label and units
      CHARACTER*64 LABEL,UNITS

*  Stokes parameters in data
      LOGICAL QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      INTEGER ELEMENTS,DUMMY,CSLOT
      CHARACTER*64 STRINGS(2)
      REAL TIME

*  ICH functions
      INTEGER CHR_LEN

*  Get the Input data

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Access the Figaro frame

      CALL PAR_GET0C('CALSPECT',FNAME,STATUS)
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

*  Get Label and Units for data

         CALL DSA_GET_DATA_INFO('INPUT',2,STRINGS,1,DDUMMY,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            LABEL = STRINGS(2)
            UNITS = STRINGS(1)
         ELSE
            LABEL = ' '
            UNITS = ' '
         ENDIF

*  Map data array

         CALL DSA_MAP_DATA('INPUT','READ','FLOAT',CPTR,CSLOT,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             CALL MSG_OUT(' ','Error Mapping calibration data',STATUS)
             STATUS = USER__001
             GOTO 100
         ENDIF

*  Get the integration time

         CALL PAR_GET0R('TIME',TIME,STATUS)

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of output file

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)

*  Check that it matches calibration data

         IF (DIMS(1) .NE. SIZE) THEN
              CALL MSG_OUT('MSG','Wavelength Axes are different Sizes',
     :           STATUS)
              STATUS = USER__001
         ENDIF

*  Map axis array and data

         CALL TSP_MAP_LAMBDA(OLOC,'WRITE',XPTR,XLOC,STATUS)
         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Calibrate Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                  %VAL(DPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)

*  Calibrate variance if present

         STAT = SAI__OK

*  Map the variance

         CALL TSP_MAP_VAR(OLOC,'UPDATE',DPTR,DLOC,STAT)
         IF (STAT .EQ. SAI__OK) THEN

*  Calibrate it twice to scale by the square of the intensity factor

             CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
             CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
         ENDIF


*  Write label and units for clibrated data

         CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Calibrate the Stokes parameters, and variances if present

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*  Q Stokes parameter
             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q stokes data
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN

*  Calibrate the Q stokes data
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
             ENDIF

*  Unmap the array
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Map the Q stokes variance
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN

*  Calibrate it twice to scale by the square of the intensity factor
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  U stokes parameter
             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map the U stokes data
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN

*  Calibrate the U stokes data
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
             ENDIF

*  Unmap the array
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Map the U stokes variance
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN

*  Calibrate it twice to scale by the square of the intensity factor
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))

*  Unmap the array
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  V Stokes parameter
             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map the V stokes data
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN

*  Calibrate the V stokes data
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
             ENDIF

*  Unmap the array
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Map the V stokes variance
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN

*  Calibrate it twice to scale by the sqaure of the intensity factor
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))
                 CALL TSP_SPFLUX(TIME,SIZE,%VAL(XPTR),%VAL(CPTR),
     :                     %VAL(DPTR))

*  Unmap the data
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Unmap output arrays and annul locators

         CALL TSP_UNMAP(XLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

*  Unmap input arrays

      ENDIF
100   CONTINUE

*  Close DSA
      CALL DSA_CLOSE(STATUS)
      END



      SUBROUTINE TSP_SPFLUX (TIME,NX,XDATA,CDATA,DATA)
C+
C     T S P _ S P F L U X
C
C     Applies a calibration spectrum to an observed spectrum to
C     generate a flux calibrated spectrum.  The 'units' of the
C     resulting spectrum are unimportant; this routine assumes that
C     the units of the calibration spectrum are 'whatnots per (count
C     per angstrom per second)', that the units of the X data array
C     is angstroms, and that the spectrum to be calibrated is in
C     counts.  The resulting spectrum will then be in 'whatnots'.
C
C     Parameters -   (">" input, "!" modified)
C
C     (>) TIME    (Real) Exposure time in seconds of spectrum to
C                 be calibrated.
C     (>) NX      (Integer) Number of spectral elements
C     (>) XDATA   (Real array XDATA(NX)) Array giving wavelength
C                 of center of each element.
C     (>) CDATA   (Real array CDATA(NX)) The calibration spectrum.
C     (!) DATA    (Real array DATA(NX)) Passed as the spectrum to
C                 be calibrated, returned calibrated.
C
C     Common variables used -  None
C
C     Functions / subroutines used -
C
C     TSP_WAVEST  Wavelength of start of element
C
C                                          KS / CIT 16th May 1984
C            Adapted from Figaro Version   JAB/AAO  16/8/1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL    TIME, XDATA(NX), CDATA(NX), DATA(NX)
C
C     Functions
C
      REAL TSP_WAVEST
C
C     Local variables
C
      INTEGER IX
      REAL    WEND, WSTART
C
      WSTART=TSP_WAVEST(1,NX,XDATA)
      DO IX=1,NX
         WEND=TSP_WAVEST(IX+1,NX,XDATA)
         DATA(IX)=DATA(IX)*CDATA(IX)/(TIME*ABS(WEND-WSTART))
         WSTART=WEND
      END DO
C
      END



C+
      REAL FUNCTION TSP_WAVEST (IX,NX,XDATA)
C
C     T S P _ W A V E S T
C
C     Figaro holds wavelength data in an X-array where each element
C     holds the value corresponding to the CENTER of the element.
C     This routine calculates - by interpolation - the value that
C     corresponds to the START of the element.  It will also calculate
C     values that lie outside the actual range of the data, so can be
C     used to calculate the value corresponding to the END of the final
C     element by calling it with IX set equal to NX+1, for example.
C
C     Parameters  -   (">" input, "<" output)
C
C     (>) IX     (Integer) The element number
C     (>) NX     (Integer) The number of elements in the array
C     (>) XDATA  (Real array XDATA(NX)) The data array
C
C     Returns
C
C     (<) TSP_WAVEST  (Real) The interpolated start value
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                            KS / CIT 14th May 1984
C          Adapted from Figaro Version       JAB/AAO  16/8/1988
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX, NX
      REAL    XDATA(NX)
C
      IF (IX.LE.1) THEN
         TSP_WAVEST=XDATA(1)-(FLOAT(1-IX)+.5)*(XDATA(2)-XDATA(1))
      ELSE IF (IX.GT.NX) THEN
         TSP_WAVEST=XDATA(NX)+(FLOAT(IX-NX)-.5)*
     :                                (XDATA(NX)-XDATA(NX-1))
      ELSE
         TSP_WAVEST=(XDATA(IX)+XDATA(IX-1))*.5
      END IF
C
      END

