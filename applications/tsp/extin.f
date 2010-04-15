C+
      SUBROUTINE EXTIN(STATUS)
C
C            E X T I N
C
C     Command name:
C        EXTIN
C
C     Function:
C        Correct a polarization spectrum for extinction
C
C     Description:
C        Correct a polarization spectrum for extinction using a coefficient
C        spectrum containing the interpolated extinxtion coefficients over
C        the wavelength range of the spectrum. This can be generated using
C        Figaro as described in the section on extinction in the Figaro
C        manual.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be corrected.
C    (2) COEFF      (Char)     The name of the Figaro file containing
C                                the coefficient spectrum.
C    (3) AIRMASS    (Real)     The air-mass (approximately sec z) of
C                                the observation.
C    (4) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C           Jeremy Bailey, AAO
C
C     Version date:
C           6/12/1991
C
C-
C
C  History:
C    16/8/1990   Original Version.   JAB/AAO
C    6/12/1991   Handle bad values.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER DPTR,CPTR

*  Array sizes
      INTEGER NDIM, DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,DLOC,SLOC
      LOGICAL OK,QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      CHARACTER*40 LABEL,UNITS
      INTEGER NTYPE,NCH
      INTEGER CSLOT
      INTEGER ELEMENTS
      CHARACTER*64 FNAME
      REAL AM

*  ICH functions
      INTEGER ICH_FOLD, ICH_CLEAN

*  Get the Input data

         CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Get the Figaro coefficient spectrum

         CALL PAR_GET0C('COEFF',FNAME,STATUS)
         CALL DSA_OPEN(STATUS)
         CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)
         IF (STATUS .NE. SAI__OK) GOTO 100

*  Find size of data
         CALL DSA_DATA_SIZE('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)
         SIZE = DIMS(1)

*  Map the coefficient data
         CALL DSA_MAP_DATA('INPUT','READ','FLOAT',CPTR,CSLOT,STATUS)
         IF (STATUS .NE. SAI__OK) GOTO 100

*  Get the airmass

         CALL PAR_GET0R('AIRMASS',AM,STATUS)

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get the data size

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)

*  Check that it matches the calibration spectrum size

         IF (DIMS(1) .NE. SIZE) THEN
             CALL MSG_OUT(' ','Spectra have different sizes',STATUS)
             STATUS = USER__001
         ENDIF

*  Map intensity data

         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Correct Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)

*  Correct variance data

         STAT = SAI__OK
         CALL TSP_MAP_VAR(OLOC,'UPDATE',DPTR,DLOC,STAT)
         IF (STAT .EQ. SAI__OK) THEN
             CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
             CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
         ENDIF

*  Scale the Stokes parameters, and variances if present

*  Find stokes parameters present in the data

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*  Get Q data and correct it

             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Get Q variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  Get U Stokes data and correct it

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Get U variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  Get V stokes data and correct it

             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Get V variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_EXTIN(SIZE,AM,%VAL(CPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Unmap output arrays and annul locators

         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

*  Close down DSA

100      CONTINUE
         CALL DSA_CLOSE(STATUS)

      END



      SUBROUTINE TSP_EXTIN (NX,AM,C,DATA)
C+
C
C     T S P _ E X T I N
C
C     Apply extinction correction to a data array. The array C is an
C     array of extinction coefficients as a function of wavelength which
C     is used to calcualte the extinction correction to the data for a
C     given airmass.
C
C     (>)  NX   (Integer)        Size of data arrays
C     (>)  AM   (Real)           Airmass of observation
C     (>)  C    (Real array(NX)) Array of extinction coefficients
C     (!)  DATA (Real array(NX)) Array of data to be calibrated
C
C     Jeremy Bailey   16/8/1990
C
C     Modified:
C        6/12/1991   Handle bad values
C
C+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      INTEGER NX
      REAL AM
      REAL C(NX), DATA(NX)
C
C     Local variables
C
      INTEGER IX

      DO IX=1,NX
        IF (DATA(IX) .NE. VAL__BADR .AND. C(IX) .NE. VAL__BADR) THEN
          DATA(IX)=DATA(IX)*10.0**(0.4*C(IX)*AM)
        ELSE
          DATA(IX) = VAL__BADR
        ENDIF
      END DO
C
      END

