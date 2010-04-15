C+
      SUBROUTINE FLCONV(STATUS)
C
C            F L C O N V
C
C     Command name:
C        FLCONV
C
C     Function:
C        Convert a flux calibrated spectrum to f-lambda
C
C     Description:
C        A polarization spectrum flux calibrated in f-nu (Jy, mJy or
C        micro-Jy) is converted to f-lambda (ergs/sec/cm**2/A). The
C        units of the original data are sensed from the UNITS field
C        of the axis structure. The spectrum must have a wavelength
C        axis in Angstroms.
C
C        This program is similar to the Figaro command of the same name,
C        but applies the calibration to the Stokes parameters as well
C        as to the intensity data.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be converted.
C    (2) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support:
C        Jeremy Bailey, AAO
C
C     Version date:
C        6/12/1991
C
C-
C
C  History:
C    16/8/1990   Original Version.   JAB/AAO
C    6/12/1991   Handle bad values.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER DPTR,XPTR

*  Array size information
      INTEGER NDIM, DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,DLOC,SLOC,XLOC
      LOGICAL OK,QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      CHARACTER*40 LABEL,UNITS
      INTEGER NTYPE,NCH

*  Get the Input data

         CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Get the output file

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of data

         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
         SIZE = DIMS(1)

*  Map intensity data

         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Map X-axis (wavelength) data

         CALL TSP_MAP_LAMBDA(OLOC,'READ',XPTR,XLOC,STATUS)

*  Get units of data

         CALL TSP_RLU(OLOC,LABEL,UNITS,STATUS)
         NTYPE = 0

*  Convert to upper case

         CALL CHR_UCASE(UNITS)

*  Find which type of units

         IF ((INDEX(UNITS,'JANSKY').NE.0).OR.
     :       (INDEX(UNITS,'JY').NE.0)) THEN
             IF (INDEX(UNITS,'MICRO').NE.0) THEN
                 NTYPE = 3
             ELSE IF ((INDEX(UNITS,'MILLI').NE.0) .OR.
     :           (INDEX(UNITS,'MJY').NE.0)) THEN
                 NTYPE = 2
             ELSE
                 NTYPE = 1
             ENDIF
         ENDIF

*  Complain if the units string is not recognized

         IF (NTYPE .EQ. 0) THEN
             CALL MSG_OUT(' ','Don''t know how to convert from '//
     :           UNITS,STATUS)
             STATUS = USER__001
         ENDIF

*  Scale Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)

*  Scale the variance if present

         STAT = SAI__OK
         CALL TSP_MAP_VAR(OLOC,'UPDATE',DPTR,DLOC,STAT)
         IF (STAT .EQ. SAI__OK) THEN
             CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
             CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
         ENDIF

*  Scale the Stokes parameters, and variances if present

*  Get Stokes parameters present in data

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*  Get the Q data and scale it

             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*   Get the Q variance and scale it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  Get the U stokes data and scale it

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Get the U variance and scale it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  Get the V stokes data and scale it

             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)
             STAT = SAI__OK

*  Get the V variance and scale it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STAT .EQ. SAI__OK) THEN
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_FLCONV(SIZE,NTYPE,%VAL(XPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Set new units

         UNITS = 'Ergs/s/cm**2/A'
         CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Unmap output arrays and annul locators

         CALL TSP_UNMAP(XLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

      END



      SUBROUTINE TSP_FLCONV (NX,NTYPE,X,DATA)
C+
C
C     T S P _ F L C O N V
C
C     Convert data from F-nu to F-lambda
C
C     (>)   NX    (Integer)         Number of data points
C     (>)   NTYPE (Integer)         Type of input data
C                                    1 = Jansky
C                                    2 = Millijansky
C                                    3 = Microjansky
C     (>)   X     (Real array(NX))  Wavelength array
C     (!)   DATA  (Real array(NX))  Data to be converted
C
C     Jeremy Bailey    16/8/1990
C
C     Modified:
C        6/12/1991    Handle bad values
C
C+

      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      INTEGER NX
      INTEGER NTYPE
      REAL    X(NX), DATA(NX)
C
C     Local variables
C
      INTEGER IX
      REAL SCALES(3)

      DATA SCALES/1.0,0.001,0.000001/

      IF (NTYPE .LE.0 .OR. NTYPE .GT. 3) RETURN
      DO IX=1,NX
        IF (DATA(IX) .NE. VAL__BADR) THEN
          DATA(IX)=DATA(IX)*SCALES(NTYPE)*(2.998E-5/(X(IX)*X(IX)))
        ENDIF
      END DO
C
      END

