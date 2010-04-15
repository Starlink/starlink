C+
      SUBROUTINE TEXTIN(STATUS)
C
C            T E X T I N
C
C     Command Name:
C        TEXTIN
C
C     Function:
C        Correct a time series dataset for extinction.
C
C     Description:
C        Correct a dataset for extinction by calculating the airmass
C        of each point and correcting the intensity to a value for
C        airmass 1.
C
C        The extinction coefficients (magnitude per airmass) for each
C        channel must be provided.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) OUTPUT     (TSP, 2D)  The output extinction corrected dataset.
C    (3) RA         (Char)     The B1950 mean Right Ascension of the
C                               observed source.
C    (4) DEC        (Char)     The B1950 mean declination of the observed
C                               source.
C    (5) OBS        (Char)     Observing station (? for list), NO to give
C                               longitude and latitude explicitly.
C        LONG       (Double)   Longitude of site (degrees, west +ve)
C        LAT        (Double)   Geodetic latitude of site (degrees, north +ve)
C        EXTCOEF    (Real)     Extinction coefficient (prompt is repeated
C                              for each channel).
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 24/2/1992
C
C-
C
C  History:
C    24/2/1992   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  PI
      DOUBLE PRECISION DPI
      PARAMETER (DPI=3.141592653589793238462643D0)

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,LOC,ALOC,ELOC,DLOC,SLOC

*  Array sizes
      INTEGER SIZE,CHANS
      INTEGER DIMS(7),NDIMS

*  Pointers
      INTEGER PTR,APTR,EPTR,DPTR
      CHARACTER*80 RASTRING,DECSTRING
      INTEGER IH,ID,IM
      DOUBLE PRECISION SEC,RM,DM
      INTEGER J,SIGN
      INTEGER NSTRT

      CHARACTER*64 LABEL,UNITS
      CHARACTER*64 OBS
      CHARACTER*10 C
      CHARACTER*40 NAME
      DOUBLE PRECISION LONG,LAT,HT
      INTEGER I
      LOGICAL QZ,UZ,VZ
      LOGICAL DONE
      INTEGER N
      INTEGER STAT
      INTEGER NUM

*  Get Locators to the input and output datasets

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Get size of data

      CALL TSP_SIZE(OLOC,7,DIMS,NDIMS,STATUS)
      SIZE = DIMS(NDIMS)
      IF (NDIMS .EQ. 3) THEN
         CHANS = DIMS(1)*DIMS(2)
      ELSE IF (NDIMS .EQ. 2) THEN
         CHANS = DIMS(1)
      ELSE
         CALL MSG_OUT(' ','Data has incorrect number of dimensions',
     :             STATUS)
         GOTO 500
      ENDIF

*  Map the time axis of the output data set

      CALL TSP_MAP_TIME(OLOC,'UPDATE',PTR,LOC,STATUS)

*  Read label and units

      CALL TSP_RLU_TIME(OLOC,LABEL,UNITS,STATUS)

*  Get RA and Dec

      CALL PAR_GET0C('RA',RASTRING,STATUS)
      CALL PAR_GET0C('DEC',DECSTRING,STATUS)

*  Decode RA

      IF (STATUS .EQ. SAI__OK) THEN
         NSTRT = 1
         CALL SLA_INTIN(RASTRING,NSTRT,IH,J)
         CALL SLA_INTIN(RASTRING,NSTRT,IM,J)
         CALL SLA_DFLTIN(RASTRING,NSTRT,SEC,J)
         CALL SLA_DTF2R(IH,IM,SEC,RM,J)
         IF (J .NE. 0) THEN
             CALL MSG_OUT('MSG','Invalid RA',STATUS)
             GOTO 500
         ENDIF

*  Decode Dec

         NSTRT = 1

*  Degrees and sign

         CALL SLA_INTIN(DECSTRING,NSTRT,ID,J)
         IF (J .EQ. -1) THEN
             ID = -ID
             SIGN = -1
         ELSE
             SIGN = 1
         ENDIF

*  Minutes and seconds

         CALL SLA_INTIN(DECSTRING,NSTRT,IM,J)
         CALL SLA_DFLTIN(DECSTRING,NSTRT,SEC,J)
         CALL SLA_DAF2R(ID,IM,SEC,DM,J)
         IF (SIGN .EQ. -1) THEN
             DM = -DM
         ENDIF
         IF (J .NE. 0) THEN
             CALL MSG_OUT('MSG','Invalid DEC',STATUS)
             GOTO 500
         ENDIF

*  Get the observing station

         DONE = .FALSE.
         DO WHILE (.NOT. DONE)
             CALL PAR_GET0C('OBS',OBS,STATUS)

*  Convert to upper case
             CALL CHR_UCASE(OBS)
             IF (STATUS .NE. SAI__OK) THEN
                 GOTO 500
             ELSE IF (OBS .EQ. 'HELP') THEN

*  If it is HELP output a list of possible station identifiers

                 N=1
                 DO WHILE(NAME .NE. '?')
                     CALL SLA_OBS(N,C,NAME,LONG,LAT,HT)
                     N=N+1
                     IF (NAME .NE. '?') THEN
                         CALL MSG_OUT(' ',C//NAME,STATUS)
                     ENDIF
                 ENDDO
                 CALL PAR_CANCL('OBS',STATUS)
             ELSE IF (OBS .EQ. 'NO ') THEN

*  If it is NO prompt for the longitude and latitude . . .

                 CALL PAR_GET0D('LONG',LONG,STATUS)
                 CALL PAR_GET0D('LAT',LAT,STATUS)

*  . . . and convert to radians

                 LONG = LONG * DPI / 180D0
                 LAT = LAT * DPI / 180D0
                 DONE = .TRUE.
             ELSE

*  Otherwise get the station parameters

                 CALL SLA_OBS(0,OBS,NAME,LONG,LAT,HT)
                 IF (NAME .EQ. '?') THEN
                     CALL PAR_CANCL('OBS',STATUS)
                 ELSE
                     DONE = .TRUE.
                 ENDIF
             ENDIF
         ENDDO

         CALL TSP_TEMP(SIZE,'_REAL',APTR,ALOC,STATUS)
         CALL TSP_AIRMASS(RM,DM,LONG,LAT,SIZE,%VAL(PTR),
     :        %VAL(APTR),STATUS)

         CALL TSP_TEMP(CHANS,'_REAL',EPTR,ELOC,STATUS)
         DO I=1,CHANS
             CALL MSG_SETI('CHN',I)
             CALL MSG_OUT(' ',
     :        'Enter Extinction Coefficient for channel ^CHN',STATUS)
             CALL PAR_GET0R('EXTCOEF',%VAL(EPTR+(I-1)*4),STATUS)
             CALL PAR_CANCL('EXTCOEF',STATUS)
         ENDDO

*  Map intensity data

         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Correct Intensity data

         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
         ENDIF
         CALL TSP_UNMAP(DLOC,STATUS)

*  Correct variance data

         CALL TSP_MAP_VAR(OLOC,'UPDATE',DPTR,DLOC,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
             CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
             CALL TSP_UNMAP(DLOC,STATUS)
         ELSE
             CALL ERR_ANNUL(STATUS)
         ENDIF

*  Scale the Stokes parameters, and variances if present

*  Find stokes parameters present in the data

         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*  Get Q data and correct it

             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)

*  Get Q variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  Get U Stokes data and correct it

             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)

*  Get U variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  Get V stokes data and correct it

             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
             ENDIF
             CALL TSP_UNMAP(DLOC,STATUS)

*  Get V variance and correct it

             CALL TSP_MAP_VAR(SLOC,'UPDATE',DPTR,DLOC,STAT)
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_TEXTIN(SIZE,CHANS,%VAL(APTR),
     :                   %VAL(EPTR),%VAL(DPTR))
                 CALL TSP_UNMAP(DLOC,STATUS)
             ELSE
                 CALL ERR_ANNUL(STATUS)
             ENDIF
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
      ENDIF

*  Unmap data and annul locators

      CALL TSP_UNMAP(LOC,STATUS)

500   CONTINUE
      CALL DAT_ANNUL(ILOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      IF (STATUS .EQ. USER__001) STATUS = SAI__OK
      END



      SUBROUTINE TSP_TEXTIN (NX,NC,AM,C,DATA)
C+
C
C     T S P _ E X T I N
C
C     Apply extinction correction to a data array. The array C is an
C     array of extinction coefficients as a function of wavelength which
C     is used to calculate the extinction correction to the data. The array
C     AM is an array of airmasses for each point in the time series.
C
C     (>)  NX   (Integer)        Number of points in time series
C     (>)  NC   (Integer)        Number of channels
C     (>)  AM   (Real array(NX)) Airmass of observation
C     (>)  C    (Real array(NC)) Array of extinction coefficients
C     (!)  DATA (Real array(NC,NX)) Array of data to be calibrated
C
C     Jeremy Bailey   26/2/1992
C
C     Modified:
C        6/12/1991   Handle bad values
C        26/2/1992   Adapt from EXTIN
C
C+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      INTEGER NX,NC
      REAL AM(NX)
      REAL C(NC), DATA(NC,NX)
C
C     Local variables
C
      INTEGER IX,IC

      DO IX=1,NX
        DO IC=1,NC
          IF (DATA(IC,IX) .NE. VAL__BADR .AND.
     :              C(IC) .NE. VAL__BADR) THEN
             DATA(IC,IX)=DATA(IC,IX)*10.0**(0.4*C(IC)*(AM(IX)-1))

          ELSE
             DATA(IC,IX) = VAL__BADR
          ENDIF
        ENDDO
      END DO
C
      END




      SUBROUTINE TSP_AIRMASS(RM,DM,LONG,LAT,SIZE,MJD,AM,STATUS)
*+
*
*   T S P _ A I R M A S S
*
*   TEXTIN command  (Airmass calculation)
*
*   Calculate the airmass of an object for each time point in a time series
*   and return the results in the array AM.
*
*   (>)  RM       (Double)          B1950 RA of object
*   (>)  DM       (Double)          B1950 Dec of object
*   (>)  SIZE     (Integer)         Size of time axis array.
*   (>)  MJD      (Double array(SIZE)) Array of MJD times.
*   (<)  AM       (Real array(SIZE)) Array of airmasses.
*   (!)  STATUS   (Integer)         Status value.
*
*   Jeremy Bailey    24/2/1992
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      DOUBLE PRECISION RM,DM,LONG,LAT,MJD(SIZE)
      REAL AM(SIZE)
      INTEGER STATUS

*  Local variables
      INTEGER I
      DOUBLE PRECISION R2000,D2000,RA,DA,HA,ZD,ST

*  Functions
      DOUBLE PRECISION SLA_GMST,SLA_EQEQX,SLA_ZD,SLA_AIRMAS

*  Convert position to FK5
      CALL SLA_FK45Z(RM,DM,1950.0D0,R2000,D2000)

*  Convert to apparent - we make the assumption that the apparent position
*  doesn't change significantly over the duration of the time series
*  so that we can do this just once.
      CALL SLA_MAP(R2000,D2000,0D0,0D0,0D0,0D0,2000D0,MJD(1),RA,DA)

*  Loop over points in the time series
      DO I=1,SIZE

*  Calculate local apparent sidereal time (We ignore the difference between
*  UTC and UT1, and the fact that SLA_EQEQX should really use TDB rather than
*  UTC)
          ST = SLA_GMST(MJD(I))-LONG+SLA_EQEQX(MJD(I))

*  Calculate hour angle and zenith distance
          HA = ST-RA
          ZD = SLA_ZD(HA,DA,LAT)

*  Calculate air mass
          AM(I) = REAL(SLA_AIRMAS(ZD))
      ENDDO

*  Report range of air masses
      CALL MSG_SETR('AM1',AM(1))
      CALL MSG_SETR('AM2',AM(SIZE))
      CALL MSG_OUT(' ','Air mass ranges from ^AM1 to ^AM2',STATUS)
      END


