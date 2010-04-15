C+
      SUBROUTINE IRFLUX(STATUS)
C
C            I R F L U X
C
C     Command name:
C        IRFLUX
C
C     Function:
C        Apply flux calibration to an infrared polarization spectrum
C
C     Description:
C        IRFLUX flux calibrates a polarization spectrum using a
C        calibration spectrum (normally a standard star observation)
C        which is assumed to be a black body.
C        The parameters of the black body are specified as a temperature,
C        and magnitude in one of the standard bands. As an alternative to
C        the magnitude the flux at a specified wavelength may be given.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input spectrum to be calibrated.
C    (2) CALSPECT   (TSP, 1D)  The calibration spectrum.
C    (3) TEMP       (Real)     Temperature of black body.
C    (4) CALTYPE    (Char)     The type of calibration data. A single
C                                character as follows:
C                          J     => Magnitude in J band
C                          H     => Magnitude in H band
C                          K     => Magnitude in H band
C                          L     => Magnitude in L' band
C                          M     => Magnitude in M band
C                          F     => Flux at specified wavelength
C    (5) MAG        (Real)     The magnitude of the standard.
C        FLUX       (Real)     Flux of standard at calibration wavelength.
C        WAVE       (Real)     Calibration wavelength.
C    (6) OUTPUT     (TSP, 1D)  The Output dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 20/9/1990
C
C-
C
C  History:
C    20/9/1990   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER XPTR,CPTR,DPTR,DEPTR,CEPTR

*  Array sizes
      INTEGER NDIM, DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,DLOC,SLOC,CLOC,CDLOC
      CHARACTER*(DAT__SZLOC) CELOC,DELOC
      CHARACTER*64 LABEL,UNITS
      LOGICAL OK,QZ,UZ,VZ
      INTEGER SIZE,NUM,STAT
      REAL FLAM,WAVE,MAG,TEMP
      CHARACTER*1 CALTYPE
      INTEGER ICH_LEN
      LOGICAL ERROR1,ERROR2

*  Get the Input data

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Access the calibration frame

      CALL DAT_ASSOC('CALSPECT','READ',CLOC,STATUS)

*  Get the data size

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_SIZE(CLOC,7,DIMS,ACTDIM,STATUS)
         SIZE = DIMS(1)

*  Get Label and Units for data

         CALL TSP_RLU(CLOC,LABEL,UNITS,STATUS)

*  Map data array

         CALL TSP_MAP_DATA(CLOC,'READ',CPTR,CDLOC,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             CALL MSG_OUT(' ','Error Mapping calibration data',STATUS)
             STATUS = USER__001
             GOTO 100
         ENDIF

*  Map the variance array if there is one, otherwise map a dummy array

         CALL TSP_MAP_VAR(CLOC,'READ',CEPTR,CELOC,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             STATUS = SAI__OK
             CALL TSP_TEMP(SIZE,'_REAL',CEPTR,CELOC,STATUS)
             ERROR2 = .FALSE.
         ELSE
             ERROR2 = .TRUE.
         ENDIF

*  Get the temperature of the standard

         CALL PAR_GET0R('TEMP',TEMP,STATUS)

*  Get type of calibration data

         CALL PAR_GET0C('CALTYPE',CALTYPE,STATUS)
         CALL CHR_UCASE(CALTYPE)

*  Is it one of the standard bands
         IF (CALTYPE .EQ. 'J' .OR. CALTYPE .EQ. 'K' .OR.
     :        CALTYPE .EQ. 'H' .OR. CALTYPE .EQ. 'L' .OR.
     :        CALTYPE .EQ. 'M') THEN

*  Get the magnitude of the standard
              CALL PAR_GET0R('MAG',MAG,STATUS)

*  and calculate the zero point and set the wavelength
              IF (CALTYPE .EQ. 'J') THEN
                  FLAM = 1650000*10**(-0.4*MAG)
                  WAVE = 1.25
              ELSE IF (CALTYPE .EQ. 'H') THEN
                  FLAM = 1060000*10**(-0.4*MAG)
                  WAVE = 1.64
              ELSE IF (CALTYPE .EQ. 'K') THEN
                  FLAM = 650000*10**(-0.4*MAG)
                  WAVE = 2.2
              ELSE IF (CALTYPE .EQ. 'L') THEN
                  FLAM = 250000*10**(-0.4*MAG)
                  WAVE = 3.8
              ELSE IF (CALTYPE .EQ. 'M') THEN
                  FLAM = 150000*10**(-0.4*MAG)
                  WAVE = 4.8
              ENDIF

*  If it is flux at agiven wvelength get the flux and wavelength
          ELSE IF (CALTYPE .EQ. 'F') THEN
              CALL PAR_GET0R('FLUX',FLAM,STATUS)
              CALL PAR_GET0R('WAVE',WAVE,STATUS)
          ELSE

*  Otherwise CALTYPE is illegal
              CALL MSG_OUT(' ','CALTYPE must be J, H, K, L, M or F',
     :              STATUS)
              GO TO 500
          ENDIF

*  Get the output file
         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output
         CALL TSP_COPY(ILOC,OLOC,STATUS)

*  Check size matches that of calibration
         CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
         IF (DIMS(1) .NE. SIZE) THEN
              CALL MSG_OUT('MSG','Wavelength Axes are different Sizes',
     :           STATUS)
              STATUS = USER__001
         ENDIF

*  Map axis array and data
         CALL TSP_MAP_LAMBDA(OLOC,'READ',XPTR,XLOC,STATUS)
         CALL TSP_MAP_DATA(OLOC,'UPDATE',DPTR,DLOC,STATUS)

*  Map the variance array if there is one, otherwise map a dummy array
         CALL TSP_MAP_VAR(OLOC,'UPDATE',DEPTR,DELOC,STATUS)
         IF (STATUS .NE. SAI__OK) THEN
             STATUS = SAI__OK
             CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
             ERROR1 = .FALSE.
         ELSE
             ERROR1 = .TRUE.
         ENDIF

*  Calibrate Intensity data
         IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_IRFLUX(ERROR1,ERROR2,SIZE,FLAM,WAVE,TEMP,
     :          %VAL(DPTR),%VAL(DEPTR),%VAL(CPTR),%VAL(CEPTR),
     :          %VAL(XPTR))
         ENDIF

*  Unmap the arrays
         CALL TSP_UNMAP(DELOC,STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)

*  Write label and units for calibrated data
         LABEL = 'Flux'
         Units = 'mJy'
         CALL TSP_WLU(OLOC,LABEL,UNITS,STATUS)

*  Calibrate the Stokes parameters, and variances if present
         CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
         IF (QZ) THEN

*   Q Stokes parameter
             CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*   Map the data and variance
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*   Calibrate stokes data
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_IRFLUX(ERROR1,ERROR2,SIZE,FLAM,WAVE,TEMP,
     :             %VAL(DPTR),%VAL(DEPTR),%VAL(CPTR),%VAL(CEPTR),
     :             %VAL(XPTR))
             ENDIF

*   Unmap arrays
             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (UZ) THEN

*  U Stokes parameter
             CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map data and variance
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*  Calibrate U Stokes data
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_IRFLUX(ERROR1,ERROR2,SIZE,FLAM,WAVE,TEMP,
     :             %VAL(DPTR),%VAL(DEPTR),%VAL(CPTR),%VAL(CEPTR),
     :             %VAL(XPTR))
             ENDIF

*  Unmap arrays
             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF
         IF (VZ) THEN

*  V Stokes parameter
             CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map data and variance
             CALL TSP_MAP_DATA(SLOC,'UPDATE',DPTR,DLOC,STATUS)
             CALL TSP_MAP_VAR(SLOC,'UPDATE',DEPTR,DELOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 STATUS = SAI__OK
                 CALL TSP_TEMP(SIZE,'_REAL',DEPTR,DELOC,STATUS)
                 ERROR1 = .FALSE.
             ELSE
                 ERROR1 = .TRUE.
             ENDIF

*  Calibrate V Stokes data
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL TSP_IRFLUX(ERROR1,ERROR2,SIZE,FLAM,WAVE,TEMP,
     :             %VAL(DPTR),%VAL(DEPTR),%VAL(CPTR),%VAL(CEPTR),
     :             %VAL(XPTR))
             ENDIF

*  Unmap arrays
             CALL TSP_UNMAP(DELOC,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)
             CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Unmap arrays and annul locators

500      CONTINUE
         CALL TSP_UNMAP(CDLOC,STATUS)
         CALL TSP_UNMAP(CELOC,STATUS)
         CALL DAT_ANNUL(CLOC,STATUS)
         CALL TSP_UNMAP(XLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

      ENDIF
100   CONTINUE
      END


      SUBROUTINE TSP_IRFLUX(ERROR1,ERROR2,N,FLAM,WAVE,TEMP,STAR,ESTAR,
     :     STAN,ESTAN,LAMBDA)
C+
C
C     T S P _ I R F L U X
C
C     Divide a spectrum by a standard taking account of errors
C     and generating a flux calibrated result in mJy
C
C     (>)  ERROR1    (Logical) - True if the star spectrum has errors
C     (>)  ERROR2    (Logical) - True if the standard spectrum has errors
C     (>)  N         (Integer) - Number of elements of the spectra
C     (>)  FLAM      (Real)    - Flux of standard at calibration wavelength
C     (>)  WAVE      (Real)    - Calibration wavelength
C     (>)  TEMP      (Real)    - Temperature of standard
C     (!)  STAR      (Real array STAR(N)) - The star spectrum
C     (!)  ESTAR     (Real array ESTAR(N)) - The errors on the star spectrum
C     (>)  STAN      (Real array STAN(N)) - The standard spectrum
C     (>)  ESTAN     (Real array ESTAN(N)) - The errors on the standard
C     (>)  LAMBDA    (Real array LAMBDA(N)) - The wavelength array
C
C     Jeremy Bailey    20/9/1990
C
C+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
C
C     Parameters
C
      LOGICAL ERROR1,ERROR2
      INTEGER N
      REAL FLAM,WAVE,TEMP
      REAL STAR(N), ESTAR(N), STAN(N), ESTAN(N), LAMBDA(N)
C
C     Local variables
C
      INTEGER I
      REAL E1, E2, F
C
C     Functions
C
      REAL LFLUX
C
      DO I=1,N
       IF (STAR(I) .NE. VAL__BADR .AND. STAN(I) .NE. VAL__BADR) THEN
C
C     Handle the errors - combine the fractional errors quadratically
C
         E1=0.
         E2=0.
         IF (ERROR1) THEN
            IF (STAR(I).NE.0.) E1=SQRT(ESTAR(I))/STAR(I)
	 ENDIF
         IF (ERROR2) THEN
            IF (STAN(I).NE.0.) E2=SQRT(ESTAN(I))/STAN(I)
         ENDIF
C
C     Divide star by standard
C
         STAR(I)=STAR(I)/STAN(I)
         E1=SQRT(E1*E1+E2*E2)
C
C     Convert to flux
C
         F = LFLUX(FLAM,WAVE,TEMP,LAMBDA(I))
         STAR(I)=STAR(I)*F
         IF (ERROR1) ESTAR(I)=(E1*STAR(I))**2
        ELSE
         STAR(I) = VAL__BADR
         ESTAR(I) = VAL__BADR
        ENDIF
      ENDDO
      END



      REAL FUNCTION LFLUX(FLAM,WAVE,TEMP,L)
C+
C
C     L F L U X
C
C     Return the flux in mJy of a black body which has flux FLAM
C     at wavelength WAVE, temperature T.
C     Return the value for wavelength L microns
C
C     (>)  FLAM     (Real)        Flux at wavelength WAVE
C     (>)  WAVE     (Real)        Wavelength at which flux is FLAM
C     (>)  TEMP     (Real)        Black body temperature
C     (>)  L        (Real)        Wavelength to calculate result for
C
C     Returns   LFLUX   (Real)    Flux at wavelenght L
C
C     Jeremy Bailey    20/9/1990
C
C+
      REAL WAVE,FLAM,L,TEMP
      REAL F,LC,F22,L2
C
C     Calculate flux in mJy
C
      LC = L*1E-4
      L2 = WAVE*1E-4
      F = 1.0/(LC*LC*LC*(EXP(1.43883/(LC*TEMP))-1))
      F22 = 1.0/(L2*L2*L2*(EXP(1.43883/(L2*TEMP))-1))
C
C     Convert to mJy
C
      LFLUX=(F*FLAM)/F22
      END

