C+
      SUBROUTINE TSEXTRACT(STATUS)
C
C            T S E X T R A C T
C
C     Command name:
C        TSEXTRACT
C
C     Function:
C        Optimal extraction of a light curve from a time series image
C
C     Description:
C        This command performs optimal extraction of a light curve of
C        a star from a time series image, using an algorithm which is
C        a 3 dimensional analogue of Horne's algorithm for optimal
C        extraction of spectra from long slit data.
C        The extraction is performed using a profile time series which is
C        obtained using the TSPROFILE command.
C
C        This command is an alternative to CCDPHOT which performs the
C        same procedure using simple aperture photometry.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset.
C    (2) PROFILE   (TSP, 3D)   The spatial profile dataset.
C    (3) OUTPUT    (TSP, 2D)   The output photometry dataset.
C    (4) LAMBDA    (Real)      Wavelength of observation (microns)
C    (5) FLUXCAL   (Real)      Counts per Jansky
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         16/11/1991
C
C-
C
C  History:
C    16/11/1991   Original Version.   JAB/JAC
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,TLOC,OTLOC,OLOC2,LOC2,LLOC
      CHARACTER*(DAT__SZLOC) PLOC,PPLOC

*  Data pointers
      INTEGER PTR,IPTR,TPTR,OTPTR,OPTR,LPTR,PPTR

*  Array dimensions
      INTEGER NDIMS,DIMS(3)
      INTEGER WPTR
      REAL X,Y,RADIUS,LAMBDA,ZEROPT
      CHARACTER*64 LABEL,UNITS
      INTEGER PNDIMS,PDIMS(3)

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)

*  Map time axis
      CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)

*  Check that data is three dimensional
      IF (NDIMS .NE. 3) THEN
          CALL MSG_OUT(' ','Input must be 3 dimensional',STATUS)
          GOTO 100
      ENDIF

*  Map the data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)

*  Get the profile file
      CALL DAT_ASSOC('PROFILE','READ',PLOC,STATUS)
      CALL TSP_SIZE(PLOC,3,PDIMS,PNDIMS,STATUS)

*  Check Dimensions Match
      IF (PNDIMS .NE. 3) THEN
          CALL MSG_OUT(' ','Profile must be 3 dimensional',STATUS)
          GOTO 100
      ELSE IF (DIMS(1) .NE. PDIMS(1) .OR. DIMS(2) .NE. PDIMS(2)
     :        .OR. DIMS(3) .NE. PDIMS(3)) THEN
          CALL MSG_OUT(' ',
     :         'Dimensions of INPUT and PROFILE are different',
     :         STATUS)
          GOTO 100
      ENDIF

*  Map the profile data
      CALL TSP_MAP_DATA(PLOC,'READ',PPTR,PPLOC,STATUS)

*  Make the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create 2D data structure
      CALL TSP_CREATE_2D(OLOC,1,DIMS(3),' ',.FALSE.,.FALSE.,STATUS)

*  Map the time axis
      CALL TSP_MAP_TIME(OLOC,'WRITE',OTPTR,OTLOC,STATUS)

*  Map the data array
      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Get wavelength
      CALL PAR_GET0R('LAMBDA',LAMBDA,STATUS)
      CALL TSP_MAP_LAMBDA(OLOC,'WRITE',LPTR,LLOC,STATUS)

*  Copy to output file
      CALL TSP_CCDPHOT_LAMBDA(LAMBDA,%VAL(LPTR))
      CALL TSP_UNMAP(LLOC,STATUS)

*  Set units of wavelength
      CALL TSP_WLU_LAMBDA(OLOC,'Wavelength','Angstroms',STATUS)

*  Get zero point
      CALL PAR_GET0R('FLUXCAL',ZEROPT,STATUS)

*  Copy the time axis
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CCDPHOT_COPYT(DIMS(3),%VAL(TPTR),%VAL(OTPTR))
      ENDIF

*  Copy units and label of time axis
      CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)

*  Do the photometry on the frames
      IF (STATUS .EQ. SAI__OK) THEN
           CALL TSP_TSEXTRACT(DIMS(1),DIMS(2),DIMS(3),
     :           ZEROPT,%VAL(PTR),%VAL(PPTR),%VAL(OPTR),
     :           STATUS)
      ENDIF

*  Write label for data
      CALL TSP_WLU(OLOC,'Flux','Jansky',STATUS)

*  Tidy up
100   CONTINUE
      CALL TSP_UNMAP(PPLOC,STATUS)
      CALL DAT_ANNUL(PLOC,STATUS)
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL TSP_UNMAP(OTLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END



      SUBROUTINE TSP_TSEXTRACT(NX,NY,NZ,FLUXCAL,IN,PROFILE,OUT,STATUS)
*+
*
*  T S P _ T S E X T R A C T
*
*  TSEXTRACT command
*
*  Subroutine to do the extraction of the light curve from the time series
*  image
*
*  (>)  NX      (Integer)                First dimension of input data
*  (>)  NY      (Integer)                Second dimension of input data
*  (>)  NZ      (Integer)                Third dimension of input data
*  (>)  FLUXCAL (Real)                   Flux calibration zero point
*  (>)  IN      (Real array(NX,NY,NZ))   Input array
*  (>)  PROFILE (Real array(NX,NY,NZ))   Profile array
*  (<)  OUT     (Real array(NZ))         Output light curve array
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   16/11/1991
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,NZ
      REAL IN(NX,NY,NZ),OUT(NZ),PROFILE(NX,NY,NZ)
      REAL FLUXCAL
      INTEGER STATUS

*  Local variables
      INTEGER IX,IY,IZ
      REAL SUM,SUM2

*  Loop over time points
      DO IZ=1,NZ

*  Zero sums
          SUM = 0
          SUM2 = 0

*  Loop over spatial points
          DO IY=1,NY
              DO IX=1,NX

*  For all good points form sum of profile x input and sum
*  of sqaure of profile
                  IF (IN(IX,IY,IZ) .NE. VAL__BADR .AND.
     :              PROFILE(IX,IY,IZ) .NE. VAL__BADR) THEN
                      SUM=SUM+PROFILE(IX,IY,IZ)*IN(IX,IY,IZ)
                      SUM2=SUM2+PROFILE(IX,IY,IZ)*PROFILE(IX,IY,IZ)
                  ENDIF
              ENDDO
          ENDDO

*  Form weighted mean value in output array
          IF (SUM2 .GT. 0.0) THEN
              OUT(IZ)=SUM/SUM2
          ELSE

*  If no good data set output to bad value
              OUT(IZ)=VAL__BADR
          ENDIF

*  Apply flux calibration
          OUT(IZ)=OUT(IZ)/FLUXCAL
      ENDDO
      END

