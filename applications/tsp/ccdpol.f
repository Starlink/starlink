
C+
      SUBROUTINE CCDPOL(STATUS)
C
C            C C D P O L
C
C     Command name:
C        CCDPOL
C
C     Function:
C        Polarimetry of a star on a time series image
C
C     Description:
C        Measure the brightness of the O and E images of a star on each
C        frame of a time series image, and generate a 2D TSP dataset
C        containing the resulting time series polarimetry. The data should
C        previously have been sky subtracted.
C
C        Aperture photometry is performed on each of the two star images
C        and used to derive the polarization. A polarization offset can
C        be applied to correct for instrumental effects.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset.
C    (2) OUTPUT    (TSP, 2D)   The output photometry dataset
C    (3) XE        (Real)      X position of centre of E image
C    (4) YE        (Real)      Y position of centre of E image
C    (5) RADIUS    (Real)      Radius of aperture (pixels)
C    (6) XO        (Real)      X position of centre of O image
C    (7) YO        (Real)      Y position of centre of O image
C    (8) LAMBDA    (Real)      Wavelength of observation (microns)
C    (9) STOKESPAR (Real)      Stokes Parameter (Q,U,V)
C    (10) OFFSET   (Real)      Polarization offset (per cent)
C    (11) FLUXCAL  (Real)      Counts per Jansky
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 2/4/1995
C
C-
C
C  History:
C    26/10/1989   Original Version.   JAB/JAC
C    1/11/1989    Add OFFSET          JAB/JAC
C    1/11/1989    Add FLUXCAL         JAB/JAC
C    2/4/1995     Reorder code and add additional NDF_BEGIN, NDF_END pairs to get
C                  round ARY_UNMAP error     JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,TLOC,OTLOC,OLOC2,LOC2,LLOC
      CHARACTER*(DAT__SZLOC) IELOC,IOLOC,SLOC,SLOC2,VLOC,VVLOC

*  Data pointers
      INTEGER PTR,NDIMS,IPTR,DIMS(3),TPTR,OTPTR,OPTR,LPTR,VPTR
      INTEGER IEPTR,IOPTR,SPTR,VVPTR
      REAL XE,YE,XO,YO,RADIUS,LAMBDA
      CHARACTER*64 LABEL,UNITS,STOKESPAR
      REAL OFFSET,FLUXCAL

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Map the input data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)



*  Make the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)
      CALL TSP_CREATE_2D(OLOC,1,DIMS(3),' ',.TRUE.,.FALSE.,STATUS)



*  Temporary arrays for O and E photometry
      CALL TSP_TEMP(DIMS(3),'_REAL',IEPTR,IELOC,STATUS)
      CALL TSP_TEMP(DIMS(3),'_REAL',IOPTR,IOLOC,STATUS)

*  Get parameters of 1st aperture
      CALL PAR_GET0R('XE',XE,STATUS)
      CALL PAR_GET0R('YE',YE,STATUS)
      CALL PAR_GET0R('RADIUS',RADIUS,STATUS)

*  check that it is within image
      IF (XE+RADIUS .GT. REAL(DIMS(1)) .OR. XE-RADIUS .LT. 1.0
     :  .OR. YE+RADIUS .GT. REAL(DIMS(2)) .OR. YE-RADIUS .LT. 1.0) THEN
          CALL MSG_OUT(' ','Aperture is outside image',STATUS)
          GOTO 100
      ENDIF

*  Get parameters of second aperture
      CALL PAR_GET0R('XO',XO,STATUS)
      CALL PAR_GET0R('YO',YO,STATUS)

*  Check that it is within image
      IF (XO+RADIUS .GT. REAL(DIMS(1)) .OR. XO-RADIUS .LT. 1.0
     :  .OR. YO+RADIUS .GT. REAL(DIMS(2)) .OR. YO-RADIUS .LT. 1.0) THEN
          CALL MSG_OUT(' ','Aperture is outside image',STATUS)
          GOTO 100
      ENDIF

*  Get wavelength
      CALL PAR_GET0R('LAMBDA',LAMBDA,STATUS)
      CALL TSP_MAP_LAMBDA(OLOC,'WRITE',LPTR,LLOC,STATUS)

*  Write wavelength to output array
      CALL TSP_CCDPHOT_LAMBDA(LAMBDA,%VAL(LPTR))
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL TSP_WLU_LAMBDA(OLOC,'Wavelength','Angstroms',STATUS)

*  Get Stokes parameter
      CALL PAR_GET0C('STOKESPAR',STOKESPAR,STATUS)


*  Add Stokes parameter to output dataset and map its data and variance
      IF (STOKESPAR .EQ. 'Q' .OR. STOKESPAR .EQ. 'U' .OR.
     :     STOKESPAR .EQ. 'V') THEN
          CALL TSP_ADD_STOKES(OLOC,STOKESPAR,.TRUE.,STATUS)
          CALL TSP_GET_STOKES(OLOC,STOKESPAR,SLOC,STATUS)

      ELSE
          CALL MSG_OUT(' ','Invalid Stokes Parameter',STATUS)
          GOTO 200
      ENDIF

*  Get OFFSET
      CALL PAR_GET0R('OFFSET',OFFSET,STATUS)

*  Get Flux calibration
      CALL PAR_GET0R('FLUXCAL',FLUXCAL,STATUS)

*  Copy the time axis
*  Map the input and output time axes
      CALL NDF_BEGIN
      CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)
      CALL TSP_MAP_TIME(OLOC,'WRITE',OTPTR,OTLOC,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CCDPHOT_COPYT(DIMS(3),%VAL(TPTR),%VAL(OTPTR))
      ENDIF
      CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)
      CALL NDF_END(STATUS)

*  Do the photometry on the frames
      IF (STATUS .EQ. SAI__OK) THEN
          CALL NDF_BEGIN
          CALL TSP_CCDPHOT(DIMS(1),DIMS(2),DIMS(3),XE,YE,RADIUS,
     :       FLUXCAL,%VAL(PTR),%VAL(IOPTR))
          CALL TSP_CCDPHOT(DIMS(1),DIMS(2),DIMS(3),XO,YO,RADIUS,
     :       FLUXCAL,%VAL(PTR),%VAL(IEPTR))

          CALL TSP_UNMAP(LOC2,STATUS)
          CALL TSP_UNMAP(TLOC,STATUS)
          CALL TSP_UNMAP(OTLOC,STATUS)

*  Map the output data and variance
          CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)
          CALL TSP_MAP_VAR(OLOC,'WRITE',VVPTR,VVLOC,STATUS)
          CALL TSP_MAP_DATA(SLOC,'WRITE',SPTR,SLOC2,STATUS)
          CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)

*  Combine the two sets of photometry to give the polarization
          CALL TSP_CCDPOL(DIMS(3),%VAL(IEPTR),%VAL(IOPTR),OFFSET,
     :       %VAL(SPTR),%VAL(OPTR),%VAL(VPTR),%VAL(VVPTR),STATUS)
          CALL NDF_END(STATUS)
      ENDIF

*  Write label for data
       CALL TSP_WLU(OLOC,'Flux','Jansky',STATUS)


*  Tidy up
100   CONTINUE
      CALL TSP_UNMAP(SLOC2,STATUS)
      CALL TSP_UNMAP(VLOC,STATUS)
      CALL DAT_ANNUL(SLOC,STATUS)
200   CONTINUE
      CALL TSP_UNMAP(VVLOC,STATUS)
      CALL TSP_UNMAP(IELOC,STATUS)
      CALL TSP_UNMAP(IOLOC,STATUS)

      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)

      END


