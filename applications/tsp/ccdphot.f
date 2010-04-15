C+
      SUBROUTINE CCDPHOT(STATUS)
C
C            C C D P H O T
C
C     Command name:
C        CCDPHOT
C
C     Function:
C        Photometry of a star on a time series image
C
C     Description:
C        Measure the brightness of a star on each frame of a time
C        series image, and generate a 2D TSP dataset containing the
C        resulting time series photometry. The data should previously
C        have been sky subtracted.
C
C        The photometry is done by means of summing the signal
C        within a specified aperture. An alternative method is to
C        use the commands TSPROFILE and TSEXTRACT which perform
C        photometry weighting according to a smoothed spatial
C        profile.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset.
C    (2) OUTPUT    (TSP, 2D)   The output photometry dataset
C    (3) X         (Real)      X position of centre of star
C    (4) Y         (Real)      Y position of centre of star
C    (5) RADIUS    (Real)      Radius of aperture (pixels)
C    (6) LAMBDA    (Real)      Wavelength of observation (microns)
C    (7) FLUXCAL   (Real)      Counts per Jansky
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 20/03/2000
C
C-
C
C  History:
C    26/10/1989   Original Version.   JAB/JAC
C    29/10/1989   Add WEIGHT option   JAB/JAC
C    1/11/1989    Add photometry zero point  JAB/JAC
C    20/11/1990   Remove WEIGHT option - This is now done with
C                    the separate command TSEXTRACT    JAB/JAC
C    20/03/2000   ZEROPT changed from INTEGER to REAL.   BLY/RAL
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,TLOC,OTLOC,OLOC2,LOC2,LLOC

*  Data Pointers
      INTEGER PTR,NDIMS,IPTR,DIMS(3),TPTR,OTPTR,OPTR,LPTR,PPTR
      INTEGER WPTR
      REAL X,Y,RADIUS,LAMBDA,ZEROPT
      CHARACTER*64 LABEL,UNITS

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Map the input data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)

*  Map the time axis
      CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)

*  Make the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create output array with dimensions of 1 by Number of time points
      CALL TSP_CREATE_2D(OLOC,1,DIMS(3),' ',.FALSE.,.FALSE.,STATUS)

*  Map the time axis and data
      CALL TSP_MAP_TIME(OLOC,'WRITE',OTPTR,OTLOC,STATUS)
      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Get parameters of aperture
      CALL PAR_GET0R('X',X,STATUS)
      CALL PAR_GET0R('Y',Y,STATUS)
      CALL PAR_GET0R('RADIUS',RADIUS,STATUS)

*  Get wavelength
      CALL PAR_GET0R('LAMBDA',LAMBDA,STATUS)
      CALL TSP_MAP_LAMBDA(OLOC,'WRITE',LPTR,LLOC,STATUS)
      CALL TSP_CCDPHOT_LAMBDA(LAMBDA,%VAL(LPTR))
      CALL TSP_UNMAP(LLOC,STATUS)
      CALL TSP_WLU_LAMBDA(OLOC,'Wavelength','Angstroms',STATUS)

*  Get zero point
      CALL PAR_GET0R('FLUXCAL',ZEROPT,STATUS)

*  Copy the time axis
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CCDPHOT_COPYT(DIMS(3),%VAL(TPTR),%VAL(OTPTR))
      ENDIF
      CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)

*  Do the photometry on the frames
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CCDPHOT(DIMS(1),DIMS(2),DIMS(3),X,Y,RADIUS,
     :           ZEROPT,%VAL(PTR),%VAL(OPTR))
          CALL TSP_CCDPHOTM(DIMS(3),%VAL(OPTR),STATUS)
      ENDIF

*  Write label for data
      CALL TSP_WLU(OLOC,'Flux','Jansky',STATUS)

*  Tidy up
100   CONTINUE
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL TSP_UNMAP(OTLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END

