*+  REDS_PHOTOM - routine to reduce SCUBA PHOTOM data
      SUBROUTINE REDS_PHOTOM (STATUS)
*    Description :
*        This routine reduces the data for a single sub-instrument from a PHOTOM
*     observation. For each bolometer used to look at the source the data will
*     be analysed as follows:-
*
*       An ndf called <bolname>_map (e.g. h7_map) will be created in the OUT 
*       file to hold the coadded data from all the integrations. If the
*       jiggle pattern points fit a 2-d rectangular pattern then these data
*       will be arranged as a 2-d map suitable for plotting as an image. A
*       2-d parabola will be fitted to the coadded image and the results 
*       written in ASCII form to FILE.
*
*       Second, an ndf called <bolname>_peak (e.g. h7_peak) will be created in
*       the OUT file to hold the fit results to the data for each 
*       integration. The results stored are the fit peak, its variance and
*       quality and they are held as a 1-d array suitable for plotting as
*       a graph. The fit results are also written in ASCII form to FILE, as
*       is the coadd of all the individual fits to the data.
*
*     The parameters used are:-
*
*     IN                      The name of the input file containing 
*                             demodulated SCUBA data.
*
*     OUT                     The name of the HDS output file to contain
*                             the ndfs described above. This file will have
*                             the extension .sdf but this should not be
*                             specified in the name.
*
*     FILE                    The name of the ASCII output file.
*
*
*        In more detail the routine works as follows. If status is good on
*     entry the routine opens the IN file, reads some FITS items describing
*     the observation and reports them to the user. A check is made that the
*     data does come from a PHOTOM observation. The file `history' is read
*     and a check made that the REDUCE_SWITCH, EXTINCTION and FLATFIELD
*     applications have been run on it but that PHOTOM has not. A note is
*     taken if the SKY_ERROR application has been run to remove `sky noise'
*     error from the data.
*        Other FITS items are read, mainly so that they can be passed on to
*     the ASCII output file. These are the coordinate system and coordinates
*     of the telescope centre, the coordinate system and magnitude of the
*     offsets of the source from the centre, the date and time of the
*     observation.
*        Next, the components of the main data array are mapped and their
*     dimensions checked. All the component integrations of the observation
*     are butted end to end in this array so the `pointer' array is also 
*     mapped, containing the start and finish indices of each exposure. 
*        The indices of the target bolometers in each `beam' are read from
*     the input file together with the weights to be associated with the
*     data from each. A search is made for the INT_QUALITY array in the
*     REDS extension of the input file. This may have been placed there
*     by the REDS MODIFY application to specify those integrations whose
*     data are to be ignored in the reduction. If no such array is found
*     then data from all the integrations will be used.
*        The jiggle pattern used is read from the file along with the
*     coordinate system of the offsets. SCULIB_CALC_GRID is called
*     to determine whether or not the jiggle pattern conforms to a square
*     grid. If it does then the coadded integrations for each bolometer
*     wll be stored as a small map in a 2-d ndf in the OUT file, otherwise
*     they will be stored as a 1-d vector.
*        Next some scratch memory is obtained to hold the integration data
*     for each target bolometer and the relevant data copied into it from 
*     the IN data array.
*        The name of the OUT file is read from the parameter and the file
*     created. The routine cycles through the 3 possible beams that can
*     be used in a PHOTOM observation and, if a bolometer was working in
*     that beam, creates 2 ndfs in the OUT file, one called <bolname>_map
*     to hold the coadded integration result and a second called <bolname>_peak
*     to hold the fit results to the data for each integration. The weight
*     to be given to this bolometer's data is stored in the REDS.BEAM_WT
*     object in the MORE structure of each ndf.
*        The routine cycles through the integrations taken and for each one, 
*     if INT_QUALITY is 0, calls SCULIB_COADD to add it to the coadd result and
*     SCULIB_FIT_2D_PARABOLA to fit a parabola to it. The fit results are
*     themselves coadded. When all the integrations have been dealt with
*     SCULIB_FIT_2D_PARABOLA is called to fit a parabola to the coadded map
*     and SCULIB_UNPACK_JIGGLE_SEPARATES to store the map to the output ndf.
*     After the integration fit results have been copied to their ndf the
*     analysis is complete and REDS_WRITE_PHOTOM is called to write the ASCII
*     version of the results to the file whose name is given in parameter
*     FILE.
*        Lastly, the IN and OUT files are closed.
*    Invocation :
*     CALL REDS_PHOTOM (STATUS)
*    Parameters :
*     STATUS          = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (JFL/ROE)
*    History :
*     $Id$
*     16-JUL-1995: Original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'REDS_SYS'               ! REDS constants
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN                  ! CHR used-string-length function
*    Global variables :
*    Local Constants :
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
*    Local variables :
      INTEGER          BEAM            ! beam index in DO loop
      REAL             BEAM_WEIGHT (SCUBA__MAX_BEAM)
                                       ! the weight assigned to the measurement
                                       ! in each beam
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      CHARACTER*15     CENTRE_COORDS   ! coordinate system of telescope centre
      INTEGER          DEM_PNTR_ARY    ! ARY pointer to SCUBA.DEM_PNTR extension
      INTEGER          DEM_PNTR_PTR    ! array pointer to SCUBA.DEM_PNTR array
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      REAL             EXPOSURE_TIME   ! exposure time per jiggle point
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      CHARACTER*15     FILTER          ! the name of the filter being used
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
				       ! array of FITS keywords
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      INTEGER          I               ! DO loop index
      INTEGER          IBEAM           ! ndf identifier
      INTEGER          INT             ! integration index in DO loop
      INTEGER          INT_D_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration data
      INTEGER          INT_D_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration 
                                       ! data
      INTEGER          INT_OFFSET      ! offset of the start of an integration
                                       ! in the data array
      INTEGER          INT_QUALITY_END ! end of space holding array that
                                       ! specifies which integrations from the
                                       ! input file are to be used
      INTEGER          INT_QUALITY_PTR ! start of space holding array that
                                       ! specifies which integrations from the
                                       ! input file are to be used
      INTEGER          INT_Q_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration
                                       ! quality
      INTEGER          INT_Q_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration
                                       ! quality
      INTEGER          INT_V_END (SCUBA__MAX_BEAM)
                                       ! end of space holding integration
                                       ! variance
      INTEGER          INT_V_PTR (SCUBA__MAX_BEAM)
                                       ! start of space holding integration
                                       ! variance
      CHARACTER*15     IN_CENTRE_COORDS! coord system of telescope centre in
                                       ! an input file
      INTEGER          IN_D_PTR        ! pointer to input data array
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_LOC    ! locator of item in input file
      INTEGER          IN_OFFSET       ! offset in input array
      INTEGER          IN_NDF          ! NDF index of input file
      INTEGER          IN_Q_PTR        ! pointer to input quality array
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC
                                       ! locator to REDS extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                       ! locator to SCUCD extension in input
                                       ! file
      INTEGER          IN_V_PTR        ! pointer to input variance array
      INTEGER          IPEAK           ! ndf identifier
      INTEGER          IPOS (SCUBA__MAX_JIGGLE)
				       ! i index of jiggle measurement in
				       ! 2d map
      INTEGER          ITEMP           ! scratch integer
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      REAL             JIGGLE_2D_A1 (SCUBA__MAX_JIGGLE)
                                       ! x-axis of jiggle map
      REAL             JIGGLE_2D_A2 (SCUBA__MAX_JIGGLE)
                                       ! y-axis of jiggle map
      INTEGER          JPOS (SCUBA__MAX_JIGGLE)
				       ! j index of jiggle measurement in
				       ! 2d map
      CHARACTER*15     LAT             ! the latitude of the telescope centre
      CHARACTER*15     LAT2            ! the second latitude of the telescope
                                       ! centre for a PLANET coord system
      INTEGER          LBND (2)        ! pixel indices of bottom left corner
                                       ! of output image
      CHARACTER*15     LONG            ! the longitude of the telescope centre
      CHARACTER*15     LONG2           ! the second longitude of the telescope
                                       ! centre for a PLANET coord system
      INTEGER          MAP_D_PTR       ! pointer to output map data array
      INTEGER          MAP_Q_PTR       ! pointer to output map quality array
      INTEGER          MAP_V_PTR       ! pointer to output map variance array
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      REAL             MEAS_D (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the coadded data for each beam
      INTEGER          MEAS_N (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
                                       ! number of coadded integrations in
                                       ! MEAS_D
      INTEGER          MEAS_Q (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the quality on MEAS_D
      REAL             MEAS_V (SCUBA__MAX_JIGGLE, SCUBA__MAX_BEAM)
				       ! the variance on MEAS_D
      REAL             MEAS_1_A0 (SCUBA__MAX_BEAM)
				       ! the a0 of the parabola fit to the
				       ! coadded measurement
      REAL             MEAS_1_A1 (SCUBA__MAX_BEAM)
				       ! the a1 of the parabola fit to the
				       ! coadded measurement
      REAL             MEAS_1_D (SCUBA__MAX_BEAM)
				       ! the fitted peak to the coadded
				       ! integrations
      INTEGER          MEAS_1_Q (SCUBA__MAX_BEAM)
				       ! the quality on MEAS_1_D
      REAL             MEAS_1_V (SCUBA__MAX_BEAM)
				       ! the variance on MEAS_1_D
      REAL             MEAS_1_X (SCUBA__MAX_BEAM)
				       ! the x offset of the fitted peak
      REAL             MEAS_1_Y (SCUBA__MAX_BEAM)
				       ! the y offset of the fitted peak
      REAL             MEAS_2_D (SCUBA__MAX_BEAM)
				       ! the coadd of the peaks fitted to
				       ! the individual integrations for
				       ! each bolometer
      INTEGER          MEAS_2_N (SCUBA__MAX_BEAM)
                                       ! the number of integrations coadded
      INTEGER          MEAS_2_Q (SCUBA__MAX_BEAM)
				       ! the quality on MEAS_2_D
      REAL             MEAS_2_V (SCUBA__MAX_BEAM)
				       ! the variance on MEAS_2_D
      DOUBLE PRECISION MJD1            ! the Julian date at which the object
                                       ! was at LAT,LONG
      DOUBLE PRECISION MJD2            ! the Julian date at which the object
                                       ! was at LAT2, LONG2
      CHARACTER*15     NDF_NAME        ! name of ndf
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NELM            ! number of array elements mapped
      INTEGER          NREC            ! number of history records in input file
      INTEGER          N_BOLS          ! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_OBSDIM        ! number of dimensions in jiggle map
      INTEGER          N_POS           ! number of positions measured in input
                                       ! file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*64     ODF_NAME        ! name of observation definition file
      CHARACTER*15     OFFSET_COORDS   ! coord system of MAP_X and MAP_Y
      CHARACTER*(DAT__SZLOC) OUT_LOC   ! locator of HDS container file for
				       ! output
      CHARACTER*80     OUT             ! name of output HDS container file
      INTEGER          OUT_OFFSET      ! offset in output array
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! pointer to REDS extension in output
                                       ! NDFs
      REAL             PEAK_D (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! fitted peaks
      INTEGER          PEAK_Q (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! quality on fitted peaks
      REAL             PEAK_V (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! variance on fitted peaks
      REAL             PEAK_X (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! x coord of fitted peak
      REAL             PEAK_Y (SCUBA__MAX_INT, SCUBA__MAX_BEAM)
                                       ! y coord of fitted peak
      INTEGER          PEAK_D_PTR      ! pointer to PEAK_D array, holding
				       ! fitted peaks in ndf
      INTEGER          PEAK_Q_PTR      ! pointer to PEAK_Q array, holding
				       ! quality of fitted peaks in ndf
      INTEGER          PEAK_V_PTR      ! pointer to PEAK_V array, holding
				       ! variance of fitted peaks in ndf
      LOGICAL          PHOTOM          ! .TRUE. if the PHOTOM application
                                       ! has already been run on the file
      INTEGER          PHOT_BB (SCUBA__MAX_BEAM)
                                       ! index of target bolometers in 
                                       ! input data array
      INTEGER          PLACE           ! place holder for ndf in output file
      INTEGER          POS             ! measurement index in DO loop
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      LOGICAL          SKY_ERROR       ! .TRUE. if SKY_ERROR application has
                                       ! been run on the data
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*15     SUB_INSTRUMENT  ! the sub-instrument used to make the
                                       ! maps
      INTEGER          TEMP_PTR        ! temporary array pointer
      INTEGER          UBND (2)        ! pixel indices of top right corner
                                       ! of output image
      CHARACTER*15     UTDATE          ! date of input observation
      CHARACTER*15     UTSTART         ! UT of start of input observation
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
      REAL             XMAX            ! maximum x jiggle offset
      REAL             XMIN            ! minimum x jiggle offset
      REAL             XSPACE          ! spacing between x jiggle offsets
      REAL             YMAX            ! maximum y jiggle offset
      REAL             YMIN            ! minimum y jiggle offset
      REAL             YSPACE          ! spacing between y jiggle offsets
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  start up the NDF system and read in the input demodulated file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', IN_NDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'REDS', 'READ', IN_REDSX_LOC, STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_PHOTOM: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS, 
     :  STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBSDEF',
     :  ODF_NAME, STATUS)
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

*  get the number of history records present in the file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

*  check that the mode and history of the input file are OK

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.
         FLATFIELD = .FALSE.
         SKY_ERROR = .FALSE.
         PHOTOM = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               ELSE IF (STEMP .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP .EQ. 'SKY_ERROR') THEN
                  SKY_ERROR = .TRUE.
               ELSE IF (STEMP .EQ. 'PHOTOM') THEN
                  PHOTOM = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (OBSERVING_MODE .NE. 'PHOTOM') THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: the file '//
     :           'does not contain data from a PHOTOM observation',
     :           STATUS)
            END IF
         END IF
 
         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

            IF (.NOT. EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: the '//
     :           'EXTINCTION application has not been run on '//
     :           'the input file', STATUS)
            END IF

            IF (.NOT. FLATFIELD) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: the '//
     :           'FLATFIELD application has not been run on '//
     :           'the input file', STATUS)
            END IF

            IF (PHOTOM) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: the '//
     :           'PHOTOM application has already been run on '//
     :           'the input file', STATUS)
            END IF
         END IF
      END IF

*  report the run number and object of the observation

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_OUT (' ', 'REDS: run ^RUN was a PHOTOM observation '//
     :  'of ^OBJECT', STATUS)

*  get the sub-instrument and filter used 

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SUB_1',
     :  SUB_INSTRUMENT, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'WAVE_1',
     :  WAVELENGTH, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'FILT_1',
     :  FILTER, STATUS)

*  get some other FITS items that will be needed

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'EXP_TIME', EXPOSURE_TIME, STATUS)
      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*  coords of telescope centre

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :  'CENT_CRD', CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :  LAT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :  LONG, STATUS)

      IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT2',
     :     LAT2, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG2',
     :     LONG2, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD1',
     :     MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD2',
     :     MJD2, STATUS)
      END IF

*  offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_X',
     :  MAP_X, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_Y',
     :  MAP_Y, STATUS)
      OFFSET_COORDS = 'UNKNOWN'

*  the UT of the observation expressed as modified Julian day

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTDATE',
     :  UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTSTART',
     :  UTSTART, STATUS)

*  map the various components of the data array and check the data
*  dimensions

      CALL NDF_DIM (IN_NDF, MAX_DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_D_PTR,
     :  NELM, STATUS)
      CALL NDF_MAP (IN_NDF, 'VARIANCE', '_REAL', 'READ', IN_V_PTR,
     :  NELM, STATUS)
      CALL NDF_MAP (IN_NDF, 'QUALITY', '_INTEGER', 'READ', IN_Q_PTR,
     :  NELM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 3)                  .OR.
     :       (DIM(1) .NE. N_BOLS)           .OR.
     :       (DIM(2) .LT. 1)                .OR.
     :       (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETI ('DIM3', DIM(3))
            CALL ERR_REP (' ', 'REDS_PHOTOM: data array '//
     :        'has bad dimensions (^NDIM) ^DIM1, ^DIM2, ^DIM3', STATUS)
         END IF
      END IF

      N_POS = DIM (2)

*  map the DEM_PNTR array and check its dimensions

*     CALL NDF_XIARY (IN_NDF, 'SCUBA', 'DEM_PNTR', 'READ',
*    :  IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', DEM_PNTR_ARY,
     :  STATUS)
      CALL ARY_DIM (DEM_PNTR_ARY, MAX_DIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (DEM_PNTR_ARY, '_INTEGER', 'READ',
     :  DEM_PNTR_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 3) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_PHOTOM: .SCUBA.DEM_PNTR '//
     :        'array has bad number of dimensions', STATUS)
         ELSE
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1',DIM(1))
               CALL ERR_REP (' ', 'REDS_PHOTOM: .SCUBA.DEM_PNTR '//
     :           'array contains bad number of exposures - ^DIM1',
     :           STATUS)
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2',DIM(2))
               CALL ERR_REP (' ', 'REDS_PHOTOM: .SCUBA.DEM_PNTR '//
     :           'array contains bad number of integrations - ^DIM2',
     :           STATUS)
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3',DIM(3))
               CALL ERR_REP (' ', 'REDS_PHOTOM: .SCUBA.DEM_PNTR '//
     :           'array contains bad number of measurements - ^DIM3',
     :           STATUS)
            END IF
         END IF

         N_EXPOSURES = DIM (1)
         N_INTEGRATIONS = DIM (2)
         N_MEASUREMENTS = DIM (3)
      END IF

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_OUT (' ', 'REDS: file contains data for ^N_E '//
     :  'exposure(s) in ^N_I integrations(s) in ^N_M '//
     :  'measurement(s)', STATUS)

*  get the target indices of the bolometers in the data array

      CALL CMP_GETVI (IN_SCUBAX_LOC, 'PHOT_BB', SCUBA__MAX_BEAM,
     :  PHOT_BB, NELM, STATUS)

*  and the associated weights

      CALL CMP_GETVR (IN_REDSX_LOC, 'BEAM_WT', SCUBA__MAX_BEAM,
     :  BEAM_WEIGHT, NELM, STATUS)

*  search for an integration quality array in the REDS extension, if present
*  map it otherwise get some space and fill it with good quality

      CALL SCULIB_MALLOC (N_INTEGRATIONS * SCUBA__MAX_BEAM * VAL__NBI,
     :  INT_QUALITY_PTR, INT_QUALITY_END, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL DAT_FIND (IN_REDSX_LOC, 'INT_QUALITY', IN_LOC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
	    CALL SCULIB_CFILLI (N_INTEGRATIONS * SCUBA__MAX_BEAM,
     :        0, %val(INT_QUALITY_PTR))
         ELSE
            CALL DAT_GETVI (IN_LOC, N_INTEGRATIONS * SCUBA__MAX_BEAM,
     :        %val(INT_QUALITY_PTR), NELM, STATUS)
            CALL DAT_ANNUL (IN_LOC, STATUS)
         END IF
      END IF

*  get the channel and ADC numbers of the bolometers used

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_CHAN', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  BOL_CHAN, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_PHOTOM: dimension '//
     :        'of .SCUBA.BOL_CHAN does not match main data array',
     :        STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_ADC', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  BOL_ADC, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_PHOTOM: dimension '//
     :        'of .SCUBA.BOL_ADC does not match main data array',
     :        STATUS)
         END IF
      END IF

*  now read in data specific to the sample mode of the observation

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'JIGL_CNT', JIGGLE_COUNT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'J_REPEAT', JIGGLE_REPEAT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'J_PER_S', JIGGLE_P_SWITCH, STATUS)

*  the jiggle pattern itself

         CALL DAT_FIND (IN_SCUCDX_LOC, 'JIGL_X', IN_LOC, STATUS)
         CALL DAT_GET1R (IN_LOC, SCUBA__MAX_JIGGLE, JIGGLE_X, ITEMP, 
     :     STATUS)
         CALL DAT_ANNUL (IN_LOC, STATUS)

         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: mismatch between '//
     :           'JIGGLE_COUNT and number of X jiggle offsets read',
     :           STATUS)
            END IF
         END IF

         CALL DAT_FIND (IN_SCUCDX_LOC, 'JIGL_Y', IN_LOC, STATUS)
         CALL DAT_GET1R (IN_LOC, SCUBA__MAX_JIGGLE, JIGGLE_Y, ITEMP, 
     :     STATUS)
         CALL DAT_ANNUL (IN_LOC, STATUS)

         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_PHOTOM: mismatch between '//
     :           'JIGGLE_COUNT and number of Y jiggle offsets read',
     :           STATUS)
            END IF
         END IF

*  the jiggle coordinate system and its rotation

	 CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_CRDS', SAMPLE_COORDS, STATUS)
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_PA', SAMPLE_PA, STATUS)

*  find out if the jiggle pattern corresponds to a rectangular grid

	 IF (STATUS .EQ. SAI__OK) THEN
	    LBND (1) = 1
	    LBND (2) = 1

            CALL SCULIB_CALC_GRID (JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y,
     :        XMIN, XMAX, XSPACE, UBND(1), YMIN, YMAX, YSPACE, 
     :        UBND(2), IPOS, JPOS, STATUS)

	    IF ((STATUS .NE. SAI__OK) .OR.
     :          (ABS(XSPACE/YSPACE - 1.0) .GT. 0.001)) THEN
	       CALL ERR_ANNUL (STATUS)

	       N_OBSDIM = 1
	       UBND (1) = JIGGLE_COUNT
	       CALL MSG_OUT (' ', 'REDS: the jiggle pattern does '//
     :           'not fit a rectangular mesh, no images will be '//
     :           'stored', STATUS)
	    ELSE

*  construct axes for 2D map

	       N_OBSDIM = 2

	       DO I = 1, UBND (1)
		  JIGGLE_2D_A1 (I) = XMIN + REAL (I-1) * XSPACE
               END DO
	       DO I = 1, UBND (2)
		  JIGGLE_2D_A2 (I) = YMIN + REAL (I-1) * YSPACE
               END DO
            
	    END IF
         END IF
      END IF

      DO BEAM = 1, SCUBA__MAX_BEAM
         IF (PHOT_BB(BEAM) .NE. 0) THEN

*  get some scratch memory to hold the data for the target bolometers

            CALL SCULIB_MALLOC (N_POS * VAL__NBR, INT_D_PTR(BEAM), 
     :        INT_D_END(BEAM), STATUS)
            CALL SCULIB_MALLOC (N_POS * VAL__NBR, INT_V_PTR(BEAM),
     :        INT_V_END(BEAM), STATUS)
            CALL SCULIB_MALLOC (N_POS * VAL__NBI, INT_Q_PTR(BEAM),
     :        INT_Q_END(BEAM), STATUS)

*  copy the integration data from the relevant bolometer into the 
*  temporary space - clumsy method

	    IF (STATUS .EQ. SAI__OK) THEN
		DO POS = 1, N_POS
                   IN_OFFSET = (BEAM-1) * N_POS * N_BOLS + (POS-1) *
     :               N_BOLS + (PHOT_BB(BEAM) - 1)
                   OUT_OFFSET = POS - 1
 
		   CALL SCULIB_COPYR (1, %val(IN_D_PTR + IN_OFFSET *
     :               VAL__NBR), %val(INT_D_PTR(BEAM) + OUT_OFFSET *
     :               VAL__NBR))
                   CALL SCULIB_COPYR (1, %val(IN_V_PTR + IN_OFFSET *
     :               VAL__NBR), %val(INT_V_PTR(BEAM) + OUT_OFFSET *
     :               VAL__NBR))
		   CALL SCULIB_COPYI (1, %val(IN_Q_PTR + IN_OFFSET *
     :               VAL__NBI), %val(INT_Q_PTR(BEAM) + OUT_OFFSET *
     :               VAL__NBI))
                END DO
             END IF
         END IF
      END DO

*  create the output file that will contain the reduced data in NDFs

      CALL PAR_GET0C ('OUT', OUT, STATUS)
      CALL HDS_NEW (OUT, OUT, 'REDS_PHOTOM', 0, 0, OUT_LOC, STATUS)

*  cycle through the beams used in this sub-instrument

      IF (STATUS .EQ. SAI__OK) THEN

         DO BEAM = 1, SCUBA__MAX_BEAM
	    MEAS_1_Q (BEAM) = 1

	    MEAS_2_N (BEAM) = 0

	    DO POS = 1, SCUBA__MAX_JIGGLE
               MEAS_N (POS,BEAM) = 0
            END DO

            IF (PHOT_BB(BEAM) .NE. 0) THEN

*  this sub-instrument / beam combination was used, so analyse 
*  and store the data

	       CALL NDF_BEGIN 

*  first, create the NDF to hold the map data, called <bol>_map

	       CALL SCULIB_BOLNAME (BOL_ADC(PHOT_BB(BEAM)),
     :           BOL_CHAN(PHOT_BB(BEAM)), NDF_NAME, STATUS)
	       NDF_NAME = NDF_NAME(:CHR_LEN(NDF_NAME))//'_map'

	       CALL NDF_PLACE (OUT_LOC, NDF_NAME, PLACE, STATUS)
               CALL NDF_SCOPY (IN_NDF, 'NOEXTENSION(SCUBA)', PLACE, 
     :           IBEAM, STATUS)
               CALL NDF_SBND (N_OBSDIM, LBND, UBND, IBEAM, STATUS)

	       CALL NDF_MAP (IBEAM, 'DATA', '_REAL', 'WRITE/ZERO',
     :           MAP_D_PTR, NELM, STATUS)
	       CALL NDF_MAP (IBEAM, 'VARIANCE', '_REAL', 'WRITE/ZERO',
     :           MAP_V_PTR, NELM, STATUS)
	       CALL NDF_MAP (IBEAM, 'QUALITY', '_INTEGER', 'WRITE',
     :           MAP_Q_PTR, NELM, STATUS)

*  initialise quality to bad

	       IF (STATUS .EQ. SAI__OK) THEN
		  CALL SCULIB_CFILLI (NELM, 1, %val(MAP_Q_PTR))
               END IF

*  construct axes

	       IF (N_OBSDIM .EQ. 1) THEN
	          CALL NDF_AMAP (IBEAM, 'CENTRE', 1, '_REAL',
     :              'WRITE', TEMP_PTR, NELM, STATUS)
		  IF (STATUS .EQ. SAI__OK) THEN
		     CALL SCULIB_NFILLR (JIGGLE_COUNT, %val(TEMP_PTR))
                  END IF
		  CALL NDF_AUNMP (IBEAM, 'CENTRE', 1, STATUS)
               ELSE IF (N_OBSDIM .EQ. 2) THEN
		  CALL NDF_AMAP (IBEAM, 'CENTRE', 1, '_REAL',
     :              'WRITE', TEMP_PTR, NELM, STATUS)
		  IF (STATUS .EQ. SAI__OK) THEN
		     CALL SCULIB_COPYR (UBND(1), JIGGLE_2D_A1,
     :                 %val(TEMP_PTR))
		  END IF
		  CALL NDF_AUNMP (IBEAM, 'CENTRE', 1, STATUS)

		  CALL NDF_AMAP (IBEAM, 'CENTRE', 2, '_REAL',
     :              'WRITE', TEMP_PTR, NELM, STATUS)
		  IF (STATUS .EQ. SAI__OK) THEN
		     CALL SCULIB_COPYR (UBND(2), JIGGLE_2D_A2,
     :                 %val(TEMP_PTR))
		  END IF
		  CALL NDF_AUNMP (IBEAM, 'CENTRE', 2, STATUS)
               END IF

*  set the beam weight

	       IF (STATUS .EQ. SAI__OK) THEN
                  CALL NDF_XLOC (IBEAM, 'REDS', 'UPDATE',
     :              OUT_REDSX_LOC, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_ANNUL (STATUS)
		     CALL NDF_XNEW (IBEAM, 'REDS', 'REDS_EXTENSION',
     :                 0, 0, OUT_REDSX_LOC, STATUS)
                  END IF
                  CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL',
     :              0, 0, STATUS)
                  CALL CMP_PUT0R (OUT_REDSX_LOC, 'BEAM_WT', 
     :              BEAM_WEIGHT(BEAM), STATUS)
                  CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)
               END IF

*  now create the NDF to hold the fitted peaks for each integration,
*  called <bol>_peak

	       CALL SCULIB_BOLNAME (BOL_ADC(PHOT_BB(BEAM)),
     :           BOL_CHAN(PHOT_BB(BEAM)), NDF_NAME, STATUS)
	       NDF_NAME = NDF_NAME(:CHR_LEN(NDF_NAME))//'_peak'

	       CALL NDF_PLACE (OUT_LOC, NDF_NAME, PLACE, STATUS)
               CALL NDF_SCOPY (IN_NDF, 'NOEXTENSION(SCUBA)', PLACE, 
     :           IPEAK, STATUS)
               CALL NDF_SBND (1, 1, N_INTEGRATIONS, IPEAK, STATUS)

               CALL NDF_MAP (IPEAK, 'DATA', '_REAL', 'WRITE/ZERO',
     :           PEAK_D_PTR, NELM, STATUS)
               CALL NDF_MAP (IPEAK, 'VARIANCE', '_REAL', 'WRITE/ZERO',
     :           PEAK_V_PTR, NELM, STATUS)
               CALL NDF_MAP (IPEAK, 'QUALITY', '_INTEGER', 'WRITE',
     :           PEAK_Q_PTR, NELM, STATUS)

*  initialise quality to bad

	       IF (STATUS .EQ. SAI__OK) THEN
		  CALL SCULIB_CFILLI (NELM, 1, %val(PEAK_Q_PTR))
               END IF

*  construct axis

	       CALL NDF_AMAP (IPEAK, 'CENTRE', 1, '_INTEGER',
     :           'WRITE', TEMP_PTR, NELM, STATUS)
	       IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_NFILLI (N_INTEGRATIONS, %val(TEMP_PTR))
               END IF
               CALL NDF_AUNMP (IPEAK, 'CENTRE', 1, STATUS)

*  set the beam weight

	       IF (STATUS .EQ. SAI__OK) THEN
                  CALL NDF_XLOC (IPEAK, 'REDS', 'UPDATE',
     :              OUT_REDSX_LOC, STATUS)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL ERR_ANNUL (STATUS)
                     CALL NDF_XNEW (IPEAK, 'REDS', 'REDS_EXTENSION',
     :                 0, 0, OUT_REDSX_LOC, STATUS)
                  END IF
                  CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL',
     :              0, 0, STATUS)
                  CALL CMP_PUT0R (OUT_REDSX_LOC, 'BEAM_WT',
     :              BEAM_WEIGHT(BEAM), STATUS)
                  CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)
               END IF

*  cycle through the integrations coadding them as required

               DO INT = 1, N_INTEGRATIONS
                  INT_OFFSET = (INT - 1) * SCUBA__MAX_BEAM + BEAM - 1
                  IF (STATUS .EQ. SAI__OK) THEN
                     CALL SCULIB_COPYI (1, %val(INT_QUALITY_PTR +
     :                 INT_OFFSET * VAL__NBI), ITEMP)
                  END IF

                  IF (ITEMP .EQ. 0) THEN
		     INT_OFFSET = (INT - 1) * JIGGLE_COUNT

*  coadd the jiggle data for the integration into that for the measurement

		     IF (STATUS .EQ. SAI__OK) THEN
                        CALL SCULIB_COADD (JIGGLE_COUNT,
     :                    %val(INT_D_PTR(BEAM) + INT_OFFSET * VAL__NBR),
     :                    %val(INT_V_PTR(BEAM) + INT_OFFSET * VAL__NBR),
     :                    %val(INT_Q_PTR(BEAM) + INT_OFFSET * VAL__NBI),
     :                    MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :                    MEAS_Q(1,BEAM), MEAS_N(1,BEAM),
     :                    MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :                    MEAS_Q(1,BEAM), MEAS_N(1,BEAM),
     :                    .TRUE.)
             
*  derive the measured signal for this integration

                        CALL SCULIB_FIT_2D_PARABOLA (JIGGLE_COUNT,
     :                    %val(INT_D_PTR(BEAM) + INT_OFFSET * VAL__NBR),
     :                    %val(INT_V_PTR(BEAM) + INT_OFFSET * VAL__NBR),
     :                    %val(INT_Q_PTR(BEAM) + INT_OFFSET * VAL__NBR),
     :                    JIGGLE_X, JIGGLE_Y,
     :                    RTEMP, RTEMP,
     :                    PEAK_X(INT,BEAM), PEAK_Y(INT,BEAM),
     :                    PEAK_D(INT,BEAM), PEAK_V(INT,BEAM),
     :                    STATUS)

                        IF (STATUS .EQ. SAI__OK) THEN
                           PEAK_Q(INT,BEAM) = 0
                        END IF

*  coadd the fitted peak into the running mean

                        CALL SCULIB_COADD (1, 
     :                    PEAK_D(INT,BEAM), PEAK_V(INT,BEAM),
     :                    PEAK_Q(INT,BEAM),
     :                    MEAS_2_D(BEAM), MEAS_2_V(BEAM), 
     :                    MEAS_2_Q(BEAM), MEAS_2_N(BEAM),
     :                    MEAS_2_D(BEAM), MEAS_2_V(BEAM),
     :                    MEAS_2_Q(BEAM), MEAS_2_N(BEAM),
     :                    .TRUE.)
		     END IF
                  END IF
               END DO

*  derive the measured signal from the coadded measurement

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_FIT_2D_PARABOLA (JIGGLE_COUNT,
     :              MEAS_D(1,BEAM), MEAS_V(1,BEAM), MEAS_Q(1,BEAM),
     :              JIGGLE_X, JIGGLE_Y,
     :              MEAS_1_A0(BEAM), MEAS_1_A1(BEAM),
     :              MEAS_1_X(BEAM), MEAS_1_Y(BEAM),
     :              MEAS_1_D(BEAM), MEAS_1_V(BEAM),
     :              STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     MEAS_1_Q(BEAM) = 0
                  END IF
               END IF

*  store the coadded measurement to the output map

	       IF (STATUS .EQ. SAI__OK) THEN
		  CALL SCULIB_UNPACK_JIGGLE_SEPARATES (JIGGLE_COUNT,
     :              1, MEAS_D(1,BEAM), MEAS_V(1,BEAM),
     :              MEAS_Q(1,BEAM), 1, JIGGLE_COUNT, IPOS, JPOS,
     :              UBND(1), UBND(2), %val(MAP_D_PTR),
     :              %val(MAP_V_PTR), %val(MAP_Q_PTR), ITEMP, STATUS)
	       END IF

*  store the fitted peak values to the output ndf

	       IF (STATUS .EQ. SAI__OK) THEN
                  CALL SCULIB_COPYR (N_INTEGRATIONS, PEAK_D(1,BEAM),
     :              %val(PEAK_D_PTR))
                  CALL SCULIB_COPYR (N_INTEGRATIONS, PEAK_V(1,BEAM),
     :              %val(PEAK_V_PTR))
                  CALL SCULIB_COPYI (N_INTEGRATIONS, PEAK_Q(1,BEAM),
     :              %val(PEAK_Q_PTR))
               END IF

*  close the ndfs opened in this ndf context

	       CALL NDF_END (STATUS)
            END IF
         END DO
      END IF

*  write the results out to an ASCII file

      CALL REDS_WRITE_PHOTOM (ODF_NAME, UTDATE, UTSTART,
     :  RUN_NUMBER, OBJECT, SUB_INSTRUMENT, FILTER, CENTRE_COORDS,
     :  LAT, LONG, LAT2, LONG2, MJD1, MJD2, OFFSET_COORDS, MAP_X,
     :  MAP_Y, SAMPLE_COORDS, SAMPLE_PA, SKY_ERROR, SCUBA__MAX_BEAM,   
     :  N_BOLS, BOL_CHAN, BOL_ADC, PHOT_BB, SCUBA__MAX_INT,
     :  N_INTEGRATIONS,
     :  PEAK_D, PEAK_V, PEAK_X, PEAK_Y, PEAK_Q, BEAM_WEIGHT,
     :  MEAS_1_D, MEAS_1_V, MEAS_1_X, MEAS_1_Y, MEAS_1_Q,
     :  MEAS_2_D, MEAS_2_V, MEAS_2_Q, STATUS)
    
*  finish off

*  free memory

      CALL SCULIB_FREE ('INT_QUALITY', INT_QUALITY_PTR, 
     :  INT_QUALITY_END, STATUS)
      DO BEAM = 1, SCUBA__MAX_BEAM
         CALL SCULIB_FREE ('INT_D', INT_D_PTR(BEAM), INT_D_END(BEAM),
     :     STATUS)
         CALL SCULIB_FREE ('INT_V', INT_V_PTR(BEAM), INT_V_END(BEAM),
     :     STATUS)
         CALL SCULIB_FREE ('INT_Q', INT_Q_PTR(BEAM), INT_Q_END(BEAM),
     :     STATUS)
      END DO

*  close the input file

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)

      CALL NDF_ANNUL (IN_NDF, STATUS)

*  close the output file

      CALL DAT_ANNUL (OUT_LOC, STATUS)

*  and close down NDF

      CALL NDF_END (STATUS)

      END
