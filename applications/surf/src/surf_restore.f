*+  REDS_RESTORE - remove the chopped beam response from MAP/RASTER scans
      SUBROUTINE REDS_RESTORE (STATUS)
*    Description :
*     <description of what the subroutine does>
*    Invocation :
*     CALL REDS_RESTORE (STATUS)
*    Parameters :
*     STATUS           = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     21-SEP-1995: original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      INTEGER MAX_FITS                  ! maximum number of FITS items in file
      PARAMETER (MAX_FITS = 200)
*    Local variables :
      CHARACTER*15 CHOP_COORDS          ! coordinate system of chop
      CHARACTER*15 CHOP_FUN             ! chop mode used in observation
      REAL         CHOP_THROW           ! chopper throw (arcsec)
      INTEGER      DIM (MAXDIM)         ! the dimensions of an array
      INTEGER      EXPOSURE             ! exposure index in DO loop
      LOGICAL      EXTINCTION           ! .TRUE. if the EXTINCTION application
                                        ! has already been run on the 
                                        ! input file
      CHARACTER*80 FITS (MAX_FITS)      ! array of FITS keyword lines
      INTEGER      I                    ! DO loop variable
      INTEGER      INDF                 ! NDF identifier of input file
      INTEGER      INTEGRATION          ! integration index in DO loop
      INTEGER      ITEMP                ! scratch integer
      INTEGER      IN_DATA_PTR          ! pointer to data array of input file
      INTEGER      IN_DEM_PNTR_ARY      ! array identifer to .SCUBA.DEM_PNTR
      INTEGER      IN_DEM_PNTR_PTR      ! pointer to input .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                        ! locator to FITS extension in input
                                        ! file
      INTEGER      IN_QUALITY_PTR       ! pointer to quality array in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                        ! locator to SCUBA extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                        ! locator to SCUCD extension in input
                                        ! file
      INTEGER      IN_VARIANCE_PTR      ! pointer to variance array in input
                                        ! file
      INTEGER      MEASUREMENT          ! measurement index in DO loop
      INTEGER      NDIM                 ! the number of dimensions in an array
      INTEGER      NREC                 ! number of history records in file
      INTEGER      N_BOL                ! number of bolometers measured 
      INTEGER      N_EXPOSURES          ! number of exposures per integration
      INTEGER      N_FITS               ! number of FITS lines read from file
      INTEGER      N_INTEGRATIONS       ! number of integrations per measurement
      INTEGER      N_MEASUREMENTS       ! number of measurements in the file
      INTEGER      N_POS                ! the total number of positions measured
      CHARACTER*30 OBJECT               ! name of object observed
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      INTEGER      OUTNDF               ! NDF identifier of output file
      INTEGER      OUT_DATA_PTR         ! pointer to data array in output
      INTEGER      OUT_QUALITY_PTR      ! pointer to quality array in output 
      INTEGER      OUT_VARIANCE_PTR     ! pointer to variance array in output
      LOGICAL      REDUCE_SWITCH        ! .TRUE. if REDUCE_SWITCH has been run
      INTEGER      RUN_NUMBER           ! run number of observation
      REAL         SAMPLE_DX            ! sample spacing along scans
      CHARACTER*15 SAMPLE_MODE          ! SAMPLE_MODE of observation
      CHARACTER*80 STEMP                ! scratch string
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (INDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_RESTORE: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (IN_FITSX_LOC, MAX_FITS, FITS, N_FITS, STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_I (MAX_FITS, N_FITS, FITS, 'RUN', 
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, 'SAM_MODE',
     :  SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)
      CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, 'CHOP_CRD',
     :  CHOP_COORDS, STATUS)
      CALL CHR_UCASE (CHOP_COORDS)

      print *, chop_coords

*  check that the observation was suitable for RESTORE

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .NE. 'MAP') THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_RESTORE: file does not contain '//
     :        'data for a MAP observation', STATUS)
         ELSE IF (SAMPLE_MODE .NE. 'RASTER') THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_RESTORE: map was not obtained '//
     :        'with RASTER sampling', STATUS)
         ELSE IF (CHOP_COORDS .NE. 'SC') THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_RESTORE: the secondary was not '//
     :        'chopping along the direction of scan', STATUS)
         END IF
      END IF

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_OUT (' ', 'REDS: run ^RUN was a MAP observation '//
     :  'of object ^OBJECT', STATUS)

*  check that the history of the input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (INDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (INDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_RESTORE: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

            IF (EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_RESTORE: the '//
     :           'EXTINCTION application has already been run '//
     :           'on the input file. RESTORE should be run before '//
     :           'EXTINCTION', STATUS)
            END IF
         END IF
      END IF

*  get some other FITS items needed for this stage of reduction

      CALL SCULIB_GET_FITS_R (MAX_FITS, N_FITS, FITS, 'SAM_DX',
     :  SAMPLE_DX, STATUS)
      CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, 'CHOP_FUN',
     :  CHOP_FUN, STATUS)
      CALL CHR_UCASE (CHOP_FUN)
      CALL SCULIB_GET_FITS_R (MAX_FITS, N_FITS, FITS, 'CHOP_THR',
     :  CHOP_THROW, STATUS)

      print *, sample_dx, chop_fun, chop_throw

*  map the various components of the data array and check the data dimensions 

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'VARIANCE', '_REAL', 'READ', IN_VARIANCE_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'QUALITY', '_INTEGER', 'READ',
     :  IN_QUALITY_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2) .OR.
     :       (DIM(1) .LT. 1) .OR.
     :       (DIM(2) .LT. 1)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_RESTORE: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

      N_BOL = DIM (1)
      N_POS = DIM (2)

*  map the DEM_PNTR array and check its dimensions

*      CALL NDF_XIARY (INDF, 'SCUBA', 'DEM_PNTR', 'READ', 
*    :  IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_DIM (IN_DEM_PNTR_ARY, MAXDIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_DEM_PNTR_ARY, '_INTEGER', 'READ', 
     :  IN_DEM_PNTR_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 3) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_RESTORE: .SCUBA.DEM_PNTR '//
     :        'array has bad number of dimensions', STATUS)
         ELSE 
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_RESTORE: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'exposures - ^DIM1', STATUS) 
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_RESTORE: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'integrations - ^DIM2', STATUS)
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_RESTORE: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'measurements - ^DIM3', STATUS)
            END IF
         END IF
      END IF

      N_EXPOSURES = DIM (1)
      N_INTEGRATIONS = DIM (2)
      N_MEASUREMENTS = DIM (3)

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_OUT (' ', 'REDS: file contains data for ^N_E '//
     :  'exposure(s) in ^N_I integration(s) in '//
     :  '^N_M measurement(s)', STATUS)

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, ' ', 'OUT', OUTNDF, STATUS)

*  map the various components of the output data array

      CALL NDF_MAP (OUTNDF, 'DATA', '_REAL', 'WRITE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'QUALITY', '_INTEGER', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)

*  now go through the various exposures in the observation

      IF (STATUS .EQ. SAI__OK) THEN

         DO MEASUREMENT = 1, N_MEASUREMENTS
            DO INTEGRATION = 1, N_INTEGRATIONS
               DO EXPOSURE = 1, N_EXPOSURES

*  deconvolve the data using the routine appropriate for the chop
*  function used

                  IF (CHOP_FUN .EQ. 'SQUARE') THEN
                     CALL SCULIB_2POS_DECONV (N_EXPOSURES,
     :                 N_INTEGRATIONS, N_MEASUREMENTS, 
     :                 %val(IN_DEM_PNTR_PTR), N_BOL, N_POS, 
     :                 %val(IN_DATA_PTR), %val(IN_VARIANCE_PTR),
     :                 %val(IN_QUALITY_PTR), SAMPLE_DX, CHOP_THROW,
     :                 %val(OUT_DATA_PTR), %val(OUT_VARIANCE_PTR),
     :                 %val(OUT_QUALITY_PTR), STATUS)
                  ELSE IF (CHOP_FUN .EQ. 'TRIPOS') THEN
                     CALL SCULIB_3POS_DECONV (N_EXPOSURES,
     :                 N_INTEGRATIONS, N_MEASUREMENTS,
     :                 %val(IN_DEM_PNTR_PTR), N_BOL, N_POS,
     :                 %val(IN_DATA_PTR), %val(IN_VARIANCE_PTR),
     :                 %val(IN_QUALITY_PTR), SAMPLE_DX, CHOP_THROW,
     :                 %val(OUT_DATA_PTR), %val(OUT_VARIANCE_PTR),
     :                 %val(OUT_QUALITY_PTR), STATUS)
                  END IF
               END DO
            END DO
         END DO

*  unmap the main data array

         CALL NDF_UNMAP (OUTNDF, '*', STATUS)
      END IF

*  tidy up

      CALL ARY_ANNUL (IN_DEM_PNTR_ARY, STATUS)

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
