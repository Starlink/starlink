*+  RED4_REMOVE_OBSERVATION_2 - Subtract observation from reduced group file - 2
      SUBROUTINE RED4_REMOVE_OBSERVATION_2( OBSTYPE, COADDED_OBS,
     :  COADD_NAME, VARIANCE_WT, SKY_WT, STATUS )
*    Description :
*     This is a lower level routine, called by RED4_REMOVE_OBSERVATION, which
*     subtracts a reduced observation belonging to a particular group from a
*     reduced group file. The routine is called after all the checks
*     on the suitability of the reduced observation and the existence
*     of the reduced group have been made. It is assumed the reduced
*     observation has been opened with a DSA reference of 'OBSRED',
*     the reduced group has been opened with a DSA reference of 'GRPRED',
*     and the .MORE.CGS4_COADDS structure within the reduced group has
*     been opened with a reference of 'COADDS'. When this routine is
*     called non of the data arrays have been mapped.
*
*     It is assumed that reduced observations of type OBJECT were added
*     to the contents of the reduced group file, and reduced observations
*     of type SKY were subtracted from the contents of the reduced group
*     file, after being optionally multiplied by a weighting factor
*     indicated by the SKY_WT parameter.
*     Observations may also have been weighted according to their variance
*     if observing conditions dictate.
*     Observation types other than OBJECT and SKY are not allowed.
*
*     The COADDS structure in the reduced group file corresponding to
*     the given observation is deleted.
*
*     Note that it is not possible to reverse the changes to the FITS
*     header that were made when the observation was added. The exposure
*     time is removed from the EXPOSED or SKYEXP parameters, but the
*     previous values of RUTSTART, RUTEND, UTSTART, UTEND, AMSTART and
*     AMEND cannot be restored.
*    Invocation :
*     CALL RED4_REMOVE_OBSERVATION_2( OBSTYPE, COADDED_OBS, COADD_NAME,
*     :  STATUS )
*    Parameters :
*     OBSTYPE     = CHARACTER*(*)
*           The observation type. This should either be 'OBJECT' or 'SKY'.
*     COADDED_OBS = CHARACTER*(*)
*           The DTA name of the .MORE.CGS4_COADDS.COADDED_OBS structure
*           within the reduced group file.
*     COADD_NAME  = CHARACTER*(*)
*           The name of the item within the COADDED_OBS structure
*           corresponding to the observation about to be added.
*     VARIANCE_WT = LOGICAL( READ )
*           Flag indicating if variance weighting has been used when
*           combining observations together.
*     SKY_WT      = REAL( READ )
*           SKY observation weighting factor. (SKY observations are
*           multiplied by this factor before being combined with
*           OBJECT observations).
*     STATUS  = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     As stated in "Description", it is not possible to reverse the
*     changes made to the FITS header by the addition of the observation
*     being removed by this routine.
*    Bugs :
*     Some observations were being rejected because they contained stray
*     points with zero data and variance arrays. I don't know what is
*     causing these points. Adding a test for values set to -50.0 by
*     the ADP has not removed these points. An investigation is needed.
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     31-Oct-1990: Original version, based on RED4_ADD_OBSERVATION_2.(SMB)
*      7-Nov-1990: VARIANCE_WT and SKY_WT parameters included.       (SMB)
*      7-Nov-1990: Some data sets were being rejected because
*                  they had stray points with zero data and
*                  variance values. Fudge added to clean up these
*                  observations before they are applied.             (SMB)
*     28-Nov-1990: Status check from RED4_SUBTRACT_OBS added.        (SMB)
*     18-Feb-1991: Typing mistakes fixed.                            (SMB)
*     22-Feb-1991: Confusing error message removed.                  (SMB)
*      1-Oct-1991: Change GEN_*AFE to GEN_*AFV calls.                (PND)
*     23-Feb-1993: Conform to error strategy                         (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
*    Import :
      CHARACTER*(*)
     :  OBSTYPE,                   ! Observation type.
     :  COADDED_OBS,               ! Name of COADDED_OBS structure.
     :  COADD_NAME                 ! Name of coadd item for this observation.
      LOGICAL
     :  VARIANCE_WT                ! Determines whether observations were
*                                  !   added using variance weighting
      REAL
     :  SKY_WT                     ! SKY observation weighting factor
*    Status :
      INTEGER
     :  STATUS                     ! Global status
*    External references :
      INTEGER CHR_LEN              ! Character length determining function
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM               ! Maximum number of dimensions
      PARAMETER ( MAXDIM = 2 )
      INTEGER NINFO                ! Number of "data info" items which DSA uses
      PARAMETER ( NINFO = 2 )
      REAL SMALLEST_VAR            ! Smallest sensible variance for
*                                  !   variance weighting
      PARAMETER ( SMALLEST_VAR = 1.0E-18 )
      REAL LARGEST_VAR             ! Largest sensible variance for
*                                  !   variance weighting
      PARAMETER ( LARGEST_VAR = 1.0E+18 )
      REAL SNCUT                   ! Signal-to-noise cut for cleaning observation
      PARAMETER ( SNCUT = 1.0 )
      REAL TLOW                    ! Low threshold for cleaning observation
      PARAMETER ( TLOW = 1.0E-20 )
*    Local variables :
      CHARACTER*4
     :  COMMENT                    ! Dummy comment.
      REAL
     :  VARMIN,                    ! Minimum variance
     :  VARMAX,                    ! Maximum variance
     :  SKYEXP,                    ! Total exposure time of SKY observations.
     :  EXPOSED,                   ! Total exposure time of OBJECT observations.
     :  OBSEXPOSED                 ! Observation time in observation file.
      INTEGER
     :  CLEN,                      ! Length of character string.
     :  NDIM,                      ! Number of dimensions of data array.
     :  DIMS( MAXDIM ),            ! Dimensions of data array.
     :  NELM,                      ! Number of elements in data array.
     :  ADDRESS,                   ! Address returned when mapping.
     :  OBSDATA_SLOT,              ! Mapping slot for data array in
*                                  !    reduced observation file
     :  OBSDATA_PTR,               ! Pointer to data array mapped from
*                                  !    reduced observation file
     :  OBSVAR_SLOT,               ! Mapping slot for variance array in
*                                  !    reduced observation file
     :  OBSVAR_PTR,                ! Pointer to variance array mapped from
*                                  !    reduced observation file
     :  OBSQUAL_SLOT,              ! Mapping slot for quality array in
*                                  !    reduced observation file
     :  OBSQUAL_PTR,               ! Pointer to quality array mapped from
*                                  !    reduced observation file
     :  GRPDATA_SLOT,              ! Mapping slot for data array in
*                                  !    reduced group file
     :  GRPDATA_PTR                ! Pointer to data array mapped from
*                                  !    reduced group file
      INTEGER
     :  GRPVAR_SLOT,               ! Mapping slot for variance array in
*                                  !    reduced group file
     :  GRPVAR_PTR,                ! Pointer to variance array mapped from
*                                  !    reduced group file
     :  GRPQUAL_SLOT,              ! Mapping slot for quality array in
*                                  !    reduced group file
     :  GRPQUAL_PTR,               ! Pointer to quality array mapped from
*                                  !    reduced group file
     :  COADDS_SLOT,               ! Mapping slot for data array in COADDS
*                                  !    structure of reduced group file.
     :  COADDS_PTR                 ! Pointer to data array mapped in COADDS
*                                  !    structure of reduced group file.
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Determine the size of the data arrays in the reduced group file.
*   (These will be the same size as the ones in the reduced
*   observation file).
      CALL DSA_DATA_SIZE( 'GRPRED', MAXDIM, NDIM, DIMS, NELM, STATUS )

*   Indicate to DSA that a data quality array will be used to
*   indicate bad values in both structures.
      CALL DSA_USE_QUALITY( 'OBSRED', STATUS )
      CALL DSA_USE_QUALITY( 'GRPRED', STATUS )

*   Map the data, variance and quality arrays from the reduced
*   observation file.
      CALL DSA_MAP_DATA( 'OBSRED', 'READ', 'FLOAT', ADDRESS,
     :  OBSDATA_SLOT, STATUS )
      OBSDATA_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE( 'OBSRED', 'READ', 'FLOAT', ADDRESS,
     :  OBSVAR_SLOT, STATUS )
      OBSVAR_PTR = ADDRESS
      CALL DSA_MAP_QUALITY( 'OBSRED', 'READ', 'BYTE', ADDRESS,
     :  OBSQUAL_SLOT, STATUS )
      OBSQUAL_PTR = ADDRESS

*   Map the data, variance and quality arrays from the reduced group file.
      CALL DSA_MAP_DATA( 'GRPRED', 'UPDATE', 'FLOAT', ADDRESS,
     :  GRPDATA_SLOT, STATUS )
      GRPDATA_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE( 'GRPRED', 'UPDATE', 'FLOAT', ADDRESS,
     :  GRPVAR_SLOT, STATUS )
      GRPVAR_PTR = ADDRESS
      CALL DSA_MAP_QUALITY( 'GRPRED', 'UPDATE', 'BYTE', ADDRESS,
     :  GRPQUAL_SLOT, STATUS )
      GRPQUAL_PTR = ADDRESS

*   Map the COADDS array from the reduced group file.
      CALL DSA_MAP_DATA( 'COADDS', 'UPDATE', 'SHORT', ADDRESS,
     :  COADDS_SLOT, STATUS )
      COADDS_PTR = ADDRESS

*   Check that all these arrays have been mapped successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      If variance weighting has been used, check that the observation
*      variance array contains sensible values.
         IF ( VARIANCE_WT ) THEN

*         Clean up the observation before checking it. (THIS IS A FUDGE!!).
            CALL GEN_CLEANV( NELM, %val(OBSDATA_PTR),
     :        %val(OBSVAR_PTR), %val(OBSQUAL_PTR), SNCUT, TLOW,
     :        .TRUE., .FALSE., 0.0 )

*         Determine the range of the variance values.
            CALL GEN_RANGEFV( %val(OBSVAR_PTR), %val(OBSQUAL_PTR),
     :        .TRUE., .FALSE., 0.0, 1, NELM, VARMAX, VARMIN )

*         If the variances do not lie within the allowed range,
*         report an error and abort.
            IF ( ( VARMIN .LT. SMALLEST_VAR ) .OR.
     :           ( VARMAX .GT. LARGEST_VAR ) ) THEN

               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_2: '/
     :           /'These data are not suitable for variance '/
     :           /'weighting, and should never '/
     :           /'have been added!', STATUS )
            END IF
         END IF

*     Check everything is ok so far.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Determine if the observation is an OBJECT or a SKY.
            IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

*            An OBJECT observation.
*            Subtract the observation data from the reduced group,
*            propagating variance and quality and updating the
*            COADDS array.
               CALL RED4_SUBTRACT_OBS( DIMS(1), DIMS(2), 1.0,
     :           VARIANCE_WT,
     :           %val(OBSDATA_PTR), %val(OBSVAR_PTR),
     :           %val(OBSQUAL_PTR),
     :           %val(GRPDATA_PTR), %val(GRPVAR_PTR),
     :           %val(GRPQUAL_PTR),
     :           %val(COADDS_PTR), STATUS )

*            Check this has worked.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               The FITS parameters in the reduced group file should now
*               be restored to the values they had before the observation
*               was added. Unfortunately it is only possible to restore
*               the EXPOSED and SKYEXP parameters.
*               Subtract the exposure time for the observation from
*               that of the group.
                  CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :              EXPOSED, COMMENT, STATUS )
                  CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0,
     :              OBSEXPOSED, COMMENT, STATUS )

                  EXPOSED = EXPOSED - OBSEXPOSED

                  CALL DSA_PUT_FITS_F( 'GRPRED', 'EXPOSED', EXPOSED,
     :              ' ', STATUS )
                  CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED, STATUS )

*               Issue a warning if the last OBJECT observation has been
*               removed.
                  IF ( EXPOSED .LE. 0.0 ) THEN

                     CALL MSG_OUT( ' ', 'WARNING - The last OBJECT '/
     :                 /'exposure has been removed. You would be '/
     :                 /'better', STATUS )
                     CALL MSG_OUT( ' ', 'off creating a new group '/
     :                 /'file and starting again', STATUS )
                  END IF

*               Write a comment into the FITS structure warning that some
*               header items may be incorrect.
                  CALL DSA_PUT_FITS_C( 'GRPRED', 'COMMENT',
     :              'WARNING - *UTSTART, *UTEND, AMSTART, AMEND '/
     :              /'may be incorrect', ' ', STATUS )
               ELSE

*               An error occurred during the removal.
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_2: '/
     :              /'First arithmetic error during '/
     :              /'removal - suspect zero variance in group '/
     :              /'file', STATUS )
               END IF
            ELSE IF ( OBSTYPE .EQ. 'SKY' ) THEN

*            A SKY observation.
*            Add back the observation data from the reduced group,
*            propagating variance and quality and updating the COADDS
*            array. (Note that the same routine as that to subtract
*            the OBJECT observation is used, but the weight is
*            multiplied by -1 so the data values are added back).
               CALL RED4_SUBTRACT_OBS( DIMS(1), DIMS(2), -SKY_WT,
     :           VARIANCE_WT,
     :           %val(OBSDATA_PTR), %val(OBSVAR_PTR),
     :           %val(OBSQUAL_PTR),
     :           %val(GRPDATA_PTR), %val(GRPVAR_PTR),
     :           %val(GRPQUAL_PTR),
     :           %val(COADDS_PTR), STATUS )

*            Check this has worked.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               For a SKY observation, the only post-processing required
*               is to update the value of the SKYEXP parameter containing
*               the total observation time of all the contributing SKY
*               observations.
                  CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0, SKYEXP,
     :              COMMENT, STATUS )

*               Subtract the observation time of the current sky observation
*               from SKYEXP and write the new value back to the reduced group file.
                  CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0,
     :              OBSEXPOSED, COMMENT, STATUS )

                  SKYEXP = SKYEXP - OBSEXPOSED

                  CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :              ' ', STATUS )
               ELSE

*               An error occurred during the removal.
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_2: '/
     :              /'Second arithmetic error during '/
     :              /'removal - suspect zero variance in group '/
     :              /'file', STATUS )
               END IF
            END IF

*         If everything has worked, delete the COADD structure
*         corresponding to this observation.
            CLEN = MAX( 1, CHR_LEN( COADDED_OBS ) )
            CALL RED4_DELETE_STRUCTURE(
     :        COADDED_OBS(1:CLEN)//'.'//COADD_NAME, STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_2: '/
     :     /'Error mapping arrays', STATUS )
      END IF

      END
