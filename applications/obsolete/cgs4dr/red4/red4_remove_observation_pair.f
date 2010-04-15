*+  RED4_REMOVE_OBSERVATION_PAIR - Subtract pair of observations from reduced group
      SUBROUTINE RED4_REMOVE_OBSERVATION_PAIR( OBJECT_OBS, SKY_OBS,
     :  COADDED_OBS, COADD_OBJ, COADD_SKY, ERRORS, VARIANCE_WT, SKY_WT,
     :  STATUS )
*    Description :
*     This is a lower level routine, called by RED4_REMOVE_PAIR, which
*     subtracts an OBJECT/SKY pair of reduced observations belonging to
*     a particular group from a reduced group file. The routine is called
*     after all the checks on the suitability of the reduced observations
*     and the existence of the reduced group have been made. It is assumed
*     the OBJECT reduced observation has been opened with a DSA reference of
*     'OBJRED', the SKY reduced observation has been opened with a DSA
*     reference of 'SKYRED', the reduced group has been opened with a DSA
*     reference of 'GRPRED', and the .MORE.CGS4_COADDS structure within the
*     reduced group has been opened with a reference of 'COADDS'. When this
*     routine is called non of the data arrays have been mapped.
*
*     SKY observations may have been optionally multiplied by a weighting factor
*     indicated by the SKY_WT parameter.
*     Observations may also have been weighted according to their variance
*     if observing conditions dictate.
*     Observation types other than OBJECT and SKY are not allowed.
*
*     The COADDS structures in the reduced group file corresponding to
*     the given observation are deleted.
*
*     Note that it is not possible to reverse the changes to the FITS
*     header that were made when the observation was added. The exposure
*     time is removed from the EXPOSED and SKYEXP parameters, but the
*     previous values of RUTSTART, RUTEND, UTSTART, UTEND, AMSTART and
*     AMEND cannot be restored.
*    Invocation :
*     CALL RED4_REMOVE_OBSERVATION_PAIR( OBJECT_OBS, SKY_OBS,
*     :  COADDED_OBS, COADD_OBJ, COADD_SKY, ERRORS, VARIANCE_WT, SKY_WT,
*     :  STATUS )
*    Parameters :
*     OBJECT_OBS  = CHARACTER*(*)( READ )
*           The name of the OBJECT observation.
*     SKY_OBS     = CHARACTER*(*)( READ )
*           The name of the SKY observation.
*     COADDED_OBS = CHARACTER*(*)( READ )
*           The DTA name of the .MORE.CGS4_COADDS.COADDED_OBS structure
*           within the reduced group file.
*     COADD_OBJ   = CHARACTER*(*)( READ )
*           The name of the item within the COADDED_OBS structure
*           corresponding to the OBJECT observation.
*     COADD_SKY   = CHARACTER*(*)( READ )
*           The name of the item within the COADDED_OBS structure
*           corresponding to the SKY observation.
*     ERRORS      = CHARACTER*(*)( READ )
*           The error propagation method required.
*           FROM_INT - Propagate the errors in the observations obtained
*                      when the integrations were combined.
*           FROM_OBS - Estimate new errors from the variations between
*                      the observation pairs.
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
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     17-Feb-1991: Original version, based on RED4_REMOVE_OBSERVATION_2.(SMB)
*     22-Feb-1991: Confusing error message changed.                     (SMB)
*      3-Sep-1991: GEN_MULCAFE renamed to GEN_MULCAFEV. The CGS4
*                  software was assuming this routine dealt with
*                  variances, but the actual Figaro routine dealt
*                  with standard deviation.                             (SMB)
*     11-Sep-1991: GEN_MULCAFEV renamed to GEN_MULCAFV, and argument
*                  list made compatible with Figaro version.            (SMB)
*      1-Oct-1991: Change GEN_*AFE to GEN_*AFV calls.                   (PND)
*     23-Feb-1993: Conform to error strategy                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
*    Import :
      CHARACTER*(*)
     :  OBJECT_OBS,                ! Name of OBJECT observation.
     :  SKY_OBS,                   ! Name of SKY observation.
     :  COADDED_OBS,               ! Name of COADDED_OBS structure.
     :  COADD_OBJ,                 ! Name of coadd item for OBJECT observation.
     :  COADD_SKY,                 ! Name of coadd item for SKY observation.
     :  ERRORS                     ! Error propagation method.
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
     :  OBJEXPOSED,                ! Observation time in OBJECT observation file.
     :  SKYEXPOSED                 ! Observation time in SKY observation file.
      INTEGER
     :  CLEN,                      ! Length of character string.
     :  NDIM,                      ! Number of dimensions of data array.
     :  DIMS( MAXDIM ),            ! Dimensions of data array.
     :  NELM,                      ! Number of elements in data array.
     :  ADDRESS,                   ! Address returned when mapping.
     :  OBJDATA_SLOT,              ! Mapping slot for data array in
*                                  !    OBJECT reduced observation file
     :  OBJDATA_PTR,               ! Pointer to data array mapped from
*                                  !    OBJECT reduced observation file
     :  OBJVAR_SLOT,               ! Mapping slot for variance array in
*                                  !    OBJECT reduced observation file
     :  OBJVAR_PTR,                ! Pointer to variance array mapped from
*                                  !    OBJECT reduced observation file
     :  OBJQUAL_SLOT,              ! Mapping slot for quality array in
*                                  !    OBJECT reduced observation file
     :  OBJQUAL_PTR,               ! Pointer to quality array mapped from
*                                  !    OBJECT reduced observation file
     :  SKYDATA_SLOT,              ! Mapping slot for data array in
*                                  !    SKY reduced observation file
     :  SKYDATA_PTR,               ! Pointer to data array mapped from
*                                  !    SKY reduced observation file
     :  SKYVAR_SLOT,               ! Mapping slot for variance array in
*                                  !    SKY reduced observation file
     :  SKYVAR_PTR                 ! Pointer to variance array mapped from
*                                  !    SKY reduced observation file
      INTEGER
     :  SKYQUAL_SLOT,              ! Mapping slot for quality array in
*                                  !    SKY reduced observation file
     :  SKYQUAL_PTR,               ! Pointer to quality array mapped from
*                                  !    SKY reduced observation file
     :  WDATA_SLOT,                ! Mapping slot for work data array for
*                                  !    sky-subtracted data
     :  WDATA_PTR,                 ! Pointer to work data array for
*                                  !    sky-subtracted data
     :  WVAR_SLOT,                 ! Mapping slot for work variance array for
*                                  !    sky-subtracted data
     :  WVAR_PTR,                  ! Pointer to work variance array for
*                                  !    sky-subtracted data
     :  WQUAL_SLOT,                ! Mapping slot for work quality array for
*                                  !    sky-subtracted data
     :  WQUAL_PTR,                 ! Pointer to work quality array for
*                                  !    sky-subtracted data
     :  GRPDATA_SLOT,              ! Mapping slot for data array in
*                                  !    reduced group file
     :  GRPDATA_PTR,               ! Pointer to data array mapped from
*                                  !    reduced group file
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
*   observation files).
      CALL DSA_DATA_SIZE( 'GRPRED', MAXDIM, NDIM, DIMS, NELM, STATUS )

*   Indicate to DSA that a data quality array will be used to
*   indicate bad values in all the structures.
      CALL DSA_USE_QUALITY( 'OBJRED', STATUS )
      CALL DSA_USE_QUALITY( 'SKYRED', STATUS )
      CALL DSA_USE_QUALITY( 'GRPRED', STATUS )

*   Map the data, variance and quality arrays from the OBJECT reduced
*   observation file.
      CALL DSA_MAP_DATA( 'OBJRED', 'READ', 'FLOAT', ADDRESS,
     :  OBJDATA_SLOT, STATUS )
      OBJDATA_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE( 'OBJRED', 'READ', 'FLOAT', ADDRESS,
     :  OBJVAR_SLOT, STATUS )
      OBJVAR_PTR = ADDRESS
      CALL DSA_MAP_QUALITY( 'OBJRED', 'READ', 'BYTE', ADDRESS,
     :  OBJQUAL_SLOT, STATUS )
      OBJQUAL_PTR = ADDRESS

*   Map the data, variance and quality arrays from the SKY reduced
*   observation file.
      CALL DSA_MAP_DATA( 'SKYRED', 'READ', 'FLOAT', ADDRESS,
     :  SKYDATA_SLOT, STATUS )
      SKYDATA_PTR = ADDRESS
      CALL DSA_MAP_VARIANCE( 'SKYRED', 'READ', 'FLOAT', ADDRESS,
     :  SKYVAR_SLOT, STATUS )
      SKYVAR_PTR = ADDRESS
      CALL DSA_MAP_QUALITY( 'SKYRED', 'READ', 'BYTE', ADDRESS,
     :  SKYQUAL_SLOT, STATUS )
      SKYQUAL_PTR = ADDRESS

*   If required, map some work arrays to hold the sky-subtracted data,
*   variance and quality.
      IF ( ERRORS .EQ. 'FROM_INT' ) THEN

         CALL DSA_GET_WORK_ARRAY( NELM, 'FLOAT', WDATA_PTR,
     :     WDATA_SLOT, STATUS )
         CALL DSA_GET_WORK_ARRAY( NELM, 'FLOAT', WVAR_PTR,
     :     WVAR_SLOT, STATUS )
         CALL DSA_GET_WORK_ARRAY( NELM, 'BYTE', WQUAL_PTR,
     :     WQUAL_SLOT, STATUS )
      END IF

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
     : COADDS_SLOT, STATUS )
      COADDS_PTR = ADDRESS

*   Check that all these arrays have been mapped successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Decide how errors were propagated.
         IF ( ERRORS .EQ. 'FROM_INT' ) THEN

*         Errors were propagated from those mapped from the
*         observations.
*         First, multiply the SKY array by the sky weighting factor,
*         if this is significantly different from 1.0.
*         Then subtract SKY from OBJECT, propagating the variances
*         and quality, and write the result into the work array.
            IF ( ABS( SKY_WT - 1.0 ) .GT. 0.0001 ) THEN

               CALL GEN_MULCAFV( %val(SKYDATA_PTR), NELM, SKY_WT,
     :           %val(WDATA_PTR), %val(SKYQUAL_PTR), %val(WQUAL_PTR),
     :           %val(SKYVAR_PTR), %val(WVAR_PTR), .TRUE., .FALSE.,
     :           0.0, .TRUE. )

               CALL GEN_SUBAFV( NELM,
     :           %val(OBJDATA_PTR), %val(WDATA_PTR), %val(WDATA_PTR),
     :           %val(OBJQUAL_PTR), %val(WQUAL_PTR), %val(WQUAL_PTR),
     :           %val(OBJVAR_PTR), %val(WVAR_PTR), %val(WVAR_PTR),
     :           .TRUE., .FALSE., 0.0, .TRUE. )
            ELSE

               CALL GEN_SUBAFV( NELM,
     :           %val(OBJDATA_PTR), %val(SKYDATA_PTR),
     :           %val(WDATA_PTR),
     :           %val(OBJQUAL_PTR), %val(SKYQUAL_PTR),
     :           %val(WQUAL_PTR),
     :           %val(OBJVAR_PTR), %val(SKYVAR_PTR), %val(WVAR_PTR),
     :           .TRUE., .FALSE., 0.0, .TRUE. )
            END IF

*         If variance weighting has been used, check that the resultant
*         variance array contains sensible values.
            IF ( VARIANCE_WT ) THEN

*            Clean up the observation pair before checking it. (THIS IS A FUDGE!!).
               CALL GEN_CLEANV( NELM, %val(WDATA_PTR),
     :           %val(WVAR_PTR), %val(WQUAL_PTR), SNCUT, TLOW,
     :           .TRUE., .FALSE., 0.0 )

*            Determine the range of the variance values.
               CALL GEN_RANGEFV( %val(WVAR_PTR), %val(WQUAL_PTR),
     :           .TRUE., .FALSE., 0.0, 1, NELM, VARMAX, VARMIN )

*            If the variances do not lie within the allowed range,
*            report an error and abort.
               IF ( ( VARMIN .LT. SMALLEST_VAR ) .OR.
     :              ( VARMAX .GT. LARGEST_VAR ) ) THEN

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_PAIR: '/
     :              /'These data are not suitable for variance '/
     :              /'weighting, and should never '/
     :              /'have been added!', STATUS )
               END IF
            END IF

*         Subtract this sky-subtracted pair into the reduced group,
*         propagating the variance and quality.
            CALL RED4_SUBTRACT_OBS( DIMS(1), DIMS(2), 1.0,
     :        VARIANCE_WT,
     :        %val(WDATA_PTR), %val(WVAR_PTR), %val(WQUAL_PTR),
     :        %val(GRPDATA_PTR), %val(GRPVAR_PTR), %val(GRPQUAL_PTR),
     :        %val(COADDS_PTR), STATUS )
         ELSE

*         Errors have been determined from the variations between
*         OBJECT/SKY pairs.
*         Take OBJECT-SKY and subtract this from the reduced group,
*         updating the variance and propagating data quality.
            CALL RED4_SUBTRACT_PAIR( DIMS(1), DIMS(2), SKY_WT,
     :        %val(OBJDATA_PTR), %val(OBJQUAL_PTR),
     :        %val(SKYDATA_PTR), %val(SKYQUAL_PTR),
     :        %val(GRPDATA_PTR), %val(GRPVAR_PTR), %val(GRPQUAL_PTR),
     :        %val(COADDS_PTR), STATUS )
         END IF

*      Check everything has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         The FITS parameters in the reduced group file should now
*         be restored to the values they had before the observation
*         pair was added. Unfortunately it is only possible to restore
*         the EXPOSED and SKYEXP parameters.
*         Subtract the exposure time for the observation from
*         that of the group.
            CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :        EXPOSED, COMMENT, STATUS )
            CALL DSA_GET_FITS_F( 'OBJRED', 'EXPOSED', 0,
     :        OBJEXPOSED, COMMENT, STATUS )

            EXPOSED = EXPOSED - OBJEXPOSED

            CALL DSA_PUT_FITS_F( 'GRPRED', 'EXPOSED', EXPOSED,
     :        ' ', STATUS )
            CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED, STATUS )

*         Subtract the exposure time of the SKY from that of the group.
            CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0, SKYEXP,
     :        COMMENT, STATUS )
            CALL DSA_GET_FITS_F( 'SKYRED', 'EXPOSED', 0,
     :        SKYEXPOSED, COMMENT, STATUS )

            SKYEXP = SKYEXP - SKYEXPOSED

            CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :        ' ', STATUS )

*         Issue a warning if the last OBJECT/SKY pair has been removed.
            IF ( EXPOSED .LE. 0.0 ) THEN

               CALL MSG_OUT( ' ', 'WARNING - The last OBJECT/SKY '/
     :           /'pair has been removed. You would be '/
     :           /'better', STATUS )
               CALL MSG_OUT( ' ', 'off creating a new group file '/
     :           /'and starting again', STATUS )
            END IF

*         Write a comment into the FITS structure warning that some
*         header items may be incorrect.
            CALL DSA_PUT_FITS_C( 'GRPRED', 'COMMENT',
     :        'WARNING - *UTSTART, *UTEND, AMSTART, AMEND '/
     :        /'may be incorrect', ' ', STATUS )
         ELSE

*         An error occurred during the removal.
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_PAIR: '/
     :        /'Arithmetic error during '/
     :        /'removal - suspect zero variance in group '/
     :        /'file', STATUS )
         END IF

*      If everything has worked, delete the COADD structures
*      corresponding to the OBJECT and SKY observations.
         CLEN = MAX( 1, CHR_LEN( COADDED_OBS ) )
         CALL RED4_DELETE_STRUCTURE(
     :     COADDED_OBS(1:CLEN)//'.'//COADD_OBJ, STATUS )
         CALL RED4_DELETE_STRUCTURE(
     :     COADDED_OBS(1:CLEN)//'.'//COADD_SKY, STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION_PAIR: '/
     :     /'Error mapping arrays', STATUS )
      END IF

      END
