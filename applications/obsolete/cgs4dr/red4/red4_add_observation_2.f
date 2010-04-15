*+  RED4_ADD_OBSERVATION_2 - Add an observation to a reduced group file - 2
      SUBROUTINE RED4_ADD_OBSERVATION_2( OBSTYPE, COADDED_OBS,
     :  COADD_NAME, VARIANCE_WT, SKY_WT, STATUS )
*    Description :
*     This is a lower level routine, called by RED4_ADD_OBSERVATION, which
*     adds a reduced observation belonging to a particular group to a
*     reduced group file. The routine is called after all the checks
*     on the suitability of the reduced observation and the existence
*     of the reduced group have been made. It is assumed the reduced
*     observation has been opened with a DSA reference of 'OBSRED',
*     the reduced group has been opened with a DSA reference of 'GRPRED',
*     and the .MORE.CGS4_COADDS structure within the reduced group has
*     been opened with a reference of 'COADDS'. When this routine is
*     called non of the data arrays have been mapped.
*
*     Reduced observations of type OBJECT are added to the contents of th
*     reduced group file. Reduced observations of type SKY are subtracted
*     from the contents of the reduced group file, after being optionally
*     multiplied by a weighting factor indicated by the SKY_WT parameter.
*     Observations may also optionally be weighted according to their
*     variance, if observing conditions dictate.
*     Observation types other than OBJECT and SKY are not allowed.
*
*     A record of all the observations added is kept in the COADDS
*     structure of the reduced group file. If this record shows that the
*     specified observation has already been added, a warning message
*     will be issued and it will not be added again.
*
*     The FITS header information from the first OBJECT observation is
*     copied to the reduced group file. When subsequent OBJECT observations
*     are added, the header information is updated as follows :-
*
*     EXPOSED     - Value is accumulated (new = old + current)
*     RUTSTART    - Minimum value used   (new = MIN(old,current))
*     RUTEND      - Maximum value used   (new = MAX(old,current))
*     AMSTART     - Carried with RUTSTART
*     AMEND       - Carried with RUTEND
*
*     All the other parameters (i.e. RA, DEC, object name, instrument
*     configuration etc...) are assumed to be identical for all the
*     OBJECT observations making up the group.
*
*     A record of the total exposure time contributed from the SKY
*     observations will be accumulated in a SKYEXP parameter, taking
*     into account any weighting factors used :-
*
*     SKYEXP      - Value is accumulated (new = old + current*SKY_WT)
*
*     When the reduced group is completed, the values of EXPOSED and
*     SKYEXP should be equal if the sky has been subtracted correctly.
*     See CGS4/SOFT/057 for details.
*    Invocation :
*     CALL RED4_ADD_OBSERVATION_2( OBSTYPE, COADDED_OBS, COADD_NAME,
*     :  STATUS )
*    Parameters :
*     OBSTYPE     = CHARACTER*(*)( READ )
*           The observation type. This should either be 'OBJECT' or 'SKY'.
*     COADDED_OBS = CHARACTER*(*)( READ )
*           The DTA name of the .MORE.CGS4_COADDS.COADDED_OBS structure
*           within the reduced group file.
*     COADD_NAME  = CHARACTER*(*)( READ )
*           The name of the item within the COADDED_OBS structure
*           corresponding to the observation about to be added.
*     VARIANCE_WT = LOGICAL( READ )
*           Flag indicating if variance weighting is to be used when
*           combining observations together.
*     SKY_WT      = REAL( READ )
*           SKY observation weighting factor. (SKY observations are
*           multiplied by this factor before being combined with
*           OBJECT observations).
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     Some quite serious deficiencies in this method of combining OBJECT
*     and SKY observations together have come to light (31-Jan-1991) :-
*
*     1. If an equal number of OBJECT and SKY observations are observed,
*        the running mean should be calculated using the number of PAIRS
*        of observations, rather than the total number of observations,
*        and the mean signal calculated will be a factor of 2 smaller
*        than it should be. The signal will be calculated correctly for
*        a series of OBJECT observations with no SKYs.
*
*     2. If there are unequal numbers of bad pixels at a given location
*        from the SKY and OBJECT observations, that particular point will
*        not be sky-subtracted properly.
*
*     3. The variance weighting algorithm will work properly only for
*        a series of consecutive OBJECT observations. If both OBJECT and
*        SKY observations are reduced, different weights may be applied
*        to OBJECT and SKY, and the data will not be properly
*        sky-subtracted.
*
*     Because of these deficiencies, it is recommended that OBJECT and
*     SKY observations be grouped into pairs, and the routine
*     RED4_ADD_PAIR be used for sky subtraction. This routine will give
*     a warning if it encounters a SKY observation.
*
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     19-Sep-1990: Original version.                              (SMB)
*     28-Sep-1990: Mistake corrected. AMSTART and AMEND should be
*                  carried with RUTSTART and RUTEND.              (SMB)
*      1-Oct-1990: Typing mistake fixed.                          (SMB)
*     23-Oct-1990: Modified to add "- SKY" to the data label when
*                  the first SKY frame is subtracted. Also bug,
*                  which caused SKYEXP value to be corrupted when
*                  the first observation is not an OBJECT, fixed. (SMB)
*      6-Nov-1990: VARIANCE_WT and SKY_WT parameters added.       (SMB)
*      7-Nov-1990: Some data sets were being rejected because
*                  they had stray points with zero data and
*                  variance values. Fudge added to clean up these
*                  observations before they are applied.          (SMB)
*     28-Nov-1990: Status check from RED4_COADD_OBS added.        (SMB)
*     30-Jan-1991: Bug fix. OBSFITS was only being defined for
*                  the first OBJECT observation.                  (SMB)
*     31-Jan-1991: Major deficiencies discovered in sky-subtraction
*                  (see under "deficiencies").                    (SMB)
*     17-Feb-1991: Modified to write ERRPROP FITS item.           (SMB)
*     20-Feb-1991: Only add "- SKY" to the label if it has not
*                  already been added.                            (SMB)
*     21-Feb-1991: Modified to update NOBJ and NSKY counters.     (SMB)
*     22-Feb-1991: NOBJ preserved as well as NSKY.                (SMB)
*     23-Feb-1991: Initialise STDUSED.                            (SMB)
*     25-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.          (SMB)
*      1-Oct-1991: Change GEN_*AFE to GEN_*AFV calls.             (PND)
*     18-Feb-1993: Conform to error strategy                      (PND)
*     23-Jun-1994: Remove deletion of OBSNUM (too hard in NDF!)   (PND)
*     23-Apr-1995: Removed references to UTSTART and UTEND as
*                  these items used to be blank and now no longer
*                  exist, and nothing is done with them anyway    (AB,PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
      INCLUDE 'RED4_COMMON.INC'
*    Import :
      CHARACTER*(*)
     :  OBSTYPE,                   ! Observation type.
     :  COADDED_OBS,               ! Name of COADDED_OBS structure.
     :  COADD_NAME                 ! Name of coadd item for this observation.
      LOGICAL
     :  VARIANCE_WT                ! Determines whether observations are
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
      CHARACTER*32
     :  OBJECT,                    ! Object name
     :  CHAR_ARRAY( NINFO ),       ! Data info items
     :  NEWLABEL                   ! Buffer for new label.
      CHARACTER*4
     :  COMMENT                    ! Dummy comment.
      REAL
     :  VARMIN,                    ! Minimum variance
     :  VARMAX,                    ! Maximum variance
     :  SKYEXP,                    ! Total exposure time of SKY observations.
     :  EXPOSED,                   ! Total exposure time of OBJECT observations.
     :  OBSEXPOSED,                ! Observation time in observation file.
     :  RUTSTART,                  ! UT at start of group of observations.
     :  RUTEND,                    ! UT at end of group of observations.
     :  OBSRUTSTART,               ! UT at start of observation.
     :  OBSRUTEND,                 ! UT at end of observation.
     :  OBSAMSTART,                ! Air mass at start of observation.
     :  OBSAMEND                   ! Air mass at end of observation.
      DOUBLE PRECISION
     :  DIGNORE                    ! Ignored argument
      INTEGER
     :  NOBJ,                      ! Number of OBJECT observations
     :  NSKY,                      ! Number of SKY observations
     :  CPOS,                      ! Position in character string
     :  CLEN,                      ! Length of character string.
     :  DSA_STATUS,                ! DSA status value
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
*   (These have already been verified to be the same size as the ones
*   in the reduced observation file).
      DSA_STATUS = ADAM__OK
      CALL DSA_DATA_SIZE( 'GRPRED', MAXDIM, NDIM, DIMS, NELM,
     :  DSA_STATUS )

*   Indicate to DSA that a data quality array will be used to
*   indicate bad values in both structures.
      CALL DSA_USE_QUALITY( 'OBSRED', DSA_STATUS )
      CALL DSA_USE_QUALITY( 'GRPRED', DSA_STATUS )

*   Map the data, variance and quality arrays from the reduced
*   observation file.
      CALL DSA_MAP_DATA( 'OBSRED', 'READ', 'FLOAT', ADDRESS,
     :  OBSDATA_SLOT, DSA_STATUS )
      OBSDATA_PTR = ADDRESS

      CALL DSA_MAP_VARIANCE( 'OBSRED', 'READ', 'FLOAT', ADDRESS,
     :  OBSVAR_SLOT, DSA_STATUS )
      OBSVAR_PTR = ADDRESS

      CALL DSA_MAP_QUALITY( 'OBSRED', 'READ', 'BYTE', ADDRESS,
     :  OBSQUAL_SLOT, DSA_STATUS )
      OBSQUAL_PTR = ADDRESS

*   Map the data, variance and quality arrays from the reduced group file.
      CALL DSA_MAP_DATA( 'GRPRED', 'UPDATE', 'FLOAT', ADDRESS,
     :  GRPDATA_SLOT, DSA_STATUS )
      GRPDATA_PTR = ADDRESS

      CALL DSA_MAP_VARIANCE( 'GRPRED', 'UPDATE', 'FLOAT', ADDRESS,
     :  GRPVAR_SLOT, DSA_STATUS )
      GRPVAR_PTR = ADDRESS

      CALL DSA_MAP_QUALITY( 'GRPRED', 'UPDATE', 'BYTE', ADDRESS,
     :  GRPQUAL_SLOT, DSA_STATUS )
      GRPQUAL_PTR = ADDRESS

*   Map the COADDS array from the reduced group file.
      CALL DSA_MAP_DATA( 'COADDS', 'UPDATE', 'SHORT', ADDRESS,
     :  COADDS_SLOT, DSA_STATUS )
      COADDS_PTR = ADDRESS

*   Check that all these arrays have been mapped successfully.
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :     /'Error mapping data', STATUS )
      END IF

      IF ( STATUS .EQ. ADAM__OK ) THEN

*      If variance weighting is required, check that the observation
*      variance array contains sensible values.
         IF ( VARIANCE_WT ) THEN

*         Clean up the observation before checking it.
*         (THIS IS A FUDGE!!).
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
               CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :           /'These data are not suitable for variance '/
     :           /'weighting', STATUS )
            END IF
         END IF

         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Determine if the observation is an OBJECT or a SKY.
            IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

*            An OBJECT observation.
*            Co-add the observation data to the reduced group,
*            propagating variance and quality and updating the
*            COADDS array.
               CALL RED4_COADD_OBS( DIMS(1), DIMS(2), 1.0,
     :           VARIANCE_WT,
     :           %val(OBSDATA_PTR), %val(OBSVAR_PTR),
     :           %val(OBSQUAL_PTR),
     :           %val(GRPDATA_PTR), %val(GRPVAR_PTR),
     :           %val(GRPQUAL_PTR),
     :           %val(COADDS_PTR), STATUS )

*            Check this has worked.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               For an OBJECT observation, the FITS parameters in the
*               reduced group file now need to be updated.
*               Check if this is the very first OBJECT observation to be added.
*               It is assumed that a reduced group file with no OBJECT
*               observations added will have an EXPOSED parameter of zero
*               in the FITS structure.
                  DSA_STATUS = STATUS
                  CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :              EXPOSED, COMMENT, DSA_STATUS )

                  IF ( EXPOSED .LE. 0.0 ) THEN

*                  This is the first OBJECT observation.
*                  Remember the value of the SKYEXP, NOBJ and NSKY items in
*                  the FITS structure (just in case there have been any SKY
*                  observations before this OBJECT observation).
                     CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0,
     :                 SKYEXP, COMMENT, DSA_STATUS )
                     CALL DSA_GET_FITS_I( 'GRPRED', 'NOBJ', 0,
     :                 NOBJ, COMMENT, DSA_STATUS )
                     CALL DSA_GET_FITS_I( 'GRPRED', 'NSKY', 0,
     :                 NSKY, COMMENT, DSA_STATUS )

*                  Copy all the FITS structure from the reduced observation
*                  file to the reduced group file (deleting any existing FITS
*                  structure in that file).
                     CALL RED4_COPY_STRUCTURE(
     :                 'OBSRED.'//FITS_STRUCTURE,
     :                 'GRPRED.'//FITS_STRUCTURE, STATUS )

*                  Restore the SKYEXP, NOBJ and NSKY items.
                     DSA_STATUS = STATUS
                     CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :                 ' ', DSA_STATUS )
                     CALL DSA_PUT_FITS_I( 'GRPRED', 'NOBJ', NOBJ+1,
     :                 ' ', DSA_STATUS )
                     CALL DSA_PUT_FITS_I( 'GRPRED', 'NSKY', NSKY,
     :                 ' ', DSA_STATUS )
                     IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :                    /'Error setting FITS items', STATUS )
                     END IF

*                  Initialise the STDUSED item.
                     DSA_STATUS = STATUS
                     CALL DSA_PUT_FITS_C( 'GRPRED', 'STDUSED',
     :                 '(none)', ' ', DSA_STATUS )

*                  Obtain the correct values for the object name and
*                  observation time, and update these in the OBS structure.
                     CALL DSA_GET_FITS_C( 'GRPRED', 'OBJECT', 0,
     :                 OBJECT, COMMENT, DSA_STATUS )
                     CALL DSA_SET_OBJECT( 'GRPRED', OBJECT,
     :                 DSA_STATUS )

                     CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :                 EXPOSED, COMMENT, DSA_STATUS )
                     CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED,
     :                 DSA_STATUS )

*                  Record whether variance weighting is being used in
*                  the FITS structure.
*                  (It is assumed this will be the same for all the
*                  observations in the group).
                     IF ( VARIANCE_WT ) THEN

                        CALL DSA_PUT_FITS_C( 'GRPRED', 'VARWT',
     :                    'Yes', ' ', DSA_STATUS )
                     ELSE

                        CALL DSA_PUT_FITS_C( 'GRPRED', 'VARWT',
     :                    'No', ' ', DSA_STATUS )
                     END IF

*                  Record the error propagation method being used.
*                  For this routine it will be 'FROM_INT' only.
                     CALL DSA_PUT_FITS_C( 'GRPRED', 'ERRPROP',
     :                 'FROM_INT', ' ', DSA_STATUS )

                  ELSE

*                  This is the second or subsequent OBJECT observation.
*                  Most of the fixed FITS parameters will already be
*                  correct, but the following will need updating:
*                  Update RUTSTART and RUTEND to be the minimum and
*                  maximum respectively of those found to far (carry
*                  the UTSTART, AMSTART, UTEND and AMEND parameters with
*                  these).
                     CALL DSA_GET_FITS_F( 'GRPRED', 'RUTSTART', 0,
     :                 RUTSTART, COMMENT, DSA_STATUS )
                     CALL DSA_GET_FITS_F( 'GRPRED', 'RUTEND', 0,
     :                 RUTEND, COMMENT, DSA_STATUS )

                     CALL DSA_GET_FITS_F( 'OBSRED', 'RUTSTART', 0,
     :                 OBSRUTSTART, COMMENT, DSA_STATUS )
                     CALL DSA_GET_FITS_F( 'OBSRED', 'RUTEND', 0,
     :                 OBSRUTEND, COMMENT, DSA_STATUS )

                     CALL DSA_GET_FITS_F( 'OBSRED', 'AMSTART', 0,
     :                 OBSAMSTART, COMMENT, DSA_STATUS )
                     CALL DSA_GET_FITS_F( 'OBSRED', 'AMEND', 0,
     :                 OBSAMEND, COMMENT, DSA_STATUS )

                     IF ( OBSRUTSTART .LT. RUTSTART ) THEN

                        CALL DSA_PUT_FITS_F( 'GRPRED', 'RUTSTART',
     :                    OBSRUTSTART, ' ', DSA_STATUS )

                        CALL DSA_PUT_FITS_F( 'GRPRED', 'AMSTART',
     :                    OBSAMSTART, ' ', DSA_STATUS )
                     END IF

                     IF ( OBSRUTEND .GT. RUTEND ) THEN

                        CALL DSA_PUT_FITS_F( 'GRPRED', 'RUTEND',
     :                    OBSRUTEND, ' ', DSA_STATUS )

                        CALL DSA_PUT_FITS_F( 'GRPRED', 'AMEND',
     :                    OBSAMEND, ' ', DSA_STATUS )
                     END IF

*                  Add the observation time to the current value of the
*                  total exposure time.
                     CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :                 EXPOSED, COMMENT, DSA_STATUS )

                     CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0,
     :                 OBSEXPOSED, COMMENT, DSA_STATUS )

                     EXPOSED = EXPOSED + OBSEXPOSED

                     CALL DSA_PUT_FITS_F( 'GRPRED', 'EXPOSED',
     :                 EXPOSED, ' ', DSA_STATUS )
                     CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED,
     :                 DSA_STATUS )

*                  Increment the NOBJ counter.
                     CALL DSA_GET_FITS_I( 'GRPRED', 'NOBJ', 0,
     :                 NOBJ, COMMENT, DSA_STATUS )

                     NOBJ = NOBJ + 1

                     CALL DSA_PUT_FITS_I( 'GRPRED', 'NOBJ',
     :                 NOBJ, ' ', DSA_STATUS )
                  END IF
               ELSE

*               An error occurred during the co-adding.
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :              /'First arithmetic error during coadd', STATUS )
                  CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :              /'Suspect zero variance in group file',
     :              STATUS )
               END IF

            ELSE IF ( OBSTYPE .EQ. 'SKY' ) THEN

*            A SKY observation.
*            Co-subtract the observation data from the reduced group,
*            weighting the observations by the factor SKY_WT,
*            propagating variance and quality and updating the
*            COADDS array. (Note that the same routine as that to add
*            the OBJECT observation is used, but the weight is
*            multiplied by -1 so the data values are subtracted).
               CALL RED4_COADD_OBS( DIMS(1), DIMS(2), -SKY_WT,
     :           VARIANCE_WT,
     :           %val(OBSDATA_PTR), %val(OBSVAR_PTR),
     :           %val(OBSQUAL_PTR),
     :           %val(GRPDATA_PTR), %val(GRPVAR_PTR),
     :           %val(GRPQUAL_PTR),
     :           %val(COADDS_PTR), STATUS )

*            Check this has worked.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               For a SKY observation, the only post-processing required
*               is to add "- SKY" to the label and update the value of the
*               SKYEXP parameter containing the total observation time of
*               all the contributing SKY observations.
*               Check if this is the very first SKY observation to be added.
*               It is assumed that a reduced group file with no sky
*               observations added will have a SKYEXP parameter of zero
*               in the FITS structure and will not have "- SKY" present
*               in its label.
                  DSA_STATUS = STATUS
                  CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0, SKYEXP,
     :              COMMENT, DSA_STATUS )

                  IF ( SKYEXP .LE. 0.0 ) THEN

*                  This is the first SKY observation.
*                  Obtain the current data label.
                    CALL DSA_GET_DATA_INFO( 'GRPRED', NINFO,
     :                 CHAR_ARRAY, 0, DIGNORE, DSA_STATUS )

*                  Check this label does not already contain a "- SKY"
                     IF ( INDEX( CHAR_ARRAY(2), '- SKY' ) .EQ. 0 ) THEN

*                     Add the string "- SKY" to the end, to indicate that
*                     the data are sky-subtracted. The data label is
*                     CHAR_ARRAY(2). The data units, held in CHAR_ARRAY(1),
*                     are not changed. (Note that, as DSA_GET_DATA_INFO is
*                     a general purpose routine, dummy arguments are needed
*                     to take the place of values we don't need or don't
*                     want to change).
                        CPOS = 0
                        CLEN = MAX( 1, CHR_LEN( CHAR_ARRAY(2) ) )
                        CALL CHR_PUTC( CHAR_ARRAY(2)(1:CLEN),
     :                    NEWLABEL, CPOS )

                        CALL CHR_PUTC( ' - SKY', NEWLABEL, CPOS )

                        CHAR_ARRAY(2) = NEWLABEL
                        CALL DSA_SET_DATA_INFO( 'GRPRED', NINFO,
     :                    CHAR_ARRAY, 0, 0.0D0, DSA_STATUS )
                     END IF
                  END IF

*               Add the observation time of the current sky observation
*               (multiplied by SKY_WT) to SKYEXP and write the new value
*               back to the reduced group file.
                  CALL DSA_GET_FITS_F( 'OBSRED', 'EXPOSED', 0,
     :              OBSEXPOSED, COMMENT, DSA_STATUS )

                  SKYEXP = SKYEXP + OBSEXPOSED * SKY_WT

                  CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :              ' ', DSA_STATUS )

*               Record the sky weighting factor used in the FITS structure.
                  CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYWT', SKY_WT,
     :              ' ', DSA_STATUS )

*               Increment the NSKY counter.
                  CALL DSA_GET_FITS_I( 'GRPRED', 'NSKY', 0,
     :              NSKY, COMMENT, DSA_STATUS )

                  NSKY = NSKY + 1

                  CALL DSA_PUT_FITS_I( 'GRPRED', 'NSKY',
     :              NSKY, ' ', DSA_STATUS )
               ELSE

*               An error occurred during the co-adding.
                 STATUS = SAI__ERROR
                 CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :             /'Second arithmetic error during coadd', STATUS )
                 CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :             /'Suspect zero variance in group file', STATUS )
               END IF
            END IF

*         If everything has worked, copy relevant items from the FITS
*         structure of the observation file into the COADD structure
*         corresponding to this observation.
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :           /'Error getting/setting FITS items', STATUS )
            END IF

            CLEN = MAX( 1, CHR_LEN( COADDED_OBS ) )
            CALL RED4_COPY_STRUCTURE( 'OBSRED.'//FITS_STRUCTURE,
     :        COADDED_OBS(1:CLEN)//'.'//COADD_NAME, STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_2: '/
     :     /'Error mapping arrays', STATUS )
      END IF

      END
