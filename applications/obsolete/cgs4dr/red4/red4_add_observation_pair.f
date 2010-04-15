C DEC/CMS REPLACEMENT HISTORY, Element RED4_ADD_OBSERVATION_PAIR.FOR
C *12    1-JUN-1994 15:56:25 PND "OK"
C *11   31-MAR-1994 17:40:33 PND "Update for IRCAM3"
C *10   21-JAN-1994 10:38:35 PND "LOOK"
C *9    19-JAN-1994 16:08:57 PND "Add fits structure"
C *8     7-JUN-1993 10:13:12 PND "Add error context"
C *7    19-FEB-1993 08:40:55 PND "Conform to error strategY"
C *6    20-FEB-1992 15:58:41 PND "Add POLYSKY for OBJ-SKY pairs"
C *5     1-OCT-1991 11:28:44 PND "Change to GEN_*AFV calls"
C *4    11-SEP-1991 05:41:49 SMB "Renamed GEN_MULCAFEV to GEN_MULCAFV."
C *3     3-SEP-1991 02:52:05 SMB "Renamed GEN_MULCAFE to GEN_MULCAFEV."
C *2    21-AUG-1991 16:13:44 KEVIN "Did not change it"
C *1     2-JUN-1991 14:42:16 AB "Insert .FORs"
C DEC/CMS REPLACEMENT HISTORY, Element RED4_ADD_OBSERVATION_PAIR.FOR
*+  RED4_ADD_OBSERVATION_PAIR - Add pair of observations to reduced group file.
      SUBROUTINE RED4_ADD_OBSERVATION_PAIR( OBJECT_OBS, SKY_OBS,
     :  COADDED_OBS, COADD_OBJ, COADD_SKY, ERRORS, SKY_WT, VARIANCE_WT,
     :  STATUS )
*    Description :
*     This is a lower level routine, called by RED4_CONSTRUCT_PAIR, which
*     adds a pair of reduced observations belonging to a particular group
*     to a reduced group file. The routine is called after all the checks
*     on the suitability of the reduced observation and the existence
*     of the reduced group have been made. It is assumes that the reduced
*     group has been opened with a DSA reference of 'GRPRED',
*     and the .MORE.CGS4_COADDS structure within the reduced group has
*     been opened with a reference of 'COADDS'. When this routine is
*     called non of the data arrays have been mapped.
*
*     The SKY observation is subtracted from the OBJECT observation
*     and the result added to the contents of the reduced group file.
*     SKY observations may optionally be multiplied by a weighting factor
*     indicated by the SKY_WT parameter.
*     Observation types other than OBJECT and SKY are not allowed.
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
*     CALL RED4_ADD_OBSERVATION_PAIR( OBJECT_OBS, SKY_OBS, COADDED_OBS,
*     :  COADD_OBJ, COADD_SKY, ERRORS, SKY_WT, VARIANCE_WT, STATUS )
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
*     SKY_WT      = REAL( READ )
*           SKY observation weighting factor. (SKY observations are
*           multiplied by this factor before being combined with
*           OBJECT observations).
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     19-Sep-1990: Original version, as RED4_ADD_OBSERVATION_2.   (SMB)
*     26-Jan-1991: New routine created as part of a last minute
*                  major change to the data reduction
*                  specification. Pairs of observations are now
*                  co-added to a group.                           (SMB)
*     30-Jan-1991: Bug fix. OBJFITS was only being obtained for
*                  the first OBJECT observation.                  (SMB)
*     31-Jan-1991: Some major deficiencies in the sky background
*                  subtraction algorithm in RED4_ADD_OBSERVATION
*                  have been found. To get around these, this
*                  routine has been enhanced to deal with all
*                  the sky-subtraction possibilities. This routine
*                  takes OBJECT and SKY observations in pairs, so
*                  it does not suffer from the afformentioned
*                  deficiencies. (It probably suffers from some
*                  other deficiencies lurking away somewhere...). (SMB)
*     17-Feb-1991: Bug fix. This routine was not writing the
*                  VARWT, ERRPROP and SKYWT FITS items.           (SMB)
*     18-Feb-1991: Fudge to clean observations before variance
*                  weighting added.                               (SMB)
*     21-Feb-1991: This routine was corrupting the NOBJ and NSKY
*                  FITS parameters set by RED4_ADD_PAIR. Modified
*                  to preserve them. (This is rather a mess).     (SMB)
*     23-Feb-1991: Initialise STDUSED item.                       (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.          (SMB)
*      3-Sep-1991: GEN_MULCAFE renamed to GEN_MULCAFEV. The CGS4
*                  software was assuming this routine dealt with
*                  variances, but the actual Figaro routine dealt
*                  with standard deviation.                       (SMB)
*     11-Sep-1991: GEN_MULCAFEV renamed to GEN_MULCAFV, and argument
*                  list made compatible with Figaro version.      (SMB)
*      1-Oct-1991: Change GEN_*AFE calls to GEN_*AFV.             (PND)
*     10-Jan-1992: Add POLYFIT for OBJ-SKY pairs                  (PND)
*     18-Feb-1993: Conform to error strategy                      (PND)
*     19-Jan-1994: Pass FITS_STRUCTURE to DSA_SPECIFIC_STRUCTURE  (PND)
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
      INCLUDE 'RED4_COMMON.INC'    ! RED4 Common Block
*    Import :
      CHARACTER*(*)
     :  OBJECT_OBS,                ! Name of OBJECT observation.
     :  SKY_OBS,                   ! Name of SKY observation.
     :  COADDED_OBS,               ! Name of COADDED_OBS structure.
     :  COADD_OBJ,                 ! Name of coadd item for OBJECT observation.
     :  COADD_SKY,                 ! Name of coadd item for SKY observation.
     :                             !   added using variance weighting
     :  ERRORS                     ! Error propagation method.
      REAL
     :  SKY_WT                     ! SKY observation weighting factor
      LOGICAL
     :  VARIANCE_WT                ! TRUE if variance weighting switched on.
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
      CHARACTER*80
     :  OBJECT_ROBS,               ! OBJECT reduced observation file.
     :  SKY_ROBS                   ! SKY reduced observation file.
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
     :  FLEN,                      ! Length of character string.
     :  DSA_STATUS,                ! DSA status value
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
     :  SKYVAR_PTR,                ! Pointer to variance array mapped from
*                                  !    SKY reduced observation file
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

      DSA_STATUS = STATUS

*   Convert the OBJECT and SKY observation file names into reduced
*   observation names, and open them.
      CALL RED4_OBSTOROBS( OBJECT_OBS, OBJECT_ROBS, STATUS )
      CALL RED4_OBSTOROBS( SKY_OBS, SKY_ROBS, STATUS )
      CALL RED4_CHECK_INPUT( OBJECT_ROBS, STATUS )
      CALL DSA_NAMED_INPUT( 'OBJFILE', OBJECT_ROBS, DSA_STATUS )
      CALL DSA_NAMED_INPUT( 'SKYFILE', SKY_ROBS, DSA_STATUS )

*   Determine the size of the data arrays in the reduced group file.
*   (These have already been verified to be the same size as the ones
*   in the reduced observation file).
      CALL DSA_DATA_SIZE( 'GRPRED', MAXDIM, NDIM, DIMS, NELM,
     :  DSA_STATUS )

*   Indicate to DSA that a data quality array will be used to
*   indicate bad values in all the structures.
      CALL DSA_USE_QUALITY( 'OBJFILE', DSA_STATUS )
      CALL DSA_USE_QUALITY( 'SKYFILE', DSA_STATUS )
      CALL DSA_USE_QUALITY( 'GRPRED', DSA_STATUS )

*   Map the data, variance and quality arrays from the OBJECT reduced
*   observation file.
      CALL DSA_MAP_DATA( 'OBJFILE', 'READ', 'FLOAT', OBJDATA_PTR,
     :  OBJDATA_SLOT, DSA_STATUS )

      IF ( ERRORS .EQ. 'FROM_INT' ) THEN

          CALL DSA_MAP_VARIANCE( 'OBJFILE', 'READ', 'FLOAT',
     :     OBJVAR_PTR, OBJVAR_SLOT, DSA_STATUS )
      END IF

      CALL DSA_MAP_QUALITY( 'OBJFILE', 'READ', 'BYTE', OBJQUAL_PTR,
     :  OBJQUAL_SLOT, DSA_STATUS )

*   Map the data, variance and quality arrays from the SKY reduced
*   observation file.
      CALL DSA_MAP_DATA( 'SKYFILE', 'READ', 'FLOAT', SKYDATA_PTR,
     :  SKYDATA_SLOT, DSA_STATUS )

      IF ( ERRORS .EQ. 'FROM_INT' ) THEN

         CALL DSA_MAP_VARIANCE( 'SKYFILE', 'READ', 'FLOAT',
     :     SKYVAR_PTR, SKYVAR_SLOT, DSA_STATUS )
      END IF

      CALL DSA_MAP_QUALITY( 'SKYFILE', 'READ', 'BYTE', SKYQUAL_PTR,
     :  SKYQUAL_SLOT, DSA_STATUS )

*   If required, map some work arrays to hold the sky-subtracted
*   data, variance and quality.
      IF ( ERRORS .EQ. 'FROM_INT' ) THEN

         CALL DSA_GET_WORK_ARRAY( NELM, 'FLOAT', WDATA_PTR,
     :     WDATA_SLOT, DSA_STATUS )
         CALL DSA_GET_WORK_ARRAY( NELM, 'FLOAT', WVAR_PTR,
     :     WVAR_SLOT, DSA_STATUS )
         CALL DSA_GET_WORK_ARRAY( NELM, 'BYTE', WQUAL_PTR,
     :     WQUAL_SLOT, DSA_STATUS )
      END IF

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
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_PAIR: '/
     :     /'Error mapping arrays', STATUS )
      END IF

      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Decide how errors are to be propagated.
         IF ( ERRORS .EQ. 'FROM_INT' ) THEN

*         Errors are to be propagated from those mapped from the
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

*         If variance weighting is required, check that the resultant
*         variance array contains sensible values.
            IF ( VARIANCE_WT ) THEN

*            Clean up the observation pair before checking it.
*            (THIS IS A FUDGE!!).
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
                  CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_PAIR: '/
     :              /'These data are not suitable for variance '/
     :              /'weighting', STATUS )
               END IF
            END IF

*         Before coadding POLYFIT the OBJ-SKY if required
            IF ( PF_POLYFIT .EQ. 'OBJ-SKY' ) THEN

              CALL RED4_RPOLYFIT( DIMS(1), DIMS(2),
     :           %val(WDATA_PTR), %val(WVAR_PTR),
     :           %val(WQUAL_PTR), STATUS )
            END IF

*         Coadd this sky-subtracted pair into the reduced group,
*         propagating the variance and quality.
            CALL RED4_COADD_OBS( DIMS(1), DIMS(2), 1.0,
     :        VARIANCE_WT,
     :        %val(WDATA_PTR), %val(WVAR_PTR), %val(WQUAL_PTR),
     :        %val(GRPDATA_PTR), %val(GRPVAR_PTR), %val(GRPQUAL_PTR),
     :        %val(COADDS_PTR), STATUS )
         ELSE

*         Errors are to be determined from the variations between
*         OBJECT/SKY pairs.
*         Take OBJECT-SKY and coadd this to the reduced group, determining
*         the variance and propagating data quality.
            CALL RED4_COADD_PAIR( DIMS(1), DIMS(2), SKY_WT,
     :        %val(OBJDATA_PTR), %val(OBJQUAL_PTR),
     :        %val(SKYDATA_PTR), %val(SKYQUAL_PTR),
     :        %val(GRPDATA_PTR), %val(GRPVAR_PTR), %val(GRPQUAL_PTR),
     :        %val(COADDS_PTR), STATUS )
         END IF

         IF ( STATUS .EQ. ADAM__OK ) THEN

*         1) First add the OBJECT observation information to the
*            group file header.
*         2) For an OBJECT observation, the FITS parameters in the
*            reduced group file now need to be updated.
*         3) Check if this is the very first OBJECT observation to be added.
*            It is assumed that a reduced group file with no OBJECT
*            observations added will have an EXPOSED parameter of zero
*            in the FITS structure.
            CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :        EXPOSED, COMMENT, DSA_STATUS )

            IF ( EXPOSED .LE. 0.0 ) THEN

*            This is the first OBJECT observation.
*            Remember the value of the SKYEXP item in the FITS
*            structure (just in case there have been any SKY
*            observations before this OBJECT observation).
               CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0,
     :           SKYEXP, COMMENT, DSA_STATUS )

*            Remember the current values of NOBJ and NSKY.
               CALL DSA_GET_FITS_I( 'GRPRED', 'NOBJ', 0,
     :           NOBJ, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_I( 'GRPRED', 'NSKY', 0,
     :           NSKY, COMMENT, DSA_STATUS )

*            Copy all the FITS structure from the reduced observation
*            file to the reduced group file (deleting any existing FITS
*            structure in that file).
               FLEN = CHR_LEN( FITS_STRUCTURE )
               CALL RED4_COPY_STRUCTURE(
     :           'OBJFILE.'//FITS_STRUCTURE(1:FLEN),
     :           'GRPRED.'//FITS_STRUCTURE(1:FLEN), STATUS )

*            Restore the SKYEXP item.
               DSA_STATUS = STATUS
               CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :           ' ', DSA_STATUS )

*            Restore the NOBJ and NSKY items.
               CALL DSA_PUT_FITS_I( 'GRPRED', 'NOBJ', NOBJ,
     :           ' ', DSA_STATUS )
               CALL DSA_PUT_FITS_I( 'GRPRED', 'NSKY', NSKY,
     :           ' ', DSA_STATUS )
               IF ( DSA_STATUS .NE. ADAM__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_PAIR: '/
     :              /'First error putting FITS items', STATUS )
               END IF

*            Initialise the STDUSED item.
               DSA_STATUS = STATUS
               CALL DSA_PUT_FITS_C( 'GRPRED', 'STDUSED',
     :           '(none)', ' ', DSA_STATUS )

*            Obtain the correct values for the object name and
*            observation time, and update these in the OBS structure.
               CALL DSA_GET_FITS_C( 'GRPRED', 'OBJECT', 0,
     :           OBJECT, COMMENT, DSA_STATUS )
               CALL DSA_SET_OBJECT( 'GRPRED', OBJECT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :           EXPOSED, COMMENT, DSA_STATUS )
               CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED,
     :           DSA_STATUS )

*            Record whether variance weighting is being used in
*            the FITS structure.
*            (It is assumed this will be the same for all the
*            observations in the group).
               IF ( VARIANCE_WT ) THEN

                  CALL DSA_PUT_FITS_C( 'GRPRED', 'VARWT',
     :              'Yes', ' ', DSA_STATUS )
               ELSE

                  CALL DSA_PUT_FITS_C( 'GRPRED', 'VARWT',
     :              'No', ' ', DSA_STATUS )
               END IF

*            Record the error propagation method being used.
               CALL DSA_PUT_FITS_C( 'GRPRED', 'ERRPROP',
     :           ERRORS, ' ', DSA_STATUS )
            ELSE

*            This is the second or subsequent OBJECT observation.
*            Most of the fixed FITS parameters will already be
*            correct, but the following will need updating:
*            Update RUTSTART and RUTEND to be the minimum and
*            maximum respectively of those found to far (carry
*            the UTSTART, AMSTART, UTEND and AMEND parameters with
*            these).
               CALL DSA_GET_FITS_F( 'GRPRED', 'RUTSTART', 0,
     :           RUTSTART, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'GRPRED', 'RUTEND', 0,
     :           RUTEND, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'OBJFILE', 'RUTSTART', 0,
     :           OBSRUTSTART, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'OBJFILE', 'RUTEND', 0,
     :           OBSRUTEND, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'OBJFILE', 'AMSTART', 0,
     :           OBSAMSTART, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'OBJFILE', 'AMEND', 0,
     :           OBSAMEND, COMMENT, DSA_STATUS )

               IF ( OBSRUTSTART .LT. RUTSTART ) THEN

                  CALL DSA_PUT_FITS_F( 'GRPRED', 'RUTSTART',
     :              OBSRUTSTART, ' ', DSA_STATUS )
                  CALL DSA_PUT_FITS_F( 'GRPRED', 'AMSTART',
     :              OBSAMSTART, ' ', DSA_STATUS )
               END IF

               IF ( OBSRUTEND .GT. RUTEND ) THEN

                  CALL DSA_PUT_FITS_F( 'GRPRED', 'RUTEND',
     :              OBSRUTEND, ' ', DSA_STATUS )
                  CALL DSA_PUT_FITS_F( 'GRPRED', 'AMEND',
     :              OBSAMEND, ' ', DSA_STATUS )
               END IF

*            Add the observation time to the current value of the
*            total exposure time.
               CALL DSA_GET_FITS_F( 'GRPRED', 'EXPOSED', 0,
     :           EXPOSED, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'OBJFILE', 'EXPOSED', 0,
     :           OBSEXPOSED, COMMENT, DSA_STATUS )

               EXPOSED = EXPOSED + OBSEXPOSED

               CALL DSA_PUT_FITS_F( 'GRPRED', 'EXPOSED',
     :           EXPOSED, ' ', DSA_STATUS )
               CALL DSA_SET_EXPOSURE( 'GRPRED', EXPOSED,
     :           DSA_STATUS )
            END IF

*         If everything has worked, copy relevant items from the FITS
*         structure of the OBJECT observation file into the COADD structure
*         corresponding to this observation.
            CLEN = MAX( 1, CHR_LEN( COADDED_OBS ) )
            FLEN = CHR_LEN( FITS_STRUCTURE )
            CALL RED4_COPY_STRUCTURE(
     :        'OBJFILE.'//FITS_STRUCTURE(1:FLEN),
     :        COADDED_OBS(1:CLEN)//'.'//COADD_OBJ, STATUS )

*         1) Secondly, add the SKY observation information to the
*            group file info.
*         2) For a SKY observation, the only post-processing required
*            is to add "- SKY" to the label and update the value of the
*            SKYEXP parameter containing the total observation time of
*            all the contributing SKY observations.
*         3) Check if this is the very first SKY observation to be added.
*            It is assumed that a reduced group file with no sky
*            observations added will have a SKYEXP parameter of zero
*            in the FITS structure.
            DSA_STATUS = STATUS
            CALL DSA_GET_FITS_F( 'GRPRED', 'SKYEXP', 0, SKYEXP,
     :        COMMENT, DSA_STATUS )

            IF ( SKYEXP .LE. 0.0 ) THEN

*            This is the first SKY observation.
*            Obtain the current data label and add the string "- SKY"
*            to the end, to indicate that the data are sky-subtracted.
*            The data label is CHAR_ARRAY(2). The data units, held in
*            CHAR_ARRAY(1), are not changed.
*            (Note that, as DSA_GET_DATA_INFO is a general purpose routine,
*            dummy arguments are needed to take the place of values we don't
*            need or don't want to change).
               CALL DSA_GET_DATA_INFO( 'GRPRED', NINFO,
     :           CHAR_ARRAY, 0, DIGNORE, DSA_STATUS )

               CPOS = 0
               CLEN = MAX( 1, CHR_LEN( CHAR_ARRAY(2) ) )
               CALL CHR_PUTC( CHAR_ARRAY(2)(1:CLEN), NEWLABEL,
     :           CPOS )
               CALL CHR_PUTC( ' - SKY', NEWLABEL, CPOS )
               CHAR_ARRAY(2) = NEWLABEL
               CALL DSA_SET_DATA_INFO( 'GRPRED', NINFO,
     :            CHAR_ARRAY, 0, 0.0D0, DSA_STATUS )

*            Record the sky weighting factor used in the FITS structure.
               CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYWT', SKY_WT,
     :           ' ', DSA_STATUS )
            END IF

*         Add the observation time of the current sky observation
*         (multiplied by SKY_WT) to SKYEXP and write the new value
*         back to the reduced group file.
            CALL DSA_GET_FITS_F( 'SKYFILE', 'EXPOSED', 0,
     :        OBSEXPOSED, COMMENT, DSA_STATUS )

            SKYEXP = SKYEXP + OBSEXPOSED * SKY_WT

            CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYEXP', SKYEXP,
     :        ' ', DSA_STATUS )

*         Record the sky weighting factor used in the FITS structure.
            CALL DSA_PUT_FITS_F( 'GRPRED', 'SKYWT', SKY_WT,
     :        ' ', DSA_STATUS )

*         If everything has worked, copy relevant items from the FITS
*         structure of the SKY observation file into the COADD structure
*         corresponding to this observation.
            CLEN = MAX( 1, CHR_LEN( COADDED_OBS ) )
            FLEN = CHR_LEN( FITS_STRUCTURE )
            CALL RED4_COPY_STRUCTURE(
     :        'SKYFILE.'//FITS_STRUCTURE(1:FLEN),
     :        COADDED_OBS(1:CLEN)//'.'//COADD_SKY, STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_PAIR: '/
     :     /'Error mapping arrays', STATUS )
      END IF

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION_PAIR: '/
     :     /'Exit error', STATUS )
      END IF

      END
