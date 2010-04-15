*+  RED4_ADD_PAIR - Combine observation into pair in reduced group file
      SUBROUTINE RED4_ADD_PAIR( STATUS )
*    Description :
*     This routine combines a reduced observation belonging to a particular
*     group into a pair of observations. If this is the first observation
*     of the pair, its name is simply remembered until next time. If this
*     is the second observation, the SKY member of the pair is subtracted
*     from the OBJECT member, and the result co-added to the group.
*     If the reduced group file does not exist, a new one is created and
*     initialised. SKY observations may optionally be multiplied by a
*     weighting factor indicated by the SKY_WT parameter. Pairs of
*     observations may also be weighted according to their variance.
*     Pairs of observations must consist of one of type OBJECT and one
*     of type SKY. Other combinations are not allowed.
*
*     The variance in the group may be produced by combining together
*     the variances contained in the individual observations, or by
*     estimating it from the the variations between the pairs of
*     observations when they are co-added into the group. The latter
*     will be necessary if the observations consist of only one exposure
*     per integration and one detector scan, and therefore contain no
*     error information themselves.
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
*     UTSTART     - Carried with RUTSTART
*     UTEND       - Carried with RUTEND
*     AMSTART     - Carried with RUTSTART
*     AMEND       - Carried with RUTEND
*     OBSNUM      - Deleted
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
*     CALL RED4_ADD_PAIR( STATUS )
*    Parameters :
*     STATUS  = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
*    Bugs :
*     There is a bug in DSA_MATCH_UNITS, in which it claims the data
*     units in the reduced observation file and reduced group file are
*     different even when they are identical ! A private version of
*     DSA_MATCH_UNITS needs to be used until KS releases a fixed version.
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     19-Sep-1990: Original version, as RED4_ADD_OBSERVATION.      (SMB)
*     26-Jan-1991: Data reduction specification changed!!!
*                  It has transpired that a common observing mode
*                  will be one in which integrations consisting of
*                  only one exposure will be taken, and only one
*                  scan of the detector will be made before combining
*                  these into an observation. Consequently, these
*                  observations will contain no error information,
*                  and it will be necessary to estimate the errors
*                  when pairs of OBJECT and SKY observations are
*                  combined into a group. This routine is an
*                  alternative to RED4_ADD_OBSERVATION, which I
*                  have left the same. The changes have been put
*                  into this new action so the original one will
*                  be available when (if?) things stop working.    (SMB)
*     31-Jan-1991: Some major deficiencies in the sky background
*                  subtraction algorithm in RED4_ADD_OBSERVATION
*                  have been found. To get around these, this
*                  routine has been enhanced to deal with all
*                  the sky-subtraction possibilities. This routine
*                  takes OBJECT and SKY observations in pairs, so
*                  it does not suffer from the afformentioned
*                  deficiencies. (It probably suffers from some
*                  other deficiencies lurking away somewhere...).  (SMB)
*      1-Feb-1991: Typing mistake fixed.                           (SMB)
*      8-Feb-1991: Last observation info moved to common block.    (SMB)
*     15-Feb-1991: Modified to reject observations which have not
*                  been properly reduced.                          (SMB)
*     21-Feb-1991: Modified to maintain NOBJ and NSKY counters.    (SMB)
*     22-Feb-1991: Advice included if an attempt is made to create
*                  an OBJECT OBJECT pair.                          (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.           (SMB)
*     29-Aug-1991: Add POLYFIT enhanced sky subtraction            (PND)
*     19-Feb-1993: Conform to error strategy                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER
     :  STATUS                     ! Global status
*    External references :
      INTEGER CHR_LEN              ! Character length determining function
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Local Constants :
*    Local variables :
      CHARACTER*80
     :  OBSFILE,                   ! Name of observation file.
     :  OBSRED,                    ! Name of reduced observation file.
     :  GRPRED,                    ! Name of reduced group file.
     :  COADDS,                    ! Name of COADDS structure.
     :  COADDED_OBS                ! Name of COADDED_OBS structure.
      CHARACTER*32
     :  STREDUCE                   ! Time of last successful reduction step.
      CHARACTER*80
     :  OBSTYPE,                   ! Observation type.
     :  ERRORS,                    ! Type of error propagation required
     :                             !    ('FROM_INT' or 'FROM_OBS').
     :  COADD_NAME,                ! Name of coadd item for current observation.
     :  COADD_OBJ,                 ! Name of coadd item for OBJECT observation.
     :  COADD_SKY,                 ! Name of coadd item for SKY observation.
     :  SKYOBS,                    ! Name of SKY observation
     :  OBJECTOBS                  ! Name of OBJECT observation
      CHARACTER*4
     :  COMMENT                    ! Dummy comment.
      INTEGER
     :  GRPNUM,                    ! Group number.
     :  NOBJ,                      ! Number of OBJECT observations.
     :  NSKY,                      ! Number of SKY observations.
     :  CLEN,                      ! Length of character string.
     :  DSA_STATUS                 ! DSA status value
      LOGICAL
     :  VARIANCE_WT,               ! Indicates if variance weighting required.
     :  COMBINATION_OK,            ! Indicates if legal OBJECT/SKY combination
     :  EXIST,                     ! Indicates if reduced group exists.
     :  FOUND                      ! Indicates if structure found.
      REAL
     :  SKY_WT                     ! Sky observation weighting factor
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the observation file to be added.
      CALL PAR_GET0C( 'OBSFILE', OBSFILE, STATUS )

      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'OBSERVATION', OBSFILE )
         CALL MSG_OUT( ' ', 'RED4_ADD_PAIR: '/
     :        /'Observation is ^OBSERVATION', STATUS )
      ENDIF

*   Obtain the error propagation method required
      CALL PAR_GET0C( 'ERRORS', ERRORS, STATUS )
      CALL CHR_UCASE( ERRORS )

*   Obtain the sky weighting factor.
      CALL PAR_GET0R( 'SKY_WT', SKY_WT, STATUS )

*   Obtain the polyfit sky-subtraction enhancement options
      CALL PAR_GET0C( 'PF_POLYFIT', PF_POLYFIT, STATUS )
      CALL PAR_GET0L( 'PF_WEIGHT', PF_WEIGHT, STATUS )
      CALL PAR_GET0I( 'PF_DEGREE', PF_DEGREE, STATUS )
      CALL PAR_GET0I( 'PF_NREJECT', PF_NREJECT, STATUS )
      CALL PAR_GET0I( 'PF_SAYS1', PF_SAYS1, STATUS )
      CALL PAR_GET0I( 'PF_SAYE1', PF_SAYE1, STATUS )
      CALL PAR_GET0I( 'PF_SAYS2', PF_SAYS2, STATUS )
      CALL PAR_GET0I( 'PF_SAYE2', PF_SAYE2, STATUS )
      CALL PAR_GET0I( 'PF_SAYS3', PF_SAYS3, STATUS )
      CALL PAR_GET0I( 'PF_SAYE3', PF_SAYE3, STATUS )
      CALL PAR_GET0I( 'PF_SAYS4', PF_SAYS4, STATUS )
      CALL PAR_GET0I( 'PF_SAYE4', PF_SAYE4, STATUS )

*   Obtain whether variance weighting is required.
*   (This is only possible if errors have already been obtained from
*   the variation between integrations).
      CALL PAR_GET0L( 'VARIANCE_WT', VARIANCE_WT, STATUS )

      IF ( (VARIANCE_WT) .AND. (ERRORS .EQ. 'FROM_OBS') ) THEN

         CALL MSG_OUT( ' ', '*** Variance weighting is only '/
     :     /'allowed when errors already determined - ignored.',
     :     STATUS )
         VARIANCE_WT = .FALSE.
      END IF

*   Check the parameters have been obtained successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         DSA_STATUS = STATUS
         CALL DSA_OPEN( DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :        /'Error opening DSA', STATUS )
         END IF

*      Convert the observation file name into the name of the reduced
*      observation file.
         CALL RED4_OBSTOROBS( OBSFILE, OBSRED, STATUS )
         IF ( VERBOSE ) THEN
            CALL MSG_SETC( 'ROBSERVATION', OBSRED )
            CALL MSG_OUT( ' ', 'RED4_ADD_PAIR: '/
     :        /'Reduced observation is ^ROBSERVATION', STATUS )
         ENDIF

*      Open the reduced observation file for input.
         DSA_STATUS = STATUS
         CALL RED4_CHECK_INPUT( OBSRED, STATUS )
         CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED, DSA_STATUS )

*      Obtain the observation type from the FITS structure of the
*      reduced observation file.
         CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBSTYPE,
     :     COMMENT, DSA_STATUS )

*      Also, obtain the group number to which this observation belongs.
         CALL DSA_GET_FITS_I( 'OBSRED', 'GRPNUM', 0, GRPNUM,
     :     COMMENT, DSA_STATUS )

*      Obtain the data reduction status of this observation.
         CALL DSA_GET_FITS_C( 'OBSRED', 'STREDUCE', 0, STREDUCE,
     :     COMMENT, DSA_STATUS )

*      Check this has worked.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :        /'Error getting FITS items', STATUS )
         END IF

         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Ensure that the observation is of type 'OBJECT' or 'SKY'.
            IF ( (OBSTYPE .EQ. 'OBJECT') .OR.
     :           (OBSTYPE .EQ. 'SKY') ) THEN

*            Check that the observation has been properly reduced.
               IF ( ( STREDUCE .NE. ' ' ) .AND.
     :              ( STREDUCE .NE. '(not properly reduced)' ) ) THEN

*               Convert the observation file name, together with the
*               group number obtained above, into the name of the
*               reduced group file.
                  CALL RED4_ROBSTOGRP( OBSRED, GRPNUM, GRPRED, STATUS )

*               Check is this reduced group file exists.
                  DSA_STATUS = STATUS
                  CALL DSA_SEEK_NAMED_STRUCTURE( GRPRED, EXIST,
     :              DSA_STATUS )

*               If this has worked, and the structure does not exist,
*               then create it. (Note that for convenience DSA is opened
*               separately inside RED4_MAKE_GRPREDFILE and therefore has
*               to be closed and reopened. This also means the reduced
*               observation file has to be reopened).
                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                 /'First error seeking CGS4 specific structure',
     :                 STATUS )
                  END IF

                  IF ( ( STATUS .EQ. ADAM__OK ) .AND.
     :                 ( .NOT. EXIST ) ) THEN

                     DSA_STATUS = STATUS
                     CALL DSA_CLOSE( DSA_STATUS )
                     IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                    /'Error closing DSA', STATUS )
                     END IF

*                  Note that RED4_MAKE_GRPREDFILE will reset the last
*                  observation record and the observation counter in the
*                  common block.
                     CALL RED4_MAKE_GRPREDFILE( OBSRED, GRPRED, STATUS )

                     DSA_STATUS = STATUS
                     CALL DSA_OPEN( DSA_STATUS )
                     CALL RED4_CHECK_INPUT( OBSRED, STATUS )
                     CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED,
     :                 DSA_STATUS )
                  END IF

*               Open the reduced group file.
                  CALL RED4_CHECK_INPUT( GRPRED, STATUS )
                  CALL DSA_NAMED_INPUT( 'GRPRED', GRPRED,
     :              DSA_STATUS )

*               Check that the following conditions between the reduced
*               observation and reduced group structure are satisfied:-
*               (a) The data arrays are the same size (to ensure the
*                   same degree of oversampling has been used).
*               (b) The data units are the same (to ensure some observations
*                   haven't been normalised or transformed).
*               (c) The X axis size and units are the same (to ensure
*                   that wavelength calibrated data is not added by accident).
*               If all these conditions are not met, an error is generated.
                  CALL DSA_MATCH_SIZES( 'OBSRED', 'GRPRED', DSA_STATUS )
                  CALL DSA_MATCH_UNITS( 'OBSRED', 'GRPRED', DSA_STATUS )
                  CALL DSA_MATCH_AXIS( 'OBSRED', 1, 'GRPRED', 1,
     :               DSA_STATUS )

*               Close the observation file.
                  CALL DSA_CLOSE_STRUCTURE( 'OBSRED', DSA_STATUS )

*               Open the COADDS structure within the reduced group file
                  CLEN = MAX( 1, CHR_LEN( GRPRED ) )
                  COADDS = GRPRED(1:CLEN) // '.MORE.CGS4_COADDS'

                  CALL DSA_NAMED_INPUT( 'COADDS', COADDS,
     :              DSA_STATUS )

*               Obtain the DTA address of the COADDED_OBS structure
*               within the COADDS structure.
                  CALL DSA_SPECIFIC_STRUCTURE( 'COADDS',
     :               'COADDED_OBS', 'UPDATE', COADDED_OBS, DSA_STATUS )

                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                 /'Second error seeking CGS4 specific structure',
     :                 STATUS )
                  END IF

*               Convert the observation name into its corresponding
*               coadd structure name.
                  CALL RED4_OBSTOCOADD( OBSFILE, COADD_NAME, STATUS )

*               Check everything has worked so far.
                  IF ( STATUS .EQ. ADAM__OK ) THEN

*                  Check to see if this coadd structure exists. If it does,
*                  the observation has already been applied, and should be
*                  ignored.
                     CALL RED4_SEEK_ITEM( COADDED_OBS, COADD_NAME,
     :                  FOUND, STATUS )

                     IF ( .NOT. FOUND ) THEN

*                     The observation has not already been applied.
*                     Increment the observation counter, and decide if
*                     a pair of observations has been completed.
                        OBSERVATION_COUNTER = OBSERVATION_COUNTER + 1

                        IF ( MOD( OBSERVATION_COUNTER, 2 ) .EQ. 0 ) THEN

*                        A pair has been completed. Reject any pair which
*                        does not consist of one OBJECT and one SKY
*                        observation, and determine which is which.
                           COMBINATION_OK = .FALSE.

                           IF ( LAST_OBSERVATION_TYPE .EQ. 'SKY' ) THEN

                              SKYOBS = LAST_OBSERVATION_NAME
                              CALL RED4_OBSTOCOADD( SKYOBS,
     :                          COADD_SKY, STATUS )

                              IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

                                 OBJECTOBS = OBSFILE
                                 COADD_OBJ = COADD_NAME
                                 COMBINATION_OK = .TRUE.
                              END IF
                           ELSE IF ( LAST_OBSERVATION_TYPE .EQ.
     :                       'OBJECT' ) THEN

                              OBJECTOBS = LAST_OBSERVATION_NAME
                              CALL RED4_OBSTOCOADD( OBJECTOBS,
     :                          COADD_OBJ, STATUS )

                              IF ( OBSTYPE .EQ. 'SKY' ) THEN

                                 SKYOBS = OBSFILE
                                 COADD_SKY = COADD_NAME
                                 COMBINATION_OK = .TRUE.
                              ELSE IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

*                              Give some advice if both members of the
*                              pair are OBJECT.
                                 CALL MSG_OUT( ' ', 'NOTE: Adding '/
     :                             /'observations in pairs should '/
     :                             /'be switched off', STATUS )
                                 CALL MSG_OUT( ' ', '      to make '/
     :                             /'a group from OBJECT '/
     :                             /'observations only.', STATUS )
                              END IF
                           END IF

*                        Check that a legal combination of OBJECT and SKY
*                        has been given.
                           IF ( COMBINATION_OK ) THEN

*                           Issue a message.
                              CALL MSG_SETC( 'OBJECTOBS', OBJECTOBS )
                              CALL MSG_SETC( 'SKYOBS', SKYOBS )
                              CALL MSG_OUT( ' ', 'Subtracting '/
     :                          /'^SKYOBS from ^OBJECTOBS to make '/
     :                          /'a pair.', STATUS )

                              CALL MSG_SETC( 'GRPRED', GRPRED )
                              CALL MSG_OUT( ' ', 'Then adding '/
     :                          /'pair to ^GRPRED.', STATUS )

*                           In verbose mode, give some more info.
                              IF ( VERBOSE ) THEN

                                 IF ( ERRORS .EQ. 'FROM_INT' ) THEN

                                    CALL MSG_OUT( ' ',
     :                                'Errors will be propagated '/
     :                                /'from those in the '/
     :                                /'integration files', STATUS )
                                 ELSE

                                    CALL MSG_OUT( ' ',
     :                                'New errors will be '/
     :                                /'estimated from the '/
     :                                /'dispersion of the '/
     :                                /'observations', STATUS )
                                 END IF

                                 IF ( VARIANCE_WT ) THEN

                                    CALL MSG_OUT( ' ',
     :                                'Observations '/
     :                                /'will be weighted by their '/
     :                                /'variance.', STATUS )
                                 END IF

                                 IF ( ABS(SKY_WT-1.0)
     :                                .GT. 0.0001 ) THEN

                                    CALL MSG_SETR( 'SKY_WT', SKY_WT )
                                    CALL MSG_OUT( ' ', 'Sky '/
     :                                 /'observations will be '/
     :                                 /'weighted by ^SKY_WT.',
     :                                STATUS )
                                 END IF
                              END IF

*                           Increment the appropriate counter in the
*                           reduced group file.
                              DSA_STATUS = STATUS
                              IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

                                 CALL DSA_GET_FITS_I( 'GRPRED',
     :                             'NOBJ', 0, NOBJ, COMMENT,
     :                             DSA_STATUS )

                                 NOBJ = NOBJ + 1

                                 CALL DSA_PUT_FITS_I( 'GRPRED',
     :                             'NOBJ', NOBJ, ' ',
     :                              DSA_STATUS )
                              ELSE IF ( OBSTYPE .EQ. 'SKY' ) THEN

                                 CALL DSA_GET_FITS_I( 'GRPRED',
     :                              'NSKY', 0, NSKY, COMMENT,
     :                              DSA_STATUS )

                                 NSKY = NSKY + 1

                                 CALL DSA_PUT_FITS_I( 'GRPRED',
     :                             'NSKY', NSKY, ' ', DSA_STATUS )
                              END IF

*                              We may now proceed with the actual processing
*                              of the data.
                              IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                                 STATUS = SAI__ERROR
                                 CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                             /'Error putting FITS items', STATUS )
                              END IF

                              CALL RED4_ADD_OBSERVATION_PAIR(
     :                          OBJECTOBS, SKYOBS, COADDED_OBS,
     :                          COADD_OBJ, COADD_SKY, ERRORS,
     :                          SKY_WT, VARIANCE_WT, STATUS )
                           ELSE

*                           Illegal pair.
*                           Decrement the counter in the reduced group
*                           file which was incremented by the first
*                           member of the pair.
                              DSA_STATUS = STATUS
                              IF ( LAST_OBSERVATION_TYPE .EQ.
     :                             'OBJECT' ) THEN

                                 CALL DSA_GET_FITS_I( 'GRPRED',
     :                             'NOBJ', 0, NOBJ, COMMENT,
     :                             DSA_STATUS )

                                 NOBJ = NOBJ - 1

                                 CALL DSA_PUT_FITS_I( 'GRPRED',
     :                             'NOBJ', NOBJ, ' ',
     :                             DSA_STATUS )
                              ELSE IF ( LAST_OBSERVATION_TYPE .EQ.
     :                                  'SKY' ) THEN

                                 CALL DSA_GET_FITS_I( 'GRPRED',
     :                              'NSKY', 0, NSKY, COMMENT,
     :                               DSA_STATUS )

                                 NSKY = NSKY - 1

                                 CALL DSA_PUT_FITS_I( 'GRPRED',
     :                             'NSKY', NSKY, ' ',
     :                             DSA_STATUS )
                              END IF

*                           Report an error message.
                              STATUS = SAI__ERROR
                              CALL MSG_SETC( 'OBSFILE', OBSFILE )
                              CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
                              CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                          /'Current observation ^OBSFILE is of '/
     :                          /'type ^OBSTYPE', STATUS )

                              CALL MSG_SETC( 'LAST_NAME',
     :                          LAST_OBSERVATION_NAME )
                              CALL MSG_SETC( 'LAST_TYPE',
     :                          LAST_OBSERVATION_TYPE )
                              CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                          /'Last observation '/
     :                          /'^LAST_NAME was of type ^LAST_TYPE',
     :                          STATUS )

                              CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                          /'There should '/
     :                          /'be one OBJECT and one SKY in '/
     :                          /'this pair', STATUS )

                           END IF
                       ELSE

*                        The current observation is merely the first in a
*                        new pair. Issue a message and remember its name
*                        and type.
                           CALL MSG_SETC( 'OBSFILE', OBSFILE )
                           CALL MSG_OUT( ' ', 'Starting a new pair '/
     :                       /'with observation ^OBSFILE.', STATUS )

                           LAST_OBSERVATION_NAME = OBSFILE
                           LAST_OBSERVATION_TYPE = OBSTYPE

*                        Increment the appropriate counter in the
*                        reduced group file.
                           DSA_STATUS = STATUS
                           IF ( OBSTYPE .EQ. 'OBJECT' ) THEN

                              CALL DSA_GET_FITS_I( 'GRPRED', 'NOBJ',
     :                          0, NOBJ, COMMENT, DSA_STATUS )

                              NOBJ = NOBJ + 1

                              CALL DSA_PUT_FITS_I( 'GRPRED', 'NOBJ',
     :                          NOBJ, ' ', DSA_STATUS )
                           ELSE IF ( OBSTYPE .EQ. 'SKY' ) THEN

                              CALL DSA_GET_FITS_I( 'GRPRED', 'NSKY',
     :                          0, NSKY, COMMENT, DSA_STATUS )

                              NSKY = NSKY + 1

                              CALL DSA_PUT_FITS_I( 'GRPRED', 'NSKY',
     :                          NSKY, ' ', DSA_STATUS )
                           END IF
                        END IF
                     ELSE

*                     The observation has already been applied. Issue a
*                     warning message and ignore the observation.
                     CALL MSG_OUT( ' ', '****** This observation '/
     :                 /'has already been added to the group - '/
     :                 /'ignored ******.', STATUS )
                     END IF
                  ELSE

                     CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :                 /'Error accessing reduced '/
     :                 /'group file', STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     ;              /'This observation has not '/
     :              /'been properly reduced - not added to group',
     :              STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'OBSFILE', OBSFILE )
               CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
               CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     ;           /'Observation ^OBSFILE is of type '/
     :           /'^OBSTYPE', STATUS )
               CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :           /'Only OBJECT and SKY '/
     :           /'observations may be co-added into groups', STATUS )
            END IF
         ELSE

            CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :        /'Error accessing reduced '/
     :        /'observation file', STATUS )
         END IF

*      Close DSA and tidy up (regardless of the status at this point).
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :       /'Error closing DSA', STATUS )
         ENDIF
      ELSE

         CALL ERR_REP( ' ', 'RED4_ADD_PAIR: '/
     :     /'Error obtaining %OBSFILE, %ERRORS, '/
     :     /'%SKY_WT and %VARIANCE_WT parameters', STATUS )
      END IF
      END
