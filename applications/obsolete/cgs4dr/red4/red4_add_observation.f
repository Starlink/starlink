*+  RED4_ADD_OBSERVATION - Add an observation to a reduced group file
      SUBROUTINE RED4_ADD_OBSERVATION( STATUS )
*    Description :
*     This routine applies a reduced observation belonging to a particular
*     group to a reduced group file. If the reduced group file does not
*     exist, a new one is created and initialised. Reduced observations
*     of type OBJECT are added to the contents of the reduced group
*     file. Reduced observations of type SKY are subtracted from the
*     contents of the reduced group file, after being optionally
*     multiplied by a weighting factor indicated by the SKY_WT parameter.
*     Observations may also be weighted according to their variance
*     if observing conditions dictate.
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
*     CALL RED4_ADD_OBSERVATION( STATUS )
*    Parameters :
*     STATUS  = INTEGER( UPDATE )
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
*     There is a bug in DSA_MATCH_UNITS in which DSA claims the data units
*     in the reduced observation file and reduced group file are different
*     even when they are identical ! A private version of this routine
*     is needed until the bug is released.
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     19-Sep-1990: Original version.                               (SMB)
*     21-Sep-1990: Data units test commented out. It doesn't work! (SMB)
*     28-Sep-1990: Description corrected.                          (SMB)
*     23-Oct-1990: Cosmetic change.                                (SMB)
*      6-Nov-1990: VARIANCE_WT and SKY_WT parameters obtained.     (SMB)
*     31-Jan-1991: Major deficiencies discovered in sky-subtraction
*                  (see under "deficiencies"). Warning issued for
*                  SKY observations.                               (SMB)
*     15-Feb-1991: Modified to reject observations which have not
*                  been properly reduced.                          (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.           (SMB)
*     29-Aug-1991: Add polyfit enhanced sky-subtraction            (PND)
*      9-Jul-1992: Comment out some VERBOSE debugging code         (PND)
*     18-Feb-1993: Conform to error strategy                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'           ! Contains ADAM__OK
*     INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR, SAI__OK etc
      INCLUDE 'SAE_PAR'            ! Contains SAI__ERROR, SAI__OK etc
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
      CHARACTER*20
     :  OBSTYPE,                   ! Observation type.
     :  COADD_NAME                 ! Name of coadd item for this observation.
      CHARACTER*4
     :  COMMENT                    ! Dummy comment.
      INTEGER
     :  GRPNUM,                    ! Group number.
     :  CLEN,                      ! Length of character string.
     :  DSA_STATUS                 ! DSA status value
      LOGICAL
     :  VARIANCE_WT,               ! Indicates if variance weighting to be used.
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

*   Obtain the variance weighting flag.
      CALL PAR_GET0L( 'VARIANCE_WT', VARIANCE_WT, STATUS )

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

*   Check the parameters have been obtained successfully.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         DSA_STATUS = STATUS
         CALL DSA_OPEN( DSA_STATUS )

*      Convert the observation file name into a reduced observation file.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :         /'Error opening DSA', STATUS )
         END IF
         CALL RED4_OBSTOROBS( OBSFILE, OBSRED, STATUS )

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

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :         /'Error getting FITS items', STATUS )
         END IF

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Ensure that the observation is of type 'OBJECT' or 'SKY'.
            IF ( (OBSTYPE .EQ. 'OBJECT') .OR.
     :           (OBSTYPE .EQ. 'SKY') ) THEN

*            Ensure the observation has been reduced successfully.
               IF ( ( STREDUCE .NE. ' ' ) .AND.
     :              ( STREDUCE .NE. '(not properly reduced)' ) ) THEN

*               Convert the observation file name, together with the
*               group number obtained above, into the name of the
*               reduced group file.
                  CALL RED4_ROBSTOGRP( OBSRED, GRPNUM, GRPRED, STATUS )

*               Check is this reduced group file exists.
                  DSA_STATUS = STATUS
                  CALL DSA_SEEK_NAMED_STRUCTURE( GRPRED, EXIST,
     :               DSA_STATUS )

*               If this has worked, and the structure does not exist,
*               then create it. (Note that for convenience DSA is opened
*               separately inside RED4_MAKE_GRPREDFILE and therefore has
*               to be closed and reopened. This also means the reduced
*               observation file has to be reopened).
                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :                 /'Error seeking CGS4 specific structure',
     :                 STATUS )
                  END IF

                  IF ( ( STATUS .EQ. ADAM__OK ) .AND.
     :                 ( .NOT. EXIST ) ) THEN

                     CALL DSA_CLOSE( DSA_STATUS )
                     IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :                     /'First error closing DSA', STATUS )
                     END IF

                     CALL RED4_MAKE_GRPREDFILE( OBSRED, GRPRED, STATUS )

                     DSA_STATUS = STATUS
                     CALL DSA_OPEN( DSA_STATUS )
                     CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED,
     :                 DSA_STATUS )
                  END IF

                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :                  /'Error with DSA routine', STATUS )
                  END IF

*               Issue a message.
                  IF ( OBSTYPE .EQ. 'SKY' ) THEN

                     CALL MSG_SETC( 'OBSFILE', OBSFILE )
                     CALL MSG_SETC( 'GRPRED', GRPRED )
                     CALL MSG_OUT( ' ', 'Subtracting SKY '/
     :                 /'observation ^OBSFILE from ^GRPRED.', STATUS )

                     CALL MSG_OUT( ' ', 'WARNING - SKY '/
     :                 /'observations may be not subtracted '/
     :                 /'properly unless ', STATUS )
                     CALL MSG_OUT( ' ', '          combined into '/
     :                 /'OBJECT/SKY pairs.', STATUS )
                  ELSE

                     CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
                     CALL MSG_SETC( 'OBSFILE', OBSFILE )
                     CALL MSG_SETC( 'GRPRED', GRPRED )
                     CALL MSG_OUT( ' ', 'Adding ^OBSTYPE '/
     :                 /'observation ^OBSFILE to ^GRPRED.', STATUS )
                  END IF

*               In verbose mode, give some more info.
                  IF ( VERBOSE ) THEN

                     CALL MSG_OUT( ' ',
     :                 'Errors will be propagated from those in '/
     :                 /'the observation file', STATUS )

                     IF ( VARIANCE_WT ) THEN

                        CALL MSG_OUT( ' ', 'Observations will be '/
     :                    /'weighted by their variance', STATUS )
                     END IF

                     IF ( ( OBSTYPE .EQ. 'SKY' ) .AND.
     :                    ( ABS(SKY_WT-1.0) .GT. 0.0001 ) ) THEN

                        CALL MSG_SETR( 'SKY_WT', SKY_WT )
                        CALL MSG_OUT( ' ', 'This '/
     :                    /'observation will be weighted '/
     :                    /'by ^SKY_WT.', STATUS )
                     END IF
                  END IF

*               Open the reduced group file.
                  DSA_STATUS = STATUS
                  CALL RED4_CHECK_INPUT( GRPRED, STATUS )
                  CALL DSA_NAMED_INPUT( 'GRPRED', GRPRED, DSA_STATUS )

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
     :              DSA_STATUS )

*               Open the COADDS structure within the reduced group file
                  CLEN = MAX( 1, CHR_LEN( GRPRED ) )
                  COADDS = GRPRED(1:CLEN) // '.MORE.CGS4_COADDS'

                  CALL DSA_NAMED_INPUT( 'COADDS', COADDS, DSA_STATUS )

*               Obtain the DTA address of the COADDED_OBS structure
*               within the COADDS structure.
                  CALL DSA_SPECIFIC_STRUCTURE( 'COADDS',
     :              'COADDED_OBS', 'UPDATE', COADDED_OBS, DSA_STATUS )

*               Convert the observation name into its corresponding
*               coadd structure name.
                  IF ( DSA_STATUS .NE. ADAM__OK ) THEN

                     CALL ERR_ANNUL( STATUS )
                  END IF

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
*                     We may now proceed with the actual processing
*                     of the data.
                        CALL RED4_ADD_OBSERVATION_2( OBSTYPE,
     :                    COADDED_OBS, COADD_NAME, VARIANCE_WT,
     :                    SKY_WT, STATUS )

                     ELSE

*                     The observation has already been applied. Issue a
*                     warning message and ignore the observation.
                        CALL MSG_OUT( ' ', '****** This '/
     :                    /'observation has already been added to '/
     :                    /'the group - ignored ******.', STATUS )
                     END IF
                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :                 /'Error accessing reduced group file', STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :              /'This observation has not '/
     :              /'been properly reduced - not added to group',
     :              STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'OBSFILE', OBSFILE )
               CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
               CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :           /'Observation ^OBSFILE is of type '/
     :           /'^OBSTYPE', STATUS )
               CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :           /'Only OBJECT and SKY '/
     :           /'observations may be co-added into a group',
     :           STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :        /'Error accessing reduced '/
     :        /'observation file', STATUS )
         END IF

*      Close DSA and tidy up (regardless of the status at this point).
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :        /'Second error closing DSA', STATUS )
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_ADD_OBSERVATION: '/
     :     /'Error obtaining %OBSFILE, '/
     :     /'%VARIANCE_WT and %SKY_WT parameters', STATUS )
      END IF

      END
