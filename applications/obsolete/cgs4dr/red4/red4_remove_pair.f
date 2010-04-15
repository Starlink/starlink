*+  RED4_REMOVE_PAIR - Subtract a pair of observations from a reduced group file
      SUBROUTINE RED4_REMOVE_PAIR( STATUS )
*    Description :
*     This routine removes a pair of OBJECT and SKY reduced observations
*     belonging to a particular group from a reduced group file,
*     provided both observations have previously been co-added.
*     Observations may have been weighted according to their variance,
*     and SKY observations may have been given a weight of SKY_WT.
*     Observation types other than OBJECT and SKY are not allowed.
*
*     The COADDS structures in the reduced group file corresponding to
*     each of the given observations are deleted.
*
*     Note that it is not possible to reverse the changes to the FITS
*     header that were made when the observations was added. The exposure
*     time is removed from the EXPOSED and SKYEXP parameters, but the
*     previous values of RUTSTART, RUTEND, UTSTART, UTEND, AMSTART and
*     AMEND cannot be restored.
*    Invocation :
*     CALL RED4_REMOVE_PAIR( STATUS )
*    Parameters :
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
*     17-Feb-1991: Original version, based on RED4_REMOVE_OBSERVATION. (SMB)
*     23-Feb-1993: Conform to error strategy                           (PND)
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
*    Local Constants :
*    Local variables :
      CHARACTER*80
     :  OBJECTOBS,                 ! Name of OBJECT observation file.
     :  SKYOBS,                    ! Name of SKY observation file.
     :  OBJRED,                    ! Name of OBJECT reduced observation file.
     :  SKYRED,                    ! Name of SKY reduced observation file.
     :  GRPRED,                    ! Name of reduced group file.
     :  COADDS,                    ! Name of COADDS structure.
     :  COADDED_OBS                ! Name of COADDED_OBS structure.
      CHARACTER*20
     :  OBJTYPE,                   ! OBJECT observation type (should be OBJECT).
     :  SKYTYPE,                   ! SKY observation type ( should be SKY).
     :  COADD_OBJ,                 ! Name of coadd item for OBJECT observation.
     :  COADD_SKY,                 ! Name of coadd item for OBJECT observation.
     :  ERRPROP                    ! Value of ERRPROP FITS item.
      CHARACTER*4
     :  VARWT,                     ! Value of VARWT FITS item
     :  ACCESS,                    ! Access code for FITS item
     :  COMMENT                    ! Dummy comment.
      INTEGER
     :  ELEMENTS,                  ! Number of elements in FITS item
     :  STRLEN,                    ! Length of character FITS item
     :  OBJGRP,                    ! OBJECT group number.
     :  SKYGRP,                    ! SKY group number.
     :  CLEN                       ! Length of character string.
      LOGICAL
     :  VARIANCE_WT,               ! Indicates if variance weighting to be used.
     :  EXIST,                     ! Indicates if FITS item exists.
     :  OBJFOUND,                  ! Indicates if OBJECT structure found.
     :  SKYFOUND                   ! Indicates if SKY structure found.
      REAL
     :  SKY_WT                     ! Sky observation weighting factor
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Determine the names of the OBJECT and SKY observation files to be
*   removed.
      CALL PAR_GET0C( 'OBJECTOBS', OBJECTOBS, STATUS )
      CALL PAR_GET0C( 'SKYOBS', SKYOBS, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Convert the observation file names into the names of the reduced
*      observation files.
         CALL RED4_OBSTOROBS( OBJECTOBS, OBJRED, STATUS )
         CALL RED4_OBSTOROBS( SKYOBS, SKYRED, STATUS )

*      Open the reduced observation files for input.
         CALL RED4_CHECK_INPUT( OBJRED, STATUS )
         CALL DSA_NAMED_INPUT( 'OBJRED', OBJRED, STATUS )
         CALL RED4_CHECK_INPUT( SKYRED, STATUS )
         CALL DSA_NAMED_INPUT( 'SKYRED', SKYRED, STATUS )

*      Obtain the observation types from the FITS structures of the
*      reduced observation files.
         CALL DSA_GET_FITS_C( 'OBJRED', 'OBSTYPE', 0, OBJTYPE,
     :     COMMENT, STATUS )
         CALL DSA_GET_FITS_C( 'SKYRED', 'OBSTYPE', 0, SKYTYPE,
     :     COMMENT, STATUS )

*      Also, obtain the group numbers to which this observations belong.
         CALL DSA_GET_FITS_I( 'OBJRED', 'GRPNUM', 0, OBJGRP,
     :     COMMENT, STATUS )
         CALL DSA_GET_FITS_I( 'SKYRED', 'GRPNUM', 0, SKYGRP,
     :     COMMENT, STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Ensure that the OBJECT observation is of type 'OBJECT', and
*         the SKY observation is of type 'SKY'.
            IF ( (OBJTYPE .EQ. 'OBJECT') .AND.
     :           (SKYTYPE .EQ. 'SKY') ) THEN

*            Ensure that both observations belong to the same group.
               IF ( OBJGRP .EQ. SKYGRP ) THEN

*               Convert the OBJECT observation file name, together with the
*               group number obtained above, into the name of the
*               reduced group file.
                  CALL RED4_ROBSTOGRP( OBJRED, OBJGRP, GRPRED, STATUS )

*               Issue a message.
                  CALL MSG_SETC( 'OBJECTOBS', OBJECTOBS )
                  CALL MSG_SETC( 'SKYOBS', SKYOBS )
                  CALL MSG_OUT( ' ', 'Subtracting ^SKYOBS from '/
     :              /'^OBJECTOBS to make a pair', STATUS )
                  CALL MSG_SETC( 'GRPRED', GRPRED )
                  CALL MSG_OUT( ' ', 'Then removing pair from '/
     :              /'^GRPRED', STATUS )

*               Open the reduced group file.
                  CALL RED4_CHECK_INPUT( GRPRED, STATUS )
                  CALL DSA_NAMED_INPUT( 'GRPRED', GRPRED, STATUS )

*               Determine if the group was built with variance weighting
*               and determine the sky weighting factor used, if any.
                  CALL DSA_GET_FITS_C( 'GRPRED', 'VARWT', 0, VARWT,
     :              COMMENT, STATUS )
                  CALL CHR_UCASE( VARWT )

                  IF ( VARWT .EQ. 'YES' ) THEN

                     VARIANCE_WT = .TRUE.
                  ELSE

                     VARIANCE_WT = .FALSE.
                  END IF

                  CALL DSA_SEEK_FITS( 'GRPRED', 'SKYWT', EXIST,
     :              ACCESS, ELEMENTS, STRLEN, STATUS )

                  IF ( EXIST ) THEN

                     CALL DSA_GET_FITS_F( 'GRPRED', 'SKYWT', 0,
     :                 SKY_WT, COMMENT, STATUS )
                  ELSE

                     SKY_WT = 1.0
                  END IF

*               Determine the error propagation method used to
*               construct the group.
                  CALL DSA_GET_FITS_C( 'GRPRED', 'ERRPROP', 0,
     :              ERRPROP, COMMENT, STATUS )

*               Open the COADDS structure within the reduced group file
                  CLEN = MAX( 1, CHR_LEN( GRPRED ) )
                  COADDS = GRPRED(1:CLEN) // '.MORE.CGS4_COADDS'

                  CALL DSA_NAMED_INPUT( 'COADDS', COADDS, STATUS )

*               Obtain the DTA address of the COADDED_OBS structure
*               within the COADDS structure.
                  CALL DSA_SPECIFIC_STRUCTURE( 'COADDS',
     :              'COADDED_OBS', 'UPDATE', COADDED_OBS, STATUS )

*               Convert the observation names into their corresponding
*               coadd structure names.
                  CALL RED4_OBSTOCOADD( OBJECTOBS, COADD_OBJ, STATUS )
                  CALL RED4_OBSTOCOADD( SKYOBS, COADD_SKY, STATUS )

*               Check everything has worked so far.
                  IF ( STATUS .EQ. ADAM__OK ) THEN

*                  Check to see if both coadd structures exists. If
*                  either does not, then this observation pair cannot
*                  have been previously applied and therefore cannot be
*                  removed.
                     CALL RED4_SEEK_ITEM( COADDED_OBS, COADD_OBJ,
     :                  OBJFOUND, STATUS )
                     CALL RED4_SEEK_ITEM( COADDED_OBS, COADD_SKY,
     :                  SKYFOUND, STATUS )

                     IF ( OBJFOUND .AND. SKYFOUND ) THEN

*                     The observation pair has been applied.
*                     We may now proceed with the actual processing
*                     of the data.
                        CALL RED4_REMOVE_OBSERVATION_PAIR( OBJECTOBS,
     :                    SKYOBS, COADDED_OBS, COADD_OBJ, COADD_SKY,
     :                    ERRPROP, VARIANCE_WT, SKY_WT,STATUS )
                     ELSE

*                     One or both of the observations have not been applied.
*                     Issue a warning message and ignore this pair.
                        IF ( SKYFOUND ) THEN

                           CALL MSG_OUT( ' ', '****** The OBJECT '/
     :                       /'observation has not been applied - '/
     :                       /'pair not removed ******', STATUS )
                        ELSE IF ( OBJFOUND ) THEN

                           CALL MSG_OUT( ' ', '****** The SKY '/
     :                       /'observation has not been applied - '/
     :                       /'pair not removed ******', STATUS )
                        ELSE

                           CALL MSG_OUT( ' ', '****** Both '/
     :                       /'observations have not been applied '/
     :                       /'- pair not removed ******', STATUS )
                        END IF
                     END IF
                  ELSE

                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :                 /'Error accessing reduced group file', STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'OBJECTOBS', OBJECTOBS )
                  CALL MSG_SETI( 'OBJGRP', OBJGRP )
                  CALL MSG_SETC( 'SKYOBS', SKYOBS )
                  CALL MSG_SETI( 'SKYGRP', SKYGRP )
                  CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :              /'Observations ^OBJECTOBS and '/
     :              /'^SKYOBS belong to different groups (^OBJGRP '/
     :              /'and ^SKYGRP)', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR

               IF ( OBJTYPE .NE. 'OBJECT' ) THEN

                  CALL MSG_SETC( 'OBJECTOBS', OBJECTOBS )
                  CALL MSG_SETC( 'OBJTYPE', OBJTYPE )
                  CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :              /'Observation ^OBJECTOBS is '/
     :              /'of type ^OBJTYPE and not OBJECT', STATUS )
               END IF

               IF ( SKYTYPE .NE. 'SKY' ) THEN

                  CALL MSG_SETC( 'SKYOBS', SKYOBS )
                  CALL MSG_SETC( 'SKYTYPE', SKYTYPE )
                  CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :              /'Observation ^SKYOBS is '/
     :              /'of type ^SKYTYPE and not SKY', STATUS )
               END IF
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :        /'Error accessing reduced '/
     :        /'observation file', STATUS )
         END IF

*      Close DSA and tidy up (regardless of the status at this point).
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_PAIR: '/
     :     /'Error obtaining %OBJECTOBS and '/
     :     /'%SKYOBS parameters', STATUS )
      END IF

      END
