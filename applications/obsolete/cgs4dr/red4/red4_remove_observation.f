*+  RED4_REMOVE_OBSERVATION - Subtract an observation from a reduced group file
      SUBROUTINE RED4_REMOVE_OBSERVATION( STATUS )
*    Description :
*     This routine removes a reduced observation belonging to a particular
*     group from a reduced group file, provided that observation has
*     previously been co-added. It is assumed that reduced observations
*     of type OBJECT were added to the contents of the reduced group
*     file, and reduced observations of type SKY were subtracted from the
*     contents of the reduced group file, after being optionally
*     multiplied by a weighting factor indicated by the SKY_WT parameter.
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
*     CALL RED4_REMOVE_OBSERVATION( STATUS )
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
*     31-Oct-1990: Original version, based on RED4_ADD_OBSERVATION.(SMB)
*      1-Nov-1990: VARIANCE_WT and SKY_WT parameters obtained.     (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                           (SMB)
*     23-Feb-1993: Conform to error strategy                       (PND)
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
     :  OBSFILE,                   ! Name of observation file.
     :  OBSRED,                    ! Name of reduced observation file.
     :  GRPRED,                    ! Name of reduced group file.
     :  COADDS,                    ! Name of COADDS structure.
     :  COADDED_OBS                ! Name of COADDED_OBS structure.
      CHARACTER*20
     :  OBSTYPE,                   ! Observation type.
     :  COADD_NAME                 ! Name of coadd item for this observation.
      CHARACTER*4
     :  VARWT,                     ! Value of VARWT FITS item
     :  ACCESS,                    ! Access code for FITS item
     :  COMMENT                    ! Dummy comment.
      INTEGER
     :  ELEMENTS,                  ! Number of elements in FITS item
     :  STRLEN,                    ! Length of character FITS item
     :  GRPNUM,                    ! Group number.
     :  CLEN                       ! Length of character string.
      LOGICAL
     :  VARIANCE_WT,               ! Indicates if variance weighting to be used.
     :  EXIST,                     ! Indicates if FITS item exists.
     :  FOUND                      ! Indicates if structure found.
      REAL
     :  SKY_WT                     ! Sky observation weighting factor
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Determine the name of the observation file to be removed, and
*   convert this to upper case.
      CALL PAR_GET0C( 'OBSFILE', OBSFILE, STATUS )
      CALL CHR_UCASE( OBSFILE )
      CALL RED4_CHECK_INPUT( OBSFILE, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Convert the observation file name into the name of the reduced
*      observation file.
         CALL RED4_OBSTOROBS( OBSFILE, OBSRED, STATUS )

*      Open the reduced observation file for input.
         CALL RED4_CHECK_INPUT( OBSRED, STATUS )
         CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED, STATUS )

*      Obtain the observation type from the FITS structure of the
*      reduced observation file.
         CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBSTYPE,
     :     COMMENT, STATUS )

*      Also, obtain the group number to which this observation belongs.
         CALL DSA_GET_FITS_I( 'OBSRED', 'GRPNUM', 0, GRPNUM,
     :     COMMENT, STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Ensure that the observation is of type 'OBJECT' or 'SKY'.
            IF ( (OBSTYPE .EQ. 'OBJECT') .OR.
     :           (OBSTYPE .EQ. 'SKY') ) THEN

*            Convert the observation file name, together with the
*            group number obtained above, into the name of the
*            reduced group file.
               CALL RED4_ROBSTOGRP( OBSRED, GRPNUM, GRPRED, STATUS )

*            Issue a message.
               CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
               CALL MSG_SETC( 'OBSFILE', OBSFILE )
               CALL MSG_SETC( 'GRPRED', GRPRED )
               CALL MSG_OUT( ' ', 'Removing ^OBSTYPE observation '/
     :           /'^OBSFILE from ^GRPRED', STATUS )

*            Open the reduced group file.
               CALL RED4_CHECK_INPUT( GRPRED, STATUS )
               CALL DSA_NAMED_INPUT( 'GRPRED', GRPRED, STATUS )

*            Determine if the group was built with variance weighting
*            and determine the sky weighting factor used, if any.
               CALL DSA_GET_FITS_C( 'GRPRED', 'VARWT', 0, VARWT,
     :           COMMENT, STATUS )
               CALL CHR_UCASE( VARWT )

               IF ( VARWT .EQ. 'YES' ) THEN

                  VARIANCE_WT = .TRUE.
               ELSE

                  VARIANCE_WT = .FALSE.
               END IF

               CALL DSA_SEEK_FITS( 'GRPRED', 'SKYWT', EXIST, ACCESS,
     :           ELEMENTS, STRLEN, STATUS )

               IF ( EXIST ) THEN

                  CALL DSA_GET_FITS_F( 'GRPRED', 'SKYWT', 0, SKY_WT,
     :              COMMENT, STATUS )
               ELSE

                  SKY_WT = 1.0
               END IF

*            Open the COADDS structure within the reduced group file
               CLEN = MAX( 1, CHR_LEN( GRPRED ) )
               COADDS = GRPRED(1:CLEN) // '.MORE.CGS4_COADDS'

               CALL DSA_NAMED_INPUT( 'COADDS', COADDS, STATUS )

*            Obtain the DTA address of the COADDED_OBS structure
*            within the COADDS structure.
               CALL DSA_SPECIFIC_STRUCTURE( 'COADDS', 'COADDED_OBS',
     :            'UPDATE', COADDED_OBS, STATUS )

*            Convert the observation name into its corresponding
*            coadd structure name.
               CALL RED4_OBSTOCOADD( OBSFILE, COADD_NAME, STATUS )

*            Check everything has worked so far.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               Check to see if this coadd structure exists. If it does
*               not, then observation has not been previously applied
*               and therefore cannot be removed.
                  CALL RED4_SEEK_ITEM( COADDED_OBS, COADD_NAME,
     :               FOUND, STATUS )

                  IF ( FOUND ) THEN

*                  The observation has been applied.
*                  We may now proceed with the actual processing
*                  of the data.
                     CALL RED4_REMOVE_OBSERVATION_2( OBSTYPE,
     :                 COADDED_OBS, COADD_NAME, VARIANCE_WT, SKY_WT,
     :                 STATUS )
                  ELSE

*                  The observation has not been applied. Issue a
*                  warning message and ignore the observation.
                     CALL MSG_OUT( ' ', '****** This observation '/
     :                 /'has not been applied - ignored ******',
     :                 STATUS )
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION: '/
     :              /'Error accessing reduced '/
     :              /'group file', STATUS )
               END IF
            ELSE

               STATUS = SAI__ERROR
               CALL MSG_SETC( 'OBSFILE', OBSFILE )
               CALL MSG_SETC( 'OBSTYPE', OBSTYPE )
               CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION: '/
     :           /'Observation ^OBSFILE is of type '/
     :           /'^OBSTYPE', STATUS )
               CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION: '/
     :           /'Only OBJECT and SKY '/
     :           /'observations may be co-added into a group',
     :           STATUS )
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION: '/
     :        /'Error accessing reduced '/
     :        /'observation file', STATUS )
         END IF

*      Close DSA and tidy up (regardless of the status at this point).
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_REMOVE_OBSERVATION: '/
     :     /'Error obtaining %OBSFILE parameter', STATUS )
      END IF

      END
