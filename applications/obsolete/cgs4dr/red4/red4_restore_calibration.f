*+  RED4_RESTORE_CALIBRATION - Restore a CALIBRATION observation back as it was.
      SUBROUTINE RED4_RESTORE_CALIBRATION (STATUS)
*    Description :
*     This routine reverses the effect of RED4_FILE_CALIBRATION.
*     It restores the old observation type in a CALIBRATION observation
*     and re-files it.
*    Invocation :
*     CALL RED4_RESTORE_CALIBRATION (STATUS)
*    Parameters :
*     STATUS         = INTEGER( UPDATE )
*           Global status.
*    Method :
*    Deficiencies :
*    Bugs :
*     Note that there is a bug or "feature" of the interaction
*     between DSA and the ADAM parameter system which means that
*     the reference name given to DSA_NAMED_INPUT must be different
*     from the parameter name given to PAR_GET0C.
*     Bug reported to Starlink on 23-Nov-1990.
*    Authors :
*     S.M.Beard  (REVAD::SMB)
*     P.N.Daly   (JACH::PND)
*    History :
*     22-Nov-1990: Original version.                            (SMB)
*     23-Nov-1990: DSA reference name changed (see under "Bugs:"
*                  above). Made to open reduced observation file
*                  as well as observation file.                 (SMB)
*     18-Dec-1990: Reduced observation file no longer needed,
*                  as a calibration file is now used instead.   (SMB)
*     24-Feb-1991: Some error reporting mistakes fixed,
*                  which would have made this routine
*                  fail under ADAM V1.9.                        (SMB)
*     22-Aug-1991: Typing mistake fixed.                        (SMB)
*     23-Feb-1993: Conform to error strategy                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'            ! Contains SAI__ERROR
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Status :
      INTEGER STATUS               ! Global status
*    External references:
      INTEGER CHR_LEN              ! Character length determining function
*    Local Constants :
*    Local variables :
      CHARACTER*80
     :  OBSERVATION,               ! Name of observation file
     :  REDOBS                     ! Name of reduced observation file
      CHARACTER*20
     :  OBSTYPE,                   ! The current observation type
     :  OLDTYPE                    ! The old observation type
      CHARACTER*4
     :  COMMENT                    ! Dummy comment
      INTEGER
     :  CLEN                       ! Non-blank length of character string
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the CALIBRATION observation to be restored
      CALL PAR_GET0C( 'OBSERVATION', OBSERVATION, STATUS )
      CALL RED4_CHECK_INPUT( OBSERVATION, STATUS )

*   Construct a reduced observation filename
      CALL RED4_OBSTOROBS( OBSERVATION, REDOBS, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open the DSA system
         CALL DSA_OPEN (STATUS)

*      Open the observation file, assuming it is in the ODIR: directory.
         CALL DSA_NAMED_INPUT( 'OBSFILE', OBSERVATION, STATUS)

*      Obtain the current type of the observation from the FITS structure.
*      and check this really is 'CALIBRATION'.
         CALL DSA_GET_FITS_C( 'OBSFILE', 'OBSTYPE', 0,
     :     OBSTYPE, COMMENT, STATUS )

         IF ( OBSTYPE .EQ. 'CALIBRATION' ) THEN

*         Obtain the old observation type, which was saved to the
*         OLDTYPE parameter, and write this to OBSTYPE in the
*         observation file.
            CALL DSA_GET_FITS_C( 'OBSFILE', 'OLDTYPE', 0,
     :        OLDTYPE, COMMENT, STATUS )
            CLEN = MAX( 1, CHR_LEN( OLDTYPE ) )
            CALL DSA_PUT_FITS_C( 'OBSFILE', 'OBSTYPE',
     :        OLDTYPE(1:CLEN), ' ', STATUS )
            CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
            CALL MSG_SETC( 'OLDTYPE', OLDTYPE )

            IF ( ( OLDTYPE(1:1) .EQ. 'O' ) .OR.
     :           ( OLDTYPE(1:1) .EQ. 'A' ) ) THEN

               CALL MSG_OUT( ' ', 'CALIBRATION observation '/
     :           /'^OBSERVATION will be re-filed as an ^OLDTYPE',
     :           STATUS )
            ELSE

               CALL MSG_OUT( ' ', 'CALIBRATION observation '/
     :           /'^OBSERVATION will be re-filed as a ^OLDTYPE',
     :           STATUS )
            END IF

*          Put it into the parameter system
            CALL PAR_PUT0C( 'TYPE', OLDTYPE, STATUS )
         ELSE

*         The observation is not a CALIBRATION
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
            CALL ERR_REP( ' ', 'RED4_RESTORE_CALIBRATION: '/
     :        /'Observation ^OBSERVATION is not '/
     :        /'a CALIBRATION', STATUS )
         END IF

*      Close the DSA system.
         CALL DSA_CLOSE( STATUS )

*      If all the above has worked, call the RED4 routine which
*      will file the observation in the index file.
*      (This routine will look at the same OBSERVATION parameter
*      and so pick up the same file).
         CALL RED4_FILE_OBSERVATION_2( OBSERVATION,
     :     REDOBS, OLDTYPE, STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_RESTORE_CALIBRATION: '/
     :     /'Error obtaining %OBSERVATION parameter', STATUS )
      END IF

      END
