*+  RED4_FILE_CALIBRATION - File observation in index file as CALIBRATION
      SUBROUTINE RED4_FILE_CALIBRATION( STATUS )
*    Description :
*     This routine converts the given observation into a CALIBRATION
*     and writes its parameters to the index file. It is assumed that
*     the observation has been processed with the Figaro ARC function
*     and contains a valid wavelength scale in its X axis.
*    Invocation :
*     CALL RED4_FILE_CALIBRATION( STATUS )
*    Parameters :
*     STATUS         = INTEGER( UPDATE )
*           Global status.
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme. It has
*     therefore been necessary to trap these statuses before they
*     get translated into confusing messages like "exceeded quota".
*     The traps can be removed once DSA conforms.
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
*     21-Nov-1990: Original version.                            (SMB)
*     22-Nov-1990: A check on the X axis units added.           (SMB)
*     23-Nov-1990: DSA reference name changed (see under "Bugs:"
*                  above). Made to open reduced observation file
*                  as well as observation file.                 (SMB)
*     13-Dec-1990: Modified to convert the named observation
*                  into the name of the corresponding
*                  CALIBRATION and file that.                   (SMB)
*     18-Dec-1990: Typing mistake fixed.                        (SMB)
*     25-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.        (SMB)
*     22-Aug-1991: Modified to fix bug reported by Andy Adamson.
*                  Pass the name of the reduced observation file
*                  to RED4_FILE_OBSERVATION_2.                  (SMB)
*     18-Feb-1993: Conform to error strategy                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'    ! RED4 common block
*    Status :
      INTEGER STATUS               ! Global status
*    External references:
      INTEGER CHR_LEN              ! Character length determining function
*    Local Constants :
      INTEGER NINFO                ! Number of axis information items
      PARAMETER ( NINFO = 2 )
*    Local variables :
      CHARACTER*80
     :  OBSERVATION,               ! Name of observation file.
     :  CALIB                      ! Name of CALIBRATION file.
      CHARACTER*32
     :  CHAR_ARRAY( NINFO )        ! Array to hold X axis info
      CHARACTER*20
     :  OLDTYPE                    ! The old observation type
      CHARACTER*4
     :  COMMENT                    ! Dummy comment
      DOUBLE PRECISION
     :  DIGNORE                    ! Ignored parameter
      LOGICAL
     :  CHANGE_LABEL               ! TRUE if X axis labels are to be changed.
      INTEGER
     :  DSA_STATUS,                ! DSA status value
     :  CLEN                       ! Non-blank length of character string
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the observation to be filed as a CALIBRATION
      CALL PAR_GET0C( 'OBSERVATION', OBSERVATION, STATUS )
      CALL RED4_CHECK_INPUT( OBSERVATION, STATUS )

*   Convert the observation to the name of the CALIBRATION file.
      CALL RED4_OBSTOCAL( OBSERVATION, CALIB, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open the DSA system
         DSA_STATUS = STATUS
         CALL DSA_OPEN( DSA_STATUS )

*      Open the observation file and the CALIBRATION file.
         CALL DSA_NAMED_INPUT( 'OBSFILE', OBSERVATION, DSA_STATUS )
         CALL DSA_NAMED_INPUT( 'CALIB', CALIB, DSA_STATUS )

*      Obtain the current type of the observation from the FITS structure.
         CALL DSA_GET_FITS_C( 'CALIB', 'OBSTYPE', 0,
     :     OLDTYPE, COMMENT, DSA_STATUS )
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :        /'Error getting observation type', STATUS )
         END IF

*      The observation type should be 'ARC', indicating a new
*      observation which has been processed with the Figaro ARC
*      function, or 'CALIB', indicating an old CALIBRATION
*      which is being re-filed. It is possible to define other
*      observations as wavelength calibrations, but this is unusual.
*      Issue a warning for these.
         IF ( (INDEX(OLDTYPE,'ARC').EQ.0)   .AND.
     :        (INDEX(OLDTYPE,'CALIB').EQ.0) ) THEN

            CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
            CALL MSG_SETC( 'OLDTYPE', OLDTYPE )
            CALL MSG_OUT( ' ', 'NOTE - Observation ^OBSERVATION '/
     :        /'is of type ^OLDTYPE and not ARC.', STATUS )
         END IF

*      Obtain the X axis units and label for this observation.
*      The units will be returned in CHAR_ARRAY(1) and the label
*      in CHAR_ARRAY(2).
         DSA_STATUS = STATUS
         CALL DSA_GET_AXIS_INFO( 'CALIB', 1, NINFO, CHAR_ARRAY,
     :     0, DIGNORE, DSA_STATUS )
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :        /'Error getting axis info', STATUS )
         END IF

         CALL MSG_SETC( 'XLABEL', CHAR_ARRAY(2) )
         CALL MSG_SETC( 'XUNITS', CHAR_ARRAY(1) )
         CALL MSG_OUT( ' ', 'The X axis is labelled "^XLABEL" '/
     :     /'and has units "^XUNITS".', STATUS )

         CALL PAR_GET0L( 'CHANGE_LABEL', CHANGE_LABEL, STATUS )
         IF ( CHANGE_LABEL ) THEN

            CALL PAR_GET0C( 'NEWLABEL', CHAR_ARRAY(2), STATUS )
            CALL PAR_GET0C( 'NEWUNITS', CHAR_ARRAY(1), STATUS )

            DSA_STATUS = STATUS
            CALL DSA_SET_AXIS_INFO( 'CALIB', 1, NINFO,
     :        CHAR_ARRAY, 0, DIGNORE, DSA_STATUS )
            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :           /'Error setting axis label', STATUS )
            END IF
         END IF

*      Save the old observation type, so it can be restored if necessary
*      (unless it happens to be 'CALIBRATION' - indicating that it has
*      already been converted), and write a new type of 'CALIBRATION'.
*      Write these to both the observation and reduced observation files.
*      Then report what happened.
         DSA_STATUS = STATUS
         IF ( (INDEX(OLDTYPE,'CALIB').EQ.0) ) THEN

            CLEN = MAX( 1, CHR_LEN( OLDTYPE ) )
            CALL DSA_PUT_FITS_C( 'CALIB', 'OLDTYPE',
     :        OLDTYPE(1:CLEN), ' ', DSA_STATUS )
            CALL DSA_PUT_FITS_C( 'OBSFILE', 'OLDTYPE',
     :        OLDTYPE(1:CLEN), ' ', DSA_STATUS )
            CALL DSA_PUT_FITS_C( 'CALIB', 'OBSTYPE',
     :        'CALIBRATION', ' ', DSA_STATUS )
            CALL DSA_PUT_FITS_C( 'OBSFILE', 'OBSTYPE',
     :        'CALIBRATION', ' ', DSA_STATUS )

            IF ( DSA_STATUS .NE. ADAM__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :           /'Error setting FITS item', STATUS )
            END IF

            CALL MSG_SETC( 'OLDTYPE', OLDTYPE )
            CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
            CALL MSG_OUT( ' ', '^OLDTYPE observation ^OBSERVATION '/
     :        /'redefined as a CALIBRATION.', STATUS )
         END IF

*      Close the DSA system.
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )

*      If all the above has worked, call the RED4 routine which
*      will file the observation in the index file.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :        /'Error closing DSA', STATUS )
         END IF

         CALL RED4_FILE_OBSERVATION_2( OBSERVATION, CALIB,
     :      'WHATEVER_IT_IS', STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FILE_CALIBRATION: '/
     :     /'Error obtaining %OBSERVATION parameter', STATUS )
      END IF

      END
