*+  RED4_FILE_OBSERVATION_2 - File a specified observation in the index file
      SUBROUTINE RED4_FILE_OBSERVATION_2( OBSERVATION, REDOBS,
     :   TYPE, STATUS )
*    Description :
*     This routine files the given observation in the index file,
*     together with the parameters defining the setup of the instrument
*     during that observation. The index file is used to record the
*     observations that have been reduced, and is searched from time to
*     time for suitable calibration observations.
*    Invocation :
*     CALL RED4_FILE_OBSERVATION_2( OBSERVATION, REDOBS, STATUS )
*    Parameters :
*     OBSERVATION       = CHARACTER*(*)( READ )
*           The name of the observation file to be filed.
*     REDOBS            = CHARACTER*(*)( READ )
*           The name of the reduced observation file to be used
*           to obtain the observation parameters.
*     TYPE              = CHARACTER*(*)( READ )
*           The type to file the observation as
*     STATUS            = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     This routine needs restructuring without GOTOs.
*     There is insufficient error checking.
*
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
*     J. Lightfoot (REVAD::JFL)
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     1989:        Original version.                                (JFL)
*      2-Mar-1990: History added. SLIT_NUMBER changed to SLIT_NAME. (SMB)
*      2-Mar-1990: CVF_STATE changed to CVF_NAME.                   (SMB)
*      5-Jul-1990: OBSREC structure definition moved to
*                  RED4_COMMON.INC include file.                    (SMB)
*     11-Jul-1990: Code spaced out to make it readable.             (SMB)
*     12-Jul-1990: STR$UPCASE calls removed, except for the name of
*                  the observation. Character strings are now
*                  written to the index file unchanged (so that
*                  "list_index" gives more faithful output. Strings
*                  are converted to upper case in
*                  RED4_SEEK_OBSERVATION.                           (SMB)
*     23-Jul-1990: Character handling improved, and modified
*                  to accept 4 digit observation numbers.           (SMB)
*     23-Jul-1990: INDDIR changed to CGS4_INDEX.                    (SMB)
*     24-Jul-1990: Mistake fixed. The change to the format of the
*                  index file has also changed its KEY positions.   (SMB)
*     26-Jul-1990: The KEY specification was still invalid. Typing
*                  mistake fixed.                                   (SMB)
*     21-Aug-1990: Phase 1 of major changes: Header
*                  information in the observation and reduced
*                  observation files is now written in a .FITS
*                  structure.                                       (SMB)
*      2-Oct-1990: Messages made more explicit and less verbose.    (SMB)
*     24-Oct-1990: Modified to report the efficiency of each
*                  observation.                                     (SMB)
*     19-Nov-1990: Index file format changed again to include
*                  parameters required for wavelength and flux
*                  calibration. Unused variables removed. VERBOSE
*                  flag used to control output.                     (SMB)
*     23-Nov-1990: DSA reference name changed (see under "Bugs:"
*                  above).                                          (SMB)
*     30-Nov-1990: The END_TIME written to the index file record
*                  was causing problems because it wrapped round
*                  after 24 hours. Modified so that the number
*                  of elapsed days since observation 1 is taken
*                  into account.                                    (SMB)
*     11-Dec-1990: Split into RED4_FILE_OBSERVATION and
*                  RED4_FILE_OBSERVATION_2. Description altered.
*                  Reading of date from observation 1 made more
*                  efficient by remembering the previous file name
*                  and date.                                        (SMB)
*     12-Dec-1990: Efficiency calculation moved to
*                  RED4_FILE_OBSERVATION.                           (SMB)
*     12-Dec-1990: Index file format changed again! This is because
*                  it is possible for different observations to be
*                  made in NDR mode with different effective detector
*                  sizes, and the calibration frames for these must
*                  not get mixed up. Because of the GOTOs, it is
*                  possible for attempts to be made to close the index
*                  file without opening it. Flags added to prevent
*                  this.                                            (SMB)
*      2-Jan-1991: SAVE was missed by mistake. Added.               (SMB)
*     17-Jan-1991: END_TIME replaced by START_TIME.                 (SMB)
*      1-Feb-1991: Bug fix. CURRENT_OBS1 not set if observation 1
*                  cannot not be opened.                            (SMB)
*     24-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.            (SMB)
*     22-Aug-1991: Modified to fix bug reported by Andy Adamson.
*                  The name of the reduced observation file included
*                  as an argument.                                  (SMB)
*     17-Jul-1992: Create INDEX file first and iterate to find a
*                  suitable observation as a time reference         (PND)
*     19-Feb-1993: Conform to error strategy                        (PND)
*     23-Feb-1993: Add STATUS argument to IO_NXTLUN                 (PND)
*     31-Aug-1993: Add IRCAM specific stuff                         (PND)
*     23-Mar-1994: Only get CNFINDEX for CGS4                       (PND)
*     11-Nov-1994: Portable(?) version                              (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'                ! Includes SAI__ERROR
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'
*    Import :
      CHARACTER*(*)
     :  OBSERVATION,                   ! Name of observation file
     :  TYPE,                          ! Type to file observation as
     :  REDOBS                         ! Name of reduced observation file
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! ADAM stringlength function
*    Local Constants :
      INTEGER HOURS_PER_DAY            ! Number of hours in a day
      PARAMETER ( HOURS_PER_DAY = 24 )
*    Local variables :
      INTEGER CPOS                     ! Character string position
      INTEGER CLEN                     ! Character string length
      INTEGER FD                       ! File descriptor
      INTEGER IDATE1                   ! Date of first observation
      INTEGER IDATE2                   ! Date of second observation
      INTEGER NDAYS                    ! Number of elapsed days since
*                                      !   observation 1.
      INTEGER DSA_STATUS               ! DSA status value
      REAL RUTSTART                    ! UT at start of observation
      REAL AMSTART                     ! Air mass at start of observation
      REAL AMEND                       ! Air mass at end of observationa
      CHARACTER*80 INDEX_FILE          ! The name of the reduced-observation
*                                          index file, should be of form
*                                          CGS4_yymmdd.INDEX
      CHARACTER*80 OBS1FILE            ! First observation file.
      CHARACTER*32 STREDUCE            ! Data reduction time.
      CHARACTER*4 COMMENT              ! Dummy comment
*
      RECORD /OBSREC/ OBSREC           ! The observation record
      CHARACTER*20 INSTRUMENT          ! The instrument
      CHARACTER*80
     :  CURRENT_OBS1                   ! Current name of observation 1
      INTEGER
     :  CURRENT_IDATE1                 ! Current date of observation 1
      CHARACTER*4 ACCESS
      INTEGER     ELEMENTS
      INTEGER     STRLEN
      LOGICAL     EXIST
*    Local data :

*   Initialise the current name and date of observation 1 and ensure
*   they are saved between subsequent calls to this routine.
      DATA CURRENT_OBS1 / ' ' /        ! Blank means no observation
      DATA CURRENT_IDATE1 / 0 /
      SAVE CURRENT_OBS1, CURRENT_IDATE1
*-

      IF (STATUS .NE. ADAM__OK) RETURN

*    From the name of the observation file construct the name of the
*    index file, which should have a name of the form CGS4_890818.INDEX
      CALL RED4_OBSTOINDEX( OBSERVATION, INDEX_FILE, STATUS )

*    Open the index file
      CALL RIO_OPEN (INDEX_FILE, 'APPEND', 'UNFORMATTED', OBSRECSZ,
     : FD, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*       Open the DSA system
         DSA_STATUS = ADAM__OK
         CALL DSA_OPEN( DSA_STATUS )

*       Open the reduced observation file and find out the important points
*       of the observation
         DSA_STATUS = STATUS
         CALL RED4_CHECK_INPUT( REDOBS, STATUS )
         CALL DSA_NAMED_INPUT( 'REDOBS', REDOBS, DSA_STATUS )

*      Check the observation in question has been successfully reduced.
         CALL DSA_GET_FITS_C( 'REDOBS', 'STREDUCE', 0, STREDUCE, COMMENT,
     :    DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :        /'First error getting FITS item', STATUS )
         END IF

         IF ( ( STREDUCE .NE. ' ' ) .AND.
     :    ( STREDUCE .NE. '(not properly reduced)' ) ) THEN

*         Obtain the name of the first observation file. If this file
*         does not exist then iterate to find a suitable file.
            CALL RED4_OBSTOOBSN( OBSERVATION, OBS1FILE, STATUS )

*         If this is not the same as the current observation, obtain the
*         date when the two observations were taken, and determine the
*         number of days which have elapsed since observation 1.
            IF ( OBS1FILE .NE. OBSERVATION ) THEN

*            If this first observation is the same as the one opened last time,
*            do not open it again, but simply recall the information.
               IF ( OBS1FILE .EQ. CURRENT_OBS1 ) THEN

                  IDATE1 = CURRENT_IDATE1
               ELSE

*               The first observation is different from the last one.
*               Open it and reset the current first observation and date
*               if it worked.
                  DSA_STATUS = STATUS
                  CALL DSA_NAMED_INPUT( 'OBS1', OBS1FILE, DSA_STATUS )

                  CALL DSA_GET_FITS_I( 'OBS1', 'IDATE', 0,
     :             IDATE1, COMMENT, DSA_STATUS )

                  IF ( DSA_STATUS .EQ. ADAM__OK ) THEN

                     CURRENT_OBS1 = OBS1FILE
                     CURRENT_IDATE1 = IDATE1
                  END IF
               END IF

*            Obtain the date for the observation to be filed, and determine
*            the difference in days.
               CALL DSA_GET_FITS_I( 'REDOBS', 'IDATE', 0,
     :          IDATE2, COMMENT, DSA_STATUS )

               IF ( DSA_STATUS .NE. ADAM__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :             /'Second error getting FITS item', STATUS )
               END IF

               CALL RED4_DAY_DIFFERENCE( IDATE1, IDATE2, NDAYS, STATUS )
            ELSE

*            The observation being filed IS the first observation.
*            The number of days different will be zero.
               NDAYS = 0
            END IF

*         Check everything has worked so far.
            IF (STATUS .EQ. ADAM__OK) THEN

*            Obtain all the parameters to be written to the observation record.
*            The observation file name.
               CPOS = 0
               CPOS = INDEX( OBSERVATION, ':' )
               IF ( CPOS .EQ. 0 ) CPOS = INDEX( OBSERVATION, '/' )
               CLEN = CHR_LEN( OBSERVATION )
               OBSREC.OBSERVATION = OBSERVATION(1+CPOS:CLEN)

*            The observation number.
               DSA_STATUS = STATUS
               CALL DSA_GET_FITS_I( 'REDOBS', 'OBSNUM', 0,
     :          OBSREC.OBSNUM, COMMENT, DSA_STATUS )

*             time at which observation started
               CALL DSA_GET_FITS_F( 'REDOBS', 'RUTSTART', 0,
     :          RUTSTART, COMMENT, DSA_STATUS )

*            Convert this into a unique time for this set of observations
*            by combining it with the number of days which have elapsed
*            since observation 1.
               OBSREC.START_TIME = RUTSTART + REAL( NDAYS * HOURS_PER_DAY )

*             Get type of observation
               CALL DSA_GET_FITS_C( 'REDOBS', 'OBSTYPE', 0,
     :          OBSREC.TYPE, COMMENT, DSA_STATUS )

*             Set the quality of the file as desired
                IF ( TYPE .EQ. 'BAD' ) THEN
                  OBSREC.QUALITY = 'BAD'
                ELSE
                  OBSREC.QUALITY = 'GOOD'
                  IF ( (OBSREC.TYPE.NE.TYPE) .AND.
     :                 (TYPE.NE.'WHATEVER_IT_IS') ) THEN
                    CALL MSG_SETC( 'OBS', OBSERVATION )
                    CALL MSG_SETC( 'RT', OBSREC.TYPE )
                    CALL MSG_SETC( 'OT', TYPE )
                    CALL MSG_OUT (' ', 'WARNING: Observation ^OBS is a ^RT '/
     :               /'but you are filing it as a ^OT !', STATUS )
                    OBSREC.TYPE = TYPE
                  ENDIF
                ENDIF

*            The mode of observation (otherwise known as the "integration type").
               CALL DSA_GET_FITS_C( 'REDOBS', 'INTTYPE', 0,
     :          OBSREC.INTTYPE, COMMENT, DSA_STATUS )

*            The number of the group to which this observation belongs.
               CALL DSA_GET_FITS_I( 'REDOBS', 'GRPNUM', 0,
     :          OBSREC.GRPNUM, COMMENT, DSA_STATUS )

*             on-chip exposure time
               CALL DSA_GET_FITS_F( 'REDOBS', 'DEXPTIME', 0,
     :          OBSREC.EXPOSURE_TIME, COMMENT, DSA_STATUS )

*             The instrument configuration index.
               CALL DSA_SEEK_FITS( 'REDOBS', 'CNFINDEX', EXIST, ACCESS,
     :          ELEMENTS, STRLEN, DSA_STATUS )
               IF ( EXIST ) THEN
                  CALL DSA_GET_FITS_I( 'REDOBS', 'CNFINDEX', 0,
     :             OBSREC.CNFINDEX, COMMENT, DSA_STATUS )
               ELSE
                  OBSREC.CNFINDEX = 0
               END IF

*             type of instrument
               CALL DSA_GET_FITS_C( 'REDOBS', 'INSTRUME', 0,
     :          INSTRUMENT, COMMENT, DSA_STATUS )
               CALL CHR_RMBLK( INSTRUMENT )
               CALL CHR_UCASE( INSTRUMENT )

               IF ( INDEX( INSTRUMENT, 'IRCAM' ) .EQ. 0   .AND.
     :          INDEX( INSTRUMENT, 'ALICE' ) .EQ. 0 ) THEN

*                grating name
                  CALL DSA_GET_FITS_C( 'REDOBS', 'GRATING', 0,
     :             OBSREC.GRATING_NAME, COMMENT, DSA_STATUS )

*                grating wavelength
                  CALL DSA_GET_FITS_F( 'REDOBS', 'GLAMBDA', 0,
     :             OBSREC.GRATING_WVLNGTH, COMMENT, DSA_STATUS )

*                grating angle
                  CALL DSA_GET_FITS_F( 'REDOBS', 'GANGLE', 0,
     :             OBSREC.GRATING_ANGLE, COMMENT, DSA_STATUS )

*                grating order
                  CALL DSA_GET_FITS_I( 'REDOBS', 'GORDER', 0,
     :             OBSREC.GRATING_ORDER, COMMENT, DSA_STATUS )

*                slit name
                  CALL DSA_GET_FITS_C( 'REDOBS', 'SLIT', 0,
     :             OBSREC.SLIT_NAME, COMMENT, DSA_STATUS )

*                slit angle
                  CALL DSA_GET_FITS_F( 'REDOBS', 'SANGLE', 0,
     :             OBSREC.SLIT_ANGLE, COMMENT, DSA_STATUS )

*                CVF name
                  CALL DSA_GET_FITS_C( 'REDOBS', 'CVF', 0,
     :             OBSREC.CVF_NAME, COMMENT, DSA_STATUS )

*                CVF wavelength
                  CALL DSA_GET_FITS_F( 'REDOBS', 'CLAMBDA', 0,
     :             OBSREC.CVF_WAVELENGTH, COMMENT, DSA_STATUS )

*                Filters
                  CALL DSA_GET_FITS_C( 'REDOBS', 'FILTERS', 0,
     :             OBSREC.FILTERS, COMMENT, DSA_STATUS )
               ELSE

*                Filters
                  CALL DSA_GET_FITS_C( 'REDOBS', 'FILTER', 0,
     :             OBSREC.FILTERS, COMMENT, DSA_STATUS )

                  OBSREC.GRATING_NAME    = '(NONE)'
                  OBSREC.GRATING_WVLNGTH = 0.0
                  OBSREC.GRATING_ANGLE   = 0.0
                  OBSREC.GRATING_ORDER   = 0
                  OBSREC.SLIT_NAME       = '(NONE)'
                  OBSREC.SLIT_ANGLE      = 0.0
                  OBSREC.CVF_NAME        = '(NONE)'
                  OBSREC.CVF_WAVELENGTH  = 0.0
               ENDIF

*             Number of detector columns.
               CALL DSA_GET_FITS_I( 'REDOBS', 'DCOLUMNS', 0,
     :          OBSREC.DET_NCOLUMNS, COMMENT, DSA_STATUS )

*             Number of detector rows.
               CALL DSA_GET_FITS_I( 'REDOBS', 'DROWS', 0,
     :          OBSREC.DET_NROWS, COMMENT, DSA_STATUS )

*             Detector encoder position at position index 1
               CALL DSA_GET_FITS_I( 'REDOBS', 'DENCBASE', 0,
     :          OBSREC.DET_ENC_BASE, COMMENT, DSA_STATUS )

*             Detector increment between successive positions
               CALL DSA_GET_FITS_F( 'REDOBS', 'DETINCR', 0,
     :          OBSREC.DET_INCR, COMMENT, DSA_STATUS )

*             Number of detector positions measured
               CALL DSA_GET_FITS_I( 'REDOBS', 'DETNINCR', 0,
     :          OBSREC.DET_NINCR, COMMENT, DSA_STATUS )

*            The mean airmass of the observation (taken as the mid point
*            between the start and end airmass).
               CALL DSA_GET_FITS_F( 'REDOBS', 'AMSTART', 0,
     :          AMSTART, COMMENT, DSA_STATUS )
               CALL DSA_GET_FITS_F( 'REDOBS', 'AMEND', 0,
     :          AMEND, COMMENT, DSA_STATUS )

               OBSREC.AIRMASS = (AMSTART + AMEND) / 2.0

*             If an error has occurred jump to the error processing part.
               IF ( DSA_STATUS .NE. ADAM__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :             /'Third error getting FITS item', STATUS )
               END IF

               IF ( STATUS .EQ. SAI__OK ) THEN

*               Issue a message to the user. (This may be removed if considered
*               too verbose).
                  CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
                  CALL MSG_SETC( 'INDEX', INDEX_FILE )
                  CALL MSG_OUT( ' ', 'Adding observation ^OBSERVATION to '/
     :             /'index file ^INDEX', STATUS )

*          In verbose mode, write record to screen first
            IF ( VERBOSE ) THEN
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.OBSNUM' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.OBSNUM )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.OBSERVATION' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.OBSERVATION )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.QUALITY' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.QUALITY )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.TYPE' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.TYPE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.START_TIME' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.START_TIME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.INTTYPE' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.INTTYPE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRPNUM' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.GRPNUM )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CNFINDEX' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.CNFINDEX )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.EXPOSURE_TIME' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.EXPOSURE_TIME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NCOLUMNS' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NCOLUMNS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NROWS' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NROWS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_ENC_BASE' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_ENC_BASE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_INCR' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.DET_INCR )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.DET_NINCR' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.DET_NINCR )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.GRATING_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_WVLNGTH' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.GRATING_WVLNGTH )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_ANGLE' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.GRATING_ANGLE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.GRATING_ORDER' )
              CALL MSG_SETI( 'ITMVAL', OBSREC.GRATING_ORDER )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.SLIT_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.SLIT_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.SLIT_ANGLE' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.SLIT_ANGLE )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CVF_NAME' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.CVF_NAME )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.CVF_WAVELENGTH' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.CVF_WAVELENGTH )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.FILTERS' )
              CALL MSG_SETC( 'ITMVAL', OBSREC.FILTERS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SETC( 'ITMNAM', 'OBSREC.AIRMASS' )
              CALL MSG_SETR( 'ITMVAL', OBSREC.AIRMASS )
              CALL MSG_OUT( ' ', '^ITMNAM = ^ITMVAL', STATUS )
              CALL MSG_SYNC( STATUS )
            ENDIF


*                write the record to the index file
*        NOTE - NEED TO FIND A WAY TO AVOID DUPLICATE ENTRIES...
                  CALL RIO_WRITE (FD, OBSREC.OBSNUM, OBSRECSZ, OBSREC, STATUS)
               END IF
            ENDIF

         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :       /'This observation has not been properly '/
     :       /'reduced - not filed', STATUS )
         END IF

         CALL RIO_CLOSE (FD, STATUS)

*       close the DSA system
         DSA_STATUS = STATUS
         CALL DSA_CLOSE( DSA_STATUS )

         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :        /'Error closing DSA', STATUS )
         END IF

      ELSE

*       File open error
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'INDEX', INDEX_FILE )
         CALL ERR_REP( ' ', 'RED4_FILE_OBSERVATION_2: '/
     :     /'Unable to open index file ^INDEX', STATUS )

      END IF

      END
