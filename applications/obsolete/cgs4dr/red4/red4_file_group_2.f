*+  RED4_FILE_GROUP_2 - File a specified group in the index file
      SUBROUTINE RED4_FILE_GROUP_2( GROUP, STATUS )
*    Description :
*     This routine files the given group in the index file,
*     together with the parameters defining the setup of the instrument
*     during the observations in that group. This routine is designed
*     for recording STANDARD observations which may be used for
*     subsequent reduction.
*    Invocation :
*     CALL RED4_FILE_GROUP_2( GROUP, STATUS )
*    Parameters :
*     GROUP             = CHARACTER*(*)( READ )
*           The name of the group file to be filed.
*     STATUS            = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     This routine needs restructuring without GOTOs.
*     There is insufficient error checking.
*
*     It would have been nice to have one subroutine which could file
*     either groups or observations. I tried this, but there were
*     too many subtle differences in the code, so I ended up writing
*     a whole new subroutine - this one.
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
*     13-Dec-1990: Original version, based on
*                  RED4_FILE_OBSERVATION_2. Some fiddles have been
*                  necessary to make the records written compatible
*                  with those written for observations.              (SMB)
*     14-Dec-1990: Bug fix. 5000 arbitrarily added to group number.  (SMB)
*     18-Dec-1990: 5000 changed to 10000. This will prevent clashes. (SMB)
*      2-Jan-1991: SAVE was missed by mistake. Added.                (SMB)
*     17-Jan-1991: END_TIME replaced by START_TIME.                  (SMB)
*     25-Feb-1991: DSA error statuses trapped, as these do
*                  not conform to the ADAM error scheme.             (SMB)
*     23-Jul-1992: Does not now require OBS1                         (PND)
*     18-Feb-1993: Conform to error strategy                         (PND)
*     08-Jul-1993: Remove RGDIR before filing                        (PND)
*      6-Dec-1993: Add IRCAM specific stuff                          (PND)
*     23-Mar-1994: Only get CNFINDEX if it exists                    (PND)
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
     :  GROUP                          ! Name of group file
*    Status :
      INTEGER STATUS
*    External references:
      INTEGER CHR_LEN                  ! Figaro string length function
*    Local Constants :
      INTEGER HOURS_PER_DAY            ! Number of hours in a day
      PARAMETER ( HOURS_PER_DAY = 24 )
*    Local variables :
      INTEGER DSA_STATUS               ! DSA status value
      INTEGER FD                       ! File descriptor
      INTEGER IDATE1                   ! Date of first observation
      INTEGER IDATE2                   ! Date of second observation
      INTEGER NDAYS                    ! Number of elapsed days since
*                                      !   observation 1.
      INTEGER GRPNUM                   ! The group number.
      INTEGER CPOS                     ! Character position
      REAL RUTSTART                    ! UT at start of group
      REAL AMSTART                     ! Air mass at start of group
      REAL AMEND                       ! Air mass at end of groupa
      CHARACTER*80 INDEX_FILE          ! The name of the reduced-observation
*                                          index file, should be of form
*                                          CGS4_yymmdd.INDEX
      CHARACTER*80 OBS1FILE            ! First observation file.
      CHARACTER*4 COMMENT              ! Dummy comment
*
      RECORD /OBSREC/ OBSREC           ! The observation record
      CHARACTER*20 INSTRUMENT          ! IRCAM or CGS4
      CHARACTER*80
     :  CURRENT_OBS1                   ! Current name of observation 1
      INTEGER
     :  CURRENT_IDATE1                 ! Current date of observation 1
      CHARACTER*4 ACCESS
      INTEGER     STRLEN
      INTEGER     ELEMENTS
      LOGICAL     EXIST
*    Local data :

*   Initialise the current name and date of observation 1 and ensure
*   they are saved between subsequent calls to this routine.
      DATA CURRENT_OBS1 / ' ' /        ! Blank means no observation
      DATA CURRENT_IDATE1 / 0 /
      SAVE CURRENT_OBS1, CURRENT_IDATE1
*-
      IF (STATUS .NE. ADAM__OK) RETURN

*    Open the DSA system
      DSA_STATUS = ADAM__OK
      CALL DSA_OPEN( DSA_STATUS )

*    Open the reduced group file and find out the important points of the
*    observations making up the group
      CALL RED4_CHECK_INPUT( GROUP, STATUS )
      CALL DSA_NAMED_INPUT( 'GRPFILE', GROUP, DSA_STATUS)

*   Convert the group name into the name of the first observation file
*   on the date the group was observed. (Observation file 1 is used,
*   because there is no guarantee that there will be a group 1).
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FILE_GROUP_2: '/
     :     /'Error opening reduced group', STATUS )
      END IF

      CALL RED4_GRPTOOBSN( GROUP, OBS1FILE, STATUS )
      CALL RED4_CHECK_INPUT( OBS1FILE, STATUS )

*   Obtain the date when the first observation was taken, and determine the
*   number of days which have elapsed since it and the first OBJECT observation
*   in this group.
*   If this first observation is the same as the one opened last time,
*   do not open it again, but simply recall the information.
      DSA_STATUS = STATUS
      IF ( OBS1FILE .EQ. CURRENT_OBS1 ) THEN

         IDATE1 = CURRENT_IDATE1
      ELSE

*      The first observation is different from the last one.
*      Open it and reset the current first observation and date.
         CALL DSA_NAMED_INPUT( 'OBS1', OBS1FILE, DSA_STATUS )
         CALL DSA_GET_FITS_I( 'OBS1', 'IDATE', 0,
     :     IDATE1, COMMENT, DSA_STATUS )

         CURRENT_OBS1 = OBS1FILE
         CURRENT_IDATE1 = IDATE1
      END IF

*   Obtain the date for the observation to be filed, and determine
*   the difference in days.
      CALL DSA_GET_FITS_I( 'GRPFILE', 'IDATE', 0,
     :  IDATE2, COMMENT, DSA_STATUS )

      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FILE_GROUP_2: '/
     :     /'First error getting FITS item', STATUS )
      END IF

      CALL RED4_DAY_DIFFERENCE( IDATE1, IDATE2, NDAYS, STATUS )

*   Check everything has worked so far.
      IF (STATUS .EQ. ADAM__OK) THEN

*      Obtain all the parameters to be written to the observation record.
*      Write the group file name to the "observation name" item. (A fiddle)
         CPOS = INDEX( GROUP, ':' )
         IF (CPOS .EQ. 0) CPOS = INDEX( GROUP, '/')
         OBSREC.OBSERVATION = GROUP(CPOS+1:CHR_LEN(GROUP))

*      Write the group number to the "observation number" item.
*      Add an arbitrary offset to try and prevent clashes between
*      the observation and group numbers (a fiddle). N.B. This fiddle
*      will fail if more than 10000 observations are made, but is fairly
*      safe because the maximum allowed observation number is 9999.
         DSA_STATUS = STATUS
         CALL DSA_GET_FITS_I( 'GRPFILE', 'GRPNUM', 0,
     :     GRPNUM, COMMENT, DSA_STATUS )

*      just use the grpnum for now...
!         OBSREC.OBSNUM = GRPNUM + 10000
         OBSREC.OBSNUM = GRPNUM

*      Time at which the first observation in the group started
         CALL DSA_GET_FITS_F( 'GRPFILE', 'RUTSTART', 0,
     :     RUTSTART, COMMENT, DSA_STATUS )

*      Convert this into a unique time for this set of observations
*      by combining it with the number of days which have elapsed
*      since observation 1.
         OBSREC.START_TIME = RUTSTART + REAL( NDAYS * HOURS_PER_DAY )

*      Type of observation
         CALL DSA_GET_FITS_C( 'GRPFILE', 'OBSTYPE', 0,
     :     OBSREC.TYPE, COMMENT, DSA_STATUS )

*      The mode of observation (otherwise known as the "integration type").
         CALL DSA_GET_FITS_C( 'GRPFILE', 'INTTYPE', 0,
     :     OBSREC.INTTYPE, COMMENT, DSA_STATUS )

*      The (unfiddled) group number.
         OBSREC.GRPNUM = GRPNUM

*      The instrument configuration index.
         CALL DSA_SEEK_FITS( 'GRPFILE', 'CNFINDEX', EXIST, ACCESS,
     :      ELEMENTS, STRLEN, DSA_STATUS )
         IF ( EXIST ) THEN
            CALL DSA_GET_FITS_I( 'GRPFILE', 'CNFINDEX', 0,
     :        OBSREC.CNFINDEX, COMMENT, DSA_STATUS )
         ELSE
            OBSREC.CNFINDEX = 0
         END IF

*       on-chip exposure time
         CALL DSA_GET_FITS_F( 'GRPFILE', 'DEXPTIME', 0,
     :     OBSREC.EXPOSURE_TIME, COMMENT, DSA_STATUS )

*       instrument
         CALL DSA_GET_FITS_C( 'GRPFILE', 'INSTRUME', 0,
     :     INSTRUMENT, COMMENT, DSA_STATUS )
         CALL CHR_RMBLK( INSTRUMENT )
         CALL CHR_UCASE( INSTRUMENT )

         IF ( INDEX( INSTRUMENT, 'IRCAM' ) .EQ. 0   .AND.
     :        INDEX( INSTRUMENT, 'ALICE' ) .EQ. 0 ) THEN

*          grating name
            CALL DSA_GET_FITS_C( 'GRPFILE', 'GRATING', 0,
     :        OBSREC.GRATING_NAME, COMMENT, DSA_STATUS )

*          grating wavelength
            CALL DSA_GET_FITS_F( 'GRPFILE', 'GLAMBDA', 0,
     :        OBSREC.GRATING_WVLNGTH, COMMENT, DSA_STATUS )

*          grating angle
            CALL DSA_GET_FITS_F( 'GRPFILE', 'GANGLE', 0,
     :        OBSREC.GRATING_ANGLE, COMMENT, DSA_STATUS )

*          grating order
            CALL DSA_GET_FITS_I( 'GRPFILE', 'GORDER', 0,
     :        OBSREC.GRATING_ORDER, COMMENT, DSA_STATUS )

*          slit name
            CALL DSA_GET_FITS_C( 'GRPFILE', 'SLIT', 0,
     :        OBSREC.SLIT_NAME, COMMENT, DSA_STATUS )

*          slit angle
            CALL DSA_GET_FITS_F( 'GRPFILE', 'SANGLE', 0,
     :        OBSREC.SLIT_ANGLE, COMMENT, DSA_STATUS )

*          CVF name
            CALL DSA_GET_FITS_C( 'GRPFILE', 'CVF', 0,
     :        OBSREC.CVF_NAME, COMMENT, DSA_STATUS )

*          CVF wavelength
            CALL DSA_GET_FITS_F( 'GRPFILE', 'CLAMBDA', 0,
     :        OBSREC.CVF_WAVELENGTH, COMMENT, DSA_STATUS )

*          Filters
            CALL DSA_GET_FITS_C( 'GRPFILE', 'FILTERS', 0,
     :        OBSREC.FILTERS, COMMENT, DSA_STATUS )
         ELSE

*          Filters
            CALL DSA_GET_FITS_C( 'GRPFILE', 'FILTER', 0,
     :        OBSREC.FILTERS, COMMENT, DSA_STATUS )

            OBSREC.GRATING_NAME     = '(NONE)'
            OBSREC.GRATING_WVLNGTH  = 0.0
            OBSREC.GRATING_ANGLE    = 0.0
            OBSREC.GRATING_ORDER    = 0
            OBSREC.SLIT_NAME        = '(NONE)'
            OBSREC.SLIT_ANGLE       = 0.0
            OBSREC.CVF_NAME         = '(NONE)'
            OBSREC.CVF_WAVELENGTH   = 0.0
         ENDIF

*       Number of detector columns.
         CALL DSA_GET_FITS_I( 'GRPFILE', 'DCOLUMNS', 0,
     :     OBSREC.DET_NCOLUMNS, COMMENT, DSA_STATUS )

*       Number of detector rows.
         CALL DSA_GET_FITS_I( 'GRPFILE', 'DROWS', 0,
     :     OBSREC.DET_NROWS, COMMENT, DSA_STATUS )

*       Detector encoder position at position index 1
         CALL DSA_GET_FITS_I( 'GRPFILE', 'DENCBASE', 0,
     :     OBSREC.DET_ENC_BASE, COMMENT, DSA_STATUS )

*       Detector increment between successive positions
         CALL DSA_GET_FITS_F( 'GRPFILE', 'DETINCR', 0,
     :     OBSREC.DET_INCR, COMMENT, DSA_STATUS )

*       Number of detector positions measured
         CALL DSA_GET_FITS_I( 'GRPFILE', 'DETNINCR', 0,
     :     OBSREC.DET_NINCR, COMMENT, DSA_STATUS )

*      The mean airmass of the group (taken as the mid point
*      between the start and end airmass).
         CALL DSA_GET_FITS_F( 'GRPFILE', 'AMSTART', 0,
     :     AMSTART, COMMENT, DSA_STATUS )
         CALL DSA_GET_FITS_F( 'GRPFILE', 'AMEND', 0,
     :     AMEND, COMMENT, DSA_STATUS )

         OBSREC.AIRMASS = (AMSTART + AMEND) / 2.0

*       If an error has occurred jump to the error processing part.
         IF ( DSA_STATUS .NE. ADAM__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FILE_GROUP_2: '/
     :        /'Second error getting FITS items', STATUS )
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Set the quality of the record to 'GOOD'.
            OBSREC.QUALITY = 'GOOD'

*          from the name of the group file construct the name of the
*          index file, which should have a name of the form CGS4_890818.INDEX
            CALL RED4_GRPTOINDEX( GROUP, INDEX_FILE, STATUS )

*         Issue a message to the user. (This may be removed if considered
*         too verbose).
            CALL MSG_SETC( 'GROUP', GROUP )
            CALL MSG_SETC( 'INDEX', INDEX_FILE )
            CALL MSG_OUT( ' ', 'Adding group ^GROUP to '/
     :       /'index file ^INDEX', STATUS )

*          Open the index file
            CALL RIO_OPEN (INDEX_FILE, 'APPEND', 'UNFORMATTED', OBSRECSZ,
     :       FD, STATUS)

            IF (STATUS .NE. SAI__OK) THEN

*             File open error
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'INDEX', INDEX_FILE )
               CALL ERR_REP( ' ', 'RED4_FILE_GROUP_2: '/
     :          /'Error opening index file ^INDEX', STATUS )

            ELSE

*             write the record to the index file
               CALL RIO_WRITE (FD, OBSREC.OBSNUM, OBSRECSZ, OBSREC, STATUS)
               CALL RIO_CLOSE (FD, STATUS)

            END IF
         END IF
      ENDIF

*    close the DSA system
      DSA_STATUS = STATUS
      CALL DSA_CLOSE( DSA_STATUS )
      IF ( DSA_STATUS .NE. ADAM__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FILE_GROUP_2: '/
     :     /'Error closing DSA', STATUS )
      END IF

      END
