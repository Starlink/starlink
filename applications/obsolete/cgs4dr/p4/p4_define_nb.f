*+  P4_DEFINE_NB - Define the contents of the P4_NB noticeboard
      SUBROUTINE P4_DEFINE_NB( STATUS )
*    Description :
*     This routine defines the contents of the P4_NB noticeboard.
*    Invocation :
*     CALL P4_DEFINE_NB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      4-Aug-1994: Original version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'          ! SAI error and status codes
      INCLUDE 'NBS_ERR'          ! NBS error codes
      INCLUDE 'PRM_PAR'          ! Defines machine precision codes
*    Status :
      INTEGER STATUS             ! Global status
*    External references :
      INTEGER CHR_LEN            ! Finds used length of string
*    Global variables :
      INCLUDE 'P4COM.INC'        ! P4 common block
*    Local constants :
      INTEGER NBS_TUNE_VAL       ! New value to cope with larger noticebaord
      PARAMETER ( NBS_TUNE_VAL = 4 * 32768 )
*    Local variables :
      INTEGER
     :  LSID,                    ! Local SID never used
     :  I,                       ! Loop counter
     :  IGNORE,                  ! Identified from NBS that we don't need
     :  TOPSID,                  ! Top-level SID
     :  PORT_SID( 0:MAXPORT ),   ! Port SIDs
     :  ERR_STAT                 ! An error status
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the name of the noticeboard to be used
      CALL PAR_GET0C( 'NOTICEBOARD', NOTICEBOARD, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         ERR_STAT = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STAT )
         CALL ERR_REP( ' ', 'P4_DEFINE_NB: '/
     :     /'Failed to obtain noticeboard name, Status = ^ES', STATUS )
      ELSE
        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'NB', NOTICEBOARD )
          CALL MSG_OUT( ' ', 'Starting definition of noticeboard ^NB', STATUS )
        ENDIF
      ENDIF

*    Increase the size of MAX_DEFN_SIZE by NBS_TUNE_VAL to cope with this noticeboard
      CALL NBS_TUNE( 'MAX_DEFN_SIZE', NBS_TUNE_VAL, IGNORE, STATUS )

*    Begin the noticeboard definition and obtain the top level
      CALL NBS_BEGIN_DEFINITION( TOPSID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         ERR_STAT = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STAT )
         CALL ERR_REP( ' ', 'P4_DEFINE_NB: '/
     :     /'Failed to begin noticeboard definition, Status = ^ES', STATUS )
      ELSE
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Defining noticeboard', STATUS )
      ENDIF

*    Define top structures PORT_n
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_0', 'PORT_0', PORT_SID(0), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_1', 'PORT_1', PORT_SID(1), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_2', 'PORT_2', PORT_SID(2), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_3', 'PORT_3', PORT_SID(3), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_4', 'PORT_4', PORT_SID(4), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_5', 'PORT_5', PORT_SID(5), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_6', 'PORT_6', PORT_SID(6), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_7', 'PORT_7', PORT_SID(7), STATUS )
      CALL NBS_DEFINE_STRUCTURE( TOPSID, 'PORT_8', 'PORT_8', PORT_SID(8), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         ERR_STAT = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STAT )
         CALL ERR_REP( ' ', 'P4_DEFINE_NB: '/
     :     /'Failed to define PORT_n structures, Status = ^ES', STATUS )
      ELSE
         IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Defined PORT_n structures', STATUS )
      ENDIF

*    Define lower level primitives
      CALL NBS_DEFINE_PRIMITIVE( TOPSID, 'TASK_BUSY', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
      DO I = 0, MAXPORT, 1

*      Create within this level structure all the primitive elements
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DISPLAY_DATA', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DEVICE_LUT', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'TITLE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DEVICE_NAME', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DEVICE_XOPT', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DEVICE_YOPT', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DISPLAY_TYPE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'DISPLAY_PLANE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'CONTOUR_TYPE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'OVERCOLOUR', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'COLOUR_STYLE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'FG_COLOUR', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'BG_COLOUR', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'CUT_DIRECTION', '_CHAR', 0, NBS_FLEN, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'LAST_TYPE', '_CHAR', 0, NBS_FLEN, LSID, STATUS )

        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PLOT_AXES', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PLOT_ERRORS', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PLOT_WHOLE', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PRE_ERASE_PLOT', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'AUTOSCALE', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PORT_OK', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'PLOT_OK', '_LOGICAL', 0, VAL__NBI, LSID, STATUS )

        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'CONTOUR_LEVELS', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'HISTOGRAM_BINS', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'HISTOGRAM_XSTEP', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'HISTOGRAM_YSTEP', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'HIST_SMOOTH', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'TOOSMALL', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'TOOLARGE', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'ISTART', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'IEND', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'JSTART', '_INTEGER', 0, VAL__NBI, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'JEND', '_INTEGER', 0, VAL__NBI, LSID, STATUS )

        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'VXSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'VXEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'VYSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'VYEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'AXSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'AXEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'AYSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'AYEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'XSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'XEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'YSTART', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'YEND', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'MODE', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'MEAN', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'SIGMA', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'HIGH', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'LOW', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'FMIN', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'FMAX', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'SLICE_START', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'SLICE_END', '_REAL', 0, VAL__NBR, LSID, STATUS )
        CALL NBS_DEFINE_PRIMITIVE( PORT_SID(I), 'CHAR_HEIGHT', '_REAL', 0, VAL__NBR, LSID, STATUS )

        IF ( STATUS .NE. SAI__OK ) THEN
           ERR_STAT = STATUS
           STATUS = SAI__ERROR
           CALL MSG_SETI( 'I', I )
           CALL MSG_SETI( 'ES', ERR_STAT )
           CALL ERR_REP( ' ', 'P4_DEFINE_NB: '/
     :       /'Failed to define primitives for port ^I, Status = ^ES', STATUS )
        ELSE
          IF ( VERBOSE ) THEN
            CALL MSG_SETI( 'I', I )
            CALL MSG_OUT( ' ', 'Defined primitives for port ^I', STATUS )
          ENDIF
        ENDIF
      ENDDO

*    End the noticeboard definition and create the noticeboard on the fly
      IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Creating noticeboard', STATUS )

      CALL NBS_END_DEFINITION( NOTICEBOARD(1:CHR_LEN(NOTICEBOARD)), 'CREATE_NOTICEBOARD', STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Created noticeboard OK', STATUS )
      ELSE
        IF ( STATUS .EQ. NBS__SECTIONEXISTED ) THEN
          CALL ERR_ANNUL( STATUS )
          CALL MSG_OUT( ' ', 'Re-using existing global section', STATUS )
        ELSE
          ERR_STAT = STATUS
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'ES', ERR_STAT )
          CALL ERR_REP( ' ', 'P4_DEFINE_NB: '/
     :      /'Failed to create noticeboard, Status = ^ES', STATUS )
        ENDIF
      ENDIF

      END
