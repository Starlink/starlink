*+  P4_WRITE_CONFIG - Write current configuration to file.
      SUBROUTINE P4_WRITE_CONFIG( PORT, STATUS )
*    Description :
*     This routine writes the current data reduction configuration to a file.
*    Invocation :
*     CALL P4_WRITE_CONFIG( PORT, STATUS )
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      7-Aug-1994: Original version                                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER PORT
*    Status :
      INTEGER STATUS                   ! Global status
*    External references :
      INTEGER CHR_LEN                  ! Length of string function
*    Global variables :
      INCLUDE 'P4COM.INC'              ! P4 common block
*    Local variables :
      CHARACTER*100
     :  FILE,                    ! Name of configuration file
     :  COMMENT                  ! An optional comment
      CHARACTER*32
     :  USER,                    ! Calling username
     :  SYSNAM,                  ! The system name (OS)
     :  NODENAME,                ! The system nodename
     :  RELEASE,                 ! Software release
     :  VERSION,                 ! Software version id
     :  MACHINE,                 ! Machine type
     :  CALTIME,                 ! Calendar time
     :  TMPSTR                   ! Temporary string
      CHARACTER*1
     :  CVAL                     ! Character string used for encoding
      INTEGER
     :  CPOS1, CPOS2,
     :  CLEN,                    ! Length in character ITM_STR
     :  LUN,                     ! Fortran logical unit number
     :  NTICKS,                  ! Time ticks
     :  ICOUNT,                  ! A counter
     :  NSTART,                  ! Start of loop value
     :  NEND                     ! End of loop value
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   If 0 <= PORT <= MAXPORT, write out config for just that port
      IF ( PORT.GE.0 .AND. PORT.LE.MAXPORT ) THEN
        NSTART = PORT
        NEND   = PORT
*   Else do all of them
      ELSE
        NSTART = 0
        NEND   = MAXPORT
      ENDIF

*   Obtain the name of the configuration file to be created.
      CALL PAR_GET0C( 'FILE', FILE, STATUS )
      CPOS1 = INDEX( FILE, 'P4_CONFIG' )
      CPOS2 = INDEX( FILE, SEPARATOR )
      IF ( CPOS1.EQ.0 .AND. CPOS2.EQ.0 ) THEN
        FILE = PREFIX // 'P4_CONFIG' // SEPARATOR // FILE(1:CHR_LEN(FILE))
        CALL CHR_RMBLK( FILE )
      ENDIF
      IF ( INDEX( FILE, '.p4' ).EQ.0 ) FILE = FILE(1:CHR_LEN(FILE)) // '.p4'
      CALL P4_CHECK_INPUT( FILE, STATUS )

*   Create a new configuration file
      CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'APPEND', 'LIST', 0, LUN, STATUS )

*   Do some PoSiX calls to get some information
      CALL PSX_CUSERID( USER, STATUS )
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, CALTIME, STATUS )
      CALL PSX_UNAME( SYSNAM, NODENAME,
     :   RELEASE, VERSION, MACHINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_OUT( ' ',
     :     'Unable to obtain PoSiX information '/
     :     /'(error ignored)', STATUS )
         CALL ERR_ANNUL( STATUS )
      ENDIF

*   Write out header information
      COMMENT = '{ +'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      COMMENT = '{ ' // FILE
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      COMMENT = '{ Written by ' // NODENAME(1:CHR_LEN(NODENAME)) /
     :   / '::' // USER(1:CHR_LEN(USER)) // ' on ' /
     :   / CALTIME(1:CHR_LEN(CALTIME))
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      COMMENT = '{ Written under ' // SYSNAM(1:CHR_LEN(SYSNAM)) // ' / '
     :   // RELEASE(1:CHR_LEN(RELEASE)) // ' / '
     :   // VERSION(1:CHR_LEN(VERSION)) // ' / '
     :   // MACHINE(1:CHR_LEN(MACHINE)) // ' '
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      COMMENT = '{ -'
      CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

* Now loop and write items
      DO ICOUNT = NSTART, NEND, 1

        CALL CHR_ITOC( ICOUNT, CVAL, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DEVICE_NAME'') '/
     :     /'"'//DEVICE_NAME(ICOUNT)(1:CHR_LEN(DEVICE_NAME(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DEVICE_XOPT'') '/
     :     /'"'//DEVICE_XOPT(ICOUNT)(1:CHR_LEN(DEVICE_XOPT(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DEVICE_YOPT'') '/
     :     /'"'//DEVICE_YOPT(ICOUNT)(1:CHR_LEN(DEVICE_YOPT(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DEVICE_LUT'') '/
     :     /'"'//DEVICE_LUT(ICOUNT)(1:CHR_LEN(DEVICE_LUT(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DISPLAY_DATA'') '/
     :     /'"'//DISPLAY_DATA(ICOUNT)(1:CHR_LEN(DISPLAY_DATA(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.TITLE'') '/
     :     /'"'//TITLE(ICOUNT)(1:CHR_LEN(TITLE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DISPLAY_TYPE'') '/
     :     /'"'//DISPLAY_TYPE(ICOUNT)(1:CHR_LEN(DISPLAY_TYPE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.DISPLAY_PLANE'') '/
     :     /'"'//DISPLAY_PLANE(ICOUNT)(1:CHR_LEN(DISPLAY_PLANE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.CONTOUR_TYPE'') '/
     :     /'"'//CONTOUR_TYPE(ICOUNT)(1:CHR_LEN(CONTOUR_TYPE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.OVERCOLOUR'') '/
     :     /'"'//OVERCOLOUR(ICOUNT)(1:CHR_LEN(OVERCOLOUR(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.COLOUR_STYLE'') '/
     :     /'"'//COLOUR_STYLE(ICOUNT)(1:CHR_LEN(COLOUR_STYLE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.FG_COLOUR'') '/
     :     /'"'//FG_COLOUR(ICOUNT)(1:CHR_LEN(FG_COLOUR(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.BG_COLOUR'') '/
     :     /'"'//BG_COLOUR(ICOUNT)(1:CHR_LEN(BG_COLOUR(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.CUT_DIRECTION'') '/
     :     /'"'//CUT_DIRECTION(ICOUNT)(1:CHR_LEN(CUT_DIRECTION(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.LAST_TYPE'') '/
     :     /'"'//LAST_TYPE(ICOUNT)(1:CHR_LEN(LAST_TYPE(ICOUNT)))//'"'
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

        CALL CHR_LTOC( PLOT_AXES(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PLOT_AXES'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( PLOT_ERRORS(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PLOT_ERRORS'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( PLOT_WHOLE(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PLOT_WHOLE'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( PRE_ERASE_PLOT(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PRE_ERASE_PLOT'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( AUTOSCALE(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.AUTOSCALE'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( PORT_OK(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PORT_OK'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_LTOC( PLOT_OK(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.PLOT_OK'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

        CALL CHR_ITOC( CONTOUR_LEVELS(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.CONTOUR_LEVELS'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( HISTOGRAM_BINS(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.HISTOGRAM_BINS'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( HISTOGRAM_XSTEP(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.HISTOGRAM_XSTEP'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( HISTOGRAM_YSTEP(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.HISTOGRAM_YSTEP'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( HIST_SMOOTH(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.HIST_SMOOTH'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( TOOSMALL(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.TOOSMALL'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( TOOLARGE(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.TOOLARGE'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( ISTART(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.ISTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( IEND(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.IEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( JSTART(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.JSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_ITOC( JEND(ICOUNT), TMPSTR, CLEN )
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.JEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )

        CALL CHR_RTOC( VXSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.VXSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( VXEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.VXEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( VYSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.VYSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( VYEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.VYEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( AXSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.AXSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( AXEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.AXEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( AYSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.AYSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( AYEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.AYEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( XSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.XSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( XEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.XEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( YSTART(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.YSTART'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( YEND(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.YEND'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( MODE(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.MODE'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( MEAN(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.MEAN'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( SIGMA(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.SIGMA'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( LOW(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.LOW'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( HIGH(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.HIGH'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( FMIN(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.FMIN'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( FMAX(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.FMAX'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( SLICE_START(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.SLICE_START'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( SLICE_END(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.SLICE_END'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
        CALL CHR_RTOC( CHAR_HEIGHT(ICOUNT), TMPSTR, CLEN )
        IF ( INDEX( TMPSTR, '.' ) .EQ. 0 ) TMPSTR = TMPSTR(1:CHR_LEN(TMPSTR)) // '.0000'
        COMMENT = 'PUTNBS ((P4_NB_ALIAS)&''.'/
     :     /'PORT_'//CVAL(1:CHR_LEN(CVAL))//'.CHAR_HEIGHT'') '//TMPSTR(1:CHR_LEN(TMPSTR))
        CALL FIO_WRITE( LUN, COMMENT(1:CHR_LEN(COMMENT)), STATUS )
      ENDDO

* Close file
      CALL FIO_CLOSE( LUN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'P4_WRITE_CONFIG: '/
     :    /'Failed to close file or write values', STATUS )
      ENDIF

      END
