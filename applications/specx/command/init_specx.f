*  History:
*     24 Nov 1993 (hme):
*        Disable startup from dump file. QUALIFIER has already been
*        changed to assume /NODUMP, but here we want also to avoid a
*        call to FSYRES, which would call INDEKS, which would try to call
*        DATE, which is a VMS system function, currently without
*        replacement.
*     03 Dec 1993 (hme):
*        For new file system FV4, start HDS and begin NDF.
*        With the old file system gone, we can currently not call
*        OPEN_SPECX_MAP, since it calls some header converion routine.
*        So the startup with a map is disabled. QUALIFIER has already
*        been changed to assum /NOMAP.
*     07 Feb 1994 (hme):
*        Re-instated startup with dump and map. Make .map extension
*        lower case. Call INITSP before QUALIFIERS so that it does not
*        sabotage its effect.
*     11 Feb 1994 (hme):
*        Enable opening of map named in dump file. This did not work,
*        because MAP_OPEN is read as true from the dump file. As a
*        result OPEN_SPECX_MAP would first try to close the map file.
*        Since it was not really open, librating the alleged unit failed
*        and OPEN_SPECX_MAP returned with an error before even trying to
*        open the file. Now MAP_OPEN is reset to false here before a
*        call to OPEN_SPECX_MAP is made.
*      8 May 2000 (ajc):
*        Replace 'TYPE *' with 'PRINT *'
*        Add INCLUDE 'MAPS'
*        Unused STRING
*-----------------------------------------------------------------------

      SUBROUTINE INIT_SPECX (IFAIL)

      INCLUDE 'COMMAND_TABLE'
      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPS'
      INCLUDE 'MAPTITLES'

      LOGICAL   NO_DUMP, NEW_DUMP, NO_MAP, NEW_MAP
      LOGICAL   USE_OLD_MAP
      CHARACTER MAPNAME*72

      INTEGER   GEN_ILEN
      INTEGER   STATUS

* Ok, go...

      IFAIL = 0

C  Initialize variables according to site-wide initialization file

      CALL INITSP

C  Check for command line qualifiers

      MAPNAME = ' '
      PRINT *
      CALL QUALIFIERS (NO_DUMP, NEW_DUMP, NAMEFD,
     &                 NO_MAP,  NEW_MAP,  MAPNAME)

C  Start HDS and begin NDF.

      STATUS = 0
      CALL HDS_START( STATUS )
      CALL NDF_BEGIN

C  Load stored values of flags, data etc
C  Re-open any SPECX data files

      MAP_OPEN = .FALSE.

      IF (.NOT.NO_DUMP) THEN
        IF (.NOT.NEW_DUMP) NAMEFD = 'SPECX_DUMP'
        PRINT *, 'Starting from dump file ', NAMEFD
        CALL RDUMP (NAMEFD,IFAIL)
        IF (IFAIL.EQ.43) THEN
          IFAIL = 0
        ELSE IF (IFAIL.NE.0) THEN
          RETURN
        ELSE
          CALL FSYRES (IFAIL)
          IFAIL = 0
        END IF

C  Reset MAP_OPEN. It may have been read as true from the dump file, but
C  the mapfile is not open, of course. In order that it can be opened we
C  must reset MAP_OPEN to false.

        USE_OLD_MAP = .NOT.NO_MAP .AND. .NOT.NEW_MAP .AND. MAP_OPEN
        MAP_OPEN = .FALSE.

        IF (USE_OLD_MAP) THEN
          PRINT *, 'Attempting to open previous map...'
          CALL OPEN_SPECX_MAP (IFAIL)
          IF (IFAIL.NE.0)  THEN
            IFAIL = 0
            PRINT *, 'Failed to open map!'
          END IF
        END IF
      END IF

C  Open the map file and load the data cube

      IF (NEW_MAP .AND. MAPNAME .NE. ' ') THEN
        IF (INDEX (MAPNAME,'.') .EQ. 0) THEN
          MAPNAME = MAPNAME(:GEN_ILEN(MAPNAME))//'_map.sdf'
        END IF

        PRINT *,'Attempting to open specified map ',MAPNAME
        NAMEMP = MAPNAME
        CALL OPEN_SPECX_MAP (IFAIL)
        IF (IFAIL.NE.0)  THEN
          IFAIL = 0
          PRINT *, 'Failed to open map!'
        END IF

      END IF

C  Initialize the graphics package

      CALL SXGINIT

C  Reset title for X-axis etc

      XAXIS_NAME = ' '
      IF (NXS.LE.1 .OR. NXS.GT.4) THEN
        XAXIS_NAME  = 'Points'
        XAXIS_UNITS = 'Chans'
      ELSE IF (NXS.EQ.2) THEN
        XAXIS_NAME  = 'Frequency'
        XAXIS_UNITS = 'MHz'
        IF (ABS_FREQ) XAXIS_UNITS = 'GHz'
      ELSE IF (NXS.EQ.3) THEN
        XAXIS_NAME  = 'Velocity'
        XAXIS_UNITS = 'km/s'
      ELSE IF (NXS.EQ.4) THEN
        XAXIS_NAME  = 'User'
        XAXIS_UNITS = '????'
      END IF

C  Set up titles for map axes (saved in dump file, but probably wrong!)

      CALL SET_MAPTITLE (.FALSE., 0.0, 0.0)    ! X- & Y-axes only
      AXTIT(1)  = 'arcsec.'
      AXTIT(2)  = 'arcsec.'
      MAPTIT(3) = XAXIS_NAME                   ! Map Z-axis
      AXTIT(3)  = XAXIS_UNITS                  !

C  Any others?

      INTERP_WAIT = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------

