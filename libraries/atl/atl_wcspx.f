      SUBROUTINE ATL_WCSPX( KM1, KM2, CRPIX, OBSLON, OBSLAT, IWCS,
     :                      STATUS )
*+
*  Name:
*     ATL_WCSPX

*  Purpose:
*     Create a WCS FrameSet from a SPECX file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_WCSPX( KM1, KM2, CRPIX, OBSLON, OBSLAT, IWCS, STATUS )

*  Description:
*     This returns a pointer to a FrameSet describing the WCS
*     information in a SPECX file. The current Frame is a 3D Frame
*     with RA on axis 1, DEC on axis 2, and frequency on axis 3. The
*     base Frame is a 3D GRID Frame. The parameters defining the axes
*     are read from two supplied AST KeyMaps, which should contain
*     values for various items read from a SPECX and SPECX_MAP
*     extensions in a SPECX map (the observatory location are provided
*     separately by the caller).

*  Arguments:
*     KM1 = INTEGER (Given)
*        Pointer to an AST KeyMap holding items read from the SPECX
*        extension in the required SPECX map file. The key for each entry
*        is identicial to the name of the item in the SPECX extension.
*        It should contain entries with the following keys (the data type
*        with which each entry is accessed is also shown):
*
*           "JFREST(1)" - _INTEGER
*           "RA_DEC(1)" - _DOUBLE
*           "RA_DEC(2)" - _DOUBLE
*           "DPOS(1)" - _DOUBLE
*           "DPOS(2)" - _DOUBLE
*           "IDATE" - _CHAR
*           "ITIME" - _CHAR
*           "LSRFLG" - _INTEGER
*           "V_SETL(4)" - _DOUBLE
*           "JFCEN(1)" - _INTEGER
*           "JFINC(1)" - _INTEGER
*           "IFFREQ(1)" - _DOUBLE
*
*        In addition, the KeyMap may contain an item "CENTRECODE" (_INTEGER)
*        that specifies the co-ordinate system to which the RA_DEC and DPOS
*        values refer. It may take any of the following values:
*
*           1 : AZEL
*           4 : RD (geocentric apparent RA and Dec)
*           6 : RB (FK4 1950 RA and Dec)
*           7 : RJ (FK5 2000 RA and Dec)
*           8 : GA (galactic longitude and latitude)
*
*        An error is reported if any other value is supplied for CENTRECODE.
*        If CENTRECODE is missing a value of 6 (FK4 B1950) is assumed.
*     KM2 = INTEGER (Given)
*        Pointer to an AST KeyMap holding items read from the SPECX_MAP
*        extension in the required SPECX map file. The value AST__NULL
*        should be provided if the SPECX file does not have a SPECX_MAP
*        extension. If supplied, it should contain entries with the
*        following keys:
*
*           "CELLSIZE(1)" - _DOUBLE
*           "CELLSIZE(2)"  - _DOUBLE
*           "POSANGLE" - _DOUBLE
*
*        In addition, the KeyMap may contain an item "CELLCODE" (_INTEGER)
*        that specifies the co-ordinate system to which the CELLSIZE and
*        POSANGLE values refer. It may take any of the values listed for
*        CENTRECODE in the "KM1" argument description above. If CELLCODE
*        is missing a value of 6 (FK4 B1950) is assumed.
*     CRPIX( 3 ) = DOUBLE PRECISION (Given)
*        The pixel co-ordinates at the reference point. The spatial
*        position of the reference point is given by RA_DEC(1) and RA_DEC(2)
*        offset by the DPOS(1) and DPOS(2) values (all these are in the KM1
*        KeyMap). The spectral reference value is given by the JFCEN item
*        in KM1.
*     OBSLON = DOUBLE PRECISION (Given)
*        The geodetic longitude of the observatory. Radians, positive east.
*     OBSLAT = DOUBLE PRECISION (Given)
*        The geodetic latitude of the observatory. Radians, positive north.
*     IWCS = INTEGER (Returned)
*        The returned FrameSet.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Various assumptions are made about the meaning of several items
*     in the SPECX extensions. These are described in the code comments.
*     - Double Sideband is always assumed

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-FEB-2008 (DSB):
*        Original version, derived from CON_WCSPX in the CONVERT package.
*     25-FEB-2008 (DSB):
*        Obtain SPECX information from supplied KeyMaps rather than a
*        supplied NDF.
*     26-FEB-2008 (DSB):
*        Added item CELLCODE, which determines the celestial
*        longitude/latitude system used by the other values.
*     3-MAR-2008 (DSB):
*        Re-named "KM1" CELLCODE as CENTRECODE, and added new "KM2" item
*        CELLCODE.
*     4-MAR-2008 (DSB):
*        Do not use uninitialised parts of the string returned by
*        AST_MAPGET0C.
*     5-MAR-2008 (DSB):
*        SPECX supplied tinme and date appear to be UTC rather than UT1.
*     16-APR-2008 (DSB):
*        Replace the DIM argument (array dimensions) with CRPIX.
*     2-OCT-2012 (DSB):
*        Report an error if the rate of change of topocentric frequency with 
*        respect to source frequency cannot be evaluated.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER KM1
      INTEGER KM2
      DOUBLE PRECISION CRPIX( 3 )
      DOUBLE PRECISION OBSLON
      DOUBLE PRECISION OBSLAT

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS               ! Global status

*  Local Constants:
      DOUBLE PRECISION D2R         ! Degrees to radians conversion factor
      PARAMETER ( D2R = 0.01745329252 )
      DOUBLE PRECISION R2D         ! Radians to degrees conversion factor
      PARAMETER ( R2D = 1.0D0/D2R )
      DOUBLE PRECISION C           ! Speed of light (m/s)
      PARAMETER ( C = 2.99792458E8 )
      DOUBLE PRECISION DIAM        ! Diameter of JCMT (metres)
      PARAMETER ( DIAM = 15.0 )

      CHARACTER KM1ERR*68          ! Error message for missing KM1 items
      PARAMETER ( KM1ERR = 'ATL_WCSPX: Key ''^K'' not present '//
     :            'in KeyMap KM1 (programming error).' )

      CHARACTER KM2ERR*68          ! Error message for missing KM2 items
      PARAMETER ( KM2ERR = 'ATL_WCSPX: Key ''^K'' not present '//
     :            'in KeyMap KM2 (programming error).' )


*  Local Variables:
      CHARACTER CARD*80      ! FITS header card
      CHARACTER CENSYS*10    ! System value corresponding to CENTRECODE
      CHARACTER CMONTH*3     ! Month as a three-character abbreviation
      CHARACTER EPOCH*50     ! Epoch string
      CHARACTER KEY*20       ! KeyMap key
      CHARACTER MONTHS(12)*3 ! Months of the year.
      CHARACTER SIDEBAND*3   ! USB or LSB?
      CHARACTER SOR*10       ! Value for StdOfRest attribute
      CHARACTER SYS*10       ! Value for System attribute
      CHARACTER UTDATE*15    ! UTC date of the observation
      CHARACTER UTIME*15     ! UTC time of the observation
      DOUBLE PRECISION CD1   ! RA pixel size
      DOUBLE PRECISION CD2   ! DEC pixel size
      DOUBLE PRECISION CD3   ! Frequency pixel size
      DOUBLE PRECISION CRVAL3! Frequency (GHz) at the axis 1 reference point
      DOUBLE PRECISION DDEC  ! Dec offset (arc-seconds)
      DOUBLE PRECISION DEC   ! Central Dec (degrees)
      DOUBLE PRECISION DRA   ! RA offset (seconds)
      DOUBLE PRECISION DTBYDS! Rate of change of topo freq wrt source freq
      DOUBLE PRECISION IFFREQ! IF Frequency (GHz)
      DOUBLE PRECISION MJD   ! Modified Julian Date of observation
      DOUBLE PRECISION POSANG! Position angle of Y axis (degs East of North)
      DOUBLE PRECISION RA    ! Central RA (degrees)
      DOUBLE PRECISION SRCVEL! Assumed source velocity (km/s)
      INTEGER BFRM           ! Pointer to new GRID Frame
      INTEGER CELFRM         ! SkyFrame describing CELLCODE co-ords
      INTEGER DAY            ! Day
      INTEGER DSTAT          ! Local SLA status
      INTEGER FC             ! Pointer to AST FitsChan
      INTEGER FRM            ! Pointer to AST Frame to be added to NDF
      INTEGER FS             ! Pointer to AST FrameSet
      INTEGER HOUR           ! Hour of observation
      INTEGER IAT            ! Used length of string
      INTEGER ISOR           ! LSR identifier extracted from LSRFLG
      INTEGER ISYS           ! Integer code for co-ordinate system
      INTEGER JFCEN          ! Frequency (kHz) at spectral axis centre
      INTEGER JFINC          ! Pixel size (Hz) on spectral axis
      INTEGER JFREST         ! Rest frequency (kHz)
      INTEGER LEN            ! Used length of string
      INTEGER LOOP           ! Loop index
      INTEGER LSRFLG         ! Value of SPECX LSRFLG header item
      INTEGER MAP            ! Pointer to AST Mapping
      INTEGER MIN            ! Minute of observation
      INTEGER MONTH          ! Month
      INTEGER SKYFRM         ! Pointer to AST SkyFrame
      INTEGER SPCFRM         ! Pointer to AST SpecFrame
      INTEGER TDBFRM         ! TimeFrame describing the TDB timescale
      INTEGER TPFRM          ! SpecFrame describing Topocentric rest frame
      INTEGER TIMEFS         ! FrameSet connecting TDB abd UTC timescales
      INTEGER UTCFRM         ! TimeFrame describing the UTC timescale
      INTEGER YEAR           ! Year
      LOGICAL ISDSB          ! Is this a double sideband observation?
      REAL DAYS              ! Time of observation as fraction of a day
      REAL SEC               ! Seconds of observation

      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     :             'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/


*.

*  Initialise returned values.
      IWCS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  For now, assume that all specx files are dual sideband. This is wrong
*  but good enough for testing. It's also highly likely that SSB observations
*  might like to look in the other sideband for line contamination.
*  Either this should always be true, or we should prompt for it.
      ISDSB = .TRUE.

*  First create a SkyFrame that describes the celestial coordinate system
*  specified by the CENTRECODE item.
*  ======================================================================

*  Get the integer CENTRECODE value, using a default value of 6.
      KEY = 'CENTRECODE'
      IF( .NOT. AST_MAPGET0I( KM1, KEY, ISYS, STATUS ) ) ISYS = 6

*  Find the corresponding AST "System" code.
      IF( ISYS .EQ. 1 ) THEN
         CENSYS = 'AZEL'
      ELSE IF( ISYS .EQ. 4 ) THEN
         CENSYS = 'GAPPT'
      ELSE IF( ISYS .EQ. 6 ) THEN
         CENSYS = 'FK4'
      ELSE IF( ISYS .EQ. 7 ) THEN
         CENSYS = 'FK5'
      ELSE IF( ISYS .EQ. 8 ) THEN
         CENSYS = 'GALACTIC'
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'K', KEY )
         CALL MSG_SETI( 'I', ISYS )
         CALL ERR_REP( 'ATL_WCSPX_ERR1', 'ATL_WCSPX: Illegal value '//
     :                 '^I supplied for item ^K.', STATUS )
      END IF

*  Create a default SkyFrame and then set its System to that specified by
*  CENTRECODE.
      SKYFRM = AST_SKYFRAME( ' ', STATUS )
      CALL AST_SETC( SKYFRM, 'System', CENSYS, STATUS )

*  Epoch of observation. First extract the year, month and day of the
*  UTC from the IDATE item in the SPECX extension, and convert them to
*  integers.  Then attempt to calculate the MJD. Report an error if it
*  fails.
      KEY = 'IDATE'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0C( KM1, KEY, UTDATE, LEN,
     :                        STATUS ) ) GO TO 999

      CALL CHR_CTOI( UTDATE( 1:2 ), DAY, STATUS )
      CALL CHR_CTOI( UTDATE( 8:LEN ), YEAR, STATUS )

      CMONTH( 1:3 ) = UTDATE( 4:6 )
      CALL CHR_UCASE( CMONTH )

      MONTH = 0
      DO LOOP = 1, 12
         IF( CMONTH .EQ. MONTHS( LOOP ) ) THEN
            MONTH = LOOP
         END IF
      END DO

      CALL SLA_CALDJ( YEAR, MONTH, DAY, MJD, DSTAT )
      IF( DSTAT .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'D', UTDATE )
         CALL ERR_REP( 'ATL_WCSPX_ERR2', 'The ''IDATE'' item in the '//
     :                 'SPECX header has value ''^D'' which cannot '//
     :                 'be interpreted as a date.', STATUS )
         GO TO 999
      END IF

*  Now extract the hours mins and seconds fields from the ITIME header
*  item and convert to a fraction of a day.
      KEY = 'ITIME'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0C( KM1, KEY, UTIME, LEN, STATUS ) ) GO TO 999

      CALL CHR_CTOI( UTIME( 1:2 ), HOUR, STATUS )
      CALL CHR_CTOI( UTIME( 4:5 ), MIN, STATUS )
      CALL CHR_CTOR( UTIME( 7:LEN ), SEC, STATUS )

      CALL SLA_CTF2D( HOUR, MIN, SEC, DAYS, DSTAT )
      IF( DSTAT .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'D', UTIME )
         CALL ERR_REP( 'ATL_WCSPX_ERR3', 'The ''ITIME'' item in the '//
     :                 'SPECX header has value ''^D'' which cannot '//
     :                 'be interpreted as a time.', STATUS )
         GO TO 999
      END IF

*  Add the fractional day onto the Modified Julian Date.
      MJD = MJD + DAYS

*  We need to convert this MJD from UTC (used by specx) to TDB (used by
*  AST). Create two TimeFrames describing these two time systems.
      UTCFRM = AST_TIMEFRAME( 'TimeScale=UTC', STATUS )
      CALL AST_SETD( UTCFRM, 'ObsLon', OBSLON/D2R, STATUS )
      CALL AST_SETD( UTCFRM, 'ObsLat', OBSLAT/D2R, STATUS )

      TDBFRM = AST_TIMEFRAME( 'TimeScale=TDB', STATUS )
      CALL AST_SETD( TDBFRM, 'ObsLon', OBSLON/D2R, STATUS )
      CALL AST_SETD( TDBFRM, 'ObsLat', OBSLAT/D2R, STATUS )

*  Get a Mapping that converts from UTC to TDB, and use it to transform
*  the epoch value.
      TIMEFS = AST_CONVERT( UTCFRM, TDBFRM, ' ', STATUS )
      CALL AST_TRAN1( TIMEFS, 1, MJD, .TRUE., MJD, STATUS )

*  Format the TDB MJD into a form suitable for use with AST_SETC.
      WRITE( EPOCH, * ) 'MJD ',MJD

*  Set the Epoch attribute in the SkyFrame.
      CALL AST_SETC( SKYFRM, 'Epoch', EPOCH, STATUS )

*  Observatory position.
      CALL AST_SETD( SKYFRM, 'ObsLon', OBSLON/D2R, STATUS )
      CALL AST_SETD( SKYFRM, 'ObsLat', OBSLAT/D2R, STATUS )

*  Now create an AST SpecFrame to represent the spectral axis. This
*  will be used as axis 1 within the Frame which is finally added to the
*  WCS component of the output NDF.
*  ===================================================================

*  Create a [DSB]SpecFrame describing frequency in units of GHz.
      IF (ISDSB) THEN
         SPCFRM = AST_DSBSPECFRAME( 'System=freq,unit=GHz', STATUS )
      ELSE
         SPCFRM = AST_SPECFRAME( 'System=freq,unit=GHz', STATUS )
      END IF

*  Set its attributes so that they correspond to the information stored
*  in the SPECX extensions in the input map...

*  Since the DSB SpecFrame attributes can only be derived from
*  knowledge of the observed sideband which in specx requires JFINC
*  we defer setting of these attributes until JFINC and JFCEN are
*  read. [alternatively we could move those reads up]

*  Rest frequency. Convert from kHz to GHz.
      KEY = 'JFREST(1)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0I( KM1, KEY, JFREST, STATUS ) ) GO TO 999

      CALL AST_SETD( SPCFRM, 'RestFreq', DBLE( JFREST )*1.0D-6, STATUS )

*  Source position. These are interpreted as being in the system
*  specified by the CENTRECODE item. The "SKYFRM" SkyFrame created above
*  describes this system.
      KEY = 'RA_DEC(1)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0D( KM1, KEY, RA, STATUS ) ) GO TO 999

      KEY = 'RA_DEC(2)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0D( KM1, KEY, DEC, STATUS ) ) GO TO 999

      KEY = 'DPOS(1)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0D( KM1, KEY, DRA, STATUS ) ) GO TO 999

      KEY = 'DPOS(2)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0D( KM1, KEY, DDEC, STATUS ) ) GO TO 999

      DEC = DEC + DDEC/3600.0
      RA = RA + DRA/( 3600.0*COS( DEC*D2R ) )

      CALL AST_SETREFPOS( SPCFRM, SKYFRM, RA*D2R, DEC*D2R, STATUS )

*  Also set the reference position in the SkyFrame to the same value.
      CALL AST_SETD( SKYFRM, 'SkyRef(1)', RA*D2R, STATUS )
      CALL AST_SETD( SKYFRM, 'SkyRef(2)', DEC*D2R, STATUS )
      CALL AST_SETC( SKYFRM, 'SkyRefIs', 'Ignored', STATUS )

*  Set the Epoch attribute.
      CALL AST_SETC( SPCFRM, 'Epoch', EPOCH, STATUS )

*  Observatory position.
      CALL AST_SETD( SPCFRM, 'ObsLon', OBSLON/D2R, STATUS )
      CALL AST_SETD( SPCFRM, 'ObsLat', OBSLAT/D2R, STATUS )

*  Standard of rest. In order to get consistency with plots produced by
*  SPECX, it seems that the frequency axis defined by the JFCEN and JFINC
*  SPECX extension items (created below) give the frequency in the rest
*  frame of the source. There is more info on this in the specx user
*  manual (specx_v6-3.tex) section "Notes on individual header parameters".
      CALL AST_SETC( SPCFRM, 'StdOfRest', 'Source', STATUS )

*  The source is assumed to be moving at a speed given by V_SETL(4) within
*  the rest frame given by the bottom 4 bits of the LSRFLG extension item
*  (0=Topocentric (called "Telluric" within SPECX), 1=kinematic LSR
*  2=Heliocentric, 3 = geocentric). Set the source velocity rest frame.
      KEY = 'LSRFLG'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0I( KM1, KEY, LSRFLG, STATUS ) ) GO TO 999

      ISOR = MOD( LSRFLG, 16 )

      IF( ISOR .EQ. 0 ) THEN
         SOR = 'TOPO'

      ELSE IF( ISOR .EQ. 1 ) THEN
         SOR = 'LSRK'

      ELSE IF( ISOR .EQ. 2 ) THEN
         SOR = 'HELIO'

      ELSE IF( ISOR .EQ. 3 ) THEN
         SOR = 'GEO'

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', LSRFLG )
         CALL MSG_SETI( 'J', ISOR )
         CALL ERR_REP( 'ATL_WCSPX_ERR6', 'The ''LSRFLG'' item in the '//
     :                 'SPECX header has value ^I which specifies an '//
     :                 'unknown standard of rest value (^J).', STATUS )
         GO TO 999
      END IF

      CALL AST_SETC( SPCFRM, 'SourceVRF', SOR, STATUS )

*  The source velocity is radio, optical or relativistic, depending on the
*  bits 5 and 6 of the LSRFLG extension item (0=radio, 1=optical,
*  2=relativistic). Set the source velocity system.
      ISYS = LSRFLG / 16

      IF( ISYS .EQ. 0 ) THEN
         SYS = 'VRAD'

      ELSE IF( ISYS .EQ. 1 ) THEN
         SYS = 'VOPT'

      ELSE IF( ISYS .EQ. 2 ) THEN
         SYS = 'VELO'

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', LSRFLG )
         CALL MSG_SETI( 'J', ISYS )
         CALL ERR_REP( 'ATL_WCSPX_ERR2', 'The ''LSRFLG'' item in the '//
     :                 'SPECX header has value ^I which specifies an '//
     :                 'unknown velocity system value (^J).', STATUS )
         GO TO 999
      END IF

      CALL AST_SETC( SPCFRM, 'SourceSys', SYS, STATUS )

*  Get the assumed velocity of the source in the rest frame and spectral
*  system obtained from LSRFLG.
      KEY = 'V_SETL(4)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0D( KM1, KEY, SRCVEL, STATUS ) ) GO TO 999

*  If the source velocity is not zero, assign it to the SourceVel
*  attribute. It will be interpreted as a velocity within the rest frame
*  specified by the current value of SourceVRF (i.e. the rest frame
*  extracted from LSRFLG above).
      IF( SRCVEL .NE. 0.0 ) THEN
         CALL AST_SETD( SPCFRM, 'SourceVel', SRCVEL, STATUS )

*  If the source velocity is zero, set the StdOfRest attribute to the
*  rest frame indicated by LSRFLG.
      ELSE
         CALL AST_SETC( SPCFRM, 'StdOfRest', SOR, STATUS )
      END IF

*  Now create FITS-WCS keyword values describing the transformation
*  from GRID positions on the spectral axis into the corresponding
*  frequency values as described by the above SpecFrame. The axis is
*  assumed to be linear in frequency.
*  ===================================================================

*  Get the central frequency, in kHz. Convert to GHz.
      KEY = 'JFCEN(1)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0I( KM1, KEY, JFCEN, STATUS ) ) GO TO 999

      CRVAL3 = DBLE( JFCEN )*1.0E-6

*  Get the frequency increment per pixel, in Hz. Convert to GHz.
      KEY = 'JFINC(1)'
      CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
      IF( .NOT. AST_MAPGET0I( KM1, KEY, JFINC, STATUS ) ) GO TO 999

      CD3 = DBLE( JFINC )*1.0E-9

*  The specx user manual implies that, whilst the JFCEN value is in the
*  source rest frame, the JFINC value is in the topocentric rest frame.
*  So we need to convert JFINC from topcentric to source rest frame. Take
*  a copy of the SpecFrame and set its rest frame to topocentric.
      TPFRM = AST_COPY( SPCFRM, STATUS )
      CALL AST_SETC( TPFRM, 'StdOfRest', 'Topo', STATUS )

*  Get the Mapping from source to topocentric rest frame, and get the
*  rate of change of topocentric frequency with respect to source frequency
*  at the frequency given by JFCEN.
      DTBYDS = AST_RATE( AST_CONVERT( SPCFRM, TPFRM, ' ', STATUS ),
     :                   CRVAL3, 1, 1, STATUS )
      IF( DTBYDS .EQ. AST__BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Cannot determine  rate of change of '//
     :                 'topocentric frequency wrt source frequency.',
     :                 STATUS )
      END IF

*  Convert the frequency increment from topocentric to source.
      CD3 = CD3/DTBYDS

*  Since this is a convenient time, configure the DSB-ness
      IF (ISDSB) THEN

         CALL AST_SETD( SPCFRM, 'DSBCentre', CRVAL3, STATUS)

*  For dual sideband instruments we need to get the IF frequency
*  This is always in GHz in specx. It must be -ve if the observed
*  sideband is upper, positive if we are in LSB
         KEY = 'IFFREQ(1)'
         CALL ATL_KYCHK( KM1, KEY, KM1ERR, STATUS )
         IF( .NOT. AST_MAPGET0D( KM1, KEY, IFFREQ, STATUS ) ) GO TO 999

*  Calculate observed sidband. SPECX uses a positive IF if we are USB,
*  negative IF if we are LSB.
*  AST requires that the LO = JFCEN + IF such that LSB implies
*  a positive IF, LSB a negative. (ie the reverse of SPECX convention)
         IF (IFFREQ .GT. 0) THEN
            SIDEBAND = 'USB'
         ELSE
            SIDEBAND = 'LSB'
         END IF
         IFFREQ = -IFFREQ

*   Set sideband and IF
         CALL AST_SETC(SPCFRM, 'SideBand', SIDEBAND, STATUS)
         CALL AST_SETD( SPCFRM, 'IF', IFFREQ, STATUS)

      END IF


*  Now create the 3D Mapping which goes from 3D GRID coords to
*  (RA,Dec,Freq).
*  ===========================================================

*  Does the input NDF have a SPECX_MAP extension?
      IF( KM2 .NE. AST__NULL ) THEN

*  If so, get the pixel sizes from the SPECX map. Convert from arc-seconds
*  to degrees.
         KEY = 'CELLSIZE(1)'
         CALL ATL_KYCHK( KM2, KEY, KM2ERR, STATUS )
         IF( .NOT. AST_MAPGET0D( KM2, KEY, CD1, STATUS ) ) GO TO 999

         CD1 = CD1/3600.0

         KEY = 'CELLSIZE(2)'
         CALL ATL_KYCHK( KM2, KEY, KM2ERR, STATUS )
         IF( .NOT. AST_MAPGET0D( KM2, KEY, CD2, STATUS ) ) GO TO 999

         CD2 = CD2/3600.0

*  Get the position angle of the Y axis (assumed to be in degrees).
         KEY = 'POSANGLE'
         CALL ATL_KYCHK( KM2, KEY, KM2ERR, STATUS )
         IF( .NOT. AST_MAPGET0D( KM2, KEY, POSANG, STATUS ) ) GO TO 999

*  If the central position was given in a different celestial co-ordinate
*  system, we need to convert it into the system specified by CELLCODE.
*  Get the CELLCODE co-ordinate system.
         KEY = 'CELLCODE'
         IF( .NOT. AST_MAPGET0I( KM2, KEY, ISYS, STATUS ) ) ISYS = 6

         IF( ISYS .EQ. 1 ) THEN
            SYS = 'AZEL'
         ELSE IF( ISYS .EQ. 4 ) THEN
            SYS = 'GAPPT'
         ELSE IF( ISYS .EQ. 6 ) THEN
            SYS = 'FK4'
         ELSE IF( ISYS .EQ. 7 ) THEN
            SYS = 'FK5'
         ELSE IF( ISYS .EQ. 8 ) THEN
            SYS = 'GALACTIC'
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', ISYS )
            CALL MSG_SETC( 'K', KEY )
            CALL ERR_REP( 'ATL_WCSPX_ERR1B', 'ATL_WCSPX: Illegal '//
     :                    'value ^I supplied for item ^K.', STATUS )
         END IF

*  If it is different to the CENTRECODE system, create a SkyFrame describing
*  the CELLCODE co-ordinate system.
         IF( SYS .NE. CENSYS ) THEN
            CELFRM = AST_COPY( SKYFRM, STATUS )
            CALL AST_SETC( CELFRM, 'System', SYS, STATUS )

*  Create a FrameSet connecting the CELLCODE and CENTRECODE co-ordinate
*  systems.
            FS = AST_CONVERT( SKYFRM, CELFRM, ' ', STATUS )

*  Convert the central "RA" and "DEC" position from the CENTRECODE system
*  to the CELLCODE system.
            CALL AST_TRAN2( FS, 1, RA*D2R, DEC*D2R, .TRUE., RA, DEC,
     :                      STATUS )
            RA = RA/D2R
            DEC = DEC/D2R

         ELSE
            CELFRM = AST_CLONE( SKYFRM, STATUS )
         END IF

*  If no SPECX_MAP extension, use default values. The pixel size is
*  assumed to be equal to half the resolution of the telescope (assumed
*  to be  JCMT with a diameter of 15 metres).
      ELSE
         CD1 = 0.5*R2D*C/(JFCEN*1000.0*DIAM)
         CD2 = CD1
         POSANG = 0.0
         CELFRM = AST_CLONE( SKYFRM, STATUS )
      END IF

*  Create an empty AST FitsChan.
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*  Put FITS cards into the FitsChan representing a SIN projection of the
*  sky, using the pixel sizes obtained above. It is assumed that north is
*  parallel to the third axis and east is parallel to the second axis.
*  Axis 3 is the spectral axis. The headers describing the properties of
*  the WCS frame are not complete since the resulting Frame is not used
*  (the SpecFrame and SkyFrame created above are used instead).
      CALL AST_PUTFITS( FC, ' ', .FALSE., STATUS )

      CARD = 'NAXIS1  = '
      IAT = 10
      CALL CHR_PUTI( MAX( 2*INT( ABS( CRPIX( 1 ) ) ), 100 ), CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'NAXIS2  = '
      IAT = 10
      CALL CHR_PUTI( MAX( 2*INT( ABS( CRPIX( 2 ) ) ), 100 ), CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CALL AST_PUTFITS( FC, 'NAXIS3  = 1', .FALSE., STATUS )

      CARD = 'CRPIX1  = '
      IAT = 10
      CALL CHR_PUTD( CRPIX( 1 ), CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CRVAL1  = '
      IAT = 10
      CALL CHR_PUTD( RA, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CDELT1  = '
      IAT = 10
      CALL CHR_PUTD( -CD1, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CALL AST_PUTFITS( FC, 'CTYPE1  =  ''RA---SIN''', .FALSE., STATUS )

      CARD = 'CRPIX2  = '
      IAT = 10
      CALL CHR_PUTD( CRPIX( 2 ), CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CRVAL2  = '
      IAT = 10
      CALL CHR_PUTD( DEC, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CDELT2  = '
      IAT = 10
      CALL CHR_PUTD( CD2, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CROTA2  = '
      IAT = 10
      CALL CHR_PUTD( -POSANG, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CALL AST_PUTFITS( FC, 'CTYPE2  =  ''DEC--SIN''', .FALSE., STATUS )

      CARD = 'CRPIX3  = '
      IAT = 10
      CALL CHR_PUTD( CRPIX( 3 ), CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CRVAL3  = '
      IAT = 10
      CALL CHR_PUTD( CRVAL3, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CARD = 'CDELT3  = '
      IAT = 10
      CALL CHR_PUTD( CD3, CARD, IAT )
      CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )

      CALL AST_PUTFITS( FC, 'CTYPE3  =  ''FREQ    ''', .FALSE., STATUS )

*  Read a FrameSet from the FitsChan.
      CALL AST_CLEAR( FC, 'Card', STATUS )
      FS = AST_READ( FC, STATUS )

*  Create the returned FrameSet.
*  ============================================
*  The Base Frame in the above FrameSet will correspond to 3D GRID
*  coordinates. Get the Mapping from GRID to SPECTRUM-SKY coordinates.
      MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT, STATUS )

*  Create a 3D GRID Frame, and put it in a new FrameSet.
      BFRM = AST_FRAME( 3, 'Domain=GRID,unit(1)=pixel,unit(2)=pixel,'//
     :                  'unit(3)=pixel', STATUS )
      IWCS = AST_FRAMESET( BFRM, ' ', STATUS )

*  Construct a 3D CmpFrame describing all 3 axes. RA becomes axis 1 and DEC
*  becomes axis 2, and the spectral axis becomes axis 3.
      FRM = AST_CMPFRAME( CELFRM, SPCFRM, 'Title=Compound '//
     :                    'coordinates describing celestial position '//
     :                    'and spectral position', STATUS )

*  Add the CmpFrame created above into the returned FrameSet, using the
*  Mapping created above to connect it to the GRID Frame.
      CALL AST_ADDFRAME( IWCS, AST__BASE, MAP, FRM, STATUS )

*  Set the standard of rest to the rest frame indicated by
*  the LSRFLG extension item. This was requested by JACH (rather than
*  leaving the standard of rest set to "Source").
      CALL AST_SETC( IWCS, 'StdOfRest', SOR, STATUS )

*  Set the celestial co-ordinate system to the system specified by
*  CENTRECODE (it currently represents CELLCODE co-ords).
      CALL AST_SETC( IWCS, 'System(1)', CENSYS, STATUS )

*  Export the FrameSet pointer so that it is not anulled by the following
*  call to AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END

