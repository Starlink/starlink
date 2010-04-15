      SUBROUTINE TRACB0( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT,
     :                   SCNDIR, NDISP, OFFSET, DET, DTINDX, SCALE,
     :                   XLMT, YLMT, BAND, IDC, PDET, PPOSN,
     :                   PSTRTH, PPRF, PODET, POSCN, POSKY, POSTR,
     :                   COORDS, UNITS, COLOUR, CURSOR, CLRBLK,
     :                   FID, LOG, STATUS )
*+
*  Name:
*     TRACB0

*  Purpose:
*     Overlay a point source profile on a CRDD data trace.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB0( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT,
*                  SCNDIR, NDISP, OFFSET, DET, DTINDX, SCALE,
*                  XLMT, YLMT, BAND, IDC, PDET, PPOSN, PSTRTH,
*                  PPRF, PODET, POSCN, POSKY, POSTR, COORDS,
*                  UNITS, FID, COLOUR, CURSOR, CLRBLK, LOG, STATUS )

*  Description:
*     This routine overlay a point source profile on a displayed CRDD
*     data trace at a position specified by cursor, or by keyboard when
*     cursor is not available. When cursor is available on the graphic
*     device the routine will repeatly ask for the cursor positions
*     until the cursor is outside the NCAR grid window. Once a position
*     is specified, the routine will overlay a point source profile at
*     the position. The streigth, the in- and cross- position, and the
*     sky position of the source are output to the graphic device and
*     the enirvonment. If required, these data will also be written into
*     the log file.
*
*     The point source template is got from a NDF file associated with
*     parameter PPRF. The NDF file should have a size of 1 for its
*     second dimension ( one profile for all four wave bands ) or 4
*     ( one profile for each wave band ).  All profiles in the NDF
*     file have a normalised amplitude.
*
*     Before calling this routine, the NCAR (AUTOGRAPH) grid window
*     should be made to match the current SGS zone world coordinates,
*     that is the grid window should have the bounds (0.0, 1.0,
*     0.0, 1.0).
*

*  Arguments:
*     BSMP = INTEGER (Given)
*        Begin sample index of the input CRDD array.
*     ESMP = INTEGER (Given)
*        End sample index of the input CRDD array.
*     BDET = INTEGER (Given)
*        Begin detector index of the input CRDD array.
*     EDET = INTEGER (Given)
*        End detector index of the input CRDD array.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample of each CRDD data trace.
*     XSCN( BDET : EDET ) = REAL (Given)
*        The cross-scan distance of each CRDD data trace.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The unscaled data value of the CRDD data trace.
*     SCNDIR = LOGICAL (Given)
*        If true, the scan is from north to south. Otherwise from
*        south to north.
*     NDISP = INTEGER (Given)
*        Number of the traces in the display.
*     OFFSET( NDISP ) = REAL (Given)
*        The offset of each trace in the display.
*     DET( NDISP ) = INTEGER (Given)
*        The detector number of each trace in the display.
*     DTINDX( NDISP ) = INTEGER (Given)
*        The detector index of each displayed trace.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce the scaled data in the display.
*     XLMT( 2 ) = REAL (Given)
*        The in-scan limits of the display.
*     YLMT( 2 ) = REAL (Given)
*        The vertical limits of the display.
*     BAND = INTEGER (Given)
*        The wave band number of the CRDD file.
*     IDC = INTEGER (Given)
*        The IRC identifier of the CRDD file.
*     PDET = CHARACTER * ( * ) (Given)
*        The name of the parameter used, when cursor is unavailable, to
*        get the detector number along whose data trace the point source
*        template will be overlayed.
*     PPOSN = CHARACTER * ( * ) (Given)
*        The name of the parameter used, when cursor is unavailable, to
*        get the in-scan position at which the point source template is
*        overlayed.
*     PSTRTH = CHARACTER * ( * ) (Given)
*        The name of the parameter used, when cursor is unavailable, to
*        get the peak value of the point source template.
*     PPRF = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the file name of the
*        point source template NDF file.
*     PODET = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the detector number of
*        the trace at whose data trace the point source template has
*        been overlayed.
*     POSCN = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the in- and cross-scan
*        distances of the point source from the reference position to
*        the environment.
*     POSKY = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the sky coordinates of
*        the point source to the environment.
*     POSTR = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the peak value of the
*        point source to the environment.
*     COORDS = CHARACTER * ( * ) (Given)
*        The specified sky coordinate system, in which the the sky
*        coordinates of the point source will be output.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the displayed data trace.
*     COLOUR = LOGICAL (Given)
*        If true, colour is available on the graphic device. Otherwise
*        colour is not available.
*     CURSOR = LOGICAL (Given)
*        If true, cursor is available on the graphic device.
*     CLRBLK = LOGICAL (Given)
*        If true, graphic device can clear its surface partly.
*     FID = INTEGER (Given)
*        Log file descriptor.
*     LOG = INTEGER (Given and Returned)
*        =0: Logging is not required.
*        >0: Logging is required, and something has been logged.
*        <0: Logging is required, but nothing has yet been logged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     21-MAR-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL XSCN( BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      LOGICAL SCNDIR
      INTEGER NDISP
      REAL OFFSET( NDISP )
      INTEGER DET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      INTEGER BAND
      INTEGER IDC
      CHARACTER PDET*(*)
      CHARACTER PPOSN*(*)
      CHARACTER PSTRTH*(*)
      CHARACTER PPRF*(*)
      CHARACTER PODET*(*)
      CHARACTER POSCN*(*)
      CHARACTER POSKY*(*)
      CHARACTER POSTR*(*)
      CHARACTER COORDS*(*)
      CHARACTER UNITS*(*)
      INTEGER FID
      LOGICAL COLOUR
      LOGICAL CURSOR
      LOGICAL CLRBLK

*  Arguments Given and Returned:
      INTEGER LOG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! True if two strings are equal apart
                                 ! from case.

*  Local Constants:
      REAL ASPCT                 ! Aspect ratio of the tect string
      PARAMETER ( ASPCT = 0.667 )
      INTEGER NDIMX              ! Max. dimension of input NDF file
      PARAMETER ( NDIMX = 2 )
      REAL TEXTHT                ! Character height of a string
      PARAMETER ( TEXTHT = 0.02 )

*  Local Variables:
      CHARACTER A1UNIT*11        ! Units of first axis centre values.
      CHARACTER DTST*4           ! String holding detector number
      CHARACTER INST*20          ! String holding in-scan distance
      CHARACTER NDFRST*5         ! String holding NDF row number.
      CHARACTER SAMPST*5         ! String holding sample number.
      CHARACTER SKST*( 2 * IRA__SZFSC + 2 ) ! String holding sky
                                 ! coordinates
      CHARACTER SPST*120         ! Sky position entry string in log file
      CHARACTER SSTR*120         ! Peak value string write to log file
      CHARACTER VLST*20          ! String holding data value at cloest
                                 ! smaple
      CHARACTER XSKY*( IRA__SZFSC ) ! String holding frist sky coord.
      CHARACTER XST*20           ! String holding X-scan distance
      CHARACTER YSKY*( IRA__SZFSC ) ! String holding second sky coord.


      DOUBLE PRECISION ANGLE     ! Scan angle at cloest sample
      DOUBLE PRECISION SKY( 2 )  ! Sky coordinates of the cloest sample
      DOUBLE PRECISION SMPDEC    ! DEC of the closet sample
      DOUBLE PRECISION SMPRA     ! RA of the closet sample
      DOUBLE PRECISION XOUT      ! Sky longitude in specified coord.
                                 ! system
      DOUBLE PRECISION YOUT      ! Sky latitude in specified coord.
                                 ! system


      INTEGER AEL                ! Number of elements of axis array of
                                 ! NDF
      INTEGER APNTR              ! Pointer to the axis array of the NDF
      INTEGER BG                 ! Begin index of non-zero sample of
                                 ! point source template
      INTEGER CODSLN             ! Used length of the COORDS
      INTEGER DEL                ! Number of elements of data array of
                                 ! NDF
      INTEGER DETNO              ! Selected detector number
      INTEGER DIM( 2 )           ! Size of each dimension of input NDF
      INTEGER DPNTR              ! Pointer to the data array of the NDF
      INTEGER DTSTLN             ! Used length of DTST
      INTEGER ED                 ! End index of non-zero sample of
                                 ! point source template
      INTEGER I                  ! Do loop index
      INTEGER INDEX1             ! Detector index
      INTEGER INDEX2             ! Detector index
      INTEGER INDF               ! Identifier of point source template
                                 ! NDF.
      INTEGER INSTLN             ! Used length of INST
      INTEGER LINNO              ! Line number to use of the NDF data
                                 ! array
      INTEGER NDFRLN             ! Used length of NDFRST
      INTEGER NDIM               ! Dimension of input NDF file
      INTEGER NERSMP             ! Sample number nearest to specified
                                 ! point
      INTEGER NERTRC             ! Trace number nearest to specified
                                 ! point
      INTEGER NKEY               ! Key number pressed when using cursor
      INTEGER PEN                ! Pen number used to draw point source
      INTEGER SAMPLN             ! Used length of SAMPST
      INTEGER SPSTLN             ! Used length of SPST
      INTEGER SSTRLN             ! Used length of SSTR
      INTEGER UNTLN              ! Used length of the UNITS
      INTEGER VLSTLN             ! Used length of the VLST
      INTEGER XSKYLN             ! Used length of XSKY
      INTEGER XSTLN              ! Uset length of the XST
      INTEGER YSKYLN             ! Used length of YSKY


      LOGICAL EXIT               ! Exit drawing loop flag.
      LOGICAL FOUND              ! Flag for found the nearest trace.
      LOGICAL GOT                ! Got valid detector number flag
      LOGICAL THERE              ! True if an object exists.


      REAL CONST                 ! Constant of linear back ground
      REAL DIST                  ! Distance between first sample and
                                 ! specified point in arcmin
      REAL SCAN( 2 )             ! In- and X- scan dist. of closet
                                 ! sample
      REAL SLOPE                 ! The slope of the linear background
      REAL SMPR                  ! Real sample number nearest to
                                 ! specified point
      REAL SPEED                 ! Scan speed at the closet sample
      REAL STRTH                 ! Peak value of the point source
      REAL UX                    ! User X coordinate of a point
      REAL UY                    ! User Y coordinate of a point
      REAL XPOSN                 ! In scan position of specified point
      REAL YDET                  ! Y position of output detector string
      REAL YROW                  ! Y position of output NDF row number.
      REAL YSAMP                 ! Y position of output sample number.
      REAL YSCN                  ! Y position of output scan distance
                                 ! string
      REAL YSCO                  ! Y position of output sky coord.
                                 ! string
      REAL YVAL                  ! Y position of output data value
                                 ! string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the point source template NDF file from the environment.
      CALL NDF_ASSOC( PPRF, 'READ', INDF, STATUS )

*  Get the shape of the input NDF file
      CALL NDF_DIM( INDF, NDIMX, DIM, NDIM, STATUS )

*  If the size of second dimension is not 1 or 4, set the status, report
*  the error and exit.
      IF ( DIM( 2 ) .NE. 1 .AND. DIM( 2 ) .NE. 4 .AND.
     :     STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB0_ERR1',
     :   'TRACB0: NDF containing point source profiles has incorrect '//
     :   'dimensions.', STATUS )
         GO TO 999
      END IF

*  Check that the AXIS CENTRE array exists for the first dimension.
      CALL NDF_ASTAT( INDF, 'CENTRE', 1, THERE, STATUS )
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB0_ERR2',
     :   'TRACB0: NDF containing point source profiles has no AXIS '//
     :   'CENTRE values for dimension 1.', STATUS )
         GO TO 999
      END IF

*  Check that AXIS UNITS exists for the first dimension.
      CALL NDF_ASTAT( INDF, 'UNITS', 1, THERE, STATUS )
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB0_ERR3',
     :   'TRACB0: NDF containing point source profiles has no AXIS '//
     :   'UNITS value for dimension 1.', STATUS )
         GO TO 999
      END IF

*  Check that AXIS CENTRE values are in arc-minutes.
      CALL NDF_ACGET( INDF, 'UNITS', 1, A1UNIT, STATUS )
      IF( .NOT. CHR_SIMLR( A1UNIT( : 7 ), 'ARC-MIN') .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U', A1UNIT )
         CALL ERR_REP( 'TRACB0_ERR4',
     :      'TRACB0: Units of first dimension axis coordinates ("^U")'//
     :      ' are wrong (should be arc-minutes).', STATUS )
         GO TO 999
      END IF

*  Map the data array and CENTRE array of the first axis of the NDF
*  file for reading.
      CALL NDF_MAP( INDF, 'Data', '_REAL', 'READ', DPNTR, DEL, STATUS )
      CALL NDF_AMAP( INDF, 'Centre', 1, '_REAL', 'READ', APNTR, AEL,
     :               STATUS )

*  If the file contains only one profile, use it for all wave band.
      IF ( DIM( 2 ) .EQ. 1 ) THEN
         LINNO = 1

*  If the file contains four profiles, use one for each wave band.
      ELSE
         LINNO = BAND
      END IF

*  Find the indices of the first and last non-zero values in the
*   profile.
      CALL TRACB4( DIM( 1 ), DIM( 2 ), %VAL( DPNTR ), LINNO, BG, ED,
     :            STATUS )

*  If too few non-zero values found, report an error.
      IF ( ED - BG .LT. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB0_ERR5',
     :   'TRACB0: Too few non-zero samples in the point source '//
     :   'profile', STATUS )
         GO TO 999
      END IF

*  Set SGS attributes.
      CALL SGS_SHTX( TEXTHT )
      CALL SGS_SARTX( ASPCT )
      CALL SGS_STXJ( 'BC' )

*  If cursor is available, enable it and display instruction messages.
      IF ( CURSOR ) THEN
         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_TX( 0.55, -0.17,
     :              'Position cursor at source peak and press any key' )
         CALL SGS_TX( 0.55, -0.17 - 1.5 * TEXTHT,
     :              '(Position cursor outside display window to quit)' )

*  Initalise the position of the cursor.
         UX = 0.5 * ( XLMT( 1 ) + XLMT( 2 ) )
         UY = 0.5 * ( YLMT( 1 ) + YLMT( 2 ) )
      END IF

*  Display the title of the output items.
      CALL SGS_STXJ( 'CR' )
      YSAMP = -0.07
      CALL SGS_TX( -0.25, YSAMP, 'Sample number:' )

      YROW = YSAMP - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YROW, 'NDF row number:' )

      YDET = YROW - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YDET, ' Detector number:' )

      YVAL = YDET - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YVAL, '      Peak value:' )

      YSCN = YVAL - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YSCN, 'In-scan distance:' )

      YSCO = YSCN - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YSCO, '    Sky position:' )

*  Flush out the messages.
      CALL SGS_FLUSH

*  Use pen 2 to draw point source profile
      PEN = 2

*  If colour is available on the graphic device, the pen 2 will have the
*  different colour with other pen, set the line type as solid.
      IF ( COLOUR ) CALL IRM_SOLIN( STATUS )

*  Get the used length of UNITS and COORDS for writing logging file.
      UNTLN = CHR_LEN( UNITS )
      CODSLN = CHR_LEN( COORDS )

*  Enter a loop to draw point source template repeatly, until cursor is
*  outside the grid window, or after overlaying a point source template
*  via keyboard.
      EXIT = .FALSE.
      DO WHILE( .NOT.EXIT .AND. STATUS .EQ. SAI__OK )

*  If cursor is not available or the device can not be cleared partly,
*  use keyboard.
         IF ( .NOT. CURSOR ) THEN

*  Enter a do loop until get a  valid detector number.
            GOT = .FALSE.
            DO WHILE( .NOT.GOT .AND. STATUS .EQ. SAI__OK )

*  Get a detector number from the environment, and cancel its value
*  afterward.
               CALL PAR_GET0I( PDET, DETNO, STATUS )
               CALL PAR_CANCL( PDET, STATUS )

*  Check whether the detector given is among those in display, if so
*  note down the trace index of the detector.
               DO I = 1, NDISP
                  IF ( DETNO .EQ. DET( I ) ) THEN
                     GOT = .TRUE.
                     NERTRC = I
                  END IF
               END DO

*  If the detector given is not among the displayed, report an error.
               IF ( .NOT. GOT .AND. STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__OK
                  CALL MSG_SETI( 'D', DETNO )
                  CALL ERR_REP( 'TRACB0_ERR6',
     :                          'TRACB0: Detector #^D is not displayed',
     :                          STATUS )

*  Flush the error.
                  CALL ERR_FLUSH( STATUS )

               END IF

*  Go back to get a new value if no valid detector is got.
            END DO

*  Enter a do loop to get a valid in-scan distance specification.
            GOT = .FALSE.
            DO WHILE( .NOT.GOT )

*  Get the in-scan position of the point source, and cancel the
*  parameter value afterward so that the parameter can be re-used.
               CALL PAR_GDR0R( PPOSN, VAL__BADR, XLMT( 1 ), XLMT( 2 ),
     :                        .FALSE., XPOSN, STATUS )
               CALL PAR_CANCL( PPOSN, STATUS )

*  Get the in-scan distance of the specified point to the first
*  sample of the scan. Convert it to radians.
               DIST = INSCN( BSMP, DTINDX( NERTRC ) ) - XPOSN
               DIST = REAL( IRA__DTOR ) * DIST / 60.0

*  Get the sample index of the specified position.
               INDEX1 = DTINDX( NERTRC )
               INDEX2 = DTINDX( NERTRC )
               CALL IRC_OFFST( IDC, REAL( BSMP ), INDEX1, INDEX2,
     :                         DIST, SMPR, STATUS )
               NERSMP = NINT( SMPR )

*  If NERSMP is in the trace segment, set GOT flag.
               IF ( NERSMP .GE. BSMP .AND. NERSMP .LE. ESMP ) THEN
                  GOT = .TRUE.

*  Otherwise report an error.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'TRACB0_ERR7',
     :          'TRACB0: The specified in-scan position cannot be used',
     :                         STATUS )
                  CALL ERR_FLUSH( STATUS )
               END IF

*  Go back to get position specification if no valid one is got.
            END DO

*  Get the peak value of the point source, and cancel the parameter
*  value afterward.
            CALL PAR_GDR0R( PSTRTH, VAL__BADR, 0.0,
     :                      YLMT( 2 ) - YLMT( 1 ), .FALSE., STRTH,
     :                      STATUS )
            CALL PAR_CANCL( PSTRTH, STATUS )

*  Now all parameters have been got correctly.
            FOUND = .TRUE.

*  If the cursor is available, using cursor to specified a position
*  in the display.
         ELSE

*  Get the cursor position.
            CALL SNX_CURS( UX, UY, NKEY )

*  If cursor is inside the grid window, find the trace index and sample
*  index which is closest to the cursor.
            IF ( UX .GE. XLMT( 1 ) .AND. UX .LE. XLMT( 2 ) .AND.
     :           UY .GE. YLMT( 1 ) .AND. UY .LE. YLMT( 2 ) ) THEN

               CALL TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                      NDISP, OFFSET, DTINDX, SCALE, IDC,
     :                      UX, UY, NERTRC, NERSMP, FOUND, STATUS )

*  Else, set flag to exit the do loop.
            ELSE
               EXIT = .TRUE.
            END IF
         END IF

*  If not exit and a nearest trace is found, going on to draw the point
*  source template.
         IF ( .NOT.EXIT .AND. FOUND ) THEN
            DETNO = DET( NERTRC )

*  Calculate the linear background at the position.
            CALL TRACB5( BSMP, ESMP, INSCN( BSMP, DTINDX( NERTRC ) ),
     :                   DETDAT( BSMP, DTINDX( NERTRC ) ),
     :                   SCALE( NERTRC), NERSMP, ED, BG, AEL,
     :                   %VAL( APNTR ), SLOPE, CONST, STATUS )

*  Calculate the Peak value of the point source.
            IF ( CURSOR ) STRTH = UY - CONST - OFFSET( NERTRC )

*  If calculated Peak value is less than 0, set it as 0.
            IF ( STRTH .LT. 0.0 ) STRTH = 0.0

*  Draw the point source template.
            CALL TRACB6( DIM( 1 ), DIM( 2 ), %VAL( APNTR ),
     :                   %VAL( DPNTR ), LINNO, BG, ED, XLMT, YLMT,
     :                   UX, OFFSET( NERTRC ), CONST, SLOPE, STRTH,
     :                   SCNDIR, PEN, STATUS )

*  Encoding detector number and point source Peak value into character
*  strings.
            CALL CHR_ITOC( DETNO, DTST( 2 : ), DTSTLN )
            CALL CHR_RTOC( STRTH, VLST, VLSTLN )
            DTST( 1 : 1 ) = '#'
            DTSTLN = DTSTLN + 1

*  Encode the sample number and NDF row number into character
*  strings.
            CALL CHR_ITOC( NERSMP, SAMPST, SAMPLN )
            CALL CHR_ITOC( DTINDX( NERTRC ), NDFRST, NDFRLN )

*  Get the in-scan and cross-scan distances of the position of the point
*  source.
            SCAN( 1 ) = INSCN( NERSMP, DTINDX( NERTRC ) )
            SCAN( 2 ) = XSCN( DTINDX( NERTRC ) )

*  Encoding them into character strings.
            CALL CHR_RTOC( SCAN( 1 ), INST, INSTLN )
            CALL CHR_RTOC( SCAN( 2 ), XST, XSTLN )

*  Get the sky coordinate ( B1950 FK4 ) of the sample.
            CALL IRC_DPOS( IDC, 1, REAL( NERSMP ), DTINDX( NERTRC ),
     :                     SMPRA, SMPDEC, ANGLE, SPEED, STATUS )

*  Convert the specified sky coordinate to the specified sky coordinate
*  system.
            CALL IRA_CONVT( 1, SMPRA, SMPDEC, 'EQUATORIAL(1950)',
     :                      COORDS, IRA__IRJEP, XOUT, YOUT, STATUS )
            SKY( 1 ) = XOUT
            SKY( 2 ) = YOUT

*  Encode the sky coordinates into character string.
            CALL IRA_DTOC( SKY( 1 ), SKY( 2 ), COORDS, 1, XSKY,
     :                     YSKY, STATUS )
            XSKYLN = CHR_LEN( XSKY )
            YSKYLN = CHR_LEN( YSKY )

*  Appending them into one string
            SKST = XSKY( : XSKYLN )//', '//YSKY( : YSKYLN )


*  Clear the region on the graphic device to write the new obtained
*  data.
            IF ( CLRBLK ) THEN
               CALL SGS_CLRBL( -0.23, 0.1, YDET + TEXTHT,
     :                            YSCO - 4.0 * TEXTHT )
               CALL SGS_CLRBL( -0.23, -0.1, YSAMP + TEXTHT,
     :                            YROW - TEXTHT )
            END IF

*  Set justification of the output string.
            CALL SGS_STXJ( 'CL' )

*  Write the data to the graphic device.
            CALL SGS_TX( -0.23, YSAMP, SAMPST( : SAMPLN ) )
            CALL SGS_TX( -0.23, YROW, NDFRST( : NDFRLN ) )
            CALL SGS_TX( -0.23, YDET, DTST( : DTSTLN ) )
            CALL SGS_TX( -0.23, YVAL, VLST( : VLSTLN ) )
            CALL SGS_TX( -0.23, YSCN, INST( : INSTLN ) )
            CALL TRACC1( XOUT, YOUT, -0.23, YSCO, COORDS, STATUS )
            CALL SGS_FLUSH

*  If logging is required, write the data to the logging file.
            IF ( LOG .NE. 0 ) THEN
               CALL FIO_WRITE( FID, ' ', STATUS )
               CALL FIO_WRITE( FID,
     :                        'Sample number   : '//SAMPST, STATUS )
               CALL FIO_WRITE( FID,
     :                        'NDF row number  : '//NDFRST, STATUS )
               CALL FIO_WRITE( FID,
     :                        'Detector number : '//DTST, STATUS )

               SSTR = 'Peak value      : '//VLST( : VLSTLN )//' ('/
     :                /UNITS( : UNTLN )//')'
               SSTRLN = CHR_LEN( SSTR )
               CALL FIO_WRITE( FID, SSTR( : SSTRLN ), STATUS )

               CALL FIO_WRITE( FID,
     :                        'In-scan distance: '//INST( : INSTLN ) /
     :                       /' (arc-minutes)', STATUS )

               CALL FIO_WRITE( FID,
     :                        'X-scan distance : '//XST( : XSTLN ) /
     :                       /' (arc-minutes)', STATUS )

               SPST = 'Sky position    : '//XSKY( : XSKYLN )//' ('/
     :                /COORDS( : CODSLN )//')'
               SPSTLN = MIN( 80, CHR_LEN( SPST ) )
               CALL FIO_WRITE( FID, SPST( :SPSTLN ), STATUS )

               SPST = '                  '//YSKY( : YSKYLN )
               SPSTLN = MIN( 80, CHR_LEN( SPST ) )
               CALL FIO_WRITE( FID, SPST( :SPSTLN ), STATUS )

*  Set the flag to show something have been logged in the log file.
               LOG = 1
            END IF

*  If no cursor available or graphic surface can not be cleared partly,
*  exit the do loop after first drawing.
            IF ( .NOT.CURSOR .OR. .NOT.CLRBLK ) EXIT = .TRUE.

         END IF

*  Go back to draw another point source template if not exit.
      END DO

*  Write the final data to the environment.
      CALL PAR_PUT0I( PODET, DETNO, STATUS )
      CALL PAR_PUT0R( POSTR, STRTH, STATUS )
      CALL PAR_PUT1R( POSCN, 2, SCAN, STATUS )
      CALL PAR_PUT1D( POSKY, 2, SKY, STATUS )

 999  CONTINUE

*  Recover the line type setting, if it was previously set as solid.
      IF ( COLOUR ) CALL IRM_ANTSO( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
