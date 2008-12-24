      SUBROUTINE TRACA9( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT, 
     :                   NDISP, OFFSET, DTINDX, SCALE, XLMT, 
     :                   YLMT, IDC, PDET, PVAL, PSCN, PSKY, COORDS, 
     :                   UNITS, FID, CURSOR, CLRBLK, LOG, STATUS )
*+
*  Name:
*     TRACA9

*  Purpose:
*     Get the data value of the nearest trace and sample to the cursor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA9( BSMP, ESMP, BDET, EDET, INSCN, XSCN, DETDAT, 
*                  NDISP, OFFSET, DTINDX, SCALE, XLMT, YLMT, 
*                  IDC, PDET, PVAL, PSCN, PSKY, COORD, UNITS, FID, 
*                  CURSOR, CLRBLK, STATUS )

*  Description:
*     If the graphic device does not have a cursor, the routine will
*     just report a message and return.  If the graphic device has a
*     cursor, the routine will repeatedly ask for the cursor positions
*     until the cursor position is outside the NCAR (AUTOGRAPH) grid
*     window. Once a position of the cursor is obtained, the routine
*     will find the trace and sample in the display which is cloest to
*     the cursor. The detector number of that trace, the data value at
*     that sample in the given units, the in- and cross- scan
*     positions, and the specified sky coordinates of that sample point
*     are output to the graphic device and the enirvonment. If
*     required, these data will also be written into the log file.
*
*     Before calling this routine the AUTOGRAPH grid window should be
*     made to match the current SGS zone world coordinates, that is the
*     grid window should have the bounds (0.0, 1.0, 0.0, 1.0 ).

*  Arguments:
*     BSMP = INTEGER (Given)
*        Begin index of the samples of the input CRDD data array.
*     ESMP = INTEGER (Given)
*        End index of the samples of the input CRDD data array.
*     BDET = INTEGER (Given)
*        Begin index of the detector of the input CRDD data array.
*     EDET = INTEGER (Given)
*        End index of the detector of the input CRDD data array.  
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample of each CRDD data trace.
*     XSCN( BDET : EDET ) = REAL (Given)
*        The cross-scan distance of each data trace in input CRDD file.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The unscaled data value of each CRDD trace.
*     NDISP = INTEGER (Given)
*        The number of the displayed data trace.
*     OFFSET( NDISP ) = REAL (Given)
*        The offset of each trace in the display.
*     DTINDX( NDISP ) = INTEGER (Given)
*        The detector index of each displayed trace.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce the scaled data in the display.
*     XLMT( 2 ) = REAL (Given)
*        The in-scan limits of the display.
*     YLMT( 2 ) = REAL (Given)
*        The vertical limits of the display.
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     PDET = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the detector number of
*        the trace which has a sample cloest to the cursor.
*     PVAL = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the data value of the
*        trace at the sample cloest to the cursor.
*     PSCN = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the in- and cross-scan
*        distances of the cloest sample to the reference position.
*     PSKY = CHARACTER * ( * ) (Given)
*        The name of the parameter used to write the sky coordinates of
*        the cloest sample.
*     COORDS = CHARACTER * ( * ) (Given)
*        The specified sky coordinate system, in which the the sky
*        coordinates of the closest sample will output.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the data value.
*     FID = INTEGER (Given)
*        Log file descriptor.
*     CURSOR = LOGICAL (Given)
*        If true, cursor is available on the graphic device.
*     CLRBLK = LOGICAL (Given)
*        If true, graphic device can clear part of its display surface.
*     LOG = INTEGER (Given and Returned)
*        =0: Logging is not required.
*        >0: Logging is required and something has been logged.
*        <0: Logging is required, but nothing has yet been logged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     7-MAR-1991 (WG):
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
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL XSCN( BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      REAL OFFSET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      INTEGER IDC
      CHARACTER PDET*(*) 
      CHARACTER PVAL*(*) 
      CHARACTER PSCN*(*) 
      CHARACTER PSKY*(*) 
      CHARACTER COORDS*(*) 
      CHARACTER UNITS*(*) 
      INTEGER FID
      LOGICAL CURSOR
      LOGICAL CLRBLK

*  Arguments Given and Returned:
      INTEGER LOG

*  Status:
      INTEGER STATUS             ! Global status

*  External Referance:
      INTEGER CHR_LEN            ! Used length of a string
      INTEGER IRC_DETNO          ! Returnes the detector number from index

*  Local Constants:
      REAL ASPCT                 ! Aspect ratio of the text string
      PARAMETER ( ASPCT = 0.667 )
      REAL TEXTHT                ! Character height of a string
      PARAMETER ( TEXTHT = 0.02 )

*  Local variables:
      CHARACTER DTST*4           ! String holding detector number
      CHARACTER DVST*120         ! Data value entry string in log file
      CHARACTER INST*20          ! String holding in-scan distance
      CHARACTER NDFRST*5         ! String holding NDF row number.
      CHARACTER SAMPST*5         ! String holding sample number.
      CHARACTER SKST*( 2 * IRA__SZFSC + 2 ) ! String holding sky
                                 ! coordinates
      CHARACTER SPST*120         ! Sky position entry string in log file
      CHARACTER VLST*20          ! String holding data value at closest
                                 ! sample
      CHARACTER XSKY*( IRA__SZFSC ) ! String holding frist sky coord.
      CHARACTER XST*20           ! String holding X-scan distance
      CHARACTER YSKY*( IRA__SZFSC )! String holding second sky coord.


      DOUBLE PRECISION ANGLE     ! Scan angle at cloest sample
      DOUBLE PRECISION SKY( 2 )  ! Sky coordinates of the closest sample
      DOUBLE PRECISION SMPDEC    ! DEC of the closet sample
      DOUBLE PRECISION SMPRA     ! RA of the closet sample
      DOUBLE PRECISION XOUT      ! Sky longitude in specified coord.
                                 ! system
      DOUBLE PRECISION YOUT      ! Sky latitude in specified coord.
                                 ! system


      INTEGER CODSLN             ! Used length of COORDS length
      INTEGER DETNO              ! Detector number of nearset trace
      INTEGER DTSTLN             ! Used length of DTST
      INTEGER DVSTLN             ! Used length of DVST
      INTEGER INSTLN             ! Used length of INST
      INTEGER NDFRLN             ! Used length of NDFRST
      INTEGER NERSMP             ! The nearest sample to the cursor
      INTEGER NERTRC             ! The nearest trace to the cursor
      INTEGER NKEY               ! Key number pressed when using cursor
      INTEGER SAMPLN             ! Used length of SAMPST
      INTEGER SPSTLN             ! Used length of SPST
      INTEGER UNTLN              ! Used length of UNITS string
      INTEGER VLSTLN             ! Used length of the VLST
      INTEGER XSKYLN             ! Used length of XSKY
      INTEGER XSTLN              ! Uset length of the XST
      INTEGER YSKYLN             ! Used length of YSKY


      LOGICAL EXIT               ! Flag of ending the loop of get value
      LOGICAL FOUND              ! Flag of finding the nearest trace.


      REAL SCAN( 2 )             ! In- and X- scan dist. of closest
                                 ! sample
      REAL SPEED                 ! Scan speed at the closet sample
      REAL UX                    ! User X coordinate of a point
      REAL UY                    ! User Y coordinate of a point
      REAL VALUE                 ! The data value at nearest sample
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

*  If cursor is not available on the graphic device, give user a message
*  and exit the routine.
      IF ( .NOT.CURSOR ) THEN
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )
         CALL MSG_OUTIF( MSG__QUIET, 'TRACA9_MSG1',
     :                  'WARNING: No graphics cursor available',
     :                  STATUS )
         CALL MSG_BLANKIF( MSG__QUIET, STATUS )

         GOTO 999
      END IF

*  Write instruction messages to the graphic device.
      CALL SGS_SHTX( TEXTHT )
      CALL SGS_SARTX( ASPCT )
      CALL SGS_STXJ( 'BC' )
      CALL SGS_TX( 0.5, -0.17, 'Position cursor and press any key' ) 
      CALL SGS_TX( 0.5, -0.17 - 1.5 * TEXTHT, 
     :             '(Position cursor outside the data area to quit)' )

*  Write the title of the output items.
      CALL SGS_STXJ( 'CR')

      YSAMP = -0.07
      CALL SGS_TX( -0.25, YSAMP, 'Sample number:' )

      YROW = YSAMP - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YROW, 'NDF row number:' )

      YDET = YROW - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YDET, 'Detector number:' )

      YVAL = YDET - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YVAL, 'Data value:' )

      YSCN = YVAL - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YSCN, 'In-scan distance:' )

      YSCO = YSCN - 2.0 * TEXTHT
      CALL SGS_TX( -0.25, YSCO, 'Sky position:' )

*  Flush out the messages.
      CALL SGS_FLUSH
      
*  Get the used length of UNITS and COORDS for writing log file
      UNTLN = CHR_LEN( UNITS )
      CODSLN = CHR_LEN( COORDS )
  
*  Enable the cursor.
      CALL SGS_CUVIS( .TRUE. )

*  Initially put the cursor at the centre of the grid window
      UX = 0.5 * ( XLMT( 1 ) + XLMT( 2 ) )
      UY = 0.5 * ( YLMT( 1 ) + YLMT( 2 ) )
            
*  Enter a loop to get cursor position until cursor is outside grid
*  window. 
      EXIT = .FALSE.
      DO WHILE( .NOT.EXIT .AND. STATUS .EQ. SAI__OK )

*  Get cursor position.
         CALL SNX_CURS( UX, UY, NKEY )

*  If the cursor is outside the grid window, exit do loop.
         IF ( UX .LT. XLMT( 1 ) .OR. UX .GT. XLMT( 2 ) .OR.
     :        UY .LT. YLMT( 1 ) .OR. UY .GT. YLMT( 2 ) ) THEN
            EXIT = .TRUE.

*  Otherwise, find the trace index and sample index which is cloest to
*  the cursor.
         ELSE
            CALL TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT, 
     :                   NDISP, OFFSET, DTINDX, SCALE, IDC,
     :                   UX, UY, NERTRC, NERSMP, FOUND, STATUS ) 

*  If the trace and sample are found, get the detector number of the 
*  trace and the scaled data value of the sample.
            IF ( FOUND ) THEN
               DETNO = IRC_DETNO( IDC, DTINDX( NERTRC ), STATUS )
               VALUE = SCALE( NERTRC ) *
     :                 DETDAT( NERSMP, DTINDX( NERTRC ) ) 

*  Encoding them into character strings.
               CALL CHR_ITOC( DETNO, DTST( 2 : ), DTSTLN )
               CALL CHR_RTOC( VALUE, VLST, VLSTLN )
               DTST( 1 : 1 ) = '#'
               DTSTLN = DTSTLN + 1

*  Encode the sample number and NDF row number into character 
*  strings.
               CALL CHR_ITOC( NERSMP, SAMPST, SAMPLN )
               CALL CHR_ITOC( DTINDX( NERTRC ), NDFRST, NDFRLN )

*  Get the in-scan and cross-scan distances of the sample.
               SCAN( 1 ) = INSCN( NERSMP, DTINDX( NERTRC ) )
               SCAN( 2 ) = XSCN( DTINDX( NERTRC ) )     

*  Encoding them into character strings.
               CALL CHR_RTOC( SCAN( 1 ), INST, INSTLN )
               CALL CHR_RTOC( SCAN( 2 ), XST, XSTLN )

*  Get the sky coordinate ( B1950 FK4 ) of the sample.
               CALL IRC_DPOS( IDC, 1, REAL( NERSMP ), 
     :                        DTINDX( NERTRC ), SMPRA, SMPDEC, ANGLE, 
     :                        SPEED, STATUS )

*  Convert it to specified sky coordinate.
               CALL IRA_CONVT( 1, SMPRA, SMPDEC, 'EQUATORIAL(1950)', 
     :                         COORDS, IRA__IRJEP, XOUT, YOUT,  STATUS )
               SKY( 1 ) = XOUT
               SKY( 2 ) = YOUT

*  Encode the sky coordinates into character string.
               CALL IRA_DTOC( SKY( 1 ), SKY( 2 ), COORDS, 1, XSKY,
     :                        YSKY, STATUS )
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
     :                           'Sample number   : '//SAMPST, STATUS )
                  CALL FIO_WRITE( FID,
     :                           'NDF row number  : '//NDFRST, STATUS )
                  CALL FIO_WRITE( FID, 
     :                           'Detector number : '//DTST, STATUS )
      
                  DVST = 'Data value      : '//VLST( : VLSTLN )//' ('/
     :                    /UNITS( : UNTLN )//')'
                  DVSTLN = MIN( 80, CHR_LEN( DVST ) )
                  CALL FIO_WRITE( FID, DVST( : DVSTLN ), STATUS )

                  CALL FIO_WRITE( FID, 
     :                           'In-scan distance: '/
     :                           /INST( : INSTLN )//' (arc-minutes)',
     :                            STATUS )

                  CALL FIO_WRITE( FID,
     :                           'X-scan distance : '/
     :                           /XST( : XSTLN )//' (arc-minutes)',
     :                            STATUS )
      
                  SPST = 'Sky position    : '//XSKY( : XSKYLN )//' ('/
     :                   /COORDS( : CODSLN )//')'
                  SPSTLN = MIN( 80, CHR_LEN( SPST ) )
                  CALL FIO_WRITE( FID, SPST( :SPSTLN ), STATUS )

                  SPST = '                  '//YSKY( : YSKYLN )
                  SPSTLN = MIN( 80, CHR_LEN( SPST ) )
                  CALL FIO_WRITE( FID, SPST( :SPSTLN ), STATUS )

*  Set flag to show something have been logged.
                  LOG = 1
               END IF

*  Prepare to put the cursor at the closest sample.
               UX = INSCN( NERSMP, DTINDX( NERTRC ) )
               UY = VALUE + OFFSET( NERTRC )

*  If the graphic device can not be cleared partly, exit after first
*  write.
               IF ( .NOT.CLRBLK ) EXIT = .TRUE.
            END IF
         END IF
       
*  Go back to get more data values until cursor is outside the grid
*  window.
      END DO

*  Write the final data to the environment.
      IF ( FOUND ) THEN
         CALL PAR_PUT0I( PDET, DETNO, STATUS )
         CALL PAR_PUT0R( PVAL, VALUE, STATUS )
         CALL PAR_PUT1R( PSCN, 2, SCAN, STATUS )
         CALL PAR_PUT1D( PSKY, 2, SKY, STATUS )
      END IF

 999  CONTINUE

      END
