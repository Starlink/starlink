      SUBROUTINE TRACC4( BSMP, ESMP, BDET, EDET, INSCN, DETDAT, NDISP,
     :                   OFFSET, DTINDX, SCALE, XLMT, YLMT, IDC, COLOUR,
     :                   CURSOR, CLRBLK, SCNDIR, PEN1, PEN2, INIT, MASK,
     :                   STATUS )
*+
*  Name:
*     TRACC4

*  Purpose:
*     Select samples falling in a box on the display using the cursor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACC4( BSMP, ESMP, BDET, EDET, INSCN, DETDAT, NDISP,
*                  OFFSET, DTINDX, SCALE, XLMT, YLMT, IDC, COLOUR,
*                  CURSOR, CLRBLK, SCNDIR, PEN1, PEN2, INIT, MASK,
*                  STATUS )

*  Description:
*     If the graphic device does not have a cursor, or is not block
*     clearable, the routine will just report a message and return.
*     Otherwise, the user is asked to mark two diagonally opposite
*     corners of a box on the data window.  If either are outside the
*     data window, the routine quits.  Otherwise, the traces and
*     samples which are closest to the cursor are found and indicated.
*     The samples which fall within the box are flagged by bad values
*     in the returned mask, and all such samples are highlighted on the
*     display by being redrawn using pen 3.
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
*     COLOUR = LOGICAL (Given)
*        If true, colour is available on the graphic device.
*     CURSOR = LOGICAL (Given)
*        If true, cursor is available on the graphic device.
*     CLRBLK = LOGICAL (Given)
*        If true, graphic device can clear part of its display surface.
*     SCNDIR = LOGICAL (Given)
*        If true, the scan is from north to south. Otherwise from south
*        to north.
*     PEN1 = INTEGER (Given)
*        The SGS pen number with which to draw two crosses which mark
*        the positions of the samples closest to the cursor positions
*        indicated by the user.
*     PEN2 = INTEGER (Given)
*        The SGS pen number with which to re-draw the section of the
*        data curves which correspond to the selected data.
*     INIT = LOGICAL (Given)
*        Determines if the supplied mask should be initialised by being
*        filled with zeros before being used.
*     MASK( BSMP : ESMP, BDET : EDET ) = REAL (Given and Returned)
*        A mask indicating the selected samples; those which fall
*        within the selected box are returned equal to VAL__BADR. The
*        others are returned unchanged if INIT = .FALSE., and are
*        returned equal to zero if INIT = .TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      REAL OFFSET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      INTEGER IDC
      LOGICAL COLOUR
      LOGICAL CURSOR
      LOGICAL CLRBLK
      LOGICAL SCNDIR
      INTEGER PEN1
      INTEGER PEN2
      LOGICAL INIT

*  Arguments Given and Returned:
      REAL MASK( BSMP : ESMP, BDET : EDET )

*  Status:
      INTEGER STATUS             ! Global status

*  External Referance:
      REAL SNX_AGUGX             ! Autograph user X to grid X conversion.
      REAL SNX_AGUGY             ! Autograph user Y to grid Y conversion.

*  Local Constants:
      REAL ASPCT                 ! Aspect ratio of the text string
      PARAMETER ( ASPCT = 0.667 )

      REAL TEXTHT                ! Character height of a string
      PARAMETER ( TEXTHT = 0.02 )

*  Local variables:
      INTEGER DET                ! Detector index of current trace.
      INTEGER DET1               ! Detector index corresponding to
                                 ! TRACE1.
      INTEGER DET2               ! Detector index corresponding to
                                 ! TRACE2.
      INTEGER HISAMP             ! Upper sample number limit.
      INTEGER LOSAMP             ! Lower sample number limit.
      INTEGER NKEY               ! Key number pressed when using cursor
      INTEGER OLDPEN             ! Original SGS pen number.
      INTEGER SAMP               ! The current sample numbner.
      INTEGER SAMP1              ! The sample number at the first
                                 ! corner.
      INTEGER SAMP2              ! The sample number at the second
                                 ! corner.
      INTEGER TRACE              ! The current trace number.
      INTEGER TRACE1             ! The trace containing the first
                                 ! corner.
      INTEGER TRACE2             ! The trace containing the second
                                 ! corner.
      INTEGER XORDER             ! Integer version of SCNDIR.


      LOGICAL FOUND              ! Flag if the nearest trace was found.


      REAL DY                    ! In-scan offset from 1st to 2nd corner
      REAL RSAMP1                ! Start sample.
      REAL RSAMP2                ! End sample.
      REAL SGSX1                 ! SGS X coordinate of first corner.
      REAL SGSX2                 ! SGS X coordinate of first corner.
      REAL SGSY1                 ! SGS Y coordinate of first corner.
      REAL SGSY2                 ! SGS Y coordinate of first corner.
      REAL UX1                   ! NCAR user X coordinate of first corner.
      REAL UX2                   ! NCAR user X coordinate of first corner.
      REAL UY1                   ! NCAR user Y coordinate of first corner.
      REAL UY2                   ! NCAR user Y coordinate of first corner.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If cursor is not available on the graphic device, report an error.
      IF ( .NOT.CURSOR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACC4_ERR1',
     :       'TRACC4: No graphics cursor available on this workstation',
     :                  STATUS )
         GO TO 999
      END IF

*  If block clear is not available, report an error.
      IF ( .NOT. CLRBLK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACC4_ERR2',
     :  'TRACC4: No block clear facility available on this workstation',
     :                  STATUS )
         GO TO 999
      END IF

*  Write instruction messages to the graphic device.
      CALL SGS_SHTX( TEXTHT )
      CALL SGS_SARTX( ASPCT )
      CALL SGS_STXJ( 'BC' )
      CALL SGS_TX( 0.5, -0.17, 'Position the cursor at a corner of the')
      CALL SGS_TX( 0.5, -0.17 - 1.5 * TEXTHT,
     :            'box containing the required data and press any key' )

*  Flush out the messages.
      CALL SGS_FLUSH

*  Enable the cursor.
      CALL SGS_CUVIS( .TRUE. )

*  Initially put the cursor at the centre of the grid window
      UX1 = 0.5 * ( XLMT( 1 ) + XLMT( 2 ) )
      UY1 = 0.5 * ( YLMT( 1 ) + YLMT( 2 ) )

*  Get cursor position.
      CALL SNX_CURS( UX1, UY1, NKEY )

*  If the cursor is outside the grid window, abort.
      IF ( UX1 .LT. XLMT( 1 ) .OR. UX1 .GT. XLMT( 2 ) .OR.
     :     UY1 .LT. YLMT( 1 ) .OR. UY1 .GT. YLMT( 2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACC4_ERR3',
     :                 'TRACC4: Cursor outside data area',
     :                 STATUS )
         GO TO 999
      END IF

*  Find the trace index and sample index which is closest to the cursor.
      CALL TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT, NDISP, OFFSET,
     :             DTINDX, SCALE, IDC, UX1, UY1, TRACE1, SAMP1, FOUND,
     :             STATUS )

*  If the trace was not found, or an error occurred, quit.
      IF ( .NOT. FOUND .OR. STATUS .NE. SAI__OK ) GO TO 999

*  Get the detector index of the nearest sample.
      DET1 = DTINDX( TRACE1 )

*  Get the world coordinates of the closest sample.
      UX1 = INSCN( SAMP1, DET1 )
      UY1 = SCALE( TRACE1 )*DETDAT( SAMP1, DET1 ) + OFFSET( TRACE1 )

*  Put a cross there using the supplied pen.
      CALL SGS_IPEN( OLDPEN )
      CALL SGS_SPEN( PEN1 )
      SGSX1 = SNX_AGUGX( UX1 )
      SGSY1 = SNX_AGUGY( UY1 )
      CALL SGS_MARK( SGSX1, SGSY1, 2 )
      CALL SGS_SPEN( OLDPEN )

*  Clear the area used to display instructions.
      CALL SGS_CLRBL( -0.55, 1.2, -0.5, -0.13 )

*  Write instruction messages to the graphic device, asking the user
*  to specify the second corner.
      CALL SGS_TX( 0.5, -0.15, 'Position the cursor at the diagonally')
      CALL SGS_TX( 0.5, -0.15 - 1.5 * TEXTHT,
     :             'opposite corner and press any key' )

*  Flush out the messages.
      CALL SGS_FLUSH

*  Enable the cursor.
      CALL SGS_CUVIS( .TRUE. )

*  Get cursor position.
      UX2 = UX1
      UY2 = UY1
      CALL SNX_CURS( UX2, UY2, NKEY )

*  If the cursor is outside the grid window, abort.
      IF ( UX2 .LT. XLMT( 1 ) .OR. UX2 .GT. XLMT( 2 ) .OR.
     :     UY2 .LT. YLMT( 1 ) .OR. UY2 .GT. YLMT( 2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACC4_ERR4',
     :                 'TRACC4: Cursor outside data area',
     :                 STATUS )
         GO TO 999
      END IF

*  Find the trace index and sample index which is closest to the cursor.
      CALL TRACB3( BSMP, ESMP, BDET, EDET, INSCN, DETDAT, NDISP, OFFSET,
     :             DTINDX, SCALE, IDC, UX2, UY2, TRACE2, SAMP2, FOUND,
     :             STATUS )

*  If the trace was not found, or an error occurred, quit.
      IF ( .NOT. FOUND .OR. STATUS .NE. SAI__OK ) GO TO 999

*  Get the detector index of the nearest sample.
      DET2 = DTINDX( TRACE2 )

*  Get the world coordinates of the closest sample.
      UX2 = INSCN( SAMP2, DET2 )
      UY2 = SCALE( TRACE2 )*DETDAT( SAMP2, DET2 ) + OFFSET( TRACE2 )

*  Put a cross there using the supplied pen.
      CALL SGS_IPEN( OLDPEN )
      CALL SGS_SPEN( PEN1 )
      SGSX2 = SNX_AGUGX( UX2 )
      SGSY2 = SNX_AGUGY( UY2 )
      CALL SGS_MARK( SGSX2, SGSY2, 2 )
      CALL SGS_SPEN( OLDPEN )

*  Clear the area used to display instructions.
      CALL SGS_CLRBL( -0.55, 1.2, -0.5, -0.13 )

*  Get the in-scan offset from the first to the second corner, in
*  radians.
      DY = IRA__AM2R*( UX2 - UX1 )

*  If scan direction is from north to south, set x mapping order such
*  that x is increasing from right to left in the display.
      IF ( SCNDIR ) THEN
         XORDER = 1

*  If scan direction is from south to north, set x mapping order such
*  that x is increasing from left to right in the display.
      ELSE
         XORDER = 0
      END IF

*  If required, initialise all elements of the mask to zero.
      IF( INIT ) THEN

         DO DET = BDET, EDET
            DO SAMP = BSMP, ESMP
               MASK( SAMP, DET ) = 0.0
            END DO
         END DO

      END IF

*  Loop round each data trace within the selected range.
      DO TRACE = MAX( 1, TRACE1), MIN( NDISP, TRACE2 )

*  Get the detector index for this trace.
         DET = DTINDX( TRACE )

*  Find the sample from this detector which is at zero in-scan offset
*  from the starting sample.
         CALL IRC_OFFST( IDC, REAL( SAMP1 ), DET1, DET, 0.0, RSAMP1,
     :                   STATUS )

*  Find the sample from this detector which is at the maximum required
*  in-scan offset from the starting sample.
         CALL IRC_OFFST( IDC, REAL( SAMP1 ), DET1, DET, -DY, RSAMP2,
     :                   STATUS )

*  Find the lower and upper integer sample limits.
         LOSAMP = MAX( BSMP, MIN( NINT( RSAMP1 ), NINT( RSAMP2 ) ) )
         HISAMP = MIN( ESMP, MAX( NINT( RSAMP1 ), NINT( RSAMP2 ) ) )

*  Redraw the curve through the selected samples using the supplied pen.
         CALL TRACB9( BSMP, ESMP, INSCN( BSMP, DET ),
     :                DETDAT( BSMP, DET ), LOSAMP, HISAMP, 0.0,
     :                OFFSET( TRACE ), SCALE( TRACE ), 0.0, XLMT, YLMT,
     :                XORDER, 0, PEN2, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Select all the samples between the starting and ending samples.
         DO SAMP = LOSAMP, HISAMP
            MASK( SAMP, DET ) = VAL__BADR
         END DO

      END DO

*  Flush any outstanding graphics.
      CALL SGS_FLUSH

*  If an error occurred.
 999  CONTINUE

      END
