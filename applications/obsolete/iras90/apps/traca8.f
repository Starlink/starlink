      SUBROUTINE TRACA8( PARAM, PDET, PDEV, PXSZ, PYSZ, PXLMT, PYLMT,
     :                   PTITL, PUNITS, POFMD, POFF, POMIT, PQEXP,
     :                   PXSCAN, INDF, IDC, BSMP, ESMP, BDET, EDET,
     :                   INSCN, XSCN, DATA, DETDAT, DATUNT, YMX, YMN,
     :                   NAVAIL, ADET, QEXP, DETOUT, DTINDX, NDISP,
     :                   SCALE, PIC0, PIC1, ZONE, COLOUR, CURSOR,
     :                   CLRBLK, XLMT, YLMT, USECUR, TITLE, DISUNT,
     :                   OFFMTD, OFFSET, FLAGS, STATUS )
*+
*  Name:
*     TRACA8

*  Purpose:
*     Change TRACECRDD parameter values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACA8( PARAM, PDET, PDEV, PXSZ, PYSZ, PXLMT, PYLMT, PTITL,
*                  PUNITS, POFMD, POFF, POMIT, PQEXP, PXSCAN, INDF,
*                  IDC, BSMP, ESMP, BDET, EDET, INSCN, XSCN, DATA,
*                  DETDAT, DATUNT, YMX, YMN, NAVAIL, ADET, QEXP,
*                  DETOUT, DTINDX, NDISP, SCALE, PIC0, PIC1, ZONE,
*                  COLOUR, CURSOR, CLRBLK, XLMT, YLMT, USECUR, TITLE,
*                  DISUNT, OFFMTD, OFFSET, FLAGS, STATUS )

*  Description:
*     This routine changes the parameter values being used in program
*     TRACECRDD. The effect of the following parameters may
*     be changed:
*         DETS
*         DEVICE ( including PXSZ and PYSZ )
*         XLMT
*         YMAX
*         CURSOR
*         TITLE
*         UNITS
*         SPACE
*         OFFSET
*         OMIT
*         QEXP
*     When in cursor mode, a menu of the command will be displayed on
*     the current SGS zone, and the user can use the cursor to select a
*     parameters to change its value. After changing a parameter value,
*     the user will be prompted for further changing until a command of
*     REDRAW DISPLAY is issued or the last parameter changed is DEVICE.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the command from the
*        user.
*     PDET = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the detector numbers.
*     PDEV = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the graphic device.
*     PXSZ = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the x size of frame
*        picture.
*     PYSZ = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the y size of frame
*        picture.
*     PXLMT = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the x limit of the
*        display.
*     PYLMT = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the y maximum of the
*        display.
*     PTITL = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the title of the display.
*     PUNITS = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the units of the display.
*     POFMD = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the offset method.
*     POFF = CHARACTER * ( * ) (Given)
*        The name of the parameter name used to get the offset value.
*     POMIT = CHARACTER * ( * ) (Given)
*        The name of the parameter name used to get the parts of the
*        display to imit.
*     PQEXP = CHARACTER * ( * ) (Given)
*        The name of the parameter name used to get the quality
*        expression.
*     PXSCAN = CHARACTER * ( * ) (Given)
*        The name of the parameter name used to get the cross scan range
*        of detectors to be displayed.
*     INDF = INTEGER (Given)
*        The NDF identifier for the input CRDD file.
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     BSMP = INTEGER (Given)
*        The begin index of the samples in the input CRDD data array.
*     ESMP = INTEGER (Given)
*        The end index of the samples in the input CRDD data array.
*     BDET = INTEGER (Given)
*        The begin index of the detector in the input CRDD data array.
*     EDET = INTEGER (Given)
*        The end index of the detectors in the input CRDD data array.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample.
*     XSCN( BDET : EDET ) = REAL (Given)
*        The cross-scan distance of each detector trace.
*     DATA( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The DATA array of the CRDD file.
*     DETDAT( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The CRDD data array masked with the current quality expression.
*     DATUNT = CHARACTER * ( * ) (Given)
*        The units of the CRDD NDF data.
*     YMX( BDET : EDET ) = REAL (Given)
*        The max value of each detector data trace.
*     YMN( BDET : EDET ) = REAL (Given)
*        The min. value of each detector data trace.
*     NAVAIL = INTEGER (Given)
*        The number of available detector for display.
*     ADET( NAVAIL ) = INTEGER (Given)
*        Array holding available detector numbers.
*     QEXP = CHARACTER * ( * ) (Given and Returned)
*        The current quality expression.
*     DETOUT( BDET : EDET ) = INTEGER (Given and Returned)
*        Array holding selected detector numbers.
*     DTINDX( BDET : EDET  ) = INTEGER (Given and Returned)
*        The detector indices of the detectors to be display.
*     NDISP = INTEGER (Given and Returned)
*        The number of selected detectors.
*     SCALE( NAVAIL ) = REAL (Given and Returned)
*        On entry the first NDISP values hold the scale factors which
*        convert the NDISP (on entry ) traces to their desired units.
*        On exit the first NDISP values hold the scale factors which
*        convert the NDISP (on exit ) traces to their desired units.
*     PIC0 = INTEGER (Given and Returned)
*        The picture identifier for the picture which was current on
*        entry.
*     PIC1 = INTEGER (Given and Returned)
*        The picture identifier for the new FRAME picture.
*     ZONE = INTEGER (Given and Returned)
*        The SGS zone identifier for the new FRAME picture.
*     COLOUR = LOGICAL (Given and Returned)
*        If true, colour is available on current graphic device.
*        Otherwise, not.
*     CURSOR = LOGICAL (Given and Returned)
*        If true, cursor is available on current graphic device.
*        Otherwise, not.
*     CLRBLK = LOGICAL (Given and Returned)
*        If true, the graphic device can clear a part of its display
*        surface. Otherwise, not.
*     XLMT( 2 ) = INTEGER (Given and Returned)
*        The x limits of the display.
*     YLMT( 2 ) = INTEGER (Given and Returned)
*        The y limits of the display.
*     USECUR = LOGICAL (Given and Returned)
*        The using cursor flag. When it is true, the command menu will
*        be display on the graphic device and the cursor will be used to
*        select a command. When it is false, there will be no menu display
*        on the graphic device and the keyboard will be used to get a
*        command from the user.
*     TITLE = CHARACTER * ( * ) (Given and Returned)
*        The title of the display.
*     DISUNT = CHARACTER * ( * ) (Given and Returned)
*        The desired units of the data in the display.
*     OFFMTD = INTEGER (Given and Returned)
*        Specified the method to offset the traces.
*          0 :'FREE'
*          1 :'CONSTANT'
*          2 :'AVERAGE'
*     OFFSET( NDET ) = REAL (Given and Returned)
*        The offset specified by user when the offset method is 'FREE'.
*     FLAGS( 8 ) = LOGICAL (Given and Returned)
*        Flags indicating which parts of the display are required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1991 (WG):
*        Original version.(Based on INTERIM version GTPUPD by
*                          MAVAD::DSB )
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error values
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'IRC_PAR'          ! IRC_ constants
      INCLUDE 'IRQ_PAR'          ! IRQ_ constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER PDET*(*)
      CHARACTER PDEV*(*)
      CHARACTER PXSZ*(*)
      CHARACTER PYSZ*(*)
      CHARACTER PXLMT*(*)
      CHARACTER PYLMT*(*)
      CHARACTER PTITL*(*)
      CHARACTER PUNITS*(*)
      CHARACTER POFMD*(*)
      CHARACTER POFF*(*)
      CHARACTER POMIT*(*)
      CHARACTER PQEXP*(*)
      CHARACTER PXSCAN*(*)
      INTEGER INDF
      INTEGER IDC
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL XSCN( BDET : EDET )
      REAL DATA( BSMP : ESMP, BDET : EDET )
      REAL DETDAT( BSMP : ESMP, BDET : EDET )
      CHARACTER DATUNT*(*)
      REAL YMX( BDET : EDET )
      REAL YMN( BDET : EDET )
      INTEGER NAVAIL
      INTEGER ADET( NAVAIL )

*  Arguments Given and Returned:
      CHARACTER QEXP*(*)
      INTEGER DETOUT( BDET : EDET )
      INTEGER DTINDX( BDET : EDET )
      INTEGER NDISP
      REAL SCALE( NAVAIL )
      LOGICAL COLOUR
      LOGICAL CURSOR
      LOGICAL CLRBLK
      INTEGER PIC0
      INTEGER PIC1
      INTEGER ZONE
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      LOGICAL USECUR
      CHARACTER TITLE*(*)
      CHARACTER DISUNT*(*)
      INTEGER OFFMTD
      REAL OFFSET( BDET : EDET )
      LOGICAL FLAGS( 8 )

*  Status:
      INTEGER STATUS             ! Global status

*  External reference:
      INTEGER CHR_LEN            ! The used length of a string
      INTEGER IRC_DETIN          ! Detector index of a detector

*  Local Variables:
      CHARACTER CMMNT*128        ! Comment string of the picture
      CHARACTER COMM*20          ! A selected item
      CHARACTER DETLIS*(I90__MAXDT*3)! List of detectors.
      CHARACTER MENU*200         ! The item list of the menu

      INTEGER ACTVAL             ! Actual no. of values obtained.
      INTEGER CMNTLN             ! Used length of string CMMNT
      INTEGER DETLN              ! Used length of DETLIS.
      INTEGER I                  ! Do loop index
      INTEGER ITEMNO             ! The number of selected item
      INTEGER J                  ! Do loop index
      INTEGER LSTAT              ! Local status
      INTEGER MENULN             ! Used length of the MENU string
      INTEGER NROW( 12 )         ! The number of lines of each item


      LOGICAL CURMOD             ! Current command source mode
      LOGICAL MORE               ! flag for changing more parameter

      REAL TEMP                  ! Temporary REAL storage.
      REAL XSCAN( 2 )            ! Cross scan limits.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the menu list and the number of line of each item
*  when displayed on the SGS zone.
      MENU = 'Redraw display,Det#,Device,X limit,Y limit,'/
     :      /'Title,Units,Command source,Offset method,Display'/
     :      /' structure,Quality exp.,Cross scan'
      MENULN = CHR_LEN( MENU )
      NROW( 1 ) = 2
      NROW( 2 ) = 1
      NROW( 3 ) = 1
      NROW( 4 ) = 1
      NROW( 5 ) = 1
      NROW( 6 ) = 1
      NROW( 7 ) = 1
      NROW( 8 ) = 2
      NROW( 9 ) = 2
      NROW( 10 ) = 2
      NROW( 11 ) = 2
      NROW( 12 ) = 2

*  Enter a loop to change the values of the parameters until REDRAW
*  DISPLAY or DEVICE is issued.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )
         ITEMNO = 0
         COMM = ' '

*  If in cursor mode, draw a menu and select an item from the menu.
         IF( USECUR ) THEN
            CALL SGS_CLRBL( -0.53, 1.27, -0.5, -0.13 )
            CALL IRM_HMENU( 12, MENU, NROW, 1, -0.53,  1.27, -0.3,
     :                     -0.22, 'Select a parameter by positioning'/
     :                     /' cursor and pressing any key', COLOUR,
     :                     1, 0.02, .TRUE., ITEMNO, STATUS )

*  Otherwise, select a parameter from the keyboard.
         ELSE

*  Cancel the value of the parameter PARAM for getting a new value.
            CALL PAR_CANCL( PARAM, STATUS )

*  Get a command from the environment to see which parameter to change.
            CALL PAR_CHOIC( PARAM, 'Redraw display', MENU( : MENULN ),
     :                     .TRUE., COMM, STATUS )

         END IF

*  Check status, if error, exit.
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If the detectors in the display are to be updated,
         IF ( ITEMNO .EQ. 2 .OR. COMM( : 4 ) .EQ. 'DET#' .OR.
     :        ITEMNO .EQ. 12 .OR. COMM( : 10 ) .EQ. 'CROSS SCAN' ) THEN

*  If an explicit list is to be given, cancel the previous value
*  associated with paramter PDET, and get a new value for it.
            IF( ITEMNO .EQ. 2 .OR. COMM( : 4 ) .EQ. 'DET#' ) THEN
               CALL PAR_CANCL( PDET, STATUS )
               CALL IRM_GTDET( PDET, NAVAIL, ADET, DETOUT, NDISP,
     :                         STATUS )

*  If a cross scan range is used to specify the detectors, cancel the
*  previous values of the parameter PXSCAN and get new values.
            ELSE
               CALL PAR_CANCL( PXSCAN, STATUS )
               CALL PAR_GET1R( PXSCAN, 2, XSCAN, ACTVAL, STATUS )

               IF( ACTVAL .EQ. 1 ) THEN
                  XSCAN( 1 ) = -ABS( XSCAN( 1 ) )
                  XSCAN( 2 ) = -XSCAN( 1 )

               ELSE

                  IF( XSCAN( 1 ) .GT. XSCAN( 2 ) ) THEN
                     TEMP = XSCAN( 1 )
                     XSCAN( 1 ) = XSCAN( 2 )
                     XSCAN( 2 ) = TEMP
                  END IF

               END IF

               CALL TRACA2( IDC, NAVAIL, BDET, EDET, ADET, XSCN, XSCAN,
     :                      DETOUT, NDISP, DETLIS, DETLN, STATUS )

            END IF

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If a null value was supplied, annul the error and keep the previous
*  detector numbers unchanged
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other error, exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If the current quality expression is not "ANY", reset it to "ANY".
            IF( QEXP .NE. 'ANY' ) THEN
               QEXP = 'ANY'

*  Copy all data from the DATA component of the input NDF to the DETDAT
*  array, and warn the user that data of any quality is being displayed.
               DO J = BDET, EDET
                  DO I = BSMP, ESMP
                     DETDAT( I, J ) = DATA( I, J )
                  END DO
               END DO

               CALL MSG_BLANKIF( MSG__QUIET, STATUS )
               CALL MSG_OUTIF( MSG__QUIET, 'TRACA8_MSG1',
     :                 'WARNING: Data of ANY quality will be displayed',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__QUIET, STATUS )

            END IF

*  If the present offset method is not 'AVERAGE', revert it to
*  'AVERAGE'.
            IF ( OFFMTD .NE. 2 ) THEN
               OFFMTD = 2
               CALL MSG_BLANKIF( MSG__QUIET, STATUS )
               CALL MSG_OUTIF( MSG__QUIET, 'TRACA8_MSG2',
     :                 'WARNING: Offset method reverted to AVERAGE',
     :                         STATUS )
               CALL MSG_BLANKIF( MSG__QUIET, STATUS )
            END IF

*  Get the detector indices of the newly selected detectors.
            DO I = 1, NDISP
               DTINDX( I ) = IRC_DETIN( IDC, DETOUT( I ), STATUS )
            END DO

*  Calculate the scale factors to convert the data of these newly
*  selected detectors from the units of original NDF file to the units
*  desired by the user.
            CALL IRM_UNTCV( DATUNT, DISUNT, NDISP, DETOUT, SCALE,
     :                      STATUS )

*  Set up new Y limits (the user is not prompted).
            CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, XLMT, YMX, YMN, ' ',
     :                   YLMT, STATUS )

*  If getting a new graphic device is required,
         ELSE IF ( ITEMNO .EQ. 3 .OR. COMM( : 6 ) .EQ. 'DEVICE' ) THEN

*  Close SGS and AGI, and cancel the values associated with parameter
*  PDEV, PXSZ, PYSZ.
            CALL AGS_DEACT( STATUS )
            CALL AGI_END( PIC0, STATUS )
            CALL AGI_CANCL( PDEV, STATUS )

            CALL PAR_CANCL( PXSZ, STATUS )
            CALL PAR_CANCL( PYSZ, STATUS )

*  Begin a new AGI scope.
            CALL AGI_BEGIN

*  Get a new graphic device, and open it for NCAR use.
            CMMNT = 'TRACECRDD_'//TITLE
            CMNTLN = CHR_LEN( CMMNT )
            CALL IRM_GROPN( PDEV, PXSZ, PYSZ, .TRUE.,
     :                      CMMNT( : CMNTLN ), PIC0, PIC1, ZONE,
     :                      COLOUR, CURSOR, CLRBLK, STATUS )

*  Check status, if error happened, re-active the SGS package to
*  ensure that application all be closed correctly. Then exit.
            IF ( STATUS .NE. SAI__OK ) THEN
               LSTAT = SAI__OK
               CALL AGS_ACTIV( LSTAT )
               GOTO 999

*  If no error happened, end the loop.
            ELSE
               MORE = .FALSE.

*  And if cursor can not be used on the new device, set to 'keybord mode'.
               IF ( .NOT.CURSOR .OR. .NOT.CLRBLK ) THEN
                  USECUR = .FALSE.
               END IF
            END IF

*  If X limits of the display need be changed,
         ELSE IF ( ITEMNO .EQ. 4 .OR. COMM( : 7 ) .EQ. 'X LIMIT' ) THEN

*  Cancel the previous value associated with the parameter PXLMT and
*  get a new value for it.
            CALL PAR_CANCL( PXLMT, STATUS )
            CALL TRACA5( BSMP, ESMP, BDET, EDET, INSCN, NDISP,
     :                   DTINDX, PXLMT, XLMT, STATUS )

*  Check the status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If parameter gets a NULL value, annul the error and keep the x
*  limit unchanged.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other error, exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If Y limits of the display need be changed,
         ELSE IF ( ITEMNO .EQ. 5 .OR. COMM( : 7 ) .EQ. 'Y LIMIT' ) THEN

*  Cancel the previous value associated with the parameter PYMAX, set
*  the previous value as default and get a new value for it.
            CALL PAR_CANCL( PYLMT, STATUS )
            CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, XLMT, YMX, YMN, PYLMT,
     :                   YLMT, STATUS )

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If parameter get a NULL value, annul the error and keep Y limit
*  unchanged.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL ( STATUS )
                  GOTO 888

*  For any other errors, report and exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If the title of the display is to be changed,
         ELSE IF ( ITEMNO .EQ. 6 .OR. COMM( : 5 ) .EQ. 'TITLE' ) THEN

*  Cancel the previous value and get a new value for parameter PTITL.
            CALL PAR_CANCL( PTITL, STATUS )
            CALL PAR_GET0C( PTITL, TITLE, STATUS )

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If parameter get a NULL value annul the error and keep title
*  unchanged.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other errors, exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If ask to change the display units, cancel the previous value of the
*  parameter PUNITS.
         ELSE IF ( ITEMNO .EQ. 7 .OR. COMM( : 5 ) .EQ. 'UNITS' ) THEN
            CALL PAR_CANCL( PUNITS, STATUS )

*  Get a new units value.
            CALL IRM_GTCUN( PUNITS, DATUNT, DISUNT, STATUS )

*  Find the units convert scale.
            CALL IRM_UNTCV( DATUNT, DISUNT, NDISP, DETOUT, SCALE,
     :                      STATUS )

*  Set up new Y limits (the user is not prompted).
            CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, XLMT, YMX, YMN, ' ',
     :                   YLMT, STATUS )

*  If ask to use alternative way to issue the coomand,
         ELSE IF ( ITEMNO .EQ. 8 .OR.
     :             COMM( : 14 ) .EQ. 'COMMAND SOURCE' ) THEN

*  If current mode is 'cursor', set to 'keybord'
            CURMOD = USECUR
            IF ( CURMOD ) THEN
               USECUR = .NOT.USECUR

*  If current mode is 'keybord', and the device can be in cursor mode,
*  set to cursor mode.
            ELSE
               IF ( CURSOR .AND. CLRBLK ) THEN
                  USECUR = .NOT.USECUR

*  Otherwise give a warning message and does not change the mode.
               ELSE
                  CALL MSG_BLANKIF( MSG__QUIET, STATUS )
                  CALL MSG_OUTIF( MSG__QUIET, 'TRACA8_MSG3',
     :                          'WARNING: No graphics cursor available',
     :                            STATUS )
                  CALL MSG_BLANKIF( MSG__QUIET, STATUS )
               END IF
            END IF

*  Report to the user the present command source.
            IF ( USECUR ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'TRACA8_MSG4',
     :                     '  Using cursor for command source', STATUS )
            ELSE
               CALL MSG_OUT( MSG__NORM, 'TRACA8_MSG5',
     :                   '  Using keyboard for command source', STATUS )

            END IF

*  If the offset method is to be changed.
         ELSE IF ( ITEMNO .EQ. 9 .OR.
     :             COMM( : 13 ) .EQ. 'OFFSET METHOD' ) THEN

*  Cancel the previous value and get a new value for parameter POFMD.
            CALL PAR_CANCL( POFMD, STATUS )
            CALL PAR_CANCL( POFF, STATUS )
            CALL TRACA4( POFMD, POFF, NDISP, OFFMTD, OFFSET, STATUS )

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If parameter get a NULL value annul the error and keep Y limit
*  unchanged.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other errors, report and exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If parts of the display are to be selected or omitted,
         ELSE IF( ITEMNO .EQ. 10 .OR.
     :            COMM( : 17 ) .EQ. 'DISPLAY STRUCTURE' ) THEN

*  Cancel the previous value and get a new value for parameter POMIT.
            CALL PAR_CANCL( POMIT, STATUS )
            CALL TRACC2( POMIT, FLAGS, STATUS )

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If parameter get a NULL value annul the error and keep flags
*  unchanged.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other errors, report and exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  If a new quality expression is to be used...
         ELSE IF( ITEMNO .EQ. 11 .OR.
     :            COMM( : 18 ) .EQ. 'QUALITY EXP.' ) THEN

            CALL PAR_CANCL( PQEXP, STATUS )
            CALL TRACC5( PQEXP, INDF,
     :                  ( ESMP - BSMP + 1 )*( EDET - BDET + 1 ), DATA,
     :                  DETDAT, QEXP, STATUS )

*  Check status.
            IF ( STATUS .NE. SAI__OK ) THEN

*  If a null value was supplied, annul the error and keep the previous
*  quality expression unchanged
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  GOTO 888

*  For any other error, exit.
               ELSE
                  GOTO 999
               END IF
            END IF

*  Set up new Y limits (the user is not prompted).
            CALL TRACA6( IDC, BSMP, ESMP, BDET, EDET, INSCN, DETDAT,
     :                   NDISP, DTINDX, SCALE, XLMT, YMX, YMN, ' ',
     :                   YLMT, STATUS )

*  If no data from the detectors being displayed satisfies the quality
*  expression, flush the error.
            IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  If redrew display is required, end the loop.
         ELSE IF ( ITEMNO .EQ. 1 .OR.
     :             COMM( : 14 ) .EQ. 'REDRAW DISPLAY' ) THEN
            MORE = .FALSE.
         END IF

 888     CONTINUE

*  Go back to change more parameter values, unless 'Redraw display' or
*  'Device' is selected.
      END DO

 999  CONTINUE

      END
