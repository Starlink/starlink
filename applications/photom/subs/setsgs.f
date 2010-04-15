************************************************************************

      SUBROUTINE SETSGS ( ORIGIN, BZONE, IZONE, CLEAR, PENO, PENS,
     :                    PENP, IMGDIS, STATUS )

*+
*  Name :
*     SETSGS
*
*  Purpose :
*     This switches on a device and gets the image zone from the graphics
*     database.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL SETSGS( ORIGIN, BZONE, IZONE, CLEAR, PENO, PENS, PENP,
*    :             IMGDIS, STATUS )
*
*  Description :
*     This switches on a device and gets the image zone from the graphics
*     database. The screen base zone is also returned for text. The device
*     is tested to see if it is an overlay plane, if so the clear flag is
*     set.
*
*     The cursor is initialised to use the mouse buttons if available.
*     The CHOICE values this gives us are:-
*
*         CHOICE     action
*           5         Mouse button 1
*           0         Mouse button 2
*           1         Keyboard 1
*           2         Keyboard 2
*           0         Keyboard 3
*           3         Keyboard 0
*           4         Keyboard .
*
*     The old actions where 1,2,0 from the keyboard. 0 meaning exit.
*
*  Arguments :
*     ORIGIN( 2 ) = INTEGER (Given)
*        NDF origin offset
*
*   Returned
*     BZONE = INTEGER (Returned)
*        Base SGS zone
*     IZONE = INTEGER (Returned)
*        Image zone obtained from graphics database
*     CLEAR = LOGICAL (Returned)
*        Flag to indicate if block clears are to be done
*     PENO = INTEGER (Returned)
*        Pen colour for object aperture
*     PENS = INTEGER (Returned)
*        Pen colour for sky aperture
*     PENP = INTEGER (Returned)
*        Pen colour for PSF star aperture
*     IMGDIS = LOGICAL (Returned)
*        TRUE if display has an image display cursor (mouse).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1987 (NE):
*        Original version.
*     10-SEP-1989 (NE):
*        Alter the way the base picture is obtained
*     10-OCT-1989 (NE):
*        Add clear flag and pen numbers
*     10-AUG-1990 (NE):
*        Allow for any NDF origin offset
*     10-NOV-1990 (NE):
*        Added call to AGI_BEGIN
*     5-JUN-1992 (NE):
*        Allow for WINDOW_OVERLAY device.
*     6-NOV-1996 (PWD):
*        Now initialises ISTAT (Linux port).
*     8-NOV-1996 (PWD):
*        Added cursor preparation to get mouse buttons to work
*        on X-windows.
*     3-DEC-1998 (AA):
*        Added additional pen colour (blue) for PSF stars
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'GKS_PAR'
      INCLUDE 'GNS_PAR'

*  Arguments Given :
      INTEGER ORIGIN( 2 )

*  Arguments Returned :
      INTEGER BZONE
      INTEGER IZONE
      LOGICAL CLEAR
      INTEGER PENO
      INTEGER PENS
      INTEGER PENP
      LOGICAL IMGDIS

*  Status :
      INTEGER STATUS

*  Local Variables :
      LOGICAL YESNO, CURCHO

      INTEGER COLA, CONID, ISTAT, NCOL, NPCI, PICID1, PICID2, PICID3,
     :        WKID, WTYPE

      REAL WX1, WX2, WY1, WY2, XM, YM

      CHARACTER * ( GNS__SZKEY ) GCLASS
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the device name
         CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID1, STATUS )
         CALL AGI_BEGIN
         CALL AGS_ACTIV( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SETSGS_AGIOPEN',
     :                    'Problem with graphics database', STATUS )
            GOTO 99
         ENDIF

*   Get the base zone from the database.
         CALL AGI_IBASE( PICID2, STATUS )
         CALL AGI_SELP( PICID2, STATUS )
         CALL AGS_NZONE( BZONE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SETSGS_DEVOPEN',
     :                    'Problem opening device', STATUS )
            GOTO 99
         ENDIF

*   Check the GNS class
         CALL SGS_ICURW( WKID )
         ISTAT = 0
         CALL GNS_IWCG( WKID, 'CLASS', GCLASS, ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSGS_GNS', 'GNS error', STATUS )
            GOTO 99
         ENDIF

*   Report an error if the device is non-interactive
         IF ( ( GCLASS .EQ. 'MATRIX_PRINTER' ) .OR.
     :        ( GCLASS .EQ. 'METAFILE_INPUT' ) .OR.
     :        ( GCLASS .EQ. 'METAFILE_OUTPUT' ) .OR.
     :        ( GCLASS .EQ. 'PEN_PLOTTER' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSGS_DISP', 'Device not suitable', STATUS )
            GOTO 99
         ENDIF

*   Set the clear flag to true if the device is an overlay, otherwise flase
         IF ( ( GCLASS .EQ. 'IMAGE_OVERLAY' ) .OR.
     :        ( GCLASS .EQ. 'WINDOW_OVERLAY' ) ) THEN
            CLEAR = .TRUE.
         ELSE
            CLEAR = .FALSE.
         ENDIF

*   Check that the device has selective erase
         CALL SGS_ISLER( YESNO )
         IF ( .NOT. YESNO ) THEN
            CLEAR = .FALSE.
         ENDIF

*   Recall the last data picture in the database and recreate it as a zone.
         CALL AGI_RCL( 'DATA', PICID3, STATUS )
         CALL AGS_NZONE( IZONE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'SETSGS_IMAGE',
     :                    'Image has not been properly displayed',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Reset the world coordinates to take off any NDF origin offset
         CALL SGS_IZONE( WX1, WX2, WY1, WY2, XM, YM )
         WX1 = WX1 - REAL( ORIGIN( 1 ) - 1 )
         WX2 = WX2 - REAL( ORIGIN( 1 ) - 1 )
         WY1 = WY1 - REAL( ORIGIN( 2 ) - 1 )
         WY2 = WY2 - REAL( ORIGIN( 2 ) - 1 )
         CALL SGS_SW( WX1, WX2, WY1, WY2, STATUS )

*   Inquire the colour facilities of the device
         CALL GQWKC( WKID, ISTAT, CONID, WTYPE )
         CALL GQCF( WTYPE, ISTAT, NCOL, COLA, NPCI )
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSGS_GKS', 'GKS error', STATUS )
            GOTO 99
         ENDIF

*   Set up pens 2, 3 and 4 as red, green and blue if allowed

         IF ( ( COLA .EQ. GCOLOR ) .AND. ( NCOL .GE. 4 ) ) THEN
            PENS = 2
            PENO = 3
	    PENP = 4
            CALL GSCR( WKID, PENS, 1.0, 0.0, 0.0 )
            CALL GSPLR( WKID, PENS, 1, 1.0, 2 )
            CALL GSCR( WKID, PENO, 0.0, 1.0, 0.0 )
            CALL GSPLR( WKID, PENO, 1, 1.0, 3 )
            CALL GSCR( WKID, PENP, 0.0, 0.0, 1.0 )
            CALL GSPLR( WKID, PENP, 1, 1.0, 4 )

*   Otherwise use pen 1
         ELSE
            PENS = 1
            PENO = 1
	    PENP = 1
         ENDIF

*   Select the pen to plot with
         CALL SGS_SPEN( PENO )

*   Set up the cursor for this device.
         CALL PHO1_PRCUR( 2, '120.', CURCHO, IMGDIS, STATUS )

      ENDIF

  99  CONTINUE

      END
