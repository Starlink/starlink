      SUBROUTINE KAPVIEW_MON( STATUS )
*+
*  Name:
*     KAPVIEW_MON

*  Purpose:
*     Top-level KAPVIEW subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL KAPVIEW_MON( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the KAPVIEW
*     suite of A-tasks.  Each KAPVIEW command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 August 19 (MJC):
*        Original version.
*     1995 October 30 (MJC):
*        Added PICEMPTY, PICENTIRE, and PICVIS.
*     1997 May 31 (MJC):
*        Added DRAWSIG.  Removed BLINK, IDUNZOOM, IDVISIBLE.  V1.1.
*        Temporarily removed COLUMNAR and HIDE.
*     13-MAY-1999 (DSB):
*        Changed history application name to incorporate the current version
*        of KAPPA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'          ! SSE global definitions

*  Status:
      INTEGER  STATUS

*  Local Variables:
      CHARACTER NAME * ( 15 )     ! Task name from the command

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from the environment.  This returns uppercase
*  names.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Identify and execute the task.
*  ==============================
*
*  Define the current application name for history. The package version
*  number gets substituted in here when the KAPPA release source tar file 
*  is contructed.
      CALL NDF_HAPPN( NAME // ' (KAPVIEW PKG_VERS)', STATUS )

*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

*  Draws a perspective-histogram representatation of a 2-d NDF.
*      ELSE IF ( NAME .EQ. 'COLUMNAR' ) THEN
*         CALL COLUMNAR ( STATUS )

*  Contours a 2-d NDF.
      IF ( NAME .EQ. 'CONTOUR' ) THEN
         CALL CONTOUR ( STATUS )

*  Contours a 2-d NDF overlaid on an image displayed previously.
      ELSE IF ( NAME .EQ. 'CONTOVER' ) THEN
         CALL CONTOVER ( STATUS )

*  Creates or manipulates an image-display lookup table using a palette.
      ELSE IF ( NAME .EQ. 'CRELUT' ) THEN
         CALL CRELUT ( STATUS )

*  Reports the co-ordinates of points selected using the
*  cursor and selects the current picture.
      ELSE IF ( NAME .EQ. 'CURSOR' ) THEN
         CALL CURSOR ( STATUS )

*  Displays a 2-d NDF.
      ELSE IF ( NAME .EQ. 'DISPLAY' ) THEN
         CALL DISPLAY( STATUS )

*  Draws +/-n standard-deviation lines on a line plot.
      ELSE IF ( NAME .EQ. 'DRAWSIG' ) THEN
         CALL DRAWSIG( STATUS )

*  Creates a radial or azimuthal profile of a 2-dimensional image.
      ELSE IF ( NAME .EQ. 'ELPROF' ) THEN
         CALL ELPROF ( STATUS )

*  Clears a graphics device and purges its database entries.
      ELSE IF ( NAME .EQ. 'GDCLEAR' ) THEN
         CALL GDCLEAR( STATUS )

*  Shows which graphics devices are available.
      ELSE IF ( NAME .EQ. 'GDNAMES' ) THEN
         CALL GDNAMES ( STATUS )

*  Selects a current graphics device.
      ELSE IF ( NAME .EQ. 'GDSET' ) THEN
         CALL GDSET ( STATUS )

*  Shows the current status of a graphics device.
      ELSE IF ( NAME .EQ. 'GDSTATE' ) THEN
         CALL GDSTATE( STATUS )

*  Produces a greyscale plot of a 2-d NDF.
      ELSE IF ( NAME .EQ. 'GREYPLOT' ) THEN
         CALL GREYPLOT ( STATUS )

*  Draws a perspective plot of a two-dimensional NDF.
*      ELSE IF ( NAME .EQ. 'HIDE' ) THEN
*         CALL HIDE ( STATUS )

*  Clears an image display and purges its database entries.
      ELSE IF ( NAME .EQ. 'IDCLEAR' ) THEN
         CALL GDCLEAR( STATUS )

*  Makes memory planes of an image-display device invisible.
      ELSE IF ( NAME .EQ. 'IDINVISIBLE' ) THEN
         CALL IDINVISIBLE ( STATUS )

*  Pans and zooms an image-display device.
      ELSE IF ( NAME .EQ. 'IDPAZO' ) THEN
         CALL IDPAZO ( STATUS )

*  Selects a current image-display device.
      ELSE IF ( NAME .EQ. 'IDSET' ) THEN
         CALL IDSET ( STATUS )

*   Shows the current status of an image display.
      ELSE IF ( NAME .EQ. 'IDSTATE' ) THEN
         CALL IDSTATE( STATUS )

*  Inspects a 2-d NDF in a variety of ways.
      ELSE IF ( NAME .EQ. 'INSPECT' ) THEN
         CALL INSPECT ( STATUS )

*  Draws a line plot of a 1-d NDF's data values against their axis
*  co-ordinates.
      ELSE IF ( NAME .EQ. 'LINPLOT' ) THEN
         CALL LINPLOT ( STATUS )

*  Manipulates an image-display colour table.
      ELSE IF ( NAME .EQ. 'LUTABLE' ) THEN
         CALL LUTABLE ( STATUS )

*  Flips the colour table of an image-display device.
      ELSE IF ( NAME .EQ. 'LUTFLIP' ) THEN
         CALL LUTFLIP ( STATUS )

*  Highlights a colour table of image-display device.
      ELSE IF ( NAME .EQ. 'LUTHILITE' ) THEN
         CALL LUTHILITE ( STATUS )

*  Rotates a colour table of image-display device.
      ELSE IF ( NAME .EQ. 'LUTROT' ) THEN
         CALL LUTROT ( STATUS )

*  Saves the current colour table of an image-display device in an NDF.
      ELSE IF ( NAME .EQ. 'LUTSAVE' ) THEN
         CALL LUTSAVE ( STATUS )

*  Tweaks a colour table of an image display.
      ELSE IF ( NAME .EQ. 'LUTTWEAK' ) THEN
         CALL LUTTWEAK ( STATUS )

*   Draws a colour-table key.
      ELSE IF ( NAME .EQ. 'LUTVIEW' ) THEN
         CALL LUTVIEW ( STATUS )

*  Draws a multi-line plot of a 2-d NDF's data values against their axis
*  co-ordinates.
      ELSE IF ( NAME .EQ. 'MLINPLOT' ) THEN
         CALL MLINPLOT ( STATUS )

*  Clears an image-overlay device.
      ELSE IF ( NAME .EQ. 'OVCLEAR' ) THEN
         CALL OVCLEAR ( STATUS )

*  Selects a current image-display overlay.
      ELSE IF ( NAME .EQ. 'OVSET' ) THEN
         CALL OVSET ( STATUS )

*  Loads the default palette to a colour table.
      ELSE IF ( NAME .EQ. 'PALDEF' ) THEN
         CALL PALDEF( STATUS )

*  Enters a colour into an image display's palette.
      ELSE IF ( NAME .EQ. 'PALENTRY' ) THEN
         CALL PALENTRY( STATUS )

*  Fills the reserved palette of a colour table from an NDF.
      ELSE IF ( NAME .EQ. 'PALREAD' ) THEN
         CALL PALREAD( STATUS )

*  Saves the current reserved portion of a colour table to an NDF.
      ELSE IF ( NAME .EQ. 'PALSAVE' ) THEN
         CALL PALSAVE( STATUS )

*  Uses a cursor to select the current picture and to report the
*  co-ordinates of points.
      ELSE IF ( NAME .EQ. 'PICCUR' ) THEN
         CALL PICCUR( STATUS )

*  Defines a new graphics-database picture or an array of pictures.
      ELSE IF ( NAME .EQ. 'PICDEF' ) THEN
         CALL PICDEF( STATUS )

*  Finds the first empty FRAME picture in the graphics database.
      ELSE IF ( NAME .EQ. 'PICEMPTY' ) THEN
         CALL PICEMPTY( STATUS )

*  Finds the first unobscured and unoscuring FRAME picture in the
*  graphics database.
      ELSE IF ( NAME .EQ. 'PICENTIRE' ) THEN
         CALL PICENTIRE( STATUS )

*  Finds the attributes of a picture interior to the current picture.
      ELSE IF ( NAME .EQ. 'PICIN' ) THEN
         CALL PICIN( STATUS )

*  Labels the current graphics-database picture.
      ELSE IF ( NAME .EQ. 'PICLABEL' ) THEN
         CALL PICLABEL ( STATUS )

*  Lists the pictures in the graphics database for a device.
      ELSE IF ( NAME .EQ. 'PICLIST' ) THEN
         CALL PICLIST ( STATUS )

*  Selects a graphics-database picture by its label.
      ELSE IF ( NAME .EQ. 'PICSEL' ) THEN
         CALL PICSEL ( STATUS )

*  Transforms co-ordinates between the current and base pictures.
      ELSE IF ( NAME .EQ. 'PICTRANS' ) THEN
         CALL PICTRANS ( STATUS )

*  Finds the first unobscured FRAME picture in the graphics database.
      ELSE IF ( NAME .EQ. 'PICVIS' ) THEN
         CALL PICVIS( STATUS )

*  Dumps an image-display memory to a graphics hardcopy and optionally
*  to an NDF.
      ELSE IF ( NAME .EQ. 'SNAPSHOT' ) THEN
         CALL SNAPSHOT ( STATUS )

*  Contours a 2-d NDF quickly.
      ELSE IF ( NAME .EQ. 'TURBOCONT' ) THEN
         CALL TURBOCONT ( STATUS )

*  Plots a 2-dimensional vector map.
      ELSE IF ( NAME .EQ. 'VECPLOT' ) THEN
         CALL VECPLOT ( STATUS )

      ELSE

*  No such option exists.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'CMD', NAME )
         CALL ERR_REP( 'KAPVIEW_MON_NOCOM',
     :     'KAPVIEW: No such option ^CMD.', STATUS )

      END IF

*  End and return.

      END

