      SUBROUTINE LUTEDIT( STATUS )
*+
*  Name:
*     LUTEDIT

*  Purpose:
*     Create or edit an image-display colour lookup table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LUTEDIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows a lookup table to be created or edited 
*     interactively. The process is controlled through a graphical user
*     interface which presents curves of intensity against pen number, and 
*     allows the user to change them in various ways. A specified image
*     is displayed as part of the interface in order to see the effects of
*     the changes. A histogram of pen values is also included. The colour
*     of each pen can be displayed either as red, green and blue intensity, 
*     or as hue, saturation and value. More information on the use of the 
*     GUI is available through the Help menu within the GUI.

*  Usage:
*     lutedit lut image

*  ADAM Parameters:
*     LUT = NDF (Read)
*        Name of an exiting colour table to be edited. This should be an 
*        NDF containing an array of red, green and blue intensities. The 
*        NDF must be 2-dimensional, the first dimension being 3, and the 
*        second being arbitrary.  The method used to compress or expand the 
*        colour table if the second dimension is different from the number 
*        of unreserved colour indices is controlled by the "Interpolation" 
*        option in the GUI. If LUT is null (!) the current KAPPA colour 
*        table for the xwindows graphics display is used. [!]
*     IMAGE = NDF (Read)
*        Input NDF data structure containing the image to be displayed
*        to show the effect of the created colour table. If a null value
*        is supplied a default image is used. 

*  Related Applications:
*     KAPPA: LUTABLE, LUTFLIP, LUTREAD, LUTSAVE, LUTVIEW, PALREAD, PALSAVE; 
*     Figaro: COLOUR.

*  Authors:
*     DSB David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE definitions
      INCLUDE 'PSX_ERR'        ! PSX error constants
      INCLUDE 'PAR_ERR'        ! PAR error constants

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER CMD*512        ! Command to execute lutedit tcl script
      CHARACTER IMNAM*255      ! Full path for IMAGE NDF
      CHARACTER LUTNAM*255     ! Full path for LUT NDF
      INTEGER ILEN             ! Used length of IMNAM 
      INTEGER LLEN             ! Used length of LUTNAM 
      INTEGER IAT              ! Length of CMD
      INTEGER IPLUT            ! Pointer to mapped lut array
      INTEGER NEL              ! No. of elements in IPLUT
      INTEGER INDF1            ! LUT NDF identifier
      INTEGER INDF2            ! Image NDF identifier
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN 

*  Initialize a string to hold the command to execute the lutedit tcl
*  script.
      CMD = ' '
      CALL PSX_GETENV( 'KAPPA_DIR', CMD, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CMD = '/star/bin/kappa'
      END IF

      IAT = CHR_LEN( CMD )
      CALL CHR_APPND( '/lutedit.tcl', CMD, IAT )

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
      CALL KPG1_AVLUT( 'LUT', INDF1, IPLUT, NEL, STATUS )

*  Null status means do not read a new lookup table. Instead, we will use
*  the existing table.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL CHR_APPND( ' ""', CMD, IAT )

*  Otherwise, get the full path to the NDF.
      ELSE
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_LOAD( ' ', '^NDF', LUTNAM, LLEN, STATUS )
         IAT = IAT + 1
         CALL CHR_APPND( LUTNAM, CMD, IAT )
      END IF

* Get the image to display.
      CALL LPG_ASSOC( 'IMAGE', 'READ', INDF2, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL MSG_LOAD( ' ', '^NDF', IMNAM, ILEN, STATUS )
         IAT = IAT + 1
         CALL CHR_APPND( IMNAM, CMD, IAT )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Execute the script.
      CALL KPS1_LUTED( CMD( : IAT ), STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUTEDIT_ERR', 'LUTEDIT: Failed to edit or '//
     :                 'create a colour lookup table.',  STATUS )
      END IF

      END
