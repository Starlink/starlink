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
*     lutedit lut image device

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The name of an image display device.  If a null value is
*        supplied for parameter LUT, then the current LUT associated with
*        the specified device will be loaded into the editor initially.
*        On exit, the final contents of the editor (if saved) are
*        established as the current LUT for the specified device.
*        [Current image-display device]
*     LUT = NDF (Read)
*        Name of an exiting colour table to be edited. This should be an
*        NDF containing an array of red, green and blue intensities. The
*        NDF must be 2-dimensional, the first dimension being 3, and the
*        second being arbitrary.  The method used to compress or expand the
*        colour table if the second dimension is different from the number
*        of unreserved colour indices is controlled by the "Interpolation"
*        option in the GUI. If LUT is null (!) the current KAPPA colour
*        table for the device specified by parameter DEVICE is used. [!]
*     IMAGE = NDF (Read)
*        Input NDF data structure containing the image to be displayed
*        to show the effect of the created colour table. If a null value
*        is supplied a default image is used.

*  Related Applications:
*     KAPPA: LUTABLE, LUTREAD, LUTSAVE, LUTVIEW, PALREAD, PALSAVE;
*     Figaro: COLOUR.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2011 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-2001 (DSB):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2011 May 20 (MJC):
*        Add an AGI context before calling AGI_ASSOC, so that the code
*        adheres to 2006-revised expectations of the AGI closing
*        subroutine.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE definitions
      INCLUDE 'DAT_PAR'        ! HDS constants
      INCLUDE 'PSX_ERR'        ! PSX error constants
      INCLUDE 'PAR_ERR'        ! PAR error constants
      INCLUDE 'CTM_PAR'        ! Colout Table Management constants
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER CMD*512        ! Command to execute lutedit tcl script
      CHARACTER IMNAM*255      ! Full path for IMAGE NDF
      CHARACTER LOC*(DAT__SZLOC)! HDS locator for new ndf
      CHARACTER LUTNAM*255     ! Full path for LUT NDF
      CHARACTER PLOC*(DAT__SZLOC)! HDS locator for original LUT
      INTEGER IAT              ! Length of CMD
      INTEGER ILEN             ! Used length of IMNAM
      INTEGER INDF1            ! LUT NDF identifier
      INTEGER INDF2            ! Image NDF identifier
      INTEGER IPIC             ! AGI identifier for current picture
      INTEGER IPLUT            ! Pointer to mapped lut array
      INTEGER LLEN             ! Used length of LUTNAM
      INTEGER LP               ! Lowest usable pen index
      INTEGER NEL              ! No. of elements in IPLUT
      INTEGER PLACE            ! Unused NDF placeholder
      INTEGER UP               ! Highest usable pen index
      LOGICAL DEVOPN           ! Has a graphice device been opened
      LOGICAL SAVLUT           ! Save the edited LUT?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Start an initial AGI context. This allows the corresponding AST_END
*  to annul the identifier returned by the following call to AGI_ASSOC.
*  This accounts for the 2006 Feb 6 changes to KPG1_PGOPN and
*  KPG1_PGCLS.
      CALL AGI_BEGIN

*  Associate the parameter with a workstation and current picture.
      CALL AGI_ASSOC( 'DEVICE', 'READ', IPIC, STATUS )

*  If no device given, annul the error.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DEVOPN = .FALSE.

*  Otherwise, open it.
      ELSE
         CALL AGI_BEGIN
         CALL AGP_ACTIV( STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )
         DEVOPN = .TRUE.
      END IF

*  Initialize the names of the LUT and image to use.
      LUTNAM = '""'
      IMNAM = '""'
      SAVLUT = .FALSE.

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
      CALL KPG1_AVLUT( 'LUT', INDF1, IPLUT, NEL, STATUS )

*  Null status means do not read a new lookup table. Instead, we will use
*  the existing table for the specified device (if any).
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Check a device was given.
         IF( DEVOPN ) THEN
            LUTNAM = './lutedit-temp'

*  Locate the HDS array holding the LUT for the specified device.
            CALL KPG1_LTGET( PLOC, STATUS )
            IF( PLOC .NE. DAT__NOLOC ) THEN

*  Create an NDF holding the devices colour table.
               CALL HDS_NEW( LUTNAM, 'LUTEDIT', 'LUT', 0, 0, LOC,
     :                       STATUS )

*  Copy the array into it, putting it in "data_array" so that it becomes
*  a primitive NDF, and can be accessed by other KAPPA apps.
               CALL DAT_COPY( PLOC, LOC, 'DATA_ARRAY', STATUS )

*  Close both files.
               CALL DAT_ANNUL( PLOC, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )

            END IF

*  Indicate that the edited LUT should be saved before exiting.
            SAVLUT = .TRUE.

         END IF

*  If a specific LUT was given, get the full path to the NDF.
      ELSE
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_LOAD( ' ', '^NDF', LUTNAM, LLEN, STATUS )
      END IF

*  Get the image to display.
      CALL LPG_ASSOC( 'IMAGE', 'READ', INDF2, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL MSG_LOAD( ' ', '^NDF', IMNAM, ILEN, STATUS )
      END IF

*  Get a string holding the command to execute the lutedit tcl script.
      CMD = ' '
      CALL PSX_GETENV( 'KAPPA_DIR', CMD, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CMD = '/star/bin/kappa'
      END IF

      IAT = CHR_LEN( CMD )
      CALL CHR_APPND( '/lutedit.tcl', CMD, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( LUTNAM, CMD, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( IMNAM, CMD, IAT )

*  Execute the script.
      CALL KPS1_LUTED( CMD( : IAT ), STATUS )

*  If required, save the edited colour table as the current colour table
*  for the specified device.
      IF( SAVLUT .AND. STATUS .EQ. SAI__OK ) THEN

*  Inquire the number of pens that are available on the specified device.
         CALL PGQCOL( LP, UP )

*  The lowest pen number available for used by the colour table is
*  CTM__RSVPN.  0 is reserved for the background.  Others below CTM__RSVPN
*  are reserved for annotations.
         LP = CTM__RSVPN

*  Open the NDF holding the edited LUT.
         CALL NDF_OPEN( DAT__ROOT, LUTNAM, 'UPDATE', 'OLD', INDF1,
     :                  PLACE, STATUS )

*  Ignore any error while opening the NDF.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Map the DATA array.
            CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPLUT, NEL,
     :                    STATUS )

*  Load the LUT into the colour table.
            CALL KPG1_PGLUT( NEL/3, %VAL( CNF_PVAL( IPLUT ) ),
     :                       LP, UP, .FALSE.,
     :                       STATUS )

*  Save the new colour table in $ADAM_USER/kappa_lut.sdf
            CALL KPG1_LTSAV( STATUS )

*  Remove the temporary NDF.
            CALL NDF_DELET( INDF1, STATUS )

         ENDIF

      END IF

*  Close down the graphics device, if open.
      IF( DEVOPN ) CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LUTEDIT_ERR', 'LUTEDIT: Failed to edit or '//
     :                 'create a colour lookup table.',  STATUS )
      END IF

      END
