      SUBROUTINE PROVSHOW( STATUS )
*+
*  Name:
*     PROVSHOW

*  Purpose:
*     Display provenance information for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PROVSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays details of the NDFs that were used in
*     the creation of the supplied NDF. This information is read from the
*     PROVENANCE extension within the NDF, and includes both immediate
*     parent NDFs and older ancestor NDFs (i.e. the parents of the
*     parents, etc).
*
*     Each NDF (including the supplied NDF itself) is described in a
*     block of lines. The first line holds an integer index for the NDF
*     followed by the path to the NDF. Note, this path is where the NDF 
*     was when the provenance information was recorded. It is of course 
*     possible that the NDF may subsequently have been moved or deleted .
*
*     The remaining lines in the NDF description are as follows:
*
*     - "Parents": A comma separated list of integers that are the indices 
*     of the immediate parents of the NDF. These are the integers that are 
*     displayed on the first line of each NDF description.
*
*     - "Date": The formatted UTC date and time at which the provenance 
*     information for the NDF was recorded.
*
*     - "Creator": A string identifying the software that created the
*     NDF.
*
*     - "More": A summary of any extra information about the NDF stored with 
*     the provenance information. In general this may be an arbitrary HDS
*     structure and so full details cannot be given on a single line. The
*     HDSTRACE command can be used to examine the MORE field in detail. To
*     see full details of the NDF with "ID" value of 12 (say), do
*     "hdstrace fred.more.provenance.ancestors'(12)'", where "fred" is
*     the name of the NDF supplied for parameter "NDF". If the NDF has no
*     extra information, this item will not be present.

*  Usage:
*     provshow ndf 

*  ADAM Parameters:
*     NDF = NDF (Read)
*        The NDF data structure.

*  Notes:
*     - If a KAPPA application uses one or more input NDFs to create an 
*     output NDF, the output NDF may or may not contain provenance 
*     information depending on two things: 1) whether any of the
*     input NDFs already contain provenance information and 2) the value
*     of the AUTOPROV environment variable. It is usually necessary to 
*     set the AUTOPROV variable to "1" in order to create output NDFs that
*     contain provenance information. The exception to this if you are 
*     supplied with NDFs from another source that already contain
*     provenance. If such NDFs are used as inputs to KAPPA applications
*     then the output NDFs will contain provenance even if the AUTOPROV
*     variable is unset. However, setting AUTOPROV to "0" will always
*     prevent provenance information being stored in the output NDFs.
*     - Some other packages, such as CCDPACK, follow the same strategy
*     for creating and propagating provenance information.

*  Examples:
*     provshow m51 
*        This displays the provenance information in the NDF m51. 

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JAN-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants 
      INCLUDE 'AST_PAR'          ! AST constants 

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER KEY*20           ! Key for entry within KeyMap
      CHARACTER ID*10            ! Integer index for the current NDF
      CHARACTER VALUE*255        ! Buffer for one field value
      INTEGER INDF               ! NDF identifier
      INTEGER IROW               ! Row index
      INTEGER KYMAP1             ! AST KeyMap holding all prov info
      INTEGER KYMAP2             ! AST KeyMap holding field widths
      INTEGER NC                 ! String length
      INTEGER NROW               ! No. of lines to display
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Format the provenance information in the NDF. The resulting strings
*  are returned in an AST KeyMap.
      CALL NDG_FMPRV( INDF, .FALSE., KYMAP1, STATUS )

*  Get the number of entries in the returned keymap. This will be one
*  more than the number of NDFs described in the displayed table.
      NROW = AST_MAPSIZE( KYMAP1, STATUS ) - 1

*  Loop round each NDF to be described.
      DO IROW = 1, NROW 

*  Get the KeyMap holding details for this row.
         CALL CHR_ITOC( IROW - 1, KEY, NC )
         IF( .NOT. AST_MAPGET0A( KYMAP1, KEY( : NC ), KYMAP2, 
     :                           STATUS ) ) THEN
            STATUS = SAI__ERROR
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETI( 'I', IROW )
               CALL ERR_REP( ' ', 'No "^I" entry found in KeyMap '//
     :                       'returned by NDG_FMPRV (programming '//
     :                       'error).', STATUS )
            END IF
            GO TO 999
         END IF

*  White space between NDF descriptions.
         CALL MSG_BLANK( STATUS )

*  First line starts with the ID value followed by the NDF path.
         ID = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'ID', ID, NC, STATUS ) ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'No "ID" entry found in KeyMap '//
     :                       'returned by NDG_FMPRV (programming '//
     :                       'error).', STATUS )
            END IF
            GO TO 999
         END IF

         VALUE = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'PATH', VALUE, NC, 
     :                           STATUS ) ) THEN
            VALUE = '<the NDF path is unknown>'
         END IF

         CALL MSG_SETC( 'ID', ID )
         CALL MSG_SETC( 'P', VALUE )
         CALL MSG_OUT( ' ', '^ID: ^P', STATUS )

*  Next line shows the list of identifiers for the immediate parent NDFs.
         VALUE = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'PARENTS', VALUE, NC, 
     :                           STATUS ) ) THEN
            VALUE = '<unknown>'
         END IF

         CALL MSG_SETC( 'P', VALUE )
         CALL MSG_OUT( ' ', '   Parents:  ^P', STATUS )

*  Next line shows the date at which the provenance was stored in the NDF.
         VALUE = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'DATE', VALUE, NC, 
     :                           STATUS ) ) THEN
            VALUE = '<unknown>'
         END IF

         CALL MSG_SETC( 'P', VALUE )
         CALL MSG_OUT( ' ', '   Date:  ^P', STATUS )

*  Next line shows the software that created the NDF.
         VALUE = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'CREATOR', VALUE, NC, 
     :                           STATUS ) ) THEN
            VALUE = '<unknown>'
         END IF

         CALL MSG_SETC( 'P', VALUE )
         CALL MSG_OUT( ' ', '   Creator:  ^P', STATUS )

*  Next line shows a summary of any extra info describing the NDF.
         VALUE = ' '
         IF( AST_MAPGET0C( KYMAP2, 'MORE', VALUE, NC, STATUS ) ) THEN
            CALL MSG_SETC( 'P', VALUE )
            CALL MSG_OUT( ' ', '   More:  ^P', STATUS )
         END IF

*  Annul the keymap holding details for this row.
         CALL AST_ANNUL( KYMAP2, STATUS )
      END DO

*  A final blank line.
      CALL MSG_BLANK( STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PROVSHOW_ERR', 'PROVSHOW: Failed to display '//
     :                 'provenance information in an NDF.', STATUS )
      END IF

      END
