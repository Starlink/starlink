      SUBROUTINE PROVSHOW( STATUS )
*+
*  Name:
*     PROVSHOW

*  Purpose:
*     Displays provenance information for an NDF.

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
*     the creation of the supplied NDF. This information is read from
*     the PROVENANCE extension within the NDF, and includes both
*     immediate parent NDFs and older ancestor NDFs (i.e. the parents of
*     the parents, etc.).
*
*     Each displayed NDF (see parameter SHOW) is described in a
*     block of lines. The first line holds an integer index for the NDF
*     followed by the path to that NDF. Note, this path is where the NDF
*     was when the provenance information was recorded. It is of course 
*     possible that the NDF may subsequently have been moved or deleted.
*
*     The remaining lines in the NDF description are as follows:
*
*       "Parents" -- A comma-separated list of integers that are the 
*       indices of the immediate parents of the NDF.  These are the
*       integers that are displayed on the first line of each NDF 
*       description.
*
*       "Date" -- The formatted UTC date and time at which the 
*       provenance information for the NDF was recorded.
*
*       "Creator" -- A string identifying the software that created the
*       NDF.
*
*       "More" -- A summary of any extra information about the NDF 
*       stored with the provenance information.  In general this may be 
*       an arbitrary HDS structure and so full details cannot be given 
*       on a single line.  The HDSTRACE command can be used to examine 
*       the MORE field in detail.  To see full details of the NDF with 
*       "ID" value of 12 (say), enter (from a UNIX shell)
*       "hdstrace fred.more.provenance.ancestors'(12)'", where "fred" is
*       the name of the NDF supplied for parameter "NDF".  If the NDF 
*       has no extra information, this item will not be present.
*
*    In addition, a text file can be created containing the paths for the
*    direct parents of the supplied NDF. See parameter PARENTS.

*  Usage:
*     provshow ndf show

*  ADAM Parameters:
*     NDF = NDF (Read)
*        The NDF data structure.
*     PARENTS = FILENAME (Read)
*        Name of a new text file in which to put the paths to the direct
*        parents of the supplied NDF. These are written one per line with
*        no extra text. If null, no file is created. [!]
*     SHOW = LITERAL (Read)
*        Determines which ancestors are displayed on the screen. It can
*        take any of the following case-insensitive values (or any 
*        abbreviation):
*
*        - "All" -- Display all ancestors, including the supplied NDF
*                   itself.
*
*        - "Roots" -- Display only the root ancestors (i.e. ancestors that
*                    do not themselves have any recorded parents). The 
*                    supplied NDF itself is not displayed.
*
*        - "Parents" -- Display only the direct parents of the supplied
*                       NDF. The supplied NDF itself is not displayed.
*
*        ["All"]

*  Notes:
*     - If a KAPPA application uses one or more input NDFs to create an 
*     output NDF, the output NDF may or may not contain provenance 
*     information depending on two things: 1) whether any of the
*     input NDFs already contain provenance information, and 2) the
*     value of the AUTOPROV environment variable. It is usually 
*     necessary to set the AUTOPROV variable to "1" in order to create 
*     output NDFs that contain provenance information.  The exception to
*     this if you are supplied with NDFs from another source that 
*     already contain provenance.  If such NDFs are used as inputs to 
*     KAPPA applications, then the output NDFs will contain provenance 
*     even if the AUTOPROV variable is unset.  However, setting AUTOPROV
*     to "0" will always prevent provenance information being stored in 
*     the output NDFs.
*     - Some other packages, such as CCDPACK, follow the same strategy
*     for creating and propagating provenance information.

*  Examples:
*     provshow m51 
*        This displays information about the NDF m51, and all its
*        recorded ancestors.
*     provshow m51 roots
*        This displays information about the root ancestors of the NDF 
*        m51. 
*     provshow m51 parents
*        This displays information about the direct parents of the NDF 
*        m51. 

*  Related Applications:
*     KAPPA: PROVADD, HISLIST.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-JAN-2008 (DSB):
*        Original version.
*     7-FEB-2008 (DSB):
*        Added parameter PARENTS.
*     12-AUG-2008 (DSB):
*        Added parameter SHOW.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'DAT_PAR'          ! DAT constants 
      INCLUDE 'AST_PAR'          ! AST constants 

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXPAR              ! Max number of direct parents
      PARAMETER( MXPAR = 200 )

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to array cell
      CHARACTER ID*10            ! Integer index for the current NDF
      CHARACTER KEY*20           ! Key for entry within KeyMap
      CHARACTER PLOC*(DAT__SZLOC)! Locator for PARENTS array
      CHARACTER PROV*(DAT__SZLOC)! Locator for provenance information
      CHARACTER PROVP*(DAT__SZLOC)! Locator for parent provenance info
      CHARACTER SHOW*7           ! The ancestors to be displayed
      CHARACTER PARIDS*255       ! Buffer for direct parent ID list
      CHARACTER VALUE*255        ! Buffer for one field value
      INTEGER DIRPAR( MXPAR )    ! Integer IDs for direct parents
      INTEGER FD                 ! File descriptor for parents file
      INTEGER INDF               ! NDF identifier
      INTEGER INTID              ! Integer ID for the current ancestor
      INTEGER IPAR               ! Index into list of parent indices
      INTEGER IROW               ! Row index
      INTEGER KYMAP1             ! AST KeyMap holding all prov info
      INTEGER KYMAP2             ! AST KeyMap holding field widths
      INTEGER NC                 ! String length
      INTEGER NPAR               ! Number of direct parents
      INTEGER NROW               ! No. of lines to display
      INTEGER PARI               ! Index of current parent in ancestors
      LOGICAL THERE              ! Does the named component exist?
      LOGICAL USE                ! Display the current ancestor?
*.


*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  Determine which ancestors are to be displayed on the screenn.
      CALL PAR_CHOIC( 'SHOW', 'All', 'All,Roots,Parents', .FALSE., SHOW, 
     :                STATUS )

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

*  Get the ID value for this ancestor.
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

*  Get the list of direct patent ID values for this ancestor.
         PARIDS = ' '
         IF( .NOT. AST_MAPGET0C( KYMAP2, 'PARENTS', PARIDS, NC, 
     :                           STATUS ) ) THEN
            PARIDS = '<unknown>'
         END IF

*  Decide if this ancestor should be displayed. This depends on the value
*  supplied for parameter SHOW. If "ROOT" then only display this ancestor
*  if it has no direct parents. 
         IF( SHOW .EQ. 'ROOT' ) THEN
            USE = ( PARIDS .EQ. '<unknown>' )

*  If "PARENTS" then only display this ancestor if its integer ID is 
*  included in the list of direct parent IDs stored when processing the
*  first row (if this is the first row then we always process it).
         ELSE IF( SHOW .EQ. 'PARENTS' ) THEN

*  If we are currently checking the first row, then we never display 
*  it, but we extract the direct parent ID values into an array of 
*  integers for use when checking subsequent rows.
            IF( IROW .EQ. 1 ) THEN
               USE = .FALSE.
               CALL KPG1_PRSAI( PARIDS, MXPAR, DIRPAR, NPAR, STATUS )

*  If this is not the first row, we only display it if its integer ID
*  value is in ther list od direct parent IDs obtained when checking the
*  first row.
            ELSE
               CALL CHR_CTOI( ID, INTID, STATUS )
               USE = .FALSE.
               DO IPAR = 1, NPAR
                  IF( DIRPAR( IPAR ) .EQ. INTID ) USE = .TRUE.
               END DO

            END IF

*  For other values of SHOW, we display all ancestors.
         ELSE
            USE = .TRUE.
         END IF

*  Jump to the next row if we are not displaying the current row. 
         IF( USE ) THEN

*  White space between NDF descriptions.
            CALL MSG_BLANK( STATUS )

*  First line starts with the ID value followed by the NDF path.
            VALUE = ' '
            IF( .NOT. AST_MAPGET0C( KYMAP2, 'PATH', VALUE, NC, 
     :                              STATUS ) ) THEN
               VALUE = '<the NDF path is unknown>'
            END IF
	    
            CALL MSG_SETC( 'ID', ID )
            CALL MSG_SETC( 'P', VALUE )
            CALL MSG_OUT( ' ', '^ID: ^P', STATUS )

*  Next line shows the list of identifiers for the immediate parent NDFs.
            CALL MSG_SETC( 'P', PARIDS )
            CALL MSG_OUT( ' ', '   Parents:  ^P', STATUS )

*  Next line shows the date at which the provenance was stored in the NDF.
            VALUE = ' '
            IF( .NOT. AST_MAPGET0C( KYMAP2, 'DATE', VALUE, NC, 
     :                              STATUS ) ) THEN
               VALUE = '<unknown>'
            END IF
	    
            CALL MSG_SETC( 'P', VALUE )
            CALL MSG_OUT( ' ', '   Date:  ^P', STATUS )

*  Next line shows the software that created the NDF.
            VALUE = ' '
            IF( .NOT. AST_MAPGET0C( KYMAP2, 'CREATOR', VALUE, NC, 
     :                              STATUS ) ) THEN
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

         END IF

*  Annul the keymap holding details for this row.
         CALL AST_ANNUL( KYMAP2, STATUS )

      END DO

*  A final blank line.
      CALL MSG_BLANK( STATUS )

*  If required, create a text file containing the paths to the direct
*  parents of the supplied NDF.
*  ================================================================

*  Get an FIO file descriptor for the output file. Annul the error if no
*  file is needed.
      CALL FIO_ASSOC( 'PARENTS', 'WRITE', 'LIST', 255, FD, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  If a file is beign created, get the provenance information for the
*  supplied NDF. This includes the indices of the NDFs direct parents.
      ELSE
         CALL NDG_GTPRV( INDF, 0, PROV, STATUS )

*  Check that the NDF has some parents.
         CALL DAT_THERE( PROV, 'PARENTS', THERE, STATUS )
         IF( THERE ) THEN

*  Loop round each integer index in the PARENTS component of the
*  NDFs provenance information. These are the indices of the NDFs direct
*  parents.
            CALL DAT_FIND( PROV, 'PARENTS', PLOC, STATUS )
            CALL DAT_SIZE( PLOC, NPAR, STATUS )
            DO IPAR = 1, NPAR

*  Get a locator for the current cell of the PARENTS array, and ge the
*  integer value in the cell.
               CALL DAT_CELL( PLOC, 1, IPAR, CLOC, STATUS )
               CALL DAT_GET0I( CLOC, PARI, STATUS )

*  Get the provenance information for the ancestor with the index read
*  from the current cell of the PARENTS aray.
               CALL NDG_GTPRV( INDF, PARI, PROVP, STATUS )

*  Check the provenance information includes a path to the parent file.
*  If so, get the path and write it to the output text file.
               CALL DAT_THERE( PROVP, 'PATH', THERE, STATUS )
               IF( THERE ) THEN
                  CALL CMP_GET0C( PROVP, 'PATH', VALUE, STATUS )
                  CALL FIO_WRITE( FD, VALUE( : CHR_LEN( VALUE ) ), 
     :                            STATUS )
               END IF

*  Free the locators to the parent's provenance information, and the
*  current cell of the PARENTS array.
               CALL DAT_ANNUL( PROVP, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Next parent.
            END DO

*  Free the locator to the PARENTS array.
            CALL DAT_ANNUL( PLOC, STATUS )

         END IF

*  Free the locator to the NDFs provenance information, and close the
*  output text file.
         CALL DAT_ANNUL( PROV, STATUS )
         CALL FIO_ANNUL( FD, STATUS )
      END IF

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
