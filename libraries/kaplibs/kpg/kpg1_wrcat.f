      SUBROUTINE KPG1_WRCAT( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS,
     :                       TITLE, ID0, IDENTS, KEYMAP, LABS, HIST,
     :                       NULL, STATUS )
*+
*  Name:
*     KPG1_WRCAT

*  Purpose:
*     Writes a set of positions to a text file as a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRCAT( PARAM, ARRDIM, NPOS, NAX, POS, IFRM, IWCS, TITLE,
*                      ID0, IDENTS, KEYMAP, LABS, HIST, NULL, STATUS )

*  Description:
*     This routine is equivalent to KPG1_WRLST, except that it provides
*     the option of storing a textual label with each position, via
*     the extra argument LABS, and extra arbitrary columns via the extra
*     argument KEYMAP.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     ARRDIM = INTEGER (Given)
*        The size of the first dimension of the positions array. This must
*        be larger than or equal to NPOS.
*     NPOS = INTEGER (Given)
*        The number of positions to store in the file.
*     NAX = INTEGER (Given)
*        The number of axes in the Frame specified by IFRM.
*     POS( ARRDIM, NAX ) = DOUBLE PRECISION (Given)
*        The positions to store in the file. POS( I, J ) should give the
*        axis J value for position I.
*     IFRM = INTEGER (Given)
*        The index of the Frame within IWCS to which the supplied
*        positions relate. Can be AST__BASE or AST__CURRENT.
*     IWCS = INTEGER (Given)
*        A pointer to an AST FrameSet to store with the positions.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to store at the top of the text file. Ignored if blank.
*     ID0 = INTEGER (Given)
*        The integer identifier value to associate with the first
*        supplied position. Identifiers for subsequent positions increase
*        by 1 for each position. If this is supplied less than or equal
*        to zero, then its value is ignored and the identifiers supplied
*        in array IDENTS are used instead.
*     IDENTS( NPOS ) = INTEGER (Given)
*        The individual integer identifiers to associate with each
*        position. Only accessed if ID0 is less than or equal to zero.
*     KEYMAP = INTEGER (Given)
*        An optional AST KeyMap containing data for extra columns to
*        add to the catalogue. It can be used (for instance) to add
*        character columns to the catalogue. If a value of AST__NULL
*        is supplied, no extra columns are added to the catalogue.
*        Otherwise, the column names and values can be specified using
*        either of the following two schemes:
*
*        - If the KeyMap contains an entry called "COLNAMES", it is
*        assumed to be a vector entry holding the names of the columns.
*        For each of these column names, the value to store in a
*        particular row of the catalogue is assumed to be held in a
*        scalar KeyMap entry with key "<colname>_<row number>" ( "_1"
*        for the first row). If no such entry exists for a particular
*        row, then the value is marked as bad in the catalogue. The data
*        type of the column is determined from the first row value found
*        for the column.
*
*        - If the KeyMap does not contain an entry called "COLNAMES",
*        it is assumed that each entry in the KeyMap contains a vector
*        holding all the values for a single column, in row order. The
*        entry key is used as the column name, and the column data type
*        is determined from the entry data type.
*     LABS = INTEGER (Given)
*        A GRP group identifier containing the labels to be associated
*        with the positions. The number of elements in this group should
*        be equal to NPOS. If GRP__NOID is supplied, no label column will
*        be created.
*     HIST = INTEGER (Given)
*        A GRP group identifier containing history text to store with the
*        catalogue.  If GRP__NOID is supplied, no history information
*        will be stored with the catalogue.
*     NULL = LOGICAL (Given)
*        Is the user allowed to supply a null value? If so, the error
*        status will be annulled before returning.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-APR-2009 (DSB):
*        Original version, based on KPG1_WRTAB.
*     28-APR-2009 (DSB):
*        Add a second scheme for holding column values in a KeyMap,
*        based on the use of scalar entries rather than vector entries.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER ARRDIM
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( ARRDIM, NAX )
      INTEGER IFRM
      INTEGER IWCS
      CHARACTER TITLE*(*)
      LOGICAL NULL
      INTEGER ID0
      INTEGER IDENTS( NPOS )
      INTEGER KEYMAP
      INTEGER LABS
      INTEGER HIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IBASE              ! Index of base Frame
      INTEGER ICURR              ! Index of current Frame
      INTEGER IDEF               ! Index of default catalogue Frame
      INTEGER IPW                ! Pointer to work space
      INTEGER MAP                ! AST Pointer to Mapping
      INTEGER NBAX               ! No. of axes in BASE FRAME
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Indicate we have not yet changed the Base Frame.
      IBASE = AST__NOFRAME

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  If the input FrameSet has more than 1 Frame, and its ID attribute is
*  not "FIXED_BASE", allow the user to select an alternative base Frame.
      IF( AST_GETI( IWCS, 'NFRAME', STATUS ) .GT. 1 .AND.
     :    AST_GETC( IWCS, 'ID', STATUS ) .NE. 'FIXED_BASE' ) THEN

*  Note the original base and current Frames.
         IBASE = AST_GETI( IWCS, 'BASE', STATUS )
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Set the current Frame to the default Frame to be used for parameter
*  CATFRAME. We pick the default so that the catalogue holds the supplied
*  axis values without change.
         IF( IFRM .EQ. AST__BASE ) THEN
            IDEF = IBASE
         ELSE IF( IFRM .EQ. AST__CURRENT ) THEN
            IDEF = ICURR
         ELSE
            IDEF = IFRM
         END IF

         CALL AST_SETI( IWCS, 'CURRENT', IDEF, STATUS )

*  Allow the user to change the current FRAME.
         CALL MSG_SETC( 'A', 'catalogue' )
         CALL KPG1_ASFRM( 'CATFRAME', 'CATEPOCH', IWCS, 'PIXEL', 'AXIS',
     :                    .TRUE., '^A', STATUS )

*  Set the base Frame equal to the new current Frame, and then re-instate
*  the original current Frame.
         CALL AST_SETI( IWCS, 'BASE', AST_GETI( IWCS, 'CURRENT',
     :                                          STATUS ),
     :                  STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Get the simplified Mapping from the supplied Frame to the Base Frame.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IFRM, AST__BASE,
     :                                       STATUS ), STATUS )

*  Use a UnitMap if there is only 1 Frame in the FrameSet, or if changing
*  of the base Frame has been suppressed by setting the FrameSet's ID
*  attribute to "FIXED_BASE".
      ELSE
         MAP = AST_UNITMAP( AST_GETI( IWCS, 'NIN', STATUS ), ' ',
     :                      STATUS )
      END IF

*  If the forward transformation is not defined, store the positions in
*  the Frame in which they were supplied. The associated FrameSet contains
*  just the specified Frame.
      IF( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) ) THEN
         CALL KPG1_WRTA2( PARAM, ARRDIM, NPOS, NAX, POS,
     :                    AST_FRAMESET( AST_GETFRAME( IWCS, IFRM,
     :                                                STATUS ),
     :                                  ' ', STATUS ),
     :                    TITLE, ID0, IDENTS, KEYMAP, LABS, HIST,
     :                    STATUS )

*  Otherwise, if the Mapping is a UnitMap, we can store the positions as
*  supplied, with the full FrameSet.
      ELSE IF( AST_ISAUNITMAP( MAP, STATUS ) ) THEN
         CALL KPG1_WRTA2( PARAM, ARRDIM, NPOS, NAX, POS, IWCS,
     :                    TITLE, ID0, IDENTS, KEYMAP, LABS, HIST,
     :                    STATUS )

*  Otherwise, we need to map the supplied positions into the Base Frame
*  before storing them.
      ELSE

*  Get the number of axes in the Base Frame.
         NBAX = AST_GETI( IWCS, 'NIN', STATUS )

*  Allocate memory to hold the mapped positions.
         CALL PSX_CALLOC( NBAX*NPOS, '_DOUBLE', IPW, STATUS )

*  Abort if an error occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Transform the supplied positions into the Base Frame, storing them
*  in the workspace.
         CALL AST_TRANN( MAP, NPOS, NAX, ARRDIM, POS, .TRUE., NBAX,
     :                   NPOS, %VAL( CNF_PVAL( IPW ) ), STATUS )

*  Put the stored Frame positions into the file.
         CALL KPG1_WRTA2( PARAM, NPOS, NPOS, NBAX,
     :                    %VAL( CNF_PVAL( IPW ) ), IWCS,
     :                    TITLE, ID0, IDENTS, KEYMAP, LABS, HIST,
     :                    STATUS )

*  Free the workspace.
         CALL PSX_FREE( IPW, STATUS )

      END IF

 999  CONTINUE

*  If a null parameter value was supplied, annul the error if a null
*  value is OK.
      IF( STATUS .EQ. PAR__NULL .AND. NULL ) CALL ERR_ANNUL( STATUS )

*  Re-instate the original base Frame in the FrameSet.
      IF( IBASE .NE. AST__NOFRAME ) CALL AST_SETI( IWCS, 'BASE', IBASE,
     :                                             STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If we still have a null status, or an abort status, re-report the
*  error with a more friendly message.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_WRCAT_ERR', 'Aborted attempt to create '//
     :                 'a positions list using parameter %^PARAM.',
     :                 STATUS )

      ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_WRCAT_ERR', 'Aborted attempt to create '//
     :                 'a positions list using parameter %^PARAM.',
     :                 STATUS )

      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'KPG1_WRCAT_ERR2', 'Failed to create a '//
     :                 'positions list using parameter %^PARAM.',
     :                 STATUS )
      END IF

      END
