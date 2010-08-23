      INTEGER FUNCTION KPG1_WCATW( IAST, CI, STATUS )
*+
*  Name:
*     KPG1_MKCAT

*  Purpose:
*     Writes  an AST Object to a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_WCATW( IAST, CI, STATUS )

*  Description:
*     This routine stores a textual representation of the supplied AST Object
*     within the supplied catalogue. The information is stored within COMMENT
*     strings which are appended to the supplied catalogue's textual
*     information. Lines of AST information which are too long to fit in a
*     single COMMENT are split into several lines. Continuation lines are
*     marked by having a "+" in the first column. Each line of AST
*     information is preceeded with the string "!!", which is used to
*     mark the end of any leading blanks etc. added by AST when the string is
*     read back.
*
*     Note, at the moment CAT reports errors if textual information is
*     added to a catalogue which contains no rows of data. For this reason,
*     this routine should normally be called just before closing the
*     catalogue, since this will normally ensure that the catalogue
*     contains some data.

*  Arguments:
*     IAST = INTEGER (Given)
*        An AST pointer to the Object.
*     CI = INTEGER (Given)
*        A CAT identifier for the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Function Value:
*     KPG1_WCATW = INTEGER
*        Returned equal to 1 if an Object was written to the catalogue,
*        and zero otherwise.

*  Copyright:
*     Copyright (C) 1998, 1999, 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     26-JAN-1998 (DSB):
*        Original version.
*     26-MAY-1999 (DSB):
*        Safety margin of 2 characters removed from ASTTSZ to avoid CAT
*        splitting lines itself, resulting in lines which do not start
*        with "!!".
*     6-APR-2001 (DSB):
*        Limit max length of a line of text to no more than the size of a
*        GRP element.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants (needed by KPG_AST)
      INCLUDE 'CAT_PAR'          ! CAT constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common block
*        ASTGRP= INTEGER (Write)
*           GRP identifier for the group holding AST_ data.
*        ASTLN = INTEGER (Write)
*           Next element to use in the group holding AST_ data.
*        ASTTSZ = INTEGER (Write)
*           Maximum length of text to be stored in a group element.

*  Arguments Given:
      INTEGER IAST
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_SNKTA
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER NAME*(CAT__SZCNM)! Catalogue name
      CHARACTER TEXT*(GRP__SZNAM)! Buffer for a line of text
      INTEGER CHAN               ! Pointer to an AST Channel
      INTEGER I                  ! Element index
      INTEGER SIZE               ! No. of elements in group
*.

*  Initialise the returned function value to indicate no Object has been
*  written.
      KPG1_WCATW = 0

*  Check the inherited status. Also return if no Object was supplied.
      IF( STATUS .NE. SAI__OK .OR. IAST .EQ. AST__NULL ) RETURN

*  Store the catalogue name for use in error messages.
      CALL CAT_TIQAC( CI, 'NAME', NAME, STATUS )

*  Construct the textual representation of the AST Object, storing it
*  within a GRP group.
*  ===================================================================

*  Create a new group. The returned identifier is stored in common so
*  that it can be accessed by the AST_CHANNEL sink function.
      CALL GRP_NEW( 'CAT textual information', ASTGRP, STATUS )

*  Create an AST_ Channel to write the supplied data to the catalogue.
*  Supply the KPG1_SNKTA routine as the "sink" routine for storing the
*  data, and specify that only essential information be included. The
*  source for KPG1_SNKTA is appended to this file.
      CHAN = AST_CHANNEL( AST_NULL, KPG1_SNKTA, 'Full=-1,Comment=0',
     :                    STATUS )

*  Get the maximum length of text string which can be written to a
*  COMMENT in the catalogue. Store this value in common. Limit it to be
*  no more than the size of a GRP element.
      CALL CAT_SZTXT( CI, 'WRITE', ASTTSZ, STATUS )
      ASTTSZ = MIN( ASTTSZ, GRP__SZNAM )

*  Reduce it by 4 to leave room for the "!!" string used to mark the
*  start of the AST information, plus an extra safety margin
      ASTTSZ = ASTTSZ - 4

*  Initialise the index of the first element in the group to be
*  used by the sink function.
      ASTLN = 1

*  Write the copy of the supplied AST_ object to the Channel, thus
*  transferring the data to the catalogue.
      KPG1_WCATW = AST_WRITE( CHAN, IAST, STATUS )

*  Annul the Channel pointer, thus deleting the Channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Copy the textual representation of the AST Object from the group to
*  the catalogue.
*  ===================================================================

*  Loop round each element in the group.
      CALL GRP_GRPSZ( ASTGRP, SIZE, STATUS )
      DO I = 1, SIZE

*  Get the next element from the group. Add the string "!!" to the start.
*  This allows KPG1_RCATW (which reads these lines) to identify the start
*  of the AST information. This is needed since CAT can add leading spaces,
*  which are significant to KPG1_RCATW.
         TEXT = '!!'
         CALL GRP_GET( ASTGRP, I, 1, TEXT( 3: ), STATUS )

*  Append it to the catalogue's textual information, as a COMMENT. Add
*  the string "!!" to the start. This allows KPG1_RCATW (which reads these
*  lines) to identify the start of the AST information. This is needed
*  since CAT can add leading spaces, which are significant to KPG1_RCATW.
         CALL CAT_PUTXT( CI, 'COMMENT', TEXT( : CHR_LEN( TEXT ) ),
     :                   STATUS )

      END DO

*  Delete the group.
      CALL GRP_DELET( ASTGRP, STATUS )

*  If an error occurred during data transfer, report a contextual error
*  message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'CAT', NAME )
         CALL ERR_REP( 'KPG1_WWRT_WRT', 'Error while writing AST_ '//
     :                 'data to the catalogue ^CAT.', STATUS )
      END IF

      END
